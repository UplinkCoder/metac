# The architecture of MetaC

MetaC is a C99 compatible compiler with an unusual set of features: types
are values, the compiler is reachable from the program it compiles and
the whole compiler core is written for a fiber based scheduler.

This document describes how the pieces fit together.  Every file that is
mentioned here exists in the repository; the line numbers are the ones at
the time of writing.

---

## 1. The big picture

```
                        source text
                             |
   +-------------------------+--------------------------+
   |                     os/metac_file.c               |
   |              metac_filesystem_t (vtable)         |
   +-------------------------+--------------------------+
                             |
                             v
   +---------------------------------------------------+
   |  parser/                                          |
   |    metac_lexer.c    text  -> metac_token_t         |
   |    metac_preproc.c  macros, #if, #include, #eval  |
   |    metac_parser.c   tokens -> metac_decl_t        |
   |                     tokens -> metac_stmt_t        |
   |                     tokens -> metac_expr_t        |
   |    metac_parsetree.c  the node types              |
   +-------------------------+-------------------------+
                             |  parse tree
                             v
   +---------------------------------------------------+
   |  semantic/                                         |
   |    metac_scope.c    the scope chain and its tables |
   |    metac_type*.c    the type system                |
   |    metac_semantic.c name resolution, decls         |
   |    metac_expr_semantic.c  expressions              |
   |    metac_sematree.c the semantic node types        |
   +-------------------------+-------------------------+
                             |  semantic tree
                             v
   +---------------------------------------------------+
   |  codegen/metac_codegen.c                           |
   |    the BackendInterface vtable is the only seam    |
   +--------+---------+-----------+-----------+
            |         |           |           |
            v         v           v           v
   +----------+ +----------+ +----------+ +----------+
   | bc_inter | | bc_light | | js_back  | | printer  |
   | pretter  | | ning     | | end      | | _backend |
   | (the VM) | | (the JIT)| | (JS src) | | (listing)|
   +----------+ +----------+ +----------+ +----------+
            |
            v
      BCValue on a BCHeap  -->  the result of an evaluation
```

The interesting property of this diagram is the **single seam**: the
code generator does not know anything about the backends, it only calls
the ~100 function pointers of `struct BackendInterface`
(`libinterpret/backend_interface_funcs.h`).  That is what makes it
possible to run the same program on a bytecode interpreter, on a JIT, as
JavaScript source or as a human readable listing.

---

## 2. The pipeline, layer by layer

### 2.1 The file system (`os/metac_file.h`, `os/metac_file.c`)

MetaC does not read files directly.  It asks a `metac_filesystem_t` for
them, and that file system is provided by the *front-end*:

```c
typedef struct metac_filesystem_t
{
    metac_filesystem_ctx* ctx;
    const metac_filesystem_functions_t* functions;
} metac_filesystem_t;
```

The vtable has eight entries: `Init`, `Open`,
`ReadEntireFileAndZeroTerminate`, `Close`, `GetFileInfo`,
`ReadUntilBufferFullAndZeroTerminate`, `GetFileSize` and
`GetFilesystemSize`.

There are two implementations in the tree:

* `os/metac_file.c` -- `NativeFileSystem`, which maps the calls onto
  `fopen`/`fread` (`realpath` is used to resolve the path),
* the GTK REPL (`repl/gtk/gtk_repl.c`) -- a file system whose files are
  the tabs of the editor.

The `metac_file_storage_t` sits on top of it and interns the file names
and the paths into identifier tables.

### 2.2 The lexer (`parser/metac_lexer.c`)

The lexer turns text into `metac_token_t`.  A token carries its type, its
position, a location index and a union with the payload (an identifier
key, a string key, a numeric value, a comment, ...).

Two things are worth knowing:

* **the keyword matching is generated.**  `utils/gen_metac_match_keyword.c`
  compares the crc32c hash of an identifier against the hashes of the
  keywords and generates `generated/metac_match_keyword.inl`, which is
  included by the lexer.  The hashes themselves come from
  `generated/metac_keyword_keys.h`.
* **newlines are dropped.**  A `tok_newline` is never put into the token
  array, so a statement may span several lines without any continuation
  logic.  The line and column of a token are kept in the location
  storage instead.

The location storage (`metac_location_t_array`) is a separate array of
`metac_location_t { StartLine, LineSpan, StartColumn, EndColumn,
SourceId }`; the tokens refer to it by index.

### 2.3 The preprocessor (`parser/metac_preproc.c`)

`metac_preprocessor_t` implements `#define` (with parameters and
variadic macros), the `#if`/`#ifdef`/`#else` family, `#include`,
`#pragma` and `#eval`.

`#eval` is interesting: it evaluates a constant expression with the
bytecode interpreter of the compiler itself
(`MetaCPreProcessor_Eval`).

The preprocessor has its own file storage, so `#include` goes through the
`metac_filesystem_t` of the front-end.

### 2.4 The parser (`parser/metac_parser.c`, `parser/metac_expr_parser.c`)

The parser is a recursive descent parser that produces the *parse tree*:

| node kind        | C type              | header                |
|------------------|----------------------|-----------------------|
| declarations     | `metac_decl_t`       | `parser/metac_parsetree.h` |
| statements       | `metac_stmt_t`       | `parser/metac_parsetree.h` |
| expressions      | `metac_expr_t`       | `parser/metac_parsetree.h` |
| types            | `decl_type_t`        | `parser/metac_parsetree.h` |

All of them are a header plus a union, so a node can be cast to its
concrete type with a single `cast()`.

There are **two** expression parsers in the tree:

* `MetaCParser_ParseExpr` (in `metac_parser.c`) -- the old one,
* `MetaCParser_ParseExpr2` (in `metac_expr_parser.c`) -- the new one.

The switch between them is the `OLD_PARSER` define; the REPL uses the old
one (`repl/repl.c:1`).

`MetaCParser_ParseStmt` is the entry point that is interesting for the
REPLs: it accepts **both** a declaration and an expression and reports
which one it got (`stmt_decl` vs `stmt_expr`).  Comments become
`stmt_comment` nodes, which is why the GTK REPL can echo them back.

### 2.5 The semantic analyser (`semantic/`)

This is the biggest layer.  It turns the parse tree into the *semantic
tree*, in which every expression has a resolved type and every name is
resolved to a declaration.

**The scope system** (`semantic/metac_scope.c`)

A scope is a hash table of identifiers:

```c
typedef struct metac_scope_t
{
    metac_scope_flags_t ScopeFlags;   /* temporary / mounted / closed */
    metac_scope_owner_t Owner;        /* module, function, struct, ... */
    struct metac_scope_t* Parent;
    union { metac_scope_table_t ScopeTable; metac_scope_t* MountedScope; };
    uint32_t Serial;
} metac_scope_t;
```

The owner is packed into 32 bits (`Index:28 | Kind:4`), the same trick
that is used for the type indices and the storage locations.  A scope
table is an open addressed hash table with linear probing.

**The type system** (`semantic/metac_type.h`, `semantic/metac_type_table.c`)

A type is a 32 bit handle:

```c
#define TYPE_INDEX_V(KIND, INDEX) (((KIND) << 28) | (INDEX))
```

with 16 kinds (`type_index_basic`, `type_index_ptr`, `type_index_array`,
`type_index_struct`, `type_index_union`, `type_index_enum`,
`type_index_functiontype`, `type_index_typedef`, `type_index_tuple`,
`type_index_template`, ...).  Every kind has its own table
(`metac_type_table.h`), which is generated with an X macro:

```c
#define FOREACH_TABLE_MEMBER(M) \
    M(enum, Enum, EnumSlotsEqual) M(array, Array, ArraySlotsEqual) \
    M(aggregate, Struct, AggregateSlotsEqual) ...
```

The table deduplicates the types: asking for `int*` twice gives the same
handle.  This is what makes *types are values* possible -- the value of a
type is its index.

**The semantic state** (`semantic/metac_semantic.c`)

`metac_sema_state_t` is the context of one compilation:

```c
typedef struct metac_sema_state_t
{
    bool initialized;
    metac_alloc_t Allocator, TempAlloc;
    metac_identifier_table_t SemanticIdentifierTable, SemanticStringTable;
    metac_identifier_table_t* ParserIdentifierTable;   /* aliased! */
    metac_identifier_table_t* ParserStringTable;
    metac_scope_t* CurrentScope;
    metac_scope_t* StructTagScope, *UnionTagScope, *EnumTagScope;
    /* nine type tables */
    /* arena arrays with the sema nodes */
    ARENA_ARRAY(metac_sema_expr_t*, Globals);
    ...
} metac_sema_state_t;
```

Note that the semantic state *aliases* the identifier tables of the
parser: the semantic analyser does not copy the strings, it only uses
the parser's interned identifiers.  This is why the identifiers of a
declaration have to be copied into `SemanticIdentifierTable` before they
can be used as a key of the scope table (see section 6).

**The semantic tree** (`semantic/metac_sematree.c`)

`metac_sema_expr_t` mirrors `metac_expr_t` and adds the `TypeIndex` and
the resolved payload (`Variable`, `Field`, `Function`, `Call`,
`TypeExp`, ...).  `metac_sema_stmt_t` and `metac_sema_decl_t` do the
same for statements and declarations.

The walker `MetaCSemaTree_Walk_Real(node, sema, walker_fn, ctx)` visits
every node of the tree.

### 2.6 The code generator (`codegen/metac_codegen.c`)

The code generator walks the semantic tree and emits calls into the
backend interface.  Its context is `metac_bytecode_ctx_t`:

```c
typedef struct metac_bytecode_ctx_t
{
    void* c;                                   /* the backend context */
    metac_alloc_t Allocator;
    ARENA_ARRAY(metac_bytecode_function_t, Functions);
    ARENA_ARRAY(BCValue, Globals);
    ARENA_ARRAY(metac_external_entry_t, Externals);
    tagged_arena_t GlobalMemory;               /* the globals live here */
    const BackendInterface* gen;               /* the backend */
    ARENA_ARRAY(BCValue, Locals, Parameters);  /* per function */
    variable_store_t Vstore;
    ...
} metac_bytecode_ctx_t;
```

The central function is `MetaCCodegen_doExpr(ctx, expr, result,
valueType)`, a big switch over the expression kinds.

### 2.7 The backends (`libinterpret/`)

All four backends implement the same vtable:

| file                              | backend     | produces                        |
|-----------------------------------|-------------|---------------------------------|
| `bc_interpreter_backend.c` (106K) | `BCGen`     | bytecode for the VM             |
| `bc_lightning_backend.c` (49K)    | `Lightning` | machine code (GNU lightning)    |
| `js_backend.c` (28K)              | `JsBackend` | JavaScript source               |
| `printer_backend.c` (31K)         | `Printer`   | a human readable listing        |

The vtable itself lives in `libinterpret/backend_interface_funcs.h` and
has roughly a hundred entries: `Initialize`, `BeginFunction`,
`GenTemporary`, `Load8/16/32/64`, `Store8/16/32/64`, `Add3`, `Sub3`,
`Call`, `Jmp`, `GenLabel`, `Alloc`, `MemCpy`, `Throw`, `Run`, ...

### 2.8 The bytecode (`libinterpret/bc_common.h`,
`libinterpret/bc_interpreter_backend.h`)

The values of the VM are `BCValue`:

```c
typedef struct BCValue
{
    BCValueType vType;   /* Temporary, Parameter, Local, HeapValue, ... */
    BCType type;
    const char* name;
    union { int8_t parameterIndex; uint16_t temporaryIndex;
            uint16_t localIndex; uint16_t externalIndex; };
    bool couldBeVoid;
    union { StackAddr stackAddr; HeapAddr heapAddr;
            ExternalAddr externalAddr; Imm32 imm32; Imm64 imm64; void* voidStar; };
} BCValue;
```

The instruction set is the `enum LongInst` of
`libinterpret/bc_interpreter_backend.h`.  It has the shape

```
LongInst_PrintValue, LongInst_RelJmp, LongInst_Ret32/64, LongInst_Not,
LongInst_Jmp, LongInst_JmpFalse, LongInst_JmpTrue, LongInst_JmpZ, LongInst_JmpNZ,
LongInst_PushCatch, LongInst_PopCatch, LongInst_Throw,
LongInst_Add, LongInst_Sub, LongInst_Div, LongInst_Mul, LongInst_Mod,
LongInst_Eq, LongInst_Neq, LongInst_Lt, LongInst_Le, LongInst_Gt, LongInst_Ge,
LongInst_Ult, LongInst_Ule, LongInst_Ugt, LongInst_Uge,
LongInst_And, LongInst_Or, LongInst_Xor, LongInst_Lsh, LongInst_Rsh, LongInst_Set,
LongInst_ImmAdd ... LongInst_ImmUmod (the immediate variants),
LongInst_FAdd32 ... LongInst_FGe64 (the float variants),
LongInst_SetHighImm32, LongInst_SetImm32, LongInst_SetImm8,
LongInst_ContextManip, LongInst_Call,
LongInst_HeapLoad8/16/32/64, LongInst_HeapStore8/16/32/64,
LongInst_SetMode, LongInst_Alloc, LongInst_MapExternal, LongInst_MemCpy,
LongInst_Realloc, LongInst_BuiltinCall, LongInst_Comment, LongInst_Line,
LongInst_File, LongInst_ReadI32
```

The comparison instructions form a contiguous range, which is marked with
`FIRST_REG_CMP` / `LAST_REG_CMP` (and the `FIRST_IMM_CMP` /
`LAST_IMM_CMP` pair) so that the interpreter can check the kind of an
instruction with a single comparison.

### 2.9 The operating system layer (`os/`)

| file                   | what it provides                                |
|------------------------|-------------------------------------------------|
| `os/os.c`              | `struct OS`: page allocation, time stamps        |
| `os/metac_alloc.c`     | the arena allocator (`metac_alloc_t`)            |
| `os/metac_file.c`      | the native file system                           |
| `os/metac_task.c`      | the task system and the fiber scheduler          |
| `os/metac_coro.c`      | the coroutine layer (libaco or boost.context)    |
| `os/metac_atomic.h`    | atomics                                         |
| `os/metac_simd.h`      | an SSE2/NEON abstraction (used by the scope LRU) |
| `os/bsf.h`, `bsr.h`    | bit scan intrinsics                              |

**The arena allocator** is used everywhere.  An allocator owns an array
of *arenas*; an arena is a contiguous block of memory that is handed out
in pieces.  The arrays of nodes (`ARENA_ARRAY`) live in an arena as
well, which means that growing them can move the data -- that is why the
code has to do an `ARENA_ARRAY_REFCOPY` after an `ARENA_ARRAY_ADD`.

An allocator has a *parent*: when an allocation does not fit into the
existing arenas, the allocator asks its parent for the memory
(`Allocate_` in `os/metac_alloc.c`).  **This is the reason why an
allocator must never be copied** -- see section 6.

**The task system** (`os/metac_task.c`) is the scheduler of the compiler:

```c
typedef struct task_t
{
    void (*TaskFunction)(struct task_t* task);
    void* Context;
    struct task_t* Parent;
    aco_t* Fiber;
    union { struct task_t* Continuation;
            struct { void* ResultPtr; aco_t* Caller; };
            void (*ContinuationFunc)(void*); };
    const char* YieldReason;
    volatile task_flags_t TaskFlags;
    uint8_t _inlineContext[32];
    ...
} task_t;
```

`RunWorkerThread(worker, specialFunc, specialFuncCtx)` starts the
scheduler: it creates a fiber pool (`FIBERS_PER_WORKER` fibers with
shared stacks), a task queue and runs the "special" fiber -- which is
the REPL.  When the special fiber yields (`YIELD`), the scheduler runs
the pending tasks and then resumes it.

**The coroutine layer** has two implementations with the same API:

* `3rd_party/libaco` (the default),
* `os/metac_coro_fcontext.c` -- boost.context, selected with
  `-DMETAC_CORO_FCONTEXT`.

---

## 3. The evaluation path of a REPL

The REPLs evaluate an expression by compiling *and* running it:

```
   metac_expr_t  (parse tree)
          |
          v
   MetaCSemantic_doExprSemantic()      semantic/metac_expr_semantic.c
          |
          v
   metac_sema_expr_t  (typed)
          |
          v
   EvaluateExpr(sema, expr, heap)      semantic/metac_expr_semantic.c
          |
          +--> MetaCCodegen_SetDefaultInterface(&Printer_interface)
          |        codegen with the listing backend -> the trace on stdout
          |
          +--> MetaCCodegen_SetDefaultInterface(&BCGen_interface)
          |        codegen with the VM backend
          |        MetaCCodegen_RunFunction(...) -> a BCValue on the heap
          |
          v
   a metac_sema_expr_t that holds the value
```

The result is then printed with `MetaCPrinter_PrintSemaNode()`.

---

## 4. The REPL (`repl/`)

| file                  | what it is                                            |
|-----------------------|-------------------------------------------------------|
| `repl/repl.h`         | `repl_state_t`, `repl_mode_t`, `ui_interface_t`        |
| `repl/repl.c`         | the REPL itself (~1500 lines)                         |
| `repl/linenoise_repl.c` | the terminal front-end + `main()`                   |
| `repl/curses.c`       | a curses front-end                                    |
| `repl/gtk/gtk_repl.c` | the GTK3 front-end (see `repl/gtk/README.md`)         |
| `repl/completion_trie.c` | the completion trie                                |
| `repl/exp_eval.c`     | an old expression evaluator (disabled)                |

`repl_state_t` owns the whole state of a session:

```c
typedef struct repl_state_t
{
    repl_mode_t ParseMode;
    metac_lpp_t LPP;                 /* lexer + preprocessor + parser */
    metac_sema_state_t SemanticState;
    repl_ui_context_t UiContext;
    completion_trie_root_t CompletionTrie;
    BCHeap Heap;                     /* the runtime heap              */
    metac_lexer_t FileLexer;
    metac_file_storage_t FileStorage;
    metac_printer_t Printer;
    metac_alloc_t FileAllocator, Allocator, CompletionAlloc;
} repl_state_t;
```

The `ui_interface_t` is the seam between the REPL and its front-end:

```c
typedef struct ui_interface_t
{
    const char* (*GetInputLine)(struct ui_state_t*, repl_state_t*, uint32_t*);
    void (*Message)(struct ui_state_t*, const char* fmt, ...);
    void (*ErrorMessage)(struct ui_state_t*, const char* fmt, ...);
    repl_mode_t (*QueryMode)(struct ui_state_t*);
    void (*Clear)(struct ui_state_t*);
    void (*Info)(struct ui_state_t*, const char* fmt, ...);
    void (*SetCompletionCallback)(struct ui_state_t*, repl_state_t*, completion_cb_t);
    metac_filesystem_t* (*GetFileSystem)(struct ui_state_t*);
    uint32_t (*UpdateLocalCompletionCache)(struct ui_state_t*, completion_cache_entry_t*, uint32_t);
} ui_interface_t;
```

### The modes

The mode decides how a line is interpreted:

| command | mode              | what happens                                          |
|---------|-------------------|-------------------------------------------------------|
| `:t`    | `repl_mode_token` | lex the line, print the token kinds                   |
| `:d`    | `repl_mode_decl`  | parse the line as a declaration, print it             |
| `:ds`   | `repl_mode_ds`    | parse it and run the declaration semantics            |
| `:s`    | `repl_mode_stmt`  | parse the line as a statement, print it               |
| `:ss`   | `repl_mode_ss`    | parse it and run the statement semantics              |
| `:e`    | `repl_mode_expr`  | parse the line as an expression, print it             |
| `:ee`   | `repl_mode_ee`    | parse, analyse and **evaluate** it                    |
| `:es`   | `repl_mode_es`    | parse and analyse it, print its **type**              |
| `:p`    | `repl_mode_preproc` | parse a preprocessor directive                      |
| `:l`    | `repl_mode_lex_file` | load a file and lex it                             |
| `:f`    |                   | load a file, parse it, translate its identifiers      |
| `:g lobals` |                | print the globals of the session                      |
| `:heap` |                   | hex dump of the runtime heap                          |
| `:c`    |                   | clear the screen                                      |
| `:q`    |                   | quit                                                  |
| `:h`    |                   | the help text                                         |

---

## 5. The GTK REPL (`repl/gtk/gtk_repl.c`)

The GTK front-end is described in `repl/gtk/README.md`.  The parts that
are interesting for the architecture:

* **the tabs are the files of the file system** that the GTK application
  provides (see section 2.1),
* **there is one session** for all the files, so they share the globals,
* **the compiler runs in its own thread** with its own fiber, because the
  semantic analyser may suspend itself; the GTK thread and the worker
  communicate through a command queue and an async queue,
* **the source buffer is parsed as a sequence of statements**, so
  declarations, expressions and comments can be mixed freely.

```
        GTK thread                          worker thread
   +------------------+                +---------------------+
   | GtkNotebook      |                | GtkTab_Fiber        |
   |  +------------+  |  command queue |  Repl_Init()         |
   |  | GtkTextView|  | -------------> |  Presemantic_()     |
   |  | (source)   |  |                |  parse statements    |
   |  +------------+  |                |  semantic analysis  |
   |  +------------+  |  async queue   |  codegen + run      |
   |  | GtkTextView|  | <------------- |  print the result   |
   |  | (transcript)| |                +---------------------+
   |  +------------+  |
   +------------------+
```

---

## 6. The bugs that were found while building the GTK REPL

They are listed here because they are typical for this kind of code and
because they are the kind of thing the architecture has to be robust
against.

### 6.1 An identifier was looked up with the wrong table

The scope tables are keyed with the identifiers of
`SemanticIdentifierTable` (a declaration copies its identifier into it,
see `MetaCSemantic_declSemantic` in `semantic/metac_semantic.c`), but the
`expr_type` case of the semantic analyser looked the name up with the
identifier of the *parser* table:

```c
metac_identifier_ptr_t idPtr = typeExpr->TypeIdentifier;   /* parser table */
metac_node_t node = MetaCSemantic_LookupIdentifier(self, idPtr);
```

The `expr_identifier` case does it right -- it copies the identifier
into the semantic table first.  The result was that `sizeof(x)` (where
`x` is a variable) did not find `x`, and the caller then dereferenced
the "empty node" (which is the pointer `0x1`).

### 6.2 An allocator was copied by value

`doDeclSemantic_PrintFunction` did

```c
metac_alloc_t tmpAlloc = ctx->Sema->TempAlloc;
```

A copy of an allocator shares the *array of the arenas* with the
original.  When the copy has to grow that array, `Allocator_AddArena`
calls `realloc()` on it -- which frees the array of the original
allocator and leaves it with a dangling pointer.  The same happened in
`compiler_GetEnumMembers` (`compiler_intrinsics/metac_compiler_interface.c`).

The fix is to use the original allocator (`&ctx->Sema->TempAlloc`)
instead of a copy.

### 6.3 The stacks of the libaco coroutines are not aligned

The default coroutine backend (`3rd_party/libaco`) does not keep the
16 byte alignment of the stack that the System V AMD64 ABI requires.
Every call into a function that uses SSE instructions without an aligned
stack (`printf()` for example) therefore crashes with `SIGSEGV` on a
current glibc.  The boost.context backend
(`-DMETAC_CORO_FCONTEXT`, `os/metac_coro_fcontext.c`) does not have the
problem, so the GTK REPL and the terminal REPL are built with it.

---

## 7. The build system

There are no makefiles; everything is a shell script that calls `$CC`.

| script                  | what it does                                          |
|-------------------------|-------------------------------------------------------|
| `gen_code.sh`           | runs the generators of `utils/` (the keyword tables and `compiler_intrinsics/metac_type_kind.[ch]`) |
| `run_tests.sh`          | the lexer test, the parser test and the coroutine and task system tests |
| `build_repl.sh`         | the terminal REPL                                     |
| `repl/gtk/build_gtk3.sh` | the GTK3 REPL                                       |
| `ci.sh`                 | `gen_code.sh && run_tests.sh && build_repl.sh`        |

The CI is in `.github/workflows/ci.yml` (Linux and Windows) and
`.github/workflows/tcc.yml` (the REPL built with TinyCC).

---

## 8. The X macros

The project uses the X macro idiom heavily: a list is defined once and
expanded into an enum, a switch and a table.

| macro                          | defined in                    | expands to                       |
|--------------------------------|-------------------------------|----------------------------------|
| `FOREACH_TOKEN(M)`             | `parser/metac_lexer.h`        | the token enum                   |
| `FOREACH_STATIC_TOKEN(M)`      | `parser/metac_lexer.h`        | the punctuation and the keywords  |
| `FOREACH_BINARY_TOKEN(M)`      | `parser/metac_lexer.h`        | the binary operators             |
| `FOREACH_KEYWORD_TOKEN(M)`     | `parser/metac_lexer.h`        | the keywords                     |
| `FOREACH_EXP(M)`               | `parser/metac_node.h`         | the expression kinds             |
| `FOREACH_BINARY_EXP(M)`        | `parser/metac_node.h`         | the binary expression kinds      |
| `FOREACH_UNARY_EXP(M)`         | `parser/metac_node.h`         | the unary expression kinds       |
| `FOREACH_STMT_KIND(M)`         | `parser/metac_node.h`         | the statement kinds              |
| `FOREACH_DECL_KIND(M)`         | `parser/metac_node.h`         | the declaration kinds            |
| `FOREACH_NODE_KIND(M)`         | `parser/metac_node.h`         | the node kind enum               |
| `FOREACH_PREPROC_DIRECTIVE(M)` | `parser/metac_preproc.h`      | the preprocessor directives      |
| `FOREACH_SCOPE_OWNER(M)`       | `semantic/metac_scope.h`      | the scope owners                 |
| `FOREACH_TYPE_INDEX_KIND(M)`   | `semantic/metac_type.h`       | the type kinds                   |
| `FOREACH_TYPE_TABLE(M)`        | `semantic/metac_semantic.h`   | the nine type tables             |
| `FOREACH_TABLE_MEMBER(M)`      | `semantic/metac_type_table.h` | the (type, member, compare) triples |

See `X_MACROS.md` for the details.

---

## 9. The identifiers that stitch the layers together

| type                    | what it is                                            |
|-------------------------|-------------------------------------------------------|
| `metac_identifier_ptr_t`| an interned string (a handle into an identifier table) |
| `metac_location_ptr_t`  | an index into the location storage                    |
| `metac_type_index_t`    | a packed (kind, index) handle of a type                |
| `metac_scope_owner_t`   | a packed (index, kind) handle of a scope                |
| `metac_storage_location_t` | a packed (kind, offset) handle of a storage         |
| `BCValue`               | the universal runtime value                            |
| `BackendInterface`      | the vtable that separates the code generator from the backends |

---

## 10. A glossary of the directories

| directory          | what is in it                                              |
|--------------------|------------------------------------------------------------|
| `parser/`          | lexer, preprocessor, parser, parse tree, identifier tables  |
| `semantic/`        | scopes, types, the semantic analyser, the semantic tree     |
| `codegen/`         | the code generator and the variable store                   |
| `driver/`          | `metac_lpp_t` (lexer + preprocessor + parser) and the driver |
| `libinterpret/`    | the four backends and the bytecode value model              |
| `os/`              | the OS abstraction, the allocator, the tasks and the fibers  |
| `repl/`            | the REPL and its front-ends                                 |
| `printer/`         | the pretty printer of the parse and the semantic tree       |
| `utils/`           | the generators of the keyword tables                        |
| `hash/`            | crc32c                                                     |
| `compiler_intrinsics/` | the compiler interface (`metac_compiler_t`) and the generated type kinds |
| `make_header/`, `make_tostring/` | helpers that generate C headers         |
| `tools/`           | `dump_bytecode` and friends                               |
| `debug/`           | the debug server                                           |
| `test/`, `tests/`  | the test suites                                            |
| `website/`         | the browser game that speaks a subset of MetaC             |
| `3rd_party/`       | libaco, boost.context, lightning, linenoise, fpconv        |
