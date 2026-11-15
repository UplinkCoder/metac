# MetaC — a code map, and an assessment

*Written after a full read of `parser/`, `semantic/`, `codegen/`, `os/` and a
survey of `libinterpret/`, `repl/`, `driver/`, `printer/`.*

This document has two parts:

1. **How I read the codebase** — the actual steps I took, in order, including
   the places where I was wrong and had to back up. This is the part to read
   if you want to check my conclusions or reuse the method.
2. **What the code is, what the ideas are worth, and what the author's skill
   level looks like** — with evidence.

Line numbers refer to commit `5cc5df4` plus my own working changes.

---

# Part 1 — The exploration log

## Step 0 — Orient by the artefacts, not the code

Before reading a single `.c` file I looked at what the author chose to leave
in the repository, because that is usually more honest than the code itself:

| artefact | what it told me |
|---|---|
| `README.md` | The stated goal is *C99 compatibility plus meta-programming*. It advertises "types are expressions", `_yield`, and a compiler-API reachable from the compiled program. |
| `PROJECT_DESCRIPTION.md` | A module-by-module overview — and it ends with a section literally titled **"AI-Generated Insight & Synthesis"**. So the top-level documentation is partly LLM-written; the code is not. |
| `session-ses_*.md` (3 files, 1.7 MB) | AI assistant session transcripts, committed. The author *works with an AI assistant* on this codebase, and keeps the transcripts. That is a working-style fact worth knowing before judging the code. |
| `TODO`, `x.txt`, `speller.txt`, `scheduler.txt` | Scratch notes kept in-tree. The project is developed in the open, with the mess visible. |
| `.github/workflows/ci.yml` | CI runs `gen_code.sh && run_tests.sh && build_repl.sh` — so the REPL is considered part of the product, not a toy. |
| `3rd_party/` | `libaco` (coroutines), `deboost.context` (coroutines, alternative), `lightning` (JIT), `minicoro`, `tinycthread`, `tracy` (profiler), `dyncall`. The author vendors infrastructure rather than depending on a distro. |

**Conclusion:** this is a long-lived solo project (951 commits, 2022→2026,
author Stefan Koch), not a weekend toy. The README's own disclaimer — "not
currently finished" — matches what I found later.

## Step 1 — Build the pipeline map from the includes, not from the docs

Rather than reading files top to bottom, I resolved the `#include` graph to
find the real entry points, because this project uses a *unity build* style
(everything `#include`s everything):

```
repl/linenoise_repl.c
  └─ repl/repl.c
       ├─ os/metac_task.c      ← the task system (coroutines + worker threads)
       │    └─ os/metac_task.h → os/metac_coro.h → 3rd_party/libaco/aco.h
       │                        └─ os/metac_coro.c → libaco/aco.c + acosw.c
       ├─ parser/metac_parser_obj.c
       │    ├─ parser/metac_parser.c   (the parser proper)
       │    │    └─ parser/metac_expr_parser.c  (included, not separate)
       │    └─ printer/metac_printer.c
       ├─ semantic/metac_semantic_obj.c
       │    ├─ semantic/metac_semantic.c
       │    │    ├─ semantic/metac_type_semantic.c
       │    │    ├─ semantic/metac_expr_semantic.c
       │    │    └─ semantic/metac_sematree.c
       │    └─ os/metac_coro.c
       ├─ codegen/metac_codegen.c
       │    └─ libinterpret/bc_common.c, bc_interpreter_backend.c,
       │       printer_backend.c, codegen/metac_vstore.c   ← all #included
       └─ semantic/handoff.c
```

**Two things fell out of this immediately:**

* The *only* place the whole pipeline is exercised is the REPL. There is no
  `main()` that compiles a `.metac` file to a binary. The REPL *is* the
  product.
* `codegen/metac_codegen.c` `#include`s the four backends directly. So
  "codegen" and "the interpreter" are one translation unit, and the
  `libinterpret/` directory is not a library — it is source that gets pasted
  into the compiler.

## Step 2 — Read the data structures before the algorithms

I read the type-related headers first, because in a compiler the headers
tell you what the author thinks the *world* looks like:

* `semantic/metac_type.h` — a type is a **single `uint32_t`**: 4 bits of kind
  + 28 bits of index (`metac_type.h:57-66`). Kinds: basic, enum, ptr,
  array, struct, union, class, map, functiontype, typedef, tuple, template.
* `semantic/metac_scope.h` — scopes are hash tables
  (`metac_scope_table_t`, `:88-97`) with a parent pointer; a scope can be
  *mounted* over another (`:114-127`), which is how struct tag scopes and
  temporary scopes work.
* `semantic/metac_sematree.h:42-57` — variable storage is **also** a packed
  `uint32_t`: 4 bits of storage kind + 28 bits of offset.
* `parser/metac_parsetree.h` — the parse tree uses a big `union` with
  one member per node kind, and every node carries `Hash`, `Serial` and a
  location index.

**Why this mattered:** the whole compiler is built on the idea that *types
are values*. A type is a `uint32_t` you can put in a variable, pass to a
function, compare. That idea explains most of the later design, so I kept
it in mind.

## Step 3 — The parser (where I started to suspect trouble)

I had already written `parser/type_parsing_diagnosis.txt` (a harness-driven
audit of ~80 declarations) before this exercise, so I went in knowing what to
look for, and confirmed it directly:

* `parser/metac_parser.c:1929-1981` is the special case: it recognises
  literally one declarator shape, `(* name ) ( params )`, and builds a
  function type by hand, including a hand-rolled hash
  (`CRC32C_VALUE(CRC32C_STAR, hash)`, `:1978`).
* Everything else about C declarators is either unsupported or crashes:
  `int (*p);` segfaults in `ParseParameterList` (`:949`), `int x[10][4];`
  nests the array dimensions in the *wrong order* (the array suffix loop at
  `:2037` wraps the previous type, so the first dimension ends up
  innermost), `int (*x)[10];` crashes.
* Storage classes and qualifiers are parsed and **thrown away**: `stc` is
  computed three times (`:1724`, `:1806`, `:1852`) and never written into
  any node, even though `DECL_HEADER` has a `StorageClass` field
  (`parser/metac_parsetree.h:380`).
* `MetaCParser_Match` prints an error and then **does not advance**
  (`:636-668`), so callers keep going with a stalled cursor; `ParseError`
  calls `ParseErrorBreak()`, which is `void ParseErrorBreak(void) { int k = 2; }`
  — a no-op. There is no error state on the parser.
* `assert()` is used on user input in ~30 places.

**Conclusion:** the parser is the weak link of an otherwise ambitious
system, and it is weak for a structural reason (it pattern-matches instead
of implementing a grammar), not because of carelessness.

## Step 4 — The semantic layer, in dependency order

I read it in this order, because each layer only makes sense once you know
the one below it:

1. `semantic/metac_scope.c` — the scope table is a hash table with
   displacement-based probing and a "mounted scope" union
   (`metac_scope.h:134-137`). Lookup walks up the parent chain
   (`metac_semantic.c:877-904`).
2. `semantic/metac_type_table.c` — the *type interning* tables. One table
   per kind (enum/array/aggregate/ptr/union/typedef/functiontype/tuple/
   template — `metac_semantic.h:68-77`). Insertion is lock-free: an atomic
   compare-exchange claims the slot (`:163`), the key is copied in, and the
   interned node is allocated (`:177-220`). Equality is structural
   (`EnumSlotsEqual`, `ArraySlotsEqual`, … `:33-130`).
   A nice trick lives at `:227-278`: `EntangleInts`/`UntangleInts` interleave
   the bits of two values into a single 32-bit hash (with a flag bit for the
   overflow case) — it is how the array type's `(element type, dimension)`
   pair is hashed (`metac_type_semantic.c:1619`).
3. `semantic/metac_type_semantic.c` — turns parse-tree types into
   `metac_type_index_t`. This is where the *layout* lives: a size computer
   (`:1650-1686`) computes member offsets with alignment, and
   `MetaCSemantic_ComputeStructLayout` (`:1688+`) walks the fields.
4. `semantic/metac_semantic.c` — the declaration driver
   (`MetaCSemantic_declSemantic`, `:1271-1389`): a switch over
   `decl->Kind`, and every branch ends in "resolve the type, copy the
   identifier into the semantic table, register it in the scope".
5. `semantic/metac_expr_semantic.c` — the expression analyser. The
   interesting part is not the switch but the *failure model*:

   * A name that cannot be resolved yet does **not** fail. The analyser
     registers a *waiter* (`metac_semantic_waiter_t`,
     `metac_semantic.h:39-46`) keyed by (function hash, node hash) and
     **yields its fiber** (`semantic/metac_type_semantic.c:1720-1730`,
     `semantic/metac_expr_semantic.c:1287-1294`). Another task later
     resolves the name and hands control to the waiting fiber with
     `aco_yield2(waiter.Continuation)` (`metac_type_semantic.c:1595`).
   * There is an **8-way SIMD LRU cache** in front of the scope lookup
     (`metac_semantic_lru.h`, `metac_semantic.c:844-873`) — an
     `int16x8_t` of 10-bit hashes compared with `Eq16`/`MoveMask16`. This is
     micro-optimisation well past what a hobby compiler usually does.
   * Constant evaluation is done by **compiling the expression to bytecode
     and running it** (`EvaluateExpr`, `metac_expr_semantic.c:124-251`):
     it first compiles with the *printer* backend to dump a listing
     (`:147-165`), then compiles again with the bytecode backend and
     executes it (`:167-185`), then converts the raw interpreter value back
     into a semantic node (`:196-244`).

**The constant evaluator is the single most interesting thing in the
codebase.** It means the compiler reuses its own backend to implement
`#if`-style constant folding, and it means the REPL's "evaluate this
expression" is literally "compile and run it".

## Step 5 — The code generator (delegated, then verified)

The generator is 2090 lines, so I had an agent read it in full and report a
technical map with line references, then I checked its claims against the
code myself. Key findings I verified by hand:

* The generator is **backend-agnostic**: it never emits bytes, it calls a
  vtable of ~90 function pointers (`libinterpret/backend_interface_funcs.h`).
  Four backends exist — bytecode interpreter, liblightning JIT, a textual
  printer, and a JavaScript emitter.
* Control flow is **labels plus forward fixups** with a *sticky condition
  flag*: `BeginJmp`/`EndJmp` and `BeginCndJmp`/`EndCndJmp`
  (`codegen/metac_codegen.c:1985-2075`). A conditional jump to a label
  with no condition value compiles to `JmpTrue`/`JmpFalse`, which test the
  interpreter's last comparison result.
* `switch` is implemented by comparing against the switch expression and
  then relying on that sticky flag (`:1817-1823`) — clever, and it is why
  `&&`/`||` **cannot short-circuit** (both operands are evaluated eagerly,
  `:1254-1280`).
* Storage is index-based (`STORAGE_V(storage_local, nLocals++)`,
  `metac_semantic.c:1320-1326`), and the backend turns those indices into
  stack slots. There is no register allocator; the "registers" are a
  512-entry `int64_t` array in the interpreter.
* `codegen/metac_vstore.c` is a *vestige*: a name→value map that codegen
  writes to (`:676`, `:1905`) but never reads from.

## Step 6 — Trace one feature end to end

I traced `int x = 42;` through all three stages, because a single vertical
slice exposes more than three horizontal reads:

1. **Parse** (`parser/metac_parser.c:1870-2063`): `ParseTypeDecl` produces a
   `decl_type` with `TypeKind = type_int`; the declarator branch at `:1982`
   produces a `decl_variable` with `VarType` and `VarIdentifier`; the
   initializer is parsed at `:2058-2062`. The variable's hash is computed
   at `:2035` — *before* the array-suffix loop at `:2037`, which is why
   `HashDecl` later disagrees for arrays.
2. **Semantics** (`semantic/metac_semantic.c:1294-1335`): `decl_variable` →
   `AllocNewSemaVariable`, the type is resolved to a
   `metac_type_index_t`, the identifier is *copied* from the parser's
   identifier table into the semantic one (`:1304`), the initializer is
   analysed (`:1311`), and the storage is assigned
   `STORAGE_V(storage_local, nLocals++)` or `storage_global` (`:1320-1329`).
3. **Codegen** (`codegen/metac_codegen.c`): `doGlobal` (`:285-326`) assigns a
   heap address, evaluates the initializer, and bumps
   `GlobalMemoryOffset`; a local goes through `doLocalVar` (`:1892-1912`).

**Where it breaks:** `MetaCCodegen_AccessVariable` for a *global* is
`assert(!"Not implemented")` (`:795`), and so is the assignment path
(`:1425`, `:1725`). So a global variable can be *declared* but not *read*.

## Step 7 — Sanity-check the claims by running things

I did not trust the read alone:

* The REPL crashes in `MetaCCodegen_AccessVariable` on `:ee x` after `:ds
  int x = 42;` — **with libaco and with the boost.context backend
  identically**, so it is a pre-existing codegen gap, not something I
  introduced.
* `run_tests.sh` passes; the parser test is three declarations
  (`parser/metac_parser.c:2805-2831`).
* I built the REPL with both coroutine backends and compared the output.

## Step 8 — Quantify

```
66 kLOC   own code (parser 11.5k, semantic 9.2k, libinterpret 10k,
                       codegen 2.4k, os 3.8k, repl 3.5k, printer 2.8k)
123 kLOC   vendored 3rd_party code
951 commits, 2022-04 → 2026-11, ~96% by one author
626/132/64/40/89 commits per year 2022/2023/2024/2025/2026
```

---

# Part 2 — The assessment

## 2.1 What the system actually is

MetaC is not "a C compiler that also does meta-programming". It is a
**meta-circular compiler**: a compiler whose *own* types are first-class
values in the language it compiles, whose constant evaluator is its own
backend, and which exposes itself to the compiled program through the
`.Compiler` object (`compiler_intrinsics/metac_compiler_interface.h`).

The pipeline is:

```
source ─ lexer ─ parser ─ parse tree ─ semantic ─ sema tree ─ codegen ─ backend
                     │            │                                  ├ bytecode
                     │            │                                  ├ JIT (lightning)
                     │            │                                  ├ printer listing
                     │            │                                  └ JavaScript
                     │            └ types are values (uint32_t)
                     └ compile-time evaluation = compile + run
```

## 2.2 The ideas, judged

### Good ideas, well executed

| idea | where | verdict |
|---|---|---|
| **Types as 32-bit values.** A type is a `uint32_t` you can hold in a variable. | `semantic/metac_type.h:57-66` | The strongest idea in the codebase. It makes `typeof`, type parameters, and the compiler API fall out almost for free, and it is what makes the whole "meta" story coherent rather than bolted on. |
| **Type interning with structural equality.** Nine specialised hash tables, one per type kind, lock-free insertion. | `semantic/metac_type_table.c` | Correct and idiomatic. Interning is how you make type identity cheap; the per-kind tables keep the comparisons trivial. |
| **Packed storage descriptors.** Kind+offset in one `uint32_t`. | `semantic/metac_sematree.h:42-57` | Same trick as the type index. Consistent, and it keeps the sema tree small enough to matter for a compiler that wants to be fast. |
| **Compile-and-run constant evaluation.** The analyser compiles the expression to bytecode and executes it to fold constants. | `semantic/metac_expr_semantic.c:124-251` | Genuinely clever, and it means there is exactly one evaluator instead of two (one in the analyser, one in the codegen). The cost is that the analyser is now hostage to the codegen's completeness — which is exactly why `:ee x` on a global crashes today. |
| **Deferred name resolution via fibers.** An unresolvable name registers a waiter and yields the fiber; whoever resolves the name later hands control back. | `metac_type_semantic.c:1720-1730`, `metac_expr_semantic.c:1595` | The most ambitious design decision in the project: name resolution becomes a *continuation* rather than an error. It is the natural way to support mutual recursion and forward references in a compiler that wants to be incremental and concurrent. It is also, not coincidentally, the hardest thing in the codebase to get right — see §2.3. |
| **Backend abstraction.** The generator emits *operations* (`Add3`, `Set`, `BeginCndJmp`) into a vtable; four backends consume them. | `codegen/metac_codegen.c`, `libinterpret/` | Good separation. It is what allows the printer backend to exist, which in turn is what makes the compile-and-run evaluator debuggable. |
| **Arena allocator with parent/child allocators.** | `os/metac_alloc.c/h` | The right tool for a compiler: per-phase allocation, bulk release, no malloc/free traffic in the hot paths. |
| **Coroutines as a first-class OS primitive**, with the scheduler on top. | `os/metac_task.c`, `os/metac_coro.c` | Unusual to see in a language project and it enables the deferred-resolution design above. |
| **Identifier interning by CRC32C + length.** | `parser/metac_identifier_table.c` | Cheap, and the hash is also used as the identity key in the scope tables. |
| **X-macro node definitions.** | `parser/metac_node.h`, `semantic/metac_semantic.h:19-25` | The node kinds are declared once and used to generate the enums, the unions, the printers and the walkers. This is the single biggest structural quality in the codebase — it is why adding a node kind is a one-line change in the right place. |

### Ideas that are ambitious but not yet earned

* **The class type kind.** `type_index_class` exists in the enum
  (`metac_type.h:18`) and the codegen asserts on it
  (`metac_codegen.c:149`). It is a placeholder for a feature that was never
  built.
* **Templates.** `type_index_template`, `T!(args)`, `S!(T)` — the syntax
  parses (`parser/metac_parser.c:1082`, `:1185`) and there is a template
  type table, but the semantics are thin. Templates in a language with
  first-class types are a large design problem, and the author has clearly
  deferred it.
* **Fibers as the substrate for the analyser.** The task system's scheduler
  (`os/metac_task.c:313-509`) is intricate and, as the source itself
  admits with its `printf("Grabbing Fiber: %u\n")` debugging left in, is
  still being stabilised.
* **The `AT(...)` attribute annotations.** `AT(TaskLocal)`, `AT(transient)`,
  `AT(per_function)` are declared throughout the sema state
  (`metac_semantic.h:116-153`) but `#define AT(...)` expands to nothing
  (`metac_semantic.h:15-17`). They are documentation pretending to be code.

### Ideas that are wrong or unfinished

* **The parser.** See §2.3.
* **`&&`/`||` do not short-circuit** (`metac_codegen.c:1254-1280`) — both
  operands are always evaluated. This is a direct consequence of the sticky
  condition flag and it is a semantic bug that will bite.
* **Globals cannot be read** (`metac_codegen.c:795`) — `assert(!"Not
  implemented")` on the *read* path of a global variable.
* **`continue` is not implemented** in codegen (`ctx->Continues` is declared
  at `metac_codegen.h:95` and never used; `stmt_continue` falls into the
  "Stmt unsupported" `assert(0)`).
* **The `metac_vstore` is dead weight** — a whole name→value store that
  codegen writes to and never reads, plus a full `#if 0`-ed duplicate of
  the expression evaluator in `repl/exp_eval.c`.

## 2.3 The single most important structural criticism

The parser is the project's bottleneck, and the reason is structural rather
than stylistic: **it pattern-matches declarators instead of implementing the
grammar**. One special case for `(* name ) ( params )` forced a second
special case for the typedef of a function pointer (a synthetic
`decl_variable_t` smuggled through `parser/metac_parser.c:1883-1907`), and a
third for the storage class that the parser kept throwing away.

Everything downstream then has to compensate:

* `HashDecl` has to *recompute* a hash that three different code paths
  built by hand, and asserts when they disagree (`metac_parser.c:1630-1633`).
* The codegen has to model "a function pointer" as "a function type with a
  `*` in its hash" because the parse tree cannot say "pointer to function".
* The semantic layer cannot tell a function pointer from a function.

A recursive declarator parser — ~400 lines, specified in
`parser/decl_type_parsing_plan.md` — would delete a special case, delete a
synthetic-node hack, and make the whole type system representable. The
design work has been done; it just has not been applied.

## 2.4 The author's skill

I want to be specific and evidence-based rather than flattering.

**What the code demonstrates (strong):**

* *Systems C at a high level.* Arena allocators with parent/child
  lifetimes, hand-written coroutine libraries, a JIT backend, a bytecode
  interpreter, lock-free hash tables with atomic CAS insertion, bit-packed
  32-bit handles for types and storage, and an SSE2-assisted 8-way LRU for
  identifier lookup. This is not "I read the C book" code; this is code
  written by someone who has internalised how real engines are built.
* *Compiler architecture understanding.* The interning/type-table design,
  the separation of the semantic tree from the parse tree, the backend
  abstraction, the decision to evaluate constants by reusing the backend —
  these are the right answers, chosen for the right reasons.
* *Language design imagination.* Making types first-class values, giving
  the compiled program access to the compiler (`GetTypeKind`, `GetMembers`),
  and `inject`/`eject`/`$()`/`#()` are coherent ideas, not a pile of
  keywords.
* *Sustained effort.* 951 commits over 4.5 years with a long tail, a
  working CI, a REPL that is actually used as the primary interface, and
  vendored infrastructure maintained in-tree.
* *Intellectual honesty in the code.* Comments like "XXX FIXME we want to
  assign a variable serial after we have determined the storage location
  ideally", "this is wrong!", and the explicit "DO NOT CHANGE THE ORDER"
  markers show the author knows exactly which invariants are fragile.

**What the code demonstrates (weak):**

* *Input discipline.* `assert()` on user input in ~30 places in the parser
  alone; `Match` that reports and then continues; a `ParseErrorBreak` that
  does nothing. A compiler that aborts on `int (*p);` cannot be used by
  anyone but its author.
* *Test coverage.* Three declaration tests for a 66 kLOC compiler. The
  parser's own `TestParseDecl` covers `int f(double x)`, `unsigned int x`
  and `signed int y`. Nothing in the suite would notice if the type system
  stopped interning.
* *Cross-component invariants.* The clearest example: the semantic layer
  allocates local-variable indices with a counter that is never reset per
  function (`metac_semantic.c:1318-1322`, with the TODO admitting it),
  while codegen re-creates the locals array per function
  (`metac_codegen.c:700-703`). Two components disagree about what a local
  index means, and no test catches it.
* *Completion discipline.* Dead fields, an unused `metac_vstore`, a
  `#if 0`-ed duplicate evaluator, `eval_func.c` calling
  `MetaCCodegen_RunFunction` with an outdated signature, the leftover
  `printf("Grabbing Fiber: %u\n")` in the scheduler.
* *Consistency between the "meta" story and the implementation.* The
  README advertises C99 compatibility; the parser cannot parse most of C99
  declarator syntax. The gap is well understood by the author (the
  diagnosis document is a thorough self-audit), but understanding a gap and
  closing it are different skills.

**Overall judgement.** This is the work of an **advanced systems programmer
who is architecturally ahead of the typical compiler hobbyist and
engineering-wise behind a professional codebase** — somewhere around senior
infrastructure engineer on the C side, mid-level on the "shipped software
that other people depend on" side. The *ideas* are better than the average
hobby compiler's; the *discipline* (tests, error handling, invariants
across layers, cleanup) is where the project loses points. And that gap is
the interesting part, because it is a discipline gap, not an ability gap —
and discipline gaps are the kind you close with a test suite and a rule
about `assert`s, not with a redesign.

There is also a meta-observation worth making: the repository contains
1.7 MB of AI-assistant session transcripts and a top-level document with an
LLM-written synthesis section. The author is already using tooling to
accelerate. The gap between "can produce code fast with an assistant" and
"can produce code that is *maintained*" is precisely the discipline gap
above — and the most valuable next step is not more features, but turning
the assistant into a maintainer: tests first, diagnostics instead of
asserts, and invariants checked at the boundary between components.

## 2.5 If I were the author, the order would be

1. **Make failure possible.** An error flag on the parser, diagnostics
   instead of `assert()` on user input. This unblocks everything else,
   because until the parser can fail, no test can assert on a failure.
2. **Replace the declarator special case with the grammar.** It deletes
   three hacks at once and makes function pointers representable.
3. **Grow the test suite from the diagnosis.** The `type_parsing_diagnosis`
   harness already contains ~80 inputs with expected outcomes — that is a
   test suite waiting to be written.
4. **Then the semantic layer.** With a parser that can fail, the deferred
   resolution / waiter machinery becomes debuggable, and the "not
   implemented" gaps in codegen (globals, `continue`) become reportable
   errors instead of aborts.

The order matters: each of these makes the next one possible, and none of
them requires a redesign.
