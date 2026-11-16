# The REPL Test Cases

This directory contains the test cases for the MetaC REPL, and a runner
that feeds them to the REPL and checks that it survives them.

```
    sh tests/repl/run_tests.sh [path-to-repl]
```

The runner is part of `run_tests.sh` and therefore part of `ci.sh`.

---

## 1. Why these exist

The REPL is the only program in the repository that runs the *whole*
compiler pipeline — lexer, parser, semantic layer and code generator — on
input that was not known when the compiler was built. Everything else
(`parser_test`, `lexer_test`, the semantic unit tests) stops before the
parts that break.

The test cases here were written for one reason: **each of them reproduces
a crash or a corruption that the REPL used to have.** They are the
regression tests for those bugs.

---

## 2. How the cases work

A test case is a file whose name encodes the mode the REPL has to run in:

| suffix | REPL mode | what it does |
|---|---|---|
| `.ds.mtc` | `:ds` | parse a declaration and run the declaration semantic |
| `.ss.mtc` | `:ss` | parse a statement and run the statement semantic |
| `.ee.mtc` | `:ee` | parse and evaluate an expression |

The runner pipes the case into the REPL, adds the `:q` that quits it, and
checks three things:

1. the REPL exits with the code it uses for a normal quit (1),
2. the output contains no assertion failure,
3. every line of the companion file `<name>.expect` appears in the output.

### Expected failures

A case that is known to fail has a companion file `<name>.xfail` that
explains why. The runner reports such a case as

* **XFAIL** when it fails as expected — this is not an error, and
* **XPASS** when it unexpectedly succeeds — this *is* an error, because it
  means the bug was fixed and the marker has to be removed.

That way a known bug cannot silently change into an unnoticed one.

---

## 3. The test cases

### `basic.ds` — the smoke test

A function, a global variable, a struct and an enum. If this fails,
nothing else is worth looking at.

### `arrays.ds` — arrays with dimensions and initialisers

```c
int globalArray[10];
int initialised[4] = { 1, 2, 3, 4 };
struct WithArray { int data[8]; };
```

Covers the array type semantic and the initialiser list.

### `expression_eval.ee` — expression evaluation

```c
1 + 2
3 * 4
10 / 3
sizeof(int)
typeof(1 + 2)
```

Covers the evaluation mode, which is the only path that runs the code
generator.

### `nested_scopes.ss` — statements and scopes

```c
int x = 1;
if (x > 0) { int y = 2; } else { int z = 3; }
while (x < 10) { x = x + 1; }
for (int i = 0; i < 10; i = i + 1) { }
{ int shadow = 5; }
```

Covers the statement semantic: blocks, conditionals, loops and shadowing.

### `big_struct_200_fields.ds` — the completion trie corruption

```c
struct Big { int field0; int field1; ... int field199; };
```

**This is the case that made the REPL crash.**

Every field name is registered in the completion trie. The trie used to
keep the children of a node in a contiguous block, and the block was copied
whenever it grew — which moved the children and invalidated every index
that was held elsewhere. The root's block of 53 pre-allocated identifier
characters was the first to overflow, so the REPL died with

```
    repl: ../repl/completion_trie.c:390: CompletionTrie_AddChild:
        Assertion `root->Nodes[0].ChildCount != 0' failed.
```

after roughly 53 identifiers.

The trie now stores the children as a linked list of indices and never
moves a node, so the case passes. See `repl/COMPLETION_TRIE.md`.

### `big_struct_150_fields.ds` — the same with doubles

150 `double` members instead of 200 `int` members, to make sure the fix is
not specific to one member type or one size.

### `many_identifiers.ds` — the scope table overflow

500 global variables.

**This is the case that found the scope table bug.**

The scope of the REPL session is a hash table with 32 slots, and the
semantic layer never grew it. The 33rd identifier could not be inserted
any more and the compiler aborted with

```
    ../semantic/metac_semantic.c:935: MetaCSemantic_RegisterInScope:
        Assertion `!"Insertion failed\n"' failed.
```

`MetaCScopeTable_Insert` now grows the table and rehashes its entries
when it runs out of slots (semantic/metac_scope.c).

### `overlapping_identifiers.ds` — identifiers with overlapping prefixes

```c
int cl;
int clot;
int clo;
int compiler;
int compilerP;
int prefix;
int prefixsuffix;
int pre;
```

**This is the case for the trie split.**

`cl` is a prefix of `clot`, and `prefix` is a prefix of `prefixsuffix`. A
radix tree has to split the node that holds the longer prefix so that both
words can be stored. The old implementation always appended a child, so
the tree ended up with two children that started with the same character
and the search became ambiguous.

### `big_enum_100_members.ds` — an enum with many members

```c
enum BigEnum { member0, member1, ... member99 };
```

**This is the case for the stack overflow.**

The enum semantic keeps its members in a stack array of 32 elements and
asks `ARENA_ARRAY_ENSURE_SIZE` for room for more. That macro consulted an
arena pointer that does not exist for a stack backed array, so it never
grew the array and the members were written past the end of the stack
buffer. There is now a dedicated `STACK_ARENA_ARRAY_ENSURE_SIZE` that
moves the array to the heap when it does not fit any more
(os/metac_alloc.h).

### `many_params_100.ds` — a function with many parameters

```c
int manyParams(int p0, int p1, ... int p99) { return p0; }
```

The same stack array overflow, this time for the parameter types
(`STACK_ARENA_ARRAY(metac_type_index_t, parameterTypes, 64, …)`).

### `template_instance.ds` — a template instance

```c
Pair!(int, float) thePair;
```

A template *instance* already works. The declaration of a template
aggregate does not — see below.

### `template_declaration.ds` — **XFAIL**

```c
struct Pair!(T, U) { T first; U second; };
```

The template system is not implemented yet; declaring a template aggregate
crashes the compiler. The plan for it is in `TEMPLATE_ROADMAP.md`.

### `union.ds` — **XFAIL**

```c
union Value { int asInt; double asDouble; };
```

Unions are not implemented by the semantic layer either: the
`case type_union:` branch in `semantic/metac_type_semantic.c` is empty, so
`MetaCSemantic_TypeSemantic` returns 0 and trips

```
    ../semantic/metac_type_semantic.c:1523: MetaCSemantic_TypeSemantic:
        Assertion `result.v != 0' failed.
```

This one behaves the same on the unmodified compiler.

---

## 4. What the cases found

The test cases were written after the refactoring work and immediately
paid for themselves:

| found by | defect | fix |
|---|---|---|
| `nested_scopes.ss` | the chunked semantic arrays were initialised but not *reserved*, so writing to `Body[i]` wrote to a null pointer | `ARENA_CHUNK_ARRAY_RESERVE` after every `INIT_SZ` that is filled by index (semantic/node_alloc.c, semantic/metac_semantic.c) |
| `many_identifiers.ds` | the scope table never grew, so the REPL could only hold 32 identifiers | `MetaCScopeTable_Grow` rehashes the entries into a bigger table (semantic/metac_scope.c) |
| `big_enum_100_members.ds`, `many_params_100.ds` | `ARENA_ARRAY_ENSURE_SIZE` on a stack backed array wrote past the end of the stack buffer | `STACK_ARENA_ARRAY_ENSURE_SIZE` spills to the heap (os/metac_alloc.h) |
| `big_struct_200_fields.ds` | the completion trie corrupted itself after ~53 identifiers | the children became a linked list of indices (repl/completion_trie.c) |

The first one is the interesting one: it was a regression of the
refactoring itself. The chunked array that the semantic layer was migrated
to addresses its elements by index, and an index is only valid for an
element that exists — an array that is initialised but not reserved has no
elements yet. Nothing in the compiler noticed, because the statement
semantic path is only exercised by the REPL.

---

## 5. Adding a case

1. Write `<name>.<mode>.mtc` with one declaration or statement per line.
2. If the case should print something, write `<name>.expect` with one
   expected string per line.
3. If the case is known to fail, write `<name>.xfail` with the reason and
   the assertion that fires.
4. Run `sh tests/repl/run_tests.sh`.

The mode of a case matters: a declaration has to be run in `:ds`, a
statement in `:ss` and an expression in `:ee`. Running a declaration in
`:ss` mode does not exercise the declaration semantic at all.
