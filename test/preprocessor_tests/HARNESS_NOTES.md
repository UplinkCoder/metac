# Preproc Harness — Working Notes

Status: **design settled, harness not yet functional** (draft `preproc_test.c`
still contains a placeholder `PrintTokenText`). This file documents the
reasoning so the work can be picked up consistently.

## Goal

Put `parser/metac_preproc.c` into the tiny preprocessor tests in this
directory (`test_macros.c`, `test_conditionals.c`, `test_complex.c`,
`test_undef_error.c`, plus `header.h`) and make them pass, without changing
the minimalistic style of the tests or restyling the preproc code.

## Why the current run_tests.sh is broken

The existing test driver does:

```sh
gcc -P "$file" -o "preprocessed_results/${file%.c}.preprocessed"
```

`-o` makes gcc **compile and link**; the `.preprocessed` files on disk are
ELF executables, not preprocessed text. The real reference output we should
compare against is:

```sh
gcc -E -P "$file"
```

The comparison should be done **whitespace-stripped** (`tr -d '[:space:]'`
on both sides): this makes the check independent of spacing/newlines and
still catches missing, extra or wrongly-expanded tokens. System includes
(`<stdio.h>` in `test_includes.c`) will not be expanded by the harness, so
the run script needs a stub include dir (empty `stdio.h`) so gcc's
reference is produced under the same rule.

## Build recipe

The amalgamation that pulls in `parser/metac_preproc.c` is
`parser/metac_parser_obj.c`. It compiles **without** `-DNO_PREPROCESSOR`
using the same flags the parser test uses otherwise:

```sh
cc -Iparser \
   -DNO_DOT_PRINTER -DNO_SEMANTIC -DNO_FIBERS -DOLD_PARSER=1 \
   test/preprocessor_tests/preproc_test.c -o test/preprocessor_tests/preproc_test
```

(`-DOLD_PARSER=1` avoids the second-generation expr-parser code in
`metac_expr_parser.c` that doesn't compile; `-DNO_PREPROCESSOR` must NOT be
defined — that's the difference to `run_tests.sh` / `parser/run_tests.sh`,
which build the parser test with the preprocessor stubbed out.)

## How metac's preprocessor is wired (findings)

- Directive *name* parsing and dispatch hooks live in `metac_parser.c`:
  `MetaCParser_ParsePreprocDirective` (metac_parser.c:742). The
  `ParseDecl` path (metac_parser.c:~1736) calls it when it sees `#` and
  then calls the right `MetaCPreProcessor_*` function.
- Macro expansion in ordinary text is driven by
  `MetaCParser_HandleIdentifier` (metac_parser.c:336): it looks the
  identifier up through `DefineIdentifierTable`/`GetDefine`, collects
  arguments from the (raw or define-stack) stream, and pushes the
  expansion with `MetaCPreProcessor_PushDefine`
  (metac_preproc.c:22). Consuming pushed tokens happens via
  `MetaCPreProcessor_NextDefineToken` / the `DefineTokenStack`
  machinery in `MetaCParser_NextToken`.
- The lexer emits explicit `tok_newline` tokens (used as directive /
  define-body terminators) and eats `\`-continuations inside define
  bodies via the `InDefine` mechanism (metac_lexer.c:921).
- `MetaCPreProcessor_EvalExp` is `static` in metac_preproc.c; the public
  wrapper is `MetaCPreProcessor_Eval` (metac_preproc.c:961), which parses
  with `MetaCParser_ParseExpr(expr_flags_pp)` and evaluates.

## Harness design (settled)

`test/preprocessor_tests/preproc_test.c`, usage
`preproc_test <file.c>` (prints preprocessed token stream to stdout,
exit code 0 on success):

1. **Silence chatter**: the lib prints debug noise through `printf`/
   `xprintf` (os/compat.h:6). Harness `dup(1)`, `freopen("/dev/null","w",stdout)`,
   collect output in a buffer, write it once at the end via `write()`.
2. **File ctx stack** for `#include "header.h"`: each include is lexed
   into the *same* lexer (shared identifier/string tables, so tokens stay
   valid). Because lexing appends at the end of the token array, walking
   uses a stack of `{cursor, end}` ranges so order follows *directive*
   order, not lexing order. `parser->CurrentTokenIndex` is resynced to the
   ctx cursor before calling parser machinery and resynced back after
   (parser machinery only ever moves that one index).
3. **Conditional stack** `#ifdef/#ifndef/#if/#elif/#else/#endif` with a
   raw skip-walk for inactive branches (tracks nested `#if` depth, drops
   other directives inside skipped branches, returns control to the main
   flow at the depth-0 `#elif/#else/#endif`).
4. **Own tiny `#if` evaluator** (numbers, identifiers, `defined(X)`,
   `&& || == != > < ( )`) reading raw tokens — avoids fighting
   `MetaCPreProcessor_EvalExp`'s peek-time expansions. Object-like macros
   are resolved recursively through `MetaCPreProcessor_GetDefine`.
5. **Macro expansion**: stream = raw cursor when
   `DefineTokenStackCount == 0`, top of `DefineTokenStack` otherwise.
   Function-like macros expand only when `(` positionally follows the name
   (`name.Position + LENGTH_FROM_IDENTIFIER_KEY(name.IdentifierKey) ==
   lParen.Position` — same rule as ParseDefine metac_preproc.c:726).
6. **Stringize (`#x`) and paste (`a##b`) at stream level**: the stream
   keeps the previously emitted token; `#`-followed-by-token → print
   `"<token>"` quoted; `##`-between-tokens → merge spellings into one
   new identifier (registered in `preproc.IdentifierTable`). This
   avoids changing `PushDefine`.
7. **`#undef`** implemented in the harness (zero the matching slots in
   `DefineTable` and `DefineIdentifierTable`). Good enough for the
   single-undef test; mid-cluster holes are tolerated because nothing
   else is defined in that test.
8. **Token printer**: resolves identifier chars by trying
   lexer->IdentifierTable, preproc->IdentifierTable,
   preproc->DefineIdentifierTable and validating the text with
   `crc32c_nozero(~0, chars, len)` against the low-20 bits of the token
   key — robust against paste-created tokens living in a different
   table. Comments and newlines dropped/collapsed.
9. **run_tests.sh**: regenerate references with `gcc -E -P -Istub_inc`
   (stub dir with an empty `stdio.h`), build the harness, run each test,
   compare whitespace-stripped.

## Known bugs / gaps to fix in the preproc (minimal diffs)

1. **`DefineTokenIndexStack` never initialized on push**
   (`MetaCPreProcessor_PushDefine`, metac_preproc.c:88 only appends the
   token array). Consumers read `DefineTokenIndexStack[stackIdx]`
   (metac_parser.c:458 etc.) → garbage index on first push. Fix: also
   set `DefineTokenIndexStack[stackIdx] = 0` (or same for all pushes if
   nested).
2. **va_args infinite loop**: `va_args_idx` never incremented in the
   `__VA_ARGS__` loop (metac_preproc.c:71). Not needed by current tests
   but a one-line fix.
3. **Peek-time auto-expansion corrupts define bodies**:
   `MetaCParser_PeekToken` (metac_parser.c:~621) calls
   `MetaCParser_HandleIdentifier` on the peeked token, so while
   `ParseDefine` scans a body containing an already-defined macro
   (e.g. `#define TOSTRING(x) STRINGIFY(x)`, header.h) the body gets
   expanded and pushed mid-scan. Fix: add a small `bool
   DisableExpansion` field to `metac_preprocessor_t`, set it around the
   body scan in `ParseDefine` (metac_preproc.c:775) and check it in
   `HandleIdentifier`/`PeekToken`.
4. **Argument count check inverted** in `PushDefine`
   (metac_preproc.c:30): `parameters.Count != ParameterCount &&
   ((ParameterCount >= Count) && !IsVariadic)` doesn't catch
   too-many-args for fixed macros; then the `assert` blows up instead of
   a clean error. Cosmetic for the current tests (all calls match).

## Files touched so far

- `test/preprocessor_tests/preproc_test.c` — draft harness. Current state:
  compiles with the build recipe above, lexes the input file (bug found:
  it originally lexed the *filename string* — fixed), dumps the token
  list to stderr, `PrintTokenText` has a checksum-validated
  identifier lookup. **Not yet wired**: no directive walk, no ctx stack,
  no output buffering, main only lexes and dumps.
- Everything else untouched.

## Next steps

1. Rewrite `preproc_test.c` per the design above.
2. `gcc -E -P` reference generation in `run_tests.sh` + stub include dir
   for `<stdio.h>`.
3. Iterate: run the 4 local-include tests, fix listed preproc bugs as
   they surface.
