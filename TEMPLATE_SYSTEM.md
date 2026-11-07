# MetaC Template System — Architecture Notes

This document describes how templates are represented and processed by MetaC
as of `preproc@~1f0de71`, and how the design relates to C++ and D templates.

---

## 1. The bigger picture

MetaC is a pipeline of "sema" passes that are allowed to be *partial*:

    lexer -> preprocessor -> parser -> semantic (expr/type/decl/stmt) -> codegen

The semantic layer does not require that a declaration be fully resolvable
when it is first processed. It uses three cooperating mechanisms to defer
and retry unresolved parts:

1. **Placeholder semantics** — unresolved identifiers are represented by
   semantic `expr_unknown_value` nodes which are registered in the current
   scope under their own identifier. Other expressions referencing them
   decode to the placeholder and get re-evaluated later when the
   placeholder itself is resolved (each such `metac_sema_expr_t` remembers
   its original syntax in `.UnkownValueOrigin`).
2. **Temporary / mounted scopes** — synthetic scopes (flagged
   `scope_flag_temporary` / `scope_flag_mounted`) are pushed for the
   duration of one partial evaluation and popped afterwards
   (`MetaCSemantic_PushTemporaryScope`, `MetaCSemantic_MountScope`,
   `semantic/metac_semantic.c:248-357`).
3. **Fiber-based yields** — under `NO_FIBERS=0`, a pass can `YIELD(...)`
   while a dependency is being calculated in another coroutine and retry
   (`TryAgain`) afterwards; without fibers an `OnResolveFail` stack makes
   failed lookups return "unresolved" instead of erroring
   (`semantic/metac_type_semantic.c:1420-1470`).

The template system is built entirely on top of these three mechanisms.

---

## 2. Syntax: where templates appear

### 2.1 Template arguments on declarations (headers)

`struct`/`union` constructors may take template parameters directly in the
declaration; the parser stores them in the aggregate's parameter list
(`metac_parsetree.h`, `decl_parameter_t` shared with function parameters):

    struct Vector!(T) {...}   // agg -> Parameters / ParameterCount

Function declarations can carry template parameters too
(`metac_parser.c:1994-2016`): a `!` token followed by a parameter list is
parsed with `ParseParameterList` and attached to
`decl_function_t.TemplateParameters`/`TemplateParameterCount`
(`metac_parsetree.h:565-569`).

A parameter whose type is `auto` also implies a template
(`semantic/metac_semantic.c:1018-1024`, currently just a hint print).

### 2.2 Instantiation syntax `name!(args)` — D-style "bang"

MetaC uses the **D bang-instantiation syntax**, not C++ `<...>`. It is
parsed in two positions:

* **Type position**: when a type identifier is followed by `!` (+argument
  list), the parser creates a `decl_type_template_instance_t` node
  (`parser/metac_parser.c:1091-1110`, decl defined at
  `metac_parsetree.h:713`) with `TypeKind == type_template_instance`.

* **Expression position**: `!` is treated as a binary/postfix operator
  `expr_template_instance` (`parser/metac_expr_parser.c:140-147,
  1599-1629, 1855, 1877-1881`). A `!` followed
  by `(` reuses the argument-list parse.

---

## 3. Partial semantic of a *template declaration*

The interesting path is `MetaCSemantic_doTypeSemantic` for aggregates
(`semantic/metac_type_semantic.c:1200-1300`). For a `struct`/`union` with
template parameters:

1. A temporary scope owned by *nothing* is set up:
   `tmpTemplateScope.Owner = SCOPE_OWNER_V(scope_owner_template, 0)`,
   flagged temporary, with a pre-sized `ScopeTable` of exactly
   `ParameterCount` slots (`MetaCScopeTable_InitN`), then pushed.

2. **Each parameter name is registered as an `expr_unknown_value`
   placeholder** (`:1220-1239`): an `AllocNewSemaExpr` node with
   - `Kind = expr_unknown_value`
   - `TypeIndex = ` semantic of the parameter's declared type (so
     `T!(int)`-style params act like typed variables)
   - `UnkownValueOrigin = ` empty (there is no expression to substitute yet)

   This is the *binding state of the declaration-side template*: parameter
   names resolve, but to intentionally-unknown values.

3. `PopulateTemporaryAggregateScope` walks the fields; field types and
   initializers that reference parameter names resolve to the placeholders
   above (via normal scope lookup) and stay "unresolved" in the shape-based
   sense. A placeholder swap-in later re-evaluates dependent expressions
   through `UnkownValueOrigin` (`expr_unknown_value` semantic passes the
   node through verbatim, `semantic/metac_expr_semantic.c:497-500`).

4. The temporary aggregate scope is **mounted** under the current scope
   (`MetaCSemantic_MountScope`) so nested scopes can be built inside, and
   struct layout + type-table hashing proceeds for the *non-dependent* part
   (`MetaCSemantic_ComputeStructLayout`, `AggregateHash`).
   `PersistTemporaryAggregateAndPopulateScope` converts the temporary
   aggregate into a persistent `metac_type_aggregate_t` and the scope owner
   switches from `scope_owner_template` to a real aggregate-owner scope.

5. Unmount and pop the temporary scope. What sticks around afterwards is
   a struct whose member types referencing template placeholders remain
   "unknown" until an instantiation happens.

The `expr_unknown_value` placeholder trick is *shared* with enum member
semantic (`semantic/metac_type_semantic.c:764-830`): enum members are also
registered as placeholders first, then replaced with real
`decl_enum_member` values as they resolve — same unknown-value mechanics,
different payload.

---

## 4. Instance semantics: `name!(args)` in type position

`decl_type_template_instance` semantic lives at
`semantic/metac_type_semantic.c:1354-1402`:

1. A temporary scope is pushed (future: this is where an instance-local
   scope binding parameter -> actual value will be mounted).

2. Each syntax argument gets full expression semantic
   (`metac_sema_expr_t** semaArguments`).

3. An **instance key** is computed:
   `hash = crc32c_nozero(~0, &symbol->Hash, ...)` then
   `crc32c_nozero(hash, &semaArg->Hash, ...)` per argument
   over the referenced symbol's hash plus every semantic argument's hash
   (so two same-argument instantiations dedupe, `Vector!(float)` doesn't).

4. `metac_type_template_t` (`semantic/metac_type.h:208-216`) is the
   instance record: `{ Header, TypeIndex, Symbol, Arguments, ArgumentCount }`
   — i.e. a *declaration reference + argument tuple*, **not** a
   fully-substituted copy.

5. `TemplateTypeTable` is consulted with
   `MetaCTypeTable_GetOrEmptyTemplateType`, creating with
   `MetaCTypeTable_AddTemplateType` on a miss
   (`semantic/metac_type_semantic.c:1392-1400`; the table is registered in
   `FOREACH_TYPE_TABLE` alongside `PtrTypeTable`, `StructTypeTable`,
   `TypedefTypeTable` etc. in `semantic/metac_semantic.h:77`).

`NodeFromTypeIndex` dispatches `type_index_template` to
`TemplateTypePtr(...)` (`semantic/metac_semantic.c:1241-1242`) so the rest
of the machinery can treat an instance index like any other type index.

> **Honest state of the world** — code comments in
> `metac_type_semantic.c:1354-1402` flag two TODOs:
> - `symbol` is still `NULL` during evaluation today (currently the hash is
>   only over the arguments, so two same-argument instances of different
>   templates alias). This needs a parse-side symbol fetch.
>
> - The arguments are passed still pointing at the parse tree
>   (`we may need to copy the args here?`); they are not cloned into
>   sema storage.
>
> And crucially, **no body substitution happens yet**: an instance is a
> memoized reference to the declaration + argument tuple. The
> declaration-side pass in section 3 has to be *re-run with the
> instance's actuals bound to the parameter names in place of the
> placeholders* to get a truly specialized type. That is the missing
> "instantiate" step between the hash-key allocation above and a printed
> `Vector!(int)` type.

---

## 5. Comparison with C++ and D templates

| aspect | C++ | D | MetaC (current) |
|---|---|---|---|
| syntax | `Name<Args>` | `Name!(Args)` | `Name!(Args)` (D-style) |
| definition-time checking | full (dependent types, `typename`) | full | placeholders (`expr_unknown_value`) stand in for deps |
| instance identity | deep, one COMDAT per instantiated entity | hash over `(ThisTemplate, Args)` | hash over `(Symbol, semaHash(Args))` → `TemplateTypeTable` slot |
| instantiation step | AST cloning + substitution | semantic re-entry per instantiation | **missing** — instances currently stay shallow `{Symbol, Args}` records |
| dep. name lookup | two-phase | two-phase | single pass; scope `owner_template` + mounted temporary scopes |
| deduction/SFINAE | full (SFINAE, concepts-lite via `constraints`) | full (if-constraints) | none |
| dependency on fiber runtime | no | no | yes: unresolved lookups yield instead of fail |

Key differences worth articulating:

* **MetaC's semantic runs *once* per declaration, not once per
  instantiation.** Like D's "a declaration's semantic is done close to the
  boundary", whereas C++-relatives re-run *all* semantic lazily; MetaC instead
  distinguishes shape (hashable "structural" semantics) from unresolved
  parts, memoizes the shape by hash, and *swaps in* resolved values as they
  become available — the same swap mechanism used for enum forward-value
  dependencies and for cross-function-call early-bound types.

* **The instance table is the whole trick.** It makes instantiation
  cache-friendly and lets codegen/printing treat two same-argument
  instantiations as one type
  (`MetaCTypeTable_GetOrEmptyTemplateType`). D does something extremely
  similar (`DTemplateInstance` keyed by compile-time arg hashes) — closer
  to D than to C++.

* **C++-style two-phase lookup has no analog.** MetaC avoids the
  "which names are in scope at definition of the template?" problem by
  making unverifiable names resolve to placeholders *in the template's own
  scope* (owner_template) rather than an error. That is why templates can
  reference "not-yet-declared" things: missing name => `expr_unknown_value`
  placeholder registered in the current template-parameter scope, with
  `Waiters.Waiters[]` for deferred re-resolution once it exists
  (`semantic/metac_type_semantic.c:1442-1465`).

---

## 6. Where the seams are (TODO list for future work)

1. **Symbol plumbing** — `decl_type_template_instance_t` needs the
   referenced `decl_*` symbol itself (currently only the *name* is stored;
   `metac_type_semantic.c:1386` therefore keys on `~0` at that position). Once that is
   stored, `func/struct` discovery by name instantiates into a
   genuinely-specialized type.
2. **Body re-instantiation** — re-run `MetaCSemantic_doTypeSemantic` for
   the aggregate with actual args in place of placeholders, and store
   the resulting aggregate type index in the template table's
   `TypeIndex` field (instead of `zeroIdx`).
3. **Function template bodies** — `decl_function_t.TemplateParameters` is
   parsed, but `MetaCSemantic_doFunctionSemantic` doesn't bind them or
   produce an instance-per-call-site.
4. **Hash-symbol cross-reference** — template instances of *different*
   templates with identical argument shapes alias today; carrying the
   (not-yet-None) `symbol` in the key solves that.
5. **Semantics of `expr_template_instance` (expression position)** —
   no case currently constructs/represents it in `doExprSemantic` (only the
   type-position has a path); expression position needs its own
   instantiation entry point.

---

## 7. File pointers

* `parser/metac_parser.c:1091` — type-position `name!(args)` parse
* `parser/metac_expr_parser.c:140-147, 1599-1629, 1855, 1877-1881` —
  expression-position `!` operator
* `parser/metac_parser.c:1994-2016` — templated function header parse
* `parser/metac_parsetree.h:548-575, 713` — `decl_parameter_t`,
  `decl_function_t.TemplateParameters`, `decl_type_template_instance_t`
* `semantic/metac_type.h:195-216` — `metac_type_template_t`
* `semantic/metac_semantic.h:77` — `TemplateTypeTable` in the table list
* `semantic/metac_scope.h:21` — `scope_owner_template`
* `semantic/metac_semantic.c:196-357` — temporary/mounted scope machinery
* `semantic/metac_type_semantic.c:1200-1300` — declaration-side partial
  semantic (binding placeholder parameters), temp aggregate scope setup
* `semantic/metac_type_semantic.c:1354-1402` — instance semantic and
  `TemplateTypeTable` hit/miss (with the TODOs above)
* `semantic/metac_expr_semantic.c:497-500` — `expr_unknown_value`
  pass-through
* `semantic/metac_sematree.h:130` — `UnknownValueSemaIdentifier` origin
  tracking
