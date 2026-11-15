# MetaC Template System — Implementation Roadmap

This is the work plan for turning MetaC's template support from a
front-end skeleton into a working system.

Companion document: `TEMPLATE_SYSTEM.md` — describes what the system does
today and how the design relates to C++, D, Rust and the C preprocessor.
This document is only about *what to build*.

Status: reflects `main` at `9b886cd`.

---

## 0. The target

Before the tasks, the acceptance criterion. The following program must
compile, produce the right layout, and be codegen-able:

```
struct Pair!(T, U)
{
    T first;
    U second;
};

int add!(T)(T a, T b)
{
    return a + b;
}

int main()
{
    Pair!(int, float) p;
    p.first = 1;
    p.second = 2.0;
    return add!(int)(1, 2);
}
```

Concretely:

* `sizeof(Pair!(int, float))` is 8, with `first` at offset 0 and `second`
  at offset 4.
* `Pair!(int, float)` and `Pair!(int, float)` are the *same* type
  (same type index).
* `Pair!(int, float)` and `Pair!(float, int)` are *different* types
  (different type index, different layout).
* `Pair!(int, float)` and `Pair!(char, int)` are different types.
* `add!(int)` and `add!(float)` are different functions with different
  bodies.
* The template's *own* body is never emitted as code; only instances are.

Everything below exists to make that program work.

---

## 1. Phase 0 — Foundations

### 0.1 A semantic test harness

**Defect.** `run_tests.sh` builds `parser_test` with
`-DNO_SEMANTIC -DNO_FIBERS -DNO_PREPROCESSOR -DOLD_PARSER=1`. Nothing in
the repository can currently assert anything about template *semantics*.
The REPL is the only surface that runs the semantic layer, and it is not a
test harness.

**Work.** Add a semantic test binary (e.g. `semantic/semantic_test.c`)
built without `-DNO_SEMANTIC`, that parses a source string, runs the
semantic layer, and asserts on the resulting type indices / sizes. Keep the
same case-table style as `parser/metac_decl_test.c`.

**Done when.** A test can assert `sizeof` of a template instance, and the
test suite fails when the semantic layer is broken.

### 0.2 Set the instance node's hash

**Defect.** `parser/metac_parser.c:1085-1105` computes a local `hash` for
a `decl_type_template_instance_t` and then throws it away — `tinst->Hash`
is never assigned, and `AllocNewDecl` zero-fills the node. The semantic
waiter mechanism keys on the node hash
(`semantic/metac_semantic.c:939-953`), so a template instance can never be
matched by a waiter.

**Work.** Assign `tinst->Hash = hash;` after the argument list is parsed,
and fold the identifier into it as well.

**Done when.** Two syntactically different instances of the same template
with the same arguments have equal hashes; instances with different
arguments do not.

### 0.3 Audit the `type_index_template` expansions

**Defect.** `type_index_template` was added to `FOREACH_TYPE_INDEX_KIND`
(`semantic/metac_type.h:24`), but per the rule in `X_MACROS.md` ("check all
expansions") only two of the six consuming switches were updated:

| Consumer | Updated? |
|---|---|
| `NodeFromTypeIndex` (`semantic/metac_semantic.c:1241`) | yes |
| `SetTypeIndex` (`semantic/metac_semantic.c:1190-1216`) | **no** |
| `MetaCSemantic_GetTypeSize` (`semantic/metac_type_semantic.c:463-550`) | **no** → `assert(0)` at :549 |
| `MetaCCodegen_GetBCType` (`codegen/metac_codegen.c:41-153`) | **no** → `assert(0)` at :150 |
| `PrintSemaType` (`printer/metac_printer.c:1723-1838`) | **no** → `assert(0)` at :1833 |
| `IsUnknownType` / unresolved handling (`semantic/metac_type_semantic.c:63-78`) | n/a |

**Work.** Add a `type_index_template` arm to each of the four missing
switches. For now the arms can be `assert(0)` with a clear message, but
they must be *present* so that the compiler fails loudly and locally rather
than silently doing the wrong thing.

**Done when.** `grep -rn "type_index_template" semantic/ codegen/ printer/`
shows an arm in every switch that consumes the kind.

---

## 2. Phase 1 — Instance identity

This phase makes instances *distinct and correct*. It is deliberately
separate from Phase 2 (instantiation) because these are independent bugs
that are easy to mistake for one another.

### 1.1 Symbol plumbing

**Defect.** `semantic/metac_type_semantic.c:1378` hard-codes
`metac_decl_t* symbol = NULL;`. The template being instantiated
(`tInst->Identifier`) is never looked up. Consequences:

* The hash covers only the arguments (`:1380` computes `~0` for the symbol
  contribution).
* `MetaCTypeTable_AddImpl` returns the *existing* slot index when the hash
  matches (`semantic/metac_type_table.c:128-133`), so `Foo!(int)` and
  `Bar!(int)` **alias each other** and receive the same type index.

**Work.**

* Add a `metac_decl_t* Symbol` field to `decl_type_template_instance_t`
  (`parser/metac_parsetree.h:713-724`), populated during semantic by
  looking up `tInst->Identifier` in the current scope.
* Look up the symbol *before* computing the hash, and fold
  `symbol->Hash` into the key.
* Handle the case where the name does not resolve: report a proper
  diagnostic instead of silently producing a `NULL` symbol.

**Done when.** `Foo!(int)` and `Bar!(int)` get different type indices;
`Foo!(int)` and `Foo!(int)` get the same one.

### 1.2 Copy the arguments into semantic storage

**Defect.** `semantic/metac_type_semantic.c:1393` stores the parse-tree
`expr_argument_t*` chain into `metac_type_template_t::Arguments`, which is
declared `metac_expr_t*` (`semantic/metac_type.h:216`). The source comment
says it outright:

```c
tInst->Arguments, // we may need to copy the args here?
```

The table slot therefore points into parse-tree memory with the wrong
element type.

**Work.** Copy the argument list into semantic-owned storage as
`metac_expr_t*` (an array or a copied chain) and store that.

**Done when.** The template table owns its arguments; freeing the parse
tree does not invalidate the table.

### 1.3 Real expression equality

**Defect.** `semantic/metac_type_table.h:193-198`:

```c
static inline const bool MetaCExpr_Equal(const struct metac_expr_t* a,
                                         const struct metac_expr_t* b)
{
    // assert(0);
    return false;
}
```

It always returns `false`, so `TemplateSlotsEqual` (`:200-239`) always
returns `false` for any instance with at least one argument, and
`GetOrEmptyTemplateType` never finds an existing entry.

**Work.** Implement `MetaCExpr_Equal` as a proper structural comparison
over the expression kinds that can appear as template arguments. At minimum:
`expr_type` (compare `TypeIndex`), `expr_signed_integer` (compare value),
`expr_identifier`/`expr_variable` (compare the semantic identifier), and
`expr_template_instance` (recurse).

**Done when.** Two instances with equal arguments compare equal; two with
different arguments compare unequal.

### 1.4 Fix the slot comparison traversal

**Defect.** `semantic/metac_type_table.h:220-223` treats the argument list
as a flat array:

```c
for(uint32_t i = 0; i < argumentCount; i++)
{
   metac_expr_t argA = slotA->Arguments[i];
   metac_expr_t argB = slotB->Arguments[i];
```

`Arguments` is a linked list (or, after 1.2, a semantic array — pick one
and make both the writer and the reader agree). As written, `i > 0` is an
out-of-bounds read.

**Work.** Decide on a representation (a contiguous array of `metac_expr_t`
is simplest) and make the writer, the comparator and the table agree.

**Done when.** No out-of-bounds read; `TemplateSlotsEqual` compares all
arguments.

### 1.5 The hash/equality invariant

**Defect.** `MetaCTypeTable_AddImpl` contains:

```c
else if (slot->Hash == hash)
{
    // this assert makes sure we don't have duplicates
    assert (!cmpSlot(slot, entry));
    result.v = TYPE_INDEX_V(self->Kind, slotIndex_);
}
```

With `cmpSlot` always returning `false` the assert passes trivially — which
is why the aliasing bug in 1.1 went unnoticed.

**Work.** Once 1.3 and 1.4 are done, verify that hash equality implies slot
equality for the template table. If the hash is not strong enough to
guarantee that, the table needs a secondary comparison on hash collision.

**Done when.** The assert in `AddImpl` is meaningful for the template table.

---

## 3. Phase 2 — Instantiation (the missing core)

### 2.1 Bind the actual arguments to the parameter names

**Defect.** `semantic/metac_type_semantic.c:1367-1369`:

```c
// before calling semantic we need to mount the template instance scope.
metac_scope_t tmpScope = {scope_flag_temporary};
MetaCSemantic_PushTemporaryScope(self, &tmpScope);
```

The scope is pushed empty and never used. The comment states the intent.

**Work.** In that scope, bind each actual argument to the corresponding
parameter name of the template's declaration. The binding must reproduce
the shape of the placeholder it replaces, so that the declaration's semantic
can be re-run unchanged.

Concretely, the declaration side registers each parameter as an
`expr_unknown_value` node whose `TypeIndex` is the parameter's declared type
(`semantic/metac_type_semantic.c:1220-1223`). The actual binding should be
the *resolved* counterpart:

* For a **type parameter** (`T`): register a node of kind `expr_type` whose
  `TypeIndex` is the type index of the actual argument.
* For a **value parameter** (`int N`): register the evaluated constant
  expression.

**Done when.** Inside the instance scope, a lookup of `T` yields a node
whose `TypeIndex` is the actual argument's type.

### 2.2 Re-run the declaration's semantic with the bindings in place

**Defect.** Nothing re-runs the declaration. `metac_type_template_t::TypeIndex`
is `zeroIdx` forever (`semantic/metac_type_semantic.c:1391`).

**Work.** The aggregate semantic path (`semantic/metac_type_semantic.c:1167-1277`)
currently *always* creates its own template scope and registers
placeholders:

```c
if (agg->ParameterCount != 0)
{
    …
    tmpTemplateScope.Owner.v = SCOPE_OWNER_V(scope_owner_template, 0);
    MetaCSemantic_PushTemporaryScope(self, &tmpTemplateScope);
    for(…) { placeholder->Kind = expr_unknown_value; … }
}
```

If instantiation simply re-runs this path, the placeholders would shadow
the actual bindings that 2.1 just installed. So the path needs a variant
that **skips placeholder registration when a binding scope is already
mounted**, and instead lets the existing bindings be found by normal scope
lookup.

Design options:

* (a) A flag on the semantic state ("currently instantiating") that the
  aggregate path checks.
* (b) Split the aggregate semantic into "declare" and "instantiate" entry
  points sharing a common core.
* (c) Have the instance path call a function that takes an explicit
  binding scope and a flag.

Option (b) is the cleanest but touches the most code; option (a) is the
smallest.

**Done when.** `Pair!(int, float)` produces a struct type whose member
`first` has type `int` and member `second` has type `float` — i.e. the
aggregate's *layout* is computed from the actual arguments.

### 2.3 Store the resulting concrete type

**Work.** Set `metac_type_template_t::TypeIndex` to the index of the
concrete aggregate type produced by 2.2, instead of `zeroIdx`.

**Done when.** `NodeFromTypeIndex` on a template index yields a record
whose `TypeIndex` points at a real struct type.

### 2.4 Field types that depend on parameters

**Defect.** A member whose type is a parameter (`T field;`) currently
resolves to the unknown-value placeholder and therefore has an *unknown*
type. The `expr_unknown_value` → type-index bridge
(`semantic/metac_type_semantic.c:1494-1498`) handles the *lookup*, but the
dependent member still has no concrete type until 2.2 supplies it.

**Work.** Verify that after 2.2 the dependent members carry concrete type
indices, and that `MetaCSemantic_ComputeStructLayout` sees the real sizes.

**Done when.** `sizeof` of an instance is correct and the member offsets
match the concrete types.

### 2.5 Array dimensions that depend on parameters

**Defect.** `semantic/metac_type_semantic.c:966-971`:

```c
if (dim->Kind == expr_unknown_value)
{
    // this is essentially allowd b/c of templates.
    // TODO we might have to do something more here, maybe?
}
```

An array dimension that depends on a template parameter is accepted but
ignored — the dimension stays unknown.

**Work.** Once 2.1 binds value parameters, a dimension like `T[N]` must
evaluate to a concrete integer during instantiation.

**Done when.** `struct A!(int N) { int data[N]; }` instantiated with
`A!(4)` has 4 elements.

---

## 4. Phase 3 — Type-system integration

### 3.1 `SetTypeIndex`

**Defect.** `semantic/metac_semantic.c:1190-1216` has no
`type_index_template` case, so a template type's own index is never
recorded in its `TypeIndex` field.

**Work.** Add the case.

### 3.2 `MetaCSemantic_GetTypeSize`

**Defect.** `semantic/metac_type_semantic.c:547-550` — no
`type_index_template` case, so `assert(0)`. This is the crash you get for
`Foo!(int) x;`.

**Work.** Resolve the template index to its concrete type (2.3) and
delegate.

**Done when.** `sizeof(Foo!(int))` returns a value instead of aborting.

### 3.3 Resolving a template index to a concrete one

**Work.** Decide how the rest of the compiler obtains the concrete type
from a template index. Two options:

* (a) Keep `type_index_template` as a *first-class* index kind and resolve
  it lazily everywhere a concrete type is needed.
* (b) Make instantiation return the *concrete* index directly, and keep
  `type_index_template` only as the instance record's identity.

Option (b) is simpler and matches how C++/Rust work (an instance *is* the
concrete type). Option (a) preserves the distinction, at the cost of a
resolution step everywhere. Pick one and apply it consistently.

### 3.4 Unknown types vs. template types

**Defect.** `type_index_unknown` (`semantic/metac_type.h:12`) and
`type_index_template` (`semantic/metac_type.h:24`) are different kinds,
but the code that produces unknown types does not distinguish "unknown
because unresolved" from "unknown because it is a template parameter".

**Work.** Make sure a dependent member inside a template body produces
`type_index_unknown` (not `type_index_template`), and that
`IsUnknownType` (`semantic/metac_type_semantic.c:63-78`) and
`MetaCCodegen_GetBCType` (`codegen/metac_codegen.c:46`) treat them
consistently.

---

## 5. Phase 4 — Function templates

### 5.1 Keep the parameter list for ordinary functions

**Defect.** `parser/metac_parser.c:1989-1994` parses the template parameter
list, but it is only *stored* for `@run` functions
(`parser/metac_parser.c:2009-2010`). For an ordinary function the code
falls through to `ParseFunctionDecl` at `parser/metac_parser.c:2020` and
`templateParams` is silently discarded.

**Work.** Store `decl_function_t::TemplateParameters` /
`TemplateParameterCount` for all functions.

### 5.2 Bind template parameters in function bodies

**Defect.** `semantic/metac_semantic.c:1053-1056` processes only
`func->FunctionParameters`; `func->TemplateParameters` is never bound.

**Work.** When a function has template parameters, process it like an
aggregate: create a `scope_owner_template` scope, register the parameters
as unknown-value placeholders (mirroring
`semantic/metac_type_semantic.c:1209-1228`), and analyze the body in it.

### 5.3 Instantiate function templates

**Work.** A function template instance needs its own identity and its own
body. Reuse the Phase 1 machinery: the instance key is
`(function symbol hash, argument hashes)`. The instance record needs to
live somewhere — a new function-instance table, or reuse the template type
table with a distinguishing kind.

**Done when.** `add!(int)` and `add!(float)` compile to two distinct
functions with two distinct bodies, and neither emits the un-instantiated
body.

### 5.4 Deduction

**Work (optional, later).** Support calling `add!(1, 2)` and deducing
`T = int` from the argument types, rather than requiring `add!(int)(1, 2)`.

---

## 6. Phase 5 — Expression-position templates

### 6.1 Semantic for `expr_template_instance`

**Defect.** `semantic/metac_expr_semantic.c:707-715` has no case for
`expr_template_instance`, so it hits:

```c
default:
{
    xprintf("expression semantic doesn't support %s yet.\n", …);
    assert(0);
}
```

**Work.** Add a case that resolves the callee symbol, evaluates the
arguments, and produces either a call to the instantiated function or a
value.

### 6.2 Precedence and printing

**Status.** Parsing works: `!` is a binary operator with call precedence
(`parser/metac_expr_parser.c:147, 325-332, 1597-1605, 1854-1856`).
Printing works but is inconsistent with the type-position form —
`printer/metac_printer.c:1506-1511` emits `x ! (1, 2)` with spaces, while
the type-position printer emits `Foo!(int)` without
(`printer/metac_printer.c:645-668`).

**Work.** Make the two forms print consistently.

---

## 7. Phase 6 — Code generation

### 6.1 No template code generation exists

**Defect.** `codegen/` contains **zero** occurrences of the word
"template". `MetaCCodegen_GetBCType` has no `type_index_template` case and
falls into `assert(0)` (`codegen/metac_codegen.c:145-151`). The expression
generator has no `expr_template_instance` case and falls into `assert(0)`
(`codegen/metac_codegen.c:1284-1289`).

**Work.** This phase should be **empty** if Phase 3 resolves template
indices to concrete types before codegen. A template type that reaches
codegen is a bug, and the `assert(0)` is the correct behaviour. Add a
clear diagnostic rather than a generic abort, so the failure mode is
legible.

**Done when.** Templates work end-to-end and codegen never sees a
`type_index_template`.

---

## 8. Phase 7 — Printer completion

### 8.1 `PrintSemaType` for template types

**Defect.** `printer/metac_printer.c:1830-1834` — no `type_index_template`
case, so printing a semantic tree containing a template type aborts.

**Work.** Add the case. Depending on the Phase 3.3 decision, print either
the instance syntax (`Pair!(int, float)`) or the resolved concrete type.

### 8.2 The `@run` marker is lost

**Defect.** `FuncFlag_Run` is never consulted by the printer, so
`int f!(T) @run { }` prints as `int f!(T) () { }` — lossy
(`printer/metac_printer.c:1280-1290`).

**Work.** Print the marker.

### 8.3 The template's own body is never printed

**Work.** Decide whether a template declaration (as opposed to an instance)
should be printable. Today the printer prints `struct Pair!(T) { … }`
correctly from the parse tree; after instantiation the *semantic* tree
needs a story for the original declaration.

---

## 9. Phase 8 — Tests

### 9.1 Parser-level tests

**Defect.** `parser/metac_decl_test.c` has the shape-dumper support
(`parser/metac_decl_test.c:269-270, 321-322`) but its ~55 test cases
contain no `!` at all.

**Work.** Add cases for:

* `struct Foo!(T) { T x; };`
* `Pair!(int, float) p;`
* `int f!(T)(T a);`
* `x!(1, 2)` in expression position
* `int f!(T) @run { }`

**Done when.** The shape dumper round-trips these.

### 9.2 Semantic-level tests

**Depends on 0.1.** Add cases asserting:

* distinct instances get distinct type indices
* identical instances get the same type index
* `sizeof(Pair!(int, float))` is correct
* member offsets are correct
* dependent members resolve to the actual argument's type

### 9.3 End-to-end tests

**Work.** Compile and run the acceptance program from §0 through the full
pipeline (parse → semantic → codegen) and check the result.

---

## 10. Dependency summary

```
0.1 test harness ──┬─► all later verification
0.2 instance hash ─┘
0.3 switch audit

1.1 symbol plumbing ──┐
1.2 copy arguments ───┼─► 1.5 hash/equality invariant
1.3 real equality ────┤
1.4 traversal fix ────┘

2.1 bind arguments ──┐
2.2 re-run semantic ─┼─► 2.3 store concrete type ──► 3.1-3.4 type integration
2.4 dependent fields ─┤
2.5 array dims ──────┘

5.1-5.3 function templates ──► (Phase 6 codegen)
6.1 expression templates ────► (Phase 6 codegen)

8.1-8.3 printer ──► 9.3 end-to-end
```

Phases 1 and 2 are independent and can proceed in parallel. Phase 6 is
expected to require no work beyond better diagnostics.

---

## 11. Risks and open questions

1. **The aggregate semantic path is shared with non-template structs.**
   Phase 2.2 modifies `semantic/metac_type_semantic.c:1167-1277`, which is
   on the hot path for *every* struct and union. The change must not
   regress ordinary aggregates. Option (a) in 2.2 (a flag) is lower risk
   than option (b) (a split).

2. **Type arguments are parsed as expressions.** `Foo!(int)` parses `int`
   through `MetaCParser_ParseArgumentList` with `expr_flags_type`
   (`parser/metac_parser.c:1094`), so a type argument is an `expr_type`
   expression whose `TypeIndex` is the type. A *value* argument is a
   constant expression. Instantiating a type argument therefore requires
   the expression semantic to have produced a type index for it — verify
   this works before building on it.

3. **Parameter kinds are not distinguished.** A parameter is just a
   declaration (`parser/metac_parser.c:914-988`), so `T` and `int N` are
   the same kind of thing. The instance binder in 2.1 has to decide whether
   a parameter is a type slot or a value slot. Options: decide from the
   parameter's declared type (`T` has type "the type named T" — a type
   parameter is a parameter whose declared type is itself a template
   parameter), or make it explicit in the syntax. The current syntax does
   not distinguish them.

4. **`scope_owner_template` always has index 0**
   (`semantic/metac_type_semantic.c:1206`), so a template scope does not
   record which template it belongs to. This may matter once several
   templates are nested or shadowing.

5. **No specialization/partial-specialization syntax exists.** Not needed
   for the MVP, but worth deciding whether `Foo!(int)` should be
   overridable by the user.

6. **The `auto` mechanism (§2.2 of `TEMPLATE_SYSTEM.md`) is a separate
   system.** It should not be entangled with the `!` machinery unless
   there is a deliberate decision to unify them. Unifying them later is
   easier if Phase 2's instance binder is written in terms of "bind a
   semantic node into a scope" rather than "bind a template argument".

---

## 12. Quick reference: the defects this roadmap addresses

| # | Defect | Location |
|---|---|---|
| 0.2 | Instance node hash never assigned | `parser/metac_parser.c:1085-1105` |
| 0.3 | `type_index_template` missing from 4 switches | `semantic/metac_semantic.c:1190`, `semantic/metac_type_semantic.c:463`, `codegen/metac_codegen.c:41`, `printer/metac_printer.c:1723` |
| 1.1 | Template name never looked up → instances alias | `semantic/metac_type_semantic.c:1378` |
| 1.2 | Arguments point into the parse tree | `semantic/metac_type_semantic.c:1393` |
| 1.3 | `MetaCExpr_Equal` always returns `false` | `semantic/metac_type_table.h:193-198` |
| 1.4 | Slot comparison indexes a linked list | `semantic/metac_type_table.h:220-223` |
| 2.1 | Instance scope pushed but never populated | `semantic/metac_type_semantic.c:1367-1369` |
| 2.2 | Declaration semantic never re-run | `semantic/metac_type_semantic.c:1167-1277` |
| 2.3 | `metac_type_template_t::TypeIndex` stays `zeroIdx` | `semantic/metac_type_semantic.c:1391` |
| 2.5 | Dependent array dimensions ignored | `semantic/metac_type_semantic.c:966-971` |
| 3.2 | No size for template types → `assert(0)` | `semantic/metac_type_semantic.c:547-550` |
| 5.1 | Function template params discarded | `parser/metac_parser.c:1994-2020` |
| 5.2 | Function template params never bound | `semantic/metac_semantic.c:1053-1056` |
| 6.1 | No semantic for expression templates → `assert(0)` | `semantic/metac_expr_semantic.c:707-715` |
| 8.1 | No printer for template types → `assert(0)` | `printer/metac_printer.c:1830-1834` |
| 8.2 | `@run` marker lost when printing | `printer/metac_printer.c:1280-1290` |
| 9.1 | No template test cases anywhere | `parser/metac_decl_test.c:352-437` |
