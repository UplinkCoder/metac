# The MetaC Template System

This document describes how templates are represented and processed by MetaC,
what the current implementation does and does not do, and how the design
relates to template systems in other languages.

Status of this document: reflects `main` at `9b886cd`
("Fixup resolve to unknown-value expressions for templates").

---

## 1. Summary

MetaC uses the **D-style bang instantiation syntax** (`Name!(Args)`) rather
than C++ angle brackets. Templates exist in two syntactic positions:

| Position | Syntax | Parse node | Semantic state |
|---|---|---|---|
| Type | `Foo!(int)` | `decl_type_template_instance` | shallow instance record in `TemplateTypeTable` |
| Expression | `x!(1, 2)` | `expr_template_instance` | **not implemented** (`assert(0)`) |

Template *declarations* exist for aggregates (`struct Foo!(T) { … }`) and
functions (`void f!(T) { … }`).

**The current implementation is a front-end skeleton.** Syntax, parse tree,
printer round-trip and a shallow semantic record all work. What does *not*
exist yet: instantiation (body substitution), specialization, function
template bodies, size/layout for template types, and any code generation.
Section 9 is the honest gap list; the implementation plan is in
`TEMPLATE_ROADMAP.md` (section 11 links to it).

MetaC also has a **second, unrelated genericity mechanism**: `auto` in a
function parameter position, used by `@run` functions that interrogate their
own argument types at runtime (section 2.3). The two do not interact.

---

## 2. The two genericity mechanisms

### 2.1 Explicit templates (`!`)

Parameterized declarations, explicit instantiation with actual arguments.
This is the mechanism described in the rest of this document.

### 2.2 Implicit genericity (`auto` parameters)

A parameter declared `auto` is polymorphic by construction. The semantic
layer notices this and says so:

```c
// semantic/metac_semantic.c:1018-1023
if (paramTypeIdx.Kind == type_index_basic
 && paramTypeIdx.Index == type_auto)
{
    fprintf(stderr,
        "Detected auto type in func params which implies template\n");
}
```

This is currently a *diagnostic only* — there is no instantiation machinery
behind it. It exists because `@run` functions are expected to inspect their
own argument types at runtime:

```
// repl/test.mtc
void Print(auto Value, const char* name, uint32_t indent)
{
 @run {
        type valueType = typeof(Value);
        switch(.Compiler->GetTypeKind(.Compiler, valueType))
        …
```

`typeof` plus the `.Compiler` runtime library lets such a function behave
like a Rust `impl Trait` / C++ generic lambda — one body, many types,
dispatched at runtime rather than monomorphized at compile time.

### 2.3 Why two mechanisms?

The two mechanisms answer different questions.

* `!` templates are **monomorphization**-shaped: each distinct argument
  tuple is supposed to yield a distinct concrete type. This suits aggregate
  types (`Vector!(int)`) where the *layout* depends on the arguments.
* `auto` parameters are **type-erasure**-shaped: one function, one body,
  runtime dispatch on `typeof`. This suits functions that *reason about*
  types rather than *are defined by* them.

They are not redundant. A future `!` template could, in principle, lower
`auto` parameters to the same machinery.

---

## 3. Syntax

### 3.1 Template declarations

Aggregate templates put the parameter list after the name:

```
struct Vector!(T)
{
    T* data;
    uint32_t count;
};
```

Parser: `parser/metac_parser.c:1185-1192`. The `!` is matched, then
`ParseParameterList` is called and the result stored in
`decl_type_struct_t::Parameters` / `ParameterCount`
(`parser/metac_parsetree.h:642-657`).

Function templates use the same shape:

```
void swap!(T)(T* a, T* b) { … }
```

Parser: `parser/metac_parser.c:1989-1994`. The parameters are parsed into
`decl_function_t::TemplateParameters` / `TemplateParameterCount`
(`parser/metac_parsetree.h:556-569`).

**Important limitation:** the parsed template parameter list is only *stored*
for `@run` functions (`parser/metac_parser.c:2009-2010`). For an ordinary
function the code falls through to `ParseFunctionDecl` at
`parser/metac_parser.c:2020` and `templateParams` is silently discarded.

### 3.2 Template parameters are ordinary declarations

`ParseParameterList` (`parser/metac_parser.c:914-988`) is the *same*
routine used for C function parameters. A template parameter is therefore a
name plus a type — exactly like a function parameter:

```
struct Foo!(T)        // T is a type parameter
struct Foo!(int N)    // N is a value parameter
```

Because the list is parsed by the ordinary parameter parser, template
parameters may carry the full C declarator syntax, including `*`, `[]` and
function-pointer forms.

### 3.3 Instantiation

```
Vector!(int)          // type position
x!(1, 2)              // expression position
```

**Type position** — `parser/metac_parser.c:1082-1105`. When a type
identifier is followed by `!`, the parser allocates a
`decl_type_template_instance_t` node, sets `TypeKind = type_template_instance`
and parses an argument list:

```c
if (MetaCParser_PeekMatch(self, tok_bang, 1))
{
    decl_type_template_instance_t* tinst =
        AllocNewDecl(decl_type_template_instance, &result);

    expr_argument_list_t argList;
    hash ^= CRC32C_BANG;
    tinst->TypeKind = type_template_instance;

    MetaCParser_Match(self, tok_bang);
    tinst->Identifier = RegisterIdentifier(self, identifier);
    argList = MetaCParser_ParseArgumentList(self, expr_flags_type);
    …
}
```

**Expression position** — `!` is modelled as a *binary* operator with call
precedence:

* `parser/metac_expr_parser.c:147` — `tok_bang` maps to
  `expr_template_instance`.
* `parser/metac_expr_parser.c:325-332` — precedence 30, the same as
  `expr_call` and `expr_index`.
* `parser/metac_expr_parser.c:1597-1605` — the argument list reuses
  `MetaCParser_ParseArgumentList`.
* `parser/metac_expr_parser.c:1854-1856` — the postfix form is recognised in
  `MetaCParser_ParseExpr`.

Prefix `!` (logical not, `expr_not`) and postfix `!` (template instance) are
distinguished by position, and `!=` lexes as a separate token `tok_neq`, so
there is no ambiguity.

### 3.4 Lexer and preprocessor

`tok_bang` is an ordinary single-character token (`parser/metac_lexer.h:138`).

The preprocessor has **no** template support. A template instance inside an
`#if` expression reaches the default arm of the preprocessor's constant
evaluator and aborts:

```c
// parser/metac_preproc.c:277-283
default : {
    fprintf(stderr,
        "Evaluator doesn't know how to eval: %s\n",
        MetaCExprKind_toChars(e->Kind)
    );
    assert(0);
} break;
```

---

## 4. Parse-tree representation

```c
// parser/metac_parsetree.h:713-724
typedef struct decl_type_template_instance_t
{
    DECL_HEADER
    TYPE_HEADER

    metac_identifier_ptr_t Identifier;

    expr_argument_t* Arguments;

    uint32_t ArgumentCount;
} decl_type_template_instance_t;
```

Arguments are a singly linked list:

```c
// parser/metac_parsetree.h:59-65
typedef struct expr_argument_t
{
    EXPR_HEADER

    struct metac_expr_t* Expr;
    struct expr_argument_t* Next;
} expr_argument_t;
```

The relevant node kinds are declared in the X-macro lists that generate the
enums (see `X_MACROS.md`):

* `parser/metac_node.h:37` — `M(decl_type_template_instance)`
* `parser/metac_node.h:157` — `M(expr_template_instance)`

---

## 5. Semantic: the declaration side

### 5.1 The unknown-value placeholder

The central mechanism is `expr_unknown_value` (`parser/metac_node.h:163`).
A semantic expression of this kind means: *"this expression has a known
type, but no known value (yet)"*. It carries a `TypeIndex` alongside the
`Kind`:

```c
// semantic/metac_sematree.h:127-131
struct {
    metac_expr_t* UnkownValueOrigin;
    metac_identifier_ptr_t UnknownValueSemaIdentifier;
};
```

The same mechanism is used for enum members that are defined in terms of
each other (`semantic/metac_type_semantic.c:764-830`), where each member is
first registered as an unknown-value placeholder and later swapped for its
real value. Templates reuse the exact same trick, with a different payload.

### 5.2 Binding template parameters

For an aggregate with template parameters, `MetaCSemantic_doTypeSemantic`
(`semantic/metac_type_semantic.c:1167-1277`) does this:

1. Creates a temporary scope owned by `scope_owner_template`
   (`semantic/metac_type_semantic.c:1203-1206`), with a scope table sized
   for exactly `ParameterCount` entries.

```c
metac_scope_t tmpTemplateScope = {0};
…
U32(tmpTemplateScope.ScopeFlags) |= scope_flag_temporary;
tmpTemplateScope.Owner.v = SCOPE_OWNER_V(scope_owner_template, 0);
MetaCScopeTable_InitN(&tmpTemplateScope.ScopeTable, agg->ParameterCount, &self->TempAlloc);
MetaCSemantic_PushTemporaryScope(self, &tmpTemplateScope);
```

2. Registers **each parameter name as an unknown-value placeholder**:

```c
// semantic/metac_type_semantic.c:1209-1228
for(uint32_t paramIdx = 0; paramIdx < parameterCount; paramIdx++)
{
    scope_insert_error_t inserted;
    metac_identifier_ptr_t paramIdent = param->Parameter->VarIdentifier;
    metac_location_ptr_t locIdx = param->Parameter->LocationIdx;
    metac_expr_t placeholderExpr;
    metac_sema_expr_t* placeholder = AllocNewSemaExpr(self, &placeholderExpr);

    metac_identifier_ptr_t semaId =
        MetaCIdentifierTable_CopyIdentifier(self->ParserIdentifierTable,
                                            &self->SemanticIdentifierTable, paramIdent);

    placeholder->Kind = expr_unknown_value;
    placeholder->TypeIndex = MetaCSemantic_TypeSemantic(self, param->Parameter->VarType);
    METAC_NODE(placeholder->UnkownValueOrigin) = emptyNode;
    placeholder->LocationIdx = locIdx;

    inserted = MetaCSemantic_RegisterInScope(self, semaId, METAC_NODE(placeholder));

    param = param->Next;
}
```

The parameter's `TypeIndex` is the semantic of its *declared* type. So
`T` binds to a placeholder whose type is the type named `T`, and `int N`
binds to a placeholder whose type is `int`.

3. Populates and mounts the aggregate's own scope, computes layout, and
   hashes the aggregate.

4. Unmounts and pops the temporary scope.

The consequence: inside a template body, a use of `T` resolves through
normal scope lookup to the placeholder, and the expression semantic passes
it through *verbatim*:

```c
// semantic/metac_expr_semantic.c:506-509
else if (node->Kind == node_expr_unknown_value)
{
    (*result) = *cast(metac_sema_expr_t*) node;
    hash = result->Hash;
}
```

So `T` inside the body is an expression of *unknown value* but *known type*.

### 5.3 Propagation of unknown values

Any expression that consumes an unknown-value operand becomes unknown-value
itself, while keeping the operand's type:

```c
// semantic/metac_expr_semantic.c:982-1025
if (E1->Kind == expr_unknown_value)
{
    result->Kind = expr_unknown_value;
}
result->TypeIndex = E1->TypeIndex;
…
if (castType.v == 0 || castExp->Kind == expr_unknown_value)
    result->Kind = expr_unknown_value;
…
if (result->E1->Kind == expr_unknown_value || result->E2->Kind == expr_unknown_value)
{
    result->Kind = expr_unknown_value;
}
```

This is how a template parameter's *type* propagates through an expression
while its *value* stays unknown — which is exactly what you need to compute
a layout for a dependent member.

### 5.4 The type/value boundary

The most recent commit (`9b886cd`) closed the loop between unknown values
and unknown types. When a *type* identifier resolves to an unknown-value
node, the type index is taken from the placeholder:

```c
// semantic/metac_type_semantic.c:1494-1498
case expr_unknown_value:
{
    metac_sema_expr_t* val = cast(metac_sema_expr_t*)node;
    result.v = val->TypeIndex.v;
} break;
```

Before this, using a template parameter as a type hit
`assert(0); // me not know what do do.` at `semantic/metac_type_semantic.c:1509`.

This is the bridge that lets `T` be used as a type inside a template body:
the parameter's *value* is unknown, but its *type* is known and is now
propagated as a type.

### 5.5 Unknown types

The type-level counterpart is `type_index_unknown`
(`semantic/metac_type.h:12`), a type-index kind whose value is 0. It is
used for fields whose type genuinely could not be determined
(`semantic/metac_type_semantic.c:623`) and is detected by:

```c
// semantic/metac_type_semantic.c:63-78
bool IsUnknownType(metac_sema_state_t* self, metac_type_index_t typeIndex)
{
    bool result = false;

    if (TYPE_INDEX_KIND(typeIndex) == type_index_unknown)
    {
        result = true;
    }
    if (TYPE_INDEX_KIND(typeIndex) == type_index_array)
    {
        result = IsUnknownType(self, self->ArrayTypeTable.Slots[TYPE_INDEX_INDEX(typeIndex)].TypeIndex);
    }

    return result;
}
```

Code generation refuses to emit unknown or unresolved types
(`codegen/metac_codegen.c:46`).

---

## 6. Semantic: the instance side

### 6.1 The instance record

```c
// semantic/metac_type.h:208-219
typedef struct metac_type_template_t
{
    metac_type_header_t Header;

    metac_type_index_t TypeIndex;

    metac_decl_t* Symbol;

    metac_expr_t* Arguments;

    uint32_t ArgumentCount;
} metac_type_template_t;
```

| Field | Meaning | Current value |
|---|---|---|
| `Header` | kind / location / hash / serial | `Kind = decl_type_template_instance` |
| `TypeIndex` | index of the *specialized* type | `zeroIdx` — never filled in |
| `Symbol` | the templated declaration | **always `NULL`** |
| `Arguments` | the instance's argument tuple | points into the **parse tree** |
| `ArgumentCount` | number of arguments | from the parse tree |

The type index kind is `type_index_template` (`semantic/metac_type.h:24`),
and `NodeFromTypeIndex` dispatches it to `TemplateTypePtr`
(`semantic/metac_semantic.c:1241-1242`).

### 6.2 The instance path

`semantic/metac_type_semantic.c:1356-1405`:

```c
else if (type->Kind == decl_type_template_instance)
{
    decl_type_template_instance_t* tInst = cast(decl_type_template_instance_t*) type;
    expr_argument_t* args = tInst->Arguments;
    STACK_ARENA_ARRAY(metac_sema_expr_t*, semaArguments, 64, &self->TempAlloc);

    ARENA_ARRAY_ENSURE_SIZE(semaArguments, tInst->ArgumentCount);
    // before calling semantic we need to mount the template instance scope.
    metac_scope_t tmpScope = {scope_flag_temporary};
    MetaCSemantic_PushTemporaryScope(self, &tmpScope);

    for(; METAC_NODE(args) != emptyNode; args = args->Next)
    {
        metac_expr_t* e = args->Expr;
        ARENA_ARRAY_ADD(semaArguments, MetaCSemantic_doExprSemantic(self, e, 0));
    }

    {
        metac_decl_t* symbol = NULL;
        metac_sema_expr_t** arguments = semaArguments;
        uint32_t hash = symbol ? crc32c_nozero(~0, &symbol->Hash, sizeof(symbol->Hash)) : ~0;
        uint32_t nArguments = tInst->ArgumentCount;
        for(uint32_t argIdx = 0; argIdx < nArguments; argIdx++)
        {
            metac_sema_expr_t* semaArg = arguments[argIdx];
            hash = crc32c_nozero(hash, &semaArg->Hash, sizeof(semaArg->Hash));
        }
        {
            metac_type_header_t header = {decl_type_template_instance, 0, hash, 0};
            metac_type_template_t key = {
                header,
                zeroIdx,
                symbol,
                tInst->Arguments, // we may need to copy the args here?
                nArguments,
            };
            result = MetaCTypeTable_GetOrEmptyTemplateType(&self->TemplateTypeTable, &key);
            if (result.v == 0)
            {
                result = MetaCTypeTable_AddTemplateType(&self->TemplateTypeTable, &key);
            }
        }
    }
    MetaCSemantic_PopTemporaryScope(self);
}
```

Read the comment at line 1367 carefully: *"before calling semantic we need
to mount the template instance scope."* The scope that is pushed is empty
and unused. The **intended** design — and the missing step — is to bind the
actual arguments to the parameter names in that scope and re-run the
declaration's semantic with the placeholders replaced. That re-run is what
"instantiation" means in MetaC.

### 6.3 Three concrete defects in this path

**(a) The template name is never looked up.** `symbol` is hard-coded to
`NULL` at line 1378. The hash therefore covers only the arguments
(`hash = ~0` at line 1380). Since the type table returns an existing slot
when the hash matches (`semantic/metac_type_table.c:128-133`),
`Foo!(int)` and `Bar!(int)` **alias each other** — they get the same type
index.

**(b) The arguments are not copied.** `tInst->Arguments` is a parse-tree
`expr_argument_t*` chain, stored into a field declared `metac_expr_t*`
(the type pun is visible in the source comment). The table slot therefore
points into parse-tree memory, and `TemplateSlotsEqual` reinterprets the
linked list as a flat array of `metac_expr_t`:

```c
// semantic/metac_type_table.h:220-223
for(uint32_t i = 0; i < argumentCount; i++)
{
   metac_expr_t argA = slotA->Arguments[i];
   metac_expr_t argB = slotB->Arguments[i];
```

For `i > 0` this is an out-of-bounds read of a linked list.

**(c) Slot equality is a stub.** `MetaCExpr_Equal` always returns `false`:

```c
// semantic/metac_type_table.h:193-198
static inline const bool MetaCExpr_Equal(const struct metac_expr_t* a,
                                         const struct metac_expr_t* b)
{
    // assert(0);
    return false;
}
```

So `TemplateSlotsEqual` always returns `false` for any instance with at
least one argument, and `GetOrEmptyTemplateType` never finds an existing
entry.

**(d) The instance node's hash is never set.** In the parser, `tinst->Hash`
is never assigned (`parser/metac_parser.c:1085-1105` computes a local `hash`
but discards it; `AllocNewDecl` zero-fills the node). This matters because
the semantic waiter mechanism keys on the node hash.

### 6.4 The template type table

The table is registered through the X-macro list alongside all other type
tables:

```c
// semantic/metac_semantic.h:68-77
#define FOREACH_TYPE_TABLE(M) \
    M(enum,         EnumTypeTable,             enum) \
    M(array,        ArrayTypeTable,           array) \
    M(aggregate,    StructTypeTable,         struct) \
    M(ptr,          PtrTypeTable,               ptr) \
    M(aggregate,    UnionTypeTable,           union) \
    M(typedef,      TypedefTypeTable,       typedef) \
    M(functiontype, FunctionTypeTable, functiontype) \
    M(tuple,        TupleTypeTable,           tuple) \
    M(template,     TemplateTypeTable,     template)
```

and the slot comparator is supplied by the same list:

```c
// semantic/metac_type_table.h:282
M(template, Template, TemplateSlotsEqual)
```

which is what generates `MetaCTypeTable_GetOrEmptyTemplateType` and
`MetaCTypeTable_AddTemplateType`.

---

## 7. Scopes

### 7.1 Ownership kinds

```c
// semantic/metac_scope.h:10-23
#define FOREACH_SCOPE_OWNER(M) \
    M(scope_owner_unknown   , 0x0) \
    /* unused for now */ \
    M(scope_owner_module    , 0x1) \
    M(scope_owner_function  , 0x2) \
    M(scope_owner_struct    , 0x3) \
    M(scope_owner_statement , 0x4) \
    M(scope_owner_block     , 0x5) \
    M(scope_owner_union     , 0x6) \
    M(scope_owner_extended  , 0x7) \
    M(scope_owner_enum      , 0x9) \
    M(scope_owner_template  , 0xA) \
```

`scope_owner_template` is set in exactly one place
(`semantic/metac_type_semantic.c:1206`), and always with index 0 — the scope
does not record *which* template it belongs to.

### 7.2 Temporary scopes

```c
// semantic/metac_scope.h:114-127
typedef enum metac_scope_flags_t
{
    scope_flag_none,
    scope_flag_temporary = (1 << 0),
    scope_flag_mounted   = (1 << 1),
    scope_flag_closed    = (1 << 2),
    scope_flag_max = (1 << 3)
} metac_scope_flags_t;
```

Push/pop are assertion-guarded (`semantic/metac_semantic.c:244-277`):

```c
metac_scope_t* MetaCSemantic_PushTemporaryScope_(metac_sema_state_t* self,
                                                 metac_scope_t* tmpScope,
                                                 uint32_t line,
                                                 const char* file)
{
    assert((tmpScope->ScopeFlags & scope_flag_temporary) != 0);
    assert ((!self->TemporaryScopeDepth) || IsTemporaryScope(self->CurrentScope));
    self->TemporaryScopeDepth++;
    tmpScope->Parent = self->CurrentScope;
    self->CurrentScope = tmpScope;
    return tmpScope;
}
```

### 7.3 Lookup

Lookup walks the scope chain from the current scope upwards
(`semantic/metac_semantic.c:877-904`). A template parameter resolves
because it was registered in the pushed `scope_owner_template` scope.

Note the shape of the result: the lookup yields an **expression** node
(`expr_unknown_value`), not a type node. Section 5.4 is the code that
converts one into the other.

### 7.4 Deferred resolution

The semantic layer has a general mechanism for deferring work that cannot
complete yet: a waiter list keyed by `(function hash, node hash)` with
continuations (`semantic/metac_semantic.c:939-953`,
`semantic/metac_semantic.h:39-53`). A lookup that cannot be satisfied
yields "unresolved" instead of erroring, and dependent nodes are re-evaluated
when the dependency is resolved. Template placeholders are designed to
participate in this scheme.

---

## 8. Printing

The parse-tree printer round-trips template syntax correctly.

Type position (`printer/metac_printer.c:645-668`):

```c
case decl_type_template_instance:
LprintTemplateInstance:
    {
        decl_type_template_instance_t* tinst = cast(decl_type_template_instance_t*) type;
        const int32_t argumentCount = cast(int32_t)tinst->ArgumentCount;
        expr_argument_t* arg = tinst->Arguments;
        PrintIdentifier(self, tinst->Identifier);
        PrintTokenType(self, tok_bang);
        PrintTokenType(self, tok_lParen);
        …
```

Aggregate declaration (`printer/metac_printer.c:1218-1222`):

```c
if (struct_->Parameters != 0)
{
    PrintTokenType(self, tok_bang);
    PrintParameterList(self, struct_->Parameters);
}
```

Function declaration (`printer/metac_printer.c:1286-1290`):

```c
if (function_->TemplateParameterCount)
{
    PrintTokenType(self, tok_bang);
    PrintParameterList(self, function_->TemplateParameters);
}
```

Expression position (`printer/metac_printer.c:1506-1511`):

```c
else if (expr->Kind == expr_template_instance)
{
    PrintExpr(self, expr->E1);
    PrintChar(self, '!');
    PrintExpr(self, expr->E2);
}
```

Two printer gaps:

* The `@run` marker is never printed (`FuncFlag_Run` is not consulted
  anywhere in the printer), so `int f!(T) @run { }` prints as
  `int f!(T) () { }`. This is lossy.
* `PrintSemaType` has **no** `type_index_template` case, so printing a
  semantic tree containing a template type aborts:

```c
// printer/metac_printer.c:1830-1834
default:
{
    fprintf(stderr, "Invalid type_index_kind: %x\n", TYPE_INDEX_KIND(typeIndex));
    assert(0);
}
```

---

## 9. Current state: what works and what does not

### 9.1 Works

| Area | Where |
|---|---|
| Lexing `!` | `parser/metac_lexer.h:138` |
| Parse `T!(args)` in type position | `parser/metac_parser.c:1082-1105` |
| Parse `struct S!(T) { }` | `parser/metac_parser.c:1185-1192` |
| Parse `f!(T)` on a function header | `parser/metac_parser.c:1989-1994` |
| Parse `x!(1,2)` in expression position | `parser/metac_expr_parser.c:147,1597-1605,1854-1856` |
| Print all of the above | `printer/metac_printer.c:645,1218,1286,1506` |
| Bind template parameters to unknown-value placeholders | `semantic/metac_type_semantic.c:1201-1231` |
| Propagate unknown values through expressions | `semantic/metac_expr_semantic.c:982-1025` |
| Use a template parameter as a type | `semantic/metac_type_semantic.c:1494-1498` |
| Allocate an instance record in the type table | `semantic/metac_type_semantic.c:1396-1400` |

### 9.2 Does not work

| Gap | Where |
|---|---|
| No body substitution / instantiation | `semantic/metac_type_semantic.c:1377-1403` |
| Template name never looked up → different templates alias | `semantic/metac_type_semantic.c:1378` |
| Arguments not copied out of the parse tree | `semantic/metac_type_semantic.c:1393` |
| `TemplateSlotsEqual` reads a linked list as an array | `semantic/metac_type_table.h:220-223` |
| `MetaCExpr_Equal` is a stub returning `false` | `semantic/metac_type_table.h:193-198` |
| Instance node hash never assigned | `parser/metac_parser.c:1085-1105` |
| Function template parameters discarded for non-`@run` functions | `parser/metac_parser.c:1994-2020` |
| Function template bodies never instantiated | `semantic/metac_semantic.c:1053-1056` |
| No size/layout for template types → `assert(0)` | `semantic/metac_type_semantic.c:547-550` |
| No semantic for expression-position templates → `assert(0)` | `semantic/metac_expr_semantic.c:707-715` |
| No semantic printer for template types → `assert(0)` | `printer/metac_printer.c:1830-1834` |
| **No code generation whatsoever** | `codegen/` contains no mention of "template" |

The codegen situation is worth stating plainly: `codegen/metac_codegen.c`
has zero occurrences of the word "template". `MetaCCodegen_GetBCType` has no
`type_index_template` case and falls into:

```c
// codegen/metac_codegen.c:145-151
else
{
    printf("cannot convert typekind: {%s, %u}\n",
        MetaCTypeIndexKind_toChars(TYPE_INDEX_KIND(typeIdx)),
                                   TYPE_INDEX_INDEX(typeIdx));
    assert(0);
}
```

A template type can therefore never reach codegen. Template instances must
be *resolved into concrete types* before codegen sees them — which is
precisely the missing "instantiate" step.

### 9.3 A note on X-macro discipline

`type_index_template` was added to `FOREACH_TYPE_INDEX_KIND`
(`semantic/metac_type.h:11-28`), and per the rule in `X_MACROS.md` — "check
all expansions" — every switch that consumes the kind should have been
updated. Two were (`NodeFromTypeIndex`,
`semantic/metac_semantic.c:1241`); four were not (`SetTypeIndex`,
`MetaCSemantic_GetTypeSize`, `MetaCCodegen_GetBCType`, `PrintSemaType`).
This is the single most instructive gap in the current implementation: an
X-macro gives you the enum member for free but does *not* give you the
exhaustive switch. Adding a new type-index kind to a compiler with
X-macros means auditing every `TYPE_INDEX_KIND` switch by hand.

### 9.4 Tests

There is currently **no test anywhere in the repository that exercises a
template**. `parser/metac_decl_test.c` knows about
`decl_type_template_instance` in its shape dumper
(`parser/metac_decl_test.c:269-270, 321-322`) but its ~55 test cases contain
no `!` at all.

`run_tests.sh` builds `lexer_test` and `parser_test`; `parser_test` is
compiled with `-DNO_SEMANTIC -DNO_FIBERS -DNO_PREPROCESSOR`, so it cannot
exercise the semantic layer even in principle. The REPL is the de-facto
integration surface.

---

## 10. Comparison with other template systems

### 10.1 Overview table

| Aspect | C++ | D | Rust | C macros | Haskell type classes | **MetaC (current)** |
|---|---|---|---|---|---|---|
| Instantiation syntax | `Name<Args>` | `Name!(Args)` | `Name<T>` (angle) / `Name::<T>` (turbofish) | textual | `instance C T` | `Name!(Args)` (D-style) |
| Parameter list syntax | `<class T, int N>` | `alias T = int` | `<T: Bound>` | — | `class C a` | `!(T, int N)` |
| Instantiation timing | lazy, on use | on use | monomorphised on use | preprocessor | compile-time, at instance decl | **never (not implemented)** |
| Instance identity | mangled name | hash of `(Template, Args)` | hash of `(Item, Args)` | text | class name | hash of args only — **template name ignored** |
| Body substitution | yes (AST clone) | yes (semantic re-entry) | yes (MIR-level monomorphisation) | textual | no (dictionary) | **no** |
| Deduction | yes | yes | yes (trait bounds) | no | no | no |
| Constraint checking | concepts (C++20) | `if` constraints | traits | no | class system | no |
| Error at instantiation | diagnostic | diagnostic | diagnostic | none | none | n/a |
| Runtime cost of genericity | zero (monomorphised) | zero | zero | zero | dictionary (usually erased) | n/a |
| Code size | one copy per instantiation | one copy per instantiation | one copy per instantiation | one copy per expansion | one copy per instance | n/a |

### 10.2 C++ templates

C++ is the reference point most people carry in their head, and MetaC
differs from it in one fundamental way: **C++ templates are a syntactic
feature of the language proper, whereas in MetaC a template is a
*declaration in a scope* that is processed by the normal semantic
machinery, in a partial way, and is expected to be re-processed later.**

Concretely:

* **C++ performs substitution on a cloned AST.** Every use of
  `Vector<int>` clones the template's token tree/AST and substitutes the
  template arguments for the template parameters, then runs the *whole*
  semantic analysis again on the clone. MetaC's intended design re-runs
  only the *declaration's* semantic analysis, with the parameter scope
  pre-populated with the actual arguments — a much narrower operation.
* **C++ has two-phase lookup**, with `typename` and `template` disambiguators
  required because a dependent name may or may not be a type at template
  definition time. MetaC sidesteps this entirely: names that cannot be
  resolved at declaration time become unknown-value placeholders, and the
  question "is this a type or a value?" is answered *later*, when the
  instantiation supplies the answer. MetaC has no `typename` keyword
  because it has no ambiguity to disambiguate — the placeholder carries
  both a type index and the possibility of being a value.
* **C++ instantiation is a hard error at the point of instantiation.**
  MetaC's philosophy is the opposite: the semantic layer is allowed to be
  *partial*, and unresolved parts are recorded as placeholders and resolved
  later (§5.1, §7.4). This is closer to how a linker works than how a C++
  compiler works.
* **C++ has no concept of a "template type" in the type system** — a
  `Vector<int>` is a `Vector<int>`, full stop. MetaC *does* have a distinct
  type-index kind for template instances (`type_index_template`), meaning a
  template instance is a first-class type that must be reduced to a
  concrete type before it can be laid out or compiled. This is the design
  that makes `MetaCTypeTable_GetOrEmptyTemplateType` possible, and it is
  D-like rather than C++-like.
* **C++ mangling encodes the arguments.** MetaC's intended key is a hash
  over `(symbol hash, argument hashes)`, which is D-like. The current
  implementation omits the symbol half (§6.3a), which is the bug.

### 10.3 D templates

MetaC's syntax and its intended instance-identity scheme are directly
borrowed from D:

* D writes `Name!(Args)`; MetaC does the same.
* D identifies a template instance by a hash over the template symbol and
  the compile-time argument hashes, storing the instance in a table so that
  `Vector!(int)` and `Vector!(int)` are the *same* type. MetaC has the same
  table (`TemplateTypeTable`) and the same intended key
  (`semantic/metac_type_semantic.c:1380-1386`).
* D re-runs the *semantic* of the template body per instantiation rather
  than cloning a token tree. MetaC's intended design is the same: re-run
  `MetaCSemantic_doTypeSemantic` for the aggregate with the arguments bound
  (§6.2, and the TODO in §11).

The differences:

* D's parameter syntax is `alias T = int` — a parameter with a *default*;
  MetaC reuses the ordinary C parameter syntax (`T`, `int N`).
* D has a full `if` constraint system and an `alias`/`static if`/`template`
  suite for compile-time computation; MetaC has none of this.
* D distinguishes *template parameters* from *compile-time function
  arguments* explicitly; MetaC has only one kind of parameter list.
* MetaC additionally has the `auto` mechanism (§2.2), which D does not
  have.

### 10.4 Rust generics

Rust's angle brackets are syntactically similar to C++ but semantically
closer to what MetaC is aiming for:

* Rust generics are **monomorphised**: each concrete instantiation is a
  separate compiled artifact, and there is no runtime type-erasure (except
  via `dyn`).
* Rust's trait system supplies the *constraints*, and trait resolution is
  separate from generic instantiation.
* Rust never has a "generic type" at runtime; `Vec<T>` is not a runtime
  value.

MetaC resembles Rust in two specific ways:

* the plan is monomorphisation, not type erasure;
* the `type_index_template` kind is a *compile-time-only* kind that must be
  eliminated before code generation — the same kind of obligation Rust's
  monomorphisation pass has.

MetaC differs from Rust in that the "trait" role is filled by the ordinary
type system (there are no traits) and the constraint role is not filled at
all.

### 10.5 The C preprocessor / C macros

This is the most important contrast, because it is the alternative that
MetaC's design is *not* choosing.

* **Textual expansion.** Macros expand text, before parsing. A macro cannot
  introduce a new scope, cannot be partially applied, cannot be
  overloaded, and cannot reason about types. `MAX(a,b)` expands to text
  that may or may not type-check depending on context.
* **MetaC templates are semantic objects.** They live in a scope, they are
  found by name lookup, they participate in the type system, and they
  produce a *type* — `Vector!(int)` is a type with a layout, an index in the
  type table, and a size.
* **MetaC's templates are not textual.** The instance record stores semantic
  hashes, not source text. The printer can regenerate syntax from the
  semantic tree because the semantic tree is genuinely richer than text.
* **Type-level vs value-level.** A C macro like `#define VECTOR(T) struct { T* d; }` is a text template; MetaC's `struct Vector!(T)` is a *type* template. The distinction shows up in that a MetaC template can be named, aliased, stored in a typedef, and passed to other templates.

The `!` syntax itself is a nod to the preprocessor family of languages
(Ruby, Julia, Crystal, Nim): `!` is used as the "apply/instantiate"
operator precisely because it is not otherwise used for anything in C-like
syntax. MetaC's `!=` lexes separately, so there is no conflict.

### 10.6 Haskell type classes

The opposite end of the spectrum: type classes are *runtime dictionary
passing*, not monomorphisation.

* A type class instance is resolved by the type checker and passed as an
  extra dictionary argument at every call site.
* The *same* function body is shared by all instances; the class method
  dispatches on the dictionary.
* The instance declaration (`instance C T`) is the analogue of MetaC's
  "instance record" — but in Haskell the record contains *methods*, not
  arguments.

MetaC's `auto` mechanism (§2.2) is the closest thing in MetaC to a type
class: the body is shared, `typeof` is the analogue of a dictionary, and
dispatch happens at runtime via `.Compiler->GetTypeKind`.

### 10.7 Lisp / Scheme macros

Lisp macros operate on the *representation* of the program (s-expressions),
not on its text. They are a useful contrast because they are the other
common answer to "how do you parameterize a language".

* Lisp macros are hygienic and run at compile time over the whole program
  form.
* They can rewrite *any* part of the language, including adding new
  syntax.
* They are not first-class types: a macro produces code, not a type.

MetaC's templates are *not* macros: they cannot rewrite syntax, they are
resolved by name lookup into a scoped declaration, and they produce a type
rather than code.

### 10.8 Julia parametric types and Zig comptime

* **Julia** has `struct Vector{T} … end` and `Vector{Int}` instantiation,
  which is very close to MetaC's shape — except Julia's `{}` (and `[]` for
  tuples) rather than `!()`, and Julia's parameter is a *value* type
  parameter, whereas MetaC's parameter is a plain declaration that can be a
  type or a value.
* **Zig** has `fn foo(comptime T: type) …` and `T{}` for anonymous
  struct literals. Zig's `comptime` is closer to MetaC's *`auto`* mechanism
  than to MetaC's `!` mechanism, because Zig's generic functions are
  evaluated at comptime and take their arguments as *values* (types as
  values), whereas MetaC's `!` templates are nominally *types*.

### 10.9 Summary of the design position

MetaC's template system sits at a specific point:

* **Syntax**: D-like (`!`), C-like parameters.
* **Instantiation semantics**: D-like (re-run semantic per instance), Rust-like
  (monomorphised, no runtime dictionary).
* **Type system integration**: stronger than C++/D — a template instance has
  its own type-index kind and its own table slot.
* **Name resolution**: unlike C++, a single-pass lookup with placeholders
  rather than two-phase lookup; unlike D, no `if` constraints.
* **Error model**: unlike C++, unresolved is not an error; it is a deferred
  placeholder.

And the two mechanisms, `!` and `auto`, deliberately cover the two ends of
the monomorphization/type-erasure trade-off.

---

## 11. Roadmap

The work plan for making the template system functional lives in a separate
document: **`TEMPLATE_ROADMAP.md`**.

It covers the acceptance criterion, the phases (test harness, instance
identity, instantiation, type-system integration, function templates,
expression-position templates, codegen, printer, tests), the dependency
graph between them, the open design questions, and a table of every defect
the roadmap addresses with its location.

---

## 12. File pointer index

**Parser**
* `parser/metac_node.h:37` — `decl_type_template_instance` in `FOREACH_DECL_KIND`
* `parser/metac_node.h:157` — `expr_template_instance` in `FOREACH_EXP`
* `parser/metac_node.h:163` — `expr_unknown_value`
* `parser/metac_parsetree.h:59-65` — `expr_argument_t`
* `parser/metac_parsetree.h:556-569` — `decl_function_t::TemplateParameters`
* `parser/metac_parsetree.h:642-657` — `decl_type_struct_t::Parameters`
* `parser/metac_parsetree.h:713-724` — `decl_type_template_instance_t`
* `parser/metac_parser.c:914-988` — `ParseParameterList`
* `parser/metac_parser.c:1082-1105` — type-position `name!(args)`
* `parser/metac_parser.c:1185-1192` — aggregate template declaration
* `parser/metac_parser.c:1989-1994, 2009-2010` — function template parameters
* `parser/metac_expr_parser.c:147, 325-332, 1597-1605, 1676-1680, 1854-1856` — expression-position `!`
* `parser/metac_preproc.c:277-283` — preprocessor cannot evaluate templates

**Semantic**
* `semantic/metac_type.h:11-28` — `FOREACH_TYPE_INDEX_KIND` (`type_index_template`, `type_index_unknown`)
* `semantic/metac_type.h:208-219` — `metac_type_template_t`
* `semantic/metac_semantic.h:68-77` — `FOREACH_TYPE_TABLE` (`TemplateTypeTable`)
* `semantic/metac_semantic.c:244-277` — temporary scope push/pop
* `semantic/metac_semantic.c:877-904` — identifier lookup
* `semantic/metac_semantic.c:1018-1023` — `auto` implies template (diagnostic)
* `semantic/metac_semantic.c:1241-1242` — `NodeFromTypeIndex` → `type_index_template`
* `semantic/metac_scope.h:10-23` — `scope_owner_template`
* `semantic/metac_scope.h:114-127` — scope flags
* `semantic/metac_type_semantic.c:63-78` — `IsUnknownType`
* `semantic/metac_type_semantic.c:1201-1231` — template parameter placeholders
* `semantic/metac_type_semantic.c:1356-1405` — template instance semantic
* `semantic/metac_type_semantic.c:1494-1498` — unknown-value → type index
* `semantic/metac_type_semantic.c:547-550` — missing template size case
* `semantic/metac_expr_semantic.c:506-509` — unknown-value pass-through
* `semantic/metac_expr_semantic.c:982-1025` — unknown-value propagation
* `semantic/metac_expr_semantic.c:707-715` — no expression-template semantic
* `semantic/metac_sematree.h:127-131` — unknown-value payload
* `semantic/metac_type_table.h:193-198` — `MetaCExpr_Equal` stub
* `semantic/metac_type_table.h:200-239` — `TemplateSlotsEqual`
* `semantic/metac_type_table.h:282` — template table member

**Printer**
* `printer/metac_printer.c:645-668` — type-position instance
* `printer/metac_printer.c:1218-1222` — aggregate template declaration
* `printer/metac_printer.c:1286-1290` — function template declaration
* `printer/metac_printer.c:1506-1511` — expression-position instance
* `printer/metac_printer.c:1830-1834` — missing template type case

**Codegen**
* `codegen/metac_codegen.c:41-153` — `MetaCCodegen_GetBCType` (no template case)
* `codegen/metac_codegen.c:46` — refuses unknown/unresolved types
