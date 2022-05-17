#include "metac_semantic.h"
#include <assert.h>
#include "metac_alloc_node.h"

#define AT(...)

bool IsExpressionNode(metac_node_kind_t);


static inline bool isBasicType(metac_type_kind_t typeKind)
{
    if ((typeKind >= type_void) & (typeKind <= type_unsigned_long_long))
    {
        return true;
    }
    return false;
}

static uint32_t _nodeCounter = 64;

void MetaCSemantic_Init(metac_semantic_state_t* self, metac_parser_t* parser)
{
    TypeTableInitImpl((metac_type_table_t*)&self->ArrayTypeTable,
                      sizeof(metac_type_array_slot_t),
                      type_index_array);

    TypeTableInitImpl((metac_type_table_t*)&self->StructTypeTable,
                      sizeof(metac_type_struct_slot_t),
                      type_index_struct);

    TypeTableInitImpl((metac_type_table_t*)&self->PtrTypeTable,
                      sizeof(metac_type_ptr_slot_t),
                      type_index_ptr);

    IdentifierTableInit(&self->SemanticIdentifierTable);
    self->ParserIdentifierTable = &parser->IdentifierTable;

    self->ExpressionStackCapacity = 64;
    self->ExpressionStack = malloc(
        sizeof(metac_sema_expression_t) * self->ExpressionStackCapacity);
    self->ExpressionStackSize = 0;

    self->ScopeStackCapacity = 64;
    self->ScopeStack = malloc(
        sizeof(metac_scope_t) * self->ExpressionStackCapacity);
    self->ScopeStackSize = 0;

    self->CurrentDeclarationState = 0;

    MetaCPrinter_Init(&self->Printer, &self->SemanticIdentifierTable, &parser->StringTable);
}

metac_sema_statement_t* MetaCSemantic_doStatementSemantic(metac_semantic_state_t* self,
                                                          metac_statement_t* stmt)
{
    metac_sema_statement_t* result;

    switch (stmt->StmtKind)
    {
        case stmt_exp:
        {
            sema_stmt_exp_t* sse = AllocNewSemaStatement(stmt_exp, &result);
        } break;

        default: assert(0);

        case stmt_block:
        {
            sema_stmt_block_t* ssb = AllocNewSemaStatement(stmt_block, &result);
            metac_scope_parent_t parent = {SCOPE_PARENT_V(scope_parent_stmt, StatementIndex(ssb))};
            self->CurrentScope = MetaCScope_PushScope(self->CurrentScope, parent);
            //for(int i = 0; i < ssb->Body.)
        } break;
    }

    return result;
}

metac_type_index_t MetaCSemantic_doTypeSemantic(metac_semantic_state_t* self,
                                                decl_type_t* type)
{
    metac_type_index_t result = {0};

    if (type->TypeIdentifier.v)
    {
        printf("Type: %s\n", IdentifierPtrToCharPtr(self->ParserIdentifierTable, type->TypeIdentifier));
        //self->ParserIdentifierTable->
    }

    return result;
}

void MetaCSemantic_doParameterSemantic(metac_semantic_state_t* self,
                                       sema_decl_function_t* func,
                                       sema_decl_variable_t *result,
                                       decl_parameter_t* param)
{
    uint32_t paramIndex = result - func->Parameters;

    result->VarIdentifier = param->Identifier;
    result->VarType = MetaCSemantic_doTypeSemantic(self, param->Type);
    result->VarInitExpression = 0;
}

sema_decl_function_t* MetaCSemantic_doFunctionSemantic(metac_semantic_state_t* self,
                                                       decl_function_t* func)
{
    // one cannot do nested function semantic at this point
    // assert(self->CurrentDeclarationState == 0);
    metac_sema_decl_state_t declState = {0};
    self->CurrentDeclarationState = &declState;

    sema_decl_function_t* f =
        AllocNewSemaFunction(func);
    // let's first do the parameters
    sema_decl_variable_t* params = f->Parameters =
        AllocFunctionParameters(f, func->ParameterCount);

    decl_parameter_t* currentParam = func->Parameters;
    for(int i = 0;
        i < func->ParameterCount;
        i++)
    {
        MetaCSemantic_doParameterSemantic(self, f,
                                          params + i,
                                          currentParam);
        currentParam = currentParam->Next;
    }

    assert(currentParam == emptyPointer);
    metac_scope_parent_t Parent = {SCOPE_PARENT_V(scope_parent_function, FunctionIndex(f))};

    f->Scope = MetaCScope_PushScope(self->CurrentScope, Parent);

    f->FunctionBody =
        MetaCSemantic_doStatementSemantic(self, func->FunctionBody);

    return f;
}

metac_sema_declaration_t* MetaCSemantic_doDeclSemantic(metac_semantic_state_t* self,
                                                       metac_declaration_t* decl)
{
    metac_sema_declaration_t* result;

    switch(decl->DeclKind)
    {
        case decl_function:
        {
            decl_function_t* f = cast(decl_function_t*) decl;
            result = (metac_sema_declaration_t*)
                MetaCSemantic_doFunctionSemantic(self, f);

        } break;
        case decl_variable:
        {
            decl_variable_t* v = cast(decl_variable_t*) decl;
        } break;
    }
}

#ifndef ATOMIC
#define INC(v) \
    (v++)
#else
#define INC(v)
    (__builtin_atomic_fetch_add(&v, __ATOMIC_RELEASE))
#endif

metac_type_index_t MetaCSemantic_GetTypeIndex(metac_semantic_state_t* state,
                                              metac_type_kind_t typeKind,
                                              decl_type_t* type)
{
    if (isBasicType(typeKind))
    {
        return (metac_type_index_t) {
            TYPE_INDEX_V(type_index_basic, (uint32_t) typeKind)
        };
    }

    return (metac_type_index_t) {0};
}

metac_type_index_t MetaCSemantic_GetArrayTypeOf(metac_semantic_state_t* state,
                                                metac_type_index_t elementTypeIndex,
                                                uint32_t dimension)
{
    uint32_t hash = EntangleInts(TYPE_INDEX_INDEX(elementTypeIndex), dimension);
    metac_type_array_slot_t key = {hash, elementTypeIndex, dimension};

    metac_type_index_t result =
        MetaCTypeTable_GetOrAddArrayType(&state->ArrayTypeTable, hash, &key);

    return result;
}

metac_type_index_t MetaCSemantic_GetPtrTypeOf(metac_semantic_state_t* state,
                                              metac_type_index_t elementTypeIndex)
{
    uint32_t hash = elementTypeIndex.v;
    metac_type_ptr_slot_t key = {hash, elementTypeIndex};

    metac_type_index_t result =
        MetaCTypeTable_GetOrAddPtrType(&state->PtrTypeTable, hash, &key);

    return result;
}

#include "metac_printer.h"
static inline const char* BasicTypeToChars(metac_type_index_t typeIndex)
{
    assert(TYPE_INDEX_KIND(typeIndex) == type_index_basic);
    switch((metac_type_kind_t) TYPE_INDEX_INDEX(typeIndex))
    {
        case type_invalid :
            assert(0);

        case type_void :
            return "void";

        case type_bool :
            return "bool";
        case type_char:
            return "char";

        case type_unsigned_int:
            return "unsigned int";
        case type_unsigned_long :
            return "long";
        case type_unsigned_long_long:
            return "unsigned long long";

        case type_long :
            return "long";
        case type_long_long:
            return "long long";
        case type_int :
            return "int";
        case type_float :
            return "float";
    }
    return 0;
}
#ifndef _emptyPointer
#define _emptyPointer 0x1
#define emptyNode (metac_node_header_t*) _emptyPointer
#endif


/// Returns _emptyNode to signifiy it could not be found
/// a valid node otherwise
metac_node_header_t* MetaCSemantic_LookupIdentifier(metac_semantic_state_t* self,
                                                    uint32_t identifierKey,
                                                    metac_identifier_ptr_t identifierPtr)
{
    metac_node_header_t* result = emptyNode;
#if 1
    if (self->ScopeStackSize == 0 && self->declStore)
    {
        metac_identifier_ptr_t dStoreIdPtr =
            FindMatchingIdentifier(&self->declStore->Table,
                                   self->ParserIdentifierTable,
                                   identifierPtr);
        if (dStoreIdPtr.v)
        {
            metac_declaration_t* decl =
                DeclarationStore_GetDecl(self->declStore, dStoreIdPtr);
        }
    }
    else
#endif
    {
        assert(self->ScopeStackSize >= 1);
        uint32_t StackTopIdx = self->ScopeStackSize;
        //TODO do an LRU lookup first
        while(StackTopIdx)
        {
            metac_scope_t* currentScope = &self->ScopeStack[--StackTopIdx];
            metac_node_header_t* lookupResult =
                MetaCScope_LookupIdentifier(currentScope, identifierKey, identifierPtr);
            if (lookupResult)
            {
                result = lookupResult;
                break;
            }
        }
    }
    return result;
}

static inline void TypeToCharsP(metac_semantic_state_t* self,
                                metac_printer_t* printer,
                                metac_type_index_t typeIndex)
{
    uint32_t typeIndexIndex = TYPE_INDEX_INDEX(typeIndex);

    switch (TYPE_INDEX_KIND(typeIndex))
    {
        case type_index_array:
        {
            metac_type_array_slot_t* arrayType =
                (self->ArrayTypeTable.Slots + TYPE_INDEX_INDEX(typeIndex));
            TypeToCharsP(self, printer, arrayType->ElementTypeIndex);
            MetacPrinter_PrintStringLiteral(printer, "[");
            MetacPrinter_PrintI64(printer, (int64_t)arrayType->Dimension);
            MetacPrinter_PrintStringLiteral(printer, "]");
        } break;
        case type_index_basic:
        {
            const char* typeString = BasicTypeToChars(typeIndex);
            MetacPrinter_PrintStringLiteral(printer, typeString);
        } break;
        case type_index_ptr:
        {
            metac_type_ptr_slot_t* ptrType =
                (self->PtrTypeTable.Slots + TYPE_INDEX_INDEX(typeIndex));
            TypeToCharsP(self, printer, ptrType->ElementTypeIndex);
            MetacPrinter_PrintStringLiteral(printer, "*");
        } break;
    }
}

const char* TypeToChars(metac_semantic_state_t* self, metac_type_index_t typeIndex)
{
    const char* result = 0;
    static metac_printer_t printer = {0};
    if (!printer.StringMemory)
        MetaCPrinter_InitSz(&printer, self->ParserIdentifierTable, 0, 32);
    else
        MetaCPrinter_Reset(&printer);
    TypeToCharsP(self, &printer, typeIndex);
    printer.StringMemory[printer.StringMemorySize++] = '\0';
    result = printer.StringMemory;

    return result;
}

void MetaCSemantic_PushExpr(metac_semantic_state_t* self, metac_sema_expression_t* expr)
{
    if (self->ExpressionStackCapacity < self->ExpressionStackSize)
    {
        assert(0);
        // we would need to realloc in this case.
    }
}

void MetaCSemantic_PopExpr(metac_semantic_state_t* self,  metac_sema_expression_t* expr)
{

}

bool MetaCSemantic_CanHaveAddress(metac_semantic_state_t* self,
                                  metac_expression_t* expr)
{
    switch (expr->Kind)
    {
        case exp_identifier:
            return true;
        default: return false;
    }
}
#include <stdio.h>


#undef offsetof

#define offsetof(st, m) \
    ((size_t)((char *)&((st *)0)->m - (char *)0))

metac_sema_expression_t* MetaCSemantic_doExprSemantic(metac_semantic_state_t* self,
                                                      metac_expression_t* expr)
{
    metac_sema_expression_t* result = 0;

    result = AllocNewSemaExpression(expr);

    if (IsBinaryExp(expr->Kind))
    {
        MetaCSemantic_PushExpr(self, result);

        MetaCSemantic_doExprSemantic(self, expr->E1);
        MetaCSemantic_doExprSemantic(self, expr->E2);

        MetaCSemantic_PopExpr(self, result);
    }

    switch(expr->Kind)
    {
        case exp_invalid:
            assert(0);

        case exp_char :
            result->TypeIndex = MetaCSemantic_GetTypeIndex(self, type_char, 0);
        break;
        case exp_string :
            result->TypeIndex = MetaCSemantic_GetArrayTypeOf(self,
                MetaCSemantic_GetTypeIndex(self, type_char, 0),
                LENGTH_FROM_STRING_KEY(expr->StringKey));
        break;
        case exp_signed_integer :
            result->TypeIndex = MetaCSemantic_GetTypeIndex(self, type_int, 0);
        break;
        case exp_identifier:
        {
            metac_node_header_t* node =
                MetaCSemantic_LookupIdentifier(self,
                                               result->IdentifierKey,
                                               result->IdentifierPtr);
            if (IsExpressionNode(node->Kind))
            {
                result = (metac_sema_expression_t*) node;
                if (node->Kind == exp_identifier)
                {
                    fprintf(stderr, "Identifier lookup failed\n");
                }
            }
            //assert(0);
            //
        }
        break;
        case exp_addr:
            MetaCSemantic_PushExpr(self, result);
            result->E1 = MetaCSemantic_doExprSemantic(self, expr->E1);
            MetaCSemantic_PopExpr(self, result);
            assert(result->E1->TypeIndex.v != 0 && result->E1->TypeIndex.v != ERROR_TYPE_INDEX_V);
            if (!MetaCSemantic_CanHaveAddress(self, expr->E1))
            {
                result->TypeIndex.v = ERROR_TYPE_INDEX_V;
                SemanticError(self, "cannot take the address of %s", MetaCPrinter_PrintExpression(&self->Printer, expr->E1));
            }
            else
            {
                result->TypeIndex = MetaCSemantic_GetPtrTypeOf(self, result->E1->TypeIndex);
            }
        break;
    }

    return result;
}
