#include "metac_semantic.h"
#include <assert.h>
#include "metac_alloc_node.h"
#include "metac_target_info.h"
#include "metac_default_target_info.h"

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

void MetaCSemantic_Init(metac_semantic_state_t* self, metac_parser_t* parser,
                        decl_type_struct_t *compilerStruct)
{
#define INIT_TYPE_TABLE(TYPE_NAME, MEMBER_NAME, INDEX_KIND) \
    TypeTableInitImpl((metac_type_table_t*)&self->MEMBER_NAME, \
                      sizeof(metac_type_ ## TYPE_NAME ## _slot_t), \
                      type_index_## INDEX_KIND);

    FOREACH_TYPE_TABLE(INIT_TYPE_TABLE)

    self->ExpressionStackCapacity = 64;
    self->ExpressionStackSize = 0;
    self->ExpressionStack = calloc(sizeof(metac_expression_t), self->ExpressionStackCapacity);

    IdentifierTableInit(&self->SemanticIdentifierTable, IDENTIFIER_LENGTH_SHIFT);
    self->ParserIdentifierTable = &parser->IdentifierTable;

    self->CurrentScope = 0;
    self->CurrentDeclarationState = 0;

    MetaCPrinter_Init(&self->Printer, self->ParserIdentifierTable, &parser->StringTable);
}

void MetaCSemantic_PopScope(metac_semantic_state_t* self)
{
    assert(self->CurrentScope);
    self->CurrentScope = self->CurrentScope->Parent;
}



metac_scope_t* MetaCSemantic_PushScope(metac_semantic_state_t* self,
                                       metac_scope_parent_kind_t parentKind,
                                       metac_node_t* parentNode)
{
    metac_scope_parent_t scopeParent;
    uint32_t scopeParentIndex = 0;
    switch(parentKind)
    {
    case scope_parent_invalid :
    case scope_parent_extended :
    case scope_parent_unknown :
        assert(0);

    case scope_parent_module :
        assert(0);
    case scope_parent_function :
        scopeParentIndex = FunctionIndex((sema_decl_function_t*)parentNode);
    break;
    case scope_parent_struct :
        scopeParentIndex = StructIndex((sema_type_aggregate_t*)parentNode);
    break;
    case scope_parent_statement :
        scopeParentIndex = StatementIndex((metac_sema_statement_t*)parentNode);
    break;
    case scope_parent_block :
        scopeParentIndex = BlockStatementIndex((sema_stmt_block_t*)parentNode);
    break;
    case scope_parent_union :
        scopeParentIndex = UnionIndex((sema_type_aggregate_t*)parentNode);
    break;
    }
    scopeParent.v = SCOPE_PARENT_V(parentKind, scopeParentIndex);

    self->CurrentScope = MetaCScope_PushScope(self->CurrentScope,
                                              scopeParent);
    return self->CurrentScope;
}

metac_sema_statement_t* MetaCSemantic_doStatementSemantic_(metac_semantic_state_t* self,
                                                           metac_statement_t* stmt,
                                                           const char* callFun,
                                                           uint32_t callLine)
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
            stmt_block_t* blockStatement = (stmt_block_t*) stmt;
            uint32_t statementCount = blockStatement->StatementCount;
            sema_stmt_block_t* semaBlockStatement =
                AllocNewSemaStatement(stmt_block, &result);

            metac_scope_parent_t parent = {SCOPE_PARENT_V(scope_parent_statement,
                                           BlockStatementIndex(semaBlockStatement))};

            MetaCSemantic_PushScope(self,
                                    scope_parent_block,
                                    (metac_node_t*)semaBlockStatement);

            metac_statement_t* currentStatement = blockStatement->Body;
            for(int i = 0;
                i < statementCount;
                i++)
            {
                ((&semaBlockStatement->Body)[i]) =
                    MetaCSemantic_doStatementSemantic(self, currentStatement);
                currentStatement = currentStatement->Next;
            }

            MetaCSemantic_PopScope(self);
        } break;

        case stmt_return:
        {
            stmt_return_t* returnStatement = (stmt_return_t*) stmt;
            sema_stmt_return_t* semaReturnStatement =
                AllocNewSemaStatement(stmt_return, &result);

            metac_sema_expression_t* returnValue =
                MetaCSemantic_doExprSemantic(self, returnStatement->Expression);
            semaReturnStatement->Expression = returnValue;
        } break;
    }

    return result;
}

metac_type_index_t MetaCSemantic_GetTypeIndex(metac_semantic_state_t* state,
                                              metac_type_kind_t typeKind,
                                              decl_type_t* type)
{
    metac_type_index_t result = {0};

    if (isBasicType(typeKind))
    {
        result.v = TYPE_INDEX_V(type_index_basic, (uint32_t) typeKind);

        assert((type == emptyPointer) || type->TypeKind == typeKind);
        if ((type != emptyPointer) && (type->TypeModifiers & typemod_unsigned))
        {
            if((typeKind >= type_char) & (typeKind <= type_long))
            {
                result.v +=
                    (((uint32_t)type_unsigned_char) - ((uint32_t)type_char));
            }
            else if (typeKind == type_long_long)
            {
                result.v +=
                    (((uint32_t)type_unsigned_long_long) - ((uint32_t)type_long_long));
            }
            else
            {
                //TODO Real error macro
                fprintf(stderr, "modifier unsigned cannot be applied to: %s\n", TypeToChars(state, result));
            }
        }
    }

    return result;
}

bool IsAggregateType(metac_type_kind_t TypeKind)
{
    if (TypeKind == type_struct || TypeKind == type_union)
    {
        return true;
    }
    return false;
}

bool IsPointerType(metac_type_kind_t TypeKind)
{
    if (TypeKind == type_ptr || TypeKind == type_functiontype)
    {
        return true;
    }
    return false;
}

#define INVALID_SIZE ((uint32_t)-1)
#ifndef U32
#define U32(VAR) \
	(*(uint32_t*)(&VAR))
#endif
/// Returns size in byte or INVALID_SIZE on error
uint32_t MetaCSemantic_GetTypeSize(metac_semantic_state_t* self,
                                   metac_type_index_t typeIndex)
{
    uint32_t result = INVALID_SIZE;

    if (TYPE_INDEX_KIND(typeIndex) == type_index_basic)
    {
        uint32_t idx = TYPE_INDEX_INDEX(typeIndex);

        if ((idx >= type_unsigned_char)
         && (idx <= type_unsigned_long))
        {
            idx -= ((uint32_t)type_char - (uint32_t)type_unsigned_char);
        }
        else if (idx == type_unsigned_long_long)
        {
            idx = type_long_long;
        }
        result =
            MetaCTargetInfo_GetBasicSize(&default_target_info, (basic_type_kind_t) idx);
    }
    else if (TYPE_INDEX_KIND(typeIndex) == type_index_ptr)
    {
        result = default_target_info.PtrSize;
    }
    else if (TYPE_INDEX_KIND(typeIndex) == type_identifier)
    {

    }
    else
    {
        assert(0);
    }

    return result;
}

uint32_t MetaCSemantic_GetTypeAlignment(metac_semantic_state_t* self,
                                    metac_type_index_t typeIndex)
{
    uint32_t result = INVALID_SIZE;

    if (TYPE_INDEX_KIND(typeIndex) == type_index_basic)
    {
        uint32_t idx = TYPE_INDEX_INDEX(typeIndex);

        if ((idx >= type_unsigned_char)
         && (idx <= type_unsigned_long))
        {
            idx -= ((uint32_t)type_char - (uint32_t)type_unsigned_char);
        }
        else if (idx == type_unsigned_long_long)
        {
            idx = type_long_long;
        }
        result =
            MetaCTargetInfo_GetBasicAlign(&default_target_info, (basic_type_kind_t) idx);
    }
    else
    {
        assert(0);
    }

    return result;
}

static inline uint32_t Align(uint32_t size, uint32_t alignment)
{
    assert(alignment >= 1);
    uint32_t alignmentMinusOne = (alignment - 1);
    uint32_t alignSize = (size + alignmentMinusOne);
    uint32_t alignMask = ~(alignmentMinusOne);
    return (alignSize & alignMask);
}

bool MetaCSemantic_ComputeStructLayoutPopulateScope(metac_semantic_state_t* self,
                                                    decl_type_struct_t* agg,
                                                    sema_type_aggregate_t* semaAgg)
{
    bool result = true;

    assert(semaAgg->Fields && semaAgg->Fields != emptyPointer);

    uint32_t currentFieldOffset = 0;
    uint32_t alignment = 1;

    metac_type_aggregate_field_t* onePastLast =
        semaAgg->Fields + semaAgg->FieldCount;
    decl_field_t* declField = agg->Fields;

    // first determine all the types which will determine the sizes;
    for(metac_type_aggregate_field_t* semaField = semaAgg->Fields;
        semaField < onePastLast;
        semaField++)
    {
        semaField->Identifier = declField->Field->VarIdentifier;

        semaField->Type =
            MetaCSemantic_doTypeSemantic(self, declField->Field->VarType);
        declField = declField->Next;
    }
    uint32_t maxAlignment = MetaCSemantic_GetTypeAlignment(self, semaAgg->Fields->Type);

    for(metac_type_aggregate_field_t* semaField = semaAgg->Fields;
        semaField < onePastLast;
        semaField++)
    {
        uint32_t alignedSize = MetaCSemantic_GetTypeSize(self, semaField->Type);
        assert(alignedSize != INVALID_SIZE);
        if (semaField < (onePastLast - 1))
        {
            metac_type_aggregate_field_t *nextField = semaField + 1;
            uint32_t requestedAligment =
                MetaCSemantic_GetTypeAlignment(self, nextField->Type);
            if (requestedAligment > maxAlignment)
                maxAlignment = requestedAligment;
            alignedSize = Align(alignedSize, requestedAligment);
            assert(((currentFieldOffset + alignedSize) % requestedAligment) == 0);
        }
        semaField->Offset = currentFieldOffset;
        MetaCScope_RegisterIdentifier(semaAgg->Scope,
                                      semaField->Identifier,
                                      (metac_node_t*)semaField);
        currentFieldOffset += alignedSize;
    }

    // result =
    fprintf(stderr, "sizeof(struct) = %u\n", currentFieldOffset);//DEBUG

    return result;
}

metac_type_index_t MetaCSemantic_doTypeSemantic_(metac_semantic_state_t* self,
                                                 decl_type_t* type,
                                                 const char* callFun,
                                                 uint32_t callLine)
{
    metac_type_index_t result = {0};

    const metac_type_kind_t typeKind = type->TypeKind;

    if (isBasicType(typeKind))
    {
        result = MetaCSemantic_GetTypeIndex(self, typeKind, type);
    }
    else if (IsAggregateType(typeKind))
    {
        decl_type_struct_t* agg = (decl_type_struct_t*) type;
        sema_type_aggregate_t* semaAgg = AllocNewAggregate(typeKind, agg);
        semaAgg->FieldCount = agg->FieldCount;

        metac_type_aggregate_field_t* semaFields =
            AllocAggregateFields(semaAgg, typeKind, agg->FieldCount);
        semaAgg->Fields = semaFields;

        metac_scope_parent_kind_t scopeKind = 0;
        switch(typeKind)
        {
            case decl_type_struct:
                scopeKind = scope_parent_struct;
            break;
            case decl_type_union:
                scopeKind = scope_parent_union;
            break;
            default: assert(0);
        }

        semaAgg->Scope = MetaCSemantic_PushScope(self, scopeKind, (metac_node_t*)semaAgg);

        switch(typeKind)
        {
            case type_struct:
            {
                MetaCSemantic_ComputeStructLayoutPopulateScope(self, agg, semaAgg);
            } break;

            case type_union:
            {

            } break;

            case type_class:
            {
                assert(0);
                // Not implemented yet
            } break;

            default: assert(0);

        }

        MetaCSemantic_PopScope(self);
    }
    else if (IsPointerType(typeKind))
    {

    }
    else if (type->TypeIdentifier.v)
    {
        // printf("Type: %s\n", IdentifierPtrToCharPtr(self->ParserIdentifierTable, type->TypeIdentifier));
        metac_node_t* node =
            MetaCSemantic_LookupIdentifier(self, type->TypeIdentifier);
        if (node->Kind == decl_variable)
        {

        }
    }
    else
    {

    }



    return result;
}

#include "crc32c.h"

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
        // let's do the parameter semantic inline
        // as we have an easier time if we know at which
        // param we are and how many follow
        f->Parameters[i].VarFlags |= variable_is_parameter;
        f->Parameters[i].TypeIndex =
            MetaCSemantic_doTypeSemantic(self,
                                         currentParam->Parameter->VarType);
        currentParam = currentParam->Next;
    }
    // now we should know the sizes
    assert(currentParam == emptyPointer);

    if (func->FunctionBody == emptyPointer)
    {
        return f;
    }

    metac_scope_parent_t Parent = {SCOPE_PARENT_V(scope_parent_function, FunctionIndex(f))};

    f->Scope = MetaCSemantic_PushScope(self, scope_parent_function, (metac_node_t*)f);
    // now we have to add the parameters to the scope.

    for(uint32_t i = 0;
        i < func->ParameterCount;
        i++)
    {
        //TODO we cannot hash parameter and variable names all the time
        metac_node_t* ptr = (metac_node_t*)(&f->Parameters[i]);
        uint32_t idPtrHash = crc32c(~0, &(params[i].VarIdentifier), 4);
        scope_insert_error_t result =
            MetaCScope_RegisterIdentifier(f->Scope, params[i].VarIdentifier,
                                          ptr);
        assert(MetaCScope_LookupIdentifier(f->Scope, idPtrHash, params[i].VarIdentifier)
                == (metac_node_t*)(params + i));
    }

    f->FunctionBody = (sema_stmt_block_t*)
        MetaCSemantic_doStatementSemantic(self, func->FunctionBody);

    MetaCSemantic_PopScope(self);

    return f;
}

metac_sema_declaration_t* MetaCSemantic_doDeclSemantic_(metac_semantic_state_t* self,
                                                        metac_declaration_t* decl,
                                                        const char* callFun,
                                                        uint32_t callLine)
{
    metac_sema_declaration_t* result = 0;

    switch(decl->DeclKind)
    {
        case decl_function:
        {
            decl_function_t* f = cast(decl_function_t*) decl;
            result = (metac_sema_declaration_t*)
                MetaCSemantic_doFunctionSemantic(self, f);

        } break;
        case decl_parameter:
            assert(0);
        case decl_variable:
        {
            decl_variable_t* v = cast(decl_variable_t*) decl;
            sema_decl_variable_t* var = AllocNewSemaVariable(&result);
            var->LocationIdx = v->LocationIdx;
            var->VarIdentifier = v->VarIdentifier;
            var->TypeIndex = MetaCSemantic_doTypeSemantic(self, v->VarType);
            //TODO Register in scope?
        } break;
        case decl_type_struct:
            ((decl_type_t*)decl)->TypeKind = type_struct;
            goto LdoTypeSemantic;
        case decl_type_union:
            ((decl_type_t*)decl)->TypeKind = type_union;
            goto LdoTypeSemantic;
        case decl_type_array:
            ((decl_type_t*)decl)->TypeKind = type_array;
            goto LdoTypeSemantic;
        case decl_typedef:
            ((decl_type_t*)decl)->TypeKind = type_typedef;
            goto LdoTypeSemantic;
    LdoTypeSemantic:
        {
            metac_type_index_t type_index =
                MetaCSemantic_doTypeSemantic(self, (decl_type_t*)decl);
        } break;
    }

    return result;
}

#ifndef ATOMIC
#define INC(v) \
    (v++)
#else
#define INC(v)
    (__builtin_atomic_fetch_add(&v, __ATOMIC_RELEASE))
#endif

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
        case type_short:
            return "short";
        case type_int :
            return "int";
        case type_long :
            return "long";
        case type_size_t:
            return "size_t";
        case type_long_long:
            return "long long";

        case type_float :
            return "float";
        case type_double :
            return "double";

        case type_unsigned_char:
            return "unsigned char";
        case type_unsigned_short:
            return "unsigned short";
        case type_unsigned_int:
            return "unsigned int";
        case type_unsigned_long :
            return "unsigned long";
        case type_unsigned_long_long:
            return "unsigned long long";


        default: assert(0);
    }
    return 0;
}
#ifndef _emptyPointer
#define _emptyPointer (void*)0x1
#define emptyNode (metac_node_t*) _emptyPointer
#endif


/// Returns _emptyNode to signifiy it could not be found
/// a valid node otherwise
metac_node_t* MetaCSemantic_LookupIdentifier(metac_semantic_state_t* self,
                                                    metac_identifier_ptr_t identifierPtr)
{

    metac_node_t* result = 0;
    uint32_t idPtrHash = crc32c(~0, &identifierPtr.v, sizeof(identifierPtr.v));
/*
    // first do the LRU Lookup
    uint16_t lw15 = identifierKey & 0x7FFF;

    uint32_t startSearch = 0;

#ifdef SSE2
    __m128i keyMask = _mm_set1_epi16(lw15);
    __m128i lruHashes = (__m128i)_mm_loadu_pd(&self->LRU.LRUContentHashes);
    __m128i cmp = _mm_cmpeq_epi16(keyMask, lruHashes);
    uint32_t searchResult = _mm_movemask_epi8(cmp);
    if (searchResult == 0)
        goto LtableLookup;
    startSearch = __builtin_ffs(searchResult);
#endif

    for(int i = startSearch; i < 4; i++)
    {
        if (((self->LRU.LRUContentHashes >> (16 * i)) & 0x7FFF) == lw15)
        {
            if (self->LRU.Slots[i].Ptr.v == identifierPtr.v)
                return self->LRU.Slots[i].Node;
        }
    }
*/
    metac_scope_t *currentScope = self->CurrentScope;
#if 1
    if (!currentScope && self->declStore)
    {
        metac_identifier_ptr_t dStoreIdPtr =
            FindMatchingIdentifier(&self->declStore->Table,
                                   self->ParserIdentifierTable,
                                   identifierPtr);
        if (dStoreIdPtr.v)
        {
            metac_declaration_t* decl =
                DeclarationStore_GetDecl(self->declStore, dStoreIdPtr);
            result = (metac_node_t*)decl;
        }
    }
    else
#endif
    {
        while(currentScope)
        {
          metac_node_t* lookupResult =
                MetaCScope_LookupIdentifier(currentScope, idPtrHash, identifierPtr);
            if (lookupResult)
            {
                result = lookupResult;
                break;
            }
            currentScope = currentScope->Parent;
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

const char* MetaCExpressionKind_toChars(metac_expression_kind_t);

metac_sema_expression_t* MetaCSemantic_doExprSemantic_(metac_semantic_state_t* self,
                                                       metac_expression_t* expr,
                                                       const char* callFun,
                                                       uint32_t callLine)
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
            result->TypeIndex = MetaCSemantic_GetTypeIndex(self, type_char, emptyPointer);
        break;
        case exp_string :
            result->TypeIndex = MetaCSemantic_GetArrayTypeOf(self,
                MetaCSemantic_GetTypeIndex(self, type_char, emptyPointer),
                LENGTH_FROM_STRING_KEY(expr->StringKey));
        break;
        case exp_signed_integer :
            result->TypeIndex = MetaCSemantic_GetTypeIndex(self, type_int, emptyPointer);
        break;
        case exp_dot_compiler:
        {
            if (expr->E1->Kind != exp_call)
            {
                fprintf(stderr, "Only calls are supported not %s\n",
                    MetaCExpressionKind_toChars(expr->E1->Kind));
                break;
            }
            metac_expression_t* call = expr->E1;
            metac_expression_t* fn = call->E1;
            exp_argument_t* args = call->E2->arguments;


            printf("Type(fn) %s\n", MetaCExpressionKind_toChars(fn->Kind));
            printf("call: %s\n", MetaCPrinter_PrintExpression(&self->Printer, call));

            for(int memberIdx = 0;
                memberIdx < self->CompilerInterface->FieldCount;
                memberIdx)
            {


            }
            // CompilerInterface_Call(
        } break;
        case exp_type:
        {
            result->TypeIndex = MetaCSemantic_doTypeSemantic(self, expr->TypeExp);
        } break;
        case exp_sizeof:
        {
            uint32_t size = -1;
            metac_sema_expression_t* e1 =
                MetaCSemantic_doExprSemantic(self, expr->E1);
            metac_type_index_t typeIndex;

            result->TypeIndex.v = TYPE_INDEX_V(type_index_basic, type_size_t);
            result->Kind = exp_signed_integer;
            result->ValueU64 = size;
        } break;
        case exp_identifier:
        {
            metac_node_t* node =
                MetaCSemantic_LookupIdentifier(self,
                                               result->IdentifierPtr);
            if (node == emptyPointer)
            {
                fprintf(stderr, "Identifier lookup failed\n");
            }
            else
            {
                result = (metac_sema_expression_t*) node;
                if (node->Kind == (metac_expression_kind_t)exp_identifier)
                    fprintf(stderr, "we should not be retured an identifier\n");
                if (node->Kind == decl_variable)
                {
                    metac_sema_declaration_t* sd =
                        MetaCSemantic_doDeclSemantic(self, (metac_declaration_t*)node);
                    int k = 12;
                    // an exp_variable might resolve to something
                    // diffrent than a variable so check that it is one now
                    if(sd->DeclKind == decl_variable)
                    {
                        sema_decl_variable_t* variable =
                            (sema_decl_variable_t*) sd;
                        result->Kind = exp_variable;
                        result->TypeIndex = variable->TypeIndex;
                    }
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
