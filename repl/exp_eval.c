/// little eval module to evaluate a simple expression

#include "../compat.h"
#include "../metac_parser.h"
#include "exp_eval.h"
#include "../libinterpret/bc_common.c"
#include "../libinterpret/bc_interpreter_backend.c"
#include <stdio.h>

bool IsBinaryExp(metac_expression_kind_t k);

const char* MetaCExpressionKind_toChars(metac_expression_kind_t k);

metac_identifier_ptr_t FindMatchingIdentifier(metac_identifier_table_t* searchTable,
                                              metac_identifier_table_t* sourceTable,
                                              metac_identifier_ptr_t sourcePtr,
                                              bool addIfNotFound)
{
    const char* idChars = IdentifierPtrToCharPtr(sourceTable, sourcePtr);
    uint32_t idLength = strlen(idChars);
    uint32_t idHash = crc32c_nozero(~0, idChars, idLength);
    uint32_t idKey = IDENTIFIER_KEY(idHash, idLength);

    metac_identifier_ptr_t result = IsIdentifierInTable(searchTable, idKey, idChars);

    if (!result.v && addIfNotFound)
    {
        result = GetOrAddIdentifier(searchTable, idKey, idChars);
    }

    return result;
}

metac_identifier_ptr_t AddVStoreID(variable_store_t* vstore,
                                   sema_decl_variable_t* var)
{
    metac_identifier_ptr_t vstoreId =
        FindMatchingIdentifier(&vstore->Table,
                               vstore->ExternalTable,
                               var->VarIdentifier, true);
    return vstoreId;
}

metac_identifier_ptr_t GetVStoreID(variable_store_t* vstore,
                                   sema_decl_variable_t* var)
{
    metac_identifier_ptr_t vstoreId =
        FindMatchingIdentifier(&vstore->Table,
                               vstore->ExternalTable,
                               var->VarIdentifier, false);
    return vstoreId;
}

static inline BCValue* GetValueFromVariableStore(variable_store_t* vstore,
                                                 metac_identifier_ptr_t vstoreId)
{
    for(int i = 0;
        i < vstore->VariableSize;
        i++)
    {
        variable_t var = vstore->Variables[i];
        if (var.IdentifierPtr.v == vstoreId.v)
        {
            return (BCValue*) var.value;
        }
    }

    return 0;
}

void VariableStore_AddVariable(variable_store_t* vstore,
                               sema_decl_variable_t* varDecl,
                               void* value)
{
    metac_identifier_ptr_t vstoreId = GetVStoreID(vstore, varDecl);
    assert(vstoreId.v == 0);
    vstoreId = AddVStoreID(vstore, varDecl);

    BCValue* v = GetValueFromVariableStore(vstore, vstoreId);
    assert(!v);
    assert(vstore->VariableCapacity > vstore->VariableSize);

    vstore->Variables[vstore->VariableSize++] = (variable_t) { vstoreId, value };
}

void VariableStore_RemoveVariable(variable_store_t* vstore, void* value)
{
    bool foundVar = false;
    for(uint32_t i = 0; i < vstore->VariableSize; i++)
    {
        if (foundVar)
        {
            vstore->Variables[i - 1] = vstore->Variables[i];
        }
        else if (vstore->Variables[i].value == value)
        {
            foundVar = true;
        }
    }
    assert(foundVar);
    --vstore->VariableSize;
}


void VariableStore_SetValueI32(variable_store_t* vstore,
                               metac_sema_expression_t* varExp,
                               int32_t value)
{
    assert(varExp->Kind == exp_variable);

    metac_identifier_ptr_t vstoreId = GetVStoreID(vstore, varExp->Variable);
    BCValue* v = GetValueFromVariableStore(vstore, vstoreId);
    if (!v)
    {
        v = (BCValue*)malloc(sizeof(BCValue));
        vstore->Variables[vstore->VariableSize++] = (variable_t){ vstoreId, v };
    }
    *v = imm32(value);
}

void ReadI32_cb (uint32_t value, void* userCtx)
{
    ReadI32_Ctx* ctx = cast(ReadI32_Ctx*) userCtx;
    VariableStore_SetValueI32(ctx->vstore, ctx->exp, value);
}

ReadI32_Ctx* _ReadContexts;
uint32_t _ReadContextSize;
uint32_t _ReadContextCapacity;


//FIXME we never hit this ... why?
// maybe an issue with the declaration store or maybe with
// aligning the tables ....

/*
    BCGen* gen = cast(BCGen*) c;
    printf("We should follow with some code:\n");
    for(int i = 0;
        i < gen->ip;
        i++)
    {
        printf("%d, ", gen->byteCodeArray[i]);
        if ((i % 8) == 0)
        {
            printf("\n");
        }
    }
    printf("\n");
    printf("There should have been some code\n");
*/

static inline void TupleToValue(void* c, BCValue* result,
                                metac_sema_expression_t* e)
{

}
extern const BackendInterface* bc;

void WalkTree(void* c, BCValue* result,
              metac_sema_expression_t* e,
              variable_store_t* vstore)
{
    metac_expression_kind_t op = e->Kind;

    if (op == exp_signed_integer)
    {
        (*result) = imm32(cast(int32_t)e->ValueI64);
        return ;
    }

    BCValue lhsT = bc->GenTemporary(c, BCType_i32);
    BCValue rhsT = bc->GenTemporary(c, BCType_i32);
    BCValue *lhs = &lhsT;
    BCValue *rhs = &rhsT;


    if (IsBinaryAssignExp(op))
    {
        op -= (exp_add_ass - exp_add);
    }

    if (IsBinaryExp(op))
    {
        WalkTree(c, lhs, e->E1, vstore);
        WalkTree(c, rhs, e->E2, vstore);
    }

    switch(op)
    {
        case exp_dot_compiler:
        {
            printf("ignoring unprocessed .compiler expression\n");
        } break;
        default : {
            fprintf(stderr,
                "Evaluator doesn't know how to eval: %s\n",
                MetaCExpressionKind_toChars(e->Kind)
            );
            assert(0);
        } break;
        case decl_enum_member:
        {
            metac_enum_member_t* enumMember = cast(metac_enum_member_t*) e;
            *result = imm32((int32_t)enumMember->Value->ValueI64);
        } break;
        case exp_string:
        {
            // this should not happen, we should have made it into a pointer I think
            assert(0);
        }
        case exp_assert:
        {
            /*
            BCValue errVal = imm32(0);
            BCValue cond = bc->genTemporary(c, (BCType){BCTypeEnum_u32});
            WalkTree(c, &cond, e->E1, vstore);
            bc->Assert(c, &cond, &errVal);
             */
        } break;
        case exp_assign:
        {
            assert(e->E1->Kind == exp_variable);

            //metac_identifier_ptr_t idPtr = e->E1->Variable->VarIdentifier;
            //metac_identifier_ptr_t vStorePtr = GetVStoreID(vstore, e->E1);
            bc->Set(c, lhs, rhs);
            bc->Set(c, result, lhs);
        } break;

        case exp_tuple:
        {
            TupleToValue(c, result, e);
        } break;

        case exp_type:
        {
            BCValue imm = imm32(e->TypeExp.v);
            bc->Set(c, result, &imm);
        } break;

        case exp_signed_integer:
        {
            BCValue imm = imm32((int32_t)e->ValueU64);
            bc->Set(c, result, &imm);
        } break;

        case exp_eq:
        {
            bc->Eq3(c, result, lhs, rhs);
        } break;

        case exp_neq:
        {
            bc->Neq3(c, result, lhs, rhs);
        } break;

        case exp_lt:
        {
            bc->Lt3(c, result, lhs, rhs);
        } break;

        case exp_le:
        {
            bc->Le3(c, result, lhs, rhs);
        } break;

        case exp_ge:
        {
            bc->Ge3(c, result, lhs, rhs);
        } break;

        case exp_gt:
        {
            bc->Gt3(c, result, lhs, rhs);
        } break;

        case exp_add:
        {
            bc->Add3(c, result, lhs, rhs);
        } break;
        case exp_sub:
        {
            bc->Sub3(c, result, lhs, rhs);
        } break;
        case exp_mul:
        {
            bc->Mul3(c, result, lhs, rhs);
        } break;
        case exp_div:
        {
            bc->Div3(c, result, lhs, rhs);
        } break;
        case exp_rem:
        {
            bc->Mod3(c, result, lhs, rhs);
        } break;
        case exp_andand:
        case exp_and:
        {
            bc->And3(c, result, lhs, rhs);
        } break;

        case exp_oror:
        case exp_or:
        {
            bc->Or3(c, result, lhs, rhs);
        } break;
        case exp_xor:
        {
            bc->Xor3(c, result, lhs, rhs);
        } break;
        case exp_identifier:
        {
            fprintf(stderr, "There have been unresolved identifiers ... this should not happen\n");
        }
        case exp_variable:
        {
            metac_identifier_ptr_t vstoreId =
                GetVStoreID(vstore, e->Variable);
            BCValue* v = GetValueFromVariableStore(vstore, vstoreId);
            if (v)
            {
                //bc->Set(c, result, v);
                (*result) = (*v);
            }
            else
            {
                fprintf(stderr, "Variable %s not in variable store\n",
                                IdentifierPtrToCharPtr(&vstore->Table,
                                                       vstoreId));
            }
        } break;
        case exp_paren:
        {
            WalkTree(c, result, e->E1, vstore);
        } break;
        case exp_compl:
        {
            WalkTree(c, rhs, e->E1, vstore);
            bc->Not(c, result, rhs);
        } break;
        case exp_not:
        {
            WalkTree(c, lhs, e->E1, vstore);
            BCValue zero = imm32(0);
            bc->Eq3(c, result, lhs, &zero);
        } break;
        case exp_umin:
        {
            WalkTree(c, lhs, e->E1, vstore);
            BCValue zero = imm32(0);
            bc->Sub3(c, result, &zero, lhs);
        } break;
        case exp_post_decrement:
        {
            WalkTree(c, lhs, e->E1, vstore);
            bc->Set(c, result, lhs);
            BCValue one = imm32(1);
            bc->Sub3(c, lhs, lhs, &one);
        } break;
        case exp_post_increment:
        {
            WalkTree(c, lhs, e->E1, vstore);
            bc->Set(c, result, lhs);
            BCValue one = imm32(1);
            bc->Add3(c, lhs, lhs, &one);
            if (e->E1->Kind == exp_variable)
            {
/*
                assert(_ReadContextSize < _ReadContextCapacity);
                ReadI32_Ctx* userCtx = &_ReadContexts[_ReadContextSize++];
                *userCtx = (ReadI32_Ctx){ vstore, e->E1 };
                //TODO provide an allocExecutionContext in the BCgeninterface
                bc->ReadI32(c, lhs, ReadI32_cb, userCtx);
*/
                // if (lhs->vType == BCValueType_StackValue || lhs->vType == BCValueType_Local)
            }
            else
            {
                fprintf(stderr, "lhs is not an lvalue\n");
            }
        } break;

        case exp_call:
        {
            sema_exp_call_t* call = &e->Call;

            printf("call->Function->Kind: %s\n", MetaCExpressionKind_toChars(e->E1->Kind));
            assert(call->Function->Kind == exp_function);
            sema_decl_function_t* func = call->Function->Function;
            metac_identifier_ptr_t idPtr = func->Identifier;
            assert(0); // Not supported for the time being
        } break;
    }

    if (IsBinaryAssignExp(e->Kind))
    {
        bc->Set(c, lhs, result);
/*
        ReadI32_Ctx* userCtx = &_ReadContexts[_ReadContextSize++];
        *userCtx = (ReadI32_Ctx){ vstore, e->E1 };
        //TODO provide an allocExecutionContext in the BCgeninterface
        bc->ReadI32(c, lhs, ReadI32_cb, userCtx);
*/
    }
    if (rhs->vType == BCValueType_Temporary)
        bc->DestroyTemporary(c, rhs);

    if (lhs->vType == BCValueType_Temporary)
        bc->DestroyTemporary(c, lhs);
}

metac_sema_expression_t evalWithVariables(metac_sema_expression_t* e,
                                          variable_store_t* vstore)
{
    void* c;
    bc->new_instance(&c);

    uint32_t fIdx;

    bc->Initialize(c, 0); // zero extra arguments
    {
        fIdx = bc->BeginFunction(c, 0, "eval_func");
        BCValue result = bc->GenLocal(c, (BCType){BCTypeEnum_i64}, "result");

        // walk the tree;
        WalkTree(c, &result, e, vstore);

        bc->Ret(c, &result);

        void * func = bc->EndFunction(c, fIdx);
    }
    bc->Finalize(c);

    // BCGen_printFunction(c);

    BCValue res = bc->Run(c, fIdx, 0, 0);

    metac_sema_expression_t result;

    if (e->TypeIndex.v == TYPE_INDEX_V(type_index_basic, type_type))
    {
        result.Kind = exp_type;
        result.TypeIndex.v = TYPE_INDEX_V(type_index_basic, type_type);
        result.TypeExp.v = res.imm32.imm32;
    }
    else
    {
        result.Kind = exp_signed_integer;
        result.ValueI64 = res.imm64.imm64;
    }

    return result;
}

void VariableStore_Init(variable_store_t* self, metac_identifier_table_t* externalTable)
{
    self->VariableCapacity = 32;
    self->VariableSize = 0;
    self->Variables = (variable_t*)
        malloc(sizeof(variable_t) * self->VariableCapacity);
    self->ExternalTable = externalTable;

    IdentifierTable_Init(&self->Table, IDENTIFIER_LENGTH_SHIFT, 9);
}
