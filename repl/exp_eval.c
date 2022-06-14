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
                                              metac_identifier_ptr_t sourcePtr)
{
    const char* idChars = IdentifierPtrToCharPtr(sourceTable, sourcePtr);
    uint32_t idLength = strlen(idChars);
    uint32_t idHash = crc32c_nozero(~0, idChars, idLength);
    uint32_t idKey = IDENTIFIER_KEY(idHash, idLength);

    metac_identifier_ptr_t result = IsIdentifierInTable(searchTable, idKey, idChars);

    if (result.v == 0)
        assert(0);

    return result;
}


static inline metac_identifier_ptr_t GetVStoreID(variable_store_t* vstore,
                                                 metac_sema_expression_t* varExp)
{
    assert(varExp->Kind == exp_variable);
    sema_decl_variable_t* var = varExp->Variable;

    metac_identifier_ptr_t vstoreId =
        FindMatchingIdentifier(&vstore->Table,
                           &g_lineParser.IdentifierTable,
                           var->VarIdentifier);
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


void VariableStore_SetValueI32(variable_store_t* vstore,
                               metac_sema_expression_t* varExp,
                               int32_t value)
{
    assert(varExp->Kind == exp_variable);

    metac_identifier_ptr_t vstoreId = GetVStoreID(vstore, varExp);
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

static inline void WalkTree(void* c, BCValue* result,
                            metac_sema_expression_t* e,
                            variable_store_t* vstore);

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

static inline void WalkTree(void* c, BCValue* result,
                            metac_sema_expression_t* e,
                            variable_store_t* vstore)
{
    BCValue lhsT = BCGen_interface.genTemporary(c, BCType_i32);
    BCValue rhsT = BCGen_interface.genTemporary(c, BCType_i32);
    BCValue *lhs = &lhsT;
    BCValue *rhs = &rhsT;

    metac_expression_kind_t op = e->Kind;

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
        default : {
            fprintf(stderr,
                "Evaluator doesn't know how to eval: %s\n",
                MetaCExpressionKind_toChars(e->Kind)
            );
            assert(0);
        } break;
        case exp_string:
        {
            // this should not happen, we should have made it into a pointer I think
            assert(0);
        }

        case exp_assign:
        {
            assert(e->E1->Kind == exp_variable);

            //metac_identifier_ptr_t idPtr = e->E1->Variable->VarIdentifier;
            //metac_identifier_ptr_t vStorePtr = GetVStoreID(vstore, e->E1);
            BCGen_interface.Set(c, lhs, rhs);
            BCGen_interface.Set(c, result, lhs);
        } break;

        case exp_tuple:
        {
            TupleToValue(c, result, e);
        } break;

        case exp_type:
        {
            BCValue imm = imm32(e->TypeExp.v);
            BCGen_interface.Set(c, result, &imm);
        } break;

        case exp_signed_integer:
        {
            BCValue imm = imm32((uint32_t)e->ValueU64);
            BCGen_interface.Set(c, result, &imm);
        } break;

        case exp_add:
        {
            BCGen_interface.Add3(c, result, lhs, rhs);
        } break;
        case exp_sub:
        {
            BCGen_interface.Sub3(c, result, lhs, rhs);
        } break;
        case exp_mul:
        {
            BCGen_interface.Mul3(c, result, lhs, rhs);
        } break;
        case exp_div:
        {
            BCGen_interface.Div3(c, result, lhs, rhs);
        } break;
        case exp_rem:
        {
            BCGen_interface.Mod3(c, result, lhs, rhs);
        } break;
        case exp_and:
        {
            BCGen_interface.And3(c, result, lhs, rhs);
        } break;
        case exp_or:
        {
            BCGen_interface.Or3(c, result, lhs, rhs);
        } break;
        case exp_xor:
        {
            BCGen_interface.Xor3(c, result, lhs, rhs);
        } break;
        case exp_identifier:
        {
            fprintf(stderr, "There have been unresolved identifiers ... this should not happen\n");
        }
        case exp_variable:
        {
            metac_identifier_ptr_t vstoreId =
                GetVStoreID(vstore, e);
            BCValue* v = GetValueFromVariableStore(vstore, vstoreId);
            if (v)
            {
                BCGen_interface.Set(c, result, v);
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
            BCGen_interface.Not(c, result, rhs);
        } break;
        case exp_not:
        {
            WalkTree(c, lhs, e->E1, vstore);
            BCValue zero = imm32(0);
            BCGen_interface.Neq3(c, result, lhs, &zero);
        } break;
        case exp_umin:
        {
            WalkTree(c, lhs, e->E1, vstore);
            BCValue zero = imm32(0);
            BCGen_interface.Sub3(c, result, &zero, lhs);
        } break;
        case exp_post_increment:
        {
            WalkTree(c, lhs, e->E1, vstore);
            BCGen_interface.Set(c, result, lhs);
            BCValue one = imm32(1);
            BCGen_interface.Add3(c, lhs, lhs, &one);
            if (e->E1->Kind == exp_variable)
            {
                assert(_ReadContextSize < _ReadContextCapacity);
                ReadI32_Ctx* userCtx = &_ReadContexts[_ReadContextSize++];
                *userCtx = (ReadI32_Ctx){ vstore, e->E1 };
                //TODO provide an allocExecutionContext in the BCgeninterface
                BCGen_interface.ReadI32(c, lhs, ReadI32_cb, userCtx);
            }
            else
            {
                fprintf(stderr, "lhs is not an lvalue\n");
            }
        } break;

        case exp_call:
        {
            assert(e->E1->Kind == exp_identifier);
            metac_identifier_ptr_t idPtr = e->E1->IdentifierPtr;
            assert(0); // Not supported for the time being
        } break;
    }

    if (IsBinaryAssignExp(e->Kind))
    {
        BCGen_interface.Set(c, lhs, result);
        ReadI32_Ctx* userCtx = &_ReadContexts[_ReadContextSize++];
        *userCtx = (ReadI32_Ctx){ vstore, e->E1 };
        //TODO provide an allocExecutionContext in the BCgeninterface
        BCGen_interface.ReadI32(c, lhs, ReadI32_cb, userCtx);
    }

    BCGen_interface.destroyTemporary(c, rhs);
    BCGen_interface.destroyTemporary(c, lhs);
}

metac_sema_expression_t evalWithVariables(metac_sema_expression_t* e,
                                          variable_store_t* vstore)
{
    void* c;
    BCGen_interface.new_instance(&c);

    uint32_t fIdx;

    BCGen_interface.Initialize(c, 0); // zero extra arguments
    {
        fIdx = BCGen_interface.beginFunction(c, 0, "eval_func");
        BCValue result = BCGen_interface.genLocal(c, (BCType){BCTypeEnum_i64}, "result");

        // walk the tree;
        WalkTree(c, &result, e, vstore);

        BCGen_interface.Ret(c, &result);

        void * func = BCGen_interface.endFunction(c, fIdx);
    }
    BCGen_interface.Finalize(c);

    BCValue res = BCGen_interface.run(c, fIdx, 0, 0);
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

void VariableStore_Init(variable_store_t* self)
{
    self->VariableCapacity = 32;
    self->VariableSize = 0;
    self->Variables = (variable_t*)
        malloc(sizeof(variable_t) * self->VariableCapacity);

    IdentifierTableInit(&self->Table, IDENTIFIER_LENGTH_SHIFT);
}

metac_identifier_ptr_t IdentifierPtrFromDecl(metac_declaration_t* decl)
{
    metac_identifier_ptr_t idPtr = {0};

    switch(decl->DeclKind)
    {
        case decl_function:
        {
            decl_function_t* f = cast(decl_function_t*) decl;
            idPtr = f->Identifier;
            break;
        }
        case decl_variable:
        {
            decl_variable_t* v = cast(decl_variable_t*) decl;
            idPtr = v->VarIdentifier;
            break;
        }
        default : assert(0);
    }

    return idPtr;
}
