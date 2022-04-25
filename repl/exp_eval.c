/// little eval module to evaluate a simple expression


#include "../compat.h"
#include "../metac_parser.h"
#include "exp_eval.h"
#include "../libinterpret/bc_common.c"
#include "../libinterpret/bc_interpreter_backend.c"
#include <stdio.h>

bool IsBinaryExp(metac_expression_kind_t k);

const char* MetaCExpressionKind_toChars(metac_expression_kind_t k);

static inline BCValue* GetValueFromVariableStore(variable_store_t* vstore,
                                                 metac_expression_t* identifierExp)
{
    uint32_t idKey = identifierExp->IdentifierKey;
    metac_identifier_ptr_t idPtr = identifierExp->IdentifierPtr;
    uint32_t length = LENGTH_FROM_IDENTIFIER_KEY(idKey);
    const char* idChars = IdentifierPtrToCharPtr(&g_lineParser.IdentifierTable, idPtr);

    metac_identifier_ptr_t vstoreId
        = GetOrAddIdentifier(&vstore->Table, idKey, idChars, length);

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
                               metac_expression_t* identifierExp,
                               int32_t value)
{
    uint32_t idKey = identifierExp->IdentifierKey;
    metac_identifier_ptr_t idPtr = identifierExp->IdentifierPtr;
    uint32_t length = LENGTH_FROM_IDENTIFIER_KEY(idKey);
    const char* idChars = IdentifierPtrToCharPtr(&g_lineParser.IdentifierTable, idPtr);
    metac_identifier_ptr_t vstoreId
        = GetOrAddIdentifier(&vstore->Table, idKey, idChars, length);

    BCValue* v = GetValueFromVariableStore(vstore, identifierExp);
    if (!v)
    {
        v = (BCValue*)malloc(sizeof(BCValue));
        vstore->Variables[vstore->VariableSize++] = (variable_t){ vstoreId, v };
    }
    else
    {
        fprintf(stderr, "overwriting stored version of %s\n", idChars);
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
                            metac_expression_t* e,
                            variable_store_t* vstore)
{
    BCValue lhsT = BCGen_interface.genTemporary(c, BCType_i32);
    BCValue rhsT = BCGen_interface.genTemporary(c, BCType_i32);
    BCValue *lhs = &lhsT;
    BCValue *rhs = &rhsT;

    if (IsBinaryExp(e->Kind))
    {
        WalkTree(c, lhs, e->E1, vstore);
        WalkTree(c, rhs, e->E2, vstore);
    }

    switch(e->Kind)
    {
        default : {
            fprintf(stderr,
                "Evaluator doesn't know how to eval: %s\n",
                MetaCExpressionKind_toChars(e->Kind)
            );
            assert(0);
        } break;

        case exp_signed_integer :
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
        case exp_identifier:
        {
            BCValue* v = GetValueFromVariableStore(vstore, e);
            if (v)
            {
                BCGen_interface.Set(c, result, v);
            }
            else
            {
                fprintf(stderr, "Variable %s not in variable store\n",
                                IdentifierPtrToCharPtr(&vstore->Table,
                                                       e->IdentifierPtr));
            }
        } break;
        case exp_post_increment:
        {
            WalkTree(c, lhs, e->E1, vstore);
            BCGen_interface.Set(c, result, lhs);
            BCValue one = imm32(1);
            BCGen_interface.Add3(c, lhs, lhs, &one);
            if (e->E1->Kind == exp_identifier)
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
        case exp_paren:
        {
            WalkTree(c, result, e->E1, vstore);
        } break;
    }

    BCGen_interface.destroyTemporary(c, lhs);
    BCGen_interface.destroyTemporary(c, rhs);
}

metac_expression_t evalWithVariables(metac_expression_t* e,
                                      variable_store_t* vstore)
{
    void* c;
    BCGen_interface.new_instance(&c);

    uint32_t fIdx;

    BCGen_interface.Initialize(c, 0); // zero extra arguments
    {
        fIdx = BCGen_interface.beginFunction(c, 0, "eval_func");
        BCValue result = BCGen_interface.genLocal(c, (BCType){BCTypeEnum_i32}, "result");

        // walk the tree;
        WalkTree(c, &result, e, vstore);

        BCGen_interface.Ret(c, &result);
        void * func = BCGen_interface.endFunction(c, fIdx);
    }
    BCGen_interface.Finalize(c);
    BCValue res = BCGen_interface.run(c, fIdx, 0, 0);

    metac_expression_t result;
    result.Kind = exp_signed_integer;
    result.ValueI64 = res.imm32.imm32;

    return result;
}

void VariableStore_Init(variable_store_t* self)
{
    self->VariableCapacity = 32;
    self->VariableSize = 0;
    self->Variables = (variable_t*)
        malloc(sizeof(variable_t) * self->VariableCapacity);

    IdentifierTableInit(&self->Table);
}
