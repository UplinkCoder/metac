/// little eval module to evaluate a simple expression

#include "../compat.h"
#include "../metac_parser.h"
#include "exp_eval.h"
#include "../libinterpret/bc_common.c"
#include "../libinterpret/bc_interpreter_backend.c"
#include <stdio.h>
const char* MetaCExpressionKind_toChars(metac_expression_kind_t k);
/*
    const BackendInterface i = BCGen_interface;

    void* c;
    i.new_instance(&c);

    uint32_t fIdx;

    i.Initialize(c, 0); // zero extra arguments
    {
        fIdx = i.beginFunction(c, 0, "add");
        {
            // TODO i.returnTypeAnnotation(c, BCType_i32);

            BCValue a = i.genParameter(c, (BCType){BCTypeEnum_i32}, "a");
            BCValue b = i.genParameter(c, BCType_i32, "b");

            BCValue res = i.genLocal(c, (BCType){BCTypeEnum_i32}, "result");

            i.Add3(c, &res, &a, &b);
            i.Ret(c, &res);

            i.endFunction(c, fIdx);
        }
    }
    i.Finalize(c);

    {
        int a = atoi(argv[1]);
        int b = atoi(argv[2]);

        BCValue arguments[2];

        arguments[0] = imm32(a);
        arguments[1] = imm32(b);

        BCValue res = i.run(c, fIdx, arguments, 2);

        printf("%d + %d = %d\n", a, b, res.imm32.imm32);
    }
*/

static inline void WalkTree(void* c, const BCValue* result, metac_expression_t* e)
{
    BCValue lhsT = BCGen_interface.genTemporary(c, BCType_i32);
    BCValue rhsT = BCGen_interface.genTemporary(c, BCType_i32);
    BCValue *lhs = &lhsT;
    BCValue *rhs = &rhsT;

    if (IsBinaryExp(e->Kind))
    {
        WalkTree(c, lhs, e->E1);
        WalkTree(c, rhs, e->E2);
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
    }

    BCGen_interface.destroyTemporary(c, lhs);
    BCGen_interface.destroyTemporary(c, rhs);
}

metac_expression_t* evalWithVariables(metac_expression_t* e, variable_store_t* vars)
{
    void* c;
    BCGen_interface.new_instance(&c);

    uint32_t fIdx;

    metac_expression_t result0;

    BCGen_interface.Initialize(c, 0); // zero extra arguments
    {
        fIdx = BCGen_interface.beginFunction(c, 0, "eval_func");
        BCValue result = BCGen_interface.genLocal(c, (BCType){BCTypeEnum_i32}, "result");

        // walk the tree;
        WalkTree(c, &result, e);

        BCGen_interface.Ret(c, &result);
        void * func = BCGen_interface.endFunction(c, fIdx);
    }
    BCGen_interface.Finalize(c);
    BCValue res = BCGen_interface.run(c, fIdx, 0, 0);

    metac_expression_t* result = &result0;
    result->Kind = exp_signed_integer;
    result->ValueI64 = res.imm32.imm32;

    return result;
}

void VariableStore_Init(variable_store_t* self)
{
    self->VariableCapacity = 32;
    self->VariableSize = 0;
    self->Variables = (variable_t*)
        malloc(sizeof(variable_t) * self->VariableCapacity);
}
