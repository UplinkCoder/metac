#include "metac_value_table.c"

typedef struct metac_abstract_split_state_t {
    metac_sema_expr_t*      Cond;
    metac_abstract_state_t TrueState;
    metac_abstract_state_t FalseState;
} metac_abstract_split_state_t;

typedef struct metac_abstract_state_t {
    //TODO allocator should in 'metac_absint_t'
    metac_alloc_t Allocator;
    metac_abstract_state_t* Parent;
    ARENA_ARRAY(metac_sema_expr_t*, TrueRelations)

    metac_abstract_split_state_t* (*splitOn) (struct metac_abstract_state_t* state, metac_sema_expr_t* cond);

} metac_abstract_state_t;


typedef struct metac_absint_value_range_t {
    double lo;
    double hi;
} metac_absint_value_range_t;

metac_abstract_state_t* newState(void*);

/* int f(int a)
 { if (a == 22)
  *     b = 9;
  * else
  *     b = 12;
  * return b; }
  b -> [9 .. 12[
*/
/*
 *  == -> !=
    <  -> >=
    <= -> >
    >  -> <=
    >= -> <
 */
void MetaCAbsInt_visitFunction(metac_abstract_state_t* self, sema_decl_function_t* f)
{
    void* state = self->SetupAbstractState();

    MetaCAbsInt_doStmt(state, f->FunctionBody);
}

metac_abstract_split_state_t* MetaCAbsInt_SplitOn(metac_abstract_state_t* state, metac_sema_expr_t* cond)
{
    metac_abstract_split_state_t* result = Allocator_Calloc(&state->Allocator, metac_abstract_split_state_t, 1);
    metac_sema_expr_t* negatedExpr       = Allocator_Calloc(&state->Allocator, metac_sema_expr_t, 1);
    result->TrueState.Parent  = state;
    ARENA_ARRAY_ADD(result->TrueState.TrueFacts, cond);

    result->FalseState.Parent = state;
    NegateExpr(&negateExpr, cond);
    ARENA_ARRAY_ADD(result->FalseState.TrueFacts, negatedExpr);
}

void MetaCAbsInt_doStmt(metac_abstract_state_t* self, metac_sema_stmt_t* stmt)
{
    switch(stmt->Kind)
    {
        case stmt_if: {
            sema_stmt_if_t* stmt_if = cast(sema_stmt_if_t) stmt;
            metac_abstract_state_t *mergedState = 0;
            metac_abstract_spit_state_t* splitState =  MetaCAbsInt_SplitOn(self, stmt_if->IfCond);
            if (METAC_NODE(stmt_if->IfBody) != emptyNode)
            {
                MetaCAbsInt_doStmt(splitState->TrueBranch, stmt_if->IfBody);
            }
            if (METAC_NODE(stmt_if->ElseBody) != emptyNode)
            {
                MetaCAbsInt_doStmt(splitState->FalseBranch, stmt_if->ElseBody);
            }
            mergedState = MetaCAbsInt_MergeState(self, splitState);
        } break;
    }
}


metac_absint_value_range_t MetaCAbsInt_ValueRange_GetRangeForVar(metac_abstract_state_t* self, sema_decl_variable_t* var);
void MetaCAbsInt_ValueRange_SetRangeForVar(metac_abstract_state_t* self, sema_decl_variable_t* var, metac_absint_value_range_t r);


metac_absint_value_range_t MetaCAbsInt_ValueRange_doExpr(metac_abstract_state_t* self, sema_decl_variable_t* expr)
{

}

{
    switch(stmt->Kind)
    {
        case expr_assign: {
            metac_absint_value_range_t e2 = MetaCAbsInt_ValueRange_doExpr(self, expr->E2);
            if (expr->E1->Kind == expr_variable)
            {
                MetaCAbsInt_ValueRange_SetRangeForVar(self, expr->E1->Variable, e2);
                return e2;
            }
            else
            {
                assert(!"This expression is not a variable and therefore cannot be assigned to");
            }
        } break;
        case expr_eq:
        {
            metac_absint_value_range_t e1 = MetaCAbsInt_ValueRange_doExpr(self, expr->E1);
            metac_absint_value_range_t e2 = MetaCAbsInt_ValueRange_doExpr(self, expr->E2);
            if (MetaCValueRange_Excludes(e1, e2))
            {
                return {0, 0};
            }
            if (MetaCValueRange_IsBottom(e1) || MetaCValueRange_IsBottom(e2))
            {
                return MetaCValueRange_Bottom();
            }
            if (MetaCValueRange_IsSigular(e1) && MetaCValueRange_Equal(e1, e2))
            {
                return {1, 1};
            }
        } break;
        case expr_variable: {
            return MetaCAbsInt_ValueRange_GetRangeForVar(self, expr->Variable);
        } break;
        case expr_signed_integer: {
            metac_absint_value_range_t result = {expr->ValueI64, expr->ValueI64};
            return result;
        } break;

    }
}
