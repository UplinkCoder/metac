#ifndef _METAC_CODEGEN_H_
#define _METAC_CODEGEN_H_
#include "compat.h"
#include "metac_alloc.h"
#include "metac_sematree.h"
#include "repl/exp_eval.h"
#ifndef AT
#efine AT(...)
#endif


typedef struct metac_bytecode_switch_t
{
    /// expression to compare case against
    BCValue* Exp;

    /// set to a fixup location
    ARENA_ARRAY(CndJmpBegin, PrevCaseJumps);

    /// if non-null this goes at the of the switch
    /// such that we fall through to it
    metac_sema_statement_t* DefaultBody;
} metac_bytecode_switch_t;

typedef struct metac_bytecode_ctx_t
{
    /// backend context
    AT(transient) void* c;

    AT(transient) AT(per_function) metac_identifier_table_t* IdentifierTable;
    AT(transient) AT(per_function) BCType* ParameterTypes;
    AT(transient) AT(per_function) BCValue* Parameters;
    AT(transient) AT(per_function) uint32_t ParameterCount;

    ARENA_ARRAY(metac_bytecode_switch_t, SwitchStack);

    metac_identifier_table_t knownVariables;
    metac_identifier_table_t knownFunctions;

} metac_bytecode_ctx_t;


typedef struct metac_function_bytecode_t metac_function_bytecode_t;

metac_function_bytecode_t* MetaCCodegen_GenerateFunction(metac_bytecode_ctx_t* ctx,
                                                         sema_decl_function_t* function);
uint32_t MetaCCodegen_GetTypeABISize(metac_bytecode_ctx_t* ctx, metac_type_index_t type);
BCType MetaCCodegen_GetBCType(metac_bytecode_ctx_t* ctx, metac_type_index_t type);

#endif
