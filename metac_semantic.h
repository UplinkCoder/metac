#include "metac_parsetree.h"
#include "metac_parser.h"
#include "metac_type_table.h"
#include "metac_printer.h"
#include "metac_sematree.h"
#include "metac_scope.h"
//TODO get rid of exp_eval after testing
#include "repl/exp_eval.h"

#ifndef AT
#  define AT(...)
#endif

typedef struct metac_sema_decl_state_t
{
    sema_decl_variable_t* CurrentVariables;
    uint32_t CurrentVariableSize;
    uint32_t CurrentVariableCapacity;

    // uint32_t CurrentOffset;
} metac_sema_decl_state_t;

typedef struct metac_semantic_state_t
{
    metac_identifier_table_t SemanticIdentifierTable;
    metac_identifier_table_t* ParserIdentifierTable;

    // has state such as the current stack offset and the like
    // to layout variables
    AT(transient) metac_sema_decl_state_t* CurrentDeclarationState;

    metac_scope_t* CurrentScope;

    metac_scope_t* ScopeStack;
    uint32_t ScopeStackSize;
    uint32_t ScopeStackCapacity;

    // metac_type_table_t* TypeTable;
    METAC_TYPE_TABLE_T(array) ArrayTypeTable;
    METAC_TYPE_TABLE_T(struct) StructTypeTable;
    METAC_TYPE_TABLE_T(ptr) PtrTypeTable;

    metac_sema_expression_t* ExpressionStack;
    uint32_t ExpressionStackSize;
    uint32_t ExpressionStackCapacity;

    // TODO remove this member when testing is done!
    declaration_store_t* declStore;

    metac_printer_t Printer;
} metac_semantic_state_t;

#define SemanticError(STATE, MSG, ...) \
    fprintf(stderr, "SemanticError[%s:%u]: "  MSG  "\n", __FILE__, __LINE__, __VA_ARGS__)

void RegisterType(metac_semantic_state_t* state, decl_type_t* type);
const char* TypeToChars(metac_semantic_state_t* self, metac_type_index_t typeIndex);
void MetaCSemantic_Init(metac_semantic_state_t* self, metac_parser_t* parser);

void MetaCSemantic_PushExpr(metac_semantic_state_t* self, metac_sema_expression_t* expr);
void MetaCSemantic_PopExpr(metac_semantic_state_t* self,  metac_sema_expression_t* expr);
metac_sema_expression_t* MetaCSemantic_doExprSemantic(metac_semantic_state_t* self,
                                                      metac_expression_t* expr);

/// Returns _emptyNode to signifiy it could not be found
/// a valid node otherwise
metac_node_header_t* MetaCSemantic_LookupIdentifier(metac_semantic_state_t* self,
                                                    uint32_t identifierKey,
                                                    metac_identifier_ptr_t identifierPtr);
