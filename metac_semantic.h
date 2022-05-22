#include "metac_semantic_lru.h"
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

#define DECLARE_TYPE_TABLE(TYPE_NAME, MEMBER_NAME, INDEX_KIND) \
    METAC_TYPE_TABLE_T(TYPE_NAME) MEMBER_NAME;

#define FOREACH_TYPE_TABLE(M) \
    M(enum,   EnumTypeTable, enum) \
    M(array, ArrayTypeTable, array) \
    M(aggregate, StructTypeTable, struct) \
    M(ptr, PtrTypeTable, ptr) \
    M(aggregate, UnionTypeTable, union)

typedef struct metac_semantic_state_t
{
    bool initialized;
    metac_identifier_table_t SemanticIdentifierTable;
    metac_identifier_table_t* ParserIdentifierTable;

    // has state such as the current stack offset and the like
    // to layout variables
    AT(transient) metac_sema_decl_state_t* CurrentDeclarationState;

    AT(transient) metac_scope_t* CurrentScope;

    metac_semantic_lru_t LRU;

    // metac_type_table_t* TypeTable;
    FOREACH_TYPE_TABLE(DECLARE_TYPE_TABLE)

    metac_sema_expression_t* ExpressionStack;
    uint32_t ExpressionStackSize;
    uint32_t ExpressionStackCapacity;

    sema_type_aggregate_t* CompilerInterface;

    // TODO remove this member when testing is done!
    declaration_store_t* declStore;

    metac_printer_t Printer;
} metac_semantic_state_t;

#define SemanticError(STATE, MSG, ...) \
    fprintf(stderr, "SemanticError[%s:%u]: "  MSG  "\n", __FILE__, __LINE__, __VA_ARGS__)

void RegisterType(metac_semantic_state_t* state, decl_type_t* type);
const char* TypeToChars(metac_semantic_state_t* self, metac_type_index_t typeIndex);
void MetaCSemantic_Init(metac_semantic_state_t* self,
                        metac_parser_t* parser,
                        decl_type_struct_t* compilerStruct);

void MetaCSemantic_PushExpr(metac_semantic_state_t* self, metac_sema_expression_t* expr);
void MetaCSemantic_PopExpr(metac_semantic_state_t* self,  metac_sema_expression_t* expr);

#define MetaCSemantic_doExprSemantic(SELF, NODE) \
    MetaCSemantic_doExprSemantic_(SELF, ((metac_expression_t*)(NODE)), \
                                  __FUNCTION__, __LINE__)

metac_sema_expression_t* MetaCSemantic_doExprSemantic_(metac_semantic_state_t* self,
                                                       metac_expression_t* expr,
                                                       const char* callFun,
                                                       uint32_t callLine);

#define MetaCSemantic_doStatementSemantic(SELF, NODE) \
    MetaCSemantic_doStatementSemantic_(SELF, ((metac_statement_t*)(NODE)), \
                                       __FUNCTION__, __LINE__)

metac_sema_statement_t* MetaCSemantic_doStatementSemantic_(metac_semantic_state_t* self,
                                                           metac_statement_t* stmt,
                                                           const char* callFun,
                                                           uint32_t callLine);

#define MetaCSemantic_doDeclSemantic(SELF, NODE) \
    MetaCSemantic_doDeclSemantic_(SELF, ((metac_declaration_t*)(NODE)), \
                                  __FUNCTION__, __LINE__)

metac_sema_declaration_t* MetaCSemantic_doDeclSemantic_(metac_semantic_state_t* self,
                                                        metac_declaration_t* decl,
                                                        const char* callFun,
                                                        uint32_t callLine);

#define MetaCSemantic_doTypeSemantic(SELF, NODE) \
    MetaCSemantic_doTypeSemantic_(SELF, ((decl_type_t*)(NODE)), \
                                  __FUNCTION__, __LINE__)

metac_type_index_t MetaCSemantic_doTypeSemantic_(metac_semantic_state_t* self,
                                                decl_type_t* type,
                                                const char* callFun, uint32_t callLine);

/// Returns _emptyNode to signifiy it could not be found
/// a valid node otherwise
metac_node_header_t* MetaCSemantic_LookupIdentifier(metac_semantic_state_t* self,
                                                    metac_identifier_ptr_t identifierPtr);
