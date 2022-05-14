#include "metac_parsetree.h"
#include "metac_type_table.h"
#include "metac_printer.h"
#include "metac_sematree.h"

typedef struct metac_scope_ptr_t
{
    uint32_t v;
} metac_scope_ptr_t;

typedef struct metac_scope_t
{
    struct metac_scope_ptr_t Parent;
} metac_scope_t;

typedef struct metac_semantic_state_t
{
    metac_identifier_table_t SemanticIdentifierTable;
    metac_identifier_table_t* ParserIdentifierTable;

    metac_declaration_t* currentDeclaration;

    metac_scope_t* ScopeStack;
    uint32_t ScopeStackSize;
    uint32_t ScopeStackCapacity;

    // metac_type_table_t* TypeTable;
    METAC_TYPE_TABLE_T(array) ArrayTypeTable;
    METAC_TYPE_TABLE_T(struct) StructTypeTable;
    METAC_TYPE_TABLE_T(ptr) PtrTypeTable;

    metac_expression_t* ExpressionStack;
    uint32_t ExpressionStackSize;
    uint32_t ExpressionStackCapacity;

    metac_printer_t Printer;
} metac_semantic_state_t;

#define SemanticError(STATE, MSG, ...) \
    fprintf(stderr, "SemanticError[%s:%u]: "  MSG  "\n", __FILE__, __LINE__, __VA_ARGS__);

void RegisterType(metac_semantic_state_t* state, decl_type_t* type);
const char* TypeToChars(metac_semantic_state_t* self, metac_type_index_t typeIndex);
void MetaCSemantic_Init(metac_semantic_state_t* self);

void MetaCSemantic_PushExpr(metac_semantic_state_t* self, metac_sema_expression_t* expr);
void MetaCSemantic_PopExpr(metac_semantic_state_t* self,  metac_sema_expression_t* expr);
