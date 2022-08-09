#include <stdio.h>

#include "metac_lexer.h"
#include "metac_parser.h"
#include "metac_sematree.h"

void main()
{
    printf("%d token are defined\n", (int)tok_error);
    printf("%d expression types are defined\n", (int)exp_max);
    printf("%d node types are defined\n", (int)node_max);

    printf("\n\n");
    printf("sizeof(metac_token_t): %d\n", (int) sizeof(metac_token_t));
    printf("sizeof(metac_lexer_t): %d\n", (int) sizeof(metac_lexer_t));
    printf("sizeof(scope_kind_t): %d\n", (int) sizeof(scope_kind_t));
    printf("sizeof(metac_location_t): %d\n", (int) sizeof(metac_location_t));
    printf("sizeof(sema_decl_function_t): %d\n", (int) sizeof(sema_decl_function_t));
    printf("sizeof(sema_stmt_for_t): %d\n", (int) sizeof(sema_stmt_for_t));

#if defined(IDENTIFIER_TABLE)
    printf("sizeof(metac_identifier_table_t): %d\n", (int) sizeof(metac_identifier_table_t));
#endif
    printf("sizeof(metac_token_t): %d\n", (int) sizeof(metac_token_t));

#define PRINT_SIZE(NODE_KIND) \
    printf("sizeof("  #NODE_KIND  "): %d\n", (int) sizeof(NODE_KIND ## _t));

    printf("sizeof(metac_expression_t): %d\n", (int) sizeof(metac_expression_t));
    printf("sizeof(metac_expression_header_t) %d\n", (int) sizeof(metac_expression_header_t));
    printf("sizeof(metac_sema_expression_t): %d\n", (int) sizeof(metac_sema_expression_t));
    printf("sizeof(metac_sema_expression_header_t) %d\n", (int) sizeof(metac_sema_expression_header_t));

    printf("sizeof(metac_declaration_t): %d\n", (int) sizeof(metac_declaration_t));
    FOREACH_DECL_KIND(PRINT_SIZE);

    printf("sizeof(metac_statement_t): %d\n", (int) sizeof(metac_statement_t));
    FOREACH_STMT_KIND(PRINT_SIZE)
}
