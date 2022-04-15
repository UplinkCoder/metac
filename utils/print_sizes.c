#include <stdio.h>
#include "metac_lexer.h"
#include "metac_parser.h"
void main()
{
    printf("%d token are defined\n", (int)tok_max);
    printf("%d expression types are defined\n", (int)exp_max);
    printf("\n\n");
    printf("sizeof(metac_token_t): %d\n", (int) sizeof(metac_token_t));
    printf("sizeof(metac_expression_t): %d\n", (int) sizeof(metac_expression_t));
    printf("sizeof(metac_declaration_t): %d\n", (int) sizeof(metac_declaration_t));
    printf("sizeof(metac_statement_t): %d\n", (int) sizeof(metac_statement_t));

#define PRINT_SIZE(STMT_KIND) \
    printf("sizeof("  #STMT_KIND  "): %d\n", (int) sizeof(STMT_KIND ## _t));

    FOREACH_STMT_KIND_(PRINT_SIZE)
}
