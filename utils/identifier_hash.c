#include "../os/compat.h"
#include "../hash/crc32c.c"
#include "../parser/metac_lexer.h"
#include "../parser/metac_identifier_table.h"
#include <stdio.h>
#include <string.h>

void print_case_for(const char* id)
{
    uint32_t len = strlen(id);
    uint32_t crc32 =
        crc32c(~0, id, len);
    uint32_t identifier_hash = IDENTIFIER_KEY(crc32, len);
    printf("bool Is%s(metac_token_t token)", id);
    printf("{\n");
    printf("    return (token->IdentifierHash == 0x%x) &&\n",
        identifier_hash);
    printf("      && (!memcmp(identifier, \"%s\", %u) )\n", id, len);
    printf("}\n\n");
}

int main(int argc, char* argv[])
{
    for(int arg = 1;
        arg < argc;
        arg++)
    {
        printf("// %s\n", argv[arg]);
        print_case_for(argv[arg]);
    }
}
