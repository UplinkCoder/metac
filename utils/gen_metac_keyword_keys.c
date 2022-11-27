#include <stdio.h>

#include "../os/compat.h"
#include "../hash/crc32c.c"
#include "../parser/metac_lexer.h"
#include "../parser/metac_identifier_table.h"

#include "kw_macros.h"

int main(int argc, const char* argv[])
{
#define KW_WRITE_DEFINE(KW) \
    printf("#define %s_key 0x%x\n", \
        KW_STR(KW), KW_KEY(KW));

    FOREACH_KEYWORD_TOKEN(KW_WRITE_DEFINE)

    return 0;
}
