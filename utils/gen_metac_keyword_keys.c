#include "../os/compat.h"
#include "../hash/crc32c.c"
#include "../parser/metac_lexer.h"
#include "../parser/metac_identifier_table.h"

#include <stdio.h>
#include "kw_macros.h"

void main(int argc, const char* argv[])
{
#define KW_WRITE_DEFINE(KW) \
    printf("#define %s_key 0x%x\n", \
        KW_STR(KW), KW_KEY(KW));

    FOREACH_KEYWORD_TOKEN(KW_WRITE_DEFINE)
}
