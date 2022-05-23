#include "../compat.h"
#include "../crc32c.c"
#include "../metac_lexer.h"
#include "../metac_identifier_table.h"

#include "kw_macros.h"

void main(int argc, char* argv)
{
#define KW_WRITE_DEFINE(KW) \
    printf("#define %s_key 0x%x\n", \
        KW_STR(KW), KW_KEY(KW));

    FOREACH_KEYWORD_TOKEN(KW_WRITE_DEFINE)
}
