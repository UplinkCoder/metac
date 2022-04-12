#include "../compat.h"
#include "../cache/crc32.c"
#include "../metac_lexer.h"

#include <stdio.h>
void WriteMatchFunction(void)
{
    printf("#ifndef _MSC_VER\n");
    printf("#  define memcmp __builtin_memcmp\n");
    printf("#endif\n");

    printf("// void MetaCLexerMatchKeywordIdentifier(metac_token_t* tok)\n");
    printf("{\n");
    printf("    assert(tok->TokenType == tok_identifier);\n\n");

    printf("    switch (tok->IdentifierKey)\n");
    printf("    {\n");

#define KW_PREFIX \
    "tok_kw_"

#define KW_PREFIX_LEN \
    (sizeof(KW_PREFIX) - 1)

#define KW_LEN(KW) \
    ((unsigned int)(sizeof(#KW) - sizeof(KW_PREFIX)))

#define KW_CRC32C(KW) \
    ( crc32c(~0, #KW + KW_PREFIX_LEN, KW_LEN(KW)) )

#define KW_KEY(KW) \
    ((uint32_t)( (KW_LEN(KW) << 16) | (KW_CRC32C(KW) & 0xFFFF) ))

#define KW_WRITE_CMP(KW) \
    printf("    case 0x%x :\n", KW_KEY(KW)); \
    printf("        if (!memcmp(tok->Identifier, \"%s\", %u))\n", \
        #KW + KW_PREFIX_LEN, KW_LEN(KW)); \
    printf("            tok->TokenType = %s;\n", #KW); \
    printf("    break;\n");


    FOREACH_KEYWORD_TOKEN(KW_WRITE_CMP)

#undef KW_WRITE_HASH
    printf("    }\n");
    printf("}\n");

}


int main(int argc, char* argv[])
{
    WriteMatchFunction();
}

#undef KW_PREFIX
#undef KW_LEN
#undef KW_KEY
#undef KW_CRC32C
