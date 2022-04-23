#include "../compat.h"
#include "../cache/crc32.c"
#include "../metac_lexer.h"

#include <stdio.h>
/*
void WriteCmp(const char* kw, uint32_t kw_len)
{
	printf("if (");

	uint32_t kw_pos = 0;
	while (kw_len > 4)
	{
		printf("identifier")

	}

	printf(")");
}
*/
void WriteMatchFunction(void)
{
#define KW_PREFIX \
    "tok_kw_"

#define KW_PREFIX_LEN \
    (sizeof(KW_PREFIX) - 1)

#define KW_LEN(KW) \
    ((unsigned int)(sizeof(#KW) - sizeof(KW_PREFIX)))

#define KW_CRC32C(KW) \
    ( crc32c(~0, #KW + KW_PREFIX_LEN, KW_LEN(KW)) )

#define KW_KEY(KW) \
    ( IDENTIFIER_KEY(KW_CRC32C(KW), KW_LEN(KW)) )

#define KW_STR(KW) \
    #KW + KW_PREFIX_LEN

#define KW_WRITE_DEFINE(KW) \
    printf("#define %s_key 0x%x\n", \
        KW_STR(KW), KW_KEY(KW));

    FOREACH_KEYWORD_TOKEN(KW_WRITE_DEFINE)

    printf("\n\n");
    printf("#if !defined(__TINYC__) && !defined(_MSC_VER)\n");
    printf("#  define memcmp __builtin_memcmp\n");
    printf("#endif\n");

    printf("// void MetaCLexerMatchKeywordIdentifier(metac_token_t* tok, const char* identifier)\n");
    printf("{\n");
    printf("    assert(tok->TokenType == tok_identifier);\n\n");

    printf("    switch (tok->IdentifierKey)\n");
    printf("    {\n");
//    WriteCmp(#KW + KW_PREFIX_LEN, KW_LEN(KW)); \

#define KW_WRITE_CASE(KW) \
    printf("    case %s_key :\n", KW_STR(KW)); \
    printf("    if(!memcmp(identifier, \"%s\", %u))\n", KW_STR(KW), KW_LEN(KW)); \
    printf("            tok->TokenType = %s;\n", #KW); \
    printf("    break;\n");

    FOREACH_KEYWORD_TOKEN(KW_WRITE_CASE)

#undef KW_WRITE_CASE
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
