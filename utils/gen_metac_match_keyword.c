#include <stdio.h>

#include "../os/compat.h"
#include "../hash/crc32c.c"
#include "../parser/metac_lexer.h"
#include "../parser/metac_identifier_table.h"
#define USE_NO_ALIGN_MACROS

#include "kw_macros.h"

const char* C4Macros =
"#define C4(A, B, C, D) \\\n"
"    ((uint32_t)(A | B << 8 | C << 16 | D << 24))\n"
"\n"
"#define C2(A, B) \\\n"
"    ((uint16_t)(A | B << 8))\n"
"\n"
#ifdef USE_NO_ALIGN_MACROS
"#define U2(PTR, OFFSET) \\\n"
"    ((uint16_t)((*((PTR) + OFFSET + 0))      \\\n"
"              | (*((PTR) + OFFSET + 1)) << 8))\n"
"\n"
"#define U4(PTR, OFFSET) \\\n"
"    ((uint32_t)((*((PTR) + OFFSET + 0)) << 0  \\\n"
"              | (*((PTR) + OFFSET + 1)) << 8  \\\n"
"              | (*((PTR) + OFFSET + 2)) << 16 \\\n"
"              | (*((PTR) + OFFSET + 3)) << 24))\n";
#else
"#define U2(PTR, OFFSET) \\\n"
"    (*(uint16_t*)((PTR) + OFFSET))\n"
"\n"
"#define U4(PTR, OFFSET) \\\n"
"    (*(uint32_t*)((PTR) + OFFSET))\n"
"\n";
#endif

#include <stdio.h>
/*
    switch (tok->IdentifierKey)
    {
    case struct_key :
    if(!memcmp(identifier, "struct", 6))
            tok->TokenType = tok_kw_struct;
    break;

*/
void WriteCmp(const char* kw, uint32_t kw_len)
{
	printf("if (");

	uint32_t kw_pos = 0;
	while (kw_len >= 4)
	{
        kw_len -= 4;

        const char c1 = kw[kw_pos + 0];
        const char c2 = kw[kw_pos + 1];
        const char c3 = kw[kw_pos + 2];
        const char c4 = kw[kw_pos + 3];

        if (kw_pos != 0)
            printf("\n && ");
        else
            printf("\n    ");
		printf("U4(identifier, %u) == C4('%c', '%c', '%c', '%c')",
                             kw_pos,      c1,   c2,   c3,   c4);
        kw_pos += 4;
	}

    if (kw_pos && kw_len)
        printf("\n && ");
    else
        printf("\n    ");
    switch(kw_len)
    {
        case 3 :
        {
            const char c1 = kw[kw_pos];
            const char c2 = kw[kw_pos + 1];
            const char c3 = kw[kw_pos + 2];
            printf("U2(identifier, %u) == C2('%c', '%c')\n",
                                    kw_pos,   c1,   c2);
            printf(" && ((*(identifier + %u)) == '%c')\n", kw_pos + 2, c3);
        } break;
        case 2 :
        {
            const char c1 = kw[kw_pos];
            const char c2 = kw[kw_pos + 1];
            printf("U2(identifier, %u) == C2('%c', '%c')\n",
                                    kw_pos,   c1,   c2);
        } break;
        case 1 :
        {
            const char c1 = kw[kw_pos];
            printf("((*(identifier + %u)) == '%c')\n", kw_pos, c1);
        }
    }

	printf(")\n");
}

void WriteMatchFunction(void)
{
    printf("%s\n", C4Macros);

    printf("#include \"metac_keyword_keys.h\"\n");

    printf("\n\n");

    printf("// void MetaCLexerMatchKeywordIdentifier(metac_token_t* tok, const char* identifier)\n");
    printf("{\n");
    printf("    assert(tok->TokenType == tok_identifier);\n\n");

    printf("    switch (tok->IdentifierKey)\n");
    printf("    {\n");
//    WriteCmp(#KW + KW_PREFIX_LEN, KW_LEN(KW)); \

//    printf("    if(!memcmp(identifier, \"%s\", %u))\n", KW_STR(KW), KW_LEN(KW));

#define KW_WRITE_CASE(KW) \
    printf("    case %s_key :\n", KW_STR(KW)); \
    WriteCmp(KW_STR(KW), KW_LEN(KW)); \
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
