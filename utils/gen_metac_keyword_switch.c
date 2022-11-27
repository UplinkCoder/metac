#include "../os/compat.h"
#include "../hash/crc32c.c"
#include "../parser/metac_lexer.h"
#include "../parser/metac_identifier_table.h"
#include <stdio.h>
#define USE_NO_ALIGN_MACROS

#include "kw_macros.h"

typedef struct keyword_t
{
    const char* kw_chars;
    uint32_t kw_len;
} keyword_t;

#include <stdlib.h>

void GenerateSwitchStmts(char* outP, keyword_t* keywords, uint32_t n_keywords)
{
    char* out = outP;
    char* caseByStartingLetter[256] = {0};
    char* caseByStartingLetterStart[256] = {0};

    for(uint32_t i = 0; i < n_keywords; i++)
    {
        keyword_t* kw = keywords + i;

        const char startingLetter = kw->kw_chars[0];
        char** caseStr = &caseByStartingLetter[startingLetter];
        bool isFirst = false;
        if ((*caseStr) == 0)
        {
            isFirst = true;
            caseByStartingLetterStart[startingLetter] = (*caseStr) = cast(char*) malloc(1024);
            uint32_t len = sprintf((*caseStr), "    case '%c':\n", startingLetter);
            (*caseStr) += len;
        }

        uint32_t len;
        if (isFirst)
        {
            len = sprintf(*caseStr, "        if ");
        }
        else
        {
            len = sprintf(*caseStr, "        else if ");
        }
        (*caseStr) += len;

        len = sprintf(*caseStr, "(0 == strncmp(_chrs + 1, \"%s\", %u))\n"
                        "        {\n"
                        "            return tok_kw_%s;\n"
                        "        }\n",
                      kw->kw_chars + 1, (kw->kw_len - 1), kw->kw_chars
        );
        (*caseStr) += len;

    }

    bool writtenOutCase = 0;
    for(uint32_t i = 0; i < ARRAY_SIZE(caseByStartingLetter); i++)
    {
        char* caseP = caseByStartingLetterStart[i];
        if (caseP == 0)
            continue;

        uint32_t len = sprintf(out, "%s    break;\n", caseP);
        out += len;
    }
}

int main(int argc, char* argv[])
{
    uint32_t n_keywords = 0;
#define COUNT_KEYWORD(K) \
        n_keywords++;

    FOREACH_KEYWORD_TOKEN(COUNT_KEYWORD)

    char* out_str = cast(char*)malloc(65535);

    keyword_t* keywords = cast(keyword_t*)
        calloc(sizeof(keyword_t), n_keywords);


    uint32_t currentKw = 0;
#define FILL_KEYWORD(KW) \
    { keyword_t kw = { KW_STR(KW), KW_LEN(KW) }; \
    keywords[currentKw++] = kw; }

    FOREACH_KEYWORD_TOKEN(FILL_KEYWORD);

    GenerateSwitchStmts(out_str, keywords, n_keywords);

    printf("//cases to insert into switch\n%s", out_str);
}

#undef KW_PREFIX
#undef KW_LEN
#undef KW_KEY
#undef KW_CRC32C
