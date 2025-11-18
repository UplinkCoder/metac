#include "../os/compat.h"
#include "../os/os.c"
#include "../hash/crc32c.c"
#include "../os/metac_alloc.c"
#include "../parser/metac_alloc_node.c"
#include "../parser/metac_lexer.c"
#include <stdio.h>

void CountTokenLengths();

int main(int argc, char* argv[])
{
    CountTokenLengths();
}

void CountTokenLengths()
{
    uint32_t token_len[16] = {0};

#define TOK_SELF(TOK) \
    TOK

    const metac_token_enum_t first_keyword = FIRST_KEYWORD_TOKEN(TOK_SELF);
    const metac_token_enum_t last_keyword = LAST_KEYWORD_TOKEN(TOK_SELF);

#define TOK_IS_KW(TOK) \
    (TOK >= first_keyword && TOK <= last_keyword)

#define KW_LEN(TOK) \
    ((unsigned int)(sizeof(#TOK) - sizeof("tok_kw_")))

#define TOK_LENGTH(TOK) \
    ( ((TOK_IS_KW(TOK)) ? (KW_LEN(TOK)) : MetaCStaticTokenLength(TOK)) )

#define COUNT_TOK(TOK) \
    token_len[TOK_LENGTH(TOK)]++;

    FOREACH_STATIC_TOKEN(COUNT_TOK)

    uint32_t currentMaxLenCount = 0;
    uint32_t currentMaxLenCanidate = 0;

    for(uint32_t* len = &token_len[0];
       len < (token_len + (sizeof(token_len) / sizeof(token_len[0])));
        len++)
    {
        unsigned int idx = (unsigned int)(len - token_len);
        if (currentMaxLenCount < *len)
        {
            currentMaxLenCanidate = idx;
            currentMaxLenCount = *len;
        }
        fprintf(stderr, "  %u tokens have length: %u\n", *len, idx);
    }

    fprintf(stderr, "MaxCountedLength: %u\n", currentMaxLenCanidate);

    printf("switch(tok)\n");
    printf("{\n");
    printf("    default : return %u;\n\n", currentMaxLenCanidate);

//    if (TOK_LENGTH(TOK) != currentMaxLenCanidate) \

#define SWITCH_PRINT(TOK) \
    printf("    case %s : return %u;\n", #TOK, TOK_LENGTH(TOK));

    FOREACH_STATIC_TOKEN(SWITCH_PRINT)

    printf("}\n");
}
