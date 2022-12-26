/// lpp stands for lexer, preprocessor, parser
/// and it's a struct to bundle those together
#ifndef _METAC_LPP_H_
#define _METAC_LPP_H_

#include <assert.h>
#include "../driver/metac_driver.h"
#include "../os/metac_file.h"

#ifndef NO_PREPROCESSOR
#  include "../parser/metac_preproc.h"
#endif

typedef struct metac_lpp_t
{
    metac_lexer_state_t LexerState;
    metac_lexer_t Lexer;
    metac_parser_t Parser;
#ifndef NO_PREPROCESSOR
    metac_preprocessor_t Preprocessor;
#endif
} metac_lpp_t;
/// Node that the allocator parameter cannot be null
/// while fileStorage can be.
void MetaCLPP_Init(metac_lpp_t* lpp, metac_alloc_t* allocator, metac_file_storage_t* fileStorage);

metac_expr_t* MetaCLPP_ParseExprFromString(metac_lpp_t* lpp, const char* exp);

metac_expr_t* MetaCLPP_ParseExpr2FromString(metac_lpp_t* lpp, const char* exp);

metac_stmt_t* MetaCLPP_ParseStmtFromString(metac_lpp_t* lpp, const char* stmt);

metac_decl_t* MetaCLPP_ParseDeclFromString(metac_lpp_t* lpp, const char* decl);

#ifndef NO_PREPROCESSOR
metac_preprocessor_directive_t MetaCLPP_ParsePreprocFromString(metac_lpp_t* lpp, const char* line,
                                                               metac_token_buffer_t* tokenBuffer);
#endif

decl_array_t ReadLexParse(const char* filename, metac_lpp_t* lpp, metac_alloc_t* parent);
#endif
