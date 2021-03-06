#ifndef _METAC_PARSER_H_
#define _METAC_PARSER_H_

#include "metac_identifier_table.h"

#define emptyPointer ((void*)0x1)

#include "compat.h"
#include "metac_lexer.h"
#include "metac_parsetree.h"

#ifndef NO_PREPROC
#  include "metac_preproc.h"
#endif

#include "metac_identifier_table.h"

#include "metac_dot_printer.h"

/*    M(exp_bin_invalid) \*/

typedef enum parse_expression_flags_t
{
    expr_flags_none,
    expr_flags_call = (1 << 0),
    expr_flags_unary = (1 << 1),
    expr_flags_enum = (1 << 2),
    expr_flags_type = (1 << 3),
    expr_flags_addr = (1 << 4),
    expr_flags_sizeof = (1 << 5),
    expr_flags_pp     = (1 << 6),
} parse_expression_flags_t;


typedef struct metac_define_t
{
    uint32_t IdentifierKey;
    metac_identifier_ptr_t IdentifierPtr;

    /// at which tokden positon the define is
    uint32_t TokenPosition;
    uint32_t SourceId;
    ///UINT32_MAX means variadic
    uint32_t NumberOfParameters;
} metac_define_t;

typedef struct metac_parser_t
{
    metac_lexer_t* Lexer;
    metac_lexer_state_t* LexerState;

    uint32_t CurrentTokenIndex;
    metac_identifier_table_t IdentifierTable;
    metac_identifier_table_t StringTable;
#ifndef NO_PREPROC
    metac_preprocessor_t* Preprocessor;
#endif
    metac_location_t LastLocation;
    metac_define_t* Defines;
    uint32_t DefineCount;
    uint32_t DefineCapacity;

    stmt_block_t* CurrentBlockStatement;

    uint16_t* PackStack;
    /// -1 means empty
    int32_t  PackStackTop;

    metac_dot_printer_t* DotPrinter;

    metac_define_t inlineDefines[8];

    stmt_block_t** BlockStatementStack;
    uint32_t BlockStatementStackSize;
    uint32_t BlockStatementStackCapacity;

    uint32_t OpenParens;
    uint32_t PackStackCapacity;

    metac_token_t CurrentComment;
    decl_label_t* CurrentLabel;

    metac_location_storage_t LocationStorage;

#ifdef METAC_REPL
    task_t* ReplTask;
#endif
    metac_printer_t DebugPrinter;

    metac_identifier_ptr_t SpecialNamePtr_Compiler;
    metac_identifier_ptr_t SpecialNamePtr_Context;
    metac_identifier_ptr_t SpecialNamePtr_Target;
    metac_identifier_ptr_t SpecialNamePtr_Type;
    metac_identifier_ptr_t SpecialNamePtr_Defined;
} metac_parser_t;

extern metac_parser_t g_lineParser;
bool IsBinaryAssignExp(metac_expression_kind_t exp_kind);
bool IsBinaryExp(metac_expression_kind_t exp_kind);

void MetaCParser_Init(metac_parser_t* self);
void MetaCParser_InitFromLexer(metac_parser_t* self, metac_lexer_t* lexer);
/// p is the offset from the current position, 0 reads the current token
/// negative offsets allow you to look back
metac_token_t* MetaCParser_PeekToken(metac_parser_t* self, int32_t p);
uint32_t MetaCParser_HowMuchLookahead(metac_parser_t* self);
metac_expression_t* MetaCParser_ParseExpression(metac_parser_t* self, parse_expression_flags_t flags, metac_expression_t* prev);
metac_expression_t* MetaCParser_ParseExpressionFromString(const char* exp);
metac_declaration_t* MetaCParser_ParseDeclaration(metac_parser_t* self, metac_declaration_t* parent);

#ifndef NO_PREPROC
metac_preprocessor_directive_t MetaCParser_ParsePreproc(metac_parser_t* self,
                                                        metac_preprocessor_t* preproc,
                                                        metac_token_buffer_t* buffer);
#endif

#define MetaCParser_Match(SELF, TYPE) \
    (MetaCParser_Match_((SELF), (TYPE), __FILE__, __LINE__))

metac_token_t* MetaCParser_Match_(metac_parser_t* self, metac_token_enum_t type,
                                  const char* filename, uint32_t lineNumber);

const char* MetaCNodeKind_toChars(metac_node_kind_t type);
#endif
