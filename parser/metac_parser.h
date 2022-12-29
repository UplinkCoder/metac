#ifndef _METAC_PARSER_H_
#define _METAC_PARSER_H_

#include "metac_identifier_table.h"

#define emptyPointer ((void*)0x1)

#include "../os/compat.h"
#include "../os/metac_alloc.h"
#include "../parser/metac_lexer.h"
#include "../parser/metac_parsetree.h"
#include "../printer/metac_printer.h"

#if !defined(NO_PREPROCESSOR)
#  include "../parser/metac_preproc.h"
#endif

#include "../parser/metac_identifier_table.h"

/*    M(expr_bin_invalid) \*/

typedef enum parse_expr_flags_t
{
    expr_flags_none   = 0,
    expr_flags_unary  = (1 << 0),
    expr_flags_binary = (1 << 1),
    expr_flags_call   = (1 << 2),
    expr_flags_enum   = (1 << 3),
    expr_flags_type   = (1 << 4),
    expr_flags_addr   = (1 << 5),
    expr_flags_sizeof = (1 << 6),
    expr_flags_pp     = (1 << 7),
} parse_expr_flags_t;

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

typedef void (*identifier_cb_t) (const char* idString, uint32_t idKey, void* userCtx);

typedef struct identifier_callback_t
{
    void (*FuncP)(const char* idString, uint32_t idKey, void* ctx);
    void* Ctx;
} identifier_callback_t;

typedef struct metac_expr_parser_t
{
    ARENA_ARRAY(metac_expr_t*, ExprStack)
    ARENA_ARRAY(parse_expr_flags_t, ExprFlagsStack)
    ARENA_ARRAY(metac_expr_kind_t, OpStack)
} metac_expr_parser_t;

typedef struct metac_parser_t
{
    metac_lexer_t* Lexer;
    metac_lexer_state_t* LexerState;

    uint32_t CurrentTokenIndex;
    metac_identifier_table_t IdentifierTable;
    metac_identifier_table_t StringTable;

#ifndef NO_PREPROCESSOR
    metac_preprocessor_t* Preprocessor;
#endif
    metac_location_t LastLocation;
    ARENA_ARRAY(identifier_callback_t, IdentifierCallbacks)
    metac_alloc_t Allocator;

    stmt_block_t* CurrentBlockStmt;

    uint16_t* PackStack;
    /// -1 means empty
    int32_t  PackStackTop;

    metac_expr_parser_t ExprParser;

    stmt_block_t** BlockStmtStack;
    uint32_t BlockStmtStackCount;
    uint32_t BlockStmtStackCapacity;

    uint32_t OpenParens;
    uint32_t PackStackCapacity;

    metac_token_t CurrentComment;
    decl_label_t* CurrentLabel;

    metac_location_t_array LocationStorage;

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

bool IsExprNode(metac_node_kind_t Kind);

bool IsBinaryAssignExp(metac_expr_kind_t expr_kind);
static bool IsBinaryExp(metac_expr_kind_t expr_kind);
static bool IsUnaryExp(metac_expr_kind_t expr_kind);

void MetaCParser_Init(metac_parser_t* self, metac_alloc_t* allocator);
void MetaCParser_InitFromLexer(metac_parser_t* self, metac_lexer_t* lexer, metac_alloc_t* allocator);

#define MetaCParser_PeekToken(SELF, P) \
    (MetaCParser_PeekToken_(SELF, P, __LINE__))

/// p is the offset from the current position, 0 reads the current token
/// negative offsets allow you to look back
metac_token_t* MetaCParser_PeekToken_(metac_parser_t* self, int32_t p, uint32_t line);
uint32_t MetaCParser_HowMuchLookahead(metac_parser_t* self);
metac_expr_t* MetaCParser_ParseExpr(metac_parser_t* self, parse_expr_flags_t flags, metac_expr_t* prev);
metac_expr_t* MetaCParser_ParseExpr2(metac_parser_t* self, parse_expr_flags_t flags);
metac_expr_t* MetaCParser_ParseExprFromString(const char* exp);
metac_expr_t* MetaCParser_ParseExpr2FromString(const char* exp);
metac_decl_t* MetaCParser_ParseDecl(metac_parser_t* self, metac_decl_t* parent);
metac_stmt_t* MetaCParser_ParseStmt(metac_parser_t* self, metac_stmt_t* parent, metac_stmt_t* prev);

#if !defined(NO_PREPROCESSOR)
metac_preprocessor_directive_t MetaCParser_ParsePreprocDirective(metac_parser_t* self,
                                                                 metac_preprocessor_t* preproc);
#endif

#define MetaCParser_Match(SELF, TYPE) \
    (MetaCParser_Match_((SELF), (TYPE), __FILE__, __LINE__))

metac_token_t* MetaCParser_Match_(metac_parser_t* self, metac_token_enum_t type,
                                  const char* filename, uint32_t lineNumber);

const char* MetaCNodeKind_toChars(metac_node_kind_t type);

static inline metac_location_t LocationFromToken(metac_parser_t* self,
                                                 metac_token_t* tok);

metac_identifier_ptr_t RegisterString(metac_parser_t* self,
                                      metac_token_t* token);

metac_identifier_ptr_t RegisterIdentifier(metac_parser_t* self,
                                          metac_token_t* token);

bool IsTypeToken(metac_token_enum_t tokenType);

#endif
