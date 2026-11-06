// tiny harness: runs the metac preprocessor over the test_*.c files in this
// directory and prints the resulting token stream.
//
// usage: preproc_test <file.c>
//
// The harness walks the raw token stream itself (directives, conditionals,
// macro expansion) instead of driving metac_parser's ParseDecl machinery,
// which keeps the parser's peek-time auto-expansion out of the picture.
// The amalgamation (metac_parser_obj.c) is still linked in, because the
// preproc helpers live there.
//
// build:
//   cc -Iparser \
//      -DNO_DOT_PRINTER -DNO_SEMANTIC -DNO_FIBERS -DOLD_PARSER=1 \
//      test/preprocessor_tests/preproc_test.c \
//      -o test/preprocessor_tests/preproc_test

#include "../../parser/metac_parser_obj.c"

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>

static bool errored = false;

static void LexStringIntoLexer(metac_lexer_t* lexer, const char* str, int32_t length)
{
    metac_lexer_state_t lexer_state =
        MetaCLexerStateFromBuffer(1, str, length);

    while(length > 0)
    {
        uint32_t initialPosition = lexer_state.Position;

        metac_token_t token =
            *MetaCLexerLexNextToken(lexer, &lexer_state, str, length);

        uint32_t eaten = lexer_state.Position - initialPosition;
        if (eaten == 0)
        {
            fprintf(stderr, "No chars consumed ... Token: %s\n",
                MetaCTokenEnum_toChars(token.TokenType));
            errored = true;
            break;
        }
        str += eaten;
        length -= eaten;
    }
}

static char* ReadFileIntoMemory(const char* filename)
{
    char* result = 0;

    FILE* file = fopen(filename, "rb");
    if (!file)
    {
        fprintf(stderr, "cannot read '%s'\n", filename);
        errored = true;
        return 0;
    }

    fseek(file, 0, SEEK_END);
    long fileSize = ftell(file);
    fseek(file, 0, SEEK_SET);

    result = (char*) calloc(1, fileSize + 1);
    fread(result, 1, fileSize, file);
    fclose(file);

    return result;
}


// ---------------------------------------------------------------------------
// token text helpers
// ---------------------------------------------------------------------------

static metac_lexer_t* globalLexer;
static metac_preprocessor_t* globalPreproc;

// finds the chars of an identifier, checking the key's checksum so
// paste/stringize tokens living in any of the tables still resolve
static const char* FindIdentifierChars(metac_token_t* tok, uint32_t* outLength)
{
    uint32_t key = tok->IdentifierKey;
    uint32_t length = LENGTH_FROM_IDENTIFIER_KEY(key);
    const char* idChars = 0;
    metac_identifier_table_t* tables[] = {
        &globalLexer->IdentifierTable,
        &globalPreproc->IdentifierTable,
        &globalPreproc->DefineIdentifierTable,
    };

    for(uint32_t i = 0; i < ARRAY_SIZE(tables); i++)
    {
        metac_identifier_table_slot_t* slot = tables[i]->Slots;
        uint32_t slotCount = (1 << tables[i]->SlotCount_Log2);
        for(uint32_t s = 0; s < slotCount; s++)
        {
            if (slot[s].HashKey == key)
            {
                const char* chars =
                    IdentifierPtrToCharPtr(tables[i], slot[s].Ptr);
                uint32_t textHash =
                    crc32c_nozero(~0, chars, length);
                if (IDENTIFIER_KEY(textHash, length) == key)
                {
                    idChars = chars;
                }
                break;
            }
        }
        if (idChars)
            break;
    }

    *outLength = length;
    return idChars;
}

static const char* FindStringChars(metac_token_t* tok, uint32_t* outLength)
{
    uint32_t key = tok->StringKey;
    uint32_t length = LENGTH_FROM_STRING_KEY(key);
    metac_identifier_table_slot_t* slot = globalLexer->StringTable.Slots;
    uint32_t slotCount = (1 << globalLexer->StringTable.SlotCount_Log2);
    for(uint32_t s = 0; s < slotCount; s++)
    {
        if (slot[s].HashKey == key)
        {
            *outLength = length;
            return IdentifierPtrToCharPtr(&globalLexer->StringTable, slot[s].Ptr);
        }
    }
    *outLength = 0;
    return 0;
}


// directive names can be keywords ('#if', '#else') or plain identifiers
static bool IsDirectiveNameToken(const metac_token_t* tok)
{
    if (tok->TokenType == tok_identifier)
        return true;
#ifndef IS_DIR_KEYWORD_BOUND
#define IS_DIR_KEYWORD_BOUND
    return (tok->TokenType >= tok_kw_struct) &&
           (tok->TokenType <= tok_kw___attribute__);
#undef IS_DIR_KEYWORD_BOUND
#endif
}

// the source line the token was lexed on; 0 for synthesized tokens
static uint32_t TokenStartLine(const metac_token_t* tok)
{
    if (tok->LocationId.v == 0)
        return 0;
    metac_location_t* loc =
        &globalLexer->LocationStorage.Locations[tok->LocationId.v - 4];
    return loc->StartLine;
}

// prints the spelling of a token to a FILE
static void PrintTokenText(FILE* out, metac_token_t* tok)
{
    switch(tok->TokenType)
    {
        case tok_newline: return; // whitespace-stripped compare: lines don't matter

        case tok_identifier:
        {
            uint32_t length = 0;
            const char* chars = FindIdentifierChars(tok, &length);
            if (!chars)
            {
                fprintf(stderr, "unresolvable identifier (key %x)\n",
                    tok->IdentifierKey);
                errored = true;
            }
            fwrite(chars ? chars : "", 1, length, out);
        } break;

        case tok_string:
        {
            uint32_t length = 0;
            const char* chars = FindStringChars(tok, &length);
            fputc('"', out);
            fwrite(chars ? chars : "", 1, length, out);
            fputc('"', out);
        } break;

        case tok_uint:
        {
            fprintf(out, "%u", (uint32_t) tok->ValueU64);
        } break;
        case tok_float:
        case tok_double:
        {
            fprintf(out, "%g", tok->ValueF52);
        } break;
        case tok_char:
        {
            fwrite(tok->chars, 1, tok->charLength, out);
        } break;

        default:
        {
            static const struct
            {
                metac_token_enum_t type;
                const char* chars;
            } punct[] =
            {
                { tok_bang,      "!"  }, { tok_question,  "?"  },
                { tok_hash,      "#"  }, { tok_at,        "@"  },
                { tok_lParen,    "("  }, { tok_rParen,    ")"  },
                { tok_lBrace,    "{"  }, { tok_rBrace,    "}"  },
                { tok_lBracket,  "["  }, { tok_rBracket,  "]"  },
                { tok_semicolon, ";"  }, { tok_colon,     ":"  },
                { tok_dollar,    "$"  }, { tok_tilde,     "~"  },
                { tok_comma,     ","  },
                { tok_dot,       "."  },
                { tok_plus,      "+"  }, { tok_minus,     "-"  },
                { tok_star,      "*"  }, { tok_div,       "/"  },
                { tok_rem,       "%"  }, { tok_xor,       "^"  },
                { tok_or,        "|"  }, { tok_and,       "&"  },
                { tok_lsh,       "<<" }, { tok_rsh,       ">>" },
                { tok_oror,      "||" }, { tok_andand,    "&&" },
                { tok_arrow,     "->" },
                { tok_assign,    "="  },
                { tok_add_ass,   "+=" }, { tok_sub_ass,   "-=" },
                { tok_mul_ass,   "*=" }, { tok_div_ass,   "/=" },
                { tok_rem_ass,   "%=" }, { tok_xor_ass,   "^=" },
                { tok_or_ass,    "|=" }, { tok_and_ass,   "&=" },
                { tok_lsh_ass,   "<<="}, { tok_rsh_ass,   ">>="},
                { tok_equals_equals, "==" },
                { tok_not_equal,     "!=" },
                { tok_lt,        "<"  }, { tok_gt,        ">"  },
                { tok_le,        "<=" }, { tok_ge,        ">=" },
                { tok_dotdotdot, "..." },
            };
            for(uint32_t i = 0; i < ARRAY_SIZE(punct); i++)
            {
                if (punct[i].type == tok->TokenType)
                {
                    fputs(punct[i].chars, out);
                    return;
                }
            }

            const char* name = MetaCTokenEnum_toChars(tok->TokenType);
            if (!strncmp(name, "tok_kw_", 7))
            {
                fputs(name + 7, out);
            }
            else
            {
                fprintf(stderr, "cannot print token type '%s'\n", name);
                errored = true;
            }
        } break;
    }
}

// builds a token from a spelling, registering it in the lexer tables
static metac_token_t MakeSpellingToken(const char* chars, uint32_t length,
                                       bool isString)
{
    metac_token_t result = {0};
    uint32_t hash = crc32c_nozero(~0, chars, length);

    if (isString)
    {
        result.TokenType = tok_string;
        result.StringKey = STRING_KEY(hash, length);
        result.StringPtr =
            GetOrAddIdentifier(&globalLexer->StringTable, result.StringKey, chars);
    }
    else
    {
        result.TokenType = tok_identifier;
        result.IdentifierKey = IDENTIFIER_KEY(hash, length);
        result.IdentifierPtr =
            GetOrAddIdentifier(&globalLexer->IdentifierTable,
                               result.IdentifierKey, chars);
    }
    return result;
}

// joins the spellings of a token list with single spaces
#define SPELLING_CAP 512
static void SpellingFromTokens(const metac_token_t* tokens, uint32_t count,
                               char* buf, uint32_t* pos)
{
    for(uint32_t i = 0; i < count; i++)
    {
        if (*pos + 2 >= SPELLING_CAP)
            break;

        if (i > 0 && buf[*pos - 1] != ' ')
            buf[(*pos)++] = ' ';

        metac_token_t tok = tokens[i];
        switch(tok.TokenType)
        {
            case tok_identifier:
            {
                uint32_t length = 0;
                const char* chars = FindIdentifierChars(&tok, &length);
                if (chars && *pos + length <= SPELLING_CAP)
                {
                    memcpy(buf + *pos, chars, length);
                    *pos += length;
                }
            } break;
            case tok_uint:
            {
                int written =
                    snprintf(buf + *pos, SPELLING_CAP - *pos, "%u",
                             (uint32_t) tok.ValueU64);
                if (written > 0)
                    *pos += written;
            } break;
            case tok_string:
            {
                uint32_t length = 0;
                const char* chars = FindStringChars(&tok, &length);
                if (chars && *pos + length + 2 <= SPELLING_CAP)
                {
                    buf[(*pos)++] = '"';
                    memcpy(buf + *pos, chars, length);
                    *pos += length;
                    buf[(*pos)++] = '"';
                }
            } break;
            default:
            {
                // write into a memory buffer to reuse PrintTokenText
                char tmp[64] = {0};
                FILE* memOut = fmemopen(tmp, sizeof(tmp) - 1, "w");
                PrintTokenText(memOut, &tok);
                fclose(memOut);
                uint32_t length = strlen(tmp);
                if (*pos + length <= SPELLING_CAP)
                {
                    memcpy(buf + *pos, tmp, length);
                    *pos += length;
                }
            } break;
        }
    }
}


// ---------------------------------------------------------------------------
// macro store
// ---------------------------------------------------------------------------

#define MAX_MACROS 64
#define MAX_PARAMS 16
#define MAX_BODY   256

typedef struct harness_macro_t
{
    bool Used;
    metac_token_t nameTok;
    uint32_t ParamPtrs[MAX_PARAMS];
    uint32_t ParamCount;
    bool IsVariadic;
    metac_token_t Body[MAX_BODY];
    uint32_t BodyCount;
} harness_macro_t;

static harness_macro_t macros[MAX_MACROS];
static uint32_t macroCount = 0;

static harness_macro_t* LookupMacro(metac_token_t tok)
{
    if (tok.TokenType != tok_identifier)
        return 0;

    for(uint32_t i = 0; i < macroCount; i++)
    {
        harness_macro_t* macro = &macros[i];
        if (macro->Used && macro->nameTok.IdentifierKey == tok.IdentifierKey)
            return macro;
    }
    return 0;
}

static harness_macro_t* LookupMacroByName(const char* name, uint32_t length)
{
    uint32_t hash = crc32c_nozero(~0, name, length);
    uint32_t key = IDENTIFIER_KEY(hash, length);

    for(uint32_t i = 0; i < macroCount; i++)
    {
        if (macros[i].Used && macros[i].nameTok.IdentifierKey == key)
            return &macros[i];
    }
    return 0;
}

static void UndefineMacro(metac_token_t nameTok)
{
    for(uint32_t i = 0; i < macroCount; i++)
    {
        if (macros[i].Used &&
            macros[i].nameTok.IdentifierKey == nameTok.IdentifierKey)
        {
            macros[i].Used = false;
            return;
        }
    }
}

// ---------------------------------------------------------------------------
// expansion output
// ---------------------------------------------------------------------------

#define OUT_CAP (256 * 1024)
static metac_token_t outTokens[OUT_CAP];
static uint32_t outCount = 0;

static void OutEmit(metac_token_t tok)
{
    assert(outCount < OUT_CAP);
    outTokens[outCount++] = tok;
}

// pastes the spelling of tok to the last emitted token
static void OutPaste(metac_token_t tok)
{
    assert(outCount > 0);
    char buf[SPELLING_CAP];
    uint32_t pos = 0;
    SpellingFromTokens(&outTokens[outCount - 1], 1, buf, &pos);
    SpellingFromTokens(&tok, 1, buf, &pos);
    buf[pos] = 0;

    outTokens[outCount - 1] = MakeSpellingToken(buf, pos, false);
}

// ---------------------------------------------------------------------------
// context stack (the walker)
// ---------------------------------------------------------------------------

typedef struct ctx_t
{
    uint32_t Cursor;
    uint32_t End;
    uint32_t CurrentLine; // source line of the last processed token;
                          // directives only start on later lines
} ctx_t;

#define MAX_CTX 16
static ctx_t ctxStack[MAX_CTX];
static uint32_t ctxCount = 0;

typedef struct toklist_t
{
    const metac_token_t* Ptr;
    uint32_t Count;
} toklist_t;

// ---------------------------------------------------------------------------
// conditional inclusion stack
// ---------------------------------------------------------------------------

#define MAX_CONDS 64

typedef struct cond_entry_t
{
    bool ParentActive;
    bool WasActive;
    bool BranchTaken;
    bool SawElse;
} cond_entry_t;

static cond_entry_t condStack[MAX_CONDS];
static uint32_t condCount = 0;

static bool CondsActive(void)
{
    for(uint32_t i = 0; i < condCount; i++)
    {
        if (condStack[i].ParentActive && !condStack[i].WasActive)
            return false;
    }
    return true;
}


// ---------------------------------------------------------------------------
// tiny #if evaluator (numbers, identifiers, defined(), && || == != > < && parens)
// ---------------------------------------------------------------------------

#define MAX_EVAL_DEPTH 64

// evaluates a macro's body as an integer: recursively resolving
// object-like macros; anything not a number evaluates to 0
static int64_t EvalIdentifier(metac_token_t* identTok);
static int64_t EvalExpr(const metac_token_t* tokens, uint32_t count,
                        uint32_t* cursor);

static int64_t EvalIdentifier(metac_token_t* identTok)
{
    uint32_t length = 0;
    const char* chars = FindIdentifierChars(identTok, &length);
    metac_token_t* body = 0;
    uint32_t bodyCount = 0;

    harness_macro_t* macro = LookupMacro(*identTok);
    if (macro && macro->ParamCount == 0)
    {
        body = macro->Body;
        bodyCount = macro->BodyCount;
    }

    (void) chars;
    (void) length;

    if (bodyCount == 1 && body[0].TokenType == tok_uint)
        return (int64_t) body[0].ValueU64;
    if (bodyCount == 2 && body[0].TokenType == tok_minus &&
        body[1].TokenType == tok_uint)
        return -(int64_t) body[1].ValueU64;
    return 0;
}

// expr := and (|| and)*
// and  := cmp (&& cmp)*
// cmp  := prim (==,!=,>,<,<=,>= prim)?
// prim := ( expr ) | defined(name) | defined name | number | identifier | -prim
static int64_t EvalPrim(const metac_token_t* tokens, uint32_t count,
                        uint32_t* cursor)
{
    if (*cursor >= count)
        return 0;

    metac_token_t tok = tokens[*cursor];
    switch(tok.TokenType)
    {
        case tok_lParen:
        {
            (*cursor)++;
            int64_t result = EvalExpr(tokens, count, cursor);
            if (*cursor < count && tokens[*cursor].TokenType == tok_rParen)
                (*cursor)++;
            else
                errored = true;
            return result;
        }
        case tok_minus:
        {
            (*cursor)++;
            return -EvalPrim(tokens, count, cursor);
        }
        case tok_bang:
        {
            (*cursor)++;
            return !EvalPrim(tokens, count, cursor);
        }
        case tok_uint:
        {
            (*cursor)++;
            return (int64_t) tok.ValueU64;
        }
        case tok_identifier:
        {
            (*cursor)++;

            uint32_t length = 0;
            const char* chars = FindIdentifierChars(&tok, &length);
            if (length == 7 && chars && !memcmp(chars, "defined", 7))
            {
                bool paren = false;
                if (*cursor < count && tokens[*cursor].TokenType == tok_lParen)
                {
                    paren = true;
                    (*cursor)++;
                }
                metac_token_t name = {0};
                if (*cursor < count)
                {
                    name = tokens[*cursor];
                    (*cursor)++;
                }
                else
                {
                    errored = true;
                }
                if (paren)
                {
                    if (*cursor < count && tokens[*cursor].TokenType == tok_rParen)
                        (*cursor)++;
                    else
                        errored = true;
                }
                metac_token_t pinTok = name;
                return LookupMacro(pinTok) != 0;
            }
            return EvalIdentifier(&tok);
        }
        default:
        {
            (*cursor)++;
            errored = true;
            return 0;
        }
    }
}

static int64_t EvalCmp(const metac_token_t* tokens, uint32_t count,
                       uint32_t* cursor)
{
    int64_t left = EvalPrim(tokens, count, cursor);
    if (*cursor >= count)
        return left;

    metac_token_t op = tokens[*cursor];
    switch(op.TokenType)
    {
        case tok_equals_equals: (*cursor)++; return left == EvalPrim(tokens, count, cursor);
        case tok_not_equal:     (*cursor)++; return left != EvalPrim(tokens, count, cursor);
        case tok_lt:            (*cursor)++; return left <  EvalPrim(tokens, count, cursor);
        case tok_gt:            (*cursor)++; return left >  EvalPrim(tokens, count, cursor);
        case tok_le:            (*cursor)++; return left <= EvalPrim(tokens, count, cursor);
        case tok_ge:            (*cursor)++; return left >= EvalPrim(tokens, count, cursor);
        default: return left;
    }
}

static int64_t EvalAnd(const metac_token_t* tokens, uint32_t count,
                       uint32_t* cursor)
{
    int64_t left = EvalCmp(tokens, count, cursor);
    while(*cursor < count && tokens[*cursor].TokenType == tok_andand)
    {
        (*cursor)++;
        int64_t right = EvalCmp(tokens, count, cursor);
        left = (left && right);
    }
    return left;
}

static int64_t EvalExpr(const metac_token_t* tokens, uint32_t count,
                        uint32_t* cursor)
{
    int64_t left = EvalAnd(tokens, count, cursor);
    while(*cursor < count && tokens[*cursor].TokenType == tok_oror)
    {
        (*cursor)++;
        int64_t right = EvalAnd(tokens, count, cursor);
        left = (left || right);
    }
    return left;
}


// ---------------------------------------------------------------------------
// directive handling
// ---------------------------------------------------------------------------

// pushes a new context for the tokens lexed from an included file
static void PushCtxForRecentTokens(void)
{
    assert(ctxCount < MAX_CTX);
    ctxStack[ctxCount].Cursor = ctxStack[ctxCount - 1].End;
    ctxStack[ctxCount].End = globalLexer->TokenCount;
    ctxStack[ctxCount].CurrentLine = 0;
    ctxCount++;
}

static void HandleInclude(const metac_token_t* tokens, uint32_t count,
                          uint32_t* cursor, bool systemInclude)
{
    metac_token_t nameTok = {0};
    bool haveName = false;

    if (systemInclude)
    {
        // <stdio.h> style: consume everything up to >
        while(*cursor < count && tokens[*cursor].TokenType != tok_gt)
            (*cursor)++;
        if (*cursor < count)
            (*cursor)++;
    }
    else
    {
        if (*cursor >= count)
        {
            errored = true;
            return;
        }
        nameTok = tokens[(*cursor)++];
        if (nameTok.TokenType != tok_string)
        {
            fprintf(stderr, "expected a quoted include name\n");
            errored = true;
            return;
        }

        uint32_t length = 0;
        const char* filename = FindStringChars(&nameTok, &length);

        char path[512];
        memcpy(path, filename, length);
        path[length] = 0;

        char* text = ReadFileIntoMemory(path);
        if (text)
        {
            LexStringIntoLexer(globalLexer, text, cast(int32_t) strlen(text));
            free(text);
            PushCtxForRecentTokens();
        }
    }
    (void) haveName;
}

static void HandleDefine(const metac_token_t* tokens, uint32_t count,
                         uint32_t* cursor)
{
    if (*cursor >= count || tokens[*cursor].TokenType != tok_identifier)
    {
        fprintf(stderr, "macro name expected after #define\n");
        errored = true;
        return;
    }
    metac_token_t nameTok = tokens[(*cursor)++];

    harness_macro_t* macro = 0;
    for(uint32_t i = 0; i < MAX_MACROS; i++)
    {
        if (!macros[i].Used)
        {
            macro = &macros[i];
            break;
        }
    }
    if (!macro)
    {
        fprintf(stderr, "out of macro slots\n");
        errored = true;
        return;
    }
    memset(macro, 0, sizeof(*macro));
    macro->Used = true;
    macro->nameTok = nameTok;
    uint32_t macroIndex = macro - macros;
    if (macroIndex >= macroCount)
        macroCount = macroIndex + 1;

    bool isMacro = false;
    if (*cursor < count && tokens[*cursor].TokenType == tok_lParen)
    {
        metac_token_t lParen = tokens[*cursor];
        uint32_t nameEnd = nameTok.Position +
            LENGTH_FROM_IDENTIFIER_KEY(nameTok.IdentifierKey);
        uint32_t nameLen = LENGTH_FROM_IDENTIFIER_KEY(nameTok.IdentifierKey);
        (void) nameLen;
        if (lParen.Position == nameEnd)
            isMacro = true;
    }

    if (isMacro)
    {
        (*cursor)++; // (
        while(*cursor < count && tokens[*cursor].TokenType == tok_identifier)
        {
            if (macro->ParamCount < MAX_PARAMS)
            {
                metac_token_t param = tokens[(*cursor)++];
                macro->ParamPtrs[macro->ParamCount++] = param.IdentifierPtr.v;
            }
            else
            {
                (*cursor)++;
            }

            if (*cursor < count && tokens[*cursor].TokenType == tok_comma)
                (*cursor)++;
            else
                break;
        }
        if (*cursor < count && tokens[*cursor].TokenType == tok_dotdotdot)
        {
            macro->IsVariadic = true;
            (*cursor)++;
        }
        if (*cursor < count && tokens[*cursor].TokenType == tok_rParen)
            (*cursor)++;
        else
        {
            fprintf(stderr, "unterminated macro parameter list\n");
            errored = true;
        }
    }

    // body: everything up to the end of the line, with parameters
    // rewritten to tok_macro_parameter markers
    while(*cursor < count && tokens[*cursor].TokenType != tok_newline)
    {
        if (macro->BodyCount >= MAX_BODY)
        {
            fprintf(stderr, "macro body too long\n");
            errored = true;
            return;
        }

        metac_token_t tok = tokens[(*cursor)++];
        if (tok.TokenType == tok_identifier)
        {
            bool isParam = false;
            for(uint32_t i = 0; i < macro->ParamCount; i++)
            {
                if (tok.IdentifierPtr.v == macro->ParamPtrs[i])
                {
                    metac_token_t paramTok = tok;
                    paramTok.TokenType = tok_macro_parameter;
                    paramTok.MacroParameterIndex = i;
                    macro->Body[macro->BodyCount++] = paramTok;
                    isParam = true;
                    break;
                }
            }
            if (isParam)
                continue;
        }
        if (tok.TokenType == tok_macro_parameter)
        {
            // should not happen outside a body scan; guard anyway
            fprintf(stderr, "unexpected macro parameter token\n");
            errored = true;
        }
        macro->Body[macro->BodyCount++] = tok;
    }
}


// activates/deactivates conditional frames; tokens are not emitted while
// the top of the stack is inactive
static void HandleConditional(const metac_token_t* tokens, uint32_t count,
                              uint32_t* cursor, metac_preprocessor_directive_t directive)
{
    switch(directive)
    {
        case pp_ifdef:
        case pp_ifndef:
        {
            metac_token_t name = {0};
            if (*cursor < count && tokens[*cursor].TokenType == tok_identifier)
            {
                name = tokens[(*cursor)++];
            }
            else
            {
                errored = true;
            }

            bool parentActive = CondsActive();
            bool truth = parentActive && (LookupMacro(name) != 0);
            if (directive == pp_ifndef)
                truth = parentActive && !LookupMacro(name);

            assert(condCount < MAX_CONDS);
            cond_entry_t* entry = &condStack[condCount++];
            entry->ParentActive = parentActive;
            entry->WasActive = truth;
            entry->BranchTaken = truth;
            entry->SawElse = false;
            break;
        }
        case pp_if:
        {
            // gather the raw tokens of the line
            toklist_t exprTokens = { tokens, count };
            uint32_t exprCursor = *cursor;
            int64_t value = EvalExpr(exprTokens.Ptr, exprTokens.Count, &exprCursor);

            bool parentActive = CondsActive();
            bool truth = parentActive && (value != 0);

            assert(condCount < MAX_CONDS);
            cond_entry_t* entry = &condStack[condCount++];
            entry->ParentActive = parentActive;
            entry->WasActive = truth;
            entry->BranchTaken = truth;
            entry->SawElse = false;
            *cursor = exprCursor;
            break;
        }
        case pp_elif:
        case pp_else:
        {
            if (condCount == 0)
            {
                fprintf(stderr, "#%s without #if\n", directive == pp_else ? "else" : "elif");
                errored = true;
                break;
            }
            cond_entry_t* entry = &condStack[condCount - 1];
            if (entry->SawElse)
            {
                // any branch after an #else is an error
                fprintf(stderr, "branch after #else\n");
                errored = true;
            }
            if (directive == pp_else)
            {
                entry->SawElse = true;
                entry->WasActive = entry->ParentActive && !entry->BranchTaken;
                entry->BranchTaken |= entry->WasActive;
            }
            else
            {
                uint32_t exprCursor = *cursor;
                int64_t value = EvalExpr(tokens, count, &exprCursor);
                bool truth = entry->ParentActive && !entry->BranchTaken && (value != 0);
                entry->WasActive = truth;
                entry->BranchTaken |= truth;
                *cursor = exprCursor;
            }
            break;
        }
        case pp_endif:
        {
            if (condCount == 0)
            {
                fprintf(stderr, "#endif without #if\n");
                errored = true;
                break;
            }
            condCount--;
            break;
        }
        default: assert(!"not a conditional directive"); break;
    }
}


// dispatches the directive by its name
static void HandleDirective(metac_token_t nameTok, toklist_t stream,
                            uint32_t* cursor, uint32_t* directiveLine)
{
    uint32_t nameLength = 0;
    const char* name = FindIdentifierChars(&nameTok, &nameLength);
    if (!name)
    {
        // keyword tokens ('#if', '#else') never enter the identifier
        // table; their spelling lives in the token enum name
        if (nameTok.TokenType > tok_kw_struct &&
            nameTok.TokenType < tok_kw___attribute__)
        {
            name = MetaCTokenEnum_toChars(nameTok.TokenType) + 7;
            nameLength = strlen(name);
        }
        else
        {
            fprintf(stderr, "unresolvable directive name\n");
            errored = true;
            return;
        }
    }

#define IS_DIR(NAME) \
    (nameLength == (sizeof(#NAME) - 1) && \
     !memcmp(name, #NAME, sizeof(#NAME) - 1))

    if (IS_DIR(define))
    {
        HandleDefine(stream.Ptr, stream.Count, cursor);
    }
    else if (IS_DIR(undef))
    {
        if (*cursor < stream.Count &&
            stream.Ptr[*cursor].TokenType == tok_identifier)
        {
            UndefineMacro(stream.Ptr[(*cursor)++]);
        }
    }
    else if (IS_DIR(ifdef))
    {
        HandleConditional(stream.Ptr, stream.Count, cursor, pp_ifdef);
    }
    else if (IS_DIR(ifndef))
    {
        HandleConditional(stream.Ptr, stream.Count, cursor, pp_ifndef);
    }
    else if (IS_DIR(if))
    {
        HandleConditional(stream.Ptr, stream.Count, cursor, pp_if);
    }
    else if (IS_DIR(elif))
    {
        HandleConditional(stream.Ptr, stream.Count, cursor, pp_elif);
    }
    else if (IS_DIR(else))
    {
        HandleConditional(stream.Ptr, stream.Count, cursor, pp_else);
    }
    else if (IS_DIR(endif))
    {
        HandleConditional(stream.Ptr, stream.Count, cursor, pp_endif);
    }
    else if (IS_DIR(include))
    {
        metac_token_t open = stream.Ptr[*cursor];
        if (open.TokenType == tok_lt)
        {
            (*cursor)++;
            HandleInclude(stream.Ptr, stream.Count, cursor, true);
        }
        else
        {
            HandleInclude(stream.Ptr, stream.Count, cursor, false);
        }
    }
    else if (IS_DIR(error))
    {
        char text[256];
        uint32_t pos = 0;
        SpellingFromTokens(stream.Ptr + *cursor, stream.Count - *cursor,
                           text, &pos);
        text[pos] = 0;
        fprintf(stderr, "#error: %s\n", text);
        errored = true;
    }
    else if (IS_DIR(warning) || IS_DIR(pragma) || IS_DIR(line))
    {
        // skip the line
    }
    else
    {
        char text[64];
        uint32_t pos = 0;
        SpellingFromTokens(&nameTok, 1, text, &pos);
        text[pos] = 0;
        fprintf(stderr, "unknown directive '%s'\n", text);
        errored = true;
    }

    // the directive's line shifts to the line of its last consumed token
    // (define bodies use the line their last body token ends on)
    if (*cursor > 0 && *cursor <= stream.Count)
    {
        const metac_token_t* lastTok = &stream.Ptr[*cursor - 1];
        uint32_t lastLine = TokenStartLine(lastTok);
        if (lastLine > *directiveLine)
            *directiveLine = lastLine;
    }

#undef IS_DIR
}


// ---------------------------------------------------------------------------
// expansion engine (core)
// ---------------------------------------------------------------------------

// ---------------------------------------------------------------------------
// emit sink stack: depth 0 is the final output; deeper levels are scratch
// buffers used for pre-expanding macro arguments
// ---------------------------------------------------------------------------

#define MAX_DEPTH 32
#define LEVEL_TOKEN_CAP 2048
static metac_token_t levelBufs[MAX_DEPTH][LEVEL_TOKEN_CAP];
static uint32_t levelCounts[MAX_DEPTH];
static uint32_t currentLevel = 0;

static void PushScratch(void)
{
    assert(currentLevel + 1 < MAX_DEPTH);
    currentLevel++;
    levelCounts[currentLevel] = 0;
}

static uint32_t PopScratch(void)
{
    uint32_t count = levelCounts[currentLevel];
    currentLevel--;
    return count;
}

// appends a token to the current sink
static void SinkPush(metac_token_t tok)
{
    if (currentLevel == 0)
    {
        assert(outCount < OUT_CAP);
        outTokens[outCount++] = tok;
    }
    else
    {
        assert(levelCounts[currentLevel] < LEVEL_TOKEN_CAP);
        levelBufs[currentLevel][levelCounts[currentLevel]++] = tok;
    }
}

// merges the spelling of tok into the last token of the current sink
static void SinkMerge(metac_token_t tok)
{
    if (currentLevel == 0)
    {
        assert(outCount > 0);
        char buf[SPELLING_CAP];
        uint32_t pos = 0;
        SpellingFromTokens(&outTokens[outCount - 1], 1, buf, &pos);
        SpellingFromTokens(&tok, 1, buf, &pos);
        outTokens[outCount - 1] = MakeSpellingToken(buf, pos, false);
    }
    else
    {
        metac_token_t* levelBuf = levelBufs[currentLevel];
        assert(levelCounts[currentLevel] > 0);
        char buf[SPELLING_CAP];
        uint32_t pos = 0;
        SpellingFromTokens(&levelBuf[levelCounts[currentLevel] - 1], 1, buf, &pos);
        SpellingFromTokens(&tok, 1, buf, &pos);
        levelBuf[levelCounts[currentLevel] - 1] =
            MakeSpellingToken(buf, pos, false);
    }
}

// emits one token honoring a pending paste merge
static void EmitToken(metac_token_t tok, bool* pastePending)
{
    if (*pastePending)
    {
        SinkMerge(tok);
        *pastePending = false;
    }
    else
    {
        SinkPush(tok);
    }
}

// stringizes a parameter (the spellings are joined with single spaces;
// gcc preserves the call-site spacing, the tests only use single tokens)
static void EmitStringified(const toklist_t* param, bool* pastePending)
{
    char buf[SPELLING_CAP];
    uint32_t pos = 0;
    SpellingFromTokens(param->Ptr, param->Count, buf, &pos);

    metac_token_t stringTok =
        MakeSpellingToken(buf, pos, true);
    EmitToken(stringTok, pastePending);
}


// collects the tokens of a function-like macro invocation's arguments.
// stream.Ptr[*cursor] must point at the invoking '(' (it is consumed);
// macro parameter markers appearing in the stream get flattened against
// the enclosing invocation's arguments.
//
// returns false when the invocation is malformed; on success the closing
// ')' was consumed and the flattened cases were placed in args[]

static void AppendTok(metac_token_t* buf, uint32_t cap, uint32_t* used,
                      metac_token_t tok)
{
    assert(*used < cap);
    buf[(*used)++] = tok;
}

static bool CollectRawArgs(toklist_t stream, uint32_t* cursor,
                           const toklist_t* outerArgs, uint32_t outerArgCount,
                           metac_token_t* tokenBuf, uint32_t tokenCap,
                           toklist_t* args, uint32_t argCap,
                           uint32_t* argCountOut)
{
    if (*cursor >= stream.Count ||
        stream.Ptr[*cursor].TokenType != tok_lParen)
    {
        return false;
    }
    (*cursor)++;

    uint32_t used = 0;
    uint32_t argCount = 0;
    assert(argCap > 0);
    args[0].Ptr = tokenBuf;
    args[0].Count = 0;

#define NEXT_ARG() do { \
    assert(argCount + 1 < argCap); \
    args[argCount + 1].Ptr = tokenBuf + used; \
    args[argCount + 1].Count = 0; \
    argCount++; \
} while(0)

    uint32_t depth = 1;
    while(*cursor < stream.Count)
    {
        metac_token_t tok = stream.Ptr[*cursor];

        switch(tok.TokenType)
        {
            case tok_lParen:
            {
                depth++;
                args[argCount].Count++;
                AppendTok(tokenBuf, tokenCap, &used, tok);
                (*cursor)++;
            } break;
            case tok_rParen:
            {
                depth--;
                (*cursor)++;
                if (depth == 0)
                    goto Ldone;
                args[argCount].Count++;
                AppendTok(tokenBuf, tokenCap, &used, tok);
            } break;
            case tok_comma:
            {
                if (depth == 1)
                {
                    NEXT_ARG();
                    (*cursor)++;
                }
                else
                {
                    args[argCount].Count++;
                    AppendTok(tokenBuf, tokenCap, &used, tok);
                    (*cursor)++;
                }
            } break;
            case tok_newline:
            {
                if (depth == 1)
                {
                    fprintf(stderr, "unterminated macro invocation\n");
                    errored = true;
                    goto Lfail;
                }
                // inside nested parens: fall through
                (*cursor)++;
                args[argCount].Count++;
                AppendTok(tokenBuf, tokenCap, &used, tok);
            } break;
            case tok_macro_parameter:
            {
                uint32_t paramIdx = tok.MacroParameterIndex;
                if (!outerArgs || paramIdx >= outerArgCount)
                {
                    fprintf(stderr,
                        "macro parameter reference without matching argument\n");
                    errored = true;
                    goto Lfail;
                }
                toklist_t splice = outerArgs[paramIdx];
                for(uint32_t i = 0; i < splice.Count; i++)
                {
                    args[argCount].Count++;
                    AppendTok(tokenBuf, tokenCap, &used, splice.Ptr[i]);
                }
                (*cursor)++;
            } break;
            default:
            {
                args[argCount].Count++;
                AppendTok(tokenBuf, tokenCap, &used, tok);
                (*cursor)++;
            } break;
        }
    }

    fprintf(stderr, "unterminated macro invocation (end of stream)\n");
    errored = true;

Lfail:
    return false;

Ldone:
    *argCountOut = argCount + 1;
    return true;
}


// ---------------------------------------------------------------------------
// list emission with full expansion semantics
// ---------------------------------------------------------------------------

static void ExpandInvocation(harness_macro_t* macro, metac_token_t nameTok,
                             toklist_t stream, uint32_t* callCursor,
                             const toklist_t* outerArgs, uint32_t outerArgCount,
                             bool* pastePending);

static const toklist_t nullList = { 0, 0 };

static void EmitList(toklist_t list, const toklist_t* args, uint32_t argCount,
                     bool variadic, bool* pastePending)
{
    if (currentLevel + 1 >= MAX_DEPTH)
    {
        fprintf(stderr, "macro expansion too deep\n");
        errored = true;
        return;
    }

    uint32_t idx = 0;
    while(idx < list.Count)
    {
        metac_token_t tok = list.Ptr[idx];

        switch(tok.TokenType)
        {
            case tok_newline:
            case tok_comment_single:
            case tok_comment_multi:
            {
                idx++;
            } break;
            case tok_macro_parameter:
            {
                uint32_t paramIdx = tok.MacroParameterIndex;
                if (paramIdx >= argCount)
                {
                    fprintf(stderr, "macro parameter out of range (%u >= %u)\n",
                        paramIdx, argCount);
                    errored = true;
                    idx++;
                    break;
                }
                toklist_t paramTokens = args[paramIdx];
                idx++;
                EmitList(paramTokens, &nullList, 0, false, pastePending);
            } break;
            case tok_hash:
            {
                // stringizer: must be followed by a macro parameter
                if (idx + 1 >= list.Count ||
                    list.Ptr[idx + 1].TokenType != tok_macro_parameter)
                {
                    fprintf(stderr, "# not followed by macro parameter\n");
                    errored = true;
                    idx++;
                    break;
                }
                uint32_t paramIdx = list.Ptr[idx + 1].MacroParameterIndex;
                if (paramIdx >= argCount)
                {
                    errored = true;
                    idx += 2;
                    break;
                }
                toklist_t paramTokens = args[paramIdx];
                idx += 2;
                EmitStringified(&paramTokens, pastePending);
            } break;
            case tok_hashhash:
            {
                *pastePending = true;
                idx++;
            } break;
            case tok_identifier:
            {
                if (variadic && tok.IdentifierKey == va_args_key)
                {
                    // __VA_ARGS__ folds into the merged variadic argument
                    if (argCount == 0)
                    {
                        fprintf(stderr, "__VA_ARGS__ without variadic args\n");
                        errored = true;
                        idx++;
                        break;
                    }
                    toklist_t vaArgs = args[argCount - 1];
                    idx++;
                    EmitList(vaArgs, &nullList, 0, false, pastePending);
                    break;
                }

                harness_macro_t* macro = LookupMacro(tok);
                if (!macro)
                {
                    EmitToken(tok, pastePending);
                    idx++;
                    break;
                }

                bool functionLike = macro->ParamCount > 0 || macro->IsVariadic;
                if (!functionLike)
                {
                    toklist_t body = { macro->Body, macro->BodyCount };
                    idx++;
                    EmitList(body, &nullList, 0, false, pastePending);
                    break;
                }

                // function-like: needs an adjacent '(' to expand
                bool invocable =
                    idx + 1 < list.Count &&
                    list.Ptr[idx + 1].TokenType == tok_lParen &&
                    tok.Position + LENGTH_FROM_IDENTIFIER_KEY(tok.IdentifierKey)
                        == list.Ptr[idx + 1].Position;
                if (!invocable)
                {
                    EmitToken(tok, pastePending);
                    idx++;
                    break;
                }

                toklist_t callStream = { list.Ptr, list.Count };
                uint32_t callCursor = idx + 1;
                ExpandInvocation(macro, tok, callStream, &callCursor,
                                 args, argCount, pastePending);
                idx = callCursor;
            } break;
            default:
            {
                EmitToken(tok, pastePending);
                idx++;
            } break;
        }
    }
}

// expands a function-like macro invocation; *callCursor must point at
// the invoking '(' and gets advanced past the closing ')'
static void ExpandInvocation(harness_macro_t* macro, metac_token_t nameTok,
                             toklist_t stream, uint32_t* callCursor,
                             const toklist_t* outerArgs, uint32_t outerArgCount,
                             bool* pastePending)
{
    (void) nameTok;

    metac_token_t rawBuf[1024];
    toklist_t rawArgs[MAX_PARAMS + MAX_BODY];
    uint32_t rawArgCount = 0;

    if (!CollectRawArgs(stream, callCursor, outerArgs, outerArgCount,
                        rawBuf, ARRAY_SIZE(rawBuf),
                        rawArgs, ARRAY_SIZE(rawArgs), &rawArgCount))
    {
        errored = true;
        return;
    }

    if (rawArgCount < macro->ParamCount ||
        (!macro->IsVariadic && rawArgCount != macro->ParamCount))
    {
        fprintf(stderr,
            "macro takes %u arguments, invocation provides %u\n",
            macro->ParamCount, rawArgCount);
        errored = true;
        return;
    }

    // pre-expand the fixed arguments (variadic extras fold into a single
    // merged argument, separated by commas)
    metac_token_t expBuf[2048];
    toklist_t expArgs[MAX_PARAMS + MAX_BODY];
    uint32_t extraArgCount =
        macro->IsVariadic && rawArgCount > macro->ParamCount
            ? rawArgCount - macro->ParamCount : 0;
    uint32_t expArgCount = macro->ParamCount + (macro->IsVariadic ? 1 : 0);
    uint32_t expBufUsed = 0;

    for(uint32_t k = 0; k < macro->ParamCount; k++)
    {
        PushScratch();
        bool scratchPaste = false;
        if (k < rawArgCount)
        {
            EmitList(rawArgs[k], &nullList, 0, false, &scratchPaste);
        }
        uint32_t scratchCount = PopScratch();
        assert(scratchCount <= LEVEL_TOKEN_CAP);

        metac_token_t* levelBuf = levelBufs[currentLevel + 1];
        if (scratchCount > 0)
        {
            memcpy(expBuf + expBufUsed, levelBuf,
                scratchCount * sizeof(metac_token_t));
        }
        expArgs[k].Ptr = expBuf + expBufUsed;
        expArgs[k].Count = scratchCount;
        expBufUsed += scratchCount;
    }

    if (macro->IsVariadic)
    {
        PushScratch();
        bool scratchPaste = false;
        for(uint32_t k = 0; k < extraArgCount; k++)
        {
            if (k > 0)
            {
                metac_token_t commaTok = {0};
                commaTok.TokenType = tok_comma;
                EmitToken(commaTok, &scratchPaste);
            }
            uint32_t extraIdx = macro->ParamCount + k;
            if (extraIdx < rawArgCount)
            {
                EmitList(rawArgs[extraIdx], &nullList, 0, false, &scratchPaste);
            }
        }
        uint32_t scratchCount = PopScratch();
        assert(scratchCount <= LEVEL_TOKEN_CAP);

        metac_token_t* levelBuf = levelBufs[currentLevel + 1];
        if (scratchCount > 0)
        {
            memcpy(expBuf + expBufUsed, levelBuf,
                scratchCount * sizeof(metac_token_t));
        }
        expArgs[macro->ParamCount].Ptr = expBuf + expBufUsed;
        expArgs[macro->ParamCount].Count = scratchCount;
        expBufUsed += scratchCount;
    }

    toklist_t body = { macro->Body, macro->BodyCount };
    EmitList(body, expArgs, expArgCount, macro->IsVariadic, pastePending);
}

static void RunCtx(void)
{
    while(ctxCount > 0)
    {
        ctx_t* ctx = &ctxStack[ctxCount - 1];

        if (ctx->Cursor >= ctx->End)
        {
            ctxCount--;
            continue;
        }

        metac_token_t tok = globalLexer->Tokens[ctx->Cursor];

        // directives are recognized on lines the walker has not seen yet;
        // plain newlines carry no tokens (metac only emits them inside
        // define bodies), so line tracking runs over the source locations
        if (tok.TokenType == tok_hash &&
            TokenStartLine(&tok) > ctx->CurrentLine)
        {
            ctx->Cursor++;

            toklist_t stream = { globalLexer->Tokens, ctx->End };
            uint32_t cursor = ctx->Cursor;
            uint32_t directiveLine = TokenStartLine(&tok);

            if (cursor < stream.Count &&
                IsDirectiveNameToken(&stream.Ptr[cursor]))
            {
                metac_token_t nameTok = stream.Ptr[cursor];
                cursor++;
                HandleDirective(nameTok, stream, &cursor, &directiveLine);
            }
            else
            {
                // null directive: skip the rest of the line
                while(cursor < stream.Count &&
                      TokenStartLine(&stream.Ptr[cursor]) <= directiveLine)
                    cursor++;
            }

            ctx->Cursor = cursor;

            // remember the directive's line so ordinary tokens on later
            // lines qualify as directive starts
            if (ctx->Cursor > 0 && ctx->Cursor <= ctx->End)
            {
                metac_token_t* lastTok = &globalLexer->Tokens[ctx->Cursor - 1];
                if (lastTok->TokenType != tok_newline &&
                    TokenStartLine(lastTok) > directiveLine)
                {
                    directiveLine = TokenStartLine(lastTok);
                }
            }
            ctx->CurrentLine = directiveLine;
            continue;
        }

        switch(tok.TokenType)
        {
            case tok_newline:
            case tok_comment_single:
            case tok_comment_multi:
            {
                ctx->Cursor++;
            } break;

            case tok_eof:
            {
                ctx->Cursor = ctx->End;
            } break;

            default:
            {
                uint32_t line = TokenStartLine(&tok);
                if (line > ctx->CurrentLine)
                    ctx->CurrentLine = line;

                if (!CondsActive())
                {
                    // inactive branch: skip the token
                    ctx->Cursor++;
                    break;
                }

                harness_macro_t* macro = LookupMacro(tok);
                if (macro && (macro->ParamCount > 0 || macro->IsVariadic))
                {
                    // function-like: only expands with an adjacent '('
                    bool invocable =
                        ctx->Cursor + 1 < ctx->End &&
                        globalLexer->Tokens[ctx->Cursor + 1].TokenType == tok_lParen &&
                        tok.Position +
                            LENGTH_FROM_IDENTIFIER_KEY(tok.IdentifierKey)
                            == globalLexer->Tokens[ctx->Cursor + 1].Position;
                    if (invocable)
                    {
                        toklist_t stream = { globalLexer->Tokens, ctx->End };
                        uint32_t callCursor = ctx->Cursor + 1;

                        bool paste = false;
                        ExpandInvocation(macro, tok, stream, &callCursor,
                                         &nullList, 0, &paste);
                        ctx->Cursor = callCursor;
                        break;
                    }
                }

                if (macro && macro->ParamCount == 0 && !macro->IsVariadic)
                {
                    // object-like macro: expand its body
                    toklist_t body = { macro->Body, macro->BodyCount };
                    bool paste = false;
                    EmitList(body, &nullList, 0, false, &paste);
                    ctx->Cursor++;
                    break;
                }

                // ordinary token: emit via a one-token list
                toklist_t oneTokenList = { &tok, 1 };
                bool paste = false;
                EmitList(oneTokenList, &nullList, 0, false, &paste);
                ctx->Cursor++;
            } break;
        }
    }
}


int main(int argc, char* argv[])
{
    if (argc < 2)
    {
        fprintf(stderr, "usage: %s <file.c>\n", argv[0]);
        return 1;
    }

    // the real stdout is needed for the final output; the lib prints debug
    // chatter through printf, so stdout gets redirected to /dev/null while
    // running, and the collected tokens are written through a dup
    int realOut = dup(1);
    if (realOut == -1)
    {
        fprintf(stderr, "cannot dup(1)\n");
        return 1;
    }
    if (!freopen("/dev/null", "w", stdout))
    {
        fprintf(stderr, "cannot redirect stdout\n");
        return 1;
    }

    metac_alloc_t topAlloc;
    metac_lexer_t lexer;
    metac_parser_t parser;
    metac_preprocessor_t preprocessor;

    Allocator_Init(&topAlloc, 0);
    MetaCLexer_Init(&lexer, &topAlloc);
    MetaCParser_InitFromLexer(&parser, &lexer, &topAlloc);
    MetaCPreProcessor_Init(&preprocessor, &lexer, &topAlloc, 0, 0);
    parser.Preprocessor = &preprocessor;

    globalLexer = &lexer;
    globalPreproc = &preprocessor;

    // lex the file into the shared token array
    char* text = ReadFileIntoMemory(argv[1]);
    if (!text)
    {
        return 1;
    }
    LexStringIntoLexer(&lexer, text, cast(int32_t) strlen(text));
    free(text);

    if (errored)
    {
        return 1;
    }

    assert(ctxCount == 0);
    ctxStack[0].Cursor = 0;
    ctxStack[0].End = lexer.TokenCount;
    ctxStack[0].CurrentLine = 0;
    ctxCount = 1;

    RunCtx();

    // print the preprocessed token stream through the real stdout
    FILE* out = fdopen(realOut, "w");
    if (!out)
    {
        fprintf(stderr, "cannot fdopen(1)\n");
        return 1;
    }
    for(uint32_t i = 0; i < outCount; i++)
    {
        PrintTokenText(out, &outTokens[i]);
    }
    fputc('\n', out);
    fclose(out);

    return errored ? 1 : 0;
}
