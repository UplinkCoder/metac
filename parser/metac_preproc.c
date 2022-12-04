#if !defined(NO_PREPROCESSOR)

#include "metac_preproc.h"

#ifndef NO_FIBERS
# include "../os/metac_task.h"
#else
# include <assert.h>
#endif

#include "metac_parser.h"
#include <string.h>
#include <stdlib.h>
#include "../libinterpret/bc_common.h"
#include "../libinterpret/backend_interface_funcs.h"
#include "../os/metac_atomic.h"

#include "metac_array.c"

/// Pushes the tokens to write instead of the macro on the alternative
/// token source stack
uint32_t MetaCPreProcessor_PushDefine(metac_preprocessor_t* self,
                                      metac_preprocessor_define_t* define,
                                      metac_token_t_array_array parameters)
{
    uint32_t result = 0;

    DEF_STACK_ARRAY(metac_token_t, tokens, 64);
#define va_args_key 0xbc18fc
    if (parameters.Count != define->ParameterCount &&
        ((define->ParameterCount >= parameters.Count) && !define->IsVariadic))
    {
        printf("Macro takes %u arguments but %u are given\n",
              define->ParameterCount, parameters.Count);
        goto Lret;
    }

    assert(parameters.Count == define->ParameterCount
        || (define->IsVariadic && define->ParameterCount >= parameters.Count));

    for(uint32_t tokIdx = 0;
        tokIdx < define->TokenCount;
        tokIdx++)
    {
        metac_token_t tok =
            self->DefineTable.TokenMemory[define->TokensOffset + tokIdx];

        // printf("Seeing tok: %s\n", MetaCTokenEnum_toChars(tok.TokenType));

        if (tok.TokenType == tok_macro_parameter)
        {
            metac_token_t_array parameter =
                parameters.Ptr[tok.MacroParameterIndex];

            for(uint32_t paramTokIdx = 0;
                paramTokIdx < parameter.Count;
                paramTokIdx++)
            {
                tok = parameter.Ptr[paramTokIdx];
                //printf("Adding tok: %s\n", MetaCTokenEnum_toChars(tok.TokenType));
                ADD_STACK_ARRAY(tokens, tok);
            }
        }
        else if (tok.TokenType == tok_identifier
              && tok.IdentifierKey == va_args_key)
        {
            metac_token_t_array va_args = (*(parameters.Ptr + parameters.Count - 1));
            // printf("saw __VA_ARGS__\n");
            for(uint32_t va_args_idx = 0;
                va_args_idx < va_args.Count;
                va_args_idx)
            {
                // printf("Adding tok: %s\n", MetaCTokenEnum_toChars(tok.TokenType));
                ADD_STACK_ARRAY(tokens, va_args.Ptr[va_args_idx]);
            }
            continue;
        }
        else
        {
            // printf("Adding tok: %s\n", MetaCTokenEnum_toChars(tok.TokenType));
            ADD_STACK_ARRAY(tokens, tok);
        }
    }
    // printf("expanded to %u tokens\n", tokens.Count);
    PERSIST_STACK_ARRAY(tokens);

Lret:
    self->DefineTokenStack[self->DefineTokenStackCount++] = tokens;
    printf("PushedDefine: [%u]\n", self->DefineTokenStackCount);
    return result;
}

metac_expr_t* MetaCPreProcessor_ResolveDefineToExp(metac_preprocessor_t* self,
                                                         metac_preprocessor_define_ptr_t definePtr,
                                                         metac_token_t_array_array parameters)
{
    assert(definePtr.v >= 4);
    metac_expr_t* result = 0;
    metac_alloc_t tmpDefineParserAlloc;
    metac_lexer_t DefineLexer = { 0 };
    metac_lexer_state_t LexerState = { 0 };
    STACK_ARENA_ARRAY(metac_location_t, tokenLocationStorage, 32, 0)
    metac_location_t_array tokenLocationArray;

    Allocator_Init(&tmpDefineParserAlloc, 0);

    metac_parser_t defineParser;
    MetaCParser_Init(&defineParser, &tmpDefineParserAlloc);
    defineParser.Preprocessor = self;
    metac_preprocessor_define_t* define = &self->DefineTable.DefineMemory[definePtr.v - 4];
    DEF_STACK_ARRAY(metac_token_t, tokens, 32);
#define va_args_key 0xbc18fc

    if ((parameters.Count != define->ParameterCount) &&
        ((define->ParameterCount >= parameters.Count) && !define->IsVariadic))
    {
        printf("Macro takes %u arguments but %u are given\n",
              define->ParameterCount, parameters.Count);
        result = AllocNewExpr(expr_signed_integer);
        result->ValueU64 = 0;
        goto Lret;
    }

    assert(parameters.Count == define->ParameterCount
        || (define->IsVariadic && define->ParameterCount >= parameters.Count));

    for(uint32_t tokIdx = 0;
        tokIdx < define->TokenCount;
        tokIdx++)
    {
        metac_token_t tok =
            self->DefineTable.TokenMemory[define->TokensOffset + tokIdx];
        if (tok.TokenType == tok_macro_parameter)
        {
            metac_token_t_array parameter =
                parameters.Ptr[tok.MacroParameterIndex];
            for(uint32_t paramTokIdx = 0;
                paramTokIdx < parameter.Count;
                paramTokIdx++)
            {

            }
        }
        else if (tok.TokenType == tok_identifier
              && tok.IdentifierKey == va_args_key)
        {
            metac_token_t_array va_args = (*(parameters.Ptr + parameters.Count - 1));
            printf("saw __VA_ARGS__\n");
            for(uint32_t va_args_idx = 0;
                va_args_idx < va_args.Count;
                va_args_idx)
            {
                ADD_STACK_ARRAY(tokens, (*(va_args.Ptr + va_args_idx)));
            }
            continue;
        }
        else
            ADD_STACK_ARRAY(tokens, tok);
    }

    tokenLocationArray.Locations = tokenLocationStorage;
    tokenLocationArray.LocationCapacity = 32;
    tokenLocationArray.LocationSize = tokens.Count;

//    self->Lexer->IdentifierTable = self->IdentifierTable;
    DefineLexer.IdentifierTable = self->IdentifierTable;
    DefineLexer.StringTable = self->StringTable;
    DefineLexer.TokenCount = tokens.Count;
    DefineLexer.TokenCapacity = tokens.Count;
    DefineLexer.Tokens = tokens.Ptr;
    DefineLexer.LocationStorage = tokenLocationArray;
    DefineLexer.Allocator = &tmpDefineParserAlloc;

    printf("expanded to %u tokens\n", tokens.Count);

    MetaCParser_InitFromLexer(&defineParser, &DefineLexer, &tmpDefineParserAlloc);

    result = MetaCParser_ParseExpr(&defineParser, expr_flags_none, 0);
Lret:
    return result;
}

void MetaCPreprocessor_DefineTable_Init(metac_define_table_t* self, metac_preprocessor_t* preproc)
{
    self->DefineMemorySize = 0;
    self->DefineMemoryCapacity = 256;
    self->DefineMemory = cast(metac_preprocessor_define_t*)
        calloc(sizeof(metac_preprocessor_define_t), self->DefineMemoryCapacity);
    self->LengthShift = IDENTIFIER_LENGTH_SHIFT;

    self->SlotCount_Log2 = 9;
    self->Slots = cast(metac_define_table_slot_t*)
        calloc(sizeof(metac_define_table_slot_t), 1 << self->SlotCount_Log2);

    self->TokenMemoryCapacity = 1024;
    self->TokenMemorySize = 0;
    self->TokenMemory = cast(metac_token_t*)
        calloc(sizeof(metac_token_t), self->TokenMemoryCapacity);

    self->Preproc = preproc;
}

metac_preprocessor_define_ptr_t IsDefineInTable(metac_define_table_t* table,
                                                uint32_t key,
                                                const char* idChars)
{
    metac_preprocessor_define_ptr_t result = {0};

    const uint32_t slotIndexMask = ((1 << table->SlotCount_Log2) - 1);
    const uint32_t initialSlotIndex = (key & slotIndexMask);
    assert(slotIndexMask);
    // if slotIndexMask is 0 most likely the table has not been initialized
    // TracyCPlot("TargetIndex", initialSlotIndex);
    for(
        uint32_t slotIndex = initialSlotIndex;
        (++slotIndex & slotIndexMask) != initialSlotIndex;
    )
    {
        metac_define_table_slot_t slot =
            table->Slots[(slotIndex - 1) & slotIndexMask];

        if (slot.HashKey == 0)
            goto Lret;
        if (slot.HashKey == key)
        {
            assert(slot.DefinePtr.v != 0);
            metac_preprocessor_define_t def = table->DefineMemory[slot.DefinePtr.v - 4];

            const char* defineIdChars =
                IdentifierPtrToCharPtr(&table->Preproc->DefineIdentifierTable, def.DefineName);

            bool matches =
                !memcmp(defineIdChars, idChars,
                        LENGTH_FROM_IDENTIFIER_KEY(key));
            if (matches)
            {
                result = slot.DefinePtr;
                goto Lret;
            }
        }
    }
    // when we end up here we wrapped around and the table does not contain a terminating 0
    assert(0);
Lret:
    return result;
}

static inline metac_token_t_array ResolveDefine()
{
    metac_token_t_array Result = {0};
    return Result;
}

static inline int32_t MetaCPreProcessor_EvalExp(metac_preprocessor_t* self,
                                                metac_expr_t* e,
                                                metac_parser_t* parser)
{

    int32_t result;
    int32_t e1;
    int32_t e2;

    metac_expr_kind_t op = e->Kind;

    if (IsBinaryExp(op))
    {
        e1 = MetaCPreProcessor_EvalExp(self, e->E1, parser);
        if (op == expr_oror)
        {
            if (e1)
                return true;
        } else if (op == expr_andand)
        {
            if (!e1)
                return false;
        }
        e2 = MetaCPreProcessor_EvalExp(self, e->E2, parser);
    }


    printf("op: %s\n", MetaCExprKind_toChars(op));
    metac_identifier_ptr_t definedIdPtr;
    switch(op)
    {
        default : {
            fprintf(stderr,
                "Evaluator doesn't know how to eval: %s\n",
                MetaCExprKind_toChars(e->Kind)
            );
            assert(0);
        } break;
        case expr_string:
        {
            // this should not happen, we should have made it into a pointer I think
            assert(0);
        }


        case expr_signed_integer:
        {
            result = cast(int32_t)(e->ValueU64);
        }
        break;

        case expr_neq:
        {
            result = (e1 != e2);
        } break;

        case expr_eq:
        {
            result = (e1 == e2);
        } break;

        case expr_add:
        {
            result = (e1 + e2);
        } break;
        case expr_sub:
        {
            result = (e1 - e2);
        } break;
        case expr_mul:
        {
            result = (e1 * e2);
        } break;
        case expr_div:
        {
            result = (e1 / e2);
        } break;
        case expr_rem:
        {
            result = (e1 % e2);
        } break;
        case expr_andand:
        {
            result = (e1 && e2);
        } break;
        case expr_and:
        {
            result = (e1 & e2);
        } break;
        case expr_oror:
        {
            result = (e1 || e2);
        } break;
        case expr_or:
        {
            result = (e1 | e2);
        } break;
        case expr_xor:
        {
            result = (e1 ^ e2);
        } break;
        case expr_lsh:
        {
           result = (e1 << e2);
        } break;
        case expr_rsh:
        {
            result = (e1 >> e2);
        } break;
        case expr_identifier:
        {
            const char* identifierChars =
                IdentifierPtrToCharPtr(&parser->IdentifierTable, e->IdentifierPtr);
            metac_preprocessor_define_ptr_t definePtr;
            definePtr =
                MetaCPreProcessor_GetDefine(self, e->IdentifierKey, identifierChars);
            metac_expr_t* resolved = 0;
            if (definePtr.v)
            {
                metac_token_t_array_array emptyArray = {};
                resolved = MetaCPreProcessor_ResolveDefineToExp(self, definePtr, emptyArray);
                result = resolved ? MetaCPreProcessor_EvalExp(self, resolved, parser) : 0;
            }
            else
            {
                result = 0;
            }
        }
        break;
        case expr_variable:
        {
            // this should not happen
            assert(0);
        } break;

        case expr_paren:
        {
            result = MetaCPreProcessor_EvalExp(self, e->E1, parser);
        } break;
        case expr_compl:
        {
            result = ~MetaCPreProcessor_EvalExp(self, e->E1, parser);
        } break;
        case expr_not:
        {
            result = !MetaCPreProcessor_EvalExp(self, e->E1, parser);
        } break;
        case expr_umin:
        {
            result = -MetaCPreProcessor_EvalExp(self, e->E1, parser);
        } break;
        case expr_post_increment:
        {
            assert(0); // I don't think this can happen
        } break;

        case expr_call:
        {
            if (e->E1->Kind == expr_identifier)
            {
                if (e->E1->IdentifierKey == defined_key)
                {
                    expr_argument_t* args = (expr_argument_t*)e->E2;
                    metac_expr_t* e2 = args->Expr;
                    // this if makes sure there is only one "argument" to defiend()
                    if (args->Next != emptyPointer || e2->Kind != expr_identifier)
                    {
                        printf("single Identifier expected after defined(\n");
                        result = 0;
                        break;
                    }

                    LhandleDefined:
                    {
                        definedIdPtr = e2->IdentifierPtr;
                        const char* definedChars = IdentifierPtrToCharPtr(&parser->IdentifierTable, definedIdPtr);
                        uint32_t length = strlen(definedChars);
                        uint32_t definedHash = crc32c(~0, definedChars, length);
                        uint32_t key = IDENTIFIER_KEY(definedHash, length);
                        //printf("defined(%s)\n", definedChars);
                        result = IsDefineInTable(&self->DefineTable, key, definedChars).v != 0;
                    }
                }
            }
            else
            {
            }
        } break;
    }

    return result;
}

void MetaCPreProcessor_Init(metac_preprocessor_t *self, metac_lexer_t* lexer,
                            metac_alloc_t* alloc,
                            metac_file_storage_t* fs, const char* filepath)
{
    self->FileStorage = fs;
    if (filepath)
        self->File = MetaCFileStorage_LoadFile(fs, filepath);
    // self->Lexer = lexer;
    MetaCPreprocessor_DefineTable_Init(&self->DefineTable, self);
    IdentifierTable_Init(&self->DefineIdentifierTable, IDENTIFIER_LENGTH_SHIFT, 7, alloc);

    IdentifierTable_Init(&self->IdentifierTable, IDENTIFIER_LENGTH_SHIFT, 8, alloc);
    IdentifierTable_Init(&self->StringTable, STRING_LENGTH_SHIFT, 7, alloc);

    self->DefineTokenStackCount = 0;
    self->Parent = 0;

    self->TokenMemoryCapacity = 256;
    self->TokenMemorySize = 0;
    self->TokenMemory = Allocator_Calloc(alloc, metac_token_t, self->TokenMemoryCapacity);
}

metac_preprocessor_define_ptr_t
MetaCPreprocessor_RegisterDefine(metac_preprocessor_t* self,
                                 uint32_t key,
                                 metac_preprocessor_define_t define,
                                 metac_token_t_array tokens)
{
    metac_define_table_t* table = &self->DefineTable;

    metac_preprocessor_define_ptr_t result = {0};

    const uint32_t slotIndexMask = ((1 << table->SlotCount_Log2) - 1);
    const uint32_t initialSlotIndex = (key & slotIndexMask);
    assert(slotIndexMask);
    // if slotIndexMask is 0 most likely the table has not been initialized
    // TracyCPlot("TargetIndex", initialSlotIndex);
    for(
        uint32_t slotIndex = initialSlotIndex;
        (++slotIndex & slotIndexMask) != initialSlotIndex;
    )
    {
        metac_define_table_slot_t* slot =
            &table->Slots[(slotIndex - 1) & slotIndexMask];

        if (slot->HashKey == 0)
        {
            slot->HashKey = key;
            assert(table->DefineMemoryCapacity > table->DefineMemorySize);

            assert(table->TokenMemorySize + tokens.Count < table->TokenMemoryCapacity);

            uint32_t TokenArraryOffset = POST_ADD(table->TokenMemorySize, tokens.Count);
            memcpy(cast(uint8_t*)table->TokenMemory + TokenArraryOffset,
                   cast(void*)tokens.Ptr,
                   tokens.Count * sizeof(*tokens.Ptr));
            define.TokensOffset = TokenArraryOffset;

            uint32_t defineIdx = INC(table->DefineMemorySize);
            result.v = slot->DefinePtr.v = defineIdx + 4;
            table->DefineMemory[defineIdx] = define;
            break;
        }
        else if (slot->HashKey == key)
        {
            if (define.DefineName.v == table->DefineMemory[slot->DefinePtr.v - 4].DefineName.v)
            {
                printf("Duplicate define\n");
                break;
            }
            continue;
        }
    }

    return result;
}

void MetaCPreProcessor_Include(metac_preprocessor_t *self, metac_parser_t* parser)
{
    metac_token_t* quoteOrLt = MetaCParser_PeekToken(parser, 1);
    metac_token_t tokens[32];
    const char *filename;
    char filenameBuffer[4096];
    static const metac_filesystem_t nullFS  = {0};
    const metac_filesystem_t fs = self->FileStorage && self->FileStorage->FS ?
        *self->FileStorage->FS : nullFS;

    if (!quoteOrLt)
    {
        assert(!"Token expected after #include");
    }

    if (quoteOrLt->TokenType == tok_string)
    {
        filename =
            IdentifierPtrToCharPtr(&parser->Lexer->StringTable, quoteOrLt->StringPtr);
        MetaCParser_Match(parser, tok_string);
    }
    else if (quoteOrLt->TokenType == tok_lt)
    {
        uint32_t tokenCount = 0;
        MetaCParser_Match(parser, tok_lt);
        metac_token_t* peek = 0;
        uint32_t p = 0;

        for (;;)
        {
            peek = MetaCParser_PeekToken(parser, 1);
            if (peek->TokenType == tok_gt)
            {
                MetaCParser_Match(parser, tok_gt);
                break;
            }
            tokens[tokenCount++] = *MetaCParser_Match(parser, peek->TokenType);
        } while (peek && peek->TokenType != tok_gt);

        for(uint32_t i = 0; i < tokenCount; i++)
        {
            metac_token_t* tokP = tokens + i;
            switch(tokP->TokenType)
            {
                case tok_identifier:
                {
                    const char* idString =
                        IdentifierPtrToCharPtr(&parser->Lexer->IdentifierTable, tokP->IdentifierPtr);
                    uint32_t len = tokP->IdentifierKey >> IDENTIFIER_LENGTH_SHIFT;
                    memcpy(filenameBuffer + p, idString, len);
                    p += len;
                } break;
                case tok_dotdot:
                {
                    filenameBuffer[p++] = '.';
                }
                case tok_dot:
                {
                    filenameBuffer[p++] = '.';
                } break;
                case tok_div:
                {
                    filenameBuffer[p++] = '/';
                } break;
            }
        }

        filenameBuffer[p] = '\0';
        filename = filenameBuffer;
    }
    else if (quoteOrLt->TokenType == tok_identifier)
    {
        //TODO do macro stuff here
        goto Lerror;
    }
    else
    {
Lerror:
        assert(!"< , \" or identifier expected after #include");
    }
    // MetaCParser_PushFile(self, )

    if (fs.functions)
    {
        metac_filehandle_t fhandle = fs.functions->Open(fs.ctx, 0, filename);
        metac_buffer_t fileBuffer =
            fs.functions->ReadEntireFileAndZeroTerminate(fs.ctx, fhandle);
        printf("Loaded %u bytes\n", fileBuffer.Length);
    }
    else
    {
        fprintf(stderr, "No filesystem\n");
    }
}

metac_preprocessor_define_ptr_t
MetaCPreProcessor_ParseDefine(metac_preprocessor_t *self, metac_parser_t* parser)
{
    assert(self);
    metac_token_t* defineName = MetaCParser_Match(parser, tok_identifier);
    bool isMacro = MetaCParser_PeekMatch(parser, tok_lParen, 1);

    bool isVariadic = false;
    /// macro contains ##
    bool hasPaste = false;

    DEF_STACK_ARRAY(metac_identifier_ptr_t, macroParameter, 16);

    DEF_STACK_ARRAY(metac_token_t, defineBodyTokens, 64);

    if (isMacro)
    {
        MetaCParser_Match(parser, tok_lParen);
        while(MetaCParser_PeekMatch(parser, tok_identifier, 1))
        {

            ADD_STACK_ARRAY(macroParameter,
                MetaCParser_Match(parser, tok_identifier)->IdentifierPtr);

            if (MetaCParser_PeekMatch(parser, tok_comma, 1))
            {
                MetaCParser_Match(parser, tok_comma);
            }
        }
        if (MetaCParser_PeekMatch(parser, tok_dotdotdot, 1))
        {
            isVariadic |= true;
            MetaCParser_Match(parser, tok_dotdotdot);
        }
        MetaCParser_Match(parser, tok_rParen);

    }

    uint32_t definedNameLength = LENGTH_FROM_IDENTIFIER_KEY(defineName->IdentifierKey);

    const char* defineNameChars =
        IdentifierPtrToCharPtr(&parser->Lexer->IdentifierTable,
                               defineName->IdentifierPtr);

    printf("define %s with %u parameters\n", defineNameChars,
                                             macroParameter.Count);
    MetaCPrinter_Reset(&parser->DebugPrinter);

    uint32_t hash = crc32c(~0, defineNameChars, definedNameLength);
    uint32_t defineKey = IDENTIFIER_KEY(hash, definedNameLength);
    uint32_t lnx = LENGTH_FROM_IDENTIFIER_KEY(defineKey);

    metac_identifier_ptr_t defineIdPtr =
        GetOrAddIdentifier(&self->DefineIdentifierTable, defineKey, defineNameChars);

    metac_token_t* currentToken;
    uint32_t peek1 = 1;

    for(;;)
    {
        const char* s = 0;

        currentToken = MetaCParser_PeekToken(parser, peek1++);
        if (!currentToken || currentToken->TokenType == tok_newline)
            break;

        if (currentToken->TokenType == tok_identifier)
        {
            for(uint32_t i = 0; i < macroParameter.Count; i++)
            {
                if (currentToken->IdentifierPtr.v == macroParameter.Ptr[i].v)
                {
                    metac_token_t tok = *currentToken;
                    tok.TokenType = tok_macro_parameter;
                    tok.MacroParameterIndex = i;
                    ADD_STACK_ARRAY(defineBodyTokens, tok);
                    goto Lcontinue;
                }
            }
            goto LaddToken;
            Lcontinue: continue;
        }
        else if (currentToken->TokenType == tok_string)
        {
            const char* stringChars = IdentifierPtrToCharPtr(&parser->Lexer->StringTable,
                                                             currentToken->StringPtr);
            uint32_t length = LENGTH_FROM_STRING_KEY(currentToken->StringKey);

            GetOrAddIdentifier(&self->StringTable, STRING_KEY(hash, length), stringChars);
            goto LaddToken;
        }
        else
        {
            if (currentToken->TokenType == tok_hashhash)
                hasPaste |= 1;
        LaddToken:
            ADD_STACK_ARRAY(defineBodyTokens, *currentToken);
        }
        MetaCPrinter_Reset(&parser->DebugPrinter);
    }

    if (currentToken && currentToken->TokenType == tok_newline)
        MetaCParser_Advance(parser);

    metac_preprocessor_define_t define;
    define.loc = LocationFromToken(parser, defineName);
    define.DefineName = defineIdPtr;
    define.ParameterCount = macroParameter.Count;
    define.IsVariadic = isVariadic;
    define.HasPaste = hasPaste;
    define.TokenCount = defineBodyTokens.Count;

    metac_preprocessor_define_ptr_t result =
        MetaCPreprocessor_RegisterDefine(self, defineKey, define, defineBodyTokens);

    return result;
}


metac_preprocessor_define_ptr_t MetaCPreProcessor_GetDefine(metac_preprocessor_t* self,
                                                            uint32_t identifierKey, const char* identifier)
{
    metac_preprocessor_define_ptr_t result = {0};

    if (self->Parent)
    {
         result = MetaCPreProcessor_GetDefine(self->Parent, identifierKey, identifier);
    }

    if(!result.v)
    {
        result = IsDefineInTable(&self->DefineTable, identifierKey, identifier);
    }

    return result;
}

void MetaCPreProcessor_FastIncludeScan(metac_preprocessor_t* self)
{
    metac_buffer_t fileBuffer
        = MetaCFileStorage_GetEntireFileBuffer(self->FileStorage, self->File);

    // we know that the fileBuffer will always be zero terminated
    // which is why we can use strstr without issues

    uint32_t HashOffsets[512];
    uint32_t HashOffsetsCount = 0;
    uint32_t HashOffsetsCapacity = ARRAY_SIZE(HashOffsets);

    {
        uint32_t lastNewlinePos = 0;
        uint32_t scanPos = 0;
        uint32_t scanLeft = fileBuffer.Length;

        for(;;) {
            const char* result =
                cast(const char*) memchr(fileBuffer.Data + lastNewlinePos, '#', scanLeft);
            if (result != 0)
            {
/*
                const char* nextNewline =
                   cast(const char*) (fileBuffer.Data + lastNewlinePos, '#', scanLeft);
*/
                uint32_t hashOffset = cast(uint32_t)(result - fileBuffer.Data);
                HashOffsets[HashOffsetsCount++] = hashOffset;
                uint32_t advance = (hashOffset - lastNewlinePos) + 1;
                scanPos += advance;
                scanLeft -= advance;
                printf("HashOffset: %u\n", hashOffset);
            }
            else
            {
                break;
            }
        }
    }
}

#if 0
BCValue MetaCPreProcessor_Defined(void* ctx, uint32_t nVals, BCValue* vals)
{

}
#endif

uint32_t MetaCPreProcessor_Eval(metac_preprocessor_t* self, struct metac_parser_t* parser)
{
    uint32_t result = 0;
    // parse_expr_state_t state;
    for(;;)
    {
        metac_token_t* tok =
            MetaCParser_PeekToken(parser, 1);
        if (!tok || tok->TokenType == tok_eof)
            return result;

        metac_expr_t* exp = MetaCParser_ParseExpr(parser, expr_flags_pp, 0);
        MetaCPrinter_Reset(&parser->DebugPrinter);
        const char* expr_string = MetaCPrinter_PrintExpr(&parser->DebugPrinter, exp);
        printf("#eval '%s'\n", expr_string);

        return MetaCPreProcessor_EvalExp(self, exp, parser);
/*
        switch (tok->TokenType)
        {
#define CASE_MACRO(BIN_OP)
            FOREACH_BINARY_EXP(CASE_MACRO)
            case tok_equals_equals:
            {
                MetaCParser_Match(parser, tok_equals_equals);
            }
            case tok_identifier: switch(tok->IdentifierKey)
            {
                case defined_key:
                {
                    //TODO compare the identifier string to be defined for real
                    metac_token_t tokDefined = *MetaCParser_Match(parser, tok_identifier);
                    bool hasParen = MetaCParser_PeekMatch(parser, tok_lParen, 1);
                    if (hasParen)
                        MetaCParser_Match(parser, tok_lParen);
                    metac_token_t* idTok = MetaCParser_Match(parser, tok_identifier);
//*
                    if (!MetaCParser_PeekMatch(self, tok_identifier, 0))
                        ParseError(parser->LastLocation, "{Preproc} Expected identifier");
*//*
                    const char *identifier = IdentifierPtrToCharPtr(&parser->Lexer->IdentifierTable,
                                                                   idTok->IdentifierPtr);
                    printf("defiend(%s) = %u\n", identifier,
                           MetaCPreProcessor_IsDefine(self, idTok->IdentifierKey, identifier));
                    //if ()
                }
            } break;
            case tok_uint:
            {
                MetaCParser_Match(parser, tok_uint);
                printf("NUMBER(%u)\n", tok->ValueU32);
            }
        }
    */
     }

}
#endif // !defined(NO_PREPROC)
