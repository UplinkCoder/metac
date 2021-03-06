#if !defined(NO_PREPROC)

#include "metac_preproc.h"
#include "metac_task.h"
#include "metac_parser.h"
#include <string.h>
#include "libinterpret/bc_common.h"
#include "libinterpret/backend_interface_funcs.h"

metac_expression_t* MetaCPreProcessor_ResolveDefineToExp(metac_preprocessor_t* self,
                                                         metac_identifier_ptr_t definedIdPtr)
{
    assert(definedIdPtr.v);
    metac_expression_t* result = 0;

    metac_parser_t defineParser;
    MetaCParser_Init(&defineParser);
    defineParser.Preprocessor = self;

}

static inline int32_t MetaCPreProcessor_EvalExp(metac_preprocessor_t* self,
                                                metac_expression_t* e,
                                                metac_parser_t* parser)
{

    int32_t result;
    int32_t e1;
    int32_t e2;

    metac_expression_kind_t op = e->Kind;

    if (IsBinaryExp(op))
    {
        e1 = MetaCPreProcessor_EvalExp(self, e->E1, parser);
        if (op == exp_oror)
        {
            if (e1)
                return true;
        } else if (op == exp_andand)
        {
            if (!e1)
                return false;
        }
        e2 = MetaCPreProcessor_EvalExp(self, e->E2, parser);
    }


    printf("op: %s\n", MetaCExpressionKind_toChars(op));
    metac_identifier_ptr_t definedIdPtr;
    switch(op)
    {
        default : {
            fprintf(stderr,
                "Evaluator doesn't know how to eval: %s\n",
                MetaCExpressionKind_toChars(e->Kind)
            );
            assert(0);
        } break;
        case exp_string:
        {
            // this should not happen, we should have made it into a pointer I think
            assert(0);
        }


        case exp_signed_integer:
        {
            result = cast(int32_t)(e->ValueU64);
        }
        break;

        case exp_neq:
        {
            result = (e1 != e2);
        } break;

        case exp_eq:
        {
            result = (e1 == e2);
        } break;

        case exp_add:
        {
            result = (e1 + e2);
        } break;
        case exp_sub:
        {
            result = (e1 - e2);
        } break;
        case exp_mul:
        {
            result = (e1 * e2);
        } break;
        case exp_div:
        {
            result = (e1 / e2);
        } break;
        case exp_rem:
        {
            result = (e1 % e2);
        } break;
        case exp_andand:
        {
            result = (e1 && e2);
        } break;
        case exp_and:
        {
            result = (e1 & e2);
        } break;
        case exp_oror:
        {
            result = (e1 || e2);
        } break;
        case exp_or:
        {
            result = (e1 | e2);
        } break;
        case exp_xor:
        {
            result = (e1 ^ e2);
        } break;
        case exp_lsh:
        {
           result = (e1 << e2);
        } break;
        case exp_rsh:
        {
            result = (e1 >> e2);
        } break;
        case exp_identifier:
        {
            char* identifierChars =
                IdentifierPtrToCharPtr(&parser->IdentifierTable, e->IdentifierPtr);
            metac_identifier_ptr_t defineIdPtr;
            definedIdPtr =
                MetaCPreProcessor_GetDefineIdPtr(self, e->IdentifierKey, identifierChars);
            metac_expression_t* resolved = 0;
            if (defineIdPtr.v)
            {
                resolved = MetaCPreProcessor_ResolveDefineToExp(self, definedIdPtr);
                result = resolved ? MetaCPreProcessor_EvalExp(self, resolved, parser) : 0;
            }
            else
            {
                result = 0;
            }
        }
        break;
        case exp_variable:
        {
            // this should not happen
            assert(0);
        } break;

        case exp_paren:
        {
            result = MetaCPreProcessor_EvalExp(self, e->E1, parser);
        } break;
        case exp_compl:
        {
            result = ~MetaCPreProcessor_EvalExp(self, e->E1, parser);
        } break;
        case exp_not:
        {
            result = !MetaCPreProcessor_EvalExp(self, e->E1, parser);
        } break;
        case exp_umin:
        {
            result = -MetaCPreProcessor_EvalExp(self, e->E1, parser);
        } break;
        case exp_post_increment:
        {
            assert(0); // I don't think this can happen
        } break;

        case exp_call:
        {
            if (e->E1->Kind == exp_identifier ||
                e->E1->IdentifierKey == defined_key)
            {
                exp_argument_t* args = (exp_argument_t*)e->E2;
                metac_expression_t* e2 = args->Expression;
                // this if makes sure there is only one "argument" to defiend()
                if (args->Next != emptyPointer)
                    goto LcallInEval;
                if (e2->Kind != exp_identifier)
                    goto LcallInEval;

                definedIdPtr = e2->IdentifierPtr;
                LhandleDefined:
                {
                    printf("defined(%s)\n", IdentifierPtrToCharPtr(&self->Lexer->IdentifierTable, definedIdPtr));
                }
            }
            else
            {
            LcallInEval:
                  printf("single Identifier expected after defined(\n");
            }
        } break;
    }
}

void MetaCPreProcessor_Init(metac_preprocessor_t *self, metac_lexer_t* lexer,
                            metac_file_storage_t* fs, const char* filepath)
{
    self->FileStorage = fs;
    if (filepath)
        self->File = MetaCFileStorage_LoadFile(fs, filepath);
    IdentifierTable_Init(&self->DefineTable, IDENTIFIER_LENGTH_SHIFT, 7);
    self->Lexer = lexer;
    printf("Initialized preproc\n");
}
#include "metac_array.h"

metac_preprocessor_define_t
MetaCPreProcessor_ParseDefine(metac_preprocessor_t *self, metac_parser_t* parser)
{
    metac_token_t* defineName = MetaCParser_Match(parser, tok_identifier);
    bool isMacro = MetaCParser_PeekMatch(parser, tok_lParen, 1);

    bool isVariadic = false;
    /// macro contains ##
    bool hasPaste = false;

    DEF_STACK_ARRAY(metac_identifier_ptr_t, macroParameter, 16);

    DEF_STACK_ARRAY(metac_token_t, defineBodyToken, 64);

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

    uint32_t definedNameLength = defineName->IdentifierKey >> IDENTIFIER_LENGTH_SHIFT;

    const char* defineNameChars =
        IdentifierPtrToCharPtr(&parser->Lexer->IdentifierTable,
                               defineName->IdentifierPtr);

    printf("define %s with %u parameters\n", defineNameChars,
                                             macroParameter.Count);
    MetaCPrinter_Reset(&parser->DebugPrinter);
/*
    if (macroParameterCount)
    {
        printf("(");
        for(uint32_t p = 0; p < PARAM_MASK(macroParameterCount) - 1; p++)
        {
            metac_identifier_ptr_t paramId = macroParameter[p];
            const char* pStr =
                IdentifierPtrToCharPtr(&parser->Lexer->IdentifierTable, paramId);
            printf("%s, ", pStr);
        }
        const char* pStr =
            IdentifierPtrToCharPtr(&parser->Lexer->IdentifierTable, macroParameter[PARAM_MASK(macroParameterCount) - 1]);
        printf("%s)\n", pStr);
    }
*/
    uint32_t hash = crc32c(~0, defineNameChars, definedNameLength);
    uint32_t defineKey = IDENTIFIER_KEY(hash, definedNameLength);

    metac_identifier_ptr_t defineIdPtr =
        GetOrAddIdentifier(&self->DefineTable, defineKey, defineNameChars);

    metac_token_t* currentToken;
    uint32_t peek1 = 1;

    for(;;)
    {
        const char* s = 0;

        currentToken = MetaCParser_PeekToken(parser, peek1++);
        if (!currentToken) break;

        if (currentToken->TokenType == tok_identifier)
        {
            for(uint32_t i = 0; i < macroParameter.Count; i++)
            {
                if (currentToken->IdentifierPtr.v == macroParameter.Ptr[i].v)
                {
                    metac_token_t tok = *currentToken;
                    tok.TokenType = tok_macro_parameter;
                    tok.MacroParameterIndex = i;
                    ADD_STACK_ARRAY(defineBodyToken, tok);
                    goto Lbreak;
                }
            }
            goto LaddToken;
            Lbreak:{}
        }
        else
        {
            if (currentToken->TokenType == tok_hashhash)
                hasPaste |= 1;
        LaddToken:
            ADD_STACK_ARRAY(defineBodyToken, *currentToken);
        }
        MetaCPrinter_Reset(&parser->DebugPrinter);
    }
    PERSIST_STACK_ARRAY(defineBodyToken);

    metac_preprocessor_define_t result;
    result.loc = LocationFromToken(parser, defineName);
    result.DefineName = defineIdPtr;
    result.ParameterCount = macroParameter.Count;
    result.IsVariadic = isVariadic;
    result.HasPaste = hasPaste;

    return result;
}


metac_identifier_ptr_t MetaCPreProcessor_GetDefineIdPtr(metac_preprocessor_t* self,
                                                        uint32_t identifierKey, const char* identifier)
{
    metac_identifier_ptr_t result = {0};

    if (self->Parent)
    {
         result = MetaCPreProcessor_GetDefineIdPtr(self->Parent, identifierKey, identifier);
    }

    if(!result.v)
    {
        result = IsIdentifierInTable(&self->DefineTable, identifierKey, identifier);
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

        metac_expression_t* exp = MetaCParser_ParseExpression(parser, expr_flags_pp, 0);
        const char* exp_string = MetaCPrinter_PrintExpression(&parser->DebugPrinter, exp);
        printf("#eval %s\n", exp_string);
        MetaCPrinter_Reset(&parser->DebugPrinter);

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
