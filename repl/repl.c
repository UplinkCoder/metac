#include "../compat.h"
#include "../metac_parser_obj.c"
#include "../metac_semantic_obj.c"
#include "../metac_driver.c"
#include "../metac_compiler_interface.h"
#include "../bsr.h"
#include "../crc32c.h"
//#include "../metac_eeP.c"
#include "../3rd_party/linenoise/linenoise.c"
#include "../int_to_str.c"
#include <stdio.h>
#include "exp_eval.c"
#include "../metac_type_table.h"
#include "../metac_task.h"

extern bool g_exernalIdentifierTable;
extern metac_lexer_t g_lineLexer;
extern void LineLexerInit();

#ifndef NO_FIBERS
#ifdef HAS_TLS
extern __thread worker_context_t *threadContext;
#else
extern worker_context_t *threadContext;
#endif
#endif

metac_statement_t* MetaCParser_ParseStatementFromString(const char* str);
metac_declaration_t* MetaCParser_ParseDeclarationFromString(const char* str);

const char* MetaCTokenEnum_toChars(metac_token_enum_t tok);

typedef enum parse_mode_t
{
    parse_mode_token = 0,

    parse_mode_decl,
    parse_mode_stmt,
    parse_mode_expr,
    parse_mode_file,
    parse_mode_preproc,

    parse_mode_ds,
    parse_mode_ss,

    parse_mode_ee,
    parse_mode_es,
    parse_mode_setvars,

    parse_mode_max
} parse_mode_t;

void PrintHelp(void)
{
    printf(
       "Type  :e for expression mode\n"
       "      :ee for evaluation mode\n"
       "      :es for expression semantic mode\n"
       "      :d for declaration mode\n"
       "      :ds for declaration semantic mode\n"
       "      :v for varible mode (set vars for eval)\n"
       "      :s for statement mode\n"
       "      :ss for statement semantic mode\n"
       "      :t for token mode\n"
       "      :l Load and lex file\n"
       "      :p for preprocessor mode\n"
       "      :q to quit\n");
}

#include "../utils/read_file.c"

typedef struct identifier_translation_context_t
{
    const uint32_t FunctionKey;
    const metac_identifier_table_t* SrcTable;
    metac_identifier_table_t* DstTable;
} identifier_translation_context_t;

static inline void TranslateIdentifier(metac_identifier_table_t* dstTable,
                         const metac_identifier_table_t* srcTable,
                         metac_identifier_ptr_t* idPtrP)
{
    assert(idPtrP->v);
    const char* idChars =
        IdentifierPtrToCharPtr((metac_identifier_table_t*)srcTable, *idPtrP);
    const uint32_t idLen = strlen(idChars);
    const uint32_t idHash = crc32c_nozero(~0, idChars, idLen);
    const uint32_t idKey = IDENTIFIER_KEY(idHash, idLen);

    metac_identifier_ptr_t newPtr =
        GetOrAddIdentifier(dstTable, idKey, idChars);
    idPtrP->v = newPtr.v;
}

static inline int TranslateIdentifiers(metac_node_t node, void* ctx)
{
    identifier_translation_context_t* context =
        (identifier_translation_context_t*) ctx;
    assert(crc32c_nozero(~0, __FUNCTION__, strlen(__FUNCTION__) == context->FunctionKey));

    const metac_identifier_table_t* SrcTable = context->SrcTable;
    metac_identifier_table_t* DstTable = context->DstTable;

    switch(node->Kind)
    {
        case decl_variable:
        {
            decl_variable_t* variable = (decl_variable_t*) node;
            TranslateIdentifier(DstTable, SrcTable, &variable->VarIdentifier);
        } break;
        case decl_type:
        {
            decl_type_t* type = (decl_type_t*) node;
            if (type->TypeIdentifier.v && type->TypeIdentifier.v != empty_identifier.v)
                TranslateIdentifier(DstTable, SrcTable, &type->TypeIdentifier);
        } break;
        case decl_type_struct:
        {
            decl_type_struct_t* struct_ = (decl_type_struct_t*) node;
            if (struct_->BaseIdentifier.v != empty_identifier.v)
                TranslateIdentifier(DstTable, SrcTable, &struct_->BaseIdentifier);
            if (struct_->Identifier.v != empty_identifier.v)
                TranslateIdentifier(DstTable, SrcTable, &struct_->Identifier);
        }
        default : break;
    }

    return 0;
}

static inline int FindStatementCb(metac_node_t node, void* ctx)
{
    switch(cast(metac_statement_kind_t)node->Kind)
    {
        case stmt_if:
        {
            stmt_if_t* stmt_if = cast(stmt_if_t*) node;
            printf("Found if statement: if (%s)", stmt_if->IfCond);
        } break;
    }
    return 0;
}

typedef struct presemantic_context_t
{
    int32_t sz;
    const uint32_t FunctionKey;
    metac_semantic_state_t* Sema;
} presemantic_context_t;

static inline int Presemantic(metac_node_t node, void* ctx)
{
    presemantic_context_t* context =
        (presemantic_context_t*) ctx;
    assert(crc32c_nozero(~0, __FUNCTION__, strlen(__FUNCTION__) == context->FunctionKey));

    if (node->Kind == node_decl_type_typedef)
    {
        decl_type_typedef_t* typedef_ = (decl_type_typedef_t*) node;
        metac_identifier_ptr_t typedefId = typedef_->Identifier;

        metac_type_index_t typeIndex =
            MetaCSemantic_doTypeSemantic(context->Sema, node);

        return 1;
    }
    else
    {
        //printf("Not a typedef: %s\n", MetaCNodeKind_toChars(node->Kind));
    }

    return 0;
}

typedef struct repl_state_t
{
    parse_mode_t parseMode;
    metac_lexer_state_t repl_state;
    metac_lexer_t lexer;
    metac_preprocessor_t preProcessor;
    metac_type_aggregate_t* compilerStruct;
    metac_semantic_state_t sema;

    const char* line;

    const char* promt;

    char* srcBuffer;
    void* freePtr;

    uint32_t lineSz;
    uint32_t srcBufferLength;

    metac_printer_t printer;
    variable_store_t vstore;
} repl_state_t;

void Presemantic_(repl_state_t* self)
{
    metac_type_aggregate_t* compilerStruct = 0;

    read_result_t fCompilterInterface =
        ReadFileAndZeroTerminate("metac_compiler_interface.h");
    if (!fCompilterInterface.FileContent0)
        fCompilterInterface =
        ReadFileAndZeroTerminate("../metac_compiler_interface.h");

    if (fCompilterInterface.FileContent0)
    {
        metac_lexer_t tmpLexer;
        metac_parser_t tmpParser;

        DeclarationArray decls = {0};
        {
            MetaCLexer_Init(&tmpLexer);

            LexFile(&tmpLexer, "metac_compiler_interface.h",
                    fCompilterInterface.FileContent0,
                    fCompilterInterface.FileLength);

            MetaCParser_InitFromLexer(&tmpParser, &tmpLexer);
            ParseFile(&tmpParser, "metac_compiler_interface.h", &decls);
        }
        MetaCLexer_Free(&tmpLexer);

        metac_semantic_state_t tmpSema;
        MetaCSemantic_Init(&tmpSema, &tmpParser, 0);
        MetaCSemantic_PushNewScope(&tmpSema, scope_parent_module, 0);

        presemantic_context_t presemanticContext = {
            sizeof(presemantic_context_t),
            crc32c_nozero(~0, "Presemantic", sizeof("Presemantic") - 1),
            &tmpSema,
        };

        for(int i = 0;
            i < decls.Length;
            i++)
        {
            MetaCNode_TreeWalk_Real(decls.Ptr[i], Presemantic, &presemanticContext);
        }

        for(int i = 0;
            i < decls.Length;
            i++)
        {
            metac_identifier_ptr_t printIdentifier = {0};

            metac_declaration_t* decl = decls.Ptr[i];

            if (decl->DeclKind == decl_type_typedef)
            {
                decl_type_typedef_t* typedef_ = (decl_type_typedef_t*) decl;
                if (typedef_->Type->DeclKind == decl_type_struct)
                {
                    decl_type_struct_t* structPtr = (decl_type_struct_t*)typedef_->Type;
                    if (structPtr->Identifier.v == empty_identifier.v)
                    {
                        printIdentifier = typedef_->Identifier;
                    }
                    decl = (metac_declaration_t*)typedef_->Type;
                }
            }

            if (decl->DeclKind == decl_type_struct)
            {
                decl_type_struct_t* struct_ = (decl_type_struct_t*) decl;
                if (struct_->Identifier.v != empty_identifier.v)
                {
                    printIdentifier = struct_->Identifier;
                }
 //               printf("found struct : '%s'\n",
 //                   IdentifierPtrToCharPtr(&tmpParser.IdentifierTable, printIdentifier));

                compilerStruct = MetaCSemantic_doDeclSemantic(&tmpSema, struct_);
            }
        }

        MetaCSemantic_Handoff(&tmpSema,
                              (metac_sema_declaration_t**)&self->sema.CompilerInterface,
                              &self->sema);
        // FreeSema
        MetaCParser_Free(&tmpParser);
    }
}

void Repl_SwtichMode(repl_state_t* self)
{
    switch (self->parseMode)
    {
    case parse_mode_max: assert(0);
    case parse_mode_file:
        self->promt = ">File<";
        break;
    case parse_mode_token:
        self->promt = "Token>";
        break;
    case parse_mode_decl:
        self->promt = "Decl>";
        linenoiseSetMultiLine(1);
        break;
    case parse_mode_expr:
        self->promt = "Exp>";
        break;
    case parse_mode_stmt:
        self->promt = "Stmt>";
        break;
    case parse_mode_preproc:
        self->promt = "Preproc>";
        break;
    case parse_mode_ee:
        self->promt = "EE>";
        break;
    case parse_mode_es:
        self->promt = "ES>";
        break;
    case parse_mode_ss:
        self->promt = "SS>";
        break;
    case parse_mode_ds:
        self->promt = "DS>";
        break;
    case parse_mode_setvars:
        self->promt = "SetVars>";
        break;
    }
}

void Repl_Init(repl_state_t* self)
{
    self->repl_state.Position = 0;
    self->repl_state.Line = 1;
    self->repl_state.Column = 1;
    self->parseMode = parse_mode_ee;

    self->compilerStruct = 0;

    self->srcBuffer = 0;
    self->freePtr = 0;
    self->srcBufferLength = 0;

    self->promt = ">";

    // init line lexer with more than the default
    // tokens
    g_lineLexer.Tokens =
        (metac_token_t*)malloc(128 * sizeof(metac_token_t));
    g_lineLexer.TokenCapacity = 128;
    g_lineLexer.LocationStorage.Locations =
        (metac_location_t*)malloc(128 * sizeof(metac_location_t));
    g_lineLexer.LocationStorage.LocationCapacity = 128;
    // make sure we know our special identifiers
    LineLexerInit();

    MetaCLexer_Init(&self->lexer);

    MetaCSemantic_Init(&self->sema, &g_lineParser, 0);
    MetaCSemantic_PushNewScope(&self->sema, scope_parent_module, 1);

    MetaCPrinter_Init(&self->printer,
        &g_lineParser.IdentifierTable,
        &g_lineParser.StringTable);

    VariableStore_Init(&self->vstore);

    _ReadContextCapacity = 32;
    _ReadContexts = (ReadI32_Ctx*)
        malloc(sizeof(ReadI32_Ctx) * _ReadContextCapacity);
    _ReadContextSize = 0;
}

#ifndef NO_FIBERS
typedef struct MetaCRepl_ExprSemantic_context_t
{
    repl_state_t* Repl;
    metac_expression_t* Exp;
} MetaCRepl_ExprSemantic_context_t;

void MetaCRepl_ExprSemantic_Task(task_t* task)
{
    MetaCRepl_ExprSemantic_context_t* ctx =
        (MetaCRepl_ExprSemantic_context_t*)
            task->Context;

    metac_sema_expression_t* result;

    ENQUEUE_TASK(result, MetaCSemantic_doExprSemantic_,
                 (&ctx->Repl->sema), ctx->Exp, 0);

    task->TaskFlags |= Task_Waiting;
    printf("Just before yield\n");
    YIELD(WaitForExprSemantic);

    printf("typeIndex.v: %x\n", result->TypeIndex.v);
    const char* type_str = TypeToChars(&ctx->Repl->sema, result->TypeIndex);

    printf("typeof(%s) = %s\n",
           MetaCPrinter_PrintExpression(&ctx->Repl->printer, ctx->Exp));
}
#endif

/// returns false if the repl is done running
bool Repl_Loop(repl_state_t* repl)
{
    worker_context_t* worker = CurrentWorker();
LswitchMode:
    Repl_SwtichMode(repl);

    {
        repl->line = linenoise(repl->promt);
        repl->lineSz = strlen(repl->line);
        linenoiseHistoryAdd(repl->line);
        TracyCMessage(repl->line, repl->lineSz)
    }

    {
        uint32_t line_length = strlen(repl->line);
        if (*repl->line == ':')
        {
            switch(repl->line[1])
            {
            case 'q':
                linenoiseHistorySave(".repl_history");
                linenoiseFree((void*)repl->line);
                return false;
            case 'f' :
            {
                repl->parseMode = parse_mode_file;
                const char* filename = repl->line + 3;
                printf("querying fileStorage");
                metac_file_storage_t* fs = Worker_GetFileStorage(worker);
                metac_file_ptr_t f = MetaCFileStorage_LoadFile(fs, filename);
            } break;
            case 'l' :
            {
                repl->parseMode = parse_mode_file;
                const char* filename = repl->line + 3;
                printf("loading and lexing: '%s'\n", filename);
                FILE* fd = fopen(filename, "rb");
                if (!fd)
                {
                    perror(filename);
#if defined(__linux__) && defined(TRACY_ENABLE)
                    char message[256];
                    uint32_t sz = sprintf(message, "%s/%s", get_current_dir_name(), filename);
#endif
                    TracyCMessage(message, sz);
                }
                else
                {
                    fseek(fd, 0, SEEK_END);
                    uint32_t sz = ftell(fd);
                    fseek(fd, 0, SEEK_SET);

                    uint32_t estimatedTokenCount = (((sz / 5) + 128) & ~127);
                    if (repl->lexer.TokenCapacity < estimatedTokenCount)
                    {
                        repl->lexer.Tokens = (metac_token_t*)
                            malloc(sizeof(metac_token_t) * estimatedTokenCount);
                        repl->lexer.TokenSize = 0;
                        repl->lexer.TokenCapacity = estimatedTokenCount;
                        repl->lexer.Tokens = (metac_token_t*)
                            malloc(sizeof(metac_token_t) * estimatedTokenCount);

                        repl->lexer.LocationStorage.Locations = (metac_location_t*)
                            malloc(sizeof(metac_location_t) * estimatedTokenCount);
                        repl->lexer.LocationStorage.LocationSize = 0;
                        repl->lexer.LocationStorage.LocationCapacity = estimatedTokenCount;
                    }

                    repl->freePtr = repl->srcBuffer = calloc(1, sz + 4);
                    repl->srcBufferLength = sz;
                    fread((void*)repl->srcBuffer, 1, sz, fd);
                    repl->parseMode = parse_mode_file;
                    repl->repl_state.Position = 0;
                    repl->repl_state.Line = 1;
                    repl->repl_state.Column = 1;
                    repl->repl_state.Size = sz;
                }
                break;
            }
            case 't' :
                repl->parseMode = parse_mode_token;
                goto LswitchMode;
            case 'd' :
                switch (repl->line[2])
                {
                default:
                    repl->parseMode = parse_mode_decl;
                    goto LswitchMode;

                 case 's':
                    repl->parseMode = parse_mode_ds;
                    goto LswitchMode;
                }

            case 'v' :
                repl->parseMode = parse_mode_setvars;
                goto LswitchMode;
            case 'e' :
                switch(repl->line[2])
                {
                default:
                    repl->parseMode = parse_mode_expr;
                    goto LswitchMode;
                case 'e':
                    repl->parseMode = parse_mode_ee;
                    goto LswitchMode;
                case 's':
                    repl->parseMode = parse_mode_es;
                    goto LswitchMode;
                }
            case 's' :
                switch (repl->line[2])
                {
                default:
                    repl->parseMode = parse_mode_stmt;
                    goto LswitchMode;
                case 's':
                    repl->parseMode = parse_mode_ss;
                    goto LswitchMode;
                }
                break;
            case 'p' :
            {
                repl->parseMode = parse_mode_preproc;
                goto LswitchMode;
            } break;
            default :
                printf("Command :%c unknown type :h for help\n", *(repl->line + 1));
                break;
            case 'h' :
                PrintHelp();
                break;
            }
        }
        else if (repl->line[0] == '.')
        {
            if (line_length == sizeof("scope")
             && 0 == memcmp(repl->line + 1, "scope", strlen("scope")))
            {
                if (repl->sema.CurrentScope)
                {
                    metac_scope_table_t* table = &repl->sema.CurrentScope->ScopeTable;
                    uint32_t nMembers = table->SlotsUsed;

                    for(uint32_t slotIdx = 0, memberIdx = 0; memberIdx < nMembers; slotIdx++)
                    {
                        metac_scope_table_slot_t slot = table->Slots[slotIdx];
                        if (slot.Hash)
                        {
                            printf("Member [%u] : %s\n", memberIdx++, MetaCPrinter_PrintSemaNode(&repl->printer, &repl->sema, slot.Node));
                        }
                    }
                }
                goto LnextLine;
            }
        }

        if (repl->parseMode != parse_mode_file)
        {
            repl->srcBuffer = (char*)repl->line;
            repl->srcBufferLength = line_length;
        }

        while (repl->srcBufferLength > 0)
        {
            metac_token_t token;

            metac_expression_t* exp;
            metac_statement_t* stmt;
            metac_declaration_t* decl;

            uint32_t initalPosition = repl->repl_state.Position;
            switch(repl->parseMode)
            {
            case parse_mode_max: break;
            case parse_mode_expr:
            {
                 exp =
                    MetaCParser_ParseExpressionFromString(repl->line);

                const char* str = MetaCPrinter_PrintExpression(&repl->printer, exp);
                printf("expr = %s\n", str);
                MetaCPrinter_Reset(&repl->printer);
                goto LnextLine;
            }

            case parse_mode_preproc:
            {
                metac_token_t _inlineTokens[32];

                metac_token_buffer_t buffer = {
                    _inlineTokens, 0, ARRAY_SIZE(_inlineTokens)
                };
                metac_preprocessor_directive_t directive =
                    MetaCParser_ParsePreprocFromString(repl->line, &repl->preProcessor);

                if (directive == pp_eval)
                {
                    MetaCPreProcessor_Eval(&repl->preProcessor, &g_lineParser);
                }
                goto LnextLine;
            }

            case parse_mode_ee:
            {
                exp =
                    MetaCParser_ParseExpressionFromString(repl->line);
                const char* str = MetaCPrinter_PrintExpression(&repl->printer, exp);

                metac_sema_expression_t* result =
                    MetaCSemantic_doExprSemantic(&repl->sema, exp, 0);

                metac_expression_t printExpStorage;

                metac_sema_expression_t eval_exp = evalWithVariables(result, &repl->vstore);
                result = &eval_exp;

                const char* result_str;
                if (eval_exp.Kind == exp_type)
                {
                    result_str = MetaCPrinter_PrintSemaNode(&repl->printer, &repl->sema, &eval_exp);
                }
                else
                {
                    result_str = MetaCPrinter_PrintSemaNode(&repl->printer, &repl->sema, &eval_exp);
                }
                printf("%s = %s\n", str, result_str);
                MetaCPrinter_Reset(&repl->printer);
                // XXX static and fixed size state like _ReadContext
                // should go away soon.
                _ReadContextSize = 0;
                goto LnextLine;
            }
            case parse_mode_es:
            {
                exp =
                    MetaCParser_ParseExpressionFromString(repl->line);

                const char* str = MetaCPrinter_PrintExpression(&repl->printer, exp);

                metac_sema_expression_t* result =
                    MetaCSemantic_doExprSemantic(&repl->sema, exp, 0);

            Lcontinuation:
                {
                    printf("typeIndex.v: %x\n", result->TypeIndex.v);
                    const char* type_str = TypeToChars(&repl->sema, result->TypeIndex);

                    metac_printer_t printer;
                    MetaCPrinter_InitSz(&printer,
                                        &repl->lexer.IdentifierTable,
                                        &repl->lexer.StringTable, 512);

                    printf("typeof(%s) = %s\n",
                           MetaCPrinter_PrintExpression(&repl->printer, exp), type_str);
                }
#if 0

//                EQUEUE_WITH_CONT(MetaCSemantic_doExprSemantic_, &continuation, &repl->sema, exp);

                do {
                    taskqueue_t* q = &CurrentWorker()->Queue;
                    task_t task = {0};
                    MetaCSemantic_doExprSemantic_task_context_t ctx = {&repl->sema, exp};
                    MetaCSemantic_doExprSemantic_task_context_t* ctxPtr = &ctx;
                    _Static_assert(sizeof(task._inlineContext) >= sizeof(MetaCSemantic_doExprSemantic_task_context_t), "Context size");
                    task.Context = task._inlineContext;
                    task.TaskFunction = MetaCSemantic_doExprSemantic_Task;
                    task.Parent = CurrentTask();
                    task.Continuation = &continuation;
                    task.ContextSize = sizeof(MetaCSemantic_doExprSemantic_task_context_t);
                    ( task.Origin.File = "repl.c", task.Origin.Func = __FUNCTION__, task.Origin.Line = 593 );
                    (*((MetaCSemantic_doExprSemantic_task_context_t*)task.Context)) = ctx;
                    TaskQueue_Push(q, &task);
                } while(0);
#endif
                goto LnextLine;
            }

            case parse_mode_setvars :
            {
                metac_declaration_t* decl = MetaCParser_ParseDeclarationFromString(repl->line);
                if (decl)
                {
                    metac_identifier_ptr_t idPtr = {0};
                    idPtr = IdentifierPtrFromDecl(decl);

                    if (idPtr.v == 0)
                    {
                        fprintf(stderr, "declaration could not be handled only functions are supported for now\n");
                        goto LnextLine;
                    }

                    const char* idChars = IdentifierPtrToCharPtr(&g_lineParser.IdentifierTable, idPtr);
                    const uint32_t length = strlen(idChars);
                    uint32_t idHash = crc32c_nozero(~0, idChars, length);
                    uint32_t idKey = IDENTIFIER_KEY(idHash, length);
/*
                    metac_identifier_ptr_t dstoreId
                        = GetOrAddIdentifier(&dstore.Table, idKey, idChars);

                    if (decl->DeclKind == decl_function)
                    {
                        decl->decl_function.Identifier = dstoreId;
                        printf("Setting dStore ID: %u\n", dstoreId.v);
                    }
                    else if (decl->DeclKind == decl_variable)
                    {
                        decl->decl_variable.VarIdentifier = dstoreId;

                        //VariableStore_SetValueI32(&vstore, assignExp->E1, (int32_t)assignExp->E2->ValueI64);
                    }

                    DeclarationStore_SetDecl(&dstore, dstoreId, decl);
*/
                    printf("Registering %s [v=%u] in scope\n", idChars, idPtr.v);
                    MetaCSemantic_RegisterInScope(&repl->sema, idPtr, decl);
                    goto LnextLine;
                }
                else
                {
                    metac_expression_t* assignExp = MetaCParser_ParseExpressionFromString(repl->line);
                    if (assignExp)
                    {
                        if (assignExp->Kind != exp_assign)
                        {
                            fprintf(stderr, "You must write an expression of the from identifier = value");
                            goto LnextLine;
                        }

                        metac_sema_expression_t* ae =
                            MetaCSemantic_doExprSemantic(&repl->sema, assignExp, 0);
                        if (ae)
                        {
                            if (ae->E1->Kind == exp_identifier)
                            {
                                assert(0);
                            }
                            assert(ae->E2->Kind == exp_signed_integer);

                            VariableStore_SetValueI32(&repl->vstore, ae->E1, (int32_t)ae->E2->ValueI64);
                        }
                        else
                        {
                            fprintf(stderr, "Semantic on assign exp failed\n");
                        }
                        // MetaCSemantic_Free(&sema);
                        goto LnextLine;
                    }
                    else
                    {
                        fprintf(stderr, "Input did not parse as either an assign-expression or declaration\n");
                    }
                }
            } break;

            case parse_mode_stmt :
            {
                stmt = MetaCParser_ParseStatementFromString(repl->line);
                if (stmt)
                    printf("stmt = %s\n", MetaCPrinter_PrintStatement(&repl->printer, stmt));
                else
                   fprintf(stderr, "couldn't parse statement\n");
                MetaCPrinter_Reset(&repl->printer);
                goto LnextLine;
            }

            case parse_mode_decl :
            {
                decl = MetaCParser_ParseDeclarationFromString(repl->line);
                if (decl)
                    printf("decl = %s\n", MetaCPrinter_PrintDeclaration(&repl->printer, decl));
                else
                    printf("Couldn't parse Declaration\n");
                linenoiseSetMultiLine(false);

                goto LnextLine;
            }

            case parse_mode_ds :
            {
                decl = MetaCParser_ParseDeclarationFromString(repl->line);
                if (decl)
                    printf("decl = %s\n", MetaCPrinter_PrintDeclaration(&repl->printer, decl));
                else
                    printf("Couldn't parse Declaration\n");
#ifndef NO_FIBERS
                MetaCSemantic_doDeclSemantic_task_context_t ctx =
                {
                    &repl->sema, decl, 0
                };
                MetaCSemantic_doDeclSemantic_task_context_t* ctxPtr =
                    &ctx;
                task_t DeclSemaTask = {0};
                DeclSemaTask.TaskFunction = MetaCSemantic_doDeclSemantic_Task;
                DeclSemaTask.Context = ctxPtr;
                DeclSemaTask.ContextSize = sizeof(ctx);

                worker_context_t* replWorker = CurrentWorker();
                taskqueue_t* q = &replWorker->Queue;
                ORIGIN(DeclSemaTask.Origin);
                uint32_t taskId = TaskQueue_Push(q, &DeclSemaTask);

                if (taskId == 0)
                {
                    printf("Couldn't Push\n");
                }
#else
                metac_sema_declaration_t* ds =
                    MetaCSemantic_doDeclSemantic(&repl->sema, decl);
#endif
                goto LnextLine;
            }

            case parse_mode_file :
                goto LlexSrcBuffer;
            case parse_mode_token :

LlexSrcBuffer: {}
#if 1
                token = *MetaCLexerLexNextToken(&repl->lexer, &repl->repl_state, repl->srcBuffer, repl->srcBufferLength);

                uint32_t eaten_chars = repl->repl_state.Position - initalPosition;
                const uint32_t token_length = MetaCTokenLength(token);
#if 1
                const uint32_t locPtr = token.LocationId;
                const metac_location_t loc = repl->lexer.LocationStorage.Locations[locPtr - 4];

                printf("read tokenType: %s {length: %d}\n Location: {Line: %d, Col: %d}",
                        MetaCTokenEnum_toChars(token.TokenType), token_length,
                        loc.StartLine, loc.StartColumn
                );

                if (token.TokenType == tok_identifier)
                {
                    printf("    %.*s\n", LENGTH_FROM_IDENTIFIER_KEY(token.Key), IDENTIFIER_PTR(&repl->lexer.MEMBER_SUFFIX(Identifier), token));
                }
                else if (token.TokenType == tok_char)
                {
                    printf("    '%.*s'\n", token.charLength, token.chars);
                }
                else if (token.TokenType == tok_char_uni)
                {
                    printf("    '\\U%.*s'\n", token.charLength, token.chars);
                }
                else if (token.TokenType == tok_uint)
                {
                    char buffer[21];
                    printf("    %s\n", u64tostr(token.ValueU64, buffer));
                }
                else if (token.TokenType == tok_string)
                {
                    printf("    \"%.*s\"\n", LENGTH_FROM_STRING_KEY(token.Key), STRING_PTR(&repl->lexer.String, token.StringPtr));
                }
#endif

                repl->srcBufferLength -= eaten_chars;
                repl->srcBuffer += eaten_chars;
                printf("eaten_chars: %d\n", eaten_chars);
                if (!token_length)
                    break;
            }
        }

        {
            repl->repl_state.Line++;
            repl->repl_state.Column = 1;
            repl->lexer.TokenSize = 0;
            repl->lexer.LocationStorage.LocationSize = 0;
        }

        if (repl->freePtr)
        {
            free(repl->freePtr);
            repl->freePtr = 0;
            repl->parseMode = parse_mode_token;
            goto LswitchMode;
        }
        repl->srcBuffer = 0;
#endif
    }
LnextLine:
    return true;
}

void Repl_Fiber()
{
    repl_state_t repl_;
    repl_state_t* repl = &repl_;
    Repl_Init(repl);

    PrintHelp();
    linenoiseHistoryLoad(".repl_history");

    // Presemantic_(repl);

    while (Repl_Loop(repl) != false)
    {
#ifndef NO_FIBERS
        YIELD(ReplYield);
#endif
    }

    fprintf(stderr, "Repl_Loop exited this should only happen on quit\n");
#ifndef NO_FIBERS
    aco_exit1(GET_CO());
#endif
}

void ReplMainFiber(void)
{
    printf("I am the repl main fiber\n");
}

int main(int argc, const char* argv[])
{


/*
    printf("Transfered decl: %s\n",
        MetaCPrinter_PrintDeclaration(&printer, compilerStruct));
*/

    // only here can we destroy tmpSema



/*
    metac_dot_printer_t dot_printer;
    MetaCDotPrinter_Init(&dot_printer, &g_lineParser.IdentifierTable);
    g_lineParser.DotPrinter = &dot_printer;
*/
#ifndef NO_FIBERS
    worker_context_t replWorkerContext = {0};
    threadContext = &replWorkerContext;
    RunWorkerThread(&replWorkerContext, Repl_Fiber, 0);
#else
    Repl_Fiber();
#endif
    return 1;
}
