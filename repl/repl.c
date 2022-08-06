#define ACCEL ACCEL_TABLE

#include "../compat.h"
#ifndef NO_FIBERS
#  include "../metac_task.c"
#endif
#include "../metac_parser_obj.c"
#include "../metac_semantic_obj.c"
#include "../metac_driver.c"
#include "../metac_lpp.c"
#include "../metac_compiler_interface.h"
#include "../bsr.h"
#include "../crc32c.h"
#include "../semantic/handoff.c"
#include "../int_to_str.c"
#include <stdio.h>
#include "exp_eval.c"
#include "../metac_type_table.h"
#ifndef NO_FIBERS
#  include "../metac_task.h"
#endif
#include "repl.h"

const char* MetaCTokenEnum_toChars(metac_token_enum_t tok);

#define MSGF(FMT, ...) uiInterface.Message(uiState, FMT, __VA_ARGS__)
#define MSG(STR) MSGF(STR, 0);

#define ERRORF(FMT, ...) MSGF(FMT, __VA_ARGS__)
#define ERROR(FMT) MSG(FMT)

void HelpMessage(ui_interface_t uiInterface, struct ui_state_t* uiState)
{
    MSG(
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
        } break;

        default : break;
    }

    return 0;
}

#if 0
typedef struct find_statement_context_t
{
    metac_printer_t* Printer;
    metac_statement_kind_t Kind;
} find_statement_context_t;

static inline int FindStatementCb(metac_node_t node, void* ctx)
{
    find_statement_context_t* context =
        (find_statement_context_t*) ctx;
    switch(cast(metac_statement_kind_t)node->Kind)
    {
        case stmt_if:
        {
            stmt_if_t* stmt_if = cast(stmt_if_t*) node;
            MetaCPrinter_Reset(context->Printer);
            MetaCPrinter_PrintStatement(context->Printer, node);
            const char* stmt_str =
                context->Printer->StringMemory;
            MSGF("Found if statement: if (%s)\n", stmt_str);
        } break;
    }
    return 0;
}
#endif

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
        //MSGF("Not a typedef: %s\n", MetaCNodeKind_toChars(node->Kind));
    }

    return 0;
}


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
        MetaCSemantic_PushNewScope(&tmpSema, scope_owner_module, 0);

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

            if (decl->Kind == decl_type_typedef)
            {
                decl_type_typedef_t* typedef_ = (decl_type_typedef_t*) decl;
                if (typedef_->Type->Kind == decl_type_struct)
                {
                    decl_type_struct_t* structPtr = (decl_type_struct_t*)typedef_->Type;
                    if (structPtr->Identifier.v == empty_identifier.v)
                    {
                        printIdentifier = typedef_->Identifier;
                    }
                    decl = (metac_declaration_t*)typedef_->Type;
                }
            }

            if (decl->Kind == decl_type_struct)
            {
                decl_type_struct_t* struct_ = (decl_type_struct_t*) decl;
                if (struct_->Identifier.v != empty_identifier.v)
                {
                    printIdentifier = struct_->Identifier;
                }
 //               MSGF("found struct : '%s'\n",
 //                   IdentifierPtrToCharPtr(&tmpParser.IdentifierTable, printIdentifier));

                compilerStruct = MetaCSemantic_doDeclSemantic(&tmpSema, struct_);
            }
        }

        MetaCSemantic_Handoff(&tmpSema,
                              (metac_sema_declaration_t**)&self->SemanticState.CompilerInterface,
                              &self->SemanticState);
        // FreeSema
        MetaCParser_Free(&tmpParser);
    }
}

void Repl_SwtichMode(repl_state_t* self)
{
    switch (self->ParseMode)
    {
    case parse_mode_max: assert(0);
    case parse_mode_file:
        self->Promt = ">File<";
        break;
    case parse_mode_token:
        self->Promt = "Token>";
        break;
    case parse_mode_decl:
        self->Promt = "Decl>";
        break;
    case parse_mode_expr:
        self->Promt = "Exp>";
        break;
    case parse_mode_stmt:
        self->Promt = "Stmt>";
        break;
    case parse_mode_preproc:
        self->Promt = "Preproc>";
        break;
    case parse_mode_ee:
        self->Promt = "EE>";
        break;
    case parse_mode_es:
        self->Promt = "ES>";
        break;
    case parse_mode_ss:
        self->Promt = "SS>";
        break;
    case parse_mode_ds:
        self->Promt = "DS>";
        break;
    case parse_mode_setvars:
        self->Promt = "SetVars>";
        break;
    }
}

void Repl_Init(repl_state_t* self)
{
    self->LPP.LexerState.Position = 0;
    self->LPP.LexerState.Line = 1;
    self->LPP.LexerState.Column = 1;
    self->ParseMode = parse_mode_ee;
    self->CompilerInterface = 0;

    self->SrcBuffer = 0;
    self->FreePtr = 0;
    self->SrcBufferLength = 0;

    self->Promt = ">";

    // init line lexer with more than the default
    // tokens
    metac_lpp_t* LPP = &self->LPP;

    // make sure we know our special identifiers


    MetaCLPP_Init(&self->LPP);
    MetaCSemantic_Init(&self->SemanticState, &LPP->Parser, 0);
    MetaCSemantic_PushNewScope(&self->SemanticState, scope_owner_module, 1);

    LPP->Lexer.Tokens =
        (metac_token_t*)malloc(128 * sizeof(metac_token_t));
    LPP->Lexer.TokenCapacity = 128;
    LPP->Lexer.LocationStorage.Locations =
        (metac_location_t*)malloc(128 * sizeof(metac_location_t));
    LPP->Lexer.LocationStorage.LocationCapacity = 128;


    MetaCPrinter_Init(&self->printer,
        &LPP->Parser.IdentifierTable,
        &LPP->Parser.StringTable);

    VariableStore_Init(&self->vstore, &LPP->Parser.IdentifierTable);

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

    metac_sema_expression_t* result = 0;

    ENQUEUE_TASK(result, MetaCSemantic_doExprSemantic_,
                 (&ctx->Repl->SemanticState), ctx->Exp, 0);

    task->TaskFlags |= Task_Waiting;
    // MSG("Just before yield\n");
    YIELD(WaitForExprSemantic);

    // MSGF("typeIndex.v: %x\n", result->TypeIndex.v);
    const char* type_str = TypeToChars(&ctx->Repl->SemanticState, result->TypeIndex);

    //MSGF("typeof(%s) = %s\n",
    //       MetaCPrinter_PrintExpression(&ctx->Repl->printer, ctx->Exp));
}
#endif

/// returns false if the repl is done running
bool Repl_Loop(repl_state_t* repl, repl_ui_context_t* context)
{
#ifndef NO_FIBERS
    worker_context_t* worker = CurrentWorker();
#endif
    const ui_interface_t uiInterface = context->UiInterface;
    struct ui_state_t* uiState = context->UiState;

LswitchMode:
    Repl_SwtichMode(repl);

    {
        repl->Line = uiInterface.GetInputLine(repl, uiState, &repl->LineSz);
        if (repl->Line)
        {
            TracyCMessage(repl->Line, repl->LineSz);
        }
    }

    if (repl->Line)
    {
        uint32_t line_length = strlen(repl->Line);
        if (*repl->Line == ':')
        {
            switch(repl->Line[1])
            {
            case 'q':
                return false;
            case 'f' :
            {
                repl->ParseMode = parse_mode_file;
                const char* filename = repl->Line + 3;
                MSG("querying fileStorage");
                // metac_file_storage_t* fs = Global_GetFileStorage(worker);
                // metac_file_ptr_t f = MetaCFileStorage_LoadFile(fs, filename);
            } break;
            case 'l' :
            {
                metac_lexer_t *fileLexer = &repl->FileLexer;
                MetaCLexer_Init(fileLexer);
                metac_lexer_state_t fileLexerState;
                repl->ParseMode = parse_mode_file;
                const char* filename = repl->Line + 3;
                MSGF("loading and lexing: '%s'\n", filename);
                FILE* fd = fopen(filename, "rb");
                if (!fd)
                {
                    perror(filename);
//                    MSGF(message, "%s/%s", get_current_dir_name(), filename);
                }
                else
                {
                    fseek(fd, 0, SEEK_END);
                    int32_t sz = cast(int32_t) ftell(fd);
                    fseek(fd, 0, SEEK_SET);

                    uint32_t estimatedTokenCount = (((sz / 5) + 128) & ~127);
                    if (fileLexer->TokenCapacity < estimatedTokenCount)
                    {
                        fileLexer->Tokens = (metac_token_t*)
                            malloc(sizeof(metac_token_t) * estimatedTokenCount);
                        fileLexer->TokenCount = 0;
                        fileLexer->TokenCapacity = estimatedTokenCount;
                        fileLexer->Tokens = (metac_token_t*)
                            malloc(sizeof(metac_token_t) * estimatedTokenCount);

                        fileLexer->LocationStorage.Locations = (metac_location_t*)
                            malloc(sizeof(metac_location_t) * estimatedTokenCount);
                        fileLexer->LocationStorage.LocationSize = 0;
                        fileLexer->LocationStorage.LocationCapacity = estimatedTokenCount;
                    }

                    repl->FreePtr = repl->SrcBuffer = calloc(1, sz + 4);
                    repl->SrcBufferLength = sz;
                    fread((void*)repl->SrcBuffer, 1, sz, fd);
                    repl->ParseMode = parse_mode_file;

                    fileLexerState.Position = 0;
                    fileLexerState.Line = 1;
                    fileLexerState.Column = 1;
                    fileLexerState.Size = sz;
                }
                break;
            }
            case 't' :
                repl->ParseMode = parse_mode_token;
                goto LswitchMode;
            case 'd' :
                switch (repl->Line[2])
                {
                default:
                    repl->ParseMode = parse_mode_decl;
                    goto LswitchMode;

                 case 's':
                    repl->ParseMode = parse_mode_ds;
                    goto LswitchMode;
                }

            case 'v' :
                repl->ParseMode = parse_mode_setvars;
                goto LswitchMode;
            case 'e' :
                switch(repl->Line[2])
                {
                default:
                    repl->ParseMode = parse_mode_expr;
                    goto LswitchMode;
                case 'e':
                    repl->ParseMode = parse_mode_ee;
                    goto LswitchMode;
                case 's':
                    repl->ParseMode = parse_mode_es;
                    goto LswitchMode;
                }
            case 's' :
                switch (repl->Line[2])
                {
                default:
                    repl->ParseMode = parse_mode_stmt;
                    goto LswitchMode;
                case 's':
                    repl->ParseMode = parse_mode_ss;
                    goto LswitchMode;
                }
                break;
            case 'p' :
            {
                repl->ParseMode = parse_mode_preproc;
                goto LswitchMode;
            } break;

            case 'h' :
            {
                HelpMessage(uiInterface, uiState);
                goto LnextLine;
            } break;
            default :
                MSGF("Command :%c unknown type :h for help\n", *(repl->Line + 1));
                break;
            }
        }
        else if (repl->Line[0] == '.')
        {
            if (line_length == sizeof("scope")
             && 0 == memcmp(repl->Line + 1, "scope", strlen("scope")))
            {
                if (repl->SemanticState.CurrentScope)
                {
                    metac_scope_table_t* table = &repl->SemanticState.CurrentScope->ScopeTable;
                    uint32_t nMembers = table->SlotsUsed;

                    for(uint32_t slotIdx = 0, memberIdx = 0; memberIdx < nMembers; slotIdx++)
                    {
                        metac_scope_table_slot_t slot = table->Slots[slotIdx];
                        if (slot.Hash)
                        {
                            MSGF("Member [%u] : %s\n", memberIdx++, MetaCPrinter_PrintSemaNode(&repl->printer, &repl->SemanticState, slot.Node));
                        }
                    }
                }
                goto LnextLine;
            }
        }

        if (repl->ParseMode != parse_mode_file)
        {
            repl->SrcBuffer = (char*)repl->Line;
            repl->SrcBufferLength = line_length;
        }

        while (repl->SrcBufferLength > 0)
        {
            metac_token_t token;

            metac_expression_t* exp;
            metac_statement_t* stmt;
            metac_declaration_t* decl;

            uint32_t initalPosition = repl->LPP.LexerState.Position;
            switch(repl->ParseMode)
            {
            case parse_mode_max: break;
            case parse_mode_expr:
            {
                 exp =
                    MetaCLPP_ParseExpressionFromString(&repl->LPP, repl->Line);

                const char* str = MetaCPrinter_PrintExpression(&repl->printer, exp);
                MSGF("expr = %s\n", str);
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
                    MetaCLPP_ParsePreprocFromString(&repl->LPP, repl->Line, &buffer);

                if (directive == pp_eval)
                {
                    int res = MetaCPreProcessor_Eval(&repl->LPP.Preprocessor, &repl->LPP.Parser);
                    MSGF("result:%u\n", res);
                }
                else if (directive == pp_define)
                {

                    metac_preprocessor_define_ptr_t define =
                        MetaCPreProcessor_ParseDefine(&repl->LPP.Preprocessor, &repl->LPP.Parser);
                }

                goto LnextLine;
            }

            case parse_mode_ee:
            {
                exp =
                    MetaCLPP_ParseExpressionFromString(&repl->LPP, repl->Line);

                const char* str = MetaCPrinter_PrintExpression(&repl->printer, exp);

                metac_sema_expression_t* result =
                    MetaCSemantic_doExprSemantic(&repl->SemanticState, exp, 0);

                metac_expression_t printExpStorage;

                metac_sema_expression_t eval_exp = evalWithVariables(result, &repl->vstore);
                result = &eval_exp;

                const char* result_str;
                if (eval_exp.Kind == exp_type)
                {
                    result_str = MetaCPrinter_PrintSemaNode(&repl->printer, &repl->SemanticState, &eval_exp);
                }
                else
                {
                    result_str = MetaCPrinter_PrintSemaNode(&repl->printer, &repl->SemanticState, &eval_exp);
                }
                MSGF("%s = %s\n", str, result_str);
                MetaCPrinter_Reset(&repl->printer);
                // XXX static and fixed size state like _ReadContext
                // should go away soon.
                _ReadContextSize = 0;
                goto LnextLine;
            }
            case parse_mode_es:
            {
                exp =
                    MetaCLPP_ParseExpressionFromString(&repl->LPP, repl->Line);

                const char* str = MetaCPrinter_PrintExpression(&repl->printer, exp);

                metac_sema_expression_t* result =
                    MetaCSemantic_doExprSemantic(&repl->SemanticState, exp, 0);

            Lcontinuation:
                {
                    MSGF("typeIndex.v: %x\n", result->TypeIndex.v);
                    const char* type_str = TypeToChars(&repl->SemanticState, result->TypeIndex);

                    metac_printer_t printer;
                    MetaCPrinter_InitSz(&printer,
                                        &repl->LPP.Lexer.IdentifierTable,
                                        &repl->LPP.Lexer.StringTable, 512);

                    MSGF("typeof(%s) = %s\n",
                           MetaCPrinter_PrintExpression(&repl->printer, exp), type_str);
                }
#if 0

//                EQUEUE_WITH_CONT(MetaCSemantic_doExprSemantic_, &continuation, &repl->sema, exp);

                do {
                    taskqueue_t* q = &CurrentWorker()->Queue;
                    task_t task = {0};
                    MetaCSemantic_doExprSemantic_task_context_t ctx = {&repl->SemanticState, exp};
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
                metac_declaration_t* decl = MetaCLPP_ParseDeclarationFromString(&repl->LPP, repl->Line);
                if (decl)
                {
                    metac_identifier_ptr_t idPtr = {0};
                    idPtr = IdentifierPtrFromDecl(decl);

                    if (idPtr.v == 0)
                    {
                        ERROR("declaration could not be handled only functions are supported for now\n");
                        goto LnextLine;
                    }

                    const char* idChars = IdentifierPtrToCharPtr(&repl->LPP.Parser.IdentifierTable, idPtr);
                    const uint32_t length = strlen(idChars);
                    uint32_t idHash = crc32c_nozero(~0, idChars, length);
                    uint32_t idKey = IDENTIFIER_KEY(idHash, length);
/*
                    metac_identifier_ptr_t dstoreId
                        = GetOrAddIdentifier(&dstore.Table, idKey, idChars);

                    if (decl->Kind == decl_function)
                    {
                        decl->decl_function.Identifier = dstoreId;
                        MSGF("Setting dStore ID: %u\n", dstoreId.v);
                    }
                    else if (decl->Kind == decl_variable)
                    {
                        decl->decl_variable.VarIdentifier = dstoreId;

                        //VariableStore_SetValueI32(&vstore, assignExp->E1, (int32_t)assignExp->E2->ValueI64);
                    }

                    DeclarationStore_SetDecl(&dstore, dstoreId, decl);
*/
                    MSGF("Registering %s [v=%u] in scope\n", idChars, idPtr.v);
                    MetaCSemantic_RegisterInScope(&repl->SemanticState, idPtr, decl);
                    goto LnextLine;
                }
                else
                {
                    metac_expression_t* assignExp = MetaCLPP_ParseExpressionFromString(&repl->LPP, repl->Line);
                    if (assignExp)
                    {
                        if (assignExp->Kind != exp_assign)
                        {
                            ERROR("You must write an expression of the from identifier = value");
                            goto LnextLine;
                        }

                        metac_sema_expression_t* ae =
                            MetaCSemantic_doExprSemantic(&repl->SemanticState, assignExp, 0);
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
                            ERROR("Semantic on assign exp failed\n");
                        }
                        // MetaCSemantic_Free(&sema);
                        goto LnextLine;
                    }
                    else
                    {
                        ERROR("Input did not parse as either an assign-expression or declaration\n");
                    }
                }
            } break;

            case parse_mode_stmt :
            {
                stmt = MetaCLPP_ParseStatementFromString(&repl->LPP, repl->Line);
                if (stmt)
                    MSGF("stmt = %s\n", MetaCPrinter_PrintStatement(&repl->printer, stmt));
                else
                   ERROR("couldn't parse statement\n");
                MetaCPrinter_Reset(&repl->printer);
                goto LnextLine;
            }

            case parse_mode_decl :
            {
                decl = MetaCLPP_ParseDeclarationFromString(&repl->LPP, repl->Line);
                if (decl)
                    MSGF("decl = %s\n", MetaCPrinter_PrintDeclaration(&repl->printer, decl));
                else
                    MSG("Couldn't parse Declaration\n");

                goto LnextLine;
            }

            case parse_mode_ds :
            {
                decl = MetaCLPP_ParseDeclarationFromString(&repl->LPP, repl->Line);
                if (decl)
                    MSGF("decl = %s\n", MetaCPrinter_PrintDeclaration(&repl->printer, decl));
                else
                    MSG("Couldn't parse Declaration\n");
#ifndef NO_FIBERS
                MetaCSemantic_doDeclSemantic_task_context_t ctx =
                {
                    &repl->SemanticState, decl, 0
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
                    ERROR("Couldn't Push\n");
                }
#else
                metac_sema_declaration_t* ds =
                    MetaCSemantic_doDeclSemantic(&repl->SemanticState, decl);
#endif
                goto LnextLine;
            }

            case parse_mode_file :
                goto LlexSrcBuffer;
            case parse_mode_token :

LlexSrcBuffer: {}
#if 1
                token = *MetaCLexerLexNextToken(&repl->LPP.Lexer, &repl->LPP.LexerState, repl->SrcBuffer, repl->SrcBufferLength);

                uint32_t eaten_chars = repl->LPP.LexerState.Position - initalPosition;
                const uint32_t token_length = MetaCTokenLength(token);
#if 1
                const uint32_t locPtr = token.LocationId;
                const metac_location_t loc = locPtr ? repl->LPP.Lexer.LocationStorage.Locations[locPtr - 4] : (metac_location_t){0};

                MSGF("read tokenType: %s {length: %d}\n Location: {Line: %d, Col: %d}",
                        MetaCTokenEnum_toChars(token.TokenType), token_length,
                        loc.StartLine, loc.StartColumn
                );

                if (token.TokenType == tok_identifier)
                {
                    MSGF("    %.*s\n", LENGTH_FROM_IDENTIFIER_KEY(token.Key),
                                         IdentifierPtrToCharPtr(&repl->LPP.Lexer.IdentifierTable, token.IdentifierPtr));
                }
                else if (token.TokenType == tok_char)
                {
                    MSGF("    '%.*s'\n", token.charLength, token.chars);
                }
                else if (token.TokenType == tok_char_uni)
                {
                    MSGF("    '\\U%.*s'\n", token.charLength, token.chars);
                }
                else if (token.TokenType == tok_uint)
                {
                    char buffer[21];
                    MSGF("    %s\n", u64tostr(token.ValueU64, buffer));
                }
                else if (token.TokenType == tok_string)
                {
                    MSGF("    \"%.*s\"\n", LENGTH_FROM_STRING_KEY(token.Key),
                                             IdentifierPtrToCharPtr(&repl->LPP.Lexer.StringTable, token.StringPtr));
                }
#endif

                repl->SrcBufferLength -= eaten_chars;
                repl->SrcBuffer += eaten_chars;
                MSGF("eaten_chars: %d\n", eaten_chars);
                if (!token_length)
                    break;
            }
        }

        if (repl->FreePtr)
        {
            free(repl->FreePtr);
            repl->FreePtr = 0;
            repl->ParseMode = parse_mode_token;
            goto LswitchMode;
        }
        repl->SrcBuffer = 0;
#endif
    }

LnextLine:
    {
        repl->LPP.LexerState.Line++;
        repl->LPP.LexerState.Column = 1;
        repl->LPP.Lexer.TokenCount = 0;
        repl->LPP.Lexer.LocationStorage.LocationSize = 0;
        repl->LPP.Parser.CurrentTokenIndex = 0;
    }

    return true;
}
repl_ui_context_t* g_uiContext = 0;


void Repl_Fiber(void)
{
    repl_ui_context_t* uiContext = g_uiContext;

    repl_state_t repl_;
    repl_state_t* repl = &repl_;
    Repl_Init(repl);

    ui_interface_t uiInterface = uiContext->UiInterface;
    struct ui_state_t* uiState = uiContext->UiState;

    // Presemantic_(repl);

    while (Repl_Loop(repl, uiContext) != false)
    {
#ifndef NO_FIBERS
        YIELD(ReplYield);
#endif
    }

    ERROR("Repl_Loop exited this should only happen on quit\n");
#ifndef NO_FIBERS
    aco_exit1(GET_CO());
#endif
}

void ReplStart(repl_ui_context_t* uiContext)
{
    g_uiContext = uiContext;
    Repl_Fiber();
}
