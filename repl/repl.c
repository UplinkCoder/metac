#define ACCEL ACCEL_TABLE

#include "../os/compat.h"
#ifndef NO_FIBERS
#  include "../os/metac_task.c"
#endif

#include "../parser/metac_parser_obj.c"
#include "../semantic/metac_semantic_obj.c"
#include "../driver/metac_driver.c"
#include "../os/bsr.h"
#include "../hash/crc32c.h"
#include "../semantic/handoff.c"
#include "../utils/int_to_str.c"
#include "../codegen/metac_codegen.c"
#include "../semantic/metac_type_table.h"
#include "../repl/completion_trie.c"
#include "repl.h"

#include <stdio.h>

const char* MetaCTokenEnum_toChars(metac_token_enum_t tok);

#define MSGF(FMT, ...) uiInterface.Message(uiState, FMT, __VA_ARGS__)
#define MSG(STR) MSGF(STR, 0);

#define ERRORMSGF(FMT, ...) uiInterface.ErrorMessage(uiState, FMT, __VA_ARGS__)
#define ERRORMSG(STR) ERRORMSGF(STR, 0)

void HelpMessage(ui_interface_t uiInterface, struct ui_state_t* uiState)
{
    MSG(
       "Type  :e for expression mode\n"
       "      :ee for evaluation mode\n"
       "      :es for expression semantic mode\n"
       "      :d for declaration mode\n"
       "      :ds for declaration semantic mode\n"
//       "      :v for varible mode (set vars for eval)\n"
       "      :s for statement mode\n"
       "      :ss for statement semantic mode\n"
       "      :t for token mode\n"
       "      :l Load and lex file\n"
       "      :heap to see the first 128 bytes of the heap\n"
       "      :p for preprocessor mode\n"
       "      :q to quit\n"
       "Pressing the Tab key auto-completes the the last word typed\n"
    );
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
            decl_variable_t* var = (decl_variable_t*) node;
            if (var->VarIdentifier.v && var->VarIdentifier.v != empty_identifier.v)
                TranslateIdentifier(DstTable, SrcTable, &var->VarIdentifier);
        } break;
        case decl_type:
        {
            decl_type_t* type = (decl_type_t*) node;
            if (type->TypeIdentifier.v && type->TypeIdentifier.v != empty_identifier.v)
                TranslateIdentifier(DstTable, SrcTable, &type->TypeIdentifier);
        } break;
        case decl_type_typedef:
        {
            decl_type_typedef_t* typedef_ = (decl_type_typedef_t*) node;
            if (typedef_->Identifier.v != empty_identifier.v)
            {
                TranslateIdentifier(DstTable, SrcTable, &typedef_->Identifier);
            }
        } break;
        case decl_type_struct:
        {
            decl_type_struct_t* struct_ = (decl_type_struct_t*) node;
            if (struct_->BaseIdentifier.v != empty_identifier.v)
                TranslateIdentifier(DstTable, SrcTable, &struct_->BaseIdentifier);
            if (struct_->Identifier.v != empty_identifier.v)
                TranslateIdentifier(DstTable, SrcTable, &struct_->Identifier);
        } break;
        case decl_type_enum:
        {
            decl_type_enum_t* enum_ = (decl_type_enum_t*) node;
            if (enum_->Identifier.v != empty_identifier.v)
            {
                TranslateIdentifier(DstTable, SrcTable, &enum_->Identifier);
            }
        } break;
        case decl_enum_member:
        {
            decl_enum_member_t* enumMember = (decl_enum_member_t*) node;
            if (enumMember->Name.v != empty_identifier.v)
            {
                TranslateIdentifier(DstTable, SrcTable, &enumMember->Name);
            }
        } break;

        default : break;
    }

    return 0;
}

#if 0
typedef struct find_stmt_context_t
{
    metac_printer_t* Printer;
    metac_stmt_kind_t Kind;
} find_stmt_context_t;

static inline int FindStmtCb(metac_node_t node, void* ctx)
{
    find_stmt_context_t* context =
        (find_stmt_context_t*) ctx;
    switch(cast(metac_stmt_kind_t)node->Kind)
    {
        case stmt_if:
        {
            stmt_if_t* stmt_if = cast(stmt_if_t*) node;
            MetaCPrinter_Reset(context->Printer);
            MetaCPrinter_PrintStmt(context->Printer, node);
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
    metac_sema_state_t* Sema;
} presemantic_context_t;


extern repl_ui_context_t* g_uiContext;
metac_type_aggregate_t* g_compilerInterface;

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
    else if (MetaCNode_IsDecl(node))
    {
        MetaCSemantic_doDeclSemantic(context->Sema, node);
    }
    else
    {
        // MSGF("Not a typedef: %s\n", MetaCNodeKind_toChars(node->Kind));
    }

    return 0;
}

void SeeIdentifier(const char* idStr, uint32_t key, repl_state_t* replCtx)
{
    CompletionTrie_Add(&replCtx->CompletionTrie, idStr, LENGTH_FROM_IDENTIFIER_KEY(key));
    // printf("WordCount: %d\n", replCtx->CompletionTrie.WordCount);
//    CompletionTrie_Print(&replCtx->CompletionTrie);
}

void AddIdentifierToCompletion(repl_state_t* self, const char* idString)
{
    uint32_t len = (uint32_t) strlen(idString);
    uint32_t hash = crc32c(~0, idString, len);
    uint32_t key = IDENTIFIER_KEY(hash, len);
    SeeIdentifier(idString, key, self);
}

void Presemantic_(repl_state_t* self)
{
    metac_type_aggregate_t* compilerStruct = 0;

    metac_alloc_t PresemanticAlloc;
    Allocator_Init(&PresemanticAlloc, 0);

    repl_ui_context_t* uiContext = g_uiContext;
    ui_interface_t uiInterface = *uiContext->UiInterface;
    struct ui_state_t* uiState = uiContext->UiState;

    read_result_t fCompilterInterface =
        ReadFileAndZeroTerminate("metac_compiler_interface.h");
    if (!fCompilterInterface.FileContent0)
        fCompilterInterface =
            ReadFileAndZeroTerminate("compiler_intrinsics/metac_compiler_interface.h");
    if (!fCompilterInterface.FileContent0)
        fCompilterInterface =
            ReadFileAndZeroTerminate("../compiler_intrinsics/metac_compiler_interface.h");
    if (!fCompilterInterface.FileContent0)
        fCompilterInterface =
            ReadFileAndZeroTerminate("/home/uplink/dev/metac/compiler_intrinsics/metac_compiler_interface.h");

    if (fCompilterInterface.FileContent0)
    {
        metac_lexer_t tmpLexer;
        metac_parser_t tmpParser;
        metac_preprocessor_t preProc;
        decl_array_t decls = {0};
        {
            MetaCLexer_Init(&tmpLexer, &PresemanticAlloc);

            LexFile(&tmpLexer, "metac_compiler_interface.h",
                    fCompilterInterface.FileContent0,
                    fCompilterInterface.FileLength);
            AddIdentifierToCompletion(self, "Compiler");

            MetaCPreProcessor_Init(&preProc, &tmpLexer, &PresemanticAlloc, 0, 0);
            MetaCParser_InitFromLexer(&tmpParser, &tmpLexer, &PresemanticAlloc);
            tmpParser.Preprocessor = &preProc;
#if 1
            {
                identifier_callback_t cb;
                cb.Ctx = (void*)self;
                cb.FuncP = cast(identifier_cb_t)&SeeIdentifier;
                tmpParser.IdentifierCallbacks[0] = cb;
                tmpParser.IdentifierCallbacksCount = 1;
            }
#endif
            ParseFile(&tmpParser, "metac_compiler_interface.h", &decls);
        }
        MetaCLexer_Free(&tmpLexer);

        presemantic_context_t presemanticContext = {
            sizeof(presemantic_context_t),
            crc32c_nozero(~0, "Presemantic", sizeof("Presemantic") - 1),
            &self->SemanticState
        };

        identifier_translation_context_t transIdCtx =
        {
            crc32c(~0, "TranslateIdentifiers", sizeof("TranslateIdentifiers") - 1),
            &tmpParser.IdentifierTable,
            &self->LPP.Parser.IdentifierTable
        };

        for(uint32_t i = 0;
            i < decls.Length;
            i++)
        {
            MetaCNode_TreeWalk_Real(METAC_NODE(decls.Ptr[i]), TranslateIdentifiers, &transIdCtx);

            MetaCNode_TreeWalk_Real(METAC_NODE(decls.Ptr[i]), Presemantic, &presemanticContext);
        }

        for(uint32_t i = 0;
            i < decls.Length;
            i++)
        {
            metac_identifier_ptr_t printIdentifier = {0};

            metac_decl_t* decl = decls.Ptr[i];

            if (METAC_NODE(decl) == emptyNode)
                continue;

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
                    decl = (metac_decl_t*)typedef_->Type;
                }
            }

            if (decl->Kind == decl_type_struct)
            {
                const char* structNameStr = 0;
                decl_type_struct_t* struct_ = (decl_type_struct_t*) decl;
                metac_printer_t printer;

                MetaCPrinter_Init(&printer,
                    self->SemanticState.ParserIdentifierTable, self->SemanticState.ParserStringTable, 0);
                MSGF("%s\n", MetaCPrinter_PrintNode(&printer, METAC_NODE(struct_), 0));

                if (struct_->Identifier.v != empty_identifier.v)
                {
                    printIdentifier = struct_->Identifier;
                }
                else
                {
                    // struct_->Identifier = printIdentifier;
                }
                structNameStr =
                    IdentifierPtrToCharPtr(self->SemanticState.ParserIdentifierTable, printIdentifier);
                xprintf("structNameStr: '%s'\n", structNameStr);
                if (printIdentifier.v
                 && printIdentifier.v != empty_identifier.v
                 && 0 == strcmp("metac_compiler_t", structNameStr))
                {
                    compilerStruct = (metac_type_aggregate_t*)
                        MetaCSemantic_doDeclSemantic(&self->SemanticState, struct_);
                    xprintf("compilerStruct: %s\n",
                        MetaCPrinter_PrintSemaNode(&printer, &self->SemanticState, cast(metac_node_t)compilerStruct));
                    self->SemanticState.CompilerInterface = compilerStruct;
                    g_compilerInterface = self->SemanticState.CompilerInterface;
                    {
                        sema_decl_variable_t fakeDotStruct = {};

                        metac_type_index_t compilerTypeIndex = self->SemanticState.CompilerInterface->TypeIndex;
                        fakeDotStruct.Kind = decl_variable;
                        metac_type_index_t compilerPtrTypeIndex = MetaCSemantic_GetPtrTypeOf(&self->SemanticState, compilerTypeIndex);
                        fakeDotStruct.TypeIndex = compilerPtrTypeIndex;
                        //fakeDotStruct.VarIdentifier = expr->E1->IdentifierPtr;
                        //TODO implement metaCCodegen_RegisterExternal
                        fakeDotStruct.Storage.v = STORAGE_V(storage_external, 0);
                        fakeDotStruct.VarIdentifier = GetOrAddIdentifier(&self->SemanticState.SemanticIdentifierTable, Compiler_key, "Compiler");
                        self->SemanticState.CompilerVariable = fakeDotStruct;
                    }
                }
            }
        }


        // FreeSema
        MetaCParser_Free(&tmpParser);
    }
    // Allocator_Remove
    Debug_RemoveAllocator(g_DebugServer, &PresemanticAlloc);
}


void Repl_SwtichMode(repl_state_t* self)
{
    switch (self->ParseMode)
    {
    case repl_mode_max: assert(0);
    case repl_mode_lex_file:
        self->Promt = ">File<";
        break;
    case repl_mode_token:
        self->Promt = "Token>";
        break;
    case repl_mode_decl:
        self->Promt = "Decl>";
        break;
    case repl_mode_expr:
        self->Promt = "Exp>";
        break;
    case repl_mode_stmt:
        self->Promt = "Stmt>";
        break;
    case repl_mode_preproc:
        self->Promt = "Preproc>";
        break;
    case repl_mode_ee:
        self->Promt = "EE>";
        break;
    case repl_mode_e2:
        self->Promt = "E2>";
        break;
    case repl_mode_es:
        self->Promt = "ES>";
        break;
    case repl_mode_ss:
        self->Promt = "SS>";
        break;
    case repl_mode_ds:
        self->Promt = "DS>";
        break;
    case repl_mode_setvars:
        self->Promt = "SetVars>";
        break;
    }
}

void Repl_Init(repl_state_t* self)
{
    metac_lpp_t* LPP = &self->LPP;

    self->LPP.LexerState.Position = 0;
    self->LPP.LexerState.Line = 1;
    self->LPP.LexerState.Column = 1;
    self->ParseMode = repl_mode_ee;
    // self->CompilerInterface = 0;
    self->SrcBuffer = 0;
    self->FreePtr = 0;
    self->SrcBufferLength = 0;

    self->Promt = ">";

    AllocDefaultHeap(&self->Heap);


    // init line lexer with more than the default
    // tokens

    // make sure we know our special identifiers
    Allocator_Init(&self->Allocator, 0);

    MetaCLPP_Init(&self->LPP, &self->Allocator, &self->FileStorage);
    MetaCSemantic_Init(&self->SemanticState, &LPP->Parser, 0);
    MetaCSemantic_PushNewScope(&self->SemanticState, scope_owner_module, cast(metac_node_t)cast(intptr_t)1);

    LPP->Lexer.TokenCapacity = 128;
    LPP->Lexer.Tokens =
        Allocator_Calloc(&self->Allocator, metac_token_t, LPP->Lexer.TokenCapacity);
    LPP->Lexer.LocationStorage.Locations =
        Allocator_Calloc(&self->Allocator, metac_location_t, LPP->Lexer.TokenCapacity);
    LPP->Lexer.LocationStorage.LocationCapacity = LPP->Lexer.TokenCapacity;

    MetaCPrinter_Init(&self->printer,
        &LPP->Parser.IdentifierTable,
        &LPP->Parser.StringTable, &self->Allocator);

    Debug_CurrentIdentifierTable(g_DebugServer, self->SemanticState.ParserIdentifierTable);
    Debug_CurrentScope(g_DebugServer, self->SemanticState.CurrentScope);

    Allocator_Init(&self->CompletionAlloc, 0);
    CompletionTrie_Init(&self->CompletionTrie, &self->CompletionAlloc);
    //VariableStore_Init(&self->vstore, &LPP->Parser.IdentifierTable);
}

#ifndef NO_FIBERS
typedef struct MetaCRepl_ExprSemantic_context_t
{
    repl_state_t* Repl;
    metac_expr_t* Exp;
} MetaCRepl_ExprSemantic_context_t;

void MetaCRepl_ExprSemantic_Task(task_t* task)
{
    MetaCRepl_ExprSemantic_context_t* ctx =
        (MetaCRepl_ExprSemantic_context_t*)
            task->Context;

    metac_sema_expr_t* result = 0;

    ENQUEUE_TASK(result, MetaCSemantic_doExprSemantic_,
                 (&ctx->Repl->SemanticState), ctx->Exp, 0);

    U32(task->TaskFlags) |= Task_Waiting;
    // MSG("Just before yield\n");
    YIELD(WaitForExprSemantic);

    // MSGF("typeIndex.v: %x\n", result->TypeIndex.v);
    const char* type_str = TypeToChars(&ctx->Repl->SemanticState, result->TypeIndex);

    //MSGF("typeof(%s) = %s\n",
    //       MetaCPrinter_PrintExpr(&ctx->Repl->printer, ctx->Exp));
}
#endif

metac_identifier_ptr_t IdentifierPtrFromDecl(metac_decl_t* decl)
{
    metac_identifier_ptr_t idPtr = {0};

    switch(decl->Kind)
    {
        case decl_function:
        {
            decl_function_t* f = cast(decl_function_t*) decl;
            idPtr = f->Identifier;
            break;
        }
        case decl_variable:
        {
            decl_variable_t* v = cast(decl_variable_t*) decl;
            idPtr = v->VarIdentifier;
            break;
        }
        default : assert(0);
    }

    return idPtr;

}

metac_identifier_ptr_t IdentifierPtrFromSemaDecl(metac_sema_decl_t* decl)
{
    metac_identifier_ptr_t idPtr = {0};

    switch(decl->Kind)
    {
        case decl_function:
        {
            sema_decl_function_t* f = cast(sema_decl_function_t*) decl;
            idPtr = f->Identifier;
            break;
        }
        case decl_variable:
        {
            sema_decl_variable_t* v = cast(sema_decl_variable_t*) decl;
            idPtr = v->VarIdentifier;
            break;
        }
        default : assert(0);
    }

    return idPtr;

}
#define COMMAND_FN(NAME) \
    void NAME (repl_state_t* repl, ui_interface_t uiInterface, struct ui_state_t* uiState)
typedef COMMAND_FN((*command_fn_t));


#ifndef NO_FIBERS
static void Repl_doDeclSemantic_cont(MetaCSemantic_doDeclSemantic_task_context_t* ctx)
{
//    ARENA_ARRAY_ADD(ctx->Sema->Globals, ctx->Result);
}
#endif

/// returns false if the repl is done running
bool Repl_Loop(repl_state_t* repl, repl_ui_context_t* context)
{
#ifndef NO_FIBERS
    worker_context_t* worker = CurrentWorker();
#endif
    const ui_interface_t uiInterface = *context->UiInterface;
    struct ui_state_t* uiState = context->UiState;

LswitchMode:
    Repl_SwtichMode(repl);

    {
        repl->Line = uiInterface.GetInputLine(uiState, repl, (uint32_t*)&repl->LineSz);
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
                if (uiInterface.GetFileSystem)
                {
                    metac_filesystem_t* fs = uiInterface.GetFileSystem(uiState);

                    repl->ParseMode = repl_mode_lex_file;
                    const char* filename = repl->Line + 3;
                       MSG("querying fileStorage\n");
                    // metac_file_storage_t* fs = Global_GetFileStorage(worker);
                    // metac_file_ptr_t f = MetaCFileStorage_LoadFile(fs, filename);
                }
                else
                {
                    MSG("No filesystem in interface\n");
                }
                goto LnextLine;
            } break;
            case 'c' :
            {
                uiInterface.Clear(uiState);
            } goto LnextLine;
            case 'l' :
            {
                repl->ParseMode = repl_mode_lex_file;
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
                    metac_lexer_t *fileLexer = &repl->FileLexer;
                    MetaCLexer_Init(fileLexer, &repl->Allocator);
                    metac_lexer_state_t fileLexerState = {};

                    fseek(fd, 0, SEEK_END);
                    int32_t sz = cast(int32_t) ftell(fd);
                    fseek(fd, 0, SEEK_SET);

                    uint32_t estimatedTokenCount = (((sz / 4) + 128) & ~127);
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
                    repl->SrcBuffer = (char*)calloc(1, sz + 4);
                    repl->FreePtr = (void*)repl->SrcBuffer;
                    repl->SrcBufferLength = sz;
                    fread((void*)repl->SrcBuffer, 1, sz, fd);
                    repl->ParseMode = repl_mode_lex_file;

                    fileLexerState.Position = 0;
                    fileLexerState.Line = 1;
                    fileLexerState.Column = 1;
                    fileLexerState.Size = sz;
                }
                break;
            }
            case 't' :
                repl->ParseMode = repl_mode_token;
                goto LswitchMode;
            case 'd' :
                if (0 == strcmp("ate", repl->Line + 2))
                {
                    MSG("Build on " __DATE__ " " __TIME__ "\n");
                }
                else switch (repl->Line[2])
                {
                default:
                    repl->ParseMode = repl_mode_decl;
                    goto LswitchMode;

                 case 's':
                    repl->ParseMode = repl_mode_ds;
                    goto LswitchMode;
                }
            case 'g' :
                if (0 == strcmp("lobals", repl->Line + 2))
                {
                    for(uint32_t i = 0; i < repl->SemanticState.GlobalsCount; i++)
                    {
                        metac_sema_decl_t* global = repl->SemanticState.Globals[i];
                        if (global->Kind == decl_variable)
                        {
                            sema_decl_variable_t var = global->sema_decl_variable;
                            metac_sema_expr_t* value = var.VarInitExpr;
                            metac_printer_t debugPrinter;
                            const char* typeString;
                            const char* valueString = "NULL";
                            metac_type_t typeNode;

                            MetaCPrinter_Init(&debugPrinter,
                                repl->SemanticState.ParserIdentifierTable,
                                repl->SemanticState.ParserStringTable,
                                0
                            );

                            typeNode = NodeFromTypeIndex(&repl->SemanticState, var.TypeIndex);

                            typeString = MetaCPrinter_PrintSemaNode(
                                &debugPrinter, &repl->SemanticState, METAC_NODE(typeNode)
                            );

                            if (METAC_NODE(value) != emptyNode)
                            {
                                valueString = MetaCPrinter_PrintSemaNode(
                                    &debugPrinter,
                                    &repl->SemanticState,
                                    METAC_NODE(value)
                                );
                            }
                            {
                                metac_identifier_ptr_t idPtr =
                                    IdentifierPtrFromSemaDecl(cast(metac_sema_decl_t*)&var);
                                const char* nameString =
                                    IdentifierPtrToCharPtr(repl->SemanticState.ParserIdentifierTable, idPtr);
                                MSGF ("Global %s %s = %s\n", typeString, nameString, valueString);
                                MetaCPrinter_Free(&debugPrinter);
                            }

                        }
                    }
                }
                goto LnextLine;
/*
            case 'v' :
                repl->ParseMode = repl_mode_setvars;
                goto LswitchMode;
*/
            case 'e' :
                switch(repl->Line[2])
                {
                default:
                    repl->ParseMode = repl_mode_expr;
                    goto LswitchMode;
                case 'e':
                    repl->ParseMode = repl_mode_ee;
                    goto LswitchMode;
                case '2':
                    repl->ParseMode = repl_mode_e2;
                    goto LswitchMode;
                case 's':
                    repl->ParseMode = repl_mode_es;
                    goto LswitchMode;
                }
            case 's' :
                switch (repl->Line[2])
                {
                default:
                    repl->ParseMode = repl_mode_stmt;
                    goto LswitchMode;
                case 's':
                    repl->ParseMode = repl_mode_ss;
                    goto LswitchMode;
                }
                break;
            case 'p' :
            {
                repl->ParseMode = repl_mode_preproc;
                goto LswitchMode;
            } break;

            case 'h' :
            {
                if (0 == strcmp("eap", repl->Line + 2))
                {
                    uint8_t* h = repl->Heap.heapData;
                    char line[14 + (16 * 3)];
                    for(uint32_t i = 0; i < repl->Heap.heapSize;)
                    {
                        int p = 0;
                        p += sprintf(line + p, "%p:", h + i);
                        for(uint32_t j = 0; j < 4; j++)
                        {
                            p += sprintf(line + p, " %.2x %.2x %.2x %.2x",
                                h[i + 0], h[i + 1], h[i + 2], h[i + 3]);
                            i += 4;
                        }
                        puts(line);

                    }
                    goto LnextLine;
                }
                else
                {
                    HelpMessage(uiInterface, uiState);
                    goto LnextLine;
                }
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
                            xprintf("Kind %s\n", MetaCNodeKind_toChars(slot.Node->Kind));
                            MSGF("Member [%u] : %s\n", memberIdx++, MetaCPrinter_PrintSemaNode(&repl->printer, &repl->SemanticState, slot.Node));
                        }
                    }
                }
                goto LnextLine;
            }
        }

        if (repl->ParseMode != repl_mode_lex_file)
        {
            repl->SrcBuffer = (char*)repl->Line;
            repl->SrcBufferLength = line_length;
        }

        while (repl->SrcBufferLength > 0)
        {
            metac_token_t token;

            metac_expr_t* exp;
            metac_stmt_t* stmt;
            metac_decl_t* decl;

            uint32_t initalPosition = repl->LPP.LexerState.Position;
            switch(repl->ParseMode)
            {
            case repl_mode_max: break;
            case repl_mode_expr:
            {
#ifdef OLD_PARSER
                 exp =
                    MetaCLPP_ParseExprFromString(&repl->LPP, repl->Line);
#else
                 exp =
                    MetaCLPP_ParseExpr2FromString(&repl->LPP, repl->Line);
#endif
                const char* str = MetaCPrinter_PrintExpr(&repl->printer, exp);
                MSGF("expr = %s\n", str);
                MetaCPrinter_Reset(&repl->printer);
                goto LnextLine;
            }
            case repl_mode_e2:
            {
                exp = MetaCLPP_ParseExpr2FromString(&repl->LPP, repl->Line);
                if (exp)
                {
                    MSGF("expr = %s\n", MetaCPrinter_PrintExpr(&repl->printer, exp));
                }
                else
                {
                    MSG("Couldn't parse expression\n");
                }
                MetaCPrinter_Reset(&repl->printer);
            } goto LnextLine;

            case repl_mode_preproc:
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
                else if (directive == pp_include)
                {
                    MetaCPreProcessor_Include(&repl->LPP.Preprocessor, &repl->LPP.Parser);
                }
                else if (directive == pp_pragma)
                {
                    xprintf("got a pragma\n");
                }

                goto LnextLine;
            }

            case repl_mode_ee:
            {
#ifdef OLD_PARSER
                exp =
                    MetaCLPP_ParseExprFromString(&repl->LPP, repl->Line);
#else
                exp =
                    MetaCLPP_ParseExpr2FromString(&repl->LPP, repl->Line);
#endif
                if (exp)
                {
                    const char* str = MetaCPrinter_PrintExpr(&repl->printer, exp);

                    metac_sema_expr_t* result = 0;

                    result =
                        MetaCSemantic_doExprSemantic(&repl->SemanticState, exp, 0);

                    // ConstantFold_SubExps(&repl->SemanticState, result);

                    if (!result)
                        goto LnextLine;
                    {
                        metac_sema_expr_t eval_exp = EvaluateExpr(&repl->SemanticState, result, &repl->Heap);
                        result = &eval_exp;

                        const char* result_str;
                        if (eval_exp.Kind == expr_type)
                        {
                            result_str = MetaCPrinter_PrintSemaNode(&repl->printer, &repl->SemanticState, cast(metac_node_t)&eval_exp);
                        }
                        else
                        {
                            result_str = MetaCPrinter_PrintSemaNode(&repl->printer, &repl->SemanticState, cast(metac_node_t)&eval_exp);
                        }
                        MSGF("%s = %s\n", str, result_str);
                        MetaCPrinter_Reset(&repl->printer);
                    }
                }
                goto LnextLine;
            }

            case repl_mode_es:
            {
#ifdef OLD_PARSER
                 exp =
                    MetaCLPP_ParseExprFromString(&repl->LPP, repl->Line);
#else
                 exp =
                    MetaCLPP_ParseExpr2FromString(&repl->LPP, repl->Line);
#endif

                const char* str = MetaCPrinter_PrintExpr(&repl->printer, exp);
                metac_sema_expr_t* result =
                    MetaCSemantic_doExprSemantic(&repl->SemanticState, exp, 0);

            Lcontinuation:
                {
                    MSGF("typeIndex.v: %x\n", result->TypeIndex.v);
                    const char* type_str = TypeToChars(&repl->SemanticState, result->TypeIndex);

                    MSGF("typeof(%s) = %s\n", str, type_str);
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

            case repl_mode_setvars :
            {
#if 0
                metac_decl_t* decl = MetaCLPP_ParseDeclFromString(&repl->LPP, repl->Line);
                if (decl)
                {
                    metac_identifier_ptr_t idPtr = {0};
                    idPtr = IdentifierPtrFromDecl(decl);

                    if (idPtr.v == 0)
                    {
                        ERRORMSG("declaration could not be handled only functions are supported for now\n");
                        goto LnextLine;
                    }

                    const char* idChars = IdentifierPtrToCharPtr(&repl->LPP.Parser.IdentifierTable, idPtr);
                    const uint32_t length = strlen(idChars);
                    uint32_t idHash = crc32c_nozero(~0, idChars, length);
                    uint32_t idKey = IDENTIFIER_KEY(idHash, length);
                    MSGF("Registering %s [v=%u] in scope\n", idChars, idPtr.v);
                    MetaCSemantic_RegisterInScope(&repl->SemanticState, idPtr, METAC_NODE(decl));
                    goto LnextLine;
                }
                else
                {
#ifdef OLD_PARSER
                 metac_expr_t* assignExp =
                    MetaCLPP_ParseExprFromString(&repl->LPP, repl->Line);
#else
                 metac_expr_t* assignExp =
                    MetaCLPP_ParseExpr2FromString(&repl->LPP, repl->Line);
#endif

                    if (assignExp)
                    {
                        if (assignExp->Kind != expr_assign)
                        {
                            ERRORMSG("You must write an expression of the from identifier = value");
                            goto LnextLine;
                        }

                        metac_sema_expr_t* ae =
                            MetaCSemantic_doExprSemantic(&repl->SemanticState, assignExp, 0);
                        if (ae)
                        {
                            if (ae->E1->Kind == expr_identifier)
                            {
                                assert(0);
                            }
                            assert(ae->E2->Kind == expr_signed_integer);

                            // VariableStore_SetValueI32(&repl->vstore, ae->E1, (int32_t)ae->E2->ValueI64);
                        }
                        else
                        {
                            ERRORMSG("Semantic on assign exp failed\n");
                        }
                        // MetaCSemantic_Free(&sema);
                        goto LnextLine;
                    }
                    else
                    {
                        ERRORMSG("Input did not parse as either an assign-expression or declaration\n");
                    }
                }
#endif
            } break;

            case repl_mode_stmt :
            {
                stmt = MetaCLPP_ParseStmtFromString(&repl->LPP, repl->Line);
                if (stmt)
                    MSGF("stmt = %s\n", MetaCPrinter_PrintStmt(&repl->printer, stmt));
                else
                   ERRORMSG("Couldn't parse statement\n");
                MetaCPrinter_Reset(&repl->printer);
                goto LnextLine;
            }

            case repl_mode_ss :
            {
                stmt = MetaCLPP_ParseStmtFromString(&repl->LPP, repl->Line);
                /*
                if (stmt)
                    MSGF("stmt = %s\n", MetaCPrinter_PrintStmt(&repl->printer, stmt));
                else
                   ERRORMSG("Couldn't parse statement\n");
                */
                if (stmt)
                {
                    metac_sema_stmt_t* semaStmt =
                        MetaCSemantic_doStmtSemantic(&repl->SemanticState, stmt);
                    MSGF("stmt = %s\n",
                        MetaCPrinter_PrintSemaNode(&repl->printer, &repl->SemanticState, METAC_NODE(semaStmt)));
                }
                else
                {
                   ERRORMSG("Couldn't parse statement\n");
                }
                MetaCPrinter_Reset(&repl->printer);
                goto LnextLine;
            }

            case repl_mode_decl :
            {
                decl = MetaCLPP_ParseDeclFromString(&repl->LPP, repl->Line);
                if (decl)
                    MSGF("decl = %s\n", MetaCPrinter_PrintDecl(&repl->printer, decl));
                else
                    MSG("Couldn't parse declaration\n");

                goto LnextLine;
            }

            case repl_mode_ds :
            {
                metac_sema_decl_t* ds;
                decl = MetaCLPP_ParseDeclFromString(&repl->LPP, repl->Line);
                if (decl)
                    MSGF("decl = %s\n", MetaCPrinter_PrintDecl(&repl->printer, decl));
                else
                    ERRORMSG("Couldn't parse declaration\n");
                decl->StorageClass = storageclass_global;

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
                U32(DeclSemaTask.TaskFlags) |= Task_Continuation_Func;
                DeclSemaTask.ContinuationFunc = cast(void (*)(void*)) Repl_doDeclSemantic_cont;

                worker_context_t* replWorker = CurrentWorker();
                taskqueue_t* q = &replWorker->Queue;
                ORIGIN(DeclSemaTask.Origin);
                uint32_t taskId = TaskQueue_Push(q, &DeclSemaTask);

                if (taskId == 0)
                {
                    ERRORMSG("Couldn't Push\n");
                }
#else
                ds = MetaCSemantic_doDeclSemantic(&repl->SemanticState, decl);
#endif
                goto LnextLine;
            }

            case repl_mode_lex_file :
                goto LlexSrcBuffer;
            case repl_mode_token :

LlexSrcBuffer: {}
#if 1
                token = *MetaCLexerLexNextToken(&repl->LPP.Lexer, &repl->LPP.LexerState, repl->SrcBuffer, repl->SrcBufferLength);

                uint32_t eaten_chars = repl->LPP.LexerState.Position - initalPosition;
                const uint32_t token_length = MetaCTokenLength(token);
#if 1
                const metac_location_ptr_t locPtr = token.LocationId;
                const static metac_location_t zeroLoc = {0};
                const metac_location_t loc = locPtr.v ? repl->LPP.Lexer.LocationStorage.Locations[locPtr.v - 4] : zeroLoc;

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
            repl->ParseMode = repl_mode_token;
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

completion_list_t CompleteCommand (repl_state_t* repl, const char *input, uint32_t inputLength)
{
    completion_list_t result = {0};
    assert(input[0] == ':');

    for(uint32_t i = 0; i < 1; i++)
    {

    }

    switch(input[1])
    {

    }

    return result;
}

typedef struct completion_state_t
{
    ARENA_ARRAY(const char*, Completions)
}  completion_state_t;

void CollectCompletionsCb(const char* completionString, uint32_t length,
                          completion_state_t* userCtx)
{
    char* entry = Allocator_Calloc(userCtx->CompletionsAlloc, char, length + 1);
    memcpy(entry, completionString, length);
    entry[length] = '\0';
    ARENA_ARRAY_ADD(userCtx->Completions, entry);
    //printf("%.*s\n", (int) length, completionString);
}

completion_list_t ReplComplete (repl_state_t* repl, const char *input, int32_t inputLength)
{
    completion_list_t result = {0};
    const char* lastWord = 0;
    uint32_t lastWordLength = 0;

    if (!inputLength)
        return result;

    if (input[0] == ':')
    {
        return CompleteCommand(repl, input, inputLength);
    }

    // let's collect a maximum of 512 Completions.

    {
        int32_t i;
        for(i = inputLength - 1;; i--)
        {
            char c = input[i];
            if (!IsIdentifierChar(c) || (i < 0))
            {
                i++;
                break;
            }
        }

        lastWordLength = inputLength - i;
        lastWord = input + i;

        result.BeforeCompletion = input;
        result.BeforeCompletionLength = lastWord - input;
    }

    {
        uint32_t originalLastWordLength = lastWordLength;
        completion_trie_node_t* PrefixNode =
            CompletionTrie_FindLongestMatchingPrefix(&repl->CompletionTrie, lastWord, &lastWordLength);
        uint32_t matchedPrefixLength = originalLastWordLength - lastWordLength;

        uint32_t n = PrefixNode - repl->CompletionTrie.Nodes;

        completion_state_t completionState;

        STACK_ARENA_ARRAY(const char*, completions, 64, &repl->CompletionAlloc)

        //CompletionTrie_Print(&repl->CompletionTrie, n, lastWord, stdout);

        ARENA_ARRAY_REFCOPY(completions, completionState.Completions);

        CompletionTrie_Collect(&repl->CompletionTrie, n,
                               lastWord, matchedPrefixLength,
                               (collect_cb_t)CollectCompletionsCb, &completionState);

        result.CompletionsLength = completionState.CompletionsCount;
        result.Completions = Allocator_Calloc(&repl->Allocator, char*, result.CompletionsLength);

        memcpy(result.Completions, completions, result.CompletionsLength * sizeof(char*));
    }

    return result;
}



void Repl_Fiber(void)
{
    repl_state_t repl_ = {repl_mode_ee};
    repl_state_t* repl = &repl_;

    repl_ui_context_t* uiContext = g_uiContext;
    ui_interface_t uiInterface = *uiContext->UiInterface;
    struct ui_state_t* uiState = uiContext->UiState;
#ifndef NO_FIBERS
    task_t replTask = {0};
    SET_CURRENT_TASK(&replTask);
#endif
    metac_filesystem_t* fs = 0;

    if (uiInterface.GetFileSystem)
    {
        fs = uiInterface.GetFileSystem(uiState);
    }

    Allocator_Init(&repl->FileAllocator, 0);

    MetaCFileStorage_Init(&repl->FileStorage, fs, &repl->FileAllocator);

    Repl_Init(repl);

    {
        identifier_callback_t cb;
        cb.Ctx = (void*)repl;
        cb.FuncP = cast(identifier_cb_t)&SeeIdentifier;
        repl->LPP.Parser.IdentifierCallbacks[0] = cb;
        repl->LPP.Parser.IdentifierCallbacksCount = 1;
    }



    Presemantic_(repl);

    //CompletionTrie_Print(&repl->CompletionTrie);

    if (uiInterface.SetCompletionCallback)
    {
        uiInterface.SetCompletionCallback(uiState, repl, ReplComplete);
    }

    MSG("Press enter :h to display some help text\n");

    while (Repl_Loop(repl, uiContext) != false)
    {
#ifndef NO_FIBERS
        task_t* replTask = (task_t*)(GET_CO()->arg);
        YIELD(ReplYield);
#endif
    }

    // CompletionTrie_PrintStats(&repl->CompletionTrie);
    ERRORMSG("Repl_Loop exited this should only happen on quit\n");
#ifndef NO_FIBERS
    aco_exit1(GET_CO());
#endif
}

void ReplStart(repl_ui_context_t* uiContext)
{
    g_uiContext = uiContext;
    Repl_Fiber();
}
