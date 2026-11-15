/*
 * gtk_repl.c -- a GTK3 front-end for the MetaC compiler / REPL.
 *
 * ---------------------------------------------------------------------------
 * The idea
 * ---------------------------------------------------------------------------
 * The terminal REPL (repl/repl.c) evaluates one line at a time.  That is
 * handy for poking at the compiler, but it is a poor way to write code:
 * you cannot keep a function definition around, comments scroll away and
 * there is only ever one anonymous buffer.
 *
 * This program is an editor-like front-end for the very same compiler
 * core.  The central idea is:
 *
 *      the tabs *are* the files of a metac_filesystem_t
 *
 * MetaC already abstracts the file system (os/metac_file.h).  A compiler
 * session is handed such a file system through the ui_interface's
 * GetFileSystem callback, and that is exactly what this program does:
 *
 *      +------------------------------------------------+
 *      |  GtkNotebook                                  |
 *      |    tab "hello.mtc"   <-- a file in the FS     |
 *      |    tab "utils.mtc"   <-- a file in the FS     |
 *      +------------------------------------------------+
 *             |                    ^
 *             |  metac_filesystem_t provided by the GTK app
 *             v                    |
 *        the compiler sees exactly these files
 *        (#include "utils.mtc" resolves to another tab)
 *
 * There is ONE compiler session for the whole application, so all the
 * files share the same globals, the same scope and the same runtime heap.
 * That is what makes "#include" useful: a file can use the declarations of
 * another file.
 *
 * ---------------------------------------------------------------------------
 * What you get
 * ---------------------------------------------------------------------------
 *   * one tab per file of the virtual file system,
 *   * a file browser (left hand side) showing the contents of the file
 *     system,
 *   * a source editor with MetaC syntax highlighting, line numbers and a
 *     "toggle comment" command (Ctrl+/) -- the code you write stays
 *     exactly as you typed it, comments included,
 *   * a transcript that echoes the code -- comments and all -- and shows
 *     the result of every statement right below it,
 *   * the whole compiler: types are values, .Compiler is available, and
 *     declarations really end up in the session scope.
 *
 * ---------------------------------------------------------------------------
 * Threading model
 * ---------------------------------------------------------------------------
 * MetaC's semantic analyser is written for a fiber based scheduler: it
 * can suspend itself ("YIELD") while it waits for a type to resolve.  The
 * project already solves this with os/metac_task.c: RunWorkerThread()
 * runs a cooperative scheduler and lets the "special" fiber (the REPL)
 * suspend and resume.
 *
 * We reuse that machinery verbatim:
 *
 *   GTK thread                      worker thread
 *   -----------                     -------------
 *   user presses Ctrl+Return  -->   the fiber wakes up
 *                                   parses + analyses + evaluates
 *   appends transcript        <--   pushes text into an async queue
 *
 * ---------------------------------------------------------------------------
 * Building
 * ---------------------------------------------------------------------------
 * See repl/gtk/build_gtk3.sh.  A GTK3 development package is required
 * (libgtk-3-dev).  On a machine without root access the headers can be
 * unpacked into a local prefix, see the comments in that script.
 */

#define OLD_PARSER 1
#define ACCEL ACCEL_TABLE

#include <gtk/gtk.h>
#include <gdk/gdkkeysyms.h>

/* The whole MetaC compiler core: lexer, parser, semantic analyser,
 * code generator, the bytecode interpreter and the REPL helpers. */
#include "../repl.c"

/* ------------------------------------------------------------------------- */
/*  Configuration                                                            */
/* ------------------------------------------------------------------------- */

#define GTK_REPL_APP_TITLE     "MetaC REPL"
#define GTK_REPL_MAX_TOKENS    16384   /* token budget of one source file */
#define GTK_REPL_DEFAULT_NAME  "scratch.mtc"

/* ------------------------------------------------------------------------- */
/*  Forward declarations                                                     */
/* ------------------------------------------------------------------------- */

typedef struct GtkApp_t  GtkApp_t;
typedef struct GtkFile_t GtkFile_t;

typedef enum
{
    GTK_REPL_MODE_EVAL = 0,   /* :ee -- evaluate, print the value        */
    GTK_REPL_MODE_TYPE,       /* :es -- analyse, print the type          */
    GTK_REPL_MODE_DECL,       /* :ds -- declarations only                */
    GTK_REPL_MODE_PARSE       /* :d  -- parse only, print the parse tree */
} GtkReplMode_t;

typedef enum
{
    GTK_REPL_CMD_RUN = 1,
    GTK_REPL_CMD_GLOBALS,
    GTK_REPL_CMD_HEAP,
    GTK_REPL_CMD_QUIT
} GtkReplCmdKind_t;

typedef struct
{
    GtkReplCmdKind_t Kind;
    char*            Text;
} GtkReplCmd_t;

/* metac_printer.h spells this "MetaCPRinter_Free", the implementation
 * spells it "MetaCPrinter_Free" -- use the one that actually exists. */
void MetaCPrinter_Free(metac_printer_t* self);

static void GtkApp_SetStatus(GtkApp_t* app, const char* fmt, ...);
static gboolean GtkApp_DrainOutput(gpointer data);
static void GtkApp_PostOutput(GtkApp_t* app, const char* text);

/* The tags live in the buffer they are created for, so every file gets
 * its own set; the regular expressions are shared. */
typedef struct
{
    GtkTextTag* Comment;
    GtkTextTag* String;
    GtkTextTag* Keyword;
    GtkTextTag* Number;
} GtkTags_t;

typedef struct
{
    GRegex* BlockComment;
    GRegex* LineComment;
    GRegex* StringRe;
    GRegex* CharRe;
    GRegex* NumberRe;
    GRegex* KeywordRe;
} GtkHighlighter_t;

/* ------------------------------------------------------------------------- */
/*  A file of the virtual file system                                        */
/* ------------------------------------------------------------------------- */

struct GtkFile_t
{
    GtkApp_t*        App;
    char*            Path;        /* "hello.mtc"                           */
    GtkWidget*       Page;        /* the notebook page                     */
    GtkWidget*       TabLabel;
    GtkWidget*       TabLabelText;
    GtkTextBuffer*   Buffer;      /* the source of the file                */
    GtkWidget*       GutterView;  /* the line numbers next to the editor   */
    GtkTextBuffer*   GutterBuffer;
    GtkTags_t        Tags;

    /* the content as the file system sees it */
    char*            Content;
    uint32_t         ContentLen;

    gboolean         Dirty;
};

/* ------------------------------------------------------------------------- */
/*  The application                                                          */
/* ------------------------------------------------------------------------- */

/* The state we hand to the compiler core through the ui_interface. */
typedef struct
{
    GtkApp_t* App;
} GtkUiState_t;

struct GtkApp_t
{
    GtkWidget*    Window;
    GtkWidget*    Notebook;
    GtkWidget*    FileList;       /* GtkListBox with the file system      */
    GtkWidget*    FileListScroll;
    GtkWidget*    StatusBar;
    GtkWidget*    ModeCombo;

    /* the shared transcript of the session */
    GtkWidget*    OutputView;
    GtkTextBuffer* OutputBuffer;

    GPtrArray*    Files;          /* GtkFile_t*  (also the file system)   */
    GHashTable*   FsIndex;        /* path -> GtkFile_t*                    */

    /* the one and only compiler session */
    repl_state_t     Repl;
    worker_context_t Worker;
    GtkUiState_t     UiState;

    GThread*     Thread;
    GMutex       InputMutex;
    GCond        InputCond;
    GQueue*      InputQueue;      /* GtkReplCmd_t*                         */
    GAsyncQueue* OutputQueue;     /* char*                                 */
    int          OutputPending;

    /* protects the file contents (the editor thread writes, the compiler
     * thread reads) */
    GMutex       FsMutex;

    /* the file system we hand to the compiler */
    metac_filesystem_t FileSystem;
    char*       FsScratch;
    uint32_t    FsScratchCap;

    GtkReplMode_t Mode;
    guint        RunCounter;
    guint        StatusContext;

    /* while the session is being set up the messages of the compiler core
     * go to stderr instead of the transcript */
    int          Quiet;
};

/* The file system context -- it is just a pointer to the application. */
typedef struct
{
    GtkApp_t* App;
} GtkFsCtx_t;

/* A handle of an open file. */
typedef struct
{
    char*    Data;
    uint32_t Length;
    uint64_t Size;
} GtkFsHandle_t;

typedef metac_file_info_t (*metac_filesystem_get_file_info_t)(
    metac_filesystem_ctx* fs, metac_filehandle_t handle);

static GtkApp_t* g_app = 0;

/* Guards the process wide globals of the compiler core. */
static GMutex g_coreMutex;

/* ------------------------------------------------------------------------- */
/*  The virtual file system                                                   */
/*                                                                           */
/*  Every tab of the notebook is a file of this file system.  The compiler   */
/*  sees exactly the files that are open in the editor.                      */
/* ------------------------------------------------------------------------- */

static metac_filesystem_ctx* GtkFs_Init(const char* config)
{
    (void)config;
    GtkFsCtx_t* ctx = g_new0(GtkFsCtx_t, 1);
    ctx->App = g_app;
    return (metac_filesystem_ctx*)ctx;
}

static metac_filehandle_t GtkFs_Open(metac_filesystem_ctx* ctx,
                                      const char* path, const char* filename)
{
    metac_filehandle_t result = {0};
    GtkApp_t* app = ctx ? ((GtkFsCtx_t*)ctx)->App : 0;

    if (!app || !filename)
        return result;

    char* fullPath;
    if (path && *path)
        fullPath = g_strdup_printf("%s/%s", path, filename);
    else
        fullPath = g_strdup(filename);

    GtkFile_t* file = (GtkFile_t*)g_hash_table_lookup(app->FsIndex, fullPath);
    g_free(fullPath);

    if (!file)
        return result;

    GtkFsHandle_t* handle = g_new0(GtkFsHandle_t, 1);

    g_mutex_lock(&app->FsMutex);
    handle->Length = file->ContentLen;
    handle->Size   = file->ContentLen;
    handle->Data   = (char*)malloc(file->ContentLen + 1);
    if (handle->Data)
    {
        if (file->ContentLen)
            memcpy(handle->Data, file->Content, file->ContentLen);
        handle->Data[file->ContentLen] = '\0';
    }
    g_mutex_unlock(&app->FsMutex);

    result.p = handle;
    return result;
}

static metac_buffer_t GtkFs_ReadEntireFileAndZeroTerminate(
    metac_filesystem_ctx* ctx, metac_filehandle_t handle)
{
    metac_buffer_t result = {0};
    GtkApp_t* app = ctx ? ((GtkFsCtx_t*)ctx)->App : 0;
    GtkFsHandle_t* h = (GtkFsHandle_t*)handle.p;

    if (!app || !h)
        return result;

    /* The buffer has to stay alive after the handle is closed, so we keep
     * a scratch buffer in the file system context around. */
    g_mutex_lock(&app->FsMutex);
    if (h->Length + 1 > app->FsScratchCap)
    {
        app->FsScratchCap = h->Length + 1;
        app->FsScratch = (char*)realloc(app->FsScratch, app->FsScratchCap);
    }
    if (app->FsScratch)
    {
        if (h->Length && h->Data)
            memcpy(app->FsScratch, h->Data, h->Length);
        app->FsScratch[h->Length] = '\0';
        result.Data   = app->FsScratch;
        result.Length = h->Length;
    }
    g_mutex_unlock(&app->FsMutex);

    return result;
}

static void GtkFs_Close(metac_filesystem_ctx* ctx, metac_filehandle_t handle)
{
    (void)ctx;
    GtkFsHandle_t* h = (GtkFsHandle_t*)handle.p;
    if (!h)
        return;
    free(h->Data);
    g_free(h);
}

static metac_file_info_t GtkFs_GetFileInfo(metac_filesystem_ctx* ctx,
                                            metac_filehandle_t handle)
{
    metac_file_info_t result = {0};
    GtkFsHandle_t* h = (GtkFsHandle_t*)handle.p;
    result.FileSize = h ? h->Size : 0;
    return result;
}

static uint32_t GtkFs_ReadUntilBufferFullAndZeroTerminate(
    metac_filesystem_ctx* ctx, metac_filehandle_t handle, metac_buffer_t* buffer)
{
    metac_buffer_t whole = GtkFs_ReadEntireFileAndZeroTerminate(ctx, handle);
    if (!buffer || !whole.Data)
        return 0;

    uint32_t length = whole.Length;
    if (length > buffer->Length)
        length = buffer->Length;

    memcpy((void*)buffer->Data, whole.Data, length);
    ((char*)buffer->Data)[length] = '\0';
    buffer->Length = length;
    return length;
}

static uint64_t GtkFs_GetFileSize(metac_filesystem_ctx* ctx,
                                   metac_filehandle_t handle)
{
    (void)ctx;
    GtkFsHandle_t* h = (GtkFsHandle_t*)handle.p;
    return h ? h->Size : 0;
}

static uint64_t GtkFs_GetFilesystemSize(metac_filesystem_ctx* ctx)
{
    GtkApp_t* app = ctx ? ((GtkFsCtx_t*)ctx)->App : 0;
    uint64_t total = 0;
    if (!app)
        return 0;

    g_mutex_lock(&app->FsMutex);
    for (guint i = 0; i < app->Files->len; i++)
    {
        GtkFile_t* file = (GtkFile_t*)g_ptr_array_index(app->Files, i);
        total += file->ContentLen;
    }
    g_mutex_unlock(&app->FsMutex);
    return total;
}

static const metac_filesystem_functions_t GtkFsFunctions =
{
    GtkFs_Init,
    GtkFs_Open,
    GtkFs_ReadEntireFileAndZeroTerminate,
    GtkFs_Close,
    GtkFs_GetFileInfo,
    GtkFs_ReadUntilBufferFullAndZeroTerminate,
    GtkFs_GetFileSize,
    GtkFs_GetFilesystemSize
};

/* ------------------------------------------------------------------------- */
/*  Syntax highlighting                                                      */
/* ------------------------------------------------------------------------- */

static GtkHighlighter_t g_highlighter;

/* Collect the language keywords straight from metac's own token table so
 * that the highlighter can never drift away from the compiler. */
static char* GtkRepl_BuildKeywordRegex(void)
{
    GString* re = g_string_new("\\b(");
    gboolean first = TRUE;

    /* The keyword tokens of the compiler are exactly the ones whose name
     * starts with "tok_kw_"; their spelling is the rest of the name. */
    for (uint32_t tok = 0; tok < (uint32_t)tok_max; tok++)
    {
        const char* name = MetaCTokenEnum_toChars((metac_token_enum_t)tok);
        if (!name || !g_str_has_prefix(name, "tok_kw_"))
            continue;
        if (!first)
            g_string_append_c(re, '|');
        g_string_append(re, name + strlen("tok_kw_"));
        first = FALSE;
    }
    g_string_append_c(re, ')');
    return g_string_free(re, FALSE);
}

static void GtkRepl_InitHighlighter(void)
{
    if (g_highlighter.KeywordRe)
        return;

    g_highlighter.BlockComment = g_regex_new("/\\*.*?\\*/",
        G_REGEX_DOTALL | G_REGEX_MULTILINE, 0, NULL);
    g_highlighter.LineComment  = g_regex_new("//[^\\n]*",
        G_REGEX_MULTILINE, 0, NULL);
    g_highlighter.StringRe     = g_regex_new("\"(\\\\.|[^\"\\\\])*\"", 0, 0, NULL);
    g_highlighter.CharRe       = g_regex_new("'(\\\\.|[^'\\\\])*'", 0, 0, NULL);
    g_highlighter.NumberRe     = g_regex_new("\\b[0-9][0-9a-fA-FxXuUlL.]*\\b", 0, 0, NULL);

    {
        char* kw = GtkRepl_BuildKeywordRegex();
        g_highlighter.KeywordRe = g_regex_new(kw, G_REGEX_OPTIMIZE, 0, NULL);
        g_free(kw);
    }
}

/* Create the syntax highlighting tags in a buffer. */
static void GtkRepl_CreateTags(GtkTextBuffer* buffer, GtkTags_t* tags)
{
    tags->Comment = gtk_text_buffer_create_tag(buffer, "comment",
        "foreground", "#6a9955", "style", PANGO_STYLE_ITALIC, NULL);
    tags->String = gtk_text_buffer_create_tag(buffer, "string",
        "foreground", "#ce9178", NULL);
    tags->Keyword = gtk_text_buffer_create_tag(buffer, "keyword",
        "foreground", "#569cd6", "weight", PANGO_WEIGHT_BOLD, NULL);
    tags->Number = gtk_text_buffer_create_tag(buffer, "number",
        "foreground", "#b5cea8", NULL);

    /* A tag priority has to be smaller than the size of the tag table,
     * and the tag with the highest priority wins where tags overlap. */
    gtk_text_tag_set_priority(tags->Comment, 3);
    gtk_text_tag_set_priority(tags->String, 2);
    gtk_text_tag_set_priority(tags->Keyword, 1);
    gtk_text_tag_set_priority(tags->Number, 0);
}

static void GtkRepl_ApplyRegex(GtkTextBuffer* buffer, GRegex* re, GtkTextTag* tag)
{
    GMatchInfo* match = NULL;
    if (!re)
        return;

    if (!g_regex_match(re, buffer, 0, &match))
    {
        if (match)
            g_match_info_free(match);
        return;
    }

    do {
        int s = 0, e = 0;
        if (g_match_info_fetch_pos(match, 0, &s, &e) && e > s)
        {
            GtkTextIter start, end;
            gtk_text_buffer_get_iter_at_offset(buffer, &start, s);
            gtk_text_buffer_get_iter_at_offset(buffer, &end, e);
            gtk_text_buffer_apply_tag(buffer, tag, &start, &end);
        }
    } while (g_match_info_next(match, NULL));

    g_match_info_free(match);
}

static void GtkRepl_HighlightBuffer(GtkTextBuffer* buffer, const GtkTags_t* tags)
{
    GtkTextIter start, end;

    if (!buffer || !tags || !tags->Comment)
        return;

    gtk_text_buffer_get_bounds(buffer, &start, &end);
    gtk_text_buffer_remove_all_tags(buffer, &start, &end);

    GtkRepl_ApplyRegex(buffer, g_highlighter.BlockComment, tags->Comment);
    GtkRepl_ApplyRegex(buffer, g_highlighter.LineComment,  tags->Comment);
    GtkRepl_ApplyRegex(buffer, g_highlighter.StringRe,     tags->String);
    GtkRepl_ApplyRegex(buffer, g_highlighter.CharRe,       tags->String);
    GtkRepl_ApplyRegex(buffer, g_highlighter.KeywordRe,    tags->Keyword);
    GtkRepl_ApplyRegex(buffer, g_highlighter.NumberRe,     tags->Number);
}

static gboolean GtkRepl_HighlightIdle(gpointer data)
{
    GtkFile_t* file = (GtkFile_t*)data;
    GtkRepl_HighlightBuffer(file->Buffer, &file->Tags);
    return G_SOURCE_REMOVE;
}

static guint g_highlightSourceId = 0;

static void GtkRepl_ScheduleHighlight(GtkFile_t* file)
{
    if (g_highlightSourceId)
        g_source_remove(g_highlightSourceId);
    g_highlightSourceId = g_timeout_add_full(G_PRIORITY_DEFAULT, 80,
        GtkRepl_HighlightIdle, file, NULL);
}

/* A monospace font for all the text views of the application. */
static void GtkRepl_InstallFont(void)
{
    static GtkCssProvider* provider = 0;

    if (provider)
        return;

    provider = gtk_css_provider_new();
    gtk_css_provider_load_from_data(provider,
        "textview { font-family: monospace; font-size: 11pt; }\n"
        "textview.output { font-family: monospace; font-size: 10pt; }\n",
        -1, NULL);

    GdkScreen* screen = gdk_display_get_default_screen(gdk_display_get_default());
    gtk_style_context_add_provider_for_screen(screen,
        GTK_STYLE_PROVIDER(provider), GTK_STYLE_PROVIDER_PRIORITY_APPLICATION);
}

/* ------------------------------------------------------------------------- */
/*  ui_interface implementation                                               */
/* ------------------------------------------------------------------------- */

static void GtkUi_VMessage(GtkApp_t* app, const char* fmt, va_list args)
{
    char* text = g_strdup_vprintf(fmt, args);
    if (app && app->Quiet)
        fprintf(stderr, "%s", text);
    else
        GtkApp_PostOutput(app, text);
    g_free(text);
}

static const char* GtkUi_GetInputLine(struct ui_state_t* state,
                                       repl_state_t* repl, uint32_t* lengthP)
{
    (void)state;
    (void)repl;
    *lengthP = 0;
    return "";
}

static void GtkUi_Message(struct ui_state_t* state, const char* fmt, ...)
{
    va_list args;
    va_start(args, fmt);
    GtkUi_VMessage(((GtkUiState_t*)state)->App, fmt, args);
    va_end(args);
}

static void GtkUi_ErrorMessage(struct ui_state_t* state, const char* fmt, ...)
{
    va_list args;
    va_start(args, fmt);
    GtkUi_VMessage(((GtkUiState_t*)state)->App, fmt, args);
    va_end(args);
}

static void GtkUi_Info(struct ui_state_t* state, const char* fmt, ...)
{
    va_list args;
    va_start(args, fmt);
    GtkUi_VMessage(((GtkUiState_t*)state)->App, fmt, args);
    va_end(args);
}

static repl_mode_t GtkUi_QueryMode(struct ui_state_t* state)
{
    GtkApp_t* app = ((GtkUiState_t*)state)->App;
    switch (app->Mode)
    {
        case GTK_REPL_MODE_TYPE: return repl_mode_es;
        case GTK_REPL_MODE_DECL: return repl_mode_ds;
        case GTK_REPL_MODE_PARSE: return repl_mode_decl;
        default: return repl_mode_ee;
    }
}

static void GtkUi_Clear(struct ui_state_t* state)
{
    (void)state;
}

static void GtkUi_SetCompletionCallback(struct ui_state_t* state,
                                        repl_state_t* repl,
                                        completion_cb_t completionCb)
{
    (void)state; (void)repl; (void)completionCb;
}

static metac_filesystem_t* GtkUi_GetFilesystem(struct ui_state_t* state)
{
    GtkApp_t* app = ((GtkUiState_t*)state)->App;
    return &app->FileSystem;
}

static uint32_t GtkUi_UpdateLocalCompletionCache(struct ui_state_t* state,
                                                 completion_cache_entry_t* entries,
                                                 uint32_t nEntries)
{
    (void)state; (void)entries; (void)nEntries;
    return 0;
}

static const ui_interface_t GtkUiInterface =
{
    GtkUi_GetInputLine,
    GtkUi_Message,
    GtkUi_ErrorMessage,
    GtkUi_QueryMode,
    GtkUi_Clear,
    GtkUi_Info,
    GtkUi_SetCompletionCallback,
    GtkUi_GetFilesystem,
    GtkUi_UpdateLocalCompletionCache
};

/* Push text to the transcript and wake the GTK thread. */
static void GtkApp_PostOutput(GtkApp_t* app, const char* text)
{
    if (!app || !text)
        return;

    g_async_queue_push(app->OutputQueue, g_strdup(text));

    if (!g_atomic_int_get(&app->OutputPending))
    {
        g_atomic_int_set(&app->OutputPending, 1);
        g_idle_add(GtkApp_DrainOutput, app);
    }
}

/* ------------------------------------------------------------------------- */
/*  The evaluation core                                                      */
/* ------------------------------------------------------------------------- */

static char* GtkRepl_StrdupPrinter(metac_printer_t* printer, const char* text)
{
    char* result = g_strdup(text ? text : "");
    MetaCPrinter_Reset(printer);
    return result;
}

static char* GtkRepl_PrintExpr(metac_printer_t* printer, metac_expr_t* exp)
{
    if (!exp)
        return g_strdup("");
    return GtkRepl_StrdupPrinter(printer, MetaCPrinter_PrintExpr(printer, exp));
}

static char* GtkRepl_PrintDecl(metac_printer_t* printer, metac_decl_t* decl)
{
    if (!decl)
        return g_strdup("");
    return GtkRepl_StrdupPrinter(printer, MetaCPrinter_PrintDecl(printer, decl));
}

static char* GtkRepl_PrintSema(metac_printer_t* printer,
                               metac_sema_state_t* sema, metac_node_t node)
{
    if (node == emptyNode)
        return g_strdup("");
    return GtkRepl_StrdupPrinter(printer,
        MetaCPrinter_PrintSemaNode(printer, sema, node));
}

static void GtkRepl_AppendLine(GString* out, const char* kind,
                               const char* what, const char* result)
{
    if (result && *result)
        g_string_append_printf(out, "  %-5s %-46s -> %s\n", kind, what, result);
    else
        g_string_append_printf(out, "  %-5s %s\n", kind, what);
}

/* Lex a whole source file into the lexer of the session.  The lexer grows
 * its token array through the arena allocator, so the arrays have to be
 * allocated with that very same allocator -- Repl_Init() only reserves
 * room for a single line, which is far too little for a source file. */
static void GtkRepl_LexBuffer(repl_state_t* repl, const char* text, uint32_t len)
{
    metac_lexer_state_t state = MetaCLexerStateFromBuffer(1, text, len);

    repl->LPP.Lexer.TokenCount = 0;
    repl->LPP.Lexer.LocationStorage.LocationSize = 0;
    repl->LPP.Parser.CurrentTokenIndex = 0;

    repl->LPP.LexerState = state;

    while (len > 0)
    {
        uint32_t initialPosition = state.Position;
        metac_token_t token =
            *MetaCLexerLexNextToken(&repl->LPP.Lexer, &state, text, len);

        if (token.TokenType == tok_eof)
            break;

        uint32_t eaten = state.Position - initialPosition;
        if (eaten == 0)
            break;

        text += eaten;
        len  -= eaten;
    }
}

/* The code generator cannot read global variables yet (see
 * MetaCCodegen_AccessVariable in codegen/metac_codegen.c -- it hits an
 * "assert(!\"Not implemented\")").  Evaluating such an expression would
 * take the whole compiler down, so we look for them beforehand and report
 * a proper error message instead. */
static bool GtkRepl_ExprUsesGlobal(metac_sema_expr_t* e, int depth)
{
    if (!e || depth > 64)
        return false;

    switch (e->Kind)
    {
        case expr_variable:
            return e->Variable && e->Variable->Storage.Kind == storage_global;

        case expr_call:
            if (GtkRepl_ExprUsesGlobal(e->Call.Function, depth + 1))
                return true;
            for (uint32_t i = 0; i < e->Call.ArgumentCount; i++)
                if (GtkRepl_ExprUsesGlobal(e->Call.Arguments[i], depth + 1))
                    return true;
            return false;

        case expr_argument:
        {
            sema_expr_argument_list_t* list = e->ArgumentList;
            if (!list)
                return false;
            for (uint32_t i = 0; i < list->ArgumentCount; i++)
                if (GtkRepl_ExprUsesGlobal(list->Arguments[i], depth + 1))
                    return true;
            return false;
        }

        case expr_cast:
            return GtkRepl_ExprUsesGlobal(e->CastExp, depth + 1);

        case expr_ternary:
            return GtkRepl_ExprUsesGlobal(e->E1, depth + 1)
                || GtkRepl_ExprUsesGlobal(e->E2, depth + 1)
                || GtkRepl_ExprUsesGlobal(e->Econd, depth + 1);

        case expr_paren:
            return GtkRepl_ExprUsesGlobal(e->E1, depth + 1);

        case expr_index:
        case expr_slice:
        case expr_full_slice:
            return GtkRepl_ExprUsesGlobal(e->E1, depth + 1)
                || GtkRepl_ExprUsesGlobal(e->E2, depth + 1);

        default:
            break;
    }

    /* binary expressions */
    switch (e->Kind)
    {
#define M_BINARY(EXP) case EXP:
        FOREACH_BINARY_EXP(M_BINARY)
#undef M_BINARY
        return GtkRepl_ExprUsesGlobal(e->E1, depth + 1)
            || GtkRepl_ExprUsesGlobal(e->E2, depth + 1);
    }

    /* unary expressions (sizeof and typeof are compile time, they do not
     * evaluate their operand) */
    if (e->Kind == expr_sizeof || e->Kind == expr_typeof)
        return false;

    switch (e->Kind)
    {
#define M_UNARY(EXP) case EXP:
        FOREACH_UNARY_EXP(M_UNARY)
#undef M_UNARY
        return GtkRepl_ExprUsesGlobal(e->E1, depth + 1);
    }

    return false;
}

/* Analyse + evaluate one parsed statement, appending to the transcript. */
static void GtkRepl_HandleStmt(GtkApp_t* app, metac_stmt_t* stmt, GString* out)
{
    repl_state_t* repl = &app->Repl;
    metac_printer_t* printer = &repl->Printer;

    switch (stmt->Kind)
    {
        case stmt_comment:
        {
            /* Echo the comment verbatim -- this is what keeps the code
             * readable once it shows up in the transcript. */
            stmt_comment_t* comment = (stmt_comment_t*)stmt;
            if (comment->Text && comment->Length)
                g_string_append_printf(out, "  %.*s\n",
                    (int)comment->Length, comment->Text);
        } break;

        case stmt_decl:
        {
            stmt_decl_t* declStmt = (stmt_decl_t*)stmt;
            metac_decl_t* decl = declStmt->Decl;

            if (!decl)
                break;

            char* declText = GtkRepl_PrintDecl(printer, decl);

            if (app->Mode == GTK_REPL_MODE_PARSE)
            {
                GtkRepl_AppendLine(out, "parse", declText, NULL);
                g_free(declText);
                break;
            }

            decl->StorageClass = storageclass_global;

            metac_sema_decl_t* semaDecl =
                MetaCSemantic_doDeclSemantic(&repl->SemanticState, decl);

            if (!semaDecl)
            {
                GtkRepl_AppendLine(out, "error", declText, "declaration failed");
                g_free(declText);
                break;
            }

            char* result = GtkRepl_PrintSema(printer, &repl->SemanticState,
                METAC_NODE(semaDecl));
            GtkRepl_AppendLine(out, "decl", declText, result);
            g_free(result);
            g_free(declText);
        } break;

        case stmt_expr:
        {
            stmt_expr_t* exprStmt = (stmt_expr_t*)stmt;
            metac_expr_t* exp = exprStmt->Expr;

            if (!exp)
                break;

            char* exprText = GtkRepl_PrintExpr(printer, exp);

            if (app->Mode == GTK_REPL_MODE_PARSE)
            {
                GtkRepl_AppendLine(out, "parse", exprText, NULL);
                g_free(exprText);
                break;
            }

            metac_sema_expr_t* result =
                MetaCSemantic_doExprSemantic(&repl->SemanticState, exp, 0);

            if (!result)
            {
                GtkRepl_AppendLine(out, "error", exprText, "could not analyse");
                g_free(exprText);
                break;
            }

            if (app->Mode == GTK_REPL_MODE_EVAL)
            {
                if (GtkRepl_ExprUsesGlobal(result, 0))
                {
                    GtkRepl_AppendLine(out, "error", exprText,
                        "reading a global variable is not implemented "
                        "in the code generator yet");
                    g_free(exprText);
                    break;
                }

                metac_sema_expr_t evaluated =
                    EvaluateExpr(&repl->SemanticState, result, &repl->Heap);
                result = &evaluated;
            }

            char* resultText = GtkRepl_PrintSema(printer, &repl->SemanticState,
                METAC_NODE(result));

            GtkRepl_AppendLine(out,
                (app->Mode == GTK_REPL_MODE_EVAL) ? "eval" : "type",
                exprText, resultText);

            g_free(resultText);
            g_free(exprText);
        } break;

        default:
        {
            char* stmtText = GtkRepl_StrdupPrinter(printer,
                MetaCPrinter_PrintStmt(printer, stmt));
            GtkRepl_AppendLine(out, "stmt", stmtText, NULL);
            g_free(stmtText);
        } break;
    }
}

/* Run a whole source file in the session. */
static void GtkRepl_Evaluate(GtkApp_t* app, const char* code)
{
    repl_state_t* repl = &app->Repl;
    GString* out = g_string_new(NULL);

    app->RunCounter++;
    g_string_append_printf(out, "\n=== run #%u ===\n\n", app->RunCounter);

    GtkRepl_LexBuffer(repl, code, (uint32_t)strlen(code));

    uint32_t guard = 0;
    while (repl->LPP.Parser.CurrentTokenIndex < repl->LPP.Lexer.TokenCount)
    {
        if (++guard > 100000)
        {
            g_string_append(out, "  error: parser did not make progress\n");
            break;
        }

        uint32_t before = repl->LPP.Parser.CurrentTokenIndex;

        metac_stmt_t* stmt = MetaCParser_ParseStmt(&repl->LPP.Parser, 0, 0);

        if (!stmt || (metac_stmt_t*)stmt == (metac_stmt_t*)emptyPointer)
        {
            g_string_append(out, "  error: could not parse the file\n");
            break;
        }

        GtkRepl_HandleStmt(app, stmt, out);

        if (repl->LPP.Parser.CurrentTokenIndex == before)
        {
            if (repl->LPP.Parser.CurrentTokenIndex < repl->LPP.Lexer.TokenCount)
                repl->LPP.Parser.CurrentTokenIndex++;
            else
                break;
        }
    }

    MetaCPrinter_Reset(&repl->Printer);

    GtkApp_PostOutput(app, out->str);
    g_string_free(out, TRUE);
}

/* Dump the globals of the session (the ":g lobals" command of the REPL). */
static void GtkRepl_DumpGlobals(GtkApp_t* app)
{
    repl_state_t* repl = &app->Repl;
    GString* out = g_string_new(NULL);

    g_string_append(out, "\n=== globals ===\n");

    for (uint32_t i = 0; i < repl->SemanticState.GlobalsCount; i++)
    {
        metac_sema_decl_t* global = repl->SemanticState.Globals[i];
        if (!global || global->Kind != decl_variable)
            continue;

        sema_decl_variable_t var = global->sema_decl_variable;
        metac_printer_t debugPrinter;

        MetaCPrinter_Init(&debugPrinter,
            repl->SemanticState.ParserIdentifierTable,
            repl->SemanticState.ParserStringTable, 0);

        metac_type_t typeNode = NodeFromTypeIndex(&repl->SemanticState,
            var.TypeIndex);
        const char* typeString = MetaCPrinter_PrintSemaNode(&debugPrinter,
            &repl->SemanticState, METAC_NODE(typeNode));

        const char* valueString = "NULL";
        if (var.VarInitExpr && METAC_NODE(var.VarInitExpr) != emptyNode)
            valueString = MetaCPrinter_PrintSemaNode(&debugPrinter,
                &repl->SemanticState, METAC_NODE(var.VarInitExpr));

        metac_identifier_ptr_t idPtr = IdentifierPtrFromSemaDecl(
            cast(metac_sema_decl_t*)&var);
        const char* nameString = IdentifierPtrToCharPtr(
            repl->SemanticState.ParserIdentifierTable, idPtr);

        g_string_append_printf(out, "  %-24s %-16s = %s\n",
            nameString ? nameString : "?", typeString, valueString);

        MetaCPrinter_Free(&debugPrinter);
    }

    GtkApp_PostOutput(app, out->str);
    g_string_free(out, TRUE);
}

/* Dump the first bytes of the runtime heap (":heap"). */
static void GtkRepl_DumpHeap(GtkApp_t* app)
{
    repl_state_t* repl = &app->Repl;
    GString* out = g_string_new(NULL);

    g_string_append(out, "\n=== heap ===\n");

    uint8_t* h = (uint8_t*)repl->Heap.heapData;
    if (h)
    {
        for (uint32_t i = 0; i < repl->Heap.heapSize && i < 128; i += 16)
        {
            g_string_append_printf(out, "  %04x:", i);
            for (uint32_t j = 0; j < 16 && i + j < repl->Heap.heapSize; j++)
                g_string_append_printf(out, " %02x", h[i + j]);
            g_string_append_c(out, '\n');
        }
    }
    else
    {
        g_string_append(out, "  (no heap)\n");
    }

    GtkApp_PostOutput(app, out->str);
    g_string_free(out, TRUE);
}

/* ------------------------------------------------------------------------- */
/*  The worker thread                                                        */
/* ------------------------------------------------------------------------- */

static GtkApp_t* t_currentApp = 0;

static void GtkRepl_Fiber(void)
{
    GtkApp_t* app = t_currentApp;
    if (!app)
        return;

    repl_state_t* repl = &app->Repl;

    Allocator_Init(&repl->FileAllocator, 0);
    MetaCFileStorage_Init(&repl->FileStorage, &app->FileSystem,
        &repl->FileAllocator);

    Repl_Init(repl);

    /* Give the lexer room for a whole source file.  Both arrays have to
     * come from the arena allocator because that is what the lexer uses
     * when it has to grow them. */
    {
        metac_lexer_t* lexer = &repl->LPP.Lexer;
        lexer->Tokens = (metac_token_t*)Allocator_Calloc(&repl->Allocator,
            metac_token_t, GTK_REPL_MAX_TOKENS);
        lexer->TokenCapacity = GTK_REPL_MAX_TOKENS;
        lexer->LocationStorage.Locations = (metac_location_t*)Allocator_Calloc(
            &repl->Allocator, metac_location_t, GTK_REPL_MAX_TOKENS);
        lexer->LocationStorage.LocationCapacity = GTK_REPL_MAX_TOKENS;
    }

    /* Every identifier the parser sees is a completion candidate. */
    {
        identifier_callback_t cb;
        cb.Ctx = (void*)repl;
        cb.FuncP = (identifier_cb_t)&SeeIdentifier;
        repl->LPP.Parser.IdentifierCallbacks[0] = cb;
        repl->LPP.Parser.IdentifierCallbacksCount = 1;
    }

    /* Pre-semantic pass: parse metac_compiler_interface.h so that the
     * ".Compiler" namespace becomes available in this session. */
    {
        g_mutex_lock(&g_coreMutex);
        g_uiContext = &repl->UiContext;
        app->Quiet = 1;
        Presemantic_(repl);
        app->Quiet = 0;
        g_mutex_unlock(&g_coreMutex);
    }

    GtkApp_PostOutput(app,
        "MetaC session ready.  Edit a file and press Ctrl+Return "
        "to evaluate it.\n");

    for (;;)
    {
        GtkReplCmd_t* cmd = 0;

        g_mutex_lock(&app->InputMutex);
        if (g_queue_is_empty(app->InputQueue))
            g_cond_wait(&app->InputCond, &app->InputMutex);
        if (!g_queue_is_empty(app->InputQueue))
            cmd = (GtkReplCmd_t*)g_queue_pop_head(app->InputQueue);
        g_mutex_unlock(&app->InputMutex);

        if (!cmd)
            continue;

        switch (cmd->Kind)
        {
            case GTK_REPL_CMD_RUN:
                GtkRepl_Evaluate(app, cmd->Text);
                break;
            case GTK_REPL_CMD_GLOBALS:
                GtkRepl_DumpGlobals(app);
                break;
            case GTK_REPL_CMD_HEAP:
                GtkRepl_DumpHeap(app);
                break;
            case GTK_REPL_CMD_QUIT:
                g_free(cmd->Text);
                g_free(cmd);
                aco_exit1(GET_CO());
                return;
        }

        g_free(cmd->Text);
        g_free(cmd);
    }
}

static gpointer GtkRepl_ThreadMain(gpointer arg)
{
    GtkApp_t* app = (GtkApp_t*)arg;
    t_currentApp = app;
    RunWorkerThread(&app->Worker, GtkRepl_Fiber, 0);
    return 0;
}

static void GtkApp_SendCommand(GtkApp_t* app, GtkReplCmdKind_t kind,
                               const char* text)
{
    GtkReplCmd_t* cmd = g_new0(GtkReplCmd_t, 1);
    cmd->Kind = kind;
    cmd->Text = text ? g_strdup(text) : NULL;

    g_mutex_lock(&app->InputMutex);
    g_queue_push_tail(app->InputQueue, cmd);
    g_cond_signal(&app->InputCond);
    g_mutex_unlock(&app->InputMutex);
}

/* ------------------------------------------------------------------------- */
/*  Files                                                                    */
/* ------------------------------------------------------------------------- */

static void GtkFile_UpdateGutter(GtkFile_t* file);

/* Copy the editor content into the file system. */
static void GtkFile_SyncToFs(GtkFile_t* file)
{
    GtkApp_t* app = file->App;
    GtkTextIter start, end;

    gtk_text_buffer_get_bounds(file->Buffer, &start, &end);
    char* text = gtk_text_buffer_get_text(file->Buffer, &start, &end, FALSE);
    uint32_t length = text ? (uint32_t)strlen(text) : 0;

    g_mutex_lock(&app->FsMutex);
    free(file->Content);
    file->Content = (char*)malloc(length + 1);
    if (file->Content)
    {
        if (length)
            memcpy(file->Content, text, length);
        file->Content[length] = '\0';
    }
    file->ContentLen = length;
    g_mutex_unlock(&app->FsMutex);

    g_free(text);
}

static void GtkFile_UpdateLabel(GtkFile_t* file)
{
    char* label = g_strdup_printf("%s%s", file->Path, file->Dirty ? " *" : "");
    gtk_label_set_text(GTK_LABEL(file->TabLabelText), label);
    g_free(label);
}

static void GtkFile_OnChanged(GtkTextBuffer* buffer, gpointer data)
{
    GtkFile_t* file = (GtkFile_t*)data;
    file->Dirty = TRUE;
    GtkFile_UpdateLabel(file);
    GtkFile_SyncToFs(file);
    GtkFile_UpdateGutter(file);
    GtkRepl_ScheduleHighlight(file);
}

/* --- the file browser -------------------------------------------------- */

static void GtkApp_RefreshFileList(GtkApp_t* app)
{
    GtkListBox* list = GTK_LIST_BOX(app->FileList);
    GList* children = gtk_container_get_children(GTK_CONTAINER(list));

    for (GList* l = children; l; l = l->next)
        gtk_widget_destroy(GTK_WIDGET(l->data));
    g_list_free(children);

    for (guint i = 0; i < app->Files->len; i++)
    {
        GtkFile_t* file = (GtkFile_t*)g_ptr_array_index(app->Files, i);
        GtkWidget* row = gtk_list_box_row_new();
        GtkWidget* label = gtk_label_new(file->Path);
        gtk_label_set_xalign(GTK_LABEL(label), 0.0);
        gtk_container_add(GTK_CONTAINER(row), label);
        g_object_set_data(G_OBJECT(row), "file", file);
        gtk_list_box_insert(list, row, -1);
    }

    gtk_widget_show_all(list);
}

static void GtkFile_OnFileListRowActivated(GtkListBox* box, GtkListBoxRow* row,
                                           gpointer data)
{
    GtkApp_t* app = (GtkApp_t*)data;
    (void)box;

    GtkFile_t* file = (GtkFile_t*)g_object_get_data(G_OBJECT(row), "file");
    if (!file)
        return;

    int page = gtk_notebook_page_num(GTK_NOTEBOOK(app->Notebook), file->Page);
    if (page >= 0)
        gtk_notebook_set_current_page(GTK_NOTEBOOK(app->Notebook), page);
}

/* --- creating / renaming / deleting files ------------------------------ */

static GtkWidget* GtkFile_CreatePage(GtkFile_t* file);

static GtkFile_t* GtkApp_AddFile(GtkApp_t* app, const char* path,
                                 const char* initialContent)
{
    if (!path || !*path)
        return 0;

    if (g_hash_table_lookup(app->FsIndex, path))
    {
        GtkApp_SetStatus(app, "'%s' already exists", path);
        return 0;
    }

    GtkFile_t* file = g_new0(GtkFile_t, 1);
    file->App = app;
    file->Path = g_strdup(path);
    file->Content = g_strdup(initialContent ? initialContent : "");
    file->ContentLen = (uint32_t)strlen(file->Content);

    file->Page = GtkFile_CreatePage(file);
    g_object_set_data(G_OBJECT(file->Page), "file", file);

    file->TabLabel = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 4);
    file->TabLabelText = gtk_label_new(file->Path);
    gtk_label_set_xalign(GTK_LABEL(file->TabLabelText), 0.0);
    gtk_box_pack_start(GTK_BOX(file->TabLabel), file->TabLabelText, TRUE, TRUE, 0);
    gtk_widget_show(file->TabLabel);
    gtk_widget_show(file->TabLabelText);

    int pageIndex = gtk_notebook_append_page(GTK_NOTEBOOK(app->Notebook),
        file->Page, file->TabLabel);
    gtk_notebook_set_tab_reorderable(GTK_NOTEBOOK(app->Notebook), file->Page, TRUE);
    gtk_notebook_set_current_page(GTK_NOTEBOOK(app->Notebook), pageIndex);

    g_ptr_array_add(app->Files, file);
    g_hash_table_insert(app->FsIndex, file->Path, file);

    if (initialContent)
    {
        gtk_text_buffer_set_text(file->Buffer, initialContent, -1);
        file->Dirty = FALSE;
        GtkFile_UpdateLabel(file);
    }

    GtkApp_RefreshFileList(app);
    GtkApp_SetStatus(app, "'%s' added to the file system", path);
    return file;
}

static void GtkApp_RemoveFile(GtkApp_t* app, GtkFile_t* file)
{
    g_hash_table_remove(app->FsIndex, file->Path);
    g_ptr_array_remove(app->Files, file);

    int page = gtk_notebook_page_num(GTK_NOTEBOOK(app->Notebook), file->Page);
    if (page >= 0)
        gtk_notebook_remove_page(GTK_NOTEBOOK(app->Notebook), page);

    GtkApp_RefreshFileList(app);
    GtkApp_SetStatus(app, "'%s' removed from the file system", file->Path);

    g_free(file->Path);
    free(file->Content);
    g_free(file);
}

/* ------------------------------------------------------------------------- */
/*  Widgets                                                                  */
/* ------------------------------------------------------------------------- */

/* The number of lines of the buffer. */
static int GtkFile_LineCount(GtkFile_t* file)
{
    GtkTextIter end;
    gtk_text_buffer_get_end_iter(file->Buffer, &end);
    return gtk_text_iter_get_line(&end) + 1;
}

/* Refresh the line numbers shown left of the editor. */
static void GtkFile_UpdateGutter(GtkFile_t* file)
{
    if (!file->GutterBuffer)
        return;

    int lines = GtkFile_LineCount(file);

    GString* numbers = g_string_new(NULL);
    for (int i = 1; i <= lines; i++)
        g_string_append_printf(numbers, "%d\n", i);

    gtk_text_buffer_set_text(file->GutterBuffer, numbers->str, -1);
    g_string_free(numbers, TRUE);
}

/* Keep the gutter in sync with the vertical scroll of the editor. */
static void GtkFile_OnEditorScroll(GtkAdjustment* adjustment, gpointer data)
{
    GtkFile_t* file = (GtkFile_t*)data;
    if (!file->GutterView)
        return;

    GtkAdjustment* gutterAdjustment =
        gtk_scrolled_window_get_vadjustment(
            GTK_SCROLLED_WINDOW(gtk_widget_get_parent(file->GutterView)));

    if (!gutterAdjustment)
        return;

    double value = gtk_adjustment_get_value(adjustment);
    double upper = gtk_adjustment_get_upper(adjustment);
    double page  = gtk_adjustment_get_page_size(adjustment);
    double gUpper = gtk_adjustment_get_upper(gutterAdjustment);
    double gPage  = gtk_adjustment_get_page_size(gutterAdjustment);

    if (upper > page && gUpper > gPage)
    {
        gtk_adjustment_set_value(gutterAdjustment,
            value / (upper - page) * (gUpper - gPage));
    }
}

/* The tab key inserts spaces -- a tab in the source would confuse the
 * indentation of the transcript. */
static gboolean GtkFile_OnKeyPress(GtkWidget* widget, GdkEventKey* event,
                                   gpointer data)
{
    GtkFile_t* file = (GtkFile_t*)data;
    if (event->keyval == GDK_KEY_Tab && !(event->state & GDK_CONTROL_MASK))
    {
        gtk_text_buffer_insert_at_cursor(file->Buffer, "    ", -1);
        return TRUE;
    }
    (void)widget;
    return FALSE;
}

static void GtkFile_CreateEditor(GtkFile_t* file, GtkWidget* page)
{
    GtkWidget* editorBox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 0);
    GtkWidget* gutterScroll = gtk_scrolled_window_new(0, 0);
    GtkWidget* gutter = gtk_text_view_new();
    GtkWidget* editorScroll = gtk_scrolled_window_new(0, 0);
    GtkWidget* editor = gtk_text_view_new();

    gtk_text_view_set_monospace(GTK_TEXT_VIEW(editor), TRUE);
    gtk_text_view_set_wrap_mode(GTK_TEXT_VIEW(editor), GTK_WRAP_NONE);
    gtk_text_view_set_accepts_tab(GTK_TEXT_VIEW(editor), FALSE);

    /* --- the line number gutter --- */
    gtk_text_view_set_editable(GTK_TEXT_VIEW(gutter), FALSE);
    gtk_text_view_set_cursor_visible(GTK_TEXT_VIEW(gutter), FALSE);
    gtk_text_view_set_monospace(GTK_TEXT_VIEW(gutter), TRUE);
    gtk_text_view_set_wrap_mode(GTK_TEXT_VIEW(gutter), GTK_WRAP_NONE);
    gtk_widget_set_size_request(gutter, 46, -1);
    gtk_widget_set_margin_start(gutter, 4);
    gtk_widget_set_margin_end(gutter, 4);
    gtk_text_view_set_left_margin(GTK_TEXT_VIEW(gutter), 0);
    gtk_text_view_set_right_margin(GTK_TEXT_VIEW(gutter), 0);
    file->GutterBuffer = gtk_text_view_get_buffer(GTK_TEXT_VIEW(gutter));

    gtk_style_context_add_class(gtk_widget_get_style_context(gutter), "output");

    gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(gutterScroll),
        GTK_POLICY_NEVER, GTK_POLICY_AUTOMATIC);
    gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(editorScroll),
        GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
    gtk_container_add(GTK_CONTAINER(gutterScroll), gutter);
    gtk_container_add(GTK_CONTAINER(editorScroll), editor);

    gtk_box_pack_start(GTK_BOX(editorBox), gutterScroll, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(editorBox), editorScroll, TRUE, TRUE, 0);
    gtk_box_pack_start(GTK_BOX(page), editorBox, TRUE, TRUE, 0);

    file->GutterView = gutter;

    file->Buffer = gtk_text_view_get_buffer(GTK_TEXT_VIEW(editor));
    GtkRepl_CreateTags(file->Buffer, &file->Tags);
    g_signal_connect(file->Buffer, "changed",
        G_CALLBACK(GtkFile_OnChanged), file);
    g_signal_connect(editor, "key-press-event",
        G_CALLBACK(GtkFile_OnKeyPress), file);
    g_signal_connect(gtk_scrolled_window_get_vadjustment(
        GTK_SCROLLED_WINDOW(editorScroll)), "value-changed",
        G_CALLBACK(GtkFile_OnEditorScroll), file);
}

static GtkWidget* GtkFile_CreatePage(GtkFile_t* file)
{
    GtkWidget* page = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
    GtkFile_CreateEditor(file, page);
    return page;
}

/* ------------------------------------------------------------------------- */
/*  Actions                                                                  */
/* ------------------------------------------------------------------------- */

static void GtkApp_SetStatus(GtkApp_t* app, const char* fmt, ...)
{
    va_list args;
    va_start(args, fmt);
    char* text = g_strdup_vprintf(fmt, args);
    va_end(args);

    gtk_statusbar_pop(GTK_STATUSBAR(app->StatusBar), app->StatusContext);
    gtk_statusbar_push(GTK_STATUSBAR(app->StatusBar), app->StatusContext, text);
    g_free(text);
}

static GtkFile_t* GtkApp_CurrentFile(GtkApp_t* app)
{
    int page = gtk_notebook_get_current_page(GTK_NOTEBOOK(app->Notebook));
    if (page < 0)
        return 0;
    GtkWidget* pageWidget =
        gtk_notebook_get_nth_page(GTK_NOTEBOOK(app->Notebook), page);
    return (GtkFile_t*)g_object_get_data(G_OBJECT(pageWidget), "file");
}

static void GtkApp_RunCurrentFile(GtkApp_t* app)
{
    GtkFile_t* file = GtkApp_CurrentFile(app);
    if (!file)
        return;

    /* make sure the file system sees the current content */
    GtkFile_SyncToFs(file);

    GtkTextIter start, end;
    gtk_text_buffer_get_bounds(file->Buffer, &start, &end);
    char* code = gtk_text_buffer_get_text(file->Buffer, &start, &end, FALSE);

    GtkApp_SendCommand(app, GTK_REPL_CMD_RUN, code);
    g_free(code);
}

static void GtkApp_OnRunClicked(GtkButton* button, gpointer data)
{
    (void)button;
    GtkApp_RunCurrentFile((GtkApp_t*)data);
}

static void GtkApp_OnGlobalsClicked(GtkButton* button, gpointer data)
{
    (void)button;
    GtkApp_SendCommand((GtkApp_t*)data, GTK_REPL_CMD_GLOBALS, NULL);
}

static void GtkApp_OnHeapClicked(GtkButton* button, gpointer data)
{
    (void)button;
    GtkApp_SendCommand((GtkApp_t*)data, GTK_REPL_CMD_HEAP, NULL);
}

static void GtkApp_OnNewFileClicked(GtkButton* button, gpointer data)
{
    (void)button;
    GtkApp_t* app = (GtkApp_t*)data;

    GtkWidget* dialog = gtk_dialog_new_with_buttons("New file",
        GTK_WINDOW(app->Window),
        GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT,
        "_Cancel", GTK_RESPONSE_CANCEL,
        "_Create", GTK_RESPONSE_ACCEPT, NULL);

    GtkWidget* content = gtk_dialog_get_content_area(GTK_DIALOG(dialog));
    GtkWidget* entry = gtk_entry_new();
    gtk_entry_set_text(GTK_ENTRY(entry), GTK_REPL_DEFAULT_NAME);
    gtk_entry_set_activates_default(GTK_ENTRY(entry), TRUE);
    gtk_dialog_set_default_response(GTK_DIALOG(dialog), GTK_RESPONSE_ACCEPT);
    gtk_container_add(GTK_CONTAINER(content), entry);
    gtk_widget_show_all(dialog);

    if (gtk_dialog_run(GTK_DIALOG(dialog)) == GTK_RESPONSE_ACCEPT)
    {
        const char* name = gtk_entry_get_text(GTK_ENTRY(entry));
        if (name && *name)
        {
            char* path = g_strdup(name);
            if (!g_str_has_suffix(path, ".mtc"))
            {
                char* withExt = g_strconcat(path, ".mtc", NULL);
                g_free(path);
                path = withExt;
            }
            GtkApp_AddFile(app, path, NULL);
            g_free(path);
        }
    }
    gtk_widget_destroy(dialog);
}

static void GtkApp_OnDeleteFileClicked(GtkButton* button, gpointer data)
{
    (void)button;
    GtkApp_t* app = (GtkApp_t*)data;
    GtkFile_t* file = GtkApp_CurrentFile(app);
    if (!file)
        return;

    GtkWidget* dialog = gtk_message_dialog_new(GTK_WINDOW(app->Window),
        GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT,
        GTK_MESSAGE_QUESTION, GTK_BUTTONS_OK_CANCEL,
        "Remove '%s' from the file system?", file->Path);
    if (gtk_dialog_run(GTK_DIALOG(dialog)) == GTK_RESPONSE_OK)
        GtkApp_RemoveFile(app, file);
    gtk_widget_destroy(dialog);
}

static void GtkApp_OnRenameFileClicked(GtkButton* button, gpointer data)
{
    (void)button;
    GtkApp_t* app = (GtkApp_t*)data;
    GtkFile_t* file = GtkApp_CurrentFile(app);
    if (!file)
        return;

    GtkWidget* dialog = gtk_dialog_new_with_buttons("Rename file",
        GTK_WINDOW(app->Window),
        GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT,
        "_Cancel", GTK_RESPONSE_CANCEL,
        "_Rename", GTK_RESPONSE_ACCEPT, NULL);

    GtkWidget* content = gtk_dialog_get_content_area(GTK_DIALOG(dialog));
    GtkWidget* entry = gtk_entry_new();
    gtk_entry_set_text(GTK_ENTRY(entry), file->Path);
    gtk_entry_set_activates_default(GTK_ENTRY(entry), TRUE);
    gtk_dialog_set_default_response(GTK_DIALOG(dialog), GTK_RESPONSE_ACCEPT);
    gtk_container_add(GTK_CONTAINER(content), entry);
    gtk_widget_show_all(dialog);

    if (gtk_dialog_run(GTK_DIALOG(dialog)) == GTK_RESPONSE_ACCEPT)
    {
        const char* name = gtk_entry_get_text(GTK_ENTRY(entry));
        if (name && *name && strcmp(name, file->Path) != 0)
        {
            char* path = g_strdup(name);
            g_hash_table_remove(app->FsIndex, file->Path);
            g_free(file->Path);
            file->Path = path;
            g_hash_table_insert(app->FsIndex, file->Path, file);
            GtkFile_UpdateLabel(file);
            GtkApp_RefreshFileList(app);
        }
    }
    gtk_widget_destroy(dialog);
}

static void GtkApp_OnModeChanged(GtkComboBox* combo, gpointer data)
{
    GtkApp_t* app = (GtkApp_t*)data;
    const char* id = gtk_combo_box_get_active_id(combo);
    if (!id)
        return;
    if (!strcmp(id, "type"))      app->Mode = GTK_REPL_MODE_TYPE;
    else if (!strcmp(id, "decl")) app->Mode = GTK_REPL_MODE_DECL;
    else if (!strcmp(id, "parse"))app->Mode = GTK_REPL_MODE_PARSE;
    else                          app->Mode = GTK_REPL_MODE_EVAL;
}

/* ------------------------------------------------------------------------- */
/*  Comment toggling                                                         */
/* ------------------------------------------------------------------------- */

static void GtkFile_ToggleComment(GtkFile_t* file)
{
    GtkTextBuffer* buffer = file->Buffer;
    GtkTextIter start, end;

    gtk_text_buffer_get_selection_bounds(buffer, &start, &end);
    if (gtk_text_iter_equal(&start, &end))
        gtk_text_buffer_get_bounds(buffer, &start, &end);

    int startLine = gtk_text_iter_get_line(&start);
    int endLine   = gtk_text_iter_get_line(&end);

    gboolean allCommented = TRUE;
    for (int line = startLine; line <= endLine && allCommented; line++)
    {
        GtkTextIter lineStart, lineEnd;
        gtk_text_buffer_get_iter_at_line(buffer, &lineStart, line);
        lineEnd = lineStart;
        if (!gtk_text_iter_ends_line(&lineEnd))
            gtk_text_iter_forward_to_line_end(&lineEnd);

        char* text = gtk_text_buffer_get_text(buffer, &lineStart, &lineEnd, FALSE);
        const char* p = text;
        while (*p == ' ' || *p == '\t')
            p++;
        if (!(*p == '/' && p[1] == '/'))
            allCommented = FALSE;
        g_free(text);
    }

    for (int line = endLine; line >= startLine; line--)
    {
        GtkTextIter lineStart, lineEnd;
        gtk_text_buffer_get_iter_at_line(buffer, &lineStart, line);
        lineEnd = lineStart;
        if (!gtk_text_iter_ends_line(&lineEnd))
            gtk_text_iter_forward_to_line_end(&lineEnd);

        char* text = gtk_text_buffer_get_text(buffer, &lineStart, &lineEnd, FALSE);
        const char* p = text;
        while (*p == ' ' || *p == '\t')
            p++;

        if (*p != '\0')
        {
            gint offset = gtk_text_iter_get_line_offset(&lineStart)
                        + (gint)(p - text);

            if (allCommented)
            {
                GtkTextIter delStart, delEnd;
                delStart = lineStart;
                gtk_text_iter_set_line_offset(&delStart, offset);
                delEnd = delStart;
                gtk_text_iter_forward_chars(&delEnd, 2);
                gtk_text_buffer_delete(buffer, &delStart, &delEnd);

                GtkTextIter s2 = delEnd, e2 = delEnd;
                if (!gtk_text_iter_ends_line(&e2))
                {
                    gtk_text_iter_forward_char(&e2);
                    char* rest = gtk_text_buffer_get_text(buffer, &s2, &e2, FALSE);
                    if (rest && rest[0] == ' ')
                        gtk_text_buffer_delete(buffer, &s2, &e2);
                    g_free(rest);
                }
            }
            else
            {
                GtkTextIter insStart = lineStart;
                gtk_text_iter_set_line_offset(&insStart, offset);
                gtk_text_buffer_insert(buffer, &insStart, "// ", -1);
            }
        }
        g_free(text);
    }
}

/* ------------------------------------------------------------------------- */
/*  Keyboard handling                                                        */
/* ------------------------------------------------------------------------- */

static gboolean GtkApp_KeyPress(GtkWidget* widget, GdkEventKey* event, gpointer data)
{
    GtkApp_t* app = (GtkApp_t*)data;
    GtkFile_t* file = GtkApp_CurrentFile(app);

    gboolean ctrl = (event->state & GDK_CONTROL_MASK) != 0;

    if (ctrl)
    {
        switch (event->keyval)
        {
            case GDK_KEY_Return:
            case GDK_KEY_KP_Enter:
                GtkApp_RunCurrentFile(app);
                return TRUE;
            case GDK_KEY_slash:
                if (file)
                    GtkFile_ToggleComment(file);
                return TRUE;
            case GDK_KEY_n:
                GtkApp_OnNewFileClicked(NULL, app);
                return TRUE;
            default:
                break;
        }
    }

    (void)widget;
    return FALSE;
}

/* ------------------------------------------------------------------------- */
/*  Draining the worker output into the transcript                           */
/* ------------------------------------------------------------------------- */

static gboolean GtkApp_DrainOutput(gpointer data)
{
    GtkApp_t* app = (GtkApp_t*)data;
    g_atomic_int_set(&app->OutputPending, 0);

    GtkTextIter end;
    gtk_text_buffer_get_end_iter(app->OutputBuffer, &end);

    for (;;)
    {
        gpointer item = g_async_queue_try_pop(app->OutputQueue);
        if (!item)
            break;
        char* text = (char*)item;
        gtk_text_buffer_insert(app->OutputBuffer, &end, text, -1);
        g_free(text);
    }

    GtkTextIter last;
    gtk_text_buffer_get_end_iter(app->OutputBuffer, &last);
    GtkTextMark* mark = gtk_text_buffer_create_mark(app->OutputBuffer,
        NULL, &last, FALSE);
    gtk_text_view_scroll_to_mark(GTK_TEXT_VIEW(app->OutputView), mark,
        0.0, TRUE, 0.0, 1.0);
    gtk_text_buffer_delete_mark(app->OutputBuffer, mark);

    return G_SOURCE_REMOVE;
}

/* ------------------------------------------------------------------------- */
/*  Self test                                                                */
/* ------------------------------------------------------------------------- */

/* Runs a small program through the whole pipeline (the editor -> the
 * worker thread -> parse -> semantic -> codegen -> bytecode interpreter ->
 * the transcript widget) and prints the transcript to stdout.
 * This is what the build script uses to check that everything works. */
static int GtkApp_SelfTest(GtkApp_t* app, const char* code)
{
    /* the widgets have to be realized, otherwise the notebook does not
     * have a current page yet */
    gtk_widget_show_all(app->Window);

    GtkFile_t* file = GtkApp_CurrentFile(app);
    if (!file)
    {
        fprintf(stderr, "self test: no current file\n");
        return 1;
    }

    /* put the code into the editor of the current file ... */
    gtk_text_buffer_set_text(file->Buffer, code, -1);
    GtkFile_SyncToFs(file);

    /* ... and press "Run" */
    GtkApp_RunCurrentFile(app);

    /* pump the main loop until the transcript shows the result */
    gboolean done = FALSE;
    for (int i = 0; i < 600 && !done; i++)
    {
        while (gtk_events_pending())
            gtk_main_iteration_do(FALSE);

        GtkTextIter start, end;
        gtk_text_buffer_get_bounds(app->OutputBuffer, &start, &end);
        char* text = gtk_text_buffer_get_text(app->OutputBuffer, &start, &end, FALSE);
        done = (strstr(text, "run #") != 0);
        g_free(text);

        if (!done)
            g_usleep(10000);
    }

    GtkTextIter start, end;
    gtk_text_buffer_get_bounds(app->OutputBuffer, &start, &end);
    char* text = gtk_text_buffer_get_text(app->OutputBuffer, &start, &end, FALSE);
    fputs(text, stdout);
    fflush(stdout);
    g_free(text);

    /* stop the worker thread *before* we leave - a thread that is still
     * running while the process exits would corrupt the heap */
    GtkApp_SendCommand(app, GTK_REPL_CMD_QUIT, NULL);
    if (app->Thread)
    {
        g_thread_join(app->Thread);
        app->Thread = 0;
    }

    return 0;
}

/* ------------------------------------------------------------------------- */
/*  main                                                                     */
/* ------------------------------------------------------------------------- */

static const char* const GtkApp_SelfTestCode =
    "// the comment of the declaration\n"
    "int x = 5;\n"
    "// the comment of the expression\n"
    "x + 1\n"
    "sizeof(x)\n";

int main(int argc, char* argv[])
{
    gtk_init(&argc, &argv);

    gboolean selfTest = FALSE;
    const char* selfTestFile = 0;
    for (int i = 1; i < argc; i++)
    {
        if (!strcmp(argv[i], "--selftest"))
            selfTest = TRUE;
        else if (!strcmp(argv[i], "--eval") && i + 1 < argc)
            selfTestFile = argv[++i];
    }

    /* the fiber library has to be initialised once per process */
    aco_global_init();

    GtkRepl_InitHighlighter();
    GtkRepl_InstallFont();

    GtkApp_t app = {0};
    g_app = &app;

    app.Files   = g_ptr_array_new();
    app.FsIndex = g_hash_table_new(g_str_hash, g_str_equal);
    g_mutex_init(&app.FsMutex);
    g_mutex_init(&app.InputMutex);
    g_cond_init(&app.InputCond);
    app.InputQueue  = g_queue_new();
    app.OutputQueue = g_async_queue_new();
    app.Mode = GTK_REPL_MODE_EVAL;

    /* the file system the compiler will see -- it contains exactly the
     * files that are open as tabs */
    app.FileSystem.ctx = (metac_filesystem_ctx*)g_new0(GtkFsCtx_t, 1);
    ((GtkFsCtx_t*)app.FileSystem.ctx)->App = &app;
    app.FileSystem.functions = &GtkFsFunctions;

    app.Window = gtk_window_new(GTK_WINDOW_TOPLEVEL);
    gtk_window_set_title(GTK_WINDOW(app.Window), GTK_REPL_APP_TITLE);
    gtk_window_set_default_size(GTK_WINDOW(app.Window), 1200, 820);
    g_signal_connect(app.Window, "destroy", G_CALLBACK(gtk_main_quit), NULL);

    /* --- header toolbar --- */
    GtkWidget* header = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 6);

    GtkWidget* newFileButton = gtk_button_new_with_label("New file");
    gtk_widget_set_tooltip_text(newFileButton, "Create a file (Ctrl+N)");
    GtkWidget* renameButton = gtk_button_new_with_label("Rename");
    GtkWidget* deleteButton = gtk_button_new_with_label("Delete");
    GtkWidget* runButton    = gtk_button_new_with_label("Run");
    gtk_widget_set_tooltip_text(runButton, "Evaluate the file (Ctrl+Return)");
    GtkWidget* globalsButton = gtk_button_new_with_label("Globals");
    GtkWidget* heapButton    = gtk_button_new_with_label("Heap");

    GtkWidget* modeLabel = gtk_label_new("Mode:");
    GtkWidget* modeCombo = gtk_combo_box_text_new();
    gtk_combo_box_text_append(GTK_COMBO_BOX_TEXT(modeCombo), "eval",
        "Evaluate (:ee)");
    gtk_combo_box_text_append(GTK_COMBO_BOX_TEXT(modeCombo), "type",
        "Type     (:es)");
    gtk_combo_box_text_append(GTK_COMBO_BOX_TEXT(modeCombo), "decl",
        "Declare  (:ds)");
    gtk_combo_box_text_append(GTK_COMBO_BOX_TEXT(modeCombo), "parse",
        "Parse    (:d)");
    gtk_combo_box_set_active_id(GTK_COMBO_BOX(modeCombo), "eval");
    app.ModeCombo = modeCombo;

    gtk_box_pack_start(GTK_BOX(header), newFileButton, FALSE, FALSE, 4);
    gtk_box_pack_start(GTK_BOX(header), renameButton, FALSE, FALSE, 4);
    gtk_box_pack_start(GTK_BOX(header), deleteButton, FALSE, FALSE, 4);
    gtk_box_pack_end(GTK_BOX(header), heapButton, FALSE, FALSE, 4);
    gtk_box_pack_end(GTK_BOX(header), globalsButton, FALSE, FALSE, 4);
    gtk_box_pack_end(GTK_BOX(header), runButton, FALSE, FALSE, 4);
    gtk_box_pack_end(GTK_BOX(header), modeCombo, FALSE, FALSE, 4);
    gtk_box_pack_end(GTK_BOX(header), modeLabel, FALSE, FALSE, 4);

    /* --- file browser (the contents of the metac file system) --- */
    GtkWidget* sidebar = gtk_box_new(GTK_ORIENTATION_VERTICAL, 4);
    GtkWidget* sidebarLabel = gtk_label_new("File system");
    gtk_label_set_xalign(GTK_LABEL(sidebarLabel), 0.0);
    app.FileList = gtk_list_box_new();
    gtk_list_box_set_selection_mode(GTK_LIST_BOX(app.FileList),
        GTK_SELECTION_SINGLE);
    /* a single click on a file of the file system switches to its tab */
    gtk_list_box_set_activate_on_single_click(GTK_LIST_BOX(app.FileList), TRUE);
    app.FileListScroll = gtk_scrolled_window_new(0, 0);
    gtk_widget_set_size_request(app.FileListScroll, 190, -1);
    gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(app.FileListScroll),
        GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
    gtk_container_add(GTK_CONTAINER(app.FileListScroll), app.FileList);
    gtk_box_pack_start(GTK_BOX(sidebar), sidebarLabel, FALSE, FALSE, 2);
    gtk_box_pack_start(GTK_BOX(sidebar), app.FileListScroll, TRUE, TRUE, 0);

    /* --- notebook: one tab per file --- */
    app.Notebook = gtk_notebook_new();
    gtk_notebook_set_scrollable(GTK_NOTEBOOK(app.Notebook), TRUE);

    GtkWidget* content = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 0);
    gtk_box_pack_start(GTK_BOX(content), sidebar, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(content), app.Notebook, TRUE, TRUE, 0);

    /* --- the shared transcript of the session --- */
    app.OutputView = gtk_text_view_new();
    gtk_text_view_set_editable(GTK_TEXT_VIEW(app.OutputView), FALSE);
    gtk_text_view_set_cursor_visible(GTK_TEXT_VIEW(app.OutputView), FALSE);
    gtk_text_view_set_monospace(GTK_TEXT_VIEW(app.OutputView), TRUE);
    gtk_text_view_set_wrap_mode(GTK_TEXT_VIEW(app.OutputView), GTK_WRAP_WORD_CHAR);
    gtk_style_context_add_class(gtk_widget_get_style_context(app.OutputView),
        "output");
    app.OutputBuffer = gtk_text_view_get_buffer(GTK_TEXT_VIEW(app.OutputView));

    GtkWidget* outputScroll = gtk_scrolled_window_new(0, 0);
    gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(outputScroll),
        GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
    gtk_container_add(GTK_CONTAINER(outputScroll), app.OutputView);

    /* --- status bar --- */
    app.StatusBar = gtk_statusbar_new();
    app.StatusContext = gtk_statusbar_get_context_id(
        GTK_STATUSBAR(app.StatusBar), "gtk-repl");

    GtkWidget* root = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
    gtk_box_pack_start(GTK_BOX(root), header, FALSE, FALSE, 0);
    gtk_box_pack_start(GTK_BOX(root), content, TRUE, TRUE, 0);
    gtk_box_pack_end(GTK_BOX(root), outputScroll, FALSE, FALSE, 0);
    gtk_box_pack_end(GTK_BOX(root), app.StatusBar, FALSE, FALSE, 0);
    gtk_container_add(GTK_CONTAINER(app.Window), root);

    g_signal_connect(newFileButton, "clicked",
        G_CALLBACK(GtkApp_OnNewFileClicked), &app);
    g_signal_connect(renameButton, "clicked",
        G_CALLBACK(GtkApp_OnRenameFileClicked), &app);
    g_signal_connect(deleteButton, "clicked",
        G_CALLBACK(GtkApp_OnDeleteFileClicked), &app);
    g_signal_connect(runButton, "clicked",
        G_CALLBACK(GtkApp_OnRunClicked), &app);
    g_signal_connect(globalsButton, "clicked",
        G_CALLBACK(GtkApp_OnGlobalsClicked), &app);
    g_signal_connect(heapButton, "clicked",
        G_CALLBACK(GtkApp_OnHeapClicked), &app);
    g_signal_connect(modeCombo, "changed",
        G_CALLBACK(GtkApp_OnModeChanged), &app);
    g_signal_connect(app.Window, "key-press-event",
        G_CALLBACK(GtkApp_KeyPress), &app);

    GtkApp_SetStatus(&app, "Ctrl+Return evaluates the file, "
                           "Ctrl+/ toggles comments");

    /* --- the compiler session --- */
    app.UiState.App = &app;
    app.Repl.UiContext.UiInterface = &GtkUiInterface;
    app.Repl.UiContext.UiState = (struct ui_state_t*)&app.UiState;

    app.Thread = g_thread_new("metac-repl", GtkRepl_ThreadMain, &app);

    /* --- some starter files --- */
    {
        static const char* const welcome =
            "// This file lives in the metac file system of the GTK REPL.\n"
            "// Every tab is a file, so this one can be #included by others.\n"
            "\n"
            "int add(int a, int b)\n"
            "{\n"
            "    return a + b;   // plain old C99\n"
            "}\n";
        GtkApp_AddFile(&app, "welcome.mtc", welcome);
        GtkApp_AddFile(&app, "scratch.mtc",
            "// Scratch pad -- write something here and press Ctrl+Return\n"
            "int x = 5;\n"
            "x + 1\n");
    }

    if (selfTest || selfTestFile)
    {
        char* code = 0;
        if (selfTestFile)
        {
            gchar* contents = NULL;
            gsize length = 0;
            if (g_file_get_contents(selfTestFile, &contents, &length, NULL))
                code = g_strndup(contents, length);
            g_free(contents);
        }
        if (!code)
            code = g_strdup(GtkApp_SelfTestCode);

        int result = GtkApp_SelfTest(&app, code);
        g_free(code);
        return result;
    }

    gtk_widget_show_all(app.Window);
    gtk_main();

    return 0;
}
