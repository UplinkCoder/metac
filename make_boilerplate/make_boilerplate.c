#define NO_FIBERS
#define ACCEL ACCEL_TABLE
#define OLD_PARSER 1

#include "../os/compat.h"
#include "../utils/read_file.c"
#include "../parser/metac_parser_obj.c"
#include "../driver/metac_lpp.c"

#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#if defined(_MSC_VER)
#  include <io.h>
#  define mb_dup _dup
#  define mb_dup2 _dup2
#  define mb_close _close
#else
#  include <unistd.h>
#  define mb_dup dup
#  define mb_dup2 dup2
#  define mb_close close
#endif

/// Generates the `switch(node->Kind)` bodies of MetaCNode_TreeWalk_Real
/// (parser/metac_parsetree.c) from the node structs declared in
/// parser/metac_parsetree.h.
///
///   gcc -E -P -I. parser/metac_parsetree.h | ./make_boilerplate -
///
/// Generated code goes to stdout, all diagnostics go to stderr.

#define MB_STR__(X) #X
#define MB_STR_(X) MB_STR__(X)
#define MB_TYPE(X) MB_STR_(X) "_t",
#define MB_KIND(X) MB_STR_(X),
#define MB_COUNT(A) ((uint32_t)(sizeof(A) / sizeof((A)[0])))

/// type names that may be stored in a node-pointer field
static const char* const g_nodeTypeNames[] =
{
    FOREACH_EXP(MB_TYPE)
    FOREACH_STMT_KIND(MB_TYPE)
    FOREACH_DECL_KIND(MB_TYPE)
    "metac_expr_t",
    "metac_stmt_t",
    "metac_decl_t",
    "metac_node_t",
};

/// names that are valid `case <name>:` labels inside the walker switch
static const char* const g_nodeKindNames[] =
{
    FOREACH_EXP(MB_KIND)
    FOREACH_STMT_KIND(MB_KIND)
    FOREACH_DECL_KIND(MB_KIND)
};

#undef MB_KIND
#undef MB_TYPE
#undef MB_STR_
#undef MB_STR__

static bool g_verbose = false;
static bool g_quiet = false;

#define Diag(...) \
    do { if (!g_quiet) fprintf(stderr, __VA_ARGS__); } while (0)
#define Verbose(...) \
    do { if (g_verbose && !g_quiet) fprintf(stderr, __VA_ARGS__); } while (0)

static void Usage(const char* program)
{
    fprintf(stderr,
        "usage: %s [options] FILE...\n"
        "       ... | %s -\n"
        "\n"
        "Emits the `case` bodies for MetaCNode_TreeWalk_Real\n"
        "(parser/metac_parsetree.c): first the metac_decl_t switch, then the\n"
        "metac_stmt_t switch. Generated code is written to stdout, diagnostics\n"
        "to stderr.\n"
        "\n"
        "The input is cleaned before it is parsed: lines starting with '#'\n"
        "(gcc line markers, pragmas, includes) are dropped and empty\n"
        "statements are removed, so plain `gcc -E` output can be piped in:\n"
        "\n"
        "  gcc -E -P -I. parser/metac_parsetree.h | %s - > /tmp/walker.c\n"
        "\n"
        "options:\n"
        "  -h, --help   show this help and exit\n"
        "  -v           verbose: log every struct considered and why it was skipped\n"
        "  -q           quiet: no diagnostics, only generated code\n",
        program, program, program);
}

typedef struct outbuf_t
{
    char* Data;
    uint32_t Size;
    uint32_t Capacity;
    uint32_t Cases;
} outbuf_t;

static void OutBuf_Init(outbuf_t* buf)
{
    buf->Capacity = 8192;
    buf->Size = 0;
    buf->Cases = 0;
    buf->Data = (char*) malloc(buf->Capacity);
    assert(buf->Data);
    buf->Data[0] = '\0';
}

static void OutBuf_Free(outbuf_t* buf)
{
    free(buf->Data);
    buf->Data = 0;
    buf->Size = 0;
    buf->Capacity = 0;
    buf->Cases = 0;
}

static void OutBuf_Reset(outbuf_t* buf)
{
    if (!buf->Data)
        OutBuf_Init(buf);
    buf->Size = 0;
    buf->Cases = 0;
    buf->Data[0] = '\0';
}

static void OutBuf_Appendf(outbuf_t* buf, const char* fmt, ...)
{
    va_list args, argsCopy;
    int needed;

    va_start(args, fmt);
    va_copy(argsCopy, args);
    needed = vsnprintf(0, 0, fmt, args);
    va_end(args);

    if (needed < 0)
    {
        va_end(argsCopy);
        return;
    }

    if (buf->Size + (uint32_t)needed + 1 > buf->Capacity)
    {
        uint32_t capacity = buf->Capacity ? buf->Capacity : 8192;
        while (capacity < buf->Size + (uint32_t)needed + 1)
            capacity *= 2;
        buf->Data = (char*) realloc(buf->Data, capacity);
        assert(buf->Data);
        buf->Capacity = capacity;
    }

    vsnprintf(buf->Data + buf->Size, buf->Capacity - buf->Size, fmt, argsCopy);
    va_end(argsCopy);
    buf->Size += (uint32_t)needed;
}

typedef struct make_boiler_ctx_t
{
    metac_identifier_table_t* IdentifierTable;

    outbuf_t Declarations;
    outbuf_t Statements;

    outbuf_t* Output;

    char** Emitted;
    uint32_t EmittedCount;
    uint32_t EmittedCapacity;

    uint32_t Skipped;
} make_boiler_ctx_t;

static char* MbStrdup(const char* str)
{
    size_t length = strlen(str);
    char* result = (char*) malloc(length + 1);
    memcpy(result, str, length + 1);
    return result;
}

static bool NameInList(const char* const* list, uint32_t count, const char* name)
{
    for (uint32_t i = 0; i < count; i++)
    {
        if (list[i][0] == name[0] && strcmp(list[i], name) == 0)
            return true;
    }
    return false;
}

static bool IsNodeTypeName(const char* name)
{
    return NameInList(g_nodeTypeNames, MB_COUNT(g_nodeTypeNames), name);
}

static bool IsNodeKindName(const char* name)
{
    return NameInList(g_nodeKindNames, MB_COUNT(g_nodeKindNames), name);
}

/// returns a freshly allocated copy of `name` without the trailing `_t`,
/// or 0 when `name` does not end in `_t`
static char* StripTypeSuffix(const char* name)
{
    size_t length = strlen(name);
    char* result;

    if (length < 3 || name[length - 2] != '_' || name[length - 1] != 't')
        return 0;

    result = (char*) malloc(length - 1);
    memcpy(result, name, length - 2);
    result[length - 2] = '\0';
    return result;
}

/// the type a node-pointer field points at, or 0 when the pointee is
/// something that is not a node (char*, metac_alloc_t*, metac_token_t*, ...)
static const char* NodeTypeOf(decl_type_t* type, metac_identifier_table_t* table)
{
    const char* name = 0;

    if (!type || type->Kind != decl_type_ptr)
        return 0;

    {
        decl_type_ptr_t* ptrType = cast(decl_type_ptr_t*) type;
        decl_type_t* element = ptrType->ElementType;

        if (element->Kind == decl_type && element->TypeIdentifier.v != 0)
        {
            name = IdentifierPtrToCharPtr(table, element->TypeIdentifier);
        }
        else if (element->Kind == decl_type_struct)
        {
            decl_type_struct_t* typeStruct = cast(decl_type_struct_t*) element;
            if (typeStruct->Identifier.v != 0
                && typeStruct->Identifier.v != (uint32_t) -1)
            {
                name = IdentifierPtrToCharPtr(table, typeStruct->Identifier);
            }
        }
    }

    if (name && IsNodeTypeName(name))
        return name;

    return 0;
}

static bool AlreadyEmitted(make_boiler_ctx_t* ctx, const char* caseName)
{
    for (uint32_t i = 0; i < ctx->EmittedCount; i++)
    {
        if (!strcmp(ctx->Emitted[i], caseName))
            return true;
    }
    return false;
}

static void RememberEmitted(make_boiler_ctx_t* ctx, const char* caseName)
{
    if (ctx->EmittedCount == ctx->EmittedCapacity)
    {
        uint32_t capacity = ctx->EmittedCapacity ? ctx->EmittedCapacity * 2 : 64;
        ctx->Emitted = (char**) realloc(ctx->Emitted, capacity * sizeof(char*));
        assert(ctx->Emitted);
        ctx->EmittedCapacity = capacity;
    }
    ctx->Emitted[ctx->EmittedCount++] = MbStrdup(caseName);
}

static void WriteSwitchCaseEntryFor(decl_type_struct_t* decl_type_struct,
                                    metac_identifier_table_t* table,
                                    outbuf_t* output)
{
    const char* typeName =
        IdentifierPtrToCharPtr(table, decl_type_struct->Identifier);
    char* caseName = StripTypeSuffix(typeName);

    OutBuf_Appendf(output, "        case %s:\n        {\n", caseName);
    OutBuf_Appendf(output, "            %s* %s = cast(%s*) node;\n",
                   typeName, caseName, typeName);

    for (decl_field_t* field = decl_type_struct->Fields;
         field != 0 && (metac_node_t) field != emptyNode;
         field = field->Next)
    {
        decl_variable_t* fieldVar = field->Field;
        const char* fieldName;
        const char* pointee;

        if (!fieldVar)
            continue;

        fieldName = IdentifierPtrToCharPtr(table, fieldVar->VarIdentifier);
        pointee = NodeTypeOf(fieldVar->VarType, table);
        if (!pointee)
            continue;

        OutBuf_Appendf(output,
            "            if (%s->%s && METAC_NODE(%s->%s) != emptyNode)\n"
            "                result = "
                "MetaCNode_TreeWalk_Real(%s->%s, walker_fn, ctx);\n"
            "            if(result)\n"
            "                return result;\n",
            caseName, fieldName, caseName, fieldName, caseName, fieldName);
    }

    OutBuf_Appendf(output, "        } break;\n\n");
    output->Cases++;

    free(caseName);
}

/// return 0 means keep recursing down
/// return != 0 means stop recursing
int MakeBoilerCb(metac_node_t node, void* ctxP)
{
    make_boiler_ctx_t* ctx = cast(make_boiler_ctx_t*) ctxP;

    switch (node->Kind)
    {
        case node_decl_type_struct:
        {
            decl_type_struct_t* declStruct = cast(decl_type_struct_t*) node;
            const char* structName;
            char* caseName;

            if (declStruct->Identifier.v == 0
                || declStruct->Identifier.v == (uint32_t) -1)
            {
                Verbose("skip: anonymous struct\n");
                ctx->Skipped++;
                return 1;
            }

            structName =
                IdentifierPtrToCharPtr(ctx->IdentifierTable,
                                       declStruct->Identifier);

            /// a struct without fields is a forward declaration
            /// (`typedef struct foo_t foo_t;`) - its full definition
            /// supplies the case, emitting both would duplicate the label
            if (declStruct->FieldCount == 0)
            {
                Verbose("skip: %s has no fields (forward declaration?)\n",
                        structName);
                ctx->Skipped++;
                return 1;
            }

            caseName = StripTypeSuffix(structName);
            if (!caseName)
            {
                Verbose("skip: %s does not end in '_t'\n", structName);
                ctx->Skipped++;
                return 1;
            }

            if (!IsNodeKindName(caseName))
            {
                Verbose("skip: %s is not a node kind "
                        "(no `case %s:` exists in metac_node_kind_t)\n",
                        structName, caseName);
                ctx->Skipped++;
                free(caseName);
                return 1;
            }

            if (AlreadyEmitted(ctx, caseName))
            {
                Verbose("skip: %s already emitted\n", structName);
                ctx->Skipped++;
                free(caseName);
                return 1;
            }

            if (!memcmp(caseName, "decl", 4))
            {
                ctx->Output = &ctx->Declarations;
            }
            else if (!memcmp(caseName, "stmt", 4))
            {
                ctx->Output = &ctx->Statements;
            }
            else
            {
                /// expr nodes are handled by the `default:` arm of the
                /// walker instead of individual cases
                Verbose("skip: %s is an expr node, "
                        "the walker has no expr cases\n", structName);
                ctx->Skipped++;
                free(caseName);
                return 1;
            }

            Verbose("emit: %s\n", caseName);
            RememberEmitted(ctx, caseName);
            WriteSwitchCaseEntryFor(declStruct, ctx->IdentifierTable,
                                    ctx->Output);
            free(caseName);

            /// don't traverse into the field types, they would be duplicates
            return 1;
        }
    }

    return 0;
}

typedef struct sanitize_info_t
{
    uint32_t DirectivesDropped;
    uint32_t IncludesDropped;
    uint32_t DefinesDropped;
    uint32_t EmptyStatementsRemoved;
} sanitize_info_t;

/// The metac parser has no preprocessor front end of its own:
///  * `#include` needs a file storage that is never registered
///  * `#pragma` and the `# 1 "file"` line markers emitted by gcc are
///    rejected by the lexer
///  * macros such as DECL_HEADER only expand when the file went through cpp
/// so directive lines are dropped and empty statements (`;;`, which
/// ARENA_ARRAY() leaves behind after macro expansion) are collapsed.
static char* SanitizeInput(char* text, sanitize_info_t* info)
{
    char* in = text;
    char* out = text;
    bool lastWasSemicolon = false;
    bool inString = false;
    bool inChar = false;
    bool escaped = false;

    while (*in)
    {
        char* lineStart = in;
        char* lineEnd;

        while (*in && *in != '\n')
            in++;
        lineEnd = in;
        if (*in == '\n')
            in++;

        {
            char* probe = lineStart;
            while (probe < lineEnd && (*probe == ' ' || *probe == '\t'))
                probe++;

            if (probe < lineEnd && *probe == '#')
            {
                info->DirectivesDropped++;
                if (!strncmp(probe, "#include", 8))
                    info->IncludesDropped++;
                else if (!strncmp(probe, "#define", 7))
                    info->DefinesDropped++;
                continue;
            }

            /// a line holding nothing but `;`s is an empty statement
            if (probe < lineEnd)
            {
                char* onlySemicolons = probe;
                while (onlySemicolons < lineEnd
                       && (*onlySemicolons == ';'
                           || *onlySemicolons == ' '
                           || *onlySemicolons == '\t'))
                    onlySemicolons++;
                if (onlySemicolons == lineEnd)
                {
                    info->EmptyStatementsRemoved++;
                    continue;
                }
            }
        }

        for (char* q = lineStart; q < lineEnd; q++)
        {
            char c = *q;

            if (escaped)
            {
                escaped = false;
            }
            else if (inString || inChar)
            {
                if (c == '\\')
                    escaped = true;
                else if (inString && c == '"')
                    inString = false;
                else if (inChar && c == '\'')
                    inChar = false;
            }
            else if (c == '"')
            {
                inString = true;
            }
            else if (c == '\'')
            {
                inChar = true;
            }
            else if (c == ';')
            {
                if (lastWasSemicolon)
                {
                    info->EmptyStatementsRemoved++;
                    continue;
                }
                lastWasSemicolon = true;
            }
            else if (c != ' ' && c != '\t' && c != '\r')
            {
                lastWasSemicolon = false;
            }

            *out++ = c;
        }
        if (in > lineEnd)
            *out++ = '\n';
    }
    *out = '\0';

    return text;
}

static char* ReadStream(uint32_t* lengthP)
{
    uint32_t capacity = 65536;
    uint32_t length = 0;
    char* buffer = (char*) malloc(capacity);

    for (;;)
    {
        size_t read = fread(buffer + length, 1, capacity - length, stdin);
        length += (uint32_t) read;
        if (length < capacity)
            break;
        capacity *= 2;
        buffer = (char*) realloc(buffer, capacity);
    }
    buffer[length] = '\0';
    *lengthP = length;
    return buffer;
}

/// the parser prints its own notes ("ignoring bitfield spec ...") to stdout,
/// which would end up in the generated code - park them on stderr instead
static int RedirectStdoutToStderr(int* savedFdP)
{
    fflush(stdout);
    *savedFdP = mb_dup(1);
    if (*savedFdP < 0)
        return 0;
    return mb_dup2(2, 1) == 0;
}

static void RestoreStdout(int savedFd)
{
    if (savedFd >= 0)
    {
        fflush(stdout);
        mb_dup2(savedFd, 1);
        mb_close(savedFd);
    }
}

int main(int argc, const char** argv)
{
    const char** inputs;
    int inputCount = 0;
    int result = 0;
    metac_alloc_t alloc;
    metac_lpp_t LPP;

    inputs = (const char**) malloc(sizeof(char*) * (argc > 1 ? argc : 1));

    for (int argIdx = 1; argIdx < argc; argIdx++)
    {
        const char* arg = argv[argIdx];

        if (!strcmp(arg, "-h") || !strcmp(arg, "--help"))
        {
            Usage(argv[0]);
            free(inputs);
            return 0;
        }
        else if (!strcmp(arg, "-v"))
        {
            g_verbose = true;
        }
        else if (!strcmp(arg, "-q"))
        {
            g_quiet = true;
        }
        else if (arg[0] == '-' && arg[1] != '\0')
        {
            fprintf(stderr, "%s: unknown option '%s'\n", argv[0], arg);
            Usage(argv[0]);
            free(inputs);
            return 2;
        }
        else
        {
            inputs[inputCount++] = arg;
        }
    }

    if (inputCount == 0)
    {
        Usage(argv[0]);
        free(inputs);
        return 2;
    }

    Allocator_Init(&alloc, 0);
    /// lpp stands for lexer preprocessor parser, it bundles them
    /// because they are almost always used together
    MetaCLPP_Init(&LPP, &alloc, 0);

    make_boiler_ctx_t ctx;
    memset(&ctx, 0, sizeof(ctx));
    OutBuf_Init(&ctx.Declarations);
    OutBuf_Init(&ctx.Statements);

    for (int argIdx = 0; argIdx < inputCount; argIdx++)
    {
        const char* path = inputs[argIdx];
        sanitize_info_t info;
        sanitize_info_t zeroInfo = {0, 0, 0, 0};
        char* text;
        uint32_t length;
        decl_array_t resultDecls;
        int savedFd = -1;

        info = zeroInfo;

        if (!strcmp(path, "-"))
        {
            text = ReadStream(&length);
        }
        else
        {
            read_result_t readResult = ReadFileAndZeroTerminate(path);
            if (!readResult.FileContent0)
            {
                fprintf(stderr, "%s: cannot read '%s'\n", argv[0], path);
                result = 1;
                continue;
            }
            text = readResult.FileContent0;
            length = readResult.FileLength;
        }
        (void) length;

        SanitizeInput(text, &info);

        if (info.IncludesDropped)
        {
            Diag("warning: %s: dropped %u '#include' line(s) - the metac "
                 "parser cannot resolve includes.\n"
                 "         The input looks unpreprocessed, run it through cpp "
                 "first:\n"
                 "           gcc -E -P -I. parser/metac_parsetree.h | %s -\n",
                 path, info.IncludesDropped, argv[0]);
        }
        if (info.EmptyStatementsRemoved)
        {
            Verbose("%s: removed %u empty statement(s)\n",
                    path, info.EmptyStatementsRemoved);
        }

        ctx.IdentifierTable = &LPP.Parser.IdentifierTable;
        ctx.Skipped = 0;
        OutBuf_Reset(&ctx.Declarations);
        OutBuf_Reset(&ctx.Statements);

        RedirectStdoutToStderr(&savedFd);
        resultDecls = MetaCLPP_LexParseBuffer(&LPP, text,
                                              (uint32_t) strlen(text),
                                              path, &alloc);
        RestoreStdout(savedFd);

        if (resultDecls.Length == 0)
        {
            Diag("warning: %s: no declarations parsed\n", path);
            result = 1;
            continue;
        }

        for (uint32_t i = 0; i < resultDecls.Length; i++)
        {
            metac_decl_t* decl = resultDecls.Ptr[i];
            MetaCNode_TreeWalk_Real((metac_node_t) decl,
                                    MakeBoilerCb, &ctx);
        }

        Diag("%s: %u declarations, %u decl + %u stmt case(s) emitted, "
             "%u struct(s) skipped\n",
             path, resultDecls.Length, ctx.Declarations.Cases,
             ctx.Statements.Cases, ctx.Skipped);

        printf("// ---- %s: `case` bodies for metac_decl_t ----\n"
               "// paste them into MetaCNode_TreeWalk_Real "
               "(parser/metac_parsetree.c)\n", path);
        fwrite(ctx.Declarations.Data, 1, ctx.Declarations.Size, stdout);
        printf("// ---- %s: `case` bodies for metac_stmt_t ----\n", path);
        fwrite(ctx.Statements.Data, 1, ctx.Statements.Size, stdout);
        printf("\n");

        free(text);
    }

    for (uint32_t i = 0; i < ctx.EmittedCount; i++)
        free(ctx.Emitted[i]);
    free(ctx.Emitted);
    OutBuf_Free(&ctx.Declarations);
    OutBuf_Free(&ctx.Statements);
    free(inputs);

    return result;
}
