#ifndef BC_LIGHTNING_DEBUG_H
#define BC_LIGHTNING_DEBUG_H

#include <assert.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/*
 * Writes an ELF64 image (ET_EXEC) carrying DWARF 2 for generated code.
 * .text holds the code bytes at their live address, so no relocation is needed.
 * In gdb, after the code is emitted: add-symbol-file <path>
 */

typedef enum DbgBaseType
{
    DbgBaseType_I8,
    DbgBaseType_I16,
    DbgBaseType_I32,
    DbgBaseType_I64,
    DbgBaseType_U8,
    DbgBaseType_U16,
    DbgBaseType_U32,
    DbgBaseType_U64,
    DbgBaseType_F32,
    DbgBaseType_F64,
    DbgBaseType_Count
} DbgBaseType;

typedef struct DbgFunc
{
    uint32_t fnIdx;
    uint64_t low;
    uint64_t high;
} DbgFunc;

typedef struct DbgRow
{
    uint64_t addr;
    // 1-based index into DbgInput.files
    uint32_t file;
    uint32_t line;
} DbgRow;

typedef struct DbgVar
{
    char name[48];
    uint32_t fnIdx;
    uint8_t baseType;
    uint8_t isParam;
    int32_t fpOffset;
} DbgVar;

typedef struct DbgInput
{
    const char* producer;
    const char* compDir;
    // files[0] is the primary source file of the compile unit
    const char* const* files;
    uint32_t fileCount;
    const uint8_t* code;
    uint64_t codeAddr;
    uint64_t codeSize;
    const DbgFunc* funcs;
    uint32_t funcCount;
    const DbgRow* rows;
    uint32_t rowCount;
    const DbgVar* vars;
    uint32_t varCount;
} DbgInput;

typedef struct DbgBuf
{
    uint8_t* data;
    size_t len;
    size_t cap;
} DbgBuf;

static void DbgBuf_Reserve(DbgBuf* b, size_t extra)
{
    if (b->len + extra <= b->cap)
        return;
    size_t cap = b->cap ? b->cap : 4096;
    while (cap < b->len + extra)
        cap *= 2;
    uint8_t* grown = realloc(b->data, cap);
    if (!grown)
        abort();
    b->data = grown;
    b->cap = cap;
}

static void DbgBuf_Bytes(DbgBuf* b, const void* src, size_t n)
{
    DbgBuf_Reserve(b, n);
    memcpy(b->data + b->len, src, n);
    b->len += n;
}

static void DbgBuf_UN(DbgBuf* b, uint64_t v, size_t n)
{
    DbgBuf_Reserve(b, n);
    for (size_t i = 0; i < n; i++)
        b->data[b->len + i] = i < 8 ? (uint8_t)(v >> (8 * i)) : 0;
    b->len += n;
}

static void DbgBuf_ULEB(DbgBuf* b, uint64_t v)
{
    do
    {
        uint8_t byte = v & 0x7f;
        v >>= 7;
        if (v)
            byte |= 0x80;
        DbgBuf_UN(b, byte, 1);
    } while (v);
}

static void DbgBuf_SLEB(DbgBuf* b, int64_t v)
{
    int more = 1;
    while (more)
    {
        uint8_t byte = v & 0x7f;
        v >>= 7;
        if ((v == 0 && !(byte & 0x40)) || (v == -1 && (byte & 0x40)))
            more = 0;
        else
            byte |= 0x80;
        DbgBuf_UN(b, byte, 1);
    }
}

static void DbgBuf_Str(DbgBuf* b, const char* s)
{
    DbgBuf_Bytes(b, s, strlen(s) + 1);
}

static void DbgBuf_PadTo(DbgBuf* b, size_t align)
{
    while (b->len % align)
        DbgBuf_UN(b, 0, 1);
}

static void DbgBuf_PatchU32(DbgBuf* b, size_t at, uint32_t v)
{
    for (size_t i = 0; i < 4; i++)
        b->data[at + i] = (uint8_t)(v >> (8 * i));
}

static void Dbg_Abbrev(DbgBuf* b, uint32_t code, uint32_t tag, uint8_t children,
                       const uint8_t* attrForm, size_t pairs)
{
    DbgBuf_ULEB(b, code);
    DbgBuf_ULEB(b, tag);
    DbgBuf_UN(b, children, 1);
    for (size_t i = 0; i < pairs; i++)
    {
        DbgBuf_ULEB(b, attrForm[2 * i]);
        DbgBuf_ULEB(b, attrForm[2 * i + 1]);
    }
    DbgBuf_UN(b, 0, 2);
}

// DW_AT_* / DW_FORM_* values below 0x80 are emitted as single-byte ULEBs
enum
{
    DW_TAG_compile_unit = 0x11,
    DW_TAG_base_type = 0x24,
    DW_TAG_formal_parameter = 0x05,
    DW_TAG_subprogram = 0x2e,
    DW_TAG_variable = 0x34,
    DW_CHILDREN_no = 0,
    DW_CHILDREN_yes = 1,
    DW_AT_location = 0x02,
    DW_AT_name = 0x03,
    DW_AT_byte_size = 0x0b,
    DW_AT_stmt_list = 0x10,
    DW_AT_low_pc = 0x11,
    DW_AT_high_pc = 0x12,
    DW_AT_language = 0x13,
    DW_AT_comp_dir = 0x1b,
    DW_AT_producer = 0x25,
    DW_AT_encoding = 0x3e,
    DW_AT_frame_base = 0x40,
    DW_AT_type = 0x49,
    DW_FORM_addr = 0x01,
    DW_FORM_data4 = 0x06,
    DW_FORM_data8 = 0x07,
    DW_FORM_string = 0x08,
    DW_FORM_data1 = 0x0b,
    DW_FORM_block1 = 0x0a,
    DW_FORM_ref4 = 0x13,
    DW_OP_fbreg = 0x91,
    DW_OP_reg6 = 0x56,
    DW_LANG_C99 = 0x0c
};

static void Dbg_WriteAbbrevs(DbgBuf* b)
{
    static const uint8_t cu[] = {
        DW_AT_producer, DW_FORM_string, DW_AT_language, DW_FORM_data1,
        DW_AT_name, DW_FORM_string, DW_AT_comp_dir, DW_FORM_string,
        DW_AT_low_pc, DW_FORM_addr, DW_AT_high_pc, DW_FORM_addr,
        DW_AT_stmt_list, DW_FORM_data4 };
    static const uint8_t base[] = {
        DW_AT_byte_size, DW_FORM_data1, DW_AT_encoding, DW_FORM_data1,
        DW_AT_name, DW_FORM_string };
    static const uint8_t sub[] = {
        DW_AT_name, DW_FORM_string, DW_AT_low_pc, DW_FORM_addr,
        DW_AT_high_pc, DW_FORM_addr, DW_AT_frame_base, DW_FORM_block1 };
    static const uint8_t var[] = {
        DW_AT_name, DW_FORM_string, DW_AT_type, DW_FORM_ref4,
        DW_AT_location, DW_FORM_block1 };

    Dbg_Abbrev(b, 1, DW_TAG_compile_unit, DW_CHILDREN_yes, cu, sizeof cu / 2);
    Dbg_Abbrev(b, 2, DW_TAG_base_type, DW_CHILDREN_no, base, sizeof base / 2);
    Dbg_Abbrev(b, 3, DW_TAG_subprogram, DW_CHILDREN_yes, sub, sizeof sub / 2);
    Dbg_Abbrev(b, 4, DW_TAG_variable, DW_CHILDREN_no, var, sizeof var / 2);
    Dbg_Abbrev(b, 5, DW_TAG_formal_parameter, DW_CHILDREN_no, var, sizeof var / 2);
    DbgBuf_UN(b, 0, 1);
}

static const struct { const char* name; uint8_t size; uint8_t encoding; } dbgBaseTypes[DbgBaseType_Count] = {
    {"int8_t", 1, 5}, {"int16_t", 2, 5}, {"int32_t", 4, 5}, {"int64_t", 8, 5},
    {"uint8_t", 1, 8}, {"uint16_t", 2, 8}, {"uint32_t", 4, 8}, {"uint64_t", 8, 8},
    {"float", 4, 4}, {"double", 8, 4}
};

static void Dbg_Section(DbgBuf* sh, uint32_t name, uint32_t type, uint64_t flags, uint64_t addr,
                        uint64_t off, uint64_t size, uint32_t link, uint32_t info, uint64_t align, uint64_t entsize)
{
    DbgBuf_UN(sh, name, 4);
    DbgBuf_UN(sh, type, 4);
    DbgBuf_UN(sh, flags, 8);
    DbgBuf_UN(sh, addr, 8);
    DbgBuf_UN(sh, off, 8);
    DbgBuf_UN(sh, size, 8);
    DbgBuf_UN(sh, link, 4);
    DbgBuf_UN(sh, info, 4);
    DbgBuf_UN(sh, align, 8);
    DbgBuf_UN(sh, entsize, 8);
}

static void Dbg_FreeBuf(DbgBuf* b)
{
    free(b->data);
    b->data = NULL;
    b->len = b->cap = 0;
}

// returns 0 on success
static int DbgWriteElf(const char* path, const DbgInput* in)
{
    DbgBuf abbrev = {0}, info = {0}, line = {0}, symtab = {0}, strtab = {0}, shstr = {0}, out = {0}, sh = {0}, hdr = {0}, ph = {0};

    Dbg_WriteAbbrevs(&abbrev);

    size_t unitStart = info.len;
    DbgBuf_UN(&info, 0, 4);
    DbgBuf_UN(&info, 2, 2);
    DbgBuf_UN(&info, 0, 4);
    DbgBuf_UN(&info, 8, 1);

    DbgBuf_ULEB(&info, 1);
    DbgBuf_Str(&info, in->producer);
    DbgBuf_UN(&info, DW_LANG_C99, 1);
    DbgBuf_Str(&info, in->files[0]);
    DbgBuf_Str(&info, in->compDir);
    DbgBuf_UN(&info, in->codeAddr, 8);
    DbgBuf_UN(&info, in->codeAddr + in->codeSize, 8);
    DbgBuf_UN(&info, 0, 4);

    // type refs are relative to the unit header, as ref4 requires
    uint32_t typeRef[DbgBaseType_Count];
    for (uint32_t i = 0; i < DbgBaseType_Count; i++)
    {
        typeRef[i] = (uint32_t)(info.len - unitStart);
        DbgBuf_ULEB(&info, 2);
        DbgBuf_UN(&info, dbgBaseTypes[i].size, 1);
        DbgBuf_UN(&info, dbgBaseTypes[i].encoding, 1);
        DbgBuf_Str(&info, dbgBaseTypes[i].name);
    }

    for (uint32_t f = 0; f < in->funcCount; f++)
    {
        const DbgFunc* fn = &in->funcs[f];
        char fnName[32];
        snprintf(fnName, sizeof fnName, "bc_fn_%u", fn->fnIdx);

        DbgBuf_ULEB(&info, 3);
        DbgBuf_Str(&info, fnName);
        DbgBuf_UN(&info, fn->low, 8);
        DbgBuf_UN(&info, fn->high, 8);
        DbgBuf_UN(&info, 1, 1);
        DbgBuf_UN(&info, DW_OP_reg6, 1);

        for (uint32_t v = 0; v < in->varCount; v++)
        {
            const DbgVar* var = &in->vars[v];
            if (var->fnIdx != fn->fnIdx)
                continue;

            // frame slots are addressed relative to rbp, the frame base
            DbgBuf loc = {0};
            DbgBuf_UN(&loc, DW_OP_fbreg, 1);
            DbgBuf_SLEB(&loc, -(int64_t)var->fpOffset);

            DbgBuf_ULEB(&info, var->isParam ? 5 : 4);
            DbgBuf_Str(&info, var->name);
            DbgBuf_UN(&info, typeRef[var->baseType], 4);
            DbgBuf_UN(&info, loc.len, 1);
            DbgBuf_Bytes(&info, loc.data, loc.len);
            Dbg_FreeBuf(&loc);
        }
        DbgBuf_UN(&info, 0, 1);
    }
    DbgBuf_UN(&info, 0, 1);
    DbgBuf_PatchU32(&info, unitStart, (uint32_t)(info.len - unitStart - 4));

    size_t lineStart = line.len;
    DbgBuf_UN(&line, 0, 4);
    DbgBuf_UN(&line, 2, 2);
    size_t hdrLenPos = line.len;
    DbgBuf_UN(&line, 0, 4);
    size_t hdrStart = line.len;
    DbgBuf_UN(&line, 1, 1);
    DbgBuf_UN(&line, 1, 1);
    DbgBuf_UN(&line, (uint8_t)-5, 1);
    DbgBuf_UN(&line, 14, 1);
    DbgBuf_UN(&line, 13, 1);
    static const uint8_t stdOpLengths[12] = {0, 1, 1, 1, 1, 0, 0, 0, 1, 0, 0, 1};
    DbgBuf_Bytes(&line, stdOpLengths, sizeof stdOpLengths);
    // no include directories
    DbgBuf_UN(&line, 0, 1);
    for (uint32_t i = 0; i < in->fileCount; i++)
    {
        DbgBuf_Str(&line, in->files[i]);
        DbgBuf_ULEB(&line, 0);
        DbgBuf_ULEB(&line, 0);
        DbgBuf_ULEB(&line, 0);
    }
    DbgBuf_UN(&line, 0, 1);
    DbgBuf_PatchU32(&line, hdrLenPos, (uint32_t)(line.len - hdrStart));

    if (in->rowCount)
    {
        uint64_t prevAddr = in->rows[0].addr;
        uint32_t prevLine = 1;
        uint32_t prevFile = 1;
        DbgBuf_UN(&line, 0, 1);
        DbgBuf_ULEB(&line, 9);
        DbgBuf_UN(&line, 2, 1);
        DbgBuf_UN(&line, prevAddr, 8);
        for (uint32_t r = 0; r < in->rowCount; r++)
        {
            const DbgRow* row = &in->rows[r];
            if (row->addr < prevAddr || row->file == 0 || row->line == 0)
                continue;
            if (row->file != prevFile)
            {
                DbgBuf_UN(&line, 4, 1);
                DbgBuf_ULEB(&line, row->file);
                prevFile = row->file;
            }
            if (row->addr != prevAddr)
            {
                DbgBuf_UN(&line, 2, 1);
                DbgBuf_ULEB(&line, row->addr - prevAddr);
                prevAddr = row->addr;
            }
            if (row->line != prevLine)
            {
                DbgBuf_UN(&line, 3, 1);
                DbgBuf_SLEB(&line, (int64_t)row->line - (int64_t)prevLine);
                prevLine = row->line;
            }
            DbgBuf_UN(&line, 1, 1);
        }
        uint64_t end = in->codeAddr + in->codeSize;
        if (end > prevAddr)
        {
            DbgBuf_UN(&line, 2, 1);
            DbgBuf_ULEB(&line, end - prevAddr);
        }
        DbgBuf_UN(&line, 0, 1);
        DbgBuf_ULEB(&line, 1);
        DbgBuf_UN(&line, 1, 1);
    }
    DbgBuf_PatchU32(&line, lineStart, (uint32_t)(line.len - lineStart - 4));

    // symbol 0 is the null symbol required by ELF
    DbgBuf_UN(&strtab, 0, 1);
    DbgBuf_UN(&symtab, 0, 24);
    for (uint32_t f = 0; f < in->funcCount; f++)
    {
        const DbgFunc* fn = &in->funcs[f];
        char fnName[32];
        snprintf(fnName, sizeof fnName, "bc_fn_%u", fn->fnIdx);
        DbgBuf_UN(&symtab, strtab.len, 4);
        DbgBuf_Str(&strtab, fnName);
        DbgBuf_UN(&symtab, (1 << 4) | 2, 1);
        DbgBuf_UN(&symtab, 0, 1);
        DbgBuf_UN(&symtab, 1, 2);
        DbgBuf_UN(&symtab, fn->low, 8);
        DbgBuf_UN(&symtab, fn->high - fn->low, 8);
    }

    DbgBuf_UN(&shstr, 0, 1);
    uint32_t nText = (uint32_t)shstr.len; DbgBuf_Str(&shstr, ".text");
    uint32_t nAbbrev = (uint32_t)shstr.len; DbgBuf_Str(&shstr, ".debug_abbrev");
    uint32_t nInfo = (uint32_t)shstr.len; DbgBuf_Str(&shstr, ".debug_info");
    uint32_t nLine = (uint32_t)shstr.len; DbgBuf_Str(&shstr, ".debug_line");
    uint32_t nSymtab = (uint32_t)shstr.len; DbgBuf_Str(&shstr, ".symtab");
    uint32_t nStrtab = (uint32_t)shstr.len; DbgBuf_Str(&shstr, ".strtab");
    uint32_t nShstr = (uint32_t)shstr.len; DbgBuf_Str(&shstr, ".shstrtab");

    // ELF header followed by one PT_LOAD program header, so gdb accepts the file as an executable
    DbgBuf_UN(&out, 0, 120);
    DbgBuf_PadTo(&out, 16);
    uint64_t textOff = out.len;
    DbgBuf_Bytes(&out, in->code, (size_t)in->codeSize);
    uint64_t abbrevOff = out.len;
    DbgBuf_Bytes(&out, abbrev.data, abbrev.len);
    uint64_t infoOff = out.len;
    DbgBuf_Bytes(&out, info.data, info.len);
    uint64_t lineOff = out.len;
    DbgBuf_Bytes(&out, line.data, line.len);
    DbgBuf_PadTo(&out, 8);
    uint64_t symOff = out.len;
    DbgBuf_Bytes(&out, symtab.data, symtab.len);
    uint64_t strOff = out.len;
    DbgBuf_Bytes(&out, strtab.data, strtab.len);
    uint64_t shstrOff = out.len;
    DbgBuf_Bytes(&out, shstr.data, shstr.len);
    DbgBuf_PadTo(&out, 8);
    uint64_t shOff = out.len;

    // section indices: 0 null, 1 .text, 2 .debug_abbrev, 3 .debug_info, 4 .debug_line, 5 .symtab, 6 .strtab, 7 .shstrtab
    Dbg_Section(&sh, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
    Dbg_Section(&sh, nText, 1, 6, in->codeAddr, textOff, in->codeSize, 0, 0, 16, 0);
    Dbg_Section(&sh, nAbbrev, 1, 0, 0, abbrevOff, abbrev.len, 0, 0, 1, 0);
    Dbg_Section(&sh, nInfo, 1, 0, 0, infoOff, info.len, 0, 0, 1, 0);
    Dbg_Section(&sh, nLine, 1, 0, 0, lineOff, line.len, 0, 0, 1, 0);
    Dbg_Section(&sh, nSymtab, 2, 0, 0, symOff, symtab.len, 6, 1, 8, 24);
    Dbg_Section(&sh, nStrtab, 3, 0, 0, strOff, strtab.len, 0, 0, 1, 0);
    Dbg_Section(&sh, nShstr, 3, 0, 0, shstrOff, shstr.len, 0, 0, 1, 0);
    DbgBuf_Bytes(&out, sh.data, sh.len);

    // ELF header, written into the reserved first 64 bytes
    DbgBuf_UN(&hdr, 0x464c457f, 4);
    DbgBuf_UN(&hdr, 2, 1);
    DbgBuf_UN(&hdr, 1, 1);
    DbgBuf_UN(&hdr, 1, 1);
    DbgBuf_UN(&hdr, 0, 9);
    DbgBuf_UN(&hdr, 2, 2);
    DbgBuf_UN(&hdr, 62, 2);
    DbgBuf_UN(&hdr, 1, 4);
    DbgBuf_UN(&hdr, 0, 8);
    DbgBuf_UN(&hdr, 64, 8);
    DbgBuf_UN(&hdr, shOff, 8);
    DbgBuf_UN(&hdr, 0, 4);
    DbgBuf_UN(&hdr, 64, 2);
    DbgBuf_UN(&hdr, 56, 2);
    DbgBuf_UN(&hdr, 1, 2);
    DbgBuf_UN(&hdr, 64, 2);
    DbgBuf_UN(&hdr, 8, 2);
    DbgBuf_UN(&hdr, 7, 2);
    memcpy(out.data, hdr.data, 64);

    DbgBuf_UN(&ph, 1, 4);
    DbgBuf_UN(&ph, 5, 4);
    DbgBuf_UN(&ph, textOff, 8);
    DbgBuf_UN(&ph, in->codeAddr, 8);
    DbgBuf_UN(&ph, in->codeAddr, 8);
    DbgBuf_UN(&ph, in->codeSize, 8);
    DbgBuf_UN(&ph, in->codeSize, 8);
    DbgBuf_UN(&ph, 1, 8);
    memcpy(out.data + 64, ph.data, 56);

    int result = -1;
    FILE* f = fopen(path, "wb");
    if (f)
    {
        result = fwrite(out.data, 1, out.len, f) == out.len ? 0 : -1;
        fclose(f);
    }

    Dbg_FreeBuf(&abbrev);
    Dbg_FreeBuf(&info);
    Dbg_FreeBuf(&line);
    Dbg_FreeBuf(&symtab);
    Dbg_FreeBuf(&strtab);
    Dbg_FreeBuf(&shstr);
    Dbg_FreeBuf(&out);
    Dbg_FreeBuf(&sh);
    Dbg_FreeBuf(&hdr);
    Dbg_FreeBuf(&ph);
    return result;
}

/*
 * Recorder: collects files, line marks, function ranges and variables while code is generated.
 * Marks and ranges hold opaque nodes; Dbg_Write turns them into addresses via a caller callback.
 */

typedef struct DbgMark
{
    void* node;
    uint32_t file;
    uint32_t line;
} DbgMark;

typedef struct DbgFuncRange
{
    uint32_t fnIdx;
    void* start;
    void* end;
} DbgFuncRange;

typedef struct DbgRecorder
{
    DbgMark* marks;
    uint32_t markCount;
    uint32_t markCap;
    DbgFuncRange* funcs;
    uint32_t funcCount;
    uint32_t funcCap;
    DbgVar* vars;
    uint32_t varCount;
    uint32_t varCap;
    char** files;
    uint32_t fileCount;
    uint32_t fileCap;
    // 1-based index into files, 0 when no file is set
    uint32_t currentFile;
} DbgRecorder;

static void* Dbg_Grow(void* arr, uint32_t* cap, uint32_t count, size_t elemSize)
{
    if (count < *cap)
        return arr;
    uint32_t newCap = *cap ? *cap * 2 : 64;
    void* grown = realloc(arr, newCap * elemSize);
    assert(grown);
    *cap = newCap;
    return grown;
}

static void Dbg_RecorderInit(DbgRecorder* r)
{
    memset(r, 0, sizeof *r);
}

static void Dbg_RecorderFree(DbgRecorder* r)
{
    for (uint32_t i = 0; i < r->fileCount; i++)
        free(r->files[i]);
    free(r->files);
    free(r->marks);
    free(r->funcs);
    free(r->vars);
    Dbg_RecorderInit(r);
}

// returns the 1-based file index, 0 for NULL
static uint32_t Dbg_SetFile(DbgRecorder* r, const char* filename)
{
    if (!filename)
    {
        r->currentFile = 0;
        return 0;
    }
    for (uint32_t i = 0; i < r->fileCount; i++)
    {
        if (strcmp(r->files[i], filename) == 0)
            return r->currentFile = i + 1;
    }
    r->files = Dbg_Grow(r->files, &r->fileCap, r->fileCount, sizeof(char*));
    size_t len = strlen(filename) + 1;
    char* copy = malloc(len);
    assert(copy);
    memcpy(copy, filename, len);
    r->files[r->fileCount++] = copy;
    return r->currentFile = r->fileCount;
}

static int Dbg_IsActive(const DbgRecorder* r)
{
    return r->currentFile != 0;
}

// node is the code position of this line; it is resolved to an address by Dbg_Write
static void Dbg_AddMark(DbgRecorder* r, void* node, uint32_t line)
{
    if (!Dbg_IsActive(r))
        return;
    r->marks = Dbg_Grow(r->marks, &r->markCap, r->markCount, sizeof(DbgMark));
    r->marks[r->markCount].node = node;
    r->marks[r->markCount].file = r->currentFile;
    r->marks[r->markCount].line = line;
    r->markCount++;
}

static void Dbg_BeginFunc(DbgRecorder* r, uint32_t fnIdx, void* start)
{
    r->funcs = Dbg_Grow(r->funcs, &r->funcCap, r->funcCount, sizeof(DbgFuncRange));
    r->funcs[r->funcCount].fnIdx = fnIdx;
    r->funcs[r->funcCount].start = start;
    r->funcs[r->funcCount].end = NULL;
    r->funcCount++;
}

static void Dbg_EndFunc(DbgRecorder* r, void* end)
{
    if (r->funcCount)
        r->funcs[r->funcCount - 1].end = end;
}

static void Dbg_AddVar(DbgRecorder* r, uint32_t fnIdx, const char* name, uint8_t baseType, int32_t fpOffset, uint8_t isParam)
{
    if (!name)
        return;
    r->vars = Dbg_Grow(r->vars, &r->varCap, r->varCount, sizeof(DbgVar));
    DbgVar* v = &r->vars[r->varCount++];
    memset(v, 0, sizeof *v);
    snprintf(v->name, sizeof v->name, "%s", name);
    v->fnIdx = fnIdx;
    v->baseType = baseType;
    v->isParam = isParam;
    v->fpOffset = fpOffset;
}

// code is the live code buffer; addrOf(ctx, node) maps a recorded node to its address. returns 0 on success
static int Dbg_Write(const DbgRecorder* r, const char* path, const uint8_t* code, uint64_t codeSize,
                     uint64_t (*addrOf)(void* ctx, void* node), void* ctx)
{
    if (!r->fileCount)
        return -1;

    DbgRow* rows = malloc(sizeof(DbgRow) * (r->markCount + 1));
    assert(rows);
    for (uint32_t i = 0; i < r->markCount; i++)
    {
        rows[i].addr = addrOf(ctx, r->marks[i].node);
        rows[i].file = r->marks[i].file;
        rows[i].line = r->marks[i].line;
    }

    DbgFunc* funcs = malloc(sizeof(DbgFunc) * (r->funcCount + 1));
    assert(funcs);
    uint32_t funcCount = 0;
    for (uint32_t i = 0; i < r->funcCount; i++)
    {
        if (!r->funcs[i].end)
            continue;
        funcs[funcCount].fnIdx = r->funcs[i].fnIdx;
        funcs[funcCount].low = addrOf(ctx, r->funcs[i].start);
        funcs[funcCount].high = addrOf(ctx, r->funcs[i].end);
        funcCount++;
    }

    DbgInput in;
    memset(&in, 0, sizeof in);
    in.producer = "metac bc_lightning_backend";
    in.compDir = ".";
    in.files = (const char* const*) r->files;
    in.fileCount = r->fileCount;
    in.code = code;
    in.codeAddr = (uint64_t)(uintptr_t) code;
    in.codeSize = codeSize;
    in.funcs = funcs;
    in.funcCount = funcCount;
    in.rows = rows;
    in.rowCount = r->markCount;
    in.vars = r->vars;
    in.varCount = r->varCount;

    int result = DbgWriteElf(path, &in);
    free(rows);
    free(funcs);
    return result;
}

#endif
