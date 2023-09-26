
typedef signed char int8_t;
typedef unsigned char uint8_t;
typedef signed short int int16_t;
typedef unsigned short int uint16_t;
typedef signed int int32_t;
typedef unsigned int uint32_t;
typedef signed long int int64_t;
typedef unsigned long int uint64_t;

typedef struct FILE FILE;

typedef struct thrd_t *thrd_t;
extern FILE* stderr;

void *malloc(uint32_t size);
void free(void *ptr);
void *calloc(uint32_t nmemb, uint32_t size);
void *realloc(void *ptr, uint32_t size);

void perror(const char* prefix);

FILE* fopen(const char* path, const char* mode);
void fseek(FILE* f, long offset, int fseek_whence);
long ftell(FILE* f);
uint32_t fread(void *ptr, uint32_t elementSize, uint32_t nElements, FILE *stream);
uint32_t fwrite(const void *ptr, uint32_t elementSize, uint32_t nElements, FILE *stream);
int fclose(FILE* f);

int fprintf(FILE* f, const char *format, ...);
int printf(const char* fmt, ...);
int sprintf(char* buffer, const char* fmt, ...);
int snprintf(char *str, uint32_t size, const char *format, ...);

void *memcpy(void *dest, const void *src,uint32_t n);
void *memset(void *s, int c, uint32_t n);
int memcmp(const void *s1, const void *s2, uint32_t n);
void *memchr(const void *s, int c, uint32_t n);

uint32_t strlen(const char *s);

char *realpath(const char *path, char *resolved_path);

typedef struct read_result_t {
    char* FileContent0;
    uint32_t FileLength;
} read_result_t;

read_result_t ReadFileAndZeroTerminate(const char* path)
{
    read_result_t result = {(char*)0, 0};

    printf("Trying to open %s ... \n", path);
    FILE* fd = fopen(path, "rb");

    if(!fd)
    {
        perror("Error Reading File: ");
    }
    else
    {
        fseek(fd, 0,

                    2

                            );
        result.FileLength = ftell(fd);
        fseek(fd, 0,

                    0

                            );
        uint32_t aligned_size =
            (((result.FileLength + 1) + 3) & ~3);

        result.FileContent0 = (char*) malloc(aligned_size);

        uint32_t read_size = fread(result.FileContent0,
                                 1, result.FileLength, fd);
        for(uint32_t p = result.FileLength;
            p < aligned_size;
            p++)
        {
            result.FileContent0[p] = '\0';
        }

        fclose(fd);
    }

    return result;
}





uint32_t crc32c_nozero(uint32_t crc, const void* s, const uint32_t len_p);
uint32_t crc32c_byte(uint32_t crc, uint8_t byte);


typedef struct metac_identifier_ptr_t
{
    uint32_t v;
} metac_identifier_ptr_t;

extern const metac_identifier_ptr_t empty_identifier;

typedef struct metac_identifier_table_slot_t
{
    uint32_t HashKey;
    metac_identifier_ptr_t Ptr;
} metac_identifier_table_slot_t;


typedef struct metac_identifier_table_t
{
    metac_identifier_table_slot_t* Slots;
    char* StringMemory;

    uint32_t StringMemorySize;
    uint32_t StringMemoryCapacity;
    uint32_t SlotCount_Log2;
    uint32_t SlotsUsed;

    uint32_t LengthShift;
    uint32_t MaxDisplacement;
} metac_identifier_table_t;


typedef struct identifier_table_file_header_t
{
    uint32_t NumberOfSlots;
    uint32_t SizeofSlot;
    uint32_t OffsetSlots;
    uint32_t OffsetStrings;

    uint32_t StringMemorySize;
    uint32_t Version;
    uint32_t HeaderCommentSize;
    uint32_t LengthShift;
} identifier_table_file_header_t;


void IdentifierTableInit(metac_identifier_table_t* table, uint32_t lengthShift, uint32_t slotCountLog2);

metac_identifier_ptr_t GetOrAddIdentifier(metac_identifier_table_t* table,
                                          uint32_t identifierKey,
                                          const char* identifier);

metac_identifier_ptr_t IsIdentifierInTable(metac_identifier_table_t* table, uint32_t key,
                                           const char* idChars);



_Bool

    IsInTable(metac_identifier_table_t* table,
               uint32_t key, metac_identifier_ptr_t value);

metac_identifier_table_slot_t* IdentifierTableLookup(
            metac_identifier_table_t* table,
            uint32_t key, metac_identifier_ptr_t value);


const char* IdentifierPtrToCharPtr(const metac_identifier_table_t* table,
                                   metac_identifier_ptr_t ptr);

metac_identifier_table_t ReadTable(const char* filename);
void WriteTable(metac_identifier_table_t* table, const char* filename, uint32_t lengthShift, const char* comment);
metac_identifier_table_slot_t* findFirstEntry(metac_identifier_table_t* table);


typedef uint32_t block_idx_t;
typedef uint16_t crc32c_lower16_t;

typedef struct metac_lexer_state_t
{
    const char * Text;

    uint32_t Column;
    uint32_t Line;
    uint32_t Position;
    uint32_t Size;

    block_idx_t OuterBlock;
    uint16_t SourceId;
} metac_lexer_state_t;

typedef enum metac_token_enum_t
{
    tok_invalid, tok_identifier, tok_uint, tok_string, tok_char, tok_char_uni, tok_comment_single, tok_comment_multi, tok_macro_parameter, tok_bang, tok_question, tok_hash, tok_at, tok_lParen, tok_rParen, tok_lBrace, tok_rBrace, tok_lBracket, tok_rBracket, tok_semicolon, tok_colon, tok_dollar, tok_cat, tok_comma, tok_dot, tok_plus, tok_minus, tok_star, tok_div, tok_rem, tok_xor, tok_or, tok_and, tok_lsh, tok_rsh, tok_oror, tok_andand, tok_arrow, tok_dotdot, tok_assign, tok_add_ass, tok_sub_ass, tok_mul_ass, tok_div_ass, tok_rem_ass, tok_xor_ass, tok_or_ass, tok_and_ass, tok_lsh_ass, tok_rsh_ass, tok_equals_equals, tok_not_equal, tok_lt, tok_le, tok_gt, tok_ge, tok_spaceship, tok_dotdotdot, tok_kw_struct, tok_kw_union, tok_kw_enum, tok_kw_typedef, tok_kw_auto, tok_kw_void, tok_kw_bool, tok_kw_char, tok_kw_short, tok_kw_int, tok_kw_long, tok_kw_uint32_t, tok_kw_float, tok_kw_double, tok_kw_signed, tok_kw_unsigned, tok_kw_const, tok_kw_volatile, tok_kw___shared, tok_kw_extern, tok_kw_for, tok_kw_sizeof, tok_kw_return, tok_kw_switch, tok_kw_while, tok_kw_do, tok_kw_typeof, tok_kw_inject, tok_kw_eject, tok_kw_assert, tok_kw_case, tok_kw_default, tok_kw_goto, tok_kw_static, tok_kw_inline, tok_kw_if, tok_kw_else, tok_kw_break, tok_kw_continue, tok_kw_until, tok_kw_yield, tok_kw___attribute__, tok_comment_begin_multi, tok_comment_end_multi, tok_comment_begin_single, tok_plusplus, tok_minusminus, tok_full_slice, tok_hashhash, tok_newline, tok_eof, tok_error,
} metac_token_enum_t;



typedef struct metac_token_t {
    metac_token_enum_t TokenType;

    uint32_t Position;
    uint32_t LocationId;

    union {
        uint32_t Key;

        struct {
            uint32_t IdentifierKey;
            metac_identifier_ptr_t IdentifierPtr;
        };

        struct {
            uint32_t StringKey;
            metac_identifier_ptr_t StringPtr;
        };

        struct {
            uint32_t CommentLength;
            const char* CommentBegin;
        };

        struct {
            uint32_t charLength;
            char chars[8];
        };
        struct {
            union {
                uint64_t ValueU64;
                int64_t ValueI64;
                uint32_t ValueU32;
                int32_t ValueI32;
                float ValueF23;
                double ValueF52;
            };
            uint32_t ValueLength;
        };

        uint32_t MacroParameterIndex;
    };
} metac_token_t;

typedef struct metac_token_buffer_t
{
    metac_token_t* Ptr;
    uint32_t Length;
    uint32_t Capacity;
} metac_token_buffer_t;

uint32_t MetaCTokenLength(metac_token_t token);

typedef struct metac_location_t
{
    uint32_t StartLine;

    uint16_t LineSpan;
    uint16_t StartColumn;
    uint16_t EndColumn;
    uint16_t SourceId;
} metac_location_t;


typedef struct metac_location_t_array
{
    metac_location_t* Locations;

    uint32_t LocationSize;
    uint32_t LocationCapacity;
} metac_location_t_array;

typedef uint32_t metac_location_ptr;

typedef struct metac_lexer_t {


    metac_token_t* Tokens;
    uint32_t TokenCount;
    uint32_t TokenCapacity;

    metac_location_t_array LocationStorage;

    metac_token_t inlineTokens[16];
    metac_location_t inlineLocations[16];

    metac_identifier_table_t IdentifierTable;
    metac_identifier_table_t StringTable;
} metac_lexer_t;


const char* MetaCTokenEnum_toChars(metac_token_enum_t tok);

void MetaCLexer_Init(metac_lexer_t* self);

void MetaCLocation_Expand(metac_location_t* self, metac_location_t endLoc);
void MetaCLocationStorage_Init(metac_location_t_array* self);

metac_location_ptr MetaCLocationStorage_StartLoc(
        metac_location_t_array* self,
        uint32_t line, uint16_t column);

void MetaCLocationStorage_EndLoc(
        metac_location_t_array* self,
        metac_location_ptr locationId,
        uint32_t line, uint16_t column);

metac_location_t MetaCLocationStorage_FromPair(metac_location_t_array *srcStorage,
                                               metac_location_ptr startLocIdx,
                                               metac_location_ptr endLocIdx);

metac_location_ptr MetaCLocationStorage_Store(metac_location_t_array* self,
                                              metac_location_t loc);

metac_lexer_state_t MetaCLexerStateFromString(uint32_t sourceId, const char* str);
metac_lexer_state_t MetaCLexerStateFromBuffer(uint32_t sourceId, const char* buffer, uint32_t bufferLength);
metac_token_t* MetaCLexerLexNextToken(metac_lexer_t* self, metac_lexer_state_t* state,
                                      const char* text, uint32_t len);
void MetaCLexer_Free(metac_lexer_t* self);











uint32_t crc32c_nozero(uint32_t crc, const void* s, const uint32_t len_p);







const unsigned int crc32Table[256] = {
    0x00000000U, 0xF26B8303U, 0xE13B70F7U, 0x1350F3F4U,
    0xC79A971FU, 0x35F1141CU, 0x26A1E7E8U, 0xD4CA64EBU,
    0x8AD958CFU, 0x78B2DBCCU, 0x6BE22838U, 0x9989AB3BU,
    0x4D43CFD0U, 0xBF284CD3U, 0xAC78BF27U, 0x5E133C24U,
    0x105EC76FU, 0xE235446CU, 0xF165B798U, 0x030E349BU,
    0xD7C45070U, 0x25AFD373U, 0x36FF2087U, 0xC494A384U,
    0x9A879FA0U, 0x68EC1CA3U, 0x7BBCEF57U, 0x89D76C54U,
    0x5D1D08BFU, 0xAF768BBCU, 0xBC267848U, 0x4E4DFB4BU,
    0x20BD8EDEU, 0xD2D60DDDU, 0xC186FE29U, 0x33ED7D2AU,
    0xE72719C1U, 0x154C9AC2U, 0x061C6936U, 0xF477EA35U,
    0xAA64D611U, 0x580F5512U, 0x4B5FA6E6U, 0xB93425E5U,
    0x6DFE410EU, 0x9F95C20DU, 0x8CC531F9U, 0x7EAEB2FAU,
    0x30E349B1U, 0xC288CAB2U, 0xD1D83946U, 0x23B3BA45U,
    0xF779DEAEU, 0x05125DADU, 0x1642AE59U, 0xE4292D5AU,
    0xBA3A117EU, 0x4851927DU, 0x5B016189U, 0xA96AE28AU,
    0x7DA08661U, 0x8FCB0562U, 0x9C9BF696U, 0x6EF07595U,
    0x417B1DBCU, 0xB3109EBFU, 0xA0406D4BU, 0x522BEE48U,
    0x86E18AA3U, 0x748A09A0U, 0x67DAFA54U, 0x95B17957U,
    0xCBA24573U, 0x39C9C670U, 0x2A993584U, 0xD8F2B687U,
    0x0C38D26CU, 0xFE53516FU, 0xED03A29BU, 0x1F682198U,
    0x5125DAD3U, 0xA34E59D0U, 0xB01EAA24U, 0x42752927U,
    0x96BF4DCCU, 0x64D4CECFU, 0x77843D3BU, 0x85EFBE38U,
    0xDBFC821CU, 0x2997011FU, 0x3AC7F2EBU, 0xC8AC71E8U,
    0x1C661503U, 0xEE0D9600U, 0xFD5D65F4U, 0x0F36E6F7U,
    0x61C69362U, 0x93AD1061U, 0x80FDE395U, 0x72966096U,
    0xA65C047DU, 0x5437877EU, 0x4767748AU, 0xB50CF789U,
    0xEB1FCBADU, 0x197448AEU, 0x0A24BB5AU, 0xF84F3859U,
    0x2C855CB2U, 0xDEEEDFB1U, 0xCDBE2C45U, 0x3FD5AF46U,
    0x7198540DU, 0x83F3D70EU, 0x90A324FAU, 0x62C8A7F9U,
    0xB602C312U, 0x44694011U, 0x5739B3E5U, 0xA55230E6U,
    0xFB410CC2U, 0x092A8FC1U, 0x1A7A7C35U, 0xE811FF36U,
    0x3CDB9BDDU, 0xCEB018DEU, 0xDDE0EB2AU, 0x2F8B6829U,
    0x82F63B78U, 0x709DB87BU, 0x63CD4B8FU, 0x91A6C88CU,
    0x456CAC67U, 0xB7072F64U, 0xA457DC90U, 0x563C5F93U,
    0x082F63B7U, 0xFA44E0B4U, 0xE9141340U, 0x1B7F9043U,
    0xCFB5F4A8U, 0x3DDE77ABU, 0x2E8E845FU, 0xDCE5075CU,
    0x92A8FC17U, 0x60C37F14U, 0x73938CE0U, 0x81F80FE3U,
    0x55326B08U, 0xA759E80BU, 0xB4091BFFU, 0x466298FCU,
    0x1871A4D8U, 0xEA1A27DBU, 0xF94AD42FU, 0x0B21572CU,
    0xDFEB33C7U, 0x2D80B0C4U, 0x3ED04330U, 0xCCBBC033U,
    0xA24BB5A6U, 0x502036A5U, 0x4370C551U, 0xB11B4652U,
    0x65D122B9U, 0x97BAA1BAU, 0x84EA524EU, 0x7681D14DU,
    0x2892ED69U, 0xDAF96E6AU, 0xC9A99D9EU, 0x3BC21E9DU,
    0xEF087A76U, 0x1D63F975U, 0x0E330A81U, 0xFC588982U,
    0xB21572C9U, 0x407EF1CAU, 0x532E023EU, 0xA145813DU,
    0x758FE5D6U, 0x87E466D5U, 0x94B49521U, 0x66DF1622U,
    0x38CC2A06U, 0xCAA7A905U, 0xD9F75AF1U, 0x2B9CD9F2U,
    0xFF56BD19U, 0x0D3D3E1AU, 0x1E6DCDEEU, 0xEC064EEDU,
    0xC38D26C4U, 0x31E6A5C7U, 0x22B65633U, 0xD0DDD530U,
    0x0417B1DBU, 0xF67C32D8U, 0xE52CC12CU, 0x1747422FU,
    0x49547E0BU, 0xBB3FFD08U, 0xA86F0EFCU, 0x5A048DFFU,
    0x8ECEE914U, 0x7CA56A17U, 0x6FF599E3U, 0x9D9E1AE0U,
    0xD3D3E1ABU, 0x21B862A8U, 0x32E8915CU, 0xC083125FU,
    0x144976B4U, 0xE622F5B7U, 0xF5720643U, 0x07198540U,
    0x590AB964U, 0xAB613A67U, 0xB831C993U, 0x4A5A4A90U,
    0x9E902E7BU, 0x6CFBAD78U, 0x7FAB5E8CU, 0x8DC0DD8FU,
    0xE330A81AU, 0x115B2B19U, 0x020BD8EDU, 0xF0605BEEU,
    0x24AA3F05U, 0xD6C1BC06U, 0xC5914FF2U, 0x37FACCF1U,
    0x69E9F0D5U, 0x9B8273D6U, 0x88D28022U, 0x7AB90321U,
    0xAE7367CAU, 0x5C18E4C9U, 0x4F48173DU, 0xBD23943EU,
    0xF36E6F75U, 0x0105EC76U, 0x12551F82U, 0xE03E9C81U,
    0x34F4F86AU, 0xC69F7B69U, 0xD5CF889DU, 0x27A40B9EU,
    0x79B737BAU, 0x8BDCB4B9U, 0x988C474DU, 0x6AE7C44EU,
    0xBE2DA0A5U, 0x4C4623A6U, 0x5F16D052U, 0xAD7D5351U
};

static inline uint32_t singletable_crc32c(uint32_t crc, const uint8_t *buf, uint32_t size)
{
    const uint8_t *p = buf;

    while (size--)
        crc = crc32Table[(crc ^ *p++) & 0xff] ^ (crc >> 8);

    return crc;
}

uint32_t crc32c(uint32_t crc, const void* s, const uint32_t len_p)
{
    const uint32_t len = len_p;
    const uint8_t* p = (const uint8_t*) s;



    crc = singletable_crc32c(crc, p, len);

    return crc;
}

uint32_t crc32c_nozero(uint32_t crc, const void* s, const uint32_t len_p)
{
    crc = crc32c(crc, s, len_p);

    return crc ? crc : ~0;
}

inline uint32_t crc32c_byte(uint32_t crc, uint8_t byte)
{



    return crc32Table[(crc ^ byte) & 0xFF] ^ (crc >> 8);

}



































typedef enum scope_kind_t
{
    scope_exit
} scope_kind_t;







typedef enum metac_expression_kind_t
{
    exp_invalid, exp_identifier, exp_string, exp_char, exp_signed_integer, exp_increment, exp_decrement, exp_post_increment, exp_post_decrement, exp_typeof, exp_sizeof, exp_inject, exp_eject, exp_assert, exp_outer, exp_unary_dot, exp_addr, exp_ptr, exp_not, exp_compl, exp_umin, exp_paren, exp_tuple, exp_ternary, exp_cast, exp_comma, exp_dot, exp_add, exp_sub, exp_mul, exp_div, exp_rem, exp_xor, exp_or, exp_and, exp_lsh, exp_rsh, exp_oror, exp_andand, exp_arrow, exp_dotdot, exp_assign, exp_add_ass, exp_sub_ass, exp_mul_ass, exp_div_ass, exp_rem_ass, exp_xor_ass, exp_or_ass, exp_and_ass, exp_lsh_ass, exp_rsh_ass, exp_eq, exp_neq, exp_lt, exp_le, exp_gt, exp_ge, exp_spaceship, exp_full_slice, exp_slice, exp_index, exp_call, exp_argument, exp_type, exp_variable, exp_addr_or_and, exp_ptr_or_mul, exp_dot_compiler, exp_dot_context, exp_dot_target, exp_max,
} metac_expression_kind_t;

typedef enum metac_statement_kind_t
{
    stmt_min = exp_max + 1,

    stmt_block, stmt_if, stmt_switch, stmt_while, stmt_for, stmt_do_while, stmt_label, stmt_case, stmt_break, stmt_yield, stmt_scope, stmt_continue, stmt_goto, stmt_return, stmt_exp, stmt_decl, stmt_comment,

    stmt_max
} metac_statement_kind_t;

typedef enum metac_declaration_kind_t
{
    decl_min = stmt_max + 1,

    decl_variable, decl_field, decl_parameter, decl_enum_member, decl_type, decl_type_struct, decl_type_union, decl_type_enum, decl_type_array, decl_type_ptr, decl_type_functiontype, decl_type_tuple, decl_type_typedef, decl_function, decl_label, decl_comment,

    decl_max
} metac_declaration_kind_t;

typedef enum metac_node_kind_t
{
    node_exp_invalid = exp_invalid, node_exp_identifier = exp_identifier, node_exp_string = exp_string, node_exp_char = exp_char, node_exp_signed_integer = exp_signed_integer, node_exp_increment = exp_increment, node_exp_decrement = exp_decrement, node_exp_post_increment = exp_post_increment, node_exp_post_decrement = exp_post_decrement, node_exp_typeof = exp_typeof, node_exp_sizeof = exp_sizeof, node_exp_inject = exp_inject, node_exp_eject = exp_eject, node_exp_assert = exp_assert, node_exp_outer = exp_outer, node_exp_unary_dot = exp_unary_dot, node_exp_addr = exp_addr, node_exp_ptr = exp_ptr, node_exp_not = exp_not, node_exp_compl = exp_compl, node_exp_umin = exp_umin, node_exp_paren = exp_paren, node_exp_tuple = exp_tuple, node_exp_ternary = exp_ternary, node_exp_cast = exp_cast, node_exp_comma = exp_comma, node_exp_dot = exp_dot, node_exp_add = exp_add, node_exp_sub = exp_sub, node_exp_mul = exp_mul, node_exp_div = exp_div, node_exp_rem = exp_rem, node_exp_xor = exp_xor, node_exp_or = exp_or, node_exp_and = exp_and, node_exp_lsh = exp_lsh, node_exp_rsh = exp_rsh, node_exp_oror = exp_oror, node_exp_andand = exp_andand, node_exp_arrow = exp_arrow, node_exp_dotdot = exp_dotdot, node_exp_assign = exp_assign, node_exp_add_ass = exp_add_ass, node_exp_sub_ass = exp_sub_ass, node_exp_mul_ass = exp_mul_ass, node_exp_div_ass = exp_div_ass, node_exp_rem_ass = exp_rem_ass, node_exp_xor_ass = exp_xor_ass, node_exp_or_ass = exp_or_ass, node_exp_and_ass = exp_and_ass, node_exp_lsh_ass = exp_lsh_ass, node_exp_rsh_ass = exp_rsh_ass, node_exp_eq = exp_eq, node_exp_neq = exp_neq, node_exp_lt = exp_lt, node_exp_le = exp_le, node_exp_gt = exp_gt, node_exp_ge = exp_ge, node_exp_spaceship = exp_spaceship, node_exp_full_slice = exp_full_slice, node_exp_slice = exp_slice, node_exp_index = exp_index, node_exp_call = exp_call, node_exp_argument = exp_argument, node_exp_type = exp_type, node_exp_variable = exp_variable, node_exp_addr_or_and = exp_addr_or_and, node_exp_ptr_or_mul = exp_ptr_or_mul, node_exp_dot_compiler = exp_dot_compiler, node_exp_dot_context = exp_dot_context, node_exp_dot_target = exp_dot_target, node_exp_max = exp_max, node_stmt_block = stmt_block, node_stmt_if = stmt_if, node_stmt_switch = stmt_switch, node_stmt_while = stmt_while, node_stmt_for = stmt_for, node_stmt_do_while = stmt_do_while, node_stmt_label = stmt_label, node_stmt_case = stmt_case, node_stmt_break = stmt_break, node_stmt_yield = stmt_yield, node_stmt_scope = stmt_scope, node_stmt_continue = stmt_continue, node_stmt_goto = stmt_goto, node_stmt_return = stmt_return, node_stmt_exp = stmt_exp, node_stmt_decl = stmt_decl, node_stmt_comment = stmt_comment, node_decl_variable = decl_variable, node_decl_field = decl_field, node_decl_parameter = decl_parameter, node_decl_enum_member = decl_enum_member, node_decl_type = decl_type, node_decl_type_struct = decl_type_struct, node_decl_type_union = decl_type_union, node_decl_type_enum = decl_type_enum, node_decl_type_array = decl_type_array, node_decl_type_ptr = decl_type_ptr, node_decl_type_functiontype = decl_type_functiontype, node_decl_type_tuple = decl_type_tuple, node_decl_type_typedef = decl_type_typedef, node_decl_function = decl_function, node_decl_label = decl_label, node_decl_comment = decl_comment,
    node_max
} metac_node_kind_t;



typedef enum metac_binary_expression_kind_t
{
    bin_exp_invalid = (exp_comma - 1),

    bin_exp_comma, bin_exp_dot, bin_exp_add, bin_exp_sub, bin_exp_mul, bin_exp_div, bin_exp_rem, bin_exp_xor, bin_exp_or, bin_exp_and, bin_exp_lsh, bin_exp_rsh, bin_exp_oror, bin_exp_andand, bin_exp_arrow, bin_exp_dotdot, bin_exp_assign, bin_exp_add_ass, bin_exp_sub_ass, bin_exp_mul_ass, bin_exp_div_ass, bin_exp_rem_ass, bin_exp_xor_ass, bin_exp_or_ass, bin_exp_and_ass, bin_exp_lsh_ass, bin_exp_rsh_ass, bin_exp_eq, bin_exp_neq, bin_exp_lt, bin_exp_le, bin_exp_gt, bin_exp_ge, bin_exp_spaceship,
} metac_binary_expression_kind_t;

typedef struct metac_node_header_t
{
    metac_node_kind_t Kind;
    uint32_t LocationIdx;
    uint32_t Hash;
    uint32_t Serial;
} metac_node_header_t;

typedef metac_node_header_t* metac_node_t;


struct metac_sema_declaration_t;

typedef enum metac_scope_parent_kind_t
{
    scope_parent_unknown = 0x0,


    scope_parent_module = 0x1,

    scope_parent_function = 0x2,
    scope_parent_struct = 0x3,
    scope_parent_statement = 0x4,
    scope_parent_block = 0x5,

    scope_parent_union = 0x6,

    scope_parent_extended = 0x7,


    scope_parent_invalid = 0xF

} metac_scope_parent_kind_t;


typedef struct metac_scope_parent_t
{
    union {
        uint32_t v;
        struct {
            uint32_t Index : 28;
            metac_scope_parent_kind_t Kind : 4;
        };
    };
} metac_scope_parent_t;

typedef enum scope_insert_error_t
{
    success, identifier_exists_already, table_full, no_scope,
} scope_insert_error_t;

typedef struct metac_scope_ptr_t
{
    uint32_t v;
} metac_scope_ptr_t;

typedef struct metac_scope_table_slot_t
{
    uint32_t Hash;
    metac_identifier_ptr_t Ptr;

    struct metac_node_header_t* Node;
} metac_scope_table_slot_t;


typedef struct metac_scope_table_t
{
    metac_scope_table_slot_t* Slots;
    uint32_t SlotCount_Log2;
    uint32_t SlotsUsed;
} metac_scope_table_t;


typedef enum metac_scope_flags_t
{
    scope_flag_none,

    scope_flag_temporary = (1 << 0),


    scope_flag_mounted = (1 << 1),


    scope_flag_closed = (1 << 2),

    scope_flag_max = (1 << 3)
} metac_scope_flags_t;

typedef struct metac_scope_t
{
    metac_scope_flags_t scopeFlags;
    metac_scope_parent_t Owner;
    struct metac_scope_t* Parent;
    union {
        metac_scope_table_t ScopeTable;
        const struct metac_scope_t* MountedScope;
    } ;
    uint32_t Serial;



   _Bool

        Closed;
} metac_scope_t;



metac_node_header_t* MetaCScope_LookupIdentifier(metac_scope_t* self,
                                                 uint32_t idPtrHash,
                                                 metac_identifier_ptr_t identifierPtr);


scope_insert_error_t MetaCScope_RegisterIdentifier(metac_scope_t* self,
                                                   metac_identifier_ptr_t idPtr,
                                                   metac_node_t node);










typedef enum metac_type_index_kind_t
{
    type_index_unknown = 0x0,

    type_index_basic = 0x1,

    type_index_enum = 0x2,

    type_index_ptr = 0x3,
    type_index_array = 0x4,

    type_index_struct = 0x5,
    type_index_union = 0x6,
    type_index_class = 0x7,

    type_index_map = 0x8,

    type_index_functiontype = 0x9,

    type_index_typedef = 0xA,

    type_index_tuple = 0xB,



    type_index_extended = 0xE,
    type_index_invalid = 0xF
} metac_type_index_kind_t;

typedef struct metac_type_index_t
{
    union {
        uint32_t v;
        struct {
            uint32_t Index : 28;
            metac_type_index_kind_t Kind : 4;
        };
    };
} metac_type_index_t;

typedef struct metac_type_header_t
{
    metac_declaration_kind_t Kind; uint32_t LocationIdx; uint32_t Hash; uint32_t Serial;
} metac_type_header_t;

typedef metac_type_header_t* metac_type_t;

typedef struct metac_type_aggregate_field_t
{
    metac_type_header_t Header;

    metac_type_index_t Type;
    uint32_t Offset;

    metac_identifier_ptr_t Identifier;
    uint32_t AggregateIndex;
} metac_type_aggregate_field_t;

typedef struct metac_type_aggregate_t
{
    metac_type_header_t Header;

    metac_identifier_ptr_t Identifier;

    uint16_t FieldCount;
    uint16_t Alignment;

    uint32_t Size;

    metac_scope_t* Scope;

    metac_type_aggregate_field_t* Fields;
} metac_type_aggregate_t;

typedef struct metac_enum_member_t
{
    metac_type_header_t Header;

    metac_type_index_t Type;
    metac_identifier_ptr_t Identifier;

    struct metac_sema_expression_t* Value;
} metac_enum_member_t;

typedef struct metac_type_enum_t
{
    metac_type_header_t Header;

    metac_identifier_ptr_t Name;

    metac_enum_member_t* Members;

    uint32_t MemberCount;
} metac_type_enum_t;

typedef struct metac_type_array_t
{
    metac_type_header_t Header;

    metac_type_index_t ElementType;

    uint32_t Dim;
} metac_type_array_t;

typedef struct metac_type_ptr_t
{
    metac_type_header_t Header;

    metac_type_index_t ElementType;
} metac_type_ptr_t;

typedef struct metac_type_functiontype_t
{
    metac_type_header_t Header;

    metac_type_index_t ReturnType;

    metac_type_index_t* ParameterTypes;

    uint32_t ParameterTypeCount;
} metac_type_functiontype_t;

typedef struct metac_type_typedef_t
{
    metac_type_header_t Header;

    metac_type_index_t Type;

    metac_identifier_ptr_t Identifier;
} metac_type_typedef_t;

typedef struct metac_type_tuple_t
{
    metac_type_header_t Header;

    metac_type_index_t* typeIndicies;

    uint32_t typeCount;
} metac_type_tuple_t;

typedef enum metac_type_kind_t
{
    type_invalid,

    type_struct,
    type_union,
    type_class,
    type_enum,

    type_typedef,
    type_functiontype,

    type_auto,


    type_void,
    type_bool,
    type_char,
    type_short,
    type_int,
    type_long,
    type_uint32_t,

    type_float,
    type_double,

    type_long_long,
    type_long_double,

    type_unsigned_char,
    type_unsigned_short,
    type_unsigned_int,
    type_unsigned_long,

    type_unsigned_long_long,

    type_type,
    type_identifier,

    type_ptr,
    type_array,

    type_map,

    type_tuple,

    type_max
} metac_type_kind_t;




uint32_t EntangleInts(uint32_t a, uint32_t b);
uint32_t UntangleInts(uint32_t tangled);

typedef struct metac_type_table_slot_t
{
    metac_declaration_kind_t Kind; uint32_t LocationIdx; uint32_t Hash; uint32_t Serial;
} metac_type_table_slot_t;

typedef struct metac_type_table_t
{
    metac_type_table_slot_t* Slots;

    uint32_t SlotCount_Log2;
    uint32_t SlotsUsed;

    uint32_t MaxDisplacement;
    metac_type_index_kind_t Kind;
} metac_type_table_t;






_Bool

    Expression_IsEqual_(const struct metac_sema_expression_t* a,
                         const struct metac_sema_expression_t* b);

static inline const

                   _Bool

                        EnumSlotsEqual(const metac_type_table_slot_t* a,
                                  const metac_type_table_slot_t* b)
{


   _Bool

        result =

                 1

                     ;
    metac_type_enum_t* slotA = (metac_type_enum_t*) a;
    metac_type_enum_t* slotB = (metac_type_enum_t*) b;
    if (slotA->MemberCount == slotB->MemberCount)
    {
        uint32_t count = slotA->MemberCount;
        for(uint32_t i = 0; i < count; i++)
        {
            if (slotA->Members[i].Identifier.v != slotB->Members[i].Identifier.v
             || !(slotA->Members[i].Value == slotB->Members[i].Value ?

                1

                : Expression_IsEqual_(slotA->Members[i].Value, slotB->Members[i].Value)))
            {
                result =

                        0

                             ;
                break;
            }
        }
    }
    else
    {
        result =

                0

                     ;
    }

    return result;
}

static inline const

                   _Bool

                        ArraySlotsEqual(const metac_type_table_slot_t* a,
                                   const metac_type_table_slot_t* b)
{
    metac_type_array_t* slotA = (metac_type_array_t*) a;
    metac_type_array_t* slotB = (metac_type_array_t*) b;
    return (slotA->ElementType.v == slotB->ElementType.v
         && slotA->Dim == slotB->Dim);
}

static inline const

                   _Bool

                        AggregateSlotsEqual(const metac_type_table_slot_t* a,
                                       const metac_type_table_slot_t* b)
{


   _Bool

        result =

                 1

                     ;

    metac_type_aggregate_t* slotA =
        (metac_type_aggregate_t*) a;

    metac_type_aggregate_t* slotB =
        (metac_type_aggregate_t*) b;

    if (slotA->FieldCount == slotB->FieldCount)
    {
        const uint32_t fieldCount = slotA->FieldCount;
        for(uint32_t i = 0; i < fieldCount; i++)
        {
            const metac_type_aggregate_field_t* fieldsA =
                slotA->Fields + i;
            const metac_type_aggregate_field_t* fieldsB =
                slotB->Fields + i;

            if (fieldsA->Identifier.v != fieldsB->Identifier.v
             || fieldsA->Type.v != fieldsB->Type.v)
            {
                result =

                        0

                             ;
                break;
            }
        }
    }
    else
    {
        result =

                0

                     ;
    }
    return result;
}

static inline const

                   _Bool

                        PtrSlotsEqual(const metac_type_table_slot_t* a,
                                 const metac_type_table_slot_t* b)
{
    metac_type_ptr_t* slotA = (metac_type_ptr_t*) a;
    metac_type_ptr_t* slotB = (metac_type_ptr_t*) b;
    return (slotA->ElementType.v == slotB->ElementType.v);
}

static inline const

                   _Bool

                        FunctiontypeSlotsEqual(const metac_type_table_slot_t* a,
                                          const metac_type_table_slot_t* b)
{


   _Bool

        result =

                 1

                     ;
    metac_type_functiontype_t* slotA =
        (metac_type_functiontype_t*) a;
    metac_type_functiontype_t* slotB =
        (metac_type_functiontype_t*) b;


    if (slotA->ReturnType.v == slotB->ReturnType.v &&
        slotA->ParameterTypeCount == slotB->ParameterTypeCount)
    {
        const uint32_t parameterTypeCount = slotA->ParameterTypeCount;
        for(uint32_t i = 0; i < parameterTypeCount; i++)
        {
            const metac_type_index_t* parameterTypesA =
                slotA->ParameterTypes;
            const metac_type_index_t* parameterTypesB =
                slotB->ParameterTypes;
            if (parameterTypesA[i].v != parameterTypesB[i].v)
            {
                result =

                        0

                             ;
                break;
            }
        }
    }
    else
    {
        result =

                0

                     ;
    }
    return result;
}

static inline const

                   _Bool

                        TypedefSlotsEqual(const metac_type_table_slot_t* a,
                                     const metac_type_table_slot_t* b)
{
    metac_type_typedef_t* slotA = (metac_type_typedef_t*) a;
    metac_type_typedef_t* slotB = (metac_type_typedef_t*) b;
    return (slotA->Identifier.v == slotB->Identifier.v
        && slotA->Type.v == slotB->Type.v);
}


static inline const

                   _Bool

                        TupleSlotsEqual(const metac_type_table_slot_t* a,
                                   const metac_type_table_slot_t* b)
{
   metac_type_tuple_t* slotA = (metac_type_tuple_t*) a;
   metac_type_tuple_t* slotB = (metac_type_tuple_t*) b;


  _Bool

       result =

                0

                     ;

   if (slotA->typeCount == slotB->typeCount)
   {
       result =

               1

                   ;
       const uint32_t typeCount = slotA->typeCount;
       for(uint32_t i = 0; i < typeCount; i++)
       {
           if (slotA->typeIndicies[i].v != slotB->typeIndicies[i].v)
           {
               result =

                       0

                            ;
               break;
           }
       }
   }

   return result;
}

typedef struct metac_type_table_enum_t { metac_type_enum_t* Slots; uint32_t SlotCount_Log2; uint32_t SlotsUsed; uint32_t MaxDisplacement; metac_type_index_kind_t Kind; } metac_type_table_enum_t; typedef struct metac_type_table_array_t { metac_type_array_t* Slots; uint32_t SlotCount_Log2; uint32_t SlotsUsed; uint32_t MaxDisplacement; metac_type_index_kind_t Kind; } metac_type_table_array_t; typedef struct metac_type_table_aggregate_t { metac_type_aggregate_t* Slots; uint32_t SlotCount_Log2; uint32_t SlotsUsed; uint32_t MaxDisplacement; metac_type_index_kind_t Kind; } metac_type_table_aggregate_t; typedef struct metac_type_table_ptr_t { metac_type_ptr_t* Slots; uint32_t SlotCount_Log2; uint32_t SlotsUsed; uint32_t MaxDisplacement; metac_type_index_kind_t Kind; } metac_type_table_ptr_t; typedef struct metac_type_table_functiontype_t { metac_type_functiontype_t* Slots; uint32_t SlotCount_Log2; uint32_t SlotsUsed; uint32_t MaxDisplacement; metac_type_index_kind_t Kind; } metac_type_table_functiontype_t; typedef struct metac_type_table_typedef_t { metac_type_typedef_t* Slots; uint32_t SlotCount_Log2; uint32_t SlotsUsed; uint32_t MaxDisplacement; metac_type_index_kind_t Kind; } metac_type_table_typedef_t; typedef struct metac_type_table_tuple_t { metac_type_tuple_t* Slots; uint32_t SlotCount_Log2; uint32_t SlotsUsed; uint32_t MaxDisplacement; metac_type_index_kind_t Kind; } metac_type_table_tuple_t;

metac_type_index_t MetaCTypeTable_GetOrEmptyEnumType (const metac_type_table_enum_t* table, metac_type_enum_t* key); metac_type_index_t MetaCTypeTable_GetOrEmptyArrayType (const metac_type_table_array_t* table, metac_type_array_t* key); metac_type_index_t MetaCTypeTable_GetOrEmptyStructType (const metac_type_table_aggregate_t* table, metac_type_aggregate_t* key); metac_type_index_t MetaCTypeTable_GetOrEmptyUnionType (const metac_type_table_aggregate_t* table, metac_type_aggregate_t* key); metac_type_index_t MetaCTypeTable_GetOrEmptyPtrType (const metac_type_table_ptr_t* table, metac_type_ptr_t* key); metac_type_index_t MetaCTypeTable_GetOrEmptyFunctionType (const metac_type_table_functiontype_t* table, metac_type_functiontype_t* key); metac_type_index_t MetaCTypeTable_GetOrEmptyTypedefType (const metac_type_table_typedef_t* table, metac_type_typedef_t* key); metac_type_index_t MetaCTypeTable_GetOrEmptyTupleType (const metac_type_table_tuple_t* table, metac_type_tuple_t* key);

metac_type_index_t MetaCTypeTable_AddEnumType (metac_type_table_enum_t* table, const metac_type_enum_t* key); metac_type_index_t MetaCTypeTable_AddArrayType (metac_type_table_array_t* table, const metac_type_array_t* key); metac_type_index_t MetaCTypeTable_AddStructType (metac_type_table_aggregate_t* table, const metac_type_aggregate_t* key); metac_type_index_t MetaCTypeTable_AddUnionType (metac_type_table_aggregate_t* table, const metac_type_aggregate_t* key); metac_type_index_t MetaCTypeTable_AddPtrType (metac_type_table_ptr_t* table, const metac_type_ptr_t* key); metac_type_index_t MetaCTypeTable_AddFunctionType (metac_type_table_functiontype_t* table, const metac_type_functiontype_t* key); metac_type_index_t MetaCTypeTable_AddTypedefType (metac_type_table_typedef_t* table, const metac_type_typedef_t* key); metac_type_index_t MetaCTypeTable_AddTupleType (metac_type_table_tuple_t* table, const metac_type_tuple_t* key);







void TypeTableInitImpl(metac_type_table_t* table, const uint32_t sizeof_slot, metac_type_index_kind_t kind);















typedef struct metac_expression_header_t
{
    metac_expression_kind_t Kind; uint32_t LocationIdx; uint32_t Hash; uint32_t Serial;
} metac_expression_header_t;

typedef struct exp_argument_t
{
    metac_expression_kind_t Kind; uint32_t LocationIdx; uint32_t Hash; uint32_t Serial;

    struct metac_expression_t* Expression;
    struct exp_argument_t* Next;
} exp_argument_t;

typedef struct exp_tuple_t
{
    metac_expression_kind_t Kind; uint32_t LocationIdx; uint32_t Hash; uint32_t Serial;

    struct metac_expression_t* Expression;
    struct exp_tuple_t* Next;
} exp_tuple_t;

typedef struct metac_expression_t
{
    metac_expression_kind_t Kind; uint32_t LocationIdx; uint32_t Hash; uint32_t Serial;

    union
    {




        struct {
            struct metac_expression_t* _E1;
            struct metac_expression_t* E2;
        };

        struct {
            struct metac_expression_t* _E1_;
            struct metac_expression_t* _E2;
            struct metac_expression_t* Econd;
        };

        struct {
            struct metac_expression_t* E1;
        };

        struct {
            struct metac_expression_t* SizeofExp;
        };

        struct {
            struct exp_tuple_t* TupleExpressionList;
            uint32_t TupleExpressionCount;
        };

        struct {
            struct metac_expression_t* CastExp;
            struct decl_type_t* CastType;
        };

        struct {
            struct decl_type_t* TypeExp;
        };


        exp_argument_t* ArgumentList;

        struct {
            uint32_t IdentifierKey;

            metac_identifier_ptr_t IdentifierPtr;



        };

        struct {
            uint32_t StringKey;


            metac_identifier_ptr_t StringPtr;



        };

        struct {
            uint32_t CharKey;
            char Chars[8];
        };


        int64_t ValueI64;

        uint64_t ValueU64;
    };
} metac_expression_t;

typedef struct statement_header_t
{
    metac_statement_kind_t StmtKind; uint32_t LocationIdx; uint32_t Hash; uint32_t Serial; struct metac_statement_t* Next;
} statement_header_t;

typedef struct stmt_block_t
{
    metac_statement_kind_t StmtKind; uint32_t LocationIdx; uint32_t Hash; uint32_t Serial; struct metac_statement_t* Next;

    struct metac_statement_t* Body;
    uint32_t StatementCount;
} stmt_block_t;

typedef struct stmt_break_t
{
    metac_statement_kind_t StmtKind; uint32_t LocationIdx; uint32_t Hash; uint32_t Serial; struct metac_statement_t* Next;
} stmt_break_t;

typedef struct stmt_continue_t
{
    metac_statement_kind_t StmtKind; uint32_t LocationIdx; uint32_t Hash; uint32_t Serial; struct metac_statement_t* Next;
} stmt_continue_t;

typedef struct stmt_yield_t
{
    metac_statement_kind_t StmtKind; uint32_t LocationIdx; uint32_t Hash; uint32_t Serial; struct metac_statement_t* Next;

    metac_expression_t* YieldExp;
} stmt_yield_t;

typedef struct stmt_scope_t
{
    metac_statement_kind_t StmtKind; uint32_t LocationIdx; uint32_t Hash; uint32_t Serial; struct metac_statement_t* Next;

    scope_kind_t ScopeKind;
    struct metac_statement_t* Stmt;
} stmt_scope_t;

typedef struct stmt_defer_t
{
    metac_statement_kind_t StmtKind; uint32_t LocationIdx; uint32_t Hash; uint32_t Serial; struct metac_statement_t* Next;

    struct metac_statement_t* Stmt;
} stmt_defer_t;

typedef struct stmt_for_t
{
    metac_statement_kind_t StmtKind; uint32_t LocationIdx; uint32_t Hash; uint32_t Serial; struct metac_statement_t* Next;


    metac_node_t ForInit;
    metac_expression_t* ForCond;
    metac_expression_t* ForPostLoop;

    struct metac_statement_t* ForBody;
} stmt_for_t;

typedef struct stmt_while_t
{
    metac_statement_kind_t StmtKind; uint32_t LocationIdx; uint32_t Hash; uint32_t Serial; struct metac_statement_t* Next;

    metac_expression_t* WhileExp;
    struct metac_statement_t* WhileBody;
} stmt_while_t;

typedef struct stmt_case_t
{
    metac_statement_kind_t StmtKind; uint32_t LocationIdx; uint32_t Hash; uint32_t Serial; struct metac_statement_t* Next;

    metac_expression_t* CaseExp;
    struct metac_statement_t* CaseBody;
} stmt_case_t;

typedef struct stmt_goto_t
{
    metac_statement_kind_t StmtKind; uint32_t LocationIdx; uint32_t Hash; uint32_t Serial; struct metac_statement_t* Next;

    metac_identifier_ptr_t GotoLabel;
} stmt_goto_t;

typedef struct stmt_exp_t
{
    metac_statement_kind_t StmtKind; uint32_t LocationIdx; uint32_t Hash; uint32_t Serial; struct metac_statement_t* Next;

    metac_expression_t* Expression;
} stmt_exp_t;

typedef struct stmt_decl_t
{
    metac_statement_kind_t StmtKind; uint32_t LocationIdx; uint32_t Hash; uint32_t Serial; struct metac_statement_t* Next;

    struct metac_declaration_t* Declaration;
} stmt_decl_t;

typedef struct stmt_if_t
{
    metac_statement_kind_t StmtKind; uint32_t LocationIdx; uint32_t Hash; uint32_t Serial; struct metac_statement_t* Next;

    struct metac_expression_t* IfCond;
    struct metac_statement_t* IfBody;
    struct metac_statement_t* ElseBody;
} stmt_if_t;

typedef struct stmt_label_t
{
    metac_statement_kind_t StmtKind; uint32_t LocationIdx; uint32_t Hash; uint32_t Serial; struct metac_statement_t* Next;

    metac_identifier_ptr_t Label;
} stmt_label_t;

typedef struct stmt_return_t
{
    metac_statement_kind_t StmtKind; uint32_t LocationIdx; uint32_t Hash; uint32_t Serial; struct metac_statement_t* Next;

    metac_expression_t* ReturnExp;
} stmt_return_t;

typedef struct stmt_switch_t
{
    metac_statement_kind_t StmtKind; uint32_t LocationIdx; uint32_t Hash; uint32_t Serial; struct metac_statement_t* Next;

    metac_expression_t* SwitchExp;
    struct metac_statement_t* SwitchBody;
} stmt_switch_t;

typedef struct stmt_do_while_t
{
    metac_statement_kind_t StmtKind; uint32_t LocationIdx; uint32_t Hash; uint32_t Serial; struct metac_statement_t* Next;

    metac_expression_t* DoWhileExp;
    struct metac_statement_t* DoWhileBody;
} stmt_do_while_t;

typedef struct stmt_comment_t
{
    metac_statement_kind_t StmtKind; uint32_t LocationIdx; uint32_t Hash; uint32_t Serial; struct metac_statement_t* Next;

    const char* Text;
    uint32_t Length;
} stmt_comment_t;




typedef struct decl_type_tuple_t decl_type_tuple_t;

typedef struct metac_statement_t
{
    union
    {
        struct {
            metac_statement_kind_t StmtKind; uint32_t LocationIdx; uint32_t Hash; uint32_t Serial; struct metac_statement_t* Next;
        };

        stmt_block_t stmt_block; stmt_if_t stmt_if; stmt_switch_t stmt_switch; stmt_while_t stmt_while; stmt_for_t stmt_for; stmt_do_while_t stmt_do_while; stmt_label_t stmt_label; stmt_case_t stmt_case; stmt_break_t stmt_break; stmt_yield_t stmt_yield; stmt_scope_t stmt_scope; stmt_continue_t stmt_continue; stmt_goto_t stmt_goto; stmt_return_t stmt_return; stmt_exp_t stmt_exp; stmt_decl_t stmt_decl; stmt_comment_t stmt_comment;
    };
} metac_statement_t;

typedef enum metac_type_modifiers
{
    typemod_none,

    typemod_const = (1 << 0),
    typemod_unsigned = (1 << 1),

} metac_type_modifiers;






typedef struct decl_type_t
{
    union {
        struct {
            metac_declaration_kind_t DeclKind; uint32_t LocationIdx; uint32_t Hash; uint32_t Serial; uint32_t AllocLine;
        };
        metac_type_header_t TypeHeader;
    };
    metac_type_kind_t TypeKind; metac_type_modifiers TypeModifiers;


    metac_identifier_ptr_t TypeIdentifier;
} decl_type_t;

typedef struct decl_comment_t
{
    metac_declaration_kind_t DeclKind; uint32_t LocationIdx; uint32_t Hash; uint32_t Serial; uint32_t AllocLine;

    const char* Text;
    uint32_t Length;
} decl_comment_t;

typedef struct decl_label_t
{
    metac_declaration_kind_t DeclKind; uint32_t LocationIdx; uint32_t Hash; uint32_t Serial; uint32_t AllocLine;

    metac_identifier_ptr_t Identifier;

    struct metac_declaration_t* Decl;
} decl_label_t;

typedef struct decl_variable_t
{
    metac_declaration_kind_t DeclKind; uint32_t LocationIdx; uint32_t Hash; uint32_t Serial; uint32_t AllocLine;

    decl_type_t* VarType;

    metac_identifier_ptr_t VarIdentifier;

    metac_expression_t* VarInitExpression;

} decl_variable_t;


typedef struct decl_field_t
{
    metac_declaration_kind_t DeclKind; uint32_t LocationIdx; uint32_t Hash; uint32_t Serial; uint32_t AllocLine;

    decl_variable_t* Field;

    struct decl_field_t* Next;
} decl_field_t;


typedef struct decl_parameter_t
{
    metac_declaration_kind_t DeclKind; uint32_t LocationIdx; uint32_t Hash; uint32_t Serial; uint32_t AllocLine;

    decl_variable_t* Parameter;

    struct decl_parameter_t* Next;
} decl_parameter_t;

typedef struct decl_parameter_list_t
{
    decl_parameter_t* List;
    uint32_t ParameterCount;


   _Bool

        IsVariadic;
} decl_parameter_list_t;

typedef struct decl_function_t
{
    metac_declaration_kind_t DeclKind; uint32_t LocationIdx; uint32_t Hash; uint32_t Serial; uint32_t AllocLine;

    decl_type_t* ReturnType;

    decl_parameter_t* Parameters;

    uint32_t ParameterCount;
    metac_identifier_ptr_t Identifier;

    stmt_block_t* FunctionBody;
} decl_function_t;

typedef struct decl_type_ptr_t
{
    metac_declaration_kind_t DeclKind; uint32_t LocationIdx; uint32_t Hash; uint32_t Serial; uint32_t AllocLine;

    metac_type_kind_t TypeKind; metac_type_modifiers TypeModifiers;

    decl_type_t* ElementType;
} decl_type_ptr_t;

typedef struct decl_enum_member_t
{
    metac_declaration_kind_t DeclKind; uint32_t LocationIdx; uint32_t Hash; uint32_t Serial; uint32_t AllocLine;

    metac_identifier_ptr_t Name;

    metac_expression_t* Value;

    struct decl_enum_member_t* Next;
} decl_enum_member_t;

typedef struct decl_type_enum_t
{
    metac_declaration_kind_t DeclKind; uint32_t LocationIdx; uint32_t Hash; uint32_t Serial; uint32_t AllocLine;

    metac_type_kind_t TypeKind; metac_type_modifiers TypeModifiers;

    metac_identifier_ptr_t Identifier;

    decl_enum_member_t* Members;

    uint32_t MemberCount;

    decl_type_t* BaseType;

} decl_type_enum_t;

typedef struct decl_type_functiontype_t
{
    metac_declaration_kind_t DeclKind; uint32_t LocationIdx; uint32_t Hash; uint32_t Serial; uint32_t AllocLine;

    metac_type_kind_t TypeKind; metac_type_modifiers TypeModifiers;

    decl_type_t* ReturnType;


    decl_parameter_t* Parameters;

    uint32_t ParameterCount;
} decl_type_functiontype_t;

typedef struct decl_type_array_t
{
    metac_declaration_kind_t DeclKind; uint32_t LocationIdx; uint32_t Hash; uint32_t Serial; uint32_t AllocLine;

    metac_type_kind_t TypeKind; metac_type_modifiers TypeModifiers;

    decl_type_t* ElementType;

    metac_expression_t* Dim;
} decl_type_array_t;


typedef struct decl_type_struct_t
{
    metac_declaration_kind_t DeclKind; uint32_t LocationIdx; uint32_t Hash; uint32_t Serial; uint32_t AllocLine;

    metac_type_kind_t TypeKind; metac_type_modifiers TypeModifiers;

    metac_identifier_ptr_t Identifier;

    metac_identifier_ptr_t BaseIdentifier;

    struct decl_field_t* Fields;

    uint32_t FieldCount;
} decl_type_struct_t;



typedef struct decl_type_union_t
{
    metac_declaration_kind_t DeclKind; uint32_t LocationIdx; uint32_t Hash; uint32_t Serial; uint32_t AllocLine;

    metac_type_kind_t TypeKind; metac_type_modifiers TypeModifiers;

    metac_identifier_ptr_t Identifier;

    metac_identifier_ptr_t BaseIdentifier;

    struct decl_field_t* Fields;

    uint32_t FieldCount;
} decl_type_union_t;

typedef struct decl_type_typedef_t
{
    metac_declaration_kind_t DeclKind; uint32_t LocationIdx; uint32_t Hash; uint32_t Serial; uint32_t AllocLine;

    metac_type_kind_t TypeKind; metac_type_modifiers TypeModifiers;

    decl_type_t* Type;

    metac_identifier_ptr_t Identifier;
} decl_type_typedef_t;

typedef struct decl_type_tuple_t
{
    metac_declaration_kind_t DeclKind; uint32_t LocationIdx; uint32_t Hash; uint32_t Serial; uint32_t AllocLine;

    metac_type_kind_t TypeKind; metac_type_modifiers TypeModifiers;

    decl_type_t** Types;
    uint32_t TypeCount;
} decl_type_tuple_t;

typedef struct metac_declaration_t
{
    union {
        struct {
            metac_declaration_kind_t DeclKind; uint32_t LocationIdx; uint32_t Hash; uint32_t Serial; uint32_t AllocLine;
        };

        decl_variable_t decl_variable; decl_field_t decl_field; decl_parameter_t decl_parameter; decl_enum_member_t decl_enum_member; decl_type_t decl_type; decl_type_struct_t decl_type_struct; decl_type_union_t decl_type_union; decl_type_enum_t decl_type_enum; decl_type_array_t decl_type_array; decl_type_ptr_t decl_type_ptr; decl_type_functiontype_t decl_type_functiontype; decl_type_tuple_t decl_type_tuple; decl_type_typedef_t decl_type_typedef; decl_function_t decl_function; decl_label_t decl_label; decl_comment_t decl_comment;
    };

} metac_declaration_t;


typedef int (*walker_function_t) (metac_node_t node, void * ctx);





int MetaCDeclaration_Walk_Debug(metac_declaration_t* decl, const char* fn_name, walker_function_t walker_fn, void* ctx);



int MetaCDeclaration_Walk_Real(metac_declaration_t* decl, walker_function_t walker_fn, void* ctx);








typedef enum metac_storage_kind_t
{
    storage_unknown = 0,

    storage_stack,
    storage_register,

    storage_thread_local,
    storage_task_local,

    storage_static,
    storage_static_thread_local,
    storage_static_task_local,


    storage_invalid = 0xE,
} metac_storage_kind_t;

typedef struct metac_storage_location_t
{
    uint32_t v;
    union {
        uint32_t Offset : 28;
        metac_storage_kind_t Kind : 4;
    };
} metac_storage_location_t;




typedef struct sema_exp_argument_list_t
{
    struct metac_sema_expression_t* Arguments;
    uint32_t ArgumentCount;
} sema_exp_argument_list_t;

typedef struct metac_sema_expression_header_t
{
    metac_expression_kind_t Kind; uint32_t LocationIdx; uint32_t Hash; uint32_t Serial; metac_type_index_t TypeIndex;
} metac_sema_expression_header_t;


typedef struct metac_sema_expression_t
{
    metac_expression_kind_t Kind; uint32_t LocationIdx; uint32_t Hash; uint32_t Serial; metac_type_index_t TypeIndex;

    union
    {




        struct {
            struct metac_sema_expression_t* _E1;
            struct metac_sema_expression_t* E2;
        };

        struct {
            struct metac_sema_expression_t* _E1_;
            struct metac_sema_expression_t* _E2;
            struct metac_sema_expression_t* Econd;
        };


        struct {
            struct metac_sema_expression_t* E1;
        };

        struct {
            struct metac_sema_expression_t* CastExp;
            struct metac_type_index_t CastType;
        };

        struct {
            struct metac_sema_expression_t* AggExp;
            uint32_t AggOffset;
        };

        struct {
            struct metac_type_index_t TypeExp;
        };

        struct {
            struct metac_sema_expression_t* TupleExpressions;
            uint32_t TupleExpressionCount;
        };

        struct {
            sema_exp_argument_list_t* ArgumentList;
        };

        struct {
            struct sema_decl_variable_t* Variable;
        };

        struct {
            uint32_t IdentifierKey;

            metac_identifier_ptr_t IdentifierPtr;



        };

        struct {
            uint32_t StringKey;


            metac_identifier_ptr_t StringPtr;



        };

        struct {
            uint32_t CharKey;
            char Chars[8];
        };


        int64_t ValueI64;

        uint64_t ValueU64;
    };
} metac_sema_expression_t;

typedef struct sema_statement_header_t
{
    metac_statement_kind_t StmtKind; uint32_t LocationIdx; uint32_t Hash; uint32_t Serial;
} sema_statement_header_t;

typedef struct sema_stmt_block_t
{
    metac_statement_kind_t StmtKind; uint32_t LocationIdx; uint32_t Hash; uint32_t Serial;

    struct metac_sema_statement_t* Body;
    uint32_t StatementCount;
} sema_stmt_block_t;

typedef struct sema_stmt_break_t
{
    metac_statement_kind_t StmtKind; uint32_t LocationIdx; uint32_t Hash; uint32_t Serial;
} sema_stmt_break_t;

typedef struct sema_stmt_continue_t
{
    metac_statement_kind_t StmtKind; uint32_t LocationIdx; uint32_t Hash; uint32_t Serial;
} sema_stmt_continue_t;

typedef struct sema_stmt_yield_t
{
    metac_statement_kind_t StmtKind; uint32_t LocationIdx; uint32_t Hash; uint32_t Serial;

    metac_sema_expression_t* YieldExp;
} sema_stmt_yield_t;

typedef struct sema_stmt_scope_t
{
    metac_statement_kind_t StmtKind; uint32_t LocationIdx; uint32_t Hash; uint32_t Serial;

    scope_kind_t ScopeKind;
    struct metac_sema_statement_t* Stmt;
} sema_stmt_scope_t;

typedef struct sema_stmt_defer_t
{
    metac_statement_kind_t StmtKind; uint32_t LocationIdx; uint32_t Hash; uint32_t Serial;

    struct metac_sema_statement_t* DeferStmt;
} sema_stmt_defer_t;

typedef struct sema_stmt_for_t
{
    metac_statement_kind_t StmtKind; uint32_t LocationIdx; uint32_t Hash; uint32_t Serial;

    metac_sema_expression_t* ForInit;
    metac_sema_expression_t* ForCond;
    metac_sema_expression_t* ForPostLoop;

    struct metac_sema_statement_t* ForBody;

    metac_scope_t* Scope;
} sema_stmt_for_t;

typedef struct sema_stmt_while_t
{
    metac_statement_kind_t StmtKind; uint32_t LocationIdx; uint32_t Hash; uint32_t Serial;

    metac_sema_expression_t* WhileExp;
} sema_stmt_while_t;

typedef struct sema_stmt_case_t
{
    metac_statement_kind_t StmtKind; uint32_t LocationIdx; uint32_t Hash; uint32_t Serial;

    metac_sema_expression_t* CaseExp;
} sema_stmt_case_t;

typedef struct sema_stmt_goto_t
{
    metac_statement_kind_t StmtKind; uint32_t LocationIdx; uint32_t Hash; uint32_t Serial;

    metac_identifier_ptr_t GotoLabel;
} sema_stmt_goto_t;

typedef struct sema_stmt_exp_t
{
    metac_statement_kind_t StmtKind; uint32_t LocationIdx; uint32_t Hash; uint32_t Serial;

    metac_sema_expression_t* Expression;
} sema_stmt_exp_t;

typedef struct sema_stmt_sema_decl_t
{
    metac_statement_kind_t StmtKind; uint32_t LocationIdx; uint32_t Hash; uint32_t Serial;

    struct metac_declaration_t* Declaration;
} sema_stmt_sema_decl_t;

typedef struct sema_stmt_if_t
{
    metac_statement_kind_t StmtKind; uint32_t LocationIdx; uint32_t Hash; uint32_t Serial;

    struct metac_sema_expression_t* IfCond;
    struct metac_sema_statement_t* IfBody;
    struct metac_sema_statement_t* ElseBody;
} sema_stmt_if_t;

typedef struct sema_stmt_label_t
{
    metac_statement_kind_t StmtKind; uint32_t LocationIdx; uint32_t Hash; uint32_t Serial;

    metac_identifier_ptr_t Label;
} sema_stmt_label_t;

typedef struct sema_stmt_return_t
{
    metac_statement_kind_t StmtKind; uint32_t LocationIdx; uint32_t Hash; uint32_t Serial;

    metac_sema_expression_t* ReturnExp;
} sema_stmt_return_t;

typedef struct sema_stmt_switch_t
{
    metac_statement_kind_t StmtKind; uint32_t LocationIdx; uint32_t Hash; uint32_t Serial;

    metac_sema_expression_t* SwitchExp;
    struct metac_sema_statement_t* SwitchBody;
} sema_stmt_switch_t;

typedef struct sema_stmt_do_while_t
{
    metac_statement_kind_t StmtKind; uint32_t LocationIdx; uint32_t Hash; uint32_t Serial;

    metac_sema_expression_t* WhileExp;
    struct metac_sema_statement_t* WhileBody;
} sema_stmt_do_while_t;

typedef struct metac_sema_statement_t
{
    union
    {
        struct {
            metac_statement_kind_t StmtKind; uint32_t LocationIdx; uint32_t Hash; uint32_t Serial;
        };



        sema_stmt_if_t sema_stmt_if;

        sema_stmt_exp_t sema_stmt_exp;

        sema_stmt_block_t sema_stmt_block;

        sema_stmt_label_t sema_stmt_label;

        sema_stmt_goto_t sema_stmt_goto;

        sema_stmt_yield_t sema_stmt_yield;

        sema_stmt_return_t sema_stmt_return;

        sema_stmt_sema_decl_t sema_stmt_decl;
    };
} metac_sema_statement_t;






typedef struct sema_declaration_header_t
{
    metac_declaration_kind_t DeclKind; uint32_t LocationIdx; uint32_t Hash; uint32_t Serial; uint32_t AllocLine; metac_node_header_t* Parent;
} sema_declaration_header_t;





typedef struct sema_decl_type_t
{
    metac_declaration_kind_t DeclKind; uint32_t LocationIdx; uint32_t Hash; uint32_t Serial; uint32_t AllocLine; metac_node_header_t* Parent;


    metac_type_index_t typeIndex;
} sema_decl_type_t;

enum metac_variable_flags_t
{
    variable_none = (1 << 0),
    variable_is_parameter = (1 << 1),
    variable_is_local = (1 << 2),
    variable_address_taken = (1 << 3),
} metac_variable_flags_t;

typedef struct sema_decl_variable_t
{
    metac_declaration_kind_t DeclKind; uint32_t LocationIdx; uint32_t Hash; uint32_t Serial; uint32_t AllocLine; metac_node_header_t* Parent;

    uint32_t VarFlags;

    metac_type_index_t TypeIndex;

    metac_identifier_ptr_t VarIdentifier;

    metac_sema_expression_t* VarInitExpression;

    metac_storage_location_t Storage;
} sema_decl_variable_t;


typedef struct sema_decl_function_t
{
    metac_declaration_kind_t DeclKind; uint32_t LocationIdx; uint32_t Hash; uint32_t Serial; uint32_t AllocLine; metac_node_header_t* Parent;

    metac_type_index_t TypeIndex;

    struct metac_scope_t* Scope;
    metac_identifier_ptr_t Identifier;

    sema_decl_variable_t* Parameters;

    sema_stmt_block_t* FunctionBody;


    struct sema_decl_function_t* ParentFunc;

    uint32_t FrameOffset;
} sema_decl_function_t;

typedef struct sema_decl_type_ptr_t
{
    metac_declaration_kind_t DeclKind; uint32_t LocationIdx; uint32_t Hash; uint32_t Serial; uint32_t AllocLine; metac_node_header_t* Parent;

    metac_type_kind_t TypeKind; metac_type_modifiers TypeModifiers; uint32_t structuralHash;

    metac_type_index_t ElementType;
} sema_decl_type_ptr_t;

typedef struct sema_decl_enum_member_t
{
    metac_declaration_kind_t DeclKind; uint32_t LocationIdx; uint32_t Hash; uint32_t Serial; uint32_t AllocLine; metac_node_header_t* Parent;

    metac_identifier_ptr_t Name;

    metac_sema_expression_t* Value;
} sema_decl_enum_member_t;

typedef struct sema_decl_type_enum_t
{
    metac_declaration_kind_t DeclKind; uint32_t LocationIdx; uint32_t Hash; uint32_t Serial; uint32_t AllocLine; metac_node_header_t* Parent;

    metac_type_kind_t TypeKind; metac_type_modifiers TypeModifiers; uint32_t structuralHash;

    metac_identifier_ptr_t Name;

    sema_decl_enum_member_t* Members;

    uint32_t MemberCount;
} sema_decl_type_enum_t;

typedef struct sema_decl_type_functiontype_t
{
    metac_declaration_kind_t DeclKind; uint32_t LocationIdx; uint32_t Hash; uint32_t Serial; uint32_t AllocLine; metac_node_header_t* Parent;

    metac_type_kind_t TypeKind; metac_type_modifiers TypeModifiers; uint32_t structuralHash;

    metac_type_index_t ReturnType;

    metac_type_index_t* ParameterTypes;

    uint32_t ParameterTypeCount;
} sema_decl_type_functiontype_t;

typedef struct sema_decl_type_array_t
{
    metac_declaration_kind_t DeclKind; uint32_t LocationIdx; uint32_t Hash; uint32_t Serial; uint32_t AllocLine; metac_node_header_t* Parent;

    metac_type_kind_t TypeKind; metac_type_modifiers TypeModifiers; uint32_t structuralHash;

    metac_type_index_t* ElementType;

    metac_sema_expression_t* Dim;
} sema_decl_type_array_t;

typedef struct sema_decl_type_union_t
{
    metac_declaration_kind_t DeclKind; uint32_t LocationIdx; uint32_t Hash; uint32_t Serial; uint32_t AllocLine; metac_node_header_t* Parent;

    metac_type_kind_t TypeKind; metac_type_modifiers TypeModifiers; uint32_t structuralHash;

    metac_identifier_ptr_t Identifier;

    struct sema_decl_field_t* Fields;

    uint32_t FieldCount;
} sema_decl_type_union_t;

typedef struct sema_decl_type_typedef_t
{
    metac_declaration_kind_t DeclKind; uint32_t LocationIdx; uint32_t Hash; uint32_t Serial; uint32_t AllocLine; metac_node_header_t* Parent;

    metac_type_index_t Type;

    metac_identifier_ptr_t Identifier;
} sema_decl_type_typedef_t;

typedef struct metac_sema_declaration_t
{
    union {
        struct {
            metac_declaration_kind_t DeclKind; uint32_t LocationIdx; uint32_t Hash; uint32_t Serial; uint32_t AllocLine; metac_node_header_t* Parent;
        };
        sema_decl_variable_t sema_decl_variable;
        sema_decl_type_typedef_t sema_decl_typedef;
        sema_decl_type_t sema_decl_type;
        sema_decl_enum_member_t sema_decl_enum_member;
        sema_decl_type_enum_t sema_decl_type_enum;
        sema_decl_type_ptr_t sema_decl_type_ptr;
        sema_decl_function_t sema_decl_function;
        sema_decl_type_array_t sema_decl_type_array;
        metac_type_aggregate_t sema_type_aggergate;
    };

} metac_sema_declaration_t;



metac_expression_t* AllocNewExpression(metac_expression_kind_t kind);

metac_declaration_t* AllocNewDeclaration_(metac_declaration_kind_t kind, uint32_t nodeSize, void** result_ptr, uint32_t line);





metac_statement_t* AllocNewStatement_(metac_statement_kind_t kind, uint32_t nodeSize, void** result_ptr);









typedef struct node_ptr_t
{
    uint32_t Ptr;
    metac_node_kind_t Kind : 7;
} ndoe_ptr_t;


__attribute__ ((noinline)) void _newMemRealloc(void** memP, uint32_t* capacityP, const uint32_t elementSize)
{
    uint32_t capacity;
    if (!*memP)
    {
        capacity = (int)(8192 / 1.6);
    }
    else
    {
        capacity = *capacityP;
    }

    {
        capacity = ((((uint32_t) ((capacity - 1) * 1.6)) + 3) & ~3);
        *memP = realloc(*memP, ((capacity) * elementSize));
    }

    *capacityP = capacity;
}

static uint32_t _newExp_size = 0;
static uint32_t _newExp_capacity = 0; static metac_expression_t* _newExp_mem = (metac_expression_t*)0; static uint32_t _newDecl_size = 0; static uint32_t _newDecl_capacity = 0;
static metac_declaration_t* _newDecl_mem = (metac_declaration_t*)0;
static uint32_t _newStmt_size = 0;
static uint32_t _newStmt_capacity = 0;
static metac_statement_t* _newStmt_mem = (metac_statement_t*)0;

static uint32_t _nodeCounter = 1;

metac_expression_t* AllocNewExpression(metac_expression_kind_t kind)
{
    metac_expression_t* result = 0;

    if (_newExp_capacity <= _newExp_size) { printf("[%s]Preforming realloc from:%u\n", __FUNCTION__, _newExp_capacity); _newMemRealloc( (void**)& _newExp_mem, &_newExp_capacity, sizeof(* _newExp_mem) ); }

    {
        result = _newExp_mem + (_newExp_size++);
        result->Kind = kind;
        result->Serial = (_nodeCounter++);
    }

    return result;
}

metac_declaration_t* AllocNewDeclaration_(metac_declaration_kind_t kind, uint32_t nodeSize, void** result_ptr, uint32_t line)
{
    metac_declaration_t* result = 0;

    if (_newDecl_capacity <= _newDecl_size) { printf("[%s]Preforming realloc from:%u\n", __FUNCTION__, _newDecl_capacity); _newMemRealloc( (void**)& _newDecl_mem, &_newDecl_capacity, sizeof(* _newDecl_mem) ); }

    {
        (*result_ptr) = result = _newDecl_mem + (_newDecl_size++);
        result->DeclKind = kind;
        result->Serial = (_nodeCounter++);
        memset(&result->Serial + 1, 0, nodeSize - ((uint32_t)((char *)&((metac_declaration_t *)0)->Serial - (char *)0)));
    }
    result->AllocLine = line;

    return result;
}

metac_statement_t* AllocNewStatement_(metac_statement_kind_t kind, uint32_t nodeSize, void** result_ptr)
{
    metac_statement_t* result = 0;

    if (_newStmt_capacity <= _newStmt_size) { printf("[%s]Preforming realloc from:%u\n", __FUNCTION__, _newStmt_capacity); _newMemRealloc( (void**)& _newStmt_mem, &_newStmt_capacity, sizeof(* _newStmt_mem) ); }

    {
        (*result_ptr) = result = _newStmt_mem + (_newStmt_size++);
        result->StmtKind = kind;
        result->Serial = (_nodeCounter++);
        result->Next = (metac_statement_t*)(0x1);
    }

    return result;
}

const metac_identifier_ptr_t empty_identifier = {~0u};

static inline

             _Bool

                  IsFilled(metac_identifier_table_slot_t slot)
{
    return slot.HashKey != 0;
}

const char* IdentifierPtrToCharPtr(const metac_identifier_table_t* table,
                                   metac_identifier_ptr_t ptr)
{

    return table->StringMemory + (ptr.v - 4);
}

void IdentifierTable_Init(metac_identifier_table_t* table, uint32_t lengthShift, uint32_t slotCountLog2)
{
    table->SlotCount_Log2 = slotCountLog2;
    const uint32_t maxSlots = (1 << table->SlotCount_Log2);
    table->Slots = (metac_identifier_table_slot_t*) calloc(maxSlots, sizeof(metac_identifier_table_slot_t));
    table->StringMemory = (char*)malloc(maxSlots * 32);
    table->StringMemoryCapacity = maxSlots * 32;
    table->StringMemorySize = 0;
    table->SlotsUsed = 0;
    table->LengthShift = lengthShift;
    table->MaxDisplacement = 0;
}
void IdentifierTable_Free(metac_identifier_table_t* table)
{
    free(table->Slots);
    free(table->StringMemory);
    static const metac_identifier_table_t zeroTable = {0};
    (*table) = zeroTable;
}

metac_identifier_ptr_t GetOrAddIdentifier(metac_identifier_table_t* table,
                                          uint32_t identifierKey,
                                          const char* identifier)
{
    uint32_t length = (identifierKey >> table->LengthShift);

    metac_identifier_ptr_t result = {0};

    const uint32_t slotIndexMask = ((1 << table->SlotCount_Log2) - 1);
    const uint32_t initialSlotIndex = (identifierKey & slotIndexMask);

    uint32_t displacement = 0;
    for(
        uint32_t slotIndex = initialSlotIndex;
        (++slotIndex & slotIndexMask) != initialSlotIndex;
    )
    {
        metac_identifier_table_slot_t* slot = &table->Slots[(slotIndex - 1) & slotIndexMask];
        const char* stringEntry;

        if (slot->HashKey == identifierKey)
        {
            stringEntry = IdentifierPtrToCharPtr(table, slot->Ptr);
            if (memcmp(identifier, stringEntry, length) == 0)
            {

                result = slot->Ptr;



                break;
            }
            else
            {

            }
        }
        else if (slot->HashKey == 0)
        {
            uint32_t expected;
            uint32_t newValue;

            do {
                result.v = (table->StringMemorySize + 4);

                expected = table->StringMemorySize;
                newValue = (result.v - 4) + (((length + 1) + 3) & ~3);

                table->StringMemorySize = newValue;
            } while (

                    0

                         );
            ++table->SlotsUsed;






            char* tableMem = (table->StringMemory + (result.v - 4));
            memcpy(tableMem, identifier, length);
            tableMem[length] = '\0';
            static uint32_t misses = 0;
            ;
            slot->HashKey = identifierKey;
            slot->Ptr = result;




            break;
        }

            ;
            ;
            ;
            displacement++;
        continue;
    }
    ;

    return result;
}


int32_t MetaCIdentifierTable_HasKey(metac_identifier_table_t* table,
                                    uint32_t key)
{
    int32_t result = -1;

    const uint32_t slotIndexMask = ((1 << table->SlotCount_Log2) - 1);
    const uint32_t initialSlotIndex = (key & slotIndexMask);

    for(
        uint32_t slotIndex = initialSlotIndex;
        (++slotIndex & slotIndexMask) != initialSlotIndex;
    )
    {
        int32_t idx = (int32_t)((slotIndex - 1) & slotIndexMask);
        metac_identifier_table_slot_t slot =
            table->Slots[idx];

        if (slot.HashKey == 0)
            break;
        else if (slot.HashKey == key)
        {
            result = idx;
            break;
        }
    }

    return result;
}





void InsertSlot(metac_identifier_table_slot_t* slots, metac_identifier_table_slot_t slot, const uint32_t slotIndexMask)
{
    const uint32_t initialSlotIndex = (slot.HashKey & slotIndexMask);

    for(
        uint32_t slotIndex = initialSlotIndex;
        (++slotIndex & slotIndexMask) != initialSlotIndex;
    )
    {
        metac_identifier_table_slot_t* dstSlot = slots + ((slotIndex - 1) & slotIndexMask);
        if (dstSlot->HashKey == 0)
        {
            *dstSlot = slot;
            break;
        }
    }
}

metac_identifier_ptr_t IsIdentifierInTable(metac_identifier_table_t* table,
                                           uint32_t key,
                                           const char* idChars)
{
    metac_identifier_ptr_t result = {0};

    const uint32_t slotIndexMask = ((1 << table->SlotCount_Log2) - 1);
    const uint32_t initialSlotIndex = (key & slotIndexMask);

    for(
        uint32_t slotIndex = initialSlotIndex;
        (++slotIndex & slotIndexMask) != initialSlotIndex;
    )
    {
        metac_identifier_table_slot_t slot =
            table->Slots[(slotIndex - 1) & slotIndexMask];

        if (slot.HashKey == 0)
            return result;
        if (slot.HashKey == key)
        {

           _Bool

                matches =
                !memcmp(IdentifierPtrToCharPtr(table, slot.Ptr), idChars,
                        ( (key) >> 20 ));
            if (matches)
            {
                result = slot.Ptr;
                return result;
            }
        }
    }

    return result;
}


metac_identifier_table_slot_t* IdentifierTableLookup(
            metac_identifier_table_t* table,
            uint32_t key, metac_identifier_ptr_t value)
{
    const uint32_t slotIndexMask = ((1 << table->SlotCount_Log2) - 1);
    const uint32_t initialSlotIndex = (key & slotIndexMask);

    for(
        uint32_t slotIndex = initialSlotIndex;
        (++slotIndex & slotIndexMask) != initialSlotIndex;
    )
    {
        uint32_t lookupIdx = (slotIndex - 1) & slotIndexMask;
        metac_identifier_table_slot_t slot =
            table->Slots[lookupIdx];

        if (slot.HashKey == 0)
            return 0;
        if (slot.HashKey == key && slot.Ptr.v == value.v)
            return table->Slots + lookupIdx;
    }


    return 0;
}



_Bool

    IsInTable(metac_identifier_table_t* table,
               uint32_t key, metac_identifier_ptr_t value)
{
    return IdentifierTableLookup(table, key, value) != 0;
}

















uint32_t crc32c_nozero(uint32_t crc, const void* s, const uint32_t len_p);
uint32_t crc32c_byte(uint32_t crc, uint8_t byte);


static inline metac_token_enum_t MetaCLexFixedLengthToken(const char _chrs[3])
{
    switch (_chrs[0])
    {
    default:
        return tok_invalid;

    case '\0':
        return tok_eof;

    case '?':
        return tok_question;

    case '#':
        switch(_chrs[1])
        {
        default:
            return tok_hash;
        case '#':
            return tok_hashhash;
        }

    case '@':
        return tok_at;

    case '!':
        switch (_chrs[1])
        {
        default:
            return tok_bang;
        case '=':
            return tok_not_equal;
        }

    case '$':
         return tok_dollar;

    case '(':
        return tok_lParen;

    case ')':
        return tok_rParen;

    case '*':
        switch (_chrs[1])
        {
        default:
            return tok_star;
        case '=':
            return tok_mul_ass;
        case '/':
            return tok_comment_end_multi;
        }

    case ',':
        return tok_comma;

    case '+':
        switch (_chrs[1])
        {
        default :
            return tok_plus;
        case '+' :
            return tok_plusplus;
        case '=':
            return tok_add_ass;
        }

    case '-':
        switch (_chrs[1])
        {
        default :
            return tok_minus;
        case '-' :
            return tok_minusminus;
        case '=':
            return tok_sub_ass;
        case '>':
            return tok_arrow;
        }

    case '.':
        switch (_chrs[1])
        {
        default:
            return tok_dot;
        case '.':
            switch(_chrs[2])
            {
            default:
                return tok_dotdot;
            case '.':
                return tok_dotdotdot;
            }

        }

    case '/':
        switch (_chrs[1])
        {
        default :
            return tok_div;
        case '/':
            return tok_comment_begin_single;
        case '*':
            return tok_comment_begin_multi;
        case '=':
            return tok_div_ass;
        }

    case '%':
    {
        switch(_chrs[1])
        {
        default :
            return tok_rem;
        case '=':
            return tok_rem_ass;
        }
    }


    case ':':
        return tok_colon;

    case ';':
        return tok_semicolon;

    case '<':
        switch (_chrs[1])
        {
        default:
            return tok_lt;
        case '<':
            switch (_chrs[2])
            {
                default:
                    return tok_lsh;
                case '=':
                    return tok_lsh_ass;
            }
        case '=':
            switch (_chrs[2])
            {
            default:
                return tok_le;
            case '>':
                return tok_spaceship;
            }
        }

    case '^':
        switch (_chrs[1])
        {
        default:
            return tok_xor;
        case '=':
            return tok_xor_ass;
        }

    case '|':
        switch (_chrs[1])
        {
        default:
            return tok_or;
        case '|':
            return tok_oror;
        case '=':
            return tok_or_ass;
        }

    case '&':
        switch (_chrs[1])
        {
        default:
            return tok_and;
        case '&':
            return tok_andand;
        case '=':
            return tok_and_ass;
        }

    case '=':
        switch (_chrs[1])
        {
        default:
            return tok_assign;
        case '=':
            return tok_equals_equals;
        }

    case '>':
        switch (_chrs[1])
        {
        default:
            return tok_gt;
        case '>':
            switch (_chrs[2])
            {
                default:
                    return tok_rsh;
                case '=':
                    return tok_rsh_ass;
            }

        case '=':
            return tok_ge;
        }

    case '[':
        switch (_chrs[1])
        {
        default:
            return tok_lBracket;
        case ']':
            return tok_full_slice;
        }

    case ']':
        return tok_rBracket;

    case '{':
        return tok_lBrace;

    case '}':
        return tok_rBrace;

    case '~':
        switch (_chrs[1])
        {
        default:
            return tok_cat;


        }

    case '\n':
        return tok_newline;

    }

    return tok_invalid;
}

const char* MetaCTokenEnum_toChars(metac_token_enum_t type)
{
    const char* result = 0;




    switch (type)
    {
        case tok_invalid : {result = "tok_invalid";} break; case tok_identifier : {result = "tok_identifier";} break; case tok_uint : {result = "tok_uint";} break; case tok_string : {result = "tok_string";} break; case tok_char : {result = "tok_char";} break; case tok_char_uni : {result = "tok_char_uni";} break; case tok_comment_single : {result = "tok_comment_single";} break; case tok_comment_multi : {result = "tok_comment_multi";} break; case tok_macro_parameter : {result = "tok_macro_parameter";} break; case tok_bang : {result = "tok_bang";} break; case tok_question : {result = "tok_question";} break; case tok_hash : {result = "tok_hash";} break; case tok_at : {result = "tok_at";} break; case tok_lParen : {result = "tok_lParen";} break; case tok_rParen : {result = "tok_rParen";} break; case tok_lBrace : {result = "tok_lBrace";} break; case tok_rBrace : {result = "tok_rBrace";} break; case tok_lBracket : {result = "tok_lBracket";} break; case tok_rBracket : {result = "tok_rBracket";} break; case tok_semicolon : {result = "tok_semicolon";} break; case tok_colon : {result = "tok_colon";} break; case tok_dollar : {result = "tok_dollar";} break; case tok_cat : {result = "tok_cat";} break; case tok_comma : {result = "tok_comma";} break; case tok_dot : {result = "tok_dot";} break; case tok_plus : {result = "tok_plus";} break; case tok_minus : {result = "tok_minus";} break; case tok_star : {result = "tok_star";} break; case tok_div : {result = "tok_div";} break; case tok_rem : {result = "tok_rem";} break; case tok_xor : {result = "tok_xor";} break; case tok_or : {result = "tok_or";} break; case tok_and : {result = "tok_and";} break; case tok_lsh : {result = "tok_lsh";} break; case tok_rsh : {result = "tok_rsh";} break; case tok_oror : {result = "tok_oror";} break; case tok_andand : {result = "tok_andand";} break; case tok_arrow : {result = "tok_arrow";} break; case tok_dotdot : {result = "tok_dotdot";} break; case tok_assign : {result = "tok_assign";} break; case tok_add_ass : {result = "tok_add_ass";} break; case tok_sub_ass : {result = "tok_sub_ass";} break; case tok_mul_ass : {result = "tok_mul_ass";} break; case tok_div_ass : {result = "tok_div_ass";} break; case tok_rem_ass : {result = "tok_rem_ass";} break; case tok_xor_ass : {result = "tok_xor_ass";} break; case tok_or_ass : {result = "tok_or_ass";} break; case tok_and_ass : {result = "tok_and_ass";} break; case tok_lsh_ass : {result = "tok_lsh_ass";} break; case tok_rsh_ass : {result = "tok_rsh_ass";} break; case tok_equals_equals : {result = "tok_equals_equals";} break; case tok_not_equal : {result = "tok_not_equal";} break; case tok_lt : {result = "tok_lt";} break; case tok_le : {result = "tok_le";} break; case tok_gt : {result = "tok_gt";} break; case tok_ge : {result = "tok_ge";} break; case tok_spaceship : {result = "tok_spaceship";} break; case tok_dotdotdot : {result = "tok_dotdotdot";} break; case tok_kw_struct : {result = "tok_kw_struct";} break; case tok_kw_union : {result = "tok_kw_union";} break; case tok_kw_enum : {result = "tok_kw_enum";} break; case tok_kw_typedef : {result = "tok_kw_typedef";} break; case tok_kw_auto : {result = "tok_kw_auto";} break; case tok_kw_void : {result = "tok_kw_void";} break; case tok_kw_bool : {result = "tok_kw_bool";} break; case tok_kw_char : {result = "tok_kw_char";} break; case tok_kw_short : {result = "tok_kw_short";} break; case tok_kw_int : {result = "tok_kw_int";} break; case tok_kw_long : {result = "tok_kw_long";} break; case tok_kw_uint32_t : {result = "tok_kw_uint32_t";} break; case tok_kw_float : {result = "tok_kw_float";} break; case tok_kw_double : {result = "tok_kw_double";} break; case tok_kw_signed : {result = "tok_kw_signed";} break; case tok_kw_unsigned : {result = "tok_kw_unsigned";} break; case tok_kw_const : {result = "tok_kw_const";} break; case tok_kw_volatile : {result = "tok_kw_volatile";} break; case tok_kw___shared : {result = "tok_kw___shared";} break; case tok_kw_extern : {result = "tok_kw_extern";} break; case tok_kw_for : {result = "tok_kw_for";} break; case tok_kw_sizeof : {result = "tok_kw_sizeof";} break; case tok_kw_return : {result = "tok_kw_return";} break; case tok_kw_switch : {result = "tok_kw_switch";} break; case tok_kw_while : {result = "tok_kw_while";} break; case tok_kw_do : {result = "tok_kw_do";} break; case tok_kw_typeof : {result = "tok_kw_typeof";} break; case tok_kw_inject : {result = "tok_kw_inject";} break; case tok_kw_eject : {result = "tok_kw_eject";} break; case tok_kw_assert : {result = "tok_kw_assert";} break; case tok_kw_case : {result = "tok_kw_case";} break; case tok_kw_default : {result = "tok_kw_default";} break; case tok_kw_goto : {result = "tok_kw_goto";} break; case tok_kw_static : {result = "tok_kw_static";} break; case tok_kw_inline : {result = "tok_kw_inline";} break; case tok_kw_if : {result = "tok_kw_if";} break; case tok_kw_else : {result = "tok_kw_else";} break; case tok_kw_break : {result = "tok_kw_break";} break; case tok_kw_continue : {result = "tok_kw_continue";} break; case tok_kw_until : {result = "tok_kw_until";} break; case tok_kw_yield : {result = "tok_kw_yield";} break; case tok_kw___attribute__ : {result = "tok_kw___attribute__";} break; case tok_comment_begin_multi : {result = "tok_comment_begin_multi";} break; case tok_comment_end_multi : {result = "tok_comment_end_multi";} break; case tok_comment_begin_single : {result = "tok_comment_begin_single";} break; case tok_plusplus : {result = "tok_plusplus";} break; case tok_minusminus : {result = "tok_minusminus";} break; case tok_full_slice : {result = "tok_full_slice";} break; case tok_hashhash : {result = "tok_hashhash";} break; case tok_newline : {result = "tok_newline";} break; case tok_eof : {result = "tok_eof";} break; case tok_error : {result = "tok_error";} break;
    }

    return result;


}

static uint32_t MetaCStaticTokenLength(metac_token_enum_t t)
{
    switch (t) {
        default : return 2;
        case tok_eof : return 0;

        case tok_bang : return 1;
        case tok_question : return 1;
        case tok_hash : return 1;
        case tok_at : return 1;
        case tok_lParen : return 1;
        case tok_rParen : return 1;
        case tok_lBrace : return 1;
        case tok_rBrace : return 1;
        case tok_lBracket : return 1;
        case tok_rBracket : return 1;
        case tok_semicolon : return 1;
        case tok_colon : return 1;
        case tok_dollar : return 1;
        case tok_comma : return 1;
        case tok_dot : return 1;
        case tok_plus : return 1;
        case tok_minus : return 1;
        case tok_star : return 1;
        case tok_rem : return 1;
        case tok_div : return 1;
        case tok_xor : return 1;
        case tok_or : return 1;
        case tok_and : return 1;
        case tok_cat : return 1;
        case tok_assign : return 1;
        case tok_lt : return 1;
        case tok_gt : return 1;
        case tok_newline : return 1;

        case tok_lsh_ass : return 3;
        case tok_rsh_ass : return 3;

        case tok_spaceship : return 3;
        case tok_dotdotdot : return 3;

        case tok_kw_struct : return 6;
        case tok_kw_union : return 5;
        case tok_kw_enum : return 4;
        case tok_kw_const : return 5;
        case tok_kw_return : return 6;
        case tok_kw_switch : return 6;
        case tok_kw_while : return 5;
        case tok_kw_typeof : return 6;
        case tok_kw_inject : return 6;
        case tok_kw_eject : return 5;
        case tok_kw_assert : return 6;
        case tok_kw_typedef : return 7;
        case tok_kw_case : return 4;
        case tok_kw_default : return 7;
        case tok_kw_static : return 6;
        case tok_kw_inline : return 6;
        case tok_kw_else : return 4;
        case tok_kw_break : return 5;
        case tok_kw_continue : return 8;
        case tok_kw_until : return 5;

        case tok_kw_auto : return 4;
        case tok_kw_bool : return 4;
        case tok_kw_double : return 6;
        case tok_kw_float : return 5;
        case tok_kw_long : return 4;
        case tok_kw_int : return 3;
        case tok_kw_short : return 5;
        case tok_kw_char : return 4;
        case tok_kw_void : return 4;
        case tok_kw_signed : return 6;
        case tok_kw_unsigned : return 8;
        case tok_kw_volatile : return 8;
        case tok_kw___shared : return 8;
        case tok_kw_extern : return 6;
        case tok_kw_for : return 3;
        case tok_kw_sizeof : return 6;
        case tok_kw_uint32_t : return 6;
        case tok_kw_goto : return 4;

        case tok_kw_yield : return 5;
        case tok_kw___attribute__ : return 13;
    }
}

static inline uint32_t fastLog10(uint32_t val)
{
  uint32_t result;

  result = 0;
  if ( val >= 10 )
  {
    result = 1;
    if ( val >= 100 )
    {
      result = 2;
      if ( val >= 1000 )
      {
        result = 3;
        if ( val >= 10000 )
        {
          result = 4;
          if ( val >= 100000 )
          {
            result = 5;
            if ( val >= 1000000 )
            {
              result = 6;
              if ( val >= 10000000 )
              {
                result = 7;
                if ( val >= 100000000 )
                  result = 9 - (val < 1000000000);
              }
            }
          }
        }
      }
    }
  }
  return result;
}

uint32_t MetaCTokenLength(metac_token_t token)
{
    if (token.TokenType >= tok_bang)
    {
        return MetaCStaticTokenLength(token.TokenType);
    }
    else
    {
        if (token.TokenType == tok_uint)
        {
            return token.ValueLength;
        }
        else if (token.TokenType == tok_identifier)
        {
            return ( (token.Key) >> 20 );
        }
        else if (token.TokenType == tok_string)
        {
            return ( (token.Key) >> 12 );
        }
        else if (token.TokenType == tok_comment_single)
        {
            return token.CommentLength + 2;
        }
        else if (token.TokenType == tok_comment_multi)
        {
            return token.CommentLength + 4;
        }
        else if (token.TokenType == tok_char)
        {
            return token.charLength + 2;
        }
        else if (token.TokenType == tok_char_uni)
        {
            return token.charLength + 4;
        }
    }



    return 0;
}



void MetaCLexer_Init(metac_lexer_t* self)
{
    self->TokenCount = 0;
    self->TokenCapacity = ((unsigned int)(sizeof((self->inlineTokens)) / sizeof((self->inlineTokens)[0])));
    self->Tokens = self->inlineTokens;

    self->LocationStorage.LocationCapacity = ((unsigned int)(sizeof((self->inlineLocations)) / sizeof((self->inlineLocations)[0])));
    self->LocationStorage.LocationSize = 0;
    self->LocationStorage.Locations = self->inlineLocations;

    IdentifierTable_Init(&((*self).IdentifierTable), 20, 13);
    IdentifierTable_Init(&((*self).StringTable), 12, 13);
}

void MetaCLexer_Free(metac_lexer_t* self)
{
    IdentifierTable_Free(&self->IdentifierTable);
    IdentifierTable_Free(&self->StringTable);

    if (self->LocationStorage.Locations != self->inlineLocations)
        free(self->LocationStorage.Locations);
    if (self->Tokens != self->inlineTokens)
        free(self->Tokens);

    static const metac_lexer_t zeroLexer = {0};
    *self = zeroLexer;
    self = 0;
}

metac_lexer_state_t MetaCLexerStateFromString(uint32_t sourceId,
                                              const char* str)
{
    uint32_t length = strlen(str);
    return MetaCLexerStateFromBuffer(sourceId, str, length);
}

metac_lexer_state_t MetaCLexerStateFromBuffer(uint32_t sourceId,
                                              const char* buffer,
                                              uint32_t bufferLength)
{



    metac_lexer_state_t result;

    result.Text = buffer;
    result.Column = 1;
    result.Line = 1;
    result.Position = 0;
    result.Size = bufferLength;
    result.OuterBlock = (block_idx_t) 0;
    result.SourceId = sourceId;

    return result;
}

static inline

             _Bool

                  IsIdentifierChar(char c)
{
    const char upper_c = (c & ~32);
    return (((upper_c >= 'A') & (upper_c <= 'Z')) | (c == '_'));
}

static inline

             _Bool

                  IsNumericChar(char c)
{
    return ((((unsigned) c) - '0') <= 9);
}

static inline

             _Bool

                  IsHexLiteralChar(char c)
{
  c |= 32;
  return ((unsigned)(c - '0') <= 9) | ((unsigned)(c - 'a') <= 6);
}

static inline metac_token_enum_t classify(char c)
{
    metac_token_enum_t result = tok_invalid;

    if (IsIdentifierChar(c))
    {
        result = tok_identifier;
    }
    else if (IsNumericChar(c))
    {
        result = tok_uint;
    }
    else if (c == '\"')
    {
        result = tok_string;
    }

    return result;
}

static inline char EscapedChar(char c)
{
    switch (c)
    {
        case 'n' : return '\n';
        case 'v' : return '\v';
        case 't' : return '\t';
        case 'r' : return '\r';
        case '"' : return '"';
        case 'a' : return '\a';
        case 'b' : return '\b';
        case 'f' : return '\f';
        case '\'' : return '\'';
        case '?' : return '?';
        case '\\': return '\\';
    }
    return 'E';
}
static void MetaCLexerMatchKeywordIdentifier(metac_token_t* tok,
                                             const char* identifier)
{







{
                                           ;

    switch (tok->IdentifierKey)
    {
    case 0x6e9213 :
if (
    ((uint32_t)((*((identifier) + 0 + 0)) << 0 | (*((identifier) + 0 + 1)) << 8 | (*((identifier) + 0 + 2)) << 16 | (*((identifier) + 0 + 3)) << 24)) == ((uint32_t)('s' | 't' << 8 | 'r' << 16 | 'u' << 24))
 && ((uint16_t)((*((identifier) + 4 + 0)) | (*((identifier) + 4 + 1)) << 8)) == ((uint16_t)('c' | 't' << 8))
)
            tok->TokenType = tok_kw_struct;
    break;
    case 0x5a08cc :
if (
    ((uint32_t)((*((identifier) + 0 + 0)) << 0 | (*((identifier) + 0 + 1)) << 8 | (*((identifier) + 0 + 2)) << 16 | (*((identifier) + 0 + 3)) << 24)) == ((uint32_t)('u' | 'n' << 8 | 'i' << 16 | 'o' << 24))
 && ((*(identifier + 4)) == 'n')
)
            tok->TokenType = tok_kw_union;
    break;
    case 0x42259d :
if (
    ((uint32_t)((*((identifier) + 0 + 0)) << 0 | (*((identifier) + 0 + 1)) << 8 | (*((identifier) + 0 + 2)) << 16 | (*((identifier) + 0 + 3)) << 24)) == ((uint32_t)('e' | 'n' << 8 | 'u' << 16 | 'm' << 24))
    )
            tok->TokenType = tok_kw_enum;
    break;
    case 0x7f3981 :
if (
    ((uint32_t)((*((identifier) + 0 + 0)) << 0 | (*((identifier) + 0 + 1)) << 8 | (*((identifier) + 0 + 2)) << 16 | (*((identifier) + 0 + 3)) << 24)) == ((uint32_t)('t' | 'y' << 8 | 'p' << 16 | 'e' << 24))
 && ((uint16_t)((*((identifier) + 4 + 0)) | (*((identifier) + 4 + 1)) << 8)) == ((uint16_t)('d' | 'e' << 8))
 && ((*(identifier + 6)) == 'f')
)
            tok->TokenType = tok_kw_typedef;
    break;
    case 0x447fd2 :
if (
    ((uint32_t)((*((identifier) + 0 + 0)) << 0 | (*((identifier) + 0 + 1)) << 8 | (*((identifier) + 0 + 2)) << 16 | (*((identifier) + 0 + 3)) << 24)) == ((uint32_t)('a' | 'u' << 8 | 't' << 16 | 'o' << 24))
    )
            tok->TokenType = tok_kw_auto;
    break;
    case 0x4c07ce :
if (
    ((uint32_t)((*((identifier) + 0 + 0)) << 0 | (*((identifier) + 0 + 1)) << 8 | (*((identifier) + 0 + 2)) << 16 | (*((identifier) + 0 + 3)) << 24)) == ((uint32_t)('v' | 'o' << 8 | 'i' << 16 | 'd' << 24))
    )
            tok->TokenType = tok_kw_void;
    break;
    case 0x4c8efd :
if (
    ((uint32_t)((*((identifier) + 0 + 0)) << 0 | (*((identifier) + 0 + 1)) << 8 | (*((identifier) + 0 + 2)) << 16 | (*((identifier) + 0 + 3)) << 24)) == ((uint32_t)('b' | 'o' << 8 | 'o' << 16 | 'l' << 24))
    )
            tok->TokenType = tok_kw_bool;
    break;
    case 0x44aaee :
if (
    ((uint32_t)((*((identifier) + 0 + 0)) << 0 | (*((identifier) + 0 + 1)) << 8 | (*((identifier) + 0 + 2)) << 16 | (*((identifier) + 0 + 3)) << 24)) == ((uint32_t)('c' | 'h' << 8 | 'a' << 16 | 'r' << 24))
    )
            tok->TokenType = tok_kw_char;
    break;
    case 0x5ca984 :
if (
    ((uint32_t)((*((identifier) + 0 + 0)) << 0 | (*((identifier) + 0 + 1)) << 8 | (*((identifier) + 0 + 2)) << 16 | (*((identifier) + 0 + 3)) << 24)) == ((uint32_t)('s' | 'h' << 8 | 'o' << 16 | 'r' << 24))
 && ((*(identifier + 4)) == 't')
)
            tok->TokenType = tok_kw_short;
    break;
    case 0x3fff6d :
if (
    ((uint16_t)((*((identifier) + 0 + 0)) | (*((identifier) + 0 + 1)) << 8)) == ((uint16_t)('i' | 'n' << 8))
 && ((*(identifier + 2)) == 't')
)
            tok->TokenType = tok_kw_int;
    break;
    case 0x46cc25 :
if (
    ((uint32_t)((*((identifier) + 0 + 0)) << 0 | (*((identifier) + 0 + 1)) << 8 | (*((identifier) + 0 + 2)) << 16 | (*((identifier) + 0 + 3)) << 24)) == ((uint32_t)('l' | 'o' << 8 | 'n' << 16 | 'g' << 24))
    )
            tok->TokenType = tok_kw_long;
    break;
    case 0x6987c6 :
if (
    ((uint32_t)((*((identifier) + 0 + 0)) << 0 | (*((identifier) + 0 + 1)) << 8 | (*((identifier) + 0 + 2)) << 16 | (*((identifier) + 0 + 3)) << 24)) == ((uint32_t)('s' | 'i' << 8 | 'z' << 16 | 'e' << 24))
 && ((uint16_t)((*((identifier) + 4 + 0)) | (*((identifier) + 4 + 1)) << 8)) == ((uint16_t)('_' | 't' << 8))
)
            tok->TokenType = tok_kw_uint32_t;
    break;
    case 0x5a71a0 :
if (
    ((uint32_t)((*((identifier) + 0 + 0)) << 0 | (*((identifier) + 0 + 1)) << 8 | (*((identifier) + 0 + 2)) << 16 | (*((identifier) + 0 + 3)) << 24)) == ((uint32_t)('f' | 'l' << 8 | 'o' << 16 | 'a' << 24))
 && ((*(identifier + 4)) == 't')
)
            tok->TokenType = tok_kw_float;
    break;
    case 0x636b8e :
if (
    ((uint32_t)((*((identifier) + 0 + 0)) << 0 | (*((identifier) + 0 + 1)) << 8 | (*((identifier) + 0 + 2)) << 16 | (*((identifier) + 0 + 3)) << 24)) == ((uint32_t)('d' | 'o' << 8 | 'u' << 16 | 'b' << 24))
 && ((uint16_t)((*((identifier) + 4 + 0)) | (*((identifier) + 4 + 1)) << 8)) == ((uint16_t)('l' | 'e' << 8))
)
            tok->TokenType = tok_kw_double;
    break;
    case 0x60165a :
if (
    ((uint32_t)((*((identifier) + 0 + 0)) << 0 | (*((identifier) + 0 + 1)) << 8 | (*((identifier) + 0 + 2)) << 16 | (*((identifier) + 0 + 3)) << 24)) == ((uint32_t)('s' | 'i' << 8 | 'g' << 16 | 'n' << 24))
 && ((uint16_t)((*((identifier) + 4 + 0)) | (*((identifier) + 4 + 1)) << 8)) == ((uint16_t)('e' | 'd' << 8))
)
            tok->TokenType = tok_kw_signed;
    break;
    case 0x8265da :
if (
    ((uint32_t)((*((identifier) + 0 + 0)) << 0 | (*((identifier) + 0 + 1)) << 8 | (*((identifier) + 0 + 2)) << 16 | (*((identifier) + 0 + 3)) << 24)) == ((uint32_t)('u' | 'n' << 8 | 's' << 16 | 'i' << 24))
 && ((uint32_t)((*((identifier) + 4 + 0)) << 0 | (*((identifier) + 4 + 1)) << 8 | (*((identifier) + 4 + 2)) << 16 | (*((identifier) + 4 + 3)) << 24)) == ((uint32_t)('g' | 'n' << 8 | 'e' << 16 | 'd' << 24))
    )
            tok->TokenType = tok_kw_unsigned;
    break;
    case 0x5d7a94 :
if (
    ((uint32_t)((*((identifier) + 0 + 0)) << 0 | (*((identifier) + 0 + 1)) << 8 | (*((identifier) + 0 + 2)) << 16 | (*((identifier) + 0 + 3)) << 24)) == ((uint32_t)('c' | 'o' << 8 | 'n' << 16 | 's' << 24))
 && ((*(identifier + 4)) == 't')
)
            tok->TokenType = tok_kw_const;
    break;
    case 0x8a95c7 :
if (
    ((uint32_t)((*((identifier) + 0 + 0)) << 0 | (*((identifier) + 0 + 1)) << 8 | (*((identifier) + 0 + 2)) << 16 | (*((identifier) + 0 + 3)) << 24)) == ((uint32_t)('v' | 'o' << 8 | 'l' << 16 | 'a' << 24))
 && ((uint32_t)((*((identifier) + 4 + 0)) << 0 | (*((identifier) + 4 + 1)) << 8 | (*((identifier) + 4 + 2)) << 16 | (*((identifier) + 4 + 3)) << 24)) == ((uint32_t)('t' | 'i' << 8 | 'l' << 16 | 'e' << 24))
    )
            tok->TokenType = tok_kw_volatile;
    break;
    case 0x8b318f :
if (
    ((uint32_t)((*((identifier) + 0 + 0)) << 0 | (*((identifier) + 0 + 1)) << 8 | (*((identifier) + 0 + 2)) << 16 | (*((identifier) + 0 + 3)) << 24)) == ((uint32_t)('_' | '_' << 8 | 's' << 16 | 'h' << 24))
 && ((uint32_t)((*((identifier) + 4 + 0)) << 0 | (*((identifier) + 4 + 1)) << 8 | (*((identifier) + 4 + 2)) << 16 | (*((identifier) + 4 + 3)) << 24)) == ((uint32_t)('a' | 'r' << 8 | 'e' << 16 | 'd' << 24))
    )
            tok->TokenType = tok_kw___shared;
    break;
    case 0x6569e1 :
if (
    ((uint32_t)((*((identifier) + 0 + 0)) << 0 | (*((identifier) + 0 + 1)) << 8 | (*((identifier) + 0 + 2)) << 16 | (*((identifier) + 0 + 3)) << 24)) == ((uint32_t)('e' | 'x' << 8 | 't' << 16 | 'e' << 24))
 && ((uint16_t)((*((identifier) + 4 + 0)) | (*((identifier) + 4 + 1)) << 8)) == ((uint16_t)('r' | 'n' << 8))
)
            tok->TokenType = tok_kw_extern;
    break;
    case 0x3dda5e :
if (
    ((uint16_t)((*((identifier) + 0 + 0)) | (*((identifier) + 0 + 1)) << 8)) == ((uint16_t)('f' | 'o' << 8))
 && ((*(identifier + 2)) == 'r')
)
            tok->TokenType = tok_kw_for;
    break;
    case 0x6222dd :
if (
    ((uint32_t)((*((identifier) + 0 + 0)) << 0 | (*((identifier) + 0 + 1)) << 8 | (*((identifier) + 0 + 2)) << 16 | (*((identifier) + 0 + 3)) << 24)) == ((uint32_t)('s' | 'i' << 8 | 'z' << 16 | 'e' << 24))
 && ((uint16_t)((*((identifier) + 4 + 0)) | (*((identifier) + 4 + 1)) << 8)) == ((uint16_t)('o' | 'f' << 8))
)
            tok->TokenType = tok_kw_sizeof;
    break;
    case 0x6363a6 :
if (
    ((uint32_t)((*((identifier) + 0 + 0)) << 0 | (*((identifier) + 0 + 1)) << 8 | (*((identifier) + 0 + 2)) << 16 | (*((identifier) + 0 + 3)) << 24)) == ((uint32_t)('r' | 'e' << 8 | 't' << 16 | 'u' << 24))
 && ((uint16_t)((*((identifier) + 4 + 0)) | (*((identifier) + 4 + 1)) << 8)) == ((uint16_t)('r' | 'n' << 8))
)
            tok->TokenType = tok_kw_return;
    break;
    case 0x63e3c4 :
if (
    ((uint32_t)((*((identifier) + 0 + 0)) << 0 | (*((identifier) + 0 + 1)) << 8 | (*((identifier) + 0 + 2)) << 16 | (*((identifier) + 0 + 3)) << 24)) == ((uint32_t)('s' | 'w' << 8 | 'i' << 16 | 't' << 24))
 && ((uint16_t)((*((identifier) + 4 + 0)) | (*((identifier) + 4 + 1)) << 8)) == ((uint16_t)('c' | 'h' << 8))
)
            tok->TokenType = tok_kw_switch;
    break;
    case 0x53c644 :
if (
    ((uint32_t)((*((identifier) + 0 + 0)) << 0 | (*((identifier) + 0 + 1)) << 8 | (*((identifier) + 0 + 2)) << 16 | (*((identifier) + 0 + 3)) << 24)) == ((uint32_t)('w' | 'h' << 8 | 'i' << 16 | 'l' << 24))
 && ((*(identifier + 4)) == 'e')
)
            tok->TokenType = tok_kw_while;
    break;
    case 0x2d63b1 :
if (
    ((uint16_t)((*((identifier) + 0 + 0)) | (*((identifier) + 0 + 1)) << 8)) == ((uint16_t)('d' | 'o' << 8))
)
            tok->TokenType = tok_kw_do;
    break;
    case 0x6affc4 :
if (
    ((uint32_t)((*((identifier) + 0 + 0)) << 0 | (*((identifier) + 0 + 1)) << 8 | (*((identifier) + 0 + 2)) << 16 | (*((identifier) + 0 + 3)) << 24)) == ((uint32_t)('t' | 'y' << 8 | 'p' << 16 | 'e' << 24))
 && ((uint16_t)((*((identifier) + 4 + 0)) | (*((identifier) + 4 + 1)) << 8)) == ((uint16_t)('o' | 'f' << 8))
)
            tok->TokenType = tok_kw_typeof;
    break;
    case 0x6d00ba :
if (
    ((uint32_t)((*((identifier) + 0 + 0)) << 0 | (*((identifier) + 0 + 1)) << 8 | (*((identifier) + 0 + 2)) << 16 | (*((identifier) + 0 + 3)) << 24)) == ((uint32_t)('i' | 'n' << 8 | 'j' << 16 | 'e' << 24))
 && ((uint16_t)((*((identifier) + 4 + 0)) | (*((identifier) + 4 + 1)) << 8)) == ((uint16_t)('c' | 't' << 8))
)
            tok->TokenType = tok_kw_inject;
    break;
    case 0x578f00 :
if (
    ((uint32_t)((*((identifier) + 0 + 0)) << 0 | (*((identifier) + 0 + 1)) << 8 | (*((identifier) + 0 + 2)) << 16 | (*((identifier) + 0 + 3)) << 24)) == ((uint32_t)('e' | 'j' << 8 | 'e' << 16 | 'c' << 24))
 && ((*(identifier + 4)) == 't')
)
            tok->TokenType = tok_kw_eject;
    break;
    case 0x68b26e :
if (
    ((uint32_t)((*((identifier) + 0 + 0)) << 0 | (*((identifier) + 0 + 1)) << 8 | (*((identifier) + 0 + 2)) << 16 | (*((identifier) + 0 + 3)) << 24)) == ((uint32_t)('a' | 's' << 8 | 's' << 16 | 'e' << 24))
 && ((uint16_t)((*((identifier) + 4 + 0)) | (*((identifier) + 4 + 1)) << 8)) == ((uint16_t)('r' | 't' << 8))
)
            tok->TokenType = tok_kw_assert;
    break;
    case 0x4064be :
if (
    ((uint32_t)((*((identifier) + 0 + 0)) << 0 | (*((identifier) + 0 + 1)) << 8 | (*((identifier) + 0 + 2)) << 16 | (*((identifier) + 0 + 3)) << 24)) == ((uint32_t)('c' | 'a' << 8 | 's' << 16 | 'e' << 24))
    )
            tok->TokenType = tok_kw_case;
    break;
    case 0x7cee1a :
if (
    ((uint32_t)((*((identifier) + 0 + 0)) << 0 | (*((identifier) + 0 + 1)) << 8 | (*((identifier) + 0 + 2)) << 16 | (*((identifier) + 0 + 3)) << 24)) == ((uint32_t)('d' | 'e' << 8 | 'f' << 16 | 'a' << 24))
 && ((uint16_t)((*((identifier) + 4 + 0)) | (*((identifier) + 4 + 1)) << 8)) == ((uint16_t)('u' | 'l' << 8))
 && ((*(identifier + 6)) == 't')
)
            tok->TokenType = tok_kw_default;
    break;
    case 0x4d7ce2 :
if (
    ((uint32_t)((*((identifier) + 0 + 0)) << 0 | (*((identifier) + 0 + 1)) << 8 | (*((identifier) + 0 + 2)) << 16 | (*((identifier) + 0 + 3)) << 24)) == ((uint32_t)('g' | 'o' << 8 | 't' << 16 | 'o' << 24))
    )
            tok->TokenType = tok_kw_goto;
    break;
    case 0x6c68bb :
if (
    ((uint32_t)((*((identifier) + 0 + 0)) << 0 | (*((identifier) + 0 + 1)) << 8 | (*((identifier) + 0 + 2)) << 16 | (*((identifier) + 0 + 3)) << 24)) == ((uint32_t)('s' | 't' << 8 | 'a' << 16 | 't' << 24))
 && ((uint16_t)((*((identifier) + 4 + 0)) | (*((identifier) + 4 + 1)) << 8)) == ((uint16_t)('i' | 'c' << 8))
)
            tok->TokenType = tok_kw_static;
    break;
    case 0x6ef668 :
if (
    ((uint32_t)((*((identifier) + 0 + 0)) << 0 | (*((identifier) + 0 + 1)) << 8 | (*((identifier) + 0 + 2)) << 16 | (*((identifier) + 0 + 3)) << 24)) == ((uint32_t)('i' | 'n' << 8 | 'l' << 16 | 'i' << 24))
 && ((uint16_t)((*((identifier) + 4 + 0)) | (*((identifier) + 4 + 1)) << 8)) == ((uint16_t)('n' | 'e' << 8))
)
            tok->TokenType = tok_kw_inline;
    break;
    case 0x23826e :
if (
    ((uint16_t)((*((identifier) + 0 + 0)) | (*((identifier) + 0 + 1)) << 8)) == ((uint16_t)('i' | 'f' << 8))
)
            tok->TokenType = tok_kw_if;
    break;
    case 0x4b7e6d :
if (
    ((uint32_t)((*((identifier) + 0 + 0)) << 0 | (*((identifier) + 0 + 1)) << 8 | (*((identifier) + 0 + 2)) << 16 | (*((identifier) + 0 + 3)) << 24)) == ((uint32_t)('e' | 'l' << 8 | 's' << 16 | 'e' << 24))
    )
            tok->TokenType = tok_kw_else;
    break;
    case 0x59983a :
if (
    ((uint32_t)((*((identifier) + 0 + 0)) << 0 | (*((identifier) + 0 + 1)) << 8 | (*((identifier) + 0 + 2)) << 16 | (*((identifier) + 0 + 3)) << 24)) == ((uint32_t)('b' | 'r' << 8 | 'e' << 16 | 'a' << 24))
 && ((*(identifier + 4)) == 'k')
)
            tok->TokenType = tok_kw_break;
    break;
    case 0x83f482 :
if (
    ((uint32_t)((*((identifier) + 0 + 0)) << 0 | (*((identifier) + 0 + 1)) << 8 | (*((identifier) + 0 + 2)) << 16 | (*((identifier) + 0 + 3)) << 24)) == ((uint32_t)('c' | 'o' << 8 | 'n' << 16 | 't' << 24))
 && ((uint32_t)((*((identifier) + 4 + 0)) << 0 | (*((identifier) + 4 + 1)) << 8 | (*((identifier) + 4 + 2)) << 16 | (*((identifier) + 4 + 3)) << 24)) == ((uint32_t)('i' | 'n' << 8 | 'u' << 16 | 'e' << 24))
    )
            tok->TokenType = tok_kw_continue;
    break;
    case 0x525c22 :
if (
    ((uint32_t)((*((identifier) + 0 + 0)) << 0 | (*((identifier) + 0 + 1)) << 8 | (*((identifier) + 0 + 2)) << 16 | (*((identifier) + 0 + 3)) << 24)) == ((uint32_t)('u' | 'n' << 8 | 't' << 16 | 'i' << 24))
 && ((*(identifier + 4)) == 'l')
)
            tok->TokenType = tok_kw_until;
    break;
    case 0x521f59 :
if (
    ((uint32_t)((*((identifier) + 0 + 0)) << 0 | (*((identifier) + 0 + 1)) << 8 | (*((identifier) + 0 + 2)) << 16 | (*((identifier) + 0 + 3)) << 24)) == ((uint32_t)('y' | 'i' << 8 | 'e' << 16 | 'l' << 24))
 && ((*(identifier + 4)) == 'd')
)
            tok->TokenType = tok_kw_yield;
    break;
    case 0xd3374b :
if (
    ((uint32_t)((*((identifier) + 0 + 0)) << 0 | (*((identifier) + 0 + 1)) << 8 | (*((identifier) + 0 + 2)) << 16 | (*((identifier) + 0 + 3)) << 24)) == ((uint32_t)('_' | '_' << 8 | 'a' << 16 | 't' << 24))
 && ((uint32_t)((*((identifier) + 4 + 0)) << 0 | (*((identifier) + 4 + 1)) << 8 | (*((identifier) + 4 + 2)) << 16 | (*((identifier) + 4 + 3)) << 24)) == ((uint32_t)('t' | 'r' << 8 | 'i' << 16 | 'b' << 24))
 && ((uint32_t)((*((identifier) + 8 + 0)) << 0 | (*((identifier) + 8 + 1)) << 8 | (*((identifier) + 8 + 2)) << 16 | (*((identifier) + 8 + 3)) << 24)) == ((uint32_t)('u' | 't' << 8 | 'e' << 16 | '_' << 24))
 && ((*(identifier + 12)) == '_')
)
            tok->TokenType = tok_kw___attribute__;
    break;
    }
}

}



static _Bool ParseOctal(const char** textP, uint32_t* eatenCharsP, uint64_t* valueP)
{


   _Bool

        result =

                 1

                     ;

    uint64_t value = 0;
    const char* text = *textP;
    uint32_t eatenChars = *eatenCharsP;

    char c = *text++;

    if(c < '0' || c > '7')
    {
        result =

                0

                     ;
    }

    while(c && (c >= '0' && c <= '7'))
    {
        eatenChars++;
        value *= 8;
        value += c - '0';
        c = *text++;
    }

    *eatenCharsP = eatenChars;
    *textP = text - 1;
    *valueP = value;

    return result;
}





_Bool

    static ParseHex(const char** textP, uint32_t* eatenCharsP, uint64_t* valueP)
{


   _Bool

        result =

                 1

                     ;

    uint64_t value = 0;
    const char* text = *textP;
    uint32_t eatenChars = *eatenCharsP;

    char c = *text++;

    if(!IsHexLiteralChar(c))
    {
        result =

                0

                     ;
    }

    while(IsHexLiteralChar(c))
    {
        eatenChars++;
        value *= 16;
        c |= 32;
        if (c <= '9')
        {
            value += (c - '0');
        }
        else
        {
            value += ((c - 'a') + 10);
        }
        c = *text++;
    }

    *eatenCharsP = eatenChars;
    *textP = text - 1;
    *valueP = value;
    return result;
}

void ParseErrorBreak(void)
{
    int k = 2;
}




_Bool

    IsValidEscapeChar(char c)
{

    return (c == 'n' || c == '"' || c == 't')
        || (c == '\'' || c == 'r' || c == 'u')
        || (c == '\\' || c == '\n'|| c == '`')
        || (c == 'x' || c == 'v' || c == 'a')
        || (c == 'f' || c == '?' || c == 'b')
        || (c >= '0' && c <= '7')
        || (c == 'U');
}

typedef uint32_t metac_location_ptr;

void MetaCLocationStorage_Init(metac_location_t_array* self)
{
    self->LocationCapacity = 128;
    self->LocationSize = 0;
    self->Locations = (metac_location_t*)
        calloc(sizeof(metac_location_t), self->LocationCapacity);
}

metac_location_ptr MetaCLocationStorage_Store(metac_location_t_array* self,
                                              metac_location_t loc)
{

    if (self->LocationSize >= self->LocationCapacity)
    {
        _newMemRealloc((void**)&self->Locations, &self->LocationCapacity, sizeof(metac_location_t));
    }




    uint32_t result = self->LocationSize++;
    self->Locations[result] = loc;

    return result + 4;
}

void MetaCLocationStorage_EndLoc(
        metac_location_t_array* self,
        metac_location_ptr locationId,
        uint32_t line, uint16_t column)
{



    uint32_t idx = locationId - 4;
    metac_location_t *location =
        self->Locations + idx;
    location->LineSpan = line - location->StartLine;
    location->EndColumn = (uint16_t)column;
}

void MetaCLocation_Expand(metac_location_t* self, metac_location_t endLoc)
{
    self->LineSpan = (endLoc.StartLine + endLoc.LineSpan) - self->StartLine;
    self->EndColumn = endLoc.EndColumn;
}

metac_location_ptr MetaCLocationStorage_StartLoc(
        metac_location_t_array* self,
        uint32_t line, uint16_t column)
{


    if (self->LocationSize >= self->LocationCapacity)
    {
        _newMemRealloc((void**)&self->Locations, &self->LocationCapacity, sizeof(metac_location_t));
    }



    uint32_t result = self->LocationSize++;

    self->Locations[result].StartLine = line;
    self->Locations[result].StartColumn = column;

    return result + 4;



}

metac_location_t MetaCLocationStorage_FromPair(metac_location_t_array *srcStorage,
                                               metac_location_ptr startLocIdx,
                                               metac_location_ptr endLocIdx)
{
    metac_location_t result;

    result = srcStorage->Locations[startLocIdx - 4];
    const metac_location_t endLoc = srcStorage->Locations[endLocIdx - 4];

    result.LineSpan = (endLoc.StartLine + endLoc.LineSpan) - result.StartLine;
    result.EndColumn = endLoc.EndColumn;

    return result;
}

metac_token_t* MetaCLexerLexNextToken(metac_lexer_t* self,
                                      metac_lexer_state_t* state,
                                      const char* text, uint32_t len)

{
    static metac_token_t stop_token = {tok_eof};
    static metac_token_t err_token = {tok_error};
    metac_token_t token = {tok_invalid};
    if (text[len] != '\0')
    {
        return &err_token;
    }
    metac_token_t* result = 0;

    if (self->TokenCount >= self->TokenCapacity)
    {
        uint32_t newCapa = 32;

        if (self->Tokens == self->inlineTokens)
        {
            metac_token_t* newTokens = (metac_token_t*)
                malloc(sizeof(metac_token_t) * newCapa);
            metac_location_t* newLocations = (metac_location_t*)
                malloc(sizeof(metac_location_t) * newCapa);

            memcpy(newTokens, self->Tokens, sizeof(metac_token_t) * ((unsigned int)(sizeof((self->inlineTokens)) / sizeof((self->inlineTokens)[0]))));
            memcpy(newLocations, self->LocationStorage.Locations,
                sizeof(metac_location_t) * ((unsigned int)(sizeof((self->inlineLocations)) / sizeof((self->inlineLocations)[0]))));
            self->Tokens = newTokens;
            self->LocationStorage.Locations = newLocations;

            self->TokenCapacity = newCapa;
            self->LocationStorage.LocationCapacity = newCapa;
        }
        else
        {
            newCapa = ((((uint32_t)((self->TokenCapacity) * 1.3)) + 3) & ~3);
            self->Tokens = (metac_token_t*)
                realloc(self->Tokens, sizeof(metac_token_t) * newCapa);
            self->TokenCapacity = newCapa;
            self->LocationStorage.Locations = (metac_location_t*)
                realloc(self->LocationStorage.Locations,
                        sizeof(metac_location_t) * newCapa);
            self->LocationStorage.LocationCapacity = newCapa;
        }

    }
    uint32_t eatenChars = 0;
    char c = *text++;
LcontinueLexnig:
    {
        uint32_t column = state->Column;

        while (c && c <= 32)
        {
            if (c == '\n')
            {
                state->Line++;
                column = 0;
            }
            if (c == '\r')
            {
                column = 0;
            }
            column++;
            eatenChars++;
            c = *text++;
        }
        state->Column = column;
    }
    if (c)
    {
        text -= 1;
    }

    state->Position += eatenChars;
    eatenChars = 0;
    token.Position = state->Position;

    token.LocationId =
        MetaCLocationStorage_StartLoc(&self->LocationStorage, state->Line, state->Column);
    metac_location_t loc = self->LocationStorage.Locations[token.LocationId - 4];

    if (c && (token.TokenType = MetaCLexFixedLengthToken(text)) == tok_invalid)
    {

        if (c)
        {
            if (IsIdentifierChar(c))
            {
                uint32_t identifierLength = 0;
                uint32_t identifierHash = ~0;
                const char* identifierBegin = text;

                while ((c = *text++) && (IsIdentifierChar(c) || IsNumericChar(c)))
                {



                    identifierLength++;
                    eatenChars++;
                }
                token.TokenType = tok_identifier;


                state->Column += eatenChars;

                identifierHash = crc32c_nozero(~0, identifierBegin, identifierLength);

                token.IdentifierKey =
                    ( ((uint32_t)(identifierHash & 0xFFFFF)) | (((uint32_t)(identifierLength)) << 20) );


                MetaCLexerMatchKeywordIdentifier(&token, identifierBegin);

                if(token.TokenType == tok_identifier)
                {
                    token.IdentifierPtr =
                        GetOrAddIdentifier(&self->IdentifierTable, token.IdentifierKey, identifierBegin);
                }
            }
            else if (IsNumericChar(c))
            {
                token.TokenType = tok_uint;


               _Bool

                    isHex;
                uint64_t value;
                uint32_t initialPos = eatenChars;

                value = 0;
                isHex =

                       0

                            ;

                if (c == '0')
                {
                    ++text;
                    c = *text;
                    eatenChars++;
                    if (c == 'x')
                    {
                        text++;
                        eatenChars++;
                        if (!ParseHex(&text, &eatenChars, &value))
                        {
                            fprintf(

                           stderr

                           , "ParseError[%s:%u]: {%u:%u}" "invalid hex literal %.*s" "\n", "../metac_lexer.c", 877, (loc.StartLine), (loc.StartColumn), 4, text - 1); ParseErrorBreak();;
                            result = &err_token;
                            goto Lreturn;
                        }
                        c = *text++;

                        goto LParseNumberDone;
                    }
                    else if (!IsNumericChar(c))
                    {
                        value = 0;
                        goto LParseNumberDone;
                    }
                    else
                    {
                        if (!ParseOctal(&text, &eatenChars, &value))
                        {
                            fprintf(

                           stderr

                           , "ParseError[%s:%u]: {%u:%u}" "invalid octal literal %.*s" "\n", "../metac_lexer.c", 894, (loc.StartLine), (loc.StartColumn), 4, text - 1); ParseErrorBreak();;
                            result = &err_token;
                            goto Lreturn;
                        }
                        c = *text++;
                        goto LParseNumberDone;
                    }
                }

                while (c && IsNumericChar((c = *text++)))
                {
                    eatenChars++;
                    value *= 10;
                    value += c - '0';
                }
            LParseNumberDone:
                c |= 32;
                while (c == 'u' || c == 'l')
                {
                    eatenChars++;
                    c = (*text++ | 32);
                }
                token.ValueU64 = value;
                token.ValueLength = eatenChars - initialPos;
                state->Column += eatenChars;
            }
            else if (c == '\'')
            {
                uint32_t charLength = 0;
                text++;
                token.TokenType = tok_char;
                uint32_t charHash = ~0u;
                c = *text++;
                eatenChars++;
                if (c == '\'')
                {
                    fprintf(

                   stderr

                   , "ParseError[%s:%u]: {%u:%u}" "Empty character Literal" "\n", "../metac_lexer.c", 930, (loc.StartLine), (loc.StartColumn)); ParseErrorBreak();;
                    result = &err_token;
                    goto Lreturn;
                }
                while(c && c != '\'')
                {
                    token.chars[charLength++] = c;
                    if (charLength > 8)
                    {
                        fprintf(

                       stderr

                       , "ParseError[%s:%u]: {%u:%u}" "Char literal too long." "\n", "../metac_lexer.c", 939, (loc.StartLine), (loc.StartColumn)); ParseErrorBreak();;
                        token.TokenType = tok_error;
                        goto Lreturn;
                    }

                    if (c == '\\')
                    {
                        c = *text++;
                        token.chars[charLength++] = c;
                        eatenChars++;
                        if (!IsValidEscapeChar(c))
                        {
                            fprintf(

                           stderr

                           , "ParseError[%s:%u]: {%u:%u}" "Invalid escape seqeunce '%.*s'" "\n", "../metac_lexer.c", 951, (loc.StartLine), (loc.StartColumn), 4, (text - 2)); ParseErrorBreak();;
                        }
                        if (c == 'U')
                        {


                            token.TokenType = tok_char_uni;
                            charLength = 0;
                        }
                    }
                    c = *text++;
                    eatenChars++;
                }
                if (*text++ != '\'')
                {
                                                      ;
                }
                state->Column += eatenChars++;
                token.charLength = charLength;
            }
            else if (c == '\"' || c == '`')
            {
                ++text;
                char matchTo = c;
                token.TokenType = tok_string;
                const char* stringBegin = text;
                uint32_t stringHash = ~0u;
                c = *text++;

                uint32_t column = state->Column;
                eatenChars++;
                uint32_t eatenCharsAtStringStart = eatenChars;

                while(c && c != matchTo)
                {



                    eatenChars++;
                    column++;
                    if (c == '\\')
                    {
                        eatenChars++;
                        column++;



                        c = *text++;
                        if (!IsValidEscapeChar(c))
                        {
                            state->Column = column;
                            fprintf(

                           stderr

                           , "ParseError[%s:%u]: {%u:%u}" "Invalid escape seqeunce '%.*s'" "\n", "../metac_lexer.c", 1002, (loc.StartLine), (loc.StartColumn), 4, (text - 2)); ParseErrorBreak();;
                        }
                        if (c == '\n')
                        {
                            state->Line++;
                            column = 0;
                        }

                     }
                     c = *text++;
                }

                if (c != matchTo)
                {
                    fprintf(

                   stderr

                   , "ParseError[%s:%u]: {%u:%u}" "Unterminated string literal '%.*s' \n" "\n", "../metac_lexer.c", 1016, (loc.StartLine), (loc.StartColumn), 10, text - eatenChars - 1); ParseErrorBreak();;
                    result = &err_token;
                    goto Lreturn;
                }

                uint32_t stringLength = (eatenChars - eatenCharsAtStringStart);

                eatenChars++;

                stringHash = crc32c_nozero(~0, stringBegin, stringLength);

                state->Column = column;
                token.Key = ( (uint32_t)((stringHash) & 0xFFF) | (((uint32_t)(stringLength)) << 12) );
                token.StringPtr = GetOrAddIdentifier(&self->StringTable, token.Key, stringBegin);
            }

            else if (c == '\\')
            {
                text++;
                c = *text++;
                if (c == '\n')
                {
                    c = *text++;
                    state->Line++;
                    goto LcontinueLexnig;
                }
                else
                {
                    fprintf(

                   stderr

                   , "ParseError[%s:%u]: {%u:%u}" "escaping '%c' in wild code '%.*s' \n" "\n", "../metac_lexer.c", 1045, (loc.StartLine), (loc.StartColumn), c, 8, text - 4); ParseErrorBreak();;


                }
            }
        }
    }
    else if (token.TokenType == tok_comment_begin_single)
    {
        token.TokenType = tok_comment_single;
        eatenChars += 2;
        text += 2;
        char* newlinePtr = (char*)memchr(text, '\n', len - eatenChars);
        uint32_t commentLength = (newlinePtr - text);
        if (!newlinePtr)
        {
            commentLength = len - eatenChars;
            c = '\0';
        }
        eatenChars += commentLength + !!newlinePtr;
        token.CommentLength = commentLength;
        token.CommentBegin = text;
        state->Column += commentLength + 1;
        state->Line += 1;
    }
    else if (token.TokenType == tok_comment_begin_multi)
    {
        token.TokenType = tok_comment_multi;

        uint32_t offset = 0;
        char* endPtr;
        char* lastNewline = 0;
        char* newlinePtr = (char*)memchr(text + 2, '\n', len - 2);
        char* slashPtr = (char*)memchr(text + 2, '/', len - 2);
        for(;;)
        {
            while (newlinePtr && newlinePtr < slashPtr)
            {
                lastNewline = newlinePtr;
                offset = (newlinePtr - text);
                state->Line++;
                newlinePtr = (char*)memchr(lastNewline + 1, '\n', len - (lastNewline - text));
            }
            if (!slashPtr)                                   ;
                offset = (slashPtr - text);

            if ((*(slashPtr - 1)) == '*')
            {
                endPtr = slashPtr + 1;
                break;
            }
            newlinePtr = (char*)memchr(slashPtr, '\n', len - 2 - offset);
            slashPtr = (char*)memchr(slashPtr + 1, '/', len - 2 - offset);
        }
        eatenChars = endPtr - text;

        if (lastNewline && lastNewline < endPtr)
        {
            state->Column = endPtr - lastNewline;
        }
        else
        {
            state->Column += eatenChars;
        }
        token.CommentLength = eatenChars - 4;
        token.CommentBegin = text + 2;


    }

    else
    {
        const uint32_t tokLen = MetaCStaticTokenLength(token.TokenType);
        eatenChars += tokLen;
        state->Column += tokLen;
    }

    MetaCLocationStorage_EndLoc(&self->LocationStorage,
        token.LocationId, state->Line, state->Column);
Lreturn:
    if (token.TokenType)
    {
        result = self->Tokens + self->TokenCount++;
        *result = token;
    }
    else
    {
        result = &stop_token;
    }

    state->Position += eatenChars;
    return result;
}




























typedef union metac_filehandle_t
{
    uint32_t v;
    void* p;
} metac_filehandle_t;

typedef struct metac_filesystem_ctx metac_filesystem_ctx;

typedef struct metac_buffer_t
{
    const char* Data;
    uint32_t Length;

    uint32_t Offset;
} metac_buffer_t;

typedef uint64_t metac_timestamp;

typedef struct metac_file_info_t
{
    const char* Path;

    uint64_t FileSize;

    metac_timestamp CreationTime;
    metac_timestamp ModificationTime;
    metac_timestamp AccessTime;
} metac_file_info_t;

typedef metac_filesystem_ctx* (*metac_filesystem_init_t)(const char* config);
typedef metac_filehandle_t (*metac_filesystem_open_t)(metac_filesystem_ctx* fs, const char* path, const char* filename);
typedef metac_buffer_t (*metac_filesystem_read_entire_file_and_zero_terminate_t)(metac_filesystem_ctx* fs, metac_filehandle_t filehandle);
typedef void (*metac_filesystem_close_t)(metac_filesystem_ctx* fs, metac_filehandle_t handle);
typedef uint32_t (*metac_filesystem_read_until_buffer_full_and_zero_terminate_t)(metac_filesystem_ctx* fs, metac_filehandle_t handle, metac_buffer_t* buffer);
typedef uint64_t (*metac_filesyem_get_file_size)(metac_filesystem_ctx* fs, metac_filehandle_t filehandle);

typedef struct metac_filesytem_functions_t
{
    metac_filesystem_ctx* (*Init)(const char* config);

    metac_filehandle_t (*Open)(metac_filesystem_ctx* fs, const char* path, const char* filename);
    metac_buffer_t (*ReadEntireFileAndZeroTerminate)(metac_filesystem_ctx* fs, metac_filehandle_t filehandle);
    void (*Close)(metac_filesystem_ctx* fs, metac_filehandle_t handle);
    metac_file_info_t (*GetFileInfo)(metac_filesystem_ctx* fs, metac_filehandle_t handle);

    uint32_t (*ReadUntilBufferFullAndZeroTerminate)(metac_filesystem_ctx* fs, metac_filehandle_t handle, metac_buffer_t* buffer);

    uint64_t (*GetFileSize)(metac_filesystem_ctx* fs, metac_filehandle_t filehandle);

    uint64_t (*GetFilesystemSize)(metac_filesystem_ctx* fs);
} metac_filesystem_functions_t;


typedef struct metac_filesystem_t
{
    metac_filesystem_ctx* ctx;
    const metac_filesystem_functions_t* functions;
} metac_filesystem_t;

typedef union metac_file_ptr_t
{
    uint32_t v;
    struct
    {
        uint16_t PathIdx;
        uint16_t FilenameIdx;
    };
} metac_file_ptr_t;

typedef struct metac_file_storage_t
{
    metac_identifier_table_t Filenames;
    metac_identifier_table_t Paths;

    metac_filesystem_t* FS;
} metac_file_storage_t;

void FileStorage_Init(metac_file_storage_t* self, metac_filesystem_t* fs);

metac_file_ptr_t MetaCFileStorage_LoadFile(metac_file_storage_t* self, const char* path);

metac_buffer_t MetaCFileStorage_GetEntireFileBuffer(metac_file_storage_t* Storage,
                                                    metac_file_ptr_t Ptr);







void _StackArrayRealloc(void** arrayPtr, uint32_t* arrayCapacityPtr,
                        const uint32_t elementSize);

typedef struct metac_identifier_ptr_t_array
{
    metac_identifier_ptr_t* Ptr;
    uint32_t Count;
    uint32_t Capacity;
} metac_identifier_ptr_t_array;

typedef struct metac_token_t_array
{
    metac_token_t* Ptr;
    uint32_t Count;
    uint32_t Capacity;
} metac_token_t_array;

typedef struct metac_token_t_array_array
{
    metac_token_t_array* Ptr;
    uint32_t Count;
    uint32_t Capacity;
} metac_token_t_array_array;


typedef enum metac_preprocessor_directive_t
{
    pp_invalid, pp_error, pp_warning, pp_undef, pp_if, pp_elif, pp_else, pp_endif, pp_ifdef, pp_ifndef, pp_line, pp_pragma, pp_inline, pp_define, pp_include, pp_eval,
} metac_preprocessor_directive_t;



typedef struct metac_preprocessor_define_ptr_t
{
    uint32_t v;
} metac_preprocessor_define_ptr_t;

typedef struct metac_define_table_slot_t
{
    uint32_t HashKey;
    metac_preprocessor_define_ptr_t DefinePtr;





} metac_define_table_slot_t;


typedef struct metac_preprocessor_define_t
{
    metac_location_t loc;

    metac_identifier_ptr_t DefineName;
    uint32_t ParameterCount : 30;


   _Bool

        IsVariadic : 1;


   _Bool

        HasPaste : 1;

    uint32_t TokensOffset;
    uint32_t TokenCount;
} metac_preprocessor_define_t;

typedef struct metac_define_table_t
{
    metac_define_table_slot_t* Slots;
    struct metac_preprocessor_t* Preproc;

    metac_token_t* TokenMemory;
    uint32_t TokenMemorySize;
    uint32_t TokenMemoryCapacity;

    uint32_t SlotCount_Log2;
    uint32_t SlotsUsed;
    uint32_t LengthShift;
    uint32_t MaxDisplacement;

    metac_preprocessor_define_t* DefineMemory;
    uint32_t DefineMemorySize;
    uint32_t DefineMemoryCapacity;
} metac_define_table_t;

typedef struct metac_preprocessor_t
{
    metac_file_storage_t* FileStorage;
    metac_identifier_table_t StringTable;
    metac_identifier_table_t IdentifierTable;

    metac_define_table_t DefineTable;
    metac_identifier_table_t DefineIdentifierTable;
    struct metac_preprocessor_t* Parent;

    metac_token_t_array DefineTokenStack[16];
    uint32_t DefineTokenIndexStack[16];
    uint32_t DefineTokenStackCount;

    metac_token_t* TokenMemory;
    uint32_t TokenMemorySize;
    uint32_t TokenMemoryCapacity;


    metac_file_ptr_t File;
} metac_preprocessor_t;

void MetaCPreProcessor_Init(metac_preprocessor_t *self, metac_lexer_t* lexer,
                            metac_file_storage_t* fs, const char* filepath);

metac_preprocessor_define_ptr_t MetaCPreProcessor_GetDefine(metac_preprocessor_t* self,
                                                            uint32_t identifierKey,
                                                            const char* identifier);

uint32_t MetaCPreProcessor_PushDefine(metac_preprocessor_t* self,
                                      metac_preprocessor_define_t* define,
                                      metac_token_t_array_array parameters);



























typedef struct metac_semantic_state_t metac_semantic_state_t;

typedef struct metac_printer_t
{
    char* StringMemory;
    uint32_t StringMemorySize;
    uint32_t StringMemoryCapacity;

    metac_identifier_table_t* IdentifierTable;
    metac_identifier_table_t* StringTable;

    uint16_t IndentLevel;
    uint16_t CurrentColumn;

    uint16_t StartColumn;
} metac_printer_t;


void MetaCPrinter_Init(metac_printer_t* self,
                       metac_identifier_table_t* identifierTable,
                       metac_identifier_table_t* stringTable);

void MetaCPrinter_InitSz(metac_printer_t* self,
                         metac_identifier_table_t* identifierTable,
                         metac_identifier_table_t* stringTable,
                         uint32_t initializeSize);

const char* MetaCPrinter_PrintExpression(metac_printer_t* self, metac_expression_t* exp);
const char* MetaCPrinter_PrintDeclaration(metac_printer_t* self, metac_declaration_t* decl);
const char* MetaCPrinter_PrintStatement(metac_printer_t* self, metac_statement_t* stmt);
const char* MetaCPrinter_PrintNode(metac_printer_t* self, metac_node_t node, uint32_t level);

const char* StatementKind_toChars(metac_statement_kind_t kind);

void MetacPrinter_PrintI64(metac_printer_t* self, const int64_t value);
void MetacPrinter_PrintStringLiteral(metac_printer_t* self, const char* str);

void MetaCPrinter_Reset(metac_printer_t* self);



const char* MetaCPrinter_PrintSemaNode(metac_printer_t* self,
                                       metac_semantic_state_t* sema,
                                       metac_node_t node);








typedef struct metac_dot_printer_label_t
{
    char* LabelMemory;
    uint32_t LabelMemorySize;
    uint32_t LabelMemoryCapacity;
} metac_dot_printer_label_t;


typedef struct metac_dot_printer_t
{
    char* StringMemory;
    uint32_t StringMemorySize;
    uint32_t StringMemoryCapacity;

    char* SnapshotMemory;
    uint32_t SnapshotMemoryCapacity;

    metac_identifier_table_t* IdTable;

    metac_dot_printer_label_t* CurrentLabel;
} metac_dot_printer_t;



void Dot_PrintString(metac_dot_printer_t* self, const char* string);
void MetaCDotPrinter_Init(metac_dot_printer_t* self,
                          metac_identifier_table_t* idTable);
const char* MetaCDotPrinter_Snapshot(metac_dot_printer_t* self);

void MetaCDotPrinter_Reset(metac_dot_printer_t* self);

metac_dot_printer_label_t* MetaCDotPrinter_BeginLabel(metac_dot_printer_t* self);
void MetaCDotPrinter_EndLabel(metac_dot_printer_t* self, metac_dot_printer_label_t* label);




typedef enum parse_expression_flags_t
{
    expr_flags_none,
    expr_flags_call = (1 << 0),
    expr_flags_unary = (1 << 1),
    expr_flags_enum = (1 << 2),
    expr_flags_type = (1 << 3),
    expr_flags_addr = (1 << 4),
    expr_flags_sizeof = (1 << 5),
    expr_flags_pp = (1 << 6),
} parse_expression_flags_t;


typedef struct metac_define_t
{
    uint32_t IdentifierKey;
    metac_identifier_ptr_t IdentifierPtr;


    uint32_t TokenPosition;
    uint32_t SourceId;

    uint32_t NumberOfParameters;
} metac_define_t;

typedef struct metac_parser_t
{
    metac_lexer_t* Lexer;
    metac_lexer_state_t* LexerState;

    uint32_t CurrentTokenIndex;
    metac_identifier_table_t IdentifierTable;
    metac_identifier_table_t StringTable;


    metac_preprocessor_t* Preprocessor;

    metac_location_t LastLocation;

    stmt_block_t* CurrentBlockStatement;

    uint16_t* PackStack;

    int32_t PackStackTop;

    metac_dot_printer_t* DotPrinter;

    stmt_block_t** BlockStatementStack;
    uint32_t BlockStatementStackCount;
    uint32_t BlockStatementStackCapacity;

    uint32_t OpenParens;
    uint32_t PackStackCapacity;

    metac_token_t CurrentComment;
    decl_label_t* CurrentLabel;

    metac_location_t_array LocationStorage;




    metac_printer_t DebugPrinter;

    metac_identifier_ptr_t SpecialNamePtr_Compiler;
    metac_identifier_ptr_t SpecialNamePtr_Context;
    metac_identifier_ptr_t SpecialNamePtr_Target;
    metac_identifier_ptr_t SpecialNamePtr_Type;
    metac_identifier_ptr_t SpecialNamePtr_Defined;
} metac_parser_t;

extern metac_parser_t g_lineParser;


_Bool

    IsBinaryAssignExp(metac_expression_kind_t exp_kind);


_Bool

    IsBinaryExp(metac_expression_kind_t exp_kind);

void MetaCParser_Init(metac_parser_t* self);
void MetaCParser_InitFromLexer(metac_parser_t* self, metac_lexer_t* lexer);






metac_token_t* MetaCParser_PeekToken_(metac_parser_t* self, int32_t p, uint32_t line);
uint32_t MetaCParser_HowMuchLookahead(metac_parser_t* self);
metac_expression_t* MetaCParser_ParseExpression(metac_parser_t* self, parse_expression_flags_t flags, metac_expression_t* prev);
metac_expression_t* MetaCParser_ParseExpressionFromString(const char* exp);
metac_declaration_t* MetaCParser_ParseDeclaration(metac_parser_t* self, metac_declaration_t* parent);
metac_statement_t* MetaCParser_ParseStatement(metac_parser_t* self, metac_statement_t* parent, metac_statement_t* prev);


metac_preprocessor_directive_t MetaCParser_ParsePreproc(metac_parser_t* self,
                                                        metac_preprocessor_t* preproc,
                                                        metac_token_buffer_t* buffer);





metac_token_t* MetaCParser_Match_(metac_parser_t* self, metac_token_enum_t type,
                                  const char* filename, uint32_t lineNumber);

const char* MetaCNodeKind_toChars(metac_node_kind_t type);







typedef struct metac_lpp_t
{
    metac_lexer_state_t LexerState;
    metac_lexer_t Lexer;
    metac_parser_t Parser;

    metac_preprocessor_t Preprocessor;

} metac_lpp_t;

void MetaCLPP_Init(metac_lpp_t*);

metac_expression_t* MetaCLPP_ParseExpressionFromString(metac_lpp_t* lpp, const char* exp);

metac_statement_t* MetaCLPP_ParseStatementFromString(metac_lpp_t* lpp, const char* stmt);

metac_declaration_t* MetaCLPP_ParseDeclarationFromString(metac_lpp_t* lpp, const char* decl);


metac_preprocessor_directive_t MetaCLPP_ParsePreprocFromString(metac_lpp_t* lpp, const char* line,
                                                               metac_token_buffer_t* tokenBuffer);







metac_expression_t* AllocNewExpression(metac_expression_kind_t kind);

metac_declaration_t* AllocNewDeclaration_(metac_declaration_kind_t kind, uint32_t nodeSize, void** result_ptr, uint32_t line);





metac_statement_t* AllocNewStatement_(metac_statement_kind_t kind, uint32_t nodeSize, void** result_ptr);









struct metac_declaration_t;
const void* _emptyPointer = (const void*)0x1;






const char* MetaCExpressionKind_toChars(metac_expression_kind_t type);



_Bool

    IsExpressionNode(metac_node_kind_t Kind)
{
    return ((Kind > node_exp_invalid) & (Kind < node_exp_max));
}






static inline void InitSpecialIdentifier(metac_parser_t* self)
{
    self->SpecialNamePtr_Compiler =
        GetOrAddIdentifier(&self->IdentifierTable, 0x8481e0, "compiler");

    self->SpecialNamePtr_Context =
        GetOrAddIdentifier(&self->IdentifierTable, 0x7a2a7f, "context");

    self->SpecialNamePtr_Target =
        GetOrAddIdentifier(&self->IdentifierTable, 0x63a0c4, "target");

    self->SpecialNamePtr_Type =
        GetOrAddIdentifier(&self->IdentifierTable, 0x40869f, "type");

    self->SpecialNamePtr_Defined =
        GetOrAddIdentifier(&self->IdentifierTable, 0x40869f, "defined");
}


void MetaCParser_Init(metac_parser_t* self)
{
    self->CurrentTokenIndex = 0;
    IdentifierTable_Init(&self->IdentifierTable, 20, 13);
    IdentifierTable_Init(&self->StringTable, 12, 13);

    self->PackStackCapacity = 8;
    self->PackStack = (uint16_t*)
        calloc(sizeof(*self->PackStack), self->PackStackCapacity);
    self->PackStackTop = -1;





    self->LexerState = 0;

    self->Preprocessor = 0;

    self->BlockStatementStackCapacity = 16;
    self->BlockStatementStackCount = 0;
    self->BlockStatementStack = (stmt_block_t**)
        malloc(sizeof(stmt_block_t*) * self->BlockStatementStackCapacity);

    self->OpenParens = 0;

    MetaCLocationStorage_Init(&self->LocationStorage);

    InitSpecialIdentifier(self);

    self->DotPrinter = (metac_dot_printer_t*)malloc(sizeof(metac_dot_printer_t));
    MetaCDotPrinter_Init(self->DotPrinter, &self->IdentifierTable);

    MetaCPrinter_Init(&self->DebugPrinter,
                      &self->IdentifierTable, &self->StringTable);
}

void MetaCParser_Free(metac_parser_t* self)
{
    IdentifierTable_Free(&self->IdentifierTable);
    IdentifierTable_Free(&self->StringTable);
    free(self->BlockStatementStack);
    static const metac_parser_t zeroParser = {0};
    *self = zeroParser;
    self = 0;
}

void MetaCParser_InitFromLexer(metac_parser_t* self, metac_lexer_t* lexer)
{
    self->Lexer = lexer;
    MetaCParser_Init(self);
}






static inline metac_location_t LocationFromToken(metac_parser_t* self,
                                                 metac_token_t* tok)
{
    return self->Lexer->LocationStorage.Locations[tok->LocationId - 4];
}

metac_identifier_ptr_t RegisterIdentifier(metac_parser_t* self,
                                          metac_token_t* token)
{
    const char* identifierString =
        IdentifierPtrToCharPtr(
            &self->Lexer->IdentifierTable,
            token->IdentifierPtr
        );

    uint32_t identifierKey = token->IdentifierKey;
    return GetOrAddIdentifier(&self->IdentifierTable,
                              identifierKey, identifierString);
}

metac_identifier_ptr_t RegisterString(metac_parser_t* self,
                                      metac_token_t* token)
{
    const char* string =
        IdentifierPtrToCharPtr(
            &self->Lexer->StringTable,
            token->StringPtr
        );
        uint32_t stringKey = token->StringKey;
        return GetOrAddIdentifier(&self->StringTable,
                                  stringKey, string);
}

static inline

             _Bool

                  MetaCParser_PeekMatch(metac_parser_t* self, metac_token_enum_t expectedType,

                                                                                               _Bool

                                                                                                    optional)
{
    metac_token_t* peekToken =
        (MetaCParser_PeekToken_(self, 1, 182));


   _Bool

        result =

                 1

                     ;

    metac_location_t loc = {0};
    if (self->LexerState)
    {
        loc.StartLine = self->LexerState->Line;
        loc.StartColumn = self->LexerState->Column;
    }

    if (!peekToken || peekToken->TokenType != expectedType)
    {
        result =

                0

                     ;
        if (!optional)
        {
            fprintf(

           stderr

           , "ParseError[%s:%u]: {%u:%u}" "expected %s but got %s" "\n",


            "../metac_parser.c"

            ,


            200

            , (loc.StartLine), (loc.StartColumn), MetaCTokenEnum_toChars(expectedType), MetaCTokenEnum_toChars(peekToken ? peekToken->TokenType : tok_eof)); ParseErrorBreak();


             ;
        }
    }

    return result;
}

void MetaCParser_Advance(metac_parser_t* self)
{

    metac_preprocessor_t* preProc = self->Preprocessor;
    if (preProc && preProc->DefineTokenStackCount)
    {
        metac_token_t_array* tokens = &preProc->DefineTokenStack[preProc->DefineTokenStackCount - 1];
        uint32_t* idx = &preProc->DefineTokenIndexStack[preProc->DefineTokenStackCount - 1];
        (*idx)++;

        if (tokens->Count <= (*idx))
        {
            preProc->DefineTokenStackCount--;
        }
    }
    else

    self->CurrentTokenIndex++;
}

static inline void LoadTokens(metac_parser_t* self,
                              uint32_t* tokenCount,
                              metac_token_t** tokens,
                              uint32_t** tokenOffset)
{





    metac_preprocessor_t* preProc = self->Preprocessor;
    (*tokenCount) = ((preProc && preProc->DefineTokenStackCount > 0) ?
        preProc->DefineTokenStack[preProc->DefineTokenStackCount - 1].Count :
        self->Lexer->TokenCount);

    (*tokens) = ((preProc ? preProc->DefineTokenStackCount > 0 : 0) ?
        preProc->DefineTokenStack[preProc->DefineTokenStackCount - 1].Ptr :
        self->Lexer->Tokens);

    (*tokenOffset) = ((preProc ? preProc->DefineTokenStackCount > 0 : 0) ?
        &preProc->DefineTokenIndexStack[preProc->DefineTokenStackCount - 1] :
        &self->CurrentTokenIndex);

}

metac_token_t* MetaCPreProcessor_NextDefineToken(metac_preprocessor_t* self)
{
    printf("F: %s\n", __FUNCTION__);


    metac_token_t* result = 0;
    metac_token_t_array tokens;
Lbegin:
     tokens =
        (self->DefineTokenStack)[self->DefineTokenStackCount - 1];

    uint32_t defineTokenIndex =
        ((self->DefineTokenIndexStack)[self->DefineTokenStackCount - 1])++;
    printf("defineTokenIndex: %u\n", defineTokenIndex);
    if (defineTokenIndex < tokens.Count)
    {
        result = &tokens.Ptr[defineTokenIndex];
    }
    else
    {
        if (--self->DefineTokenStackCount != 0)
        {
            self->DefineTokenIndexStack[self->DefineTokenStackCount] = 0;
            goto Lbegin;
        }
    }

    return result;
}

metac_token_t* MetaCPreProcessor_PeekDefineToken(metac_preprocessor_t* self,
                                                 uint32_t offset)
{


    metac_token_t* result = 0;
    metac_token_t_array tokens;
Lbegin:
    tokens =
        self->DefineTokenStack[self->DefineTokenStackCount - 1];

    uint32_t defineTokenIndex =
        self->DefineTokenIndexStack[self->DefineTokenStackCount - 1];




    if (defineTokenIndex + offset < tokens.Count)
    {
        result = &tokens.Ptr[defineTokenIndex + offset];

    }
    else
    {
        if (--self->DefineTokenStackCount != 0)
        {
            self->DefineTokenIndexStack[self->DefineTokenStackCount] = 0;
            goto Lbegin;
        }
    }

    return result;
}

metac_token_t* MetaCParser_HandleIdentifier(metac_parser_t* self,
                                            metac_token_t* tok,
                                            metac_preprocessor_t* preProc)
{
    uint32_t tokenCount;
    metac_token_t* tokens;
    uint32_t* tokenOffset;

    LoadTokens(self, &tokenCount, &tokens, &tokenOffset);

    metac_token_t* result = tok;

    const uint32_t idKey = result->IdentifierKey;

    int32_t defineSlotIdx;

    LcontinuePreprocSearch:
    do {
        defineSlotIdx = (
            preProc ?
            MetaCIdentifierTable_HasKey(&preProc->DefineIdentifierTable, idKey) :
            -1
        );
        if (defineSlotIdx != -1)
            break;
        preProc = preProc ? preProc->Parent : 0;
    } while(preProc);

    if (defineSlotIdx != -1)
    {
        const char* idChars = IdentifierPtrToCharPtr(&self->Lexer->IdentifierTable,
                                                     result->IdentifierPtr);
        metac_preprocessor_define_ptr_t matchingDefine = {0};

        matchingDefine = MetaCPreProcessor_GetDefine(preProc, idKey, idChars);

        if (!matchingDefine.v)
        {
            preProc = preProc->Parent;
            goto LcontinuePreprocSearch;
        }

        metac_preprocessor_define_t define =
            self->Preprocessor->DefineTable.DefineMemory[matchingDefine.v - 4];
        (((*tokenOffset) < tokenCount) ? (tokens + ((*tokenOffset)++)) : 0);

        if (define.ParameterCount > 0 || define.IsVariadic)
        {
            metac_token_t* tok = (((*tokenOffset) < tokenCount) ? (tokens + ((*tokenOffset)++)) : 0);
            result = (((*tokenOffset) < tokenCount) ? (tokens + ((*tokenOffset)++)) : 0);
            if (tok->TokenType != tok_lParen)
            {
                printf("Expected '(' but got %s\n", MetaCTokenEnum_toChars(tok->TokenType));
            }

            metac_token_t paramTokens[64];
            uint32_t paramTokenIndex = 0;
            uint32_t paramTokenCount = 0;
            uint32_t paramTokenCapacity = ((unsigned int)(sizeof(paramTokens) / sizeof(paramTokens[0])));

            metac_token_t_array _paramArrays[32] = {}; metac_token_t_array_array paramArrays = { _paramArrays, 0, 32 };;

            uint32_t ParenDepth = 1;

            for(;;)
            {
                if (result->TokenType == tok_lParen)
                    ParenDepth += 1;
                if (result->TokenType == tok_rParen)
                    ParenDepth -= 1;

                if (ParenDepth == 0 ||
                    (ParenDepth == 1 && result->TokenType == tok_comma))
                {
                    metac_token_t_array param = {
                        paramTokens + paramTokenIndex,
                        paramTokenCount - paramTokenIndex,
                        paramTokenCount - paramTokenIndex
                    };

                    if (paramArrays.Count >= paramArrays.Capacity) { if (((unsigned int)(sizeof(_paramArrays) / sizeof(_paramArrays[0]))) == paramArrays.Capacity) _StackArrayRealloc(((void**)&paramArrays.Ptr), &paramArrays.Capacity, sizeof(_paramArrays[0])); else _newMemRealloc(((void**)&paramArrays.Ptr), &paramArrays.Capacity, sizeof(_paramArrays[0])); } paramArrays.Ptr[paramArrays.Count++] = param;;
                    printf("Added paramTuple of %u tokens\n", paramTokenCount - paramTokenIndex);
                    paramTokenIndex = paramTokenCount;
                    if (ParenDepth == 0)
                    {
                        break;
                    }
                    else
                    {
                        result = (((*tokenOffset) < tokenCount) ? (tokens + ((*tokenOffset)++)) : 0);
                        continue;
                    }
                }

                if (paramTokenCount < paramTokenCapacity)
                {
                    printf("[%u] Adding another token\n", paramTokenCount);
                    paramTokens[paramTokenCount++] = *result;
                    result = (((*tokenOffset) < tokenCount) ? (tokens + ((*tokenOffset)++)) : 0);
                }
                else
                {



                }
            }



            (((*tokenOffset) < tokenCount) ? (tokens + ((*tokenOffset)++)) : 0);

            printf("define.TokenCount: %u\n", define.TokenCount);
            MetaCPreProcessor_PushDefine(preProc, &define, paramArrays);
            result =
                &preProc->DefineTokenStack[preProc->DefineTokenStackCount - 1].Ptr[
                    preProc->DefineTokenIndexStack[preProc->DefineTokenStackCount - 1]];
        }
        else
        {
   metac_token_t_array_array emptyParams = {0};
   MetaCPreProcessor_PushDefine(preProc, &define, emptyParams);



            uint32_t stackIdx = preProc->DefineTokenStackCount - 1;
            result = &preProc->DefineTokenStack[stackIdx].Ptr[
                preProc->DefineTokenIndexStack[stackIdx - 1]];
            printf("result: %s\n", MetaCTokenEnum_toChars(result->TokenType));
        }
    }

    return result;

}


metac_token_t* MetaCParser_NextToken(metac_parser_t* self)
{
    uint32_t tokenCount;
    metac_token_t* tokens;
    uint32_t* tokenOffset;

    metac_token_t* result;

    LoadTokens(self, &tokenCount, &tokens, &tokenOffset);



    metac_preprocessor_t* preProc = self->Preprocessor;
    if (preProc && preProc->DefineTokenStackCount)
    {
    LnextDefineToken:
        result = MetaCPreProcessor_NextDefineToken(preProc);
        if (!result)
            goto LnextToken;
    }
    else
    {

                                      ;
    LnextToken:
        result = (((*tokenOffset) < tokenCount) ? (tokens + ((*tokenOffset)++)) : 0);
    }

    if(result)
    {
        if (result->TokenType == tok_identifier && preProc)
        {
            printf(IdentifierPtrToCharPtr(&self->Lexer->IdentifierTable, result->IdentifierPtr));
            uint32_t oldDefineStackTop = preProc->DefineTokenStackCount;
            result = MetaCParser_HandleIdentifier(self, result, preProc);


           _Bool

                wasDefine =
                (oldDefineStackTop < preProc->DefineTokenStackCount);
            if (wasDefine)
            {
                printf("Exchanged identifier for: %s\n", MetaCTokenEnum_toChars(result->TokenType));
                if (result->TokenType == tok_identifier)
                printf("    id: %s\n", IdentifierPtrToCharPtr(&self->Lexer->IdentifierTable, result->IdentifierPtr));
                MetaCParser_Advance(self);
            }
        }
    }
    else
    {

    }


    if (result)
    {
        self->LastLocation = self->Lexer->LocationStorage.Locations[result->LocationId - 4];
    }
    return result;
}




uint32_t MetaCParser_HowMuchLookahead(metac_parser_t* self)
{
    return (self->Lexer->TokenCount - self->CurrentTokenIndex);
}

metac_token_t* MetaCParser_PeekToken_(metac_parser_t* self, int32_t p, uint32_t line)
{
    metac_token_t* result = 0;




    metac_preprocessor_t* preProc = self->Preprocessor;

    uint32_t CurrentTokenIndex =
        (preProc && preProc->DefineTokenStackCount)
            ? preProc->DefineTokenIndexStack[preProc->DefineTokenStackCount - 1]:
            self->CurrentTokenIndex;

LpeekDefine:
    if (preProc && preProc->DefineTokenStackCount)
    {
        result = MetaCPreProcessor_PeekDefineToken(preProc, p - 1);
        if (!result)
            goto Lpeek;
    } else

    if ((uint32_t)(self->CurrentTokenIndex + (p - 1)) < self->Lexer->TokenCount)
    {
Lpeek:
        result = self->Lexer->Tokens + self->CurrentTokenIndex + (p - 1);
        self->LastLocation =
            self->Lexer->LocationStorage.Locations[result->LocationId - 4];


        if(result)
        {
            if (result->TokenType == tok_identifier)
            {

                result = MetaCParser_HandleIdentifier(self, result, preProc);


            }
        }

    }

    return result;
}

metac_token_t* MetaCParser_Match_(metac_parser_t* self, metac_token_enum_t type,
                                 const char* filename, uint32_t lineNumber)
{
    metac_token_t* token = (MetaCParser_PeekToken_(self, 1, 582));
    metac_token_enum_t got = (token ? token->TokenType : tok_eof);
    if (got != type)
    {
        if (got != tok_eof)
        {
            metac_location_t loc = self->Lexer->LocationStorage.Locations[token->LocationId - 4];

            printf("[%s:%u] Expected: %s -- Got: %s {line: %u: col: %u}\n",
                filename, lineNumber,
                MetaCTokenEnum_toChars(type), MetaCTokenEnum_toChars(got),
                loc.StartLine, loc.StartColumn);
        }
        else
        {
            printf("[%s:%u] Expected: %s -- Got: End of file\n",
                filename, lineNumber,
                MetaCTokenEnum_toChars(type));
        }
    }
    else
    {
        MetaCParser_Advance(self);

    }
    return token;
}





const char* BinExpTypeToChars(metac_binary_expression_kind_t t)
{
    switch(t)
    {
        case bin_exp_invalid:

        case exp_comma : return ",";
        case exp_dot : return ".";
        case exp_dotdot : return "..";
        case exp_arrow : return "->";

        case exp_add : return "+";
        case exp_sub : return "-";
        case exp_mul : return "*";
        case exp_div : return "/";
        case exp_rem : return "%";
        case exp_xor : return "^";
        case exp_or : return "|";
        case exp_and : return "&";
        case exp_lsh : return "<<";
        case exp_rsh : return ">>";

        case exp_oror : return "||";
        case exp_andand : return "&&";

        case exp_assign : return "=";

        case exp_add_ass : return "+=";
        case exp_sub_ass : return "-=";
        case exp_mul_ass : return "*=";
        case exp_div_ass : return "/=";
        case exp_rem_ass : return "%=";
        case exp_xor_ass : return "^=";
        case exp_or_ass : return "|=";
        case exp_and_ass : return "&=";
        case exp_lsh_ass : return "<<=";
        case exp_rsh_ass : return ">>=";

        case exp_eq : return "==";
        case exp_neq : return "!=";
        case exp_lt : return "<";
        case exp_le : return "<=";
        case exp_gt : return ">";
        case exp_ge : return ">=";
        case exp_spaceship : return "<=>";
    }



    return 0;
}

metac_expression_kind_t BinExpTypeFromTokenType(metac_token_enum_t tokenType)
{
    metac_expression_kind_t result = exp_invalid;
    if (((tokenType >= tok_comma) & (tokenType <= tok_spaceship)))
    {
        result = (metac_expression_kind_t)((int)tokenType -
                ((int)tok_comma -
                 (int)exp_comma));
    }

    if (tokenType == tok_lParen)
        result = exp_call;

    if (tokenType == tok_lBracket)
        result = exp_index;

    return result;
}

metac_expression_kind_t ExpTypeFromTokenType(metac_token_enum_t tokenType)
{
    if (tokenType == tok_uint)
    {
        return exp_signed_integer;
    }
    else if (tokenType == tok_string)
    {
        return exp_string;
    }
    else if (tokenType == tok_lParen)
    {
        return exp_paren;
    }
    else if (tokenType == tok_kw_inject)
    {
        return exp_inject;
    }
    else if (tokenType == tok_kw_eject)
    {
        return exp_eject;
    }
    else if (tokenType == tok_kw_assert)
    {
        return exp_assert;
    }
    else if (tokenType == tok_dollar)
    {
        return exp_outer;
    }
    else if (tokenType == tok_and)
    {
        return exp_addr_or_and;
    }
    else if (tokenType == tok_star)
    {
        return exp_ptr_or_mul;
    }
    else if (tokenType == tok_identifier)
    {
        return exp_identifier;
    }
    else
    {


                ;
        return exp_invalid;
    }

}


static inline

             _Bool

                  IsPostfixOperator(metac_token_enum_t t)
{
    return (t == tok_plusplus || t == tok_minusminus);
}

static inline

             _Bool

                  IsBinaryOperator(metac_token_enum_t t, parse_expression_flags_t flags)
{
    if ((flags & (expr_flags_call | expr_flags_enum)) != 0
     && (t == tok_comma))
        return

              0

                   ;

    return ((t >= tok_comma && t <= tok_spaceship)
            || t == tok_lParen || t == tok_lBracket);
}

typedef enum precedence_level_t
{
    prec_none,
    prec_comma,
    prec_op_assign,
    prec_oror,
    prec_andand,
    prec_or,
    prec_xor,
    prec_and,
    prec_eq,
    prec_cmp,
    prec_shift,
    prec_add,
    prec_mul,
    prec_max
} precedence_level_t;

static inline uint32_t OpToPrecedence(metac_expression_kind_t exp)
{
    if (exp == exp_comma)
    {
        return 1;
    }
    else if (exp >= exp_assign && exp <= exp_rsh_ass)
    {
        return 2;
    }
    else if (exp == exp_ternary)
    {
        return 3;
    }
    else if (exp == exp_oror)
    {
        return 4;
    }
    else if (exp == exp_andand)
    {
        return 5;
    }
    else if (exp == exp_or)
    {
        return 7;
    }
    else if (exp == exp_xor)
    {
        return 8;
    }
    else if (exp == exp_and)
    {
        return 9;
    }
    else if (exp == exp_eq || exp == exp_neq)
    {
        return 10;
    }
    else if (exp >= exp_lt && exp <= exp_ge)
    {
        return 11;
    }
    else if (exp == exp_rsh || exp == exp_lsh)
    {
        return 12;
    }
    else if (exp == exp_add || exp == exp_sub)
    {
        return 13;
    }
    else if (exp == exp_div || exp == exp_mul || exp == exp_rem)
    {
        return 14;
    }
    else if (exp == exp_ptr || exp == exp_arrow || exp == exp_dot || exp == exp_addr
          || exp == exp_increment || exp == exp_decrement)
    {
        return 15;
    }
    else if (exp == exp_call || exp == exp_index
          || exp == exp_compl || exp == exp_post_increment
          || exp == exp_post_decrement)
    {
        return 16;
    }
    else if (exp == exp_umin || exp == exp_unary_dot
          || exp == exp_sizeof || exp == exp_not)
    {
        return 17;
    }
    else if (exp == exp_paren
          || exp == exp_signed_integer
          || exp == exp_string
          || exp == exp_identifier
          || exp == exp_char
          || exp == exp_tuple
          || exp == exp_type)
    {
        return 18;
    }


    return 0;
}

static inline

             _Bool

                  IsTypeToken(metac_token_enum_t tokenType)
{


   _Bool

         result =
           ( tokenType == tok_kw_const
            || (tokenType >= tok_kw_auto && tokenType <= tok_kw_double)
            || tokenType == tok_kw_unsigned
            || tokenType == tok_kw_signed
            || tokenType == tok_star
            || tokenType == tok_kw_struct
            || tokenType == tok_kw_enum
            || tokenType == tok_kw_union
            || tokenType == tok_identifier );

    return result;
}

static inline

             _Bool

                  IsDeclarationToken(metac_token_enum_t tokenType)
{



  _Bool

       result = ( tokenType == tok_kw_static
                  || tokenType == tok_kw_inline
                  || tokenType == tok_kw_extern
                  || tokenType == tok_comment_multi
                  || tokenType == tok_comment_single
                  || IsTypeToken(tokenType) );

    return result;
}

static inline

             _Bool

                  IsDeclarationFirstToken(metac_token_enum_t tokenType)
{
    if (tokenType == tok_star)
        return

              0

                   ;

    return IsDeclarationToken(tokenType);
}


static inline

             _Bool

                  IsPrimaryExpressionToken(metac_token_enum_t tokenType)
{

    if (IsTypeToken(tokenType) && tokenType != tok_star)
        return

              1

                  ;

    switch(tokenType)
    {
    case tok_lParen:
    case tok_uint:
    case tok_string:
    case tok_char:
    case tok_identifier:
    case tok_lBrace:
        return

              1

                  ;
    default:
        return

              0

                   ;
    }
}

static inline

             _Bool

                  IsPunctuationToken(metac_token_enum_t tok)
{
    if (tok == tok_star)
        return

              0

                   ;
    else
    return (
        (IsBinaryOperator(tok, expr_flags_none) && tok != tok_star)
        || tok == tok_dotdot
        || tok == tok_comma
        || tok == tok_semicolon
        || tok == tok_cat);
}

static

      _Bool

           CouldBeCast(metac_parser_t* self, metac_token_enum_t tok)
{


   _Bool

        result =

                 1

                     ;

    if (tok != tok_lParen)
        return

              0

                   ;



    metac_token_t* peek;


   _Bool

        seenStar =

                   0

                        ;
    int rParenPos = 0;
    for(int peekCount = 2;
        (peek = (MetaCParser_PeekToken_(self, peekCount, 936))), peek;
        peekCount++)
    {
        if (peek->TokenType == tok_lParen)
            return

                  0

                       ;

        if (peek->TokenType == tok_rParen)
        {
            rParenPos = peekCount;
            break;
        }
        if (peek->TokenType == tok_star)
        {
            seenStar =

                      1

                          ;
            if (peekCount == 2)
                return

                      0

                           ;
        }
        else if (IsBinaryOperator(peek->TokenType, expr_flags_none))
        {
            return

                  0

                       ;
        }
        if (!IsTypeToken(peek->TokenType))
        {
            return

                  0

                       ;
        }
        else
        {
            if (peek->TokenType == tok_identifier && seenStar)
                return

                      0

                           ;
        }
    }

    if (rParenPos)
    {
        metac_token_t* afterParen =
            (MetaCParser_PeekToken_(self, rParenPos + 1, 971));
        if (!afterParen || IsPunctuationToken(afterParen->TokenType))
            return

                  0

                       ;
    }

    return

          1

              ;
}

typedef enum typescan_flags_t
{
    TypeScan_None = 0,
    TypeScan_SeenStar = (1 << 1),
    TypeScan_FirstWasIdentifier = (1 << 2),
} typescan_flags_t;




static inline

             _Bool

                  CouldBeType(metac_parser_t* self, metac_token_enum_t tok, parse_expression_flags_t eflags)
{


   _Bool

        result =

                 0

                      ;
    metac_token_t* peek = 0;
    uint32_t peekN = 2;
    typescan_flags_t flags = TypeScan_None;

    if (eflags & expr_flags_pp)
        return

              0

                   ;

    if (tok == tok_identifier)
    {
        (*(uint32_t*)(&flags)) |= TypeScan_FirstWasIdentifier;
    }

    if (tok == tok_star)
    {
        result =

                0

                     ;
    }
    else while (IsTypeToken(tok))
    {
        result |=

                 1

                     ;



        if (tok == tok_star)
            (*(uint32_t*)(&flags)) |= TypeScan_SeenStar;
        else if (((flags & TypeScan_SeenStar) != 0)
              && tok == tok_identifier)
        {
            result =

                    0

                         ;
            break;
        }
        peek = (MetaCParser_PeekToken_(self, peekN++, 1022));
        tok = (peek ? peek->TokenType : tok_eof);
    }


    if ((flags & TypeScan_FirstWasIdentifier) != 0
      && peekN <= 3)
    {
        result =

                0

                     ;
    }

    return result;
}


decl_type_t* MetaCParser_ParseTypeDeclaration(metac_parser_t* self,
                                              metac_declaration_t* parent,
                                              metac_declaration_t* prev);

static const metac_location_t invalidLocation = {0,0,0,0,0};





metac_expression_t* MetaCParser_ParsePrimaryExpression(metac_parser_t* self, parse_expression_flags_t flags)
{
    metac_expression_t* result = 0;

    metac_token_t* currentToken = (MetaCParser_PeekToken_(self, 1, 1051));
    if (currentToken)
    {





    }

    if (self->Preprocessor)
    {
        printf("Preproc is there -- InDefine: %u\n",
               self->Preprocessor->DefineTokenStackCount > 0);
    }

    metac_token_enum_t tokenType =
        (currentToken ? currentToken->TokenType : tok_eof);

    metac_location_t loc = (currentToken ?
        LocationFromToken(self, currentToken) :
        invalidLocation);
    uint32_t hash = 0;


    if (CouldBeType(self, tokenType, flags))
    {
        decl_type_t* type =
            MetaCParser_ParseTypeDeclaration(self, 0, 0);
        result = AllocNewExpression(exp_type);
        result->TypeExp = type;
        MetaCLocation_Expand(&loc,
            self->LocationStorage.Locations[result->TypeExp->LocationIdx - 4]);
        result->Hash = (crc32c_nozero(0x40869f, &(result->TypeExp->Hash), sizeof(result->TypeExp->Hash)));
    }
    else


    if (tokenType == tok_lParen && CouldBeCast(self, tokenType))
    {

        hash = 0x4520d2;
        result = AllocNewExpression(exp_cast);

        metac_token_t* lParen = (MetaCParser_Match_((self), (tok_lParen), "../metac_parser.c", 1095));
        result->CastType = MetaCParser_ParseTypeDeclaration(self, 0, 0);
        hash = (crc32c_nozero(hash, &(result->CastType->Hash), sizeof(result->CastType->Hash)));
        (MetaCParser_Match_((self), (tok_rParen), "../metac_parser.c", 1098));
        result->CastExp = MetaCParser_ParseExpression(self, expr_flags_none, 0);
        hash = (crc32c_nozero(hash, &(result->CastExp->Hash), sizeof(result->CastExp->Hash)));
        MetaCLocation_Expand(&loc, self->LocationStorage.Locations[result->CastExp->LocationIdx - 4]);
        result->Hash = hash;
    }
    else if (tokenType == tok_uint)
    {
        (MetaCParser_Match_((self), (tok_uint), "../metac_parser.c", 1106));
        result = AllocNewExpression(exp_signed_integer);
        result->ValueI64 = currentToken->ValueI64;
        int32_t val32 = (int32_t)currentToken->ValueI64;

        if (val32 == currentToken->ValueI64)
        {
            result->Hash = (crc32c_nozero(~0, &(val32), sizeof(val32)));
        }
        else
        {
            result->Hash =
                    (crc32c_nozero(~0, &(currentToken->ValueI64), sizeof(currentToken->ValueI64)));
        }

    }
    else if (tokenType == tok_string)
    {

        (MetaCParser_Match_((self), (tok_string), "../metac_parser.c", 1125));
        result = AllocNewExpression(exp_string);
        result->StringPtr = RegisterString(self, currentToken);
        result->StringKey = currentToken->StringKey;
        result->Hash = currentToken->StringKey;

    }
    else if (tokenType == tok_char)
    {
        (MetaCParser_Match_((self), (tok_char), "../metac_parser.c", 1134));
        result = AllocNewExpression(exp_char);
        const uint32_t length = currentToken->charLength;
        const char* chars = currentToken->chars;
        const uint32_t hash = crc32c_nozero(~0, chars, length);

        (*(uint64_t*)result->Chars) =
            (*(uint64_t*) chars);
        result->CharKey = ( (uint32_t)((hash) & 0xFFFFFFF) | (((uint32_t)(length)) << 28) );
        result->Hash = result->CharKey;
    }
    else if (tokenType == tok_identifier)
    {
        result = AllocNewExpression(exp_identifier);
        (MetaCParser_Match_((self), (tok_identifier), "../metac_parser.c", 1148));
        result->IdentifierPtr = RegisterIdentifier(self, currentToken);
        result->IdentifierKey = currentToken->IdentifierKey;
        result->Hash = currentToken->IdentifierKey;

    }
    else if (tokenType == tok_lParen)
    {
        self->OpenParens++;
        (MetaCParser_Match_((self), (tok_lParen), "../metac_parser.c", 1157));
        result = AllocNewExpression(exp_paren);
        {
            if (!MetaCParser_PeekMatch(self, tok_rParen, 1))
                result->E1 = MetaCParser_ParseExpression(self, expr_flags_none, 0);
        }

        result->Hash = (crc32c_nozero(crc32c_nozero(~0, "()", 2), &(result->E1->Hash), sizeof(result->E1->Hash)));

        metac_token_t* endParen =
            (MetaCParser_Match_((self), (tok_rParen), "../metac_parser.c", 1167));
        MetaCLocation_Expand(&loc, LocationFromToken(self, endParen));
        self->OpenParens--;

    }
    else if (tokenType == tok_lBrace)
    {

        (MetaCParser_Match_((self), (tok_lBrace), "../metac_parser.c", 1175));
        exp_tuple_t* tupleList = (exp_tuple_t*)((void*)0x1);
        uint32_t hash = ~0;

        exp_tuple_t** nextElement = &tupleList;
        uint32_t nElements = 0;
        while (!MetaCParser_PeekMatch(self, tok_rBrace,

                                                       1

                                                           ))
        {
            nElements++;


            (*nextElement) = (exp_tuple_t*)AllocNewExpression(exp_tuple);
            metac_expression_t* exp = MetaCParser_ParseExpression(self, expr_flags_call, 0);
            hash = (crc32c_nozero(hash, &(exp->Hash), sizeof(exp->Hash)));
            ((*nextElement)->Expression) = exp;
            nextElement = &((*nextElement)->Next);
            (*nextElement) = (exp_tuple_t*) _emptyPointer;

            if(MetaCParser_PeekMatch(self, tok_comma,

                                                     1

                                                         ))
            {
                (MetaCParser_Match_((self), (tok_comma), "../metac_parser.c", 1195));
            }
        }
        metac_token_t* endBrace =
            (MetaCParser_Match_((self), (tok_rBrace), "../metac_parser.c", 1199));
        MetaCLocation_Expand(&loc, LocationFromToken(self, endBrace));

        result = AllocNewExpression(exp_tuple);
        result->Hash = hash;
        result->TupleExpressionList = tupleList;
        result->TupleExpressionCount = nElements;


    }

    else
    {


    }

    result->LocationIdx = MetaCLocationStorage_Store(&self->LocationStorage, loc);
    return result;
}

metac_expression_t* MetaCParser_ParsePostfixExpression(metac_parser_t* self,
                                                       metac_expression_t* left)
{
    metac_expression_t* result = 0;

    metac_token_t* peek = (MetaCParser_PeekToken_(self, 1, 1239));

    metac_token_enum_t peekTokenType = peek->TokenType;

    metac_location_t loc =
        self->LocationStorage.Locations[left->LocationIdx - 4];

    if (peekTokenType == tok_plusplus)
    {
        (MetaCParser_Match_((self), (peekTokenType), "../metac_parser.c", 1248));
        metac_expression_t* E1 = left;
        result = AllocNewExpression(exp_post_increment);
        result->E1 = E1;
        uint32_t hash = 0x61d225eb;
        result->Hash = (crc32c_nozero(result->E1->Hash, &(hash), sizeof(hash)));
    }
    else if (peekTokenType == tok_minusminus)
    {
        (MetaCParser_Match_((self), (peekTokenType), "../metac_parser.c", 1257));

        metac_expression_t* E1 = left;
        result = AllocNewExpression(exp_post_decrement);
        result->E1 = E1;
        uint32_t hash = 0x2ebc9331;
        result->Hash = (crc32c_nozero(result->E1->Hash, &(hash), sizeof(hash)));
    }
    else

    MetaCLocation_Expand(&loc, LocationFromToken(self, peek));
    result->LocationIdx =
        MetaCLocationStorage_Store(&self->LocationStorage, loc);

    return result;
}
decl_type_t* MetaCParser_ParseTypeDeclaration(metac_parser_t* self, metac_declaration_t* parent, metac_declaration_t* prev);

static inline metac_expression_t* ParseDotSpecialExpression(metac_parser_t* self,
                                                            metac_expression_kind_t k)
{
    metac_expression_t* result = 0;
    metac_token_t* startToken;

    if (MetaCParser_PeekMatch(self, tok_dot,

                                            1

                                                ))
    {
        startToken = (MetaCParser_Match_((self), (tok_dot), "../metac_parser.c", 1284));
        metac_location_t loc = LocationFromToken(self, startToken);
        metac_token_t* peek;
        peek = (MetaCParser_PeekToken_(self, 1, 1287));
        if (!peek)
        {
            fprintf(

                   stderr

                         , "Expected expression after '.special.'\n");
        }
        else
        {
            result = AllocNewExpression(k);
            result->E1 = MetaCParser_ParseExpression(self, expr_flags_none, 0);
            metac_location_t endLoc = self->LocationStorage.Locations[result->E1->LocationIdx - 4];
            MetaCLocation_Expand(&loc, endLoc);
            result->LocationIdx = MetaCLocationStorage_Store(&self->LocationStorage, loc);
        }
    }

    return result;
}

static inline uint32_t PtrVOnMatch(metac_parser_t* self,
                                   metac_identifier_ptr_t ptr,
                                   const char* matchStr,
                                   uint32_t matchLen)
{
    const char* idChars = IdentifierPtrToCharPtr(&self->IdentifierTable,
                                                 ptr);
    return ((!memcmp(idChars, matchStr, matchLen)) ? ptr.v : 0);
}


static inline metac_expression_t* ParseUnaryDotExpression(metac_parser_t* self)
{
    metac_expression_t* result = 0;

    metac_token_t* peek;
    peek = (MetaCParser_PeekToken_(self, 1, 1321));
    if (peek && peek->TokenType == tok_identifier)
    {
        switch(peek->IdentifierKey)
        {
            case 0x8481e0:
            {
                metac_identifier_ptr_t identifierPtr =
                    RegisterIdentifier(self, peek);
                if (self->SpecialNamePtr_Compiler.v == identifierPtr.v)
                {
                    (MetaCParser_Match_((self), (tok_identifier), "../metac_parser.c", 1332));
                    result = ParseDotSpecialExpression(self, exp_dot_compiler);
                }
            } break;
            case 0x7a2a7f:
            {
                metac_identifier_ptr_t identifierPtr =
                    RegisterIdentifier(self, peek);
                if (self->SpecialNamePtr_Context.v == identifierPtr.v)
                {
                    (MetaCParser_Match_((self), (tok_identifier), "../metac_parser.c", 1342));
                    result = ParseDotSpecialExpression(self, exp_dot_context);
                }
            } break;
            case 0x63a0c4:
            {
                metac_identifier_ptr_t identifierPtr =
                    RegisterIdentifier(self, peek);
                if (self->SpecialNamePtr_Target.v == identifierPtr.v)
                {
                    (MetaCParser_Match_((self), (tok_identifier), "../metac_parser.c", 1352));
                    result = ParseDotSpecialExpression(self, exp_dot_target);
                }
            } break;
        }
    }

    if (!result)
    {
        result = AllocNewExpression(exp_unary_dot);
        result->E1 = MetaCParser_ParseExpression(self, expr_flags_unary, 0);
        result->Hash = (crc32c_nozero(crc32c_nozero(~0, ".", sizeof(".") - 1), &(result->E1->Hash), sizeof(result->E1->Hash)))


         ;
    }

    return result;
}





metac_expression_t* MetaCParser_ParseUnaryExpression(metac_parser_t* self, parse_expression_flags_t eflags)
{
    metac_expression_t* result = 0;

    metac_token_t* currentToken = (MetaCParser_PeekToken_(self, 1, 1380));
    metac_token_enum_t tokenType =
        (currentToken ? currentToken->TokenType : tok_eof);
    metac_location_t loc = LocationFromToken(self, currentToken);



   _Bool

        isPrimaryExp =

                       0

                            ;

    if (tokenType == tok_dot)
    {
        (MetaCParser_Match_((self), (tok_dot), "../metac_parser.c", 1389));
        result = ParseUnaryDotExpression(self);
    }
    else if (tokenType == tok_kw_eject)
    {
        (MetaCParser_Match_((self), (tok_kw_eject), "../metac_parser.c", 1394));
        result = AllocNewExpression(exp_eject);

        result->E1 = MetaCParser_ParseExpression(self, expr_flags_none, 0);
        result->Hash = (crc32c_nozero(0x578f00, &(result->E1->Hash), sizeof(result->E1->Hash)));


    }
    else if (tokenType == tok_kw_inject)
    {
        (MetaCParser_Match_((self), (tok_kw_inject), "../metac_parser.c", 1404));
        result = AllocNewExpression(exp_inject);

        result->E1 = MetaCParser_ParseExpression(self, expr_flags_none, 0);
        result->Hash = (crc32c_nozero(0x6d00ba, &(result->E1->Hash), sizeof(result->E1->Hash)));


    }
    else if (tokenType == tok_kw_typeof)
    {
        (MetaCParser_Match_((self), (tok_kw_typeof), "../metac_parser.c", 1414));
        result = AllocNewExpression(exp_typeof);

        metac_token_t* nextToken = (MetaCParser_PeekToken_(self, 1, 1417));
        if (!nextToken || nextToken->TokenType != tok_lParen)
        {
            fprintf(

           stderr

           , "ParseError[%s:%u]: {%u:%u}" "Expected typeof to be followed by '('" "\n", "../metac_parser.c", 1420, (loc.StartLine), (loc.StartColumn)); ParseErrorBreak();;
        }

        metac_expression_t* parenExp = MetaCParser_ParseExpression(self, expr_flags_none, 0);



        result->E1 = parenExp->E1;
    }
    else if (tokenType == tok_kw_sizeof)
    {
        (MetaCParser_Match_((self), (tok_kw_sizeof), "../metac_parser.c", 1430));
        result = AllocNewExpression(exp_sizeof);
        metac_token_t* nextToken = (MetaCParser_PeekToken_(self, 1, 1432));


       _Bool

            wasParen =

                       0

                            ;
        if (nextToken->TokenType == tok_lParen)
        {
            wasParen =

                      1

                          ;
            (MetaCParser_Match_((self), (tok_lParen), "../metac_parser.c", 1437));
        }
        result->E1 = MetaCParser_ParseExpression(self, (parse_expression_flags_t)(eflags & expr_flags_pp), 0);
        if (wasParen)
        {
            (MetaCParser_Match_((self), (tok_rParen), "../metac_parser.c", 1442));
        }
        result->Hash = (crc32c_nozero(0x6222dd, &(result->E1->Hash), sizeof(result->E1->Hash)));
    }
    else if (tokenType == tok_kw_assert)
    {
        (MetaCParser_Match_((self), (tok_kw_assert), "../metac_parser.c", 1448));
        result = AllocNewExpression(exp_assert);

        metac_token_t* nextToken = (MetaCParser_PeekToken_(self, 1, 1451));
        if (!nextToken || nextToken->TokenType != tok_lParen)
        {
            fprintf(

           stderr

           , "ParseError[%s:%u]: {%u:%u}" "Expected assert to be followed by '('" "\n", "../metac_parser.c", 1454, (loc.StartLine), (loc.StartColumn)); ParseErrorBreak();;
        }
        metac_expression_t* parenExp = MetaCParser_ParseExpression(self, expr_flags_none, 0);



        result->E1 = parenExp->E1;
        result->Hash = (crc32c_nozero(0x68b26e, &(result->E1->Hash), sizeof(result->E1->Hash)));
    }
    else if (tokenType == tok_minus)
    {
        (MetaCParser_Match_((self), (tok_minus), "../metac_parser.c", 1464));
        result = AllocNewExpression(exp_umin);

        result->E1 = MetaCParser_ParseExpression(self,
   (parse_expression_flags_t)(expr_flags_unary | (eflags & expr_flags_pp)), 0);
        result->Hash = (crc32c_nozero(0x32176ea3, &(result->E1->Hash), sizeof(result->E1->Hash)));
    }
    else if (tokenType == tok_minusminus)
    {
        (MetaCParser_Match_((self), (tok_minusminus), "../metac_parser.c", 1473));
        result = AllocNewExpression(exp_decrement);

        result->E1 = MetaCParser_ParseExpression(self,
   (parse_expression_flags_t)(expr_flags_unary | (eflags & expr_flags_pp)), 0);
        result->Hash = (crc32c_nozero(0x2ebc9331, &(result->E1->Hash), sizeof(result->E1->Hash)));
    }
    else if (tokenType == tok_plusplus)
    {
        (MetaCParser_Match_((self), (tok_plusplus), "../metac_parser.c", 1482));
        result = AllocNewExpression(exp_increment);

        result->E1 = MetaCParser_ParseExpression(self,
   (parse_expression_flags_t)(expr_flags_unary | (eflags & expr_flags_pp)), 0);
        result->Hash = (crc32c_nozero(0x61d225eb, &(result->E1->Hash), sizeof(result->E1->Hash)));
    }
    else if (tokenType == tok_and)
    {
        (MetaCParser_Match_((self), (tok_and), "../metac_parser.c", 1491));
        result = AllocNewExpression(exp_addr);

        result->E1 = MetaCParser_ParseExpression(self, expr_flags_addr, result);
        result->Hash = (crc32c_nozero(0xab9ec598, &(result->E1->Hash), sizeof(result->E1->Hash)));


    }
    else if (tokenType == tok_star)
    {
        (MetaCParser_Match_((self), (tok_star), "../metac_parser.c", 1501));
        result = AllocNewExpression(exp_ptr);

        result->E1 = MetaCParser_ParseExpression(self, expr_flags_unary, 0);
        result->Hash = (crc32c_nozero(0xe6dd0a48, &(result->E1->Hash), sizeof(result->E1->Hash)));

    }
    else if (tokenType == tok_bang)
    {
        (MetaCParser_Match_((self), (tok_bang), "../metac_parser.c", 1510));
        result = AllocNewExpression(exp_not);
        result->E1 = MetaCParser_ParseExpression(self,
   (parse_expression_flags_t)(expr_flags_unary | (eflags & expr_flags_pp)), 0);
        result->Hash = (crc32c_nozero(0x7f54a173, &(result->E1->Hash), sizeof(result->E1->Hash)));
    }
    else if (tokenType == tok_cat)
    {
        (MetaCParser_Match_((self), (tok_cat), "../metac_parser.c", 1518));
        result = AllocNewExpression(exp_compl);
        result->E1 = MetaCParser_ParseExpression(self,
   (parse_expression_flags_t)(expr_flags_unary | (eflags & expr_flags_pp)), 0);
        result->Hash = (crc32c_nozero(crc32c_nozero(~0, "~", 1), &(result->E1->Hash), sizeof(result->E1->Hash)))


         ;
    }
    else if (IsPrimaryExpressionToken(tokenType))
    {
        isPrimaryExp =

                      1

                          ;
        result = MetaCParser_ParsePrimaryExpression(self, eflags);
    }
    else
    {
        if (tokenType != tok_eof)
        {
            metac_location_t location =
                self->Lexer->LocationStorage.Locations[currentToken->LocationId - 4];
            fprintf(

                   stderr

                         , "line: %d col: %d\n", location.StartLine, location.StartColumn);
        }
        fprintf(

               stderr

                     , "Unexpected Token: %s\n", MetaCTokenEnum_toChars(tokenType));


    }

    if (!isPrimaryExp)
    {
        metac_location_t endLoc =
            self->LocationStorage.Locations[result->E1->LocationIdx - 4];
        MetaCLocation_Expand(&loc, endLoc);
    }

    result->LocationIdx =
        MetaCLocationStorage_Store(&self->LocationStorage, loc);

    metac_token_t* peek_post = (MetaCParser_PeekToken_(self, 1, 1554));
    metac_token_enum_t postTokenType =
        (peek_post ? peek_post->TokenType : tok_invalid);

    if (IsPostfixOperator(postTokenType))
    {
        result = MetaCParser_ParsePostfixExpression(self, result);
    }

    return result;
}

exp_argument_t* MetaCParser_ParseArgumentList(metac_parser_t* self)
{
    metac_location_t loc =
        LocationFromToken(self, (MetaCParser_PeekToken_(self, 0, 1569)));

    metac_token_t* peekToken = (MetaCParser_PeekToken_(self, 1, 1571));
    exp_argument_t* arguments = (exp_argument_t*) _emptyPointer;
    exp_argument_t** nextArgument = &arguments;
    uint32_t nArguments = 0;
    uint32_t hash = ~0;

    while (!MetaCParser_PeekMatch(self, tok_rParen,

                                                   1

                                                       ))
    {
        nArguments++;


                                               ;

        (*nextArgument) = (exp_argument_t*)AllocNewExpression(exp_argument);
        metac_expression_t* exp = MetaCParser_ParseExpression(self, expr_flags_call, 0);
        ((*nextArgument)->Expression) = exp;


                        ;
        hash = (crc32c_nozero(hash, &(exp->Hash), sizeof(exp->Hash)));
        nextArgument = &((*nextArgument)->Next);
        (*nextArgument) = (exp_argument_t*) _emptyPointer;
        if(MetaCParser_PeekMatch(self, tok_comma,

                                                 1

                                                     ))
        {
            (MetaCParser_Match_((self), (tok_comma), "../metac_parser.c", 1591));
        }
    }

    if (arguments != ((void*)0x1))
    {
        arguments->Hash = hash;
        metac_token_t* rParen = (MetaCParser_PeekToken_(self, 1, 1598));
        MetaCLocation_Expand(&loc, LocationFromToken(self, rParen));
        arguments->LocationIdx =
            MetaCLocationStorage_Store(&self->LocationStorage, loc);
    }

    return arguments;
}
metac_expression_t* MetaCParser_ParseBinaryExpression(metac_parser_t* self,
                                                      parse_expression_flags_t eflags,
                                                      metac_expression_t* left,
                                                      uint32_t min_prec)
{
    metac_expression_t* result = 0;




    metac_token_t* peekToken;
    metac_token_enum_t peekTokenType;
    metac_location_t loc =
        self->LocationStorage.Locations[left->LocationIdx - 4];

    peekToken = (MetaCParser_PeekToken_(self, 1, 1621));
    peekTokenType = (peekToken ? peekToken->TokenType : tok_eof);

           if (IsBinaryOperator(peekTokenType, eflags))
    {


       _Bool

            rhsIsArgs =

                        0

                             ;
        metac_expression_kind_t exp_right;

        while(IsBinaryOperator(peekTokenType, eflags)
           && OpToPrecedence(BinExpTypeFromTokenType(peekTokenType)) >= min_prec)
        {
            exp_right = BinExpTypeFromTokenType(peekTokenType);
            uint32_t opPrecedence = OpToPrecedence(exp_right);
            metac_token_t* startTok = (MetaCParser_Match_((self), (peekTokenType), "../metac_parser.c", 1686));
            metac_location_t rhsLoc = LocationFromToken(self, startTok);
            metac_expression_t* rhs;

            if (exp_right == exp_index)
            {
                rhs = MetaCParser_ParseExpression(self, eflags, 0);
                metac_token_t* rBracket =
                    (MetaCParser_Match_((self), (tok_rBracket), "../metac_parser.c", 1694));
                MetaCLocation_Expand(&rhsLoc,
                    LocationFromToken(self, rBracket));
            }
            else if (exp_right == exp_call)
            {
                rhs = (metac_expression_t*)MetaCParser_ParseArgumentList(self);
                if ((metac_node_t)rhs != ((void*)0x1))
                    rhsIsArgs =

                               1

                                   ;

                metac_token_t* rParen =
                    (MetaCParser_Match_((self), (tok_rParen), "../metac_parser.c", 1705));
                MetaCLocation_Expand(&rhsLoc,
                    LocationFromToken(self, rParen));
            }
            else
            {
                rhs = MetaCParser_ParseUnaryExpression(self, eflags);
            }
            peekToken = (MetaCParser_PeekToken_(self, 1, 1713));
            peekTokenType = (peekToken ? peekToken->TokenType : tok_eof);

            while(IsBinaryOperator(peekTokenType, eflags)
               && opPrecedence <
                  OpToPrecedence(BinExpTypeFromTokenType(peekTokenType)))
            {
                rhs = MetaCParser_ParseBinaryExpression(self, eflags, rhs, opPrecedence + 0);
                peekToken = (MetaCParser_PeekToken_(self, 1, 1721));
                peekTokenType = (peekToken ? peekToken->TokenType : tok_eof);
            }

            result = AllocNewExpression(exp_right);
            result->E1 = left;
            result->E2 = rhs;
            if (rhs != ((void*)0x1))
            {
                result->Hash = (crc32c_nozero(left->Hash, &(rhs->Hash), sizeof(rhs->Hash)));
                MetaCLocation_Expand(&rhsLoc,
                    self->LocationStorage.Locations[rhs->LocationIdx - 4]);
            }
            else
            {
                uint32_t emptyHash = ~0;
                result->Hash = (crc32c_nozero(left->Hash, &(emptyHash), sizeof(emptyHash)));
            }

            MetaCLocation_Expand(&loc, rhsLoc);
            result->LocationIdx =
                MetaCLocationStorage_Store(&self->LocationStorage, loc);
            left = result;
        }
    }
    else
    {


                                  ;
    }
    if (!result->Hash)
    {
        if (result->E2 == ((void*)0x1))
        {
            result->Hash = result->E1->Hash;
        }
        else
        {
            result->Hash = ~0;
            result->Hash = (crc32c_nozero(result->E1->Hash, &(result->E2->Hash), sizeof(result->E2->Hash)));
        }

        result->LocationIdx = MetaCLocationStorage_Store(&self->LocationStorage, loc);
    }
    return result;
}



_Bool

    IsBinaryExp(metac_expression_kind_t kind)
{
    return ((kind >= exp_comma) && (kind <= exp_spaceship));
}



_Bool

    IsBinaryAssignExp(metac_expression_kind_t kind)
{
   return (kind >= exp_add_ass && kind <= exp_rsh_ass);
}



metac_preprocessor_directive_t MetaCParser_ParsePreproc(metac_parser_t* self,
                                                        metac_preprocessor_t* preproc,
                                                        metac_token_buffer_t* buffer)
{
    metac_token_t* result= buffer->Ptr;

    (MetaCParser_Match_((self), (tok_hash), "../metac_parser.c", 1785));
    metac_token_t* peek = (MetaCParser_PeekToken_(self, 1, 1786));
    metac_token_enum_t tokenType = (peek ? peek->TokenType : tok_eof);
    metac_preprocessor_directive_t directive = pp_invalid;

    switch (tokenType)
    {
        case tok_kw_if:
        {
            directive = pp_if;
        } break;
        case tok_kw_else:
        {
            directive = pp_else;
        } break;
        case tok_identifier: {
            switch(peek->IdentifierKey)
            {
                case 0x45758c:
                {
                    directive = pp_eval;
                } goto Lmatch;

                case 0x581ce0:
                {
                    directive = pp_ifdef;
                } goto Lmatch;

                case 0x4f8f4e:
                {
                    directive = pp_elif;
                } goto Lmatch;

                case 0x7e87f0:
                {
                    directive = pp_include;
                } goto Lmatch;

                case 0x6a491b:
                {
                    directive = pp_define;
                } goto Lmatch;

                case 0x506843:
                {
                    directive = pp_endif;
                } goto Lmatch;

                default:
                    printf("couldn't match directive\n");
                break;

                Lmatch:
                {
                    printf("CurrentTokenIndex before match: %u\n", self->CurrentTokenIndex);
                    (MetaCParser_Match_((self), (tok_identifier), "../metac_parser.c", 1840));


                    printf("CurrentTokenIndex after match: %u\n", self->CurrentTokenIndex);
                }
            }
        }
    }

    return directive;
}


metac_expression_t* MetaCParser_ParseExpression(metac_parser_t* self,
                                                parse_expression_flags_t eflags,
                                                metac_expression_t* prev)
{
    metac_expression_t* result = 0;
    metac_token_t* currentToken = (MetaCParser_PeekToken_(self, 1, 1858));
    metac_token_enum_t tokenType =
        (currentToken ? currentToken->TokenType : tok_invalid);
    metac_location_t loc = {0};
    if (currentToken)
        loc = LocationFromToken(self, currentToken);



   _Bool

        isDefined =

                    0

                         ;

    if (eflags & expr_flags_pp)
    {
        if (tokenType == tok_identifier &&
            currentToken->IdentifierKey == 0x7d9260)
        {
            isDefined =

                       1

                           ;
            (MetaCParser_Match_((self), (tok_identifier), "../metac_parser.c", 1873));
            currentToken = (MetaCParser_PeekToken_(self, 1, 1874));
            tokenType =
                (currentToken ? currentToken->TokenType : tok_invalid);
        }
    }


    if (IsPrimaryExpressionToken(tokenType))
    {
        result = MetaCParser_ParsePrimaryExpression(self, eflags);
    }
    else if (!prev )
    {
        result = MetaCParser_ParseUnaryExpression(self, eflags);
    }
    else
    {
        result = MetaCParser_ParseBinaryExpression(self, eflags, prev, 0);
    }


    if (isDefined)
    {
        if (result->Kind == exp_paren)
            result = result->E1;



                                             ;

        metac_expression_t* call = AllocNewExpression(exp_call);
        exp_argument_t* args = (exp_argument_t*) AllocNewExpression(exp_argument);
        metac_expression_t* definedIdExp = AllocNewExpression(exp_identifier);
        metac_location_t endLoc = self->LocationStorage.Locations[result->LocationIdx - 4];
        MetaCLocation_Expand(&loc, endLoc);

        definedIdExp->IdentifierKey = 0x7d9260;
        definedIdExp->IdentifierPtr = self->SpecialNamePtr_Defined;

        uint32_t hash = 0x7d9260;
        call->LocationIdx = MetaCLocationStorage_Store(&self->LocationStorage, loc);
        call->Hash = (crc32c_nozero(hash, &(result->Hash), sizeof(result->Hash)));
        args->Expression = result;
        args->Next = (exp_argument_t*) ((void*)0x1);

        call->E2 = (metac_expression_t*)args;
        call->E1 = definedIdExp;

        result = call;
    }



    metac_token_t* peekNext = (MetaCParser_PeekToken_(self, 1, 1925));
    if (peekNext)
    {
        uint32_t min_prec = 0;

        tokenType = peekNext->TokenType;

        if (((eflags & (expr_flags_call | expr_flags_enum)) != 0) && tokenType == tok_comma)
            goto LreturnExp;

        if ((eflags & expr_flags_unary))
            goto LreturnExp;

        if (IsBinaryOperator(tokenType, eflags))
        {

            uint32_t prec = OpToPrecedence(result->Kind);
            if (prec < min_prec)
                return result;
            result = MetaCParser_ParseBinaryExpression(self, eflags, result, min_prec);
            peekNext = (MetaCParser_PeekToken_(self, 1, 1945));
            tokenType = (peekNext ? peekNext->TokenType : tok_eof);
        }
        else if (IsPostfixOperator(tokenType))
        {
            result = MetaCParser_ParsePostfixExpression(self, result);
        }
        else if (peekNext->TokenType == tok_lParen)
        {
            result = MetaCParser_ParseBinaryExpression(self, eflags, result, OpToPrecedence(exp_call));
        }
        else if (tokenType == tok_lBracket)
        {
            result = MetaCParser_ParseBinaryExpression(self, eflags, result, OpToPrecedence(exp_index));
        }
        else if (tokenType == tok_rBracket || tokenType == tok_rParen)
        {

        }

        if (tokenType == tok_question)
        {
            uint32_t hash = 0xc372d93b;

            (MetaCParser_Match_((self), (tok_question), "../metac_parser.c", 1969));
            metac_expression_t* Econd = result;
            hash = (crc32c_nozero(hash, &(Econd->Hash), sizeof(Econd->Hash)));
            metac_expression_t* E1 =
                MetaCParser_ParseExpression(self, expr_flags_none, 0);
            hash = (crc32c_nozero(hash, &(E1->Hash), sizeof(E1->Hash)));
            (MetaCParser_Match_((self), (tok_colon), "../metac_parser.c", 1975));
            metac_expression_t* E2 =
                MetaCParser_ParseExpression(self, expr_flags_none, 0);
            hash = (crc32c_nozero(hash, &(E2->Hash), sizeof(E2->Hash)));
            result = AllocNewExpression(exp_ternary);
            result->E1 = E1;
            result->E2 = E2;
            result->Econd = Econd;
            result->Hash = hash;
        }


    }



                            ;
LreturnExp:
    return result;
}

static inline

             _Bool

                  IsDeclType(metac_declaration_t* decl)
{
    metac_declaration_kind_t kind = decl->DeclKind;
    return (kind == decl_type
         || kind == decl_type_struct
         || kind == decl_type_enum
         || kind == decl_type_union);
}

static decl_type_array_t* ParseArraySuffix(metac_parser_t* self, decl_type_t* type);

decl_type_t* MetaCParser_ParseTypeDeclaration(metac_parser_t* self, metac_declaration_t* parent, metac_declaration_t* prev)
{
    decl_type_t* result = 0;

    decl_type_t* type = (decl_type_t*) AllocNewDeclaration_(decl_type, sizeof(decl_type_t), ((void**)(&result)), 2017);
    metac_type_modifiers typeModifiers = typemod_none;
    metac_token_t* currentToken = 0;
    metac_location_t loc =
        LocationFromToken(self, (MetaCParser_PeekToken_(self, 1, 2021)));
    uint32_t hash = 0x40869f;

LnextToken:
    currentToken = MetaCParser_NextToken(self);
    metac_token_enum_t tokenType =
        (currentToken ? currentToken->TokenType : tok_invalid);


    while(IsTypeToken(tokenType))
    {
        if (tokenType == tok_identifier)
        {
            type->TypeKind = type_identifier;
            type->TypeIdentifier = RegisterIdentifier(self, currentToken);
            (*(uint32_t*)(&type->TypeModifiers)) |= typeModifiers;
            if (type->TypeIdentifier.v == self->SpecialNamePtr_Type.v)
            {
                type->TypeKind = type_type;
            }
            hash = (crc32c_nozero(hash, &(type->TypeIdentifier), sizeof(type->TypeIdentifier)));


                                                          ;
            break;
        }
        if (tokenType == tok_kw_const)
        {
            (*(uint32_t*)(&typeModifiers)) |= typemod_const;
            goto LnextToken;
        }
        else if (tokenType >= tok_kw_auto && tokenType <= tok_kw_double)
        {
            type->TypeKind = (metac_type_kind_t)(type_auto + (tokenType - tok_kw_auto));
            (*(uint32_t*)(&type->TypeModifiers)) |= typeModifiers;
            if (tokenType == tok_kw_long)
            {
                if (MetaCParser_PeekMatch(self, tok_kw_long, 1))
                {
                    (MetaCParser_Match_((self), (tok_kw_long), "../metac_parser.c", 2058));
                    type->TypeKind = type_long_long;
                }
                else if (MetaCParser_PeekMatch(self, tok_kw_double, 1))
                {
                    (MetaCParser_Match_((self), (tok_kw_double), "../metac_parser.c", 2063));
                    type->TypeKind = type_long_double;
                }
            }

            if (tokenType == tok_kw_short
             || tokenType == tok_kw_long)
            {
                if (MetaCParser_PeekMatch(self, tok_kw_int, 1))
                {
                    (MetaCParser_Match_((self), (tok_kw_int), "../metac_parser.c", 2073));
                }
            }


            hash ^= (type->TypeModifiers & typemod_unsigned);
            hash = (crc32c_nozero(hash, &(type->TypeKind), sizeof(type->TypeKind)));
            break;
        }
        else if (tokenType == tok_kw_unsigned)
        {
            (*(uint32_t*)(&typeModifiers)) |= typemod_unsigned;
            goto LnextToken;
        }
        else if (tokenType == tok_kw_signed)
        {

            goto LnextToken;
        }
        else if (tokenType == tok_star)
        {
            fprintf(

           stderr

           , "ParseError[%s:%u]: {%u:%u}" "* is unexpected to start a type declaration" "\n", "../metac_parser.c", 2093, (loc.StartLine), (loc.StartColumn)); ParseErrorBreak();;


                    ;
        }
        else if (tokenType == tok_kw_struct || tokenType == tok_kw_union)
        {


           _Bool

                isStruct = tokenType == tok_kw_struct;



           _Bool

                isPredeclated =

                                1

                                    ;

            decl_type_struct_t* struct_ = (decl_type_struct_t*) AllocNewDeclaration_(decl_type_struct, sizeof(decl_type_struct_t), ((void**)(&result)), 2102);
            type = (decl_type_t*)struct_;

            if (!isStruct)
            {
                struct_->TypeKind = type_union;
                struct_->DeclKind = decl_type_union;
            }

            if (MetaCParser_PeekMatch(self, tok_identifier, 1))
            {
                metac_token_t* structName = MetaCParser_NextToken(self);
                struct_->Identifier = RegisterIdentifier(self, structName);
            }
            else
            {
                struct_->Identifier = empty_identifier;
            }

            switch(struct_->TypeKind)
            {
                case type_struct:
                    hash = 0x6e9213;
                case type_union:
                    hash = 0x5a08cc;
            }

            hash = (crc32c_nozero(hash, &(struct_->Identifier), sizeof(struct_->Identifier)));
            if (tokenType == tok_kw_struct)
            {
                if (MetaCParser_PeekMatch(self, tok_colon, 1))
                {
                    (MetaCParser_Match_((self), (tok_colon), "../metac_parser.c", 2134));
                    metac_token_t* baseName = MetaCParser_NextToken(self);
                    struct_->BaseIdentifier = RegisterIdentifier(self, baseName);
                }
                goto LSetEmptyBase;
            }
            else
            {
        LSetEmptyBase:
                struct_->BaseIdentifier = empty_identifier;
            }
            hash = (crc32c_nozero(hash, &(struct_->BaseIdentifier), sizeof(struct_->BaseIdentifier)));

            if (MetaCParser_PeekMatch(self, tok_lBrace, 1))
            {
                (MetaCParser_Match_((self), (tok_lBrace), "../metac_parser.c", 2149));
                decl_field_t **nextMemberPtr = &struct_->Fields;

                isPredeclated =

                               0

                                    ;
                while(!MetaCParser_PeekMatch(self, tok_rBrace, 1))
                {
                    decl_field_t* field =
                        (decl_field_t*) AllocNewDeclaration_(decl_field, sizeof(decl_field_t), ((void**)((metac_declaration_t**) nextMemberPtr)),
 2157

                        )
                                                          ;
                    field->Next = (decl_field_t*)_emptyPointer;
                    metac_declaration_t *decl =
                        (metac_declaration_t*)MetaCParser_ParseDeclaration(self, (metac_declaration_t*)struct_);


                                          ;
                    hash = (crc32c_nozero(hash, &(decl->Hash), sizeof(decl->Hash)));

                    if (decl->DeclKind == decl_comment)
                        continue;

                    if (decl->DeclKind == decl_variable)
                    {
                        field->Field = (decl_variable_t*)decl;
                    }
                    else
                    {

                        (decl_variable_t*) AllocNewDeclaration_(decl_variable, sizeof(decl_variable_t), ((void**)(&field->Field)), 2177);
                        field->Field->VarType = (decl_type_t*)decl;
                        field->Field->VarIdentifier = empty_identifier;
                    }


                    nextMemberPtr = &field->Next;
                    struct_->FieldCount++;
                }
                (MetaCParser_Match_((self), (tok_rBrace), "../metac_parser.c", 2186));
            }
            else
            {
                printf("We just have a decl\n");
            }
            break;
        }
        else if (tokenType == tok_kw_enum)
        {
            decl_type_enum_t* enum_ = (decl_type_enum_t*) AllocNewDeclaration_(decl_type_enum, sizeof(decl_type_enum_t), ((void**)(&result)), 2196);

            if (MetaCParser_PeekMatch(self, tok_identifier, 1))
            {
                metac_token_t* structName = MetaCParser_NextToken(self);
                enum_->Identifier = RegisterIdentifier(self, structName);
            }
            else
            {
                enum_->Identifier = empty_identifier;
            }
            hash = (crc32c_nozero(0x42259d, &(enum_->Identifier), sizeof(enum_->Identifier)));

            if (MetaCParser_PeekMatch(self, tok_colon, 1))
            {
                (MetaCParser_Match_((self), (tok_colon), "../metac_parser.c", 2211));
                enum_->BaseType =
                    MetaCParser_ParseTypeDeclaration(self, 0, 0);
                hash = (crc32c_nozero(hash, &(enum_->BaseType->Hash), sizeof(enum_->BaseType->Hash)));
            }

            if (MetaCParser_PeekMatch(self, tok_lBrace, 1))
            {
                (MetaCParser_Match_((self), (tok_lBrace), "../metac_parser.c", 2219));
                decl_enum_member_t **nextMemberPtr = &enum_->Members;
                uint32_t memberCount = 0;

                while(!MetaCParser_PeekMatch(self, tok_rBrace, 1))
                {
                    memberCount++;
                    decl_enum_member_t* member =
                        (decl_enum_member_t*) AllocNewDeclaration_(decl_enum_member, sizeof(decl_enum_member_t), ((void**)((metac_declaration_t**) nextMemberPtr)),
 2228

                        )
                                                          ;
                    member->Next = (decl_enum_member_t*) _emptyPointer;
                    metac_token_t* idToken = (MetaCParser_Match_((self), (tok_identifier), "../metac_parser.c", 2230));
                    member->Name = RegisterIdentifier(self, idToken);
                    hash = (crc32c_nozero(hash, &(member->Name), sizeof(member->Name)));
                    metac_token_t* afterName = (MetaCParser_PeekToken_(self, 1, 2233));
                    if (afterName
                         && ((afterName->TokenType == tok_comma)
                          | (afterName->TokenType == tok_rBrace)))
                    {
                        member->Value = (metac_expression_t*)((void*)0x1);
                        if (afterName->TokenType == tok_rBrace)
                            break;
                    }
                    else
                    {
                        (MetaCParser_Match_((self), (tok_assign), "../metac_parser.c", 2244));
                        member->Value = MetaCParser_ParseExpression(self, expr_flags_enum, 0);


                        hash = (crc32c_nozero(hash, &(member->Value->Hash), sizeof(member->Value->Hash)));
                    }
                    metac_token_t* afterMember = (MetaCParser_PeekToken_(self, 1, 2249));
                    if (afterMember->TokenType != tok_rBrace)
                        (MetaCParser_Match_((self), (tok_comma), "../metac_parser.c", 2251));
                    nextMemberPtr = &member->Next;
                }
                (MetaCParser_Match_((self), (tok_rBrace), "../metac_parser.c", 2254));
                enum_->MemberCount = memberCount;
            }
            break;
        }
    }



    result->Hash = hash;

    currentToken = (MetaCParser_PeekToken_(self, 1, 2264));
    tokenType =
        (currentToken ? currentToken->TokenType : tok_invalid);



   _Bool

        nextIsConst =

                      0

                           ;
    while(tokenType == tok_star
       || tokenType == tok_kw_const)
    {
        if (tokenType == tok_kw_const)
        {
            (MetaCParser_Match_((self), (tok_kw_const), "../metac_parser.c", 2274));
            nextIsConst =

                         1

                             ;
        }
        (MetaCParser_Match_((self), (tok_star), "../metac_parser.c", 2277));
        decl_type_t* elementType = result;
        decl_type_ptr_t* ptr = (decl_type_ptr_t*) AllocNewDeclaration_(decl_type_ptr, sizeof(decl_type_ptr_t), ((void**)(&result)), 2279);
        if (nextIsConst)
            (*(uint32_t*)(&ptr->TypeModifiers)) |= typemod_const;
        ptr->ElementType = elementType;
        ptr->Hash = hash = (crc32c_nozero((crc32c_nozero(~(uint32_t)0, "*", sizeof("*") - 1)), &(hash), sizeof(hash)));
        currentToken = (MetaCParser_PeekToken_(self, 1, 2284));
        tokenType =
            (currentToken ? currentToken->TokenType : tok_invalid);
        nextIsConst =

                     0

                          ;
    }

    metac_location_t endLoc =
        LocationFromToken(self, currentToken ? currentToken : (MetaCParser_PeekToken_(self, 0, 2291)));
    MetaCLocation_Expand(&loc, endLoc);
    result->LocationIdx = MetaCLocationStorage_Store(&self->LocationStorage, loc);
    return result;
}



_Bool

    IsTypeDecl(metac_declaration_kind_t kind)
{
    return ((kind >= decl_type)
          & (kind <= decl_type_typedef));
}

decl_parameter_list_t ParseParameterList(metac_parser_t* self,
                                         decl_function_t* parent)
{
    decl_parameter_list_t result = {(decl_parameter_t*)((void*)0x1)};
    uint32_t parameterCount = 0;
    decl_parameter_t** nextParam = &result.List;

    while (!MetaCParser_PeekMatch(self, tok_rParen,

                                                   1

                                                       ))
    {



        if (result.IsVariadic)
        {
            fprintf(

           stderr

           , "ParseError[%s:%u]: {%u:%u}" "you cannot have ... at any position other than the end of the parameter list\n" "\n",
                                                                                                        "../metac_parser.c"

            ,
                                                                                                        2317

            , (self->LastLocation.StartLine), (self->LastLocation.StartColumn)); ParseErrorBreak();
                                                                                                         ;
        }

        if (MetaCParser_PeekMatch(self, tok_dotdotdot, 1))
        {
            (MetaCParser_Match_((self), (tok_dotdotdot), "../metac_parser.c", 2322));
            result.IsVariadic =

                               1

                                   ;
            continue;
        }

        decl_parameter_t* param;
        (decl_parameter_t*) AllocNewDeclaration_(decl_parameter, sizeof(decl_parameter_t), ((void**)(&param)), 2328);
        parameterCount++;
        (*nextParam) = param;

        metac_declaration_t* paramDecl =
   MetaCParser_ParseDeclaration(self, (metac_declaration_t*)parent);
        if (paramDecl->DeclKind == decl_variable)
        {
            param->Parameter = (decl_variable_t*)
                paramDecl;
        }
        else if (IsTypeDecl(paramDecl->DeclKind))
        {

            decl_variable_t* var;
            (decl_variable_t*) AllocNewDeclaration_(decl_variable, sizeof(decl_variable_t), ((void**)(&var)), 2343);
            var->VarType = (decl_type_t*)paramDecl;
            var->VarIdentifier = empty_identifier;
            var->VarInitExpression = (metac_expression_t*) ((void*)0x1);
        }
        else
        {
            metac_token_t* peek = (MetaCParser_PeekToken_(self, 1, 2350));
            metac_location_t loc = LocationFromToken(self, peek);
            fprintf(

           stderr

           , "ParseError[%s:%u]: {%u:%u}" "Invalid parameter" "\n", "../metac_parser.c", 2352, (loc.StartLine), (loc.StartColumn)); ParseErrorBreak();;
        }
        nextParam = &param->Next;
        (*nextParam) = (decl_parameter_t*) _emptyPointer;

        if (MetaCParser_PeekMatch(self, tok_comma,

                                                  1

                                                      ))
        {
            (MetaCParser_Match_((self), (tok_comma), "../metac_parser.c", 2359));
        }
        else
        {


                                                                ;
        }
    }
    (MetaCParser_Match_((self), (tok_rParen), "../metac_parser.c", 2366));
    result.ParameterCount = parameterCount;

    return result;
}

static stmt_block_t* MetaCParser_ParseBlockStatement(metac_parser_t* self,
                                                     metac_statement_t* parent,
                                                     metac_statement_t* prev);
void EatAttributes(metac_parser_t* self)
{
    while(MetaCParser_PeekMatch(self, tok_kw___attribute__, 1))
    {
        (MetaCParser_Match_((self), (tok_kw___attribute__), "../metac_parser.c", 2379));
        (MetaCParser_Match_((self), (tok_lParen), "../metac_parser.c", 2380));
        (MetaCParser_Match_((self), (tok_lParen), "../metac_parser.c", 2381));
        metac_token_t *currentToken =
            (MetaCParser_PeekToken_(self, 1, 2383));

        while(currentToken && currentToken->TokenType != tok_rParen)
        {
            (MetaCParser_Match_((self), (currentToken->TokenType), "../metac_parser.c", 2387));
        }

        if (currentToken)
        {
            (MetaCParser_Match_((self), (tok_rParen), "../metac_parser.c", 2392));
            (MetaCParser_Match_((self), (tok_rParen), "../metac_parser.c", 2393));
        }
    }
}

decl_function_t* ParseFunctionDeclaration(metac_parser_t* self, decl_type_t* type)
{
    decl_function_t result;

    metac_token_t* id = (MetaCParser_Match_((self), (tok_identifier), "../metac_parser.c", 2402));
    metac_location_t loc = LocationFromToken(self, id);
    metac_identifier_ptr_t identifier = RegisterIdentifier(self, id);

    (MetaCParser_Match_((self), (tok_lParen), "../metac_parser.c", 2406));
    decl_function_t* funcDecl = (decl_function_t*) AllocNewDeclaration_(decl_function, sizeof(decl_function_t), ((void**)(&result)), 2407);
    funcDecl->ReturnType = type;
    funcDecl->Identifier = identifier;

    funcDecl->FunctionBody = (stmt_block_t*) _emptyPointer;
    decl_parameter_list_t parameterList = ParseParameterList(self, funcDecl);
    funcDecl->Parameters = parameterList.List;
    funcDecl->ParameterCount = parameterList.ParameterCount;


    EatAttributes(self);

    if (MetaCParser_PeekMatch(self, tok_lBrace,

                                               1

                                                   ))
    {
        funcDecl->FunctionBody = MetaCParser_ParseBlockStatement(self, 0, 0);
    }

    return funcDecl;
}



uint32_t HashDecl(metac_declaration_t* decl)
{
    uint32_t result = 0;

    switch(decl->DeclKind)
    {
        case decl_label:
        {
            decl_label_t* label = (decl_label_t*) decl;
            result = (crc32c_nozero(0xf683cd27, &(label->Identifier), sizeof(label->Identifier)));
        } break;
        case decl_comment:
        {
            decl_comment_t* comment = (decl_comment_t*) decl;
            result = crc32c(0xe8c2d328, comment->Text, comment->Length);
        } break;
        case decl_type_typedef:
        {
            decl_type_typedef_t* type_typedef = (decl_type_typedef_t*) decl;
            result = 0x7f3981;
            result = (crc32c_nozero(result, &(type_typedef->Type->Hash), sizeof(type_typedef->Type->Hash)));
            result = (crc32c_nozero(result, &(type_typedef->Identifier), sizeof(type_typedef->Identifier)));
        } break;

        case decl_type:
        {
            decl_type_t* type = (decl_type_t*) decl;
            if (type->TypeKind == type_identifier)
            {
                result = (crc32c_nozero(0x40869f, &(type->TypeIdentifier), sizeof(type->TypeIdentifier)));
            }
            else
            {
                result = (crc32c_nozero((0x40869f ^ type->TypeModifiers & typemod_unsigned), &(type->TypeKind), sizeof(type->TypeKind)))


                 ;
            }
        } break;

        case decl_type_union:
            result = 0x5a08cc;
            goto LhashAgg;
        case decl_type_struct:
            result = 0x6e9213;
            goto LhashAgg;
        LhashAgg:
        {
            decl_type_struct_t* agg = (decl_type_struct_t*) decl;
            result = (crc32c_nozero(result, &(agg->Identifier), sizeof(agg->Identifier)));
            result = (crc32c_nozero(result, &(agg->BaseIdentifier), sizeof(agg->BaseIdentifier)));
            decl_field_t* field = agg->Fields;
            for(uint32_t i = 0; i < agg->FieldCount; i++)
            {
                uint32_t fieldHash = HashDecl((metac_declaration_t*)field->Field);
                result = (crc32c_nozero(result, &(fieldHash), sizeof(fieldHash)));
                field = field->Next;
            }


                                                                ;
        } break;
        case decl_variable:
        {
            decl_variable_t* variable = (decl_variable_t*) decl;
            result = HashDecl((metac_declaration_t*)variable->VarType);
            result = (crc32c_nozero(result, &(variable->VarIdentifier), sizeof(variable->VarIdentifier)));
        } break;
        case decl_type_ptr:
        {
            decl_type_ptr_t* type_ptr = (decl_type_ptr_t*) decl;
            uint32_t ElementTypeHash = HashDecl((metac_declaration_t*)type_ptr->ElementType);
            result = (crc32c_nozero(0xe6dd0a48, &(ElementTypeHash), sizeof(ElementTypeHash)));
        } break;
        case decl_type_array:
        {
            decl_type_array_t* type_array = (decl_type_array_t*) decl;
            uint32_t ElementTypeHash = HashDecl((metac_declaration_t*)type_array->ElementType);
            if (type_array->Dim->Kind == exp_signed_integer)
            {
                uint32_t dimToHash = (uint32_t)type_array->Dim->ValueU64;
                result = (crc32c_nozero(0x89b24289, &(dimToHash), sizeof(dimToHash)));
            }
            else
            {
                result = 0x89b24289;
            }

            result = (crc32c_nozero(result, &(ElementTypeHash), sizeof(ElementTypeHash)));
        }
        break;
        default:


                         ;
    }
    if (decl->Hash)




    return result;
}

metac_declaration_t* MetaCParser_ParseDeclaration(metac_parser_t* self, metac_declaration_t* parent)
{
    metac_token_t* currentToken = (MetaCParser_PeekToken_(self, 1, 2529));
    metac_token_enum_t tokenType =
        (currentToken ? currentToken->TokenType : tok_invalid);
    metac_location_t loc =
        currentToken ? LocationFromToken(self, currentToken) : invalidLocation;
    metac_declaration_t* result = 0;


   _Bool

        isStatic =

                   0

                        ;


   _Bool

        isInline =

                   0

                        ;


   _Bool

        isExtern =

                   0

                        ;

    decl_type_t* type = 0;

    if (tokenType == tok_eof)
        return 0;


    if (MetaCParser_PeekMatch(self, tok_identifier,

                                                   1

                                                       ))
    {
        metac_token_t* peek2 = (MetaCParser_PeekToken_(self, 2, 2547));
        if (peek2->TokenType == tok_colon)
        {
            metac_token_t* idToken = (MetaCParser_Match_((self), (tok_identifier), "../metac_parser.c", 2550));
            metac_token_t* colon = (MetaCParser_Match_((self), (tok_colon), "../metac_parser.c", 2551));
            decl_label_t* label = (decl_label_t*) AllocNewDeclaration_(decl_label, sizeof(decl_label_t), ((void**)(&result)), 2552);
            label->LocationIdx = MetaCLocationStorage_Store(&self->LocationStorage,
                MetaCLocationStorage_FromPair(&self->Lexer->LocationStorage,
                                              idToken->LocationId, colon->LocationId));

            label->Identifier = RegisterIdentifier(self, idToken);
            result->Hash = (crc32c_nozero(0xf683cd27, &(label->Identifier), sizeof(label->Identifier)));
            self->CurrentLabel = label;
            return result;
        }
    }

    else if (tokenType == tok_comment_multi
          || tokenType == tok_comment_single)
    {
        metac_token_t* tok = (MetaCParser_Match_((self), (tokenType), "../metac_parser.c", 2567));
        decl_comment_t* comment = (decl_comment_t*) AllocNewDeclaration_(decl_comment, sizeof(decl_comment_t), ((void**)(&result)), 2568);
        comment->Text = tok->CommentBegin;
        comment->Length = tok->CommentLength;
        self->CurrentComment = *tok;
        result->Hash = crc32c(0xe8c2d328, comment->Text, comment->Length);
        return result;
    }

    if (MetaCParser_PeekMatch(self, tok_kw_static,

                                                  1

                                                      ))
    {
        isStatic =

                  1

                      ;
        (MetaCParser_Match_((self), (tok_kw_static), "../metac_parser.c", 2579));
        currentToken = (MetaCParser_PeekToken_(self, 1, 2580));
        tokenType = currentToken ? currentToken->TokenType : tok_eof;
    }

    if (MetaCParser_PeekMatch(self, tok_kw_inline,

                                                  1

                                                      ))
    {
        isInline =

                  1

                      ;
        (MetaCParser_Match_((self), (tok_kw_inline), "../metac_parser.c", 2587));
        currentToken = (MetaCParser_PeekToken_(self, 1, 2588));
        tokenType = currentToken ? currentToken->TokenType : tok_eof;
    }

    if (MetaCParser_PeekMatch(self, tok_kw_extern,

                                                  1

                                                      ))
    {
        isExtern =

                  1

                      ;
        (MetaCParser_Match_((self), (tok_kw_extern), "../metac_parser.c", 2595));
        currentToken = (MetaCParser_PeekToken_(self, 1, 2596));
        tokenType = currentToken ? currentToken->TokenType : tok_eof;
    }

    if (IsTypeToken(tokenType))
    {
         type = MetaCParser_ParseTypeDeclaration(self, parent, 0);


                               ;

         result = (metac_declaration_t*)type;
    }

    if (tokenType == tok_kw_typedef)
    {
        (MetaCParser_Match_((self), (tok_kw_typedef), "../metac_parser.c", 2610));
        currentToken = (MetaCParser_PeekToken_(self, 1, 2611));
            tokenType =
        (currentToken ? currentToken->TokenType : tok_invalid);
        uint32_t hash = 0x7f3981;

        decl_type_typedef_t* typdef = (decl_type_typedef_t*) AllocNewDeclaration_(decl_type_typedef, sizeof(decl_type_typedef_t), ((void**)(&result)), 2616);


        decl_variable_t* var = (decl_variable_t*)MetaCParser_ParseDeclaration(self, (metac_declaration_t*) typdef);
        typdef->Type = var->VarType;
        typdef->Identifier = var->VarIdentifier;



                                      ;
        hash = (crc32c_nozero(hash, &(typdef->Type->Hash), sizeof(typdef->Type->Hash)));
        hash = (crc32c_nozero(hash, &(typdef->Identifier), sizeof(typdef->Identifier)));

        result->Hash = hash;



                                               ;
        goto LendDecl;
    }

    if (type)
    {
        if (MetaCParser_PeekMatch(self, tok_lParen,

                                                   1

                                                       ))
        {

            (MetaCParser_Match_((self), (tok_lParen), "../metac_parser.c", 2636));
            decl_variable_t* fPtrVar;
            self->OpenParens++;
            if (MetaCParser_PeekMatch(self, tok_star,

                                                     1

                                                         ))
            {
                (MetaCParser_Match_((self), (tok_star), "../metac_parser.c", 2641));

                metac_token_t* fPtrid = (MetaCParser_PeekToken_(self, 1, 2643));
                if (fPtrid->TokenType == tok_identifier)
                {
                    (MetaCParser_Match_((self), (tok_identifier), "../metac_parser.c", 2646));

                    fPtrVar = (decl_variable_t*) AllocNewDeclaration_(decl_variable, sizeof(decl_variable_t), ((void**)(&result)), 2648);
                    fPtrVar->VarInitExpression = (metac_expression_t*)((void*)0x1);

                    fPtrVar->VarIdentifier = RegisterIdentifier(self, fPtrid);
                    (MetaCParser_Match_((self), (tok_rParen), "../metac_parser.c", 2652));
                    self->OpenParens--;


                    (MetaCParser_Match_((self), (tok_lParen), "../metac_parser.c", 2656));
                    decl_type_t* returnType = type;
                    decl_parameter_list_t paramterList =
                        ParseParameterList(self, 0);

                    decl_type_functiontype_t* functionType =
                        (decl_type_functiontype_t*) AllocNewDeclaration_(decl_type_functiontype, sizeof(decl_type_functiontype_t), ((void**)(&fPtrVar->VarType)), 2662);

                    functionType->ReturnType = returnType;
                    functionType->Parameters = paramterList.List;
                    functionType->ParameterCount = paramterList.ParameterCount;

                    fPtrVar->VarType = (decl_type_t*)functionType;
                }
            }
        }
        else if (MetaCParser_PeekMatch(self, tok_identifier, 1))
        {
            metac_token_t* afterId = (MetaCParser_PeekToken_(self, 2, 2674));
            if (afterId && afterId->TokenType == tok_lParen)
            {
                decl_function_t* funcDecl = ParseFunctionDeclaration(self, type);
                result = (metac_declaration_t*) funcDecl;
            }
            else
            {
                decl_variable_t* varDecl = (decl_variable_t*) AllocNewDeclaration_(decl_variable, sizeof(decl_variable_t), ((void**)(&result)), 2682);
                metac_token_t* idToken = (MetaCParser_Match_((self), (tok_identifier), "../metac_parser.c", 2683));
                metac_identifier_ptr_t identifier = RegisterIdentifier(self, idToken);




                varDecl->VarType = type;
                varDecl->VarIdentifier = identifier;
                varDecl->VarInitExpression = (metac_expression_t*)_emptyPointer;

                varDecl->Hash = (crc32c_nozero(varDecl->VarType->Hash, &(varDecl->VarIdentifier), sizeof(varDecl->VarIdentifier)));

                while (MetaCParser_PeekMatch(self, tok_lBracket,

                                                                1

                                                                    ))
                {
                    varDecl->VarType = (decl_type_t*)
                        ParseArraySuffix(self, varDecl->VarType);
                }
                while (MetaCParser_PeekMatch(self, tok_full_slice,

                                                                  1

                                                                      ))
                {
                    (MetaCParser_Match_((self), (tok_full_slice), "../metac_parser.c", 2702));
                    printf("saw a [] ...  we should do something about that\n");

                }


                if (MetaCParser_PeekMatch(self, tok_colon,

                                                          1

                                                              ))
                {

                    (MetaCParser_Match_((self), (tok_colon), "../metac_parser.c", 2711));
                    metac_token_t* bitSz = (MetaCParser_Match_((self), (tok_uint), "../metac_parser.c", 2712));
                    printf("ignoring bitfield spec : %d\n", bitSz->ValueI64);
                }

                if (MetaCParser_PeekMatch(self, tok_assign,

                                                           1

                                                               ))
                {
                    (MetaCParser_Match_((self), (tok_assign), "../metac_parser.c", 2718));
                    varDecl->VarInitExpression = MetaCParser_ParseExpression(self, expr_flags_none, 0);
                }
            }
        }
    }
    else
    {
        fprintf(

       stderr

       , "ParseError[%s:%u]: {%u:%u}" "A declaration is expected to start with a type CurrentToken %s\n" "\n", "../metac_parser.c", 2726, (loc.StartLine), (loc.StartColumn), MetaCTokenEnum_toChars(tokenType)); ParseErrorBreak();;
    }
LendDecl:

    if (MetaCParser_PeekMatch(self, tok_semicolon,

                                                  1

                                                      ))
        (MetaCParser_Match_((self), (tok_semicolon), "../metac_parser.c", 2731));

    return result;
}

static decl_type_array_t* ParseArraySuffix(metac_parser_t* self, decl_type_t* type)
{
    decl_type_array_t* arrayType = 0;
    uint32_t hash = 0;

    if (MetaCParser_PeekMatch(self, tok_lBracket,

                                                 1

                                                     ))
    {
        (MetaCParser_Match_((self), (tok_lBracket), "../metac_parser.c", 2743));
        arrayType =
            (decl_type_array_t*) AllocNewDeclaration_(decl_type_array, sizeof(decl_type_array_t), ((void**)(&arrayType)), 2745);

        arrayType->ElementType = type;

        arrayType->Dim = MetaCParser_ParseExpression(self, expr_flags_none, 0);
        uint32_t dimToHash = 0;
        if (arrayType->Dim->Kind == exp_signed_integer)
        {
            uint32_t dimToHash = (uint32_t)arrayType->Dim->ValueU64;
            hash = (crc32c_nozero(0x89b24289, &(dimToHash), sizeof(dimToHash)));
        }
        else
        {
            hash = 0x89b24289;
        }
        hash = (crc32c_nozero(hash, &(type->Hash), sizeof(type->Hash)));

        (MetaCParser_Match_((self), (tok_rBracket), "../metac_parser.c", 2762));
        type = (decl_type_t*)arrayType;
    }
    arrayType->Hash = hash;


                    ;
    return arrayType;
}




static inline void PrintStatement(metac_printer_t* self, metac_statement_t* stmt);

metac_statement_t* MetaCParser_ParseStatement(metac_parser_t* self,
                                              metac_statement_t* parent,
                                              metac_statement_t* prev)
{
    metac_statement_t* result = 0;

    metac_token_t* currentToken = (MetaCParser_PeekToken_(self, 1, 2781));
    metac_token_enum_t tokenType =
        (currentToken ? currentToken->TokenType : tok_invalid);
    metac_location_t loc = LocationFromToken(self, currentToken);
    metac_token_t* peek2;
    uint32_t hash = 0;

    if (tokenType == tok_invalid)
    {
        return (metac_statement_t*)0;
    }

    if (self->CurrentBlockStatement)
        self->CurrentBlockStatement->StatementCount++;

    if (tokenType == tok_comment_multi
     || tokenType == tok_comment_single)
    {
        self->CurrentComment = *(MetaCParser_Match_((self), (tokenType), "../metac_parser.c", 2799));
        stmt_comment_t* comment = (stmt_comment_t*) AllocNewStatement_(stmt_comment, sizeof(stmt_comment_t), ((void**)(&result)));
        comment->Text = self->CurrentComment.CommentBegin;
        comment->Length = self->CurrentComment.CommentLength;
        comment->Hash = ~0;
    }
    else if (tokenType == tok_kw_if)
    {
        stmt_if_t* if_stmt = (stmt_if_t*) AllocNewStatement_(stmt_if, sizeof(stmt_if_t), ((void**)(&result)));
        (MetaCParser_Match_((self), (tok_kw_if), "../metac_parser.c", 2808));
        hash = 0x23826e;
        if (!MetaCParser_PeekMatch(self, tok_lParen, 0))
        {
            fprintf(

           stderr

           , "ParseError[%s:%u]: {%u:%u}" "execpected ( after if\n" "\n", "../metac_parser.c", 2812, (loc.StartLine), (loc.StartColumn)); ParseErrorBreak();;
            return (metac_statement_t*)0;
        }
        (MetaCParser_Match_((self), (tok_lParen), "../metac_parser.c", 2815));
        if_stmt->IfCond =
            MetaCParser_ParseExpression(self, expr_flags_none, 0);
        hash = (crc32c_nozero(hash, &(if_stmt->IfCond), sizeof(if_stmt->IfCond)));
        (MetaCParser_Match_((self), (tok_rParen), "../metac_parser.c", 2819));
        if_stmt->IfBody = MetaCParser_ParseStatement(self, (metac_statement_t*)result, 0);
        hash = (crc32c_nozero(hash, &(if_stmt->IfBody->Hash), sizeof(if_stmt->IfBody->Hash)));

        if (MetaCParser_PeekMatch(self, tok_kw_else, 1))
        {
            (MetaCParser_Match_((self), (tok_kw_else), "../metac_parser.c", 2825));
            if_stmt->ElseBody = (metac_statement_t*)MetaCParser_ParseStatement(self, (metac_statement_t*)result, 0);
            hash = (crc32c_nozero(hash, &(if_stmt->ElseBody->Hash), sizeof(if_stmt->ElseBody->Hash)));
        }
        else
        {
            if_stmt->ElseBody = (metac_statement_t*)_emptyPointer;
        }
        result->Hash = hash;
        goto LdoneWithStatement;
    }
    else if (tokenType == tok_kw_while)
    {
        stmt_while_t * while_stmt = (stmt_while_t*) AllocNewStatement_(stmt_while, sizeof(stmt_while_t), ((void**)(&result)));
        (MetaCParser_Match_((self), (tok_kw_while), "../metac_parser.c", 2839));
        (MetaCParser_Match_((self), (tok_lParen), "../metac_parser.c", 2840));
        while_stmt->WhileExp =
            MetaCParser_ParseExpression(self, expr_flags_none, 0);
        hash = (crc32c_nozero(hash, &(while_stmt->WhileExp->Hash), sizeof(while_stmt->WhileExp->Hash)));
        (MetaCParser_Match_((self), (tok_rParen), "../metac_parser.c", 2844));
        while_stmt->WhileBody =
            MetaCParser_ParseStatement(self, (metac_statement_t*)while_stmt, 0);
        hash = (crc32c_nozero(hash, &(while_stmt->WhileBody->Hash), sizeof(while_stmt->WhileBody->Hash)));
        while_stmt->Hash = hash;
    }
    else if (tokenType == tok_kw_for)
    {
        (MetaCParser_Match_((self), (tok_kw_for), "../metac_parser.c", 2852));
        hash = 0x3dda5e;
        stmt_for_t* for_ = (stmt_for_t*) AllocNewStatement_(stmt_for, sizeof(stmt_for_t), ((void**)(&result)));
        (MetaCParser_Match_((self), (tok_lParen), "../metac_parser.c", 2855));


        if (!MetaCParser_PeekMatch(self, tok_semicolon, 1))
        {
            metac_token_t* peek = (MetaCParser_PeekToken_(self, 1, 2860));
            metac_token_t* peek2 = (MetaCParser_PeekToken_(self, 2, 2861));

            if (IsDeclarationFirstToken(peek->TokenType)
             && IsDeclarationFirstToken(peek2->TokenType))
            {
                for_->ForInit = (metac_node_t)MetaCParser_ParseDeclaration(self, 0);
            }
            else
            {
                for_->ForInit = (metac_node_t)MetaCParser_ParseExpression(self, expr_flags_none, 0);
                (MetaCParser_Match_((self), (tok_semicolon), "../metac_parser.c", 2871));
            }

            hash = (crc32c_nozero(hash, &(for_->ForInit->Hash), sizeof(for_->ForInit->Hash)));
        }
        else
        {
            for_->ForInit = (metac_node_t)((void*)0x1);
            (MetaCParser_Match_((self), (tok_semicolon), "../metac_parser.c", 2879));
        }
        if (!MetaCParser_PeekMatch(self, tok_semicolon, 1))
        {
            for_->ForCond = MetaCParser_ParseExpression(self, expr_flags_none, 0);
            hash = (crc32c_nozero(hash, &(for_->ForCond->Hash), sizeof(for_->ForCond->Hash)));
        }
        else
        {
            for_->ForCond = (metac_expression_t*)((void*)0x1);
        }
        (MetaCParser_Match_((self), (tok_semicolon), "../metac_parser.c", 2890));

        if (!MetaCParser_PeekMatch(self, tok_rParen, 1))
        {
            for_->ForPostLoop = MetaCParser_ParseExpression(self, expr_flags_none, 0);
            hash = (crc32c_nozero(hash, &(for_->ForPostLoop->Hash), sizeof(for_->ForPostLoop->Hash)));
        }
        else
        {
            for_->ForPostLoop = (metac_expression_t*)((void*)0x1);
        }
        (MetaCParser_Match_((self), (tok_rParen), "../metac_parser.c", 2901));
        for_->ForBody = MetaCParser_ParseStatement(self, (metac_statement_t*)for_, 0);
        result->Hash = hash;
    }
    else if (tokenType == tok_kw_switch)
    {
        hash = 0x63e3c4;
        stmt_switch_t* switch_ = (stmt_switch_t*) AllocNewStatement_(stmt_switch, sizeof(stmt_switch_t), ((void**)(&result)));

        (MetaCParser_Match_((self), (tok_kw_switch), "../metac_parser.c", 2910));
        (MetaCParser_Match_((self), (tok_lParen), "../metac_parser.c", 2911));
        switch_->SwitchExp =
            MetaCParser_ParseExpression(self, expr_flags_none, 0);
        hash = (crc32c_nozero(hash, &(switch_->SwitchExp->Hash), sizeof(switch_->SwitchExp->Hash)));
        (MetaCParser_Match_((self), (tok_rParen), "../metac_parser.c", 2915));
        if (!MetaCParser_PeekMatch(self, tok_lBrace, 0))
        {
            fprintf(

           stderr

           , "ParseError[%s:%u]: {%u:%u}" "parsing switch failed\n" "\n", "../metac_parser.c", 2918, (loc.StartLine), (loc.StartColumn)); ParseErrorBreak();;
            return (metac_statement_t*)0;
        }

        switch_->SwitchBody =
            (metac_statement_t*)MetaCParser_ParseBlockStatement(self, result, 0);
        hash = (crc32c_nozero(hash, &(switch_->SwitchBody->Hash), sizeof(switch_->SwitchBody->Hash)));
        switch_->Hash = hash;
    }
    else if (tokenType == tok_identifier
        && (peek2 = (MetaCParser_PeekToken_(self, 2, 2928)))
        && (peek2->TokenType == tok_colon))
    {
        hash = 0xf683cd27;
        metac_token_t* label_tok = (MetaCParser_Match_((self), (tok_identifier), "../metac_parser.c", 2932));
        (MetaCParser_Match_((self), (tok_colon), "../metac_parser.c", 2933));

        stmt_label_t* label = (stmt_label_t*) AllocNewStatement_(stmt_label, sizeof(stmt_label_t), ((void**)(&result)));

        label->Label = RegisterIdentifier(self, label_tok);
        hash = (crc32c_nozero(hash, &(label->Label), sizeof(label->Label)));
        label->Hash = hash;
    }
    else if (tokenType == tok_kw_goto)
    {
        stmt_goto_t* goto_ = (stmt_goto_t*) AllocNewStatement_(stmt_goto, sizeof(stmt_goto_t), ((void**)(&result)));
        hash = 0x4d7ce2;

        (MetaCParser_Match_((self), (tok_kw_goto), "../metac_parser.c", 2946));
        metac_token_t* label = (MetaCParser_Match_((self), (tok_identifier), "../metac_parser.c", 2947));
        goto_->GotoLabel = RegisterIdentifier(self, label);
        goto_->Hash = (crc32c_nozero(hash, &(label->IdentifierPtr), sizeof(label->IdentifierPtr)));
    }
    else if (tokenType == tok_kw_break)
    {
        stmt_break_t* break_ = (stmt_break_t*) AllocNewStatement_(stmt_break, sizeof(stmt_break_t), ((void**)(&result)));
        hash = 0x59983a;
        (MetaCParser_Match_((self), (tok_kw_break), "../metac_parser.c", 2955));
        result->Hash = hash;
    }
    else if (tokenType == tok_kw_continue)
    {
        stmt_continue_t* continue_ = (stmt_continue_t*) AllocNewStatement_(stmt_continue, sizeof(stmt_continue_t), ((void**)(&result)));
        hash = 0x83f482;
        (MetaCParser_Match_((self), (tok_kw_continue), "../metac_parser.c", 2962));
        result->Hash = hash;
    }
    else if (tokenType == tok_kw_case || tokenType == tok_kw_default)
    {


       _Bool

            isCase = tokenType == tok_kw_case;
        hash = (isCase ? 0x4064be : 0x7cee1a);
        stmt_case_t* case_ = (stmt_case_t*) AllocNewStatement_(stmt_case, sizeof(stmt_case_t), ((void**)(&result)));

        (MetaCParser_Match_((self), (tokenType), "../metac_parser.c", 2971));
        if (isCase)
        {
            case_->CaseExp =
                MetaCParser_ParseExpression(self, expr_flags_none, 0);

            hash = (crc32c_nozero(hash, &(case_->CaseExp->Hash), sizeof(case_->CaseExp->Hash)));
        }
        else
        {
            case_->CaseExp = (metac_expression_t*)((void*)0x1);
        }
        (MetaCParser_Match_((self), (tok_colon), "../metac_parser.c", 2983));
        metac_token_t* peek = (MetaCParser_PeekToken_(self, 1, 2984));

        case_->CaseBody = (metac_statement_t*)_emptyPointer;
        metac_statement_t** nextStmtP = &case_->CaseBody;

        do
        {
            metac_statement_t* nextStmt =
                MetaCParser_ParseStatement(self, (metac_statement_t*)case_, 0);
            hash = (crc32c_nozero(hash, &(nextStmt->Hash), sizeof(nextStmt->Hash)));
            nextStmt->Next = (metac_statement_t*)_emptyPointer;
            (*nextStmtP) = nextStmt;
            nextStmtP = &nextStmt->Next;
            peek = (MetaCParser_PeekToken_(self, 1, 2997));
            peek2 = (MetaCParser_PeekToken_(self, 2, 2998));
        } while((peek->TokenType != tok_kw_case
            && peek->TokenType != tok_rBrace
            && peek->TokenType != tok_kw_default
            && (peek2 == 0 || peek2->TokenType != tok_colon)));

        case_->Hash = hash;
    }
    else if (tokenType == tok_kw_return)
    {
        hash = 0x6363a6;
        stmt_return_t* return_ = (stmt_return_t*) AllocNewStatement_(stmt_return, sizeof(stmt_return_t), ((void**)(&result)));
        (MetaCParser_Match_((self), (tok_kw_return), "../metac_parser.c", 3010));
        if (MetaCParser_PeekMatch(self, tok_semicolon,

                                                      1

                                                          ))
        {
            return_->ReturnExp = (metac_expression_t*)_emptyPointer;
        }
        else
        {
            return_->ReturnExp = MetaCParser_ParseExpression(self, expr_flags_none, 0);
            hash = (crc32c_nozero(hash, &(return_->ReturnExp->Hash), sizeof(return_->ReturnExp->Hash)));
        }
        return_->Hash = hash;
    }
    else if (tokenType == tok_kw_yield)
    {
        hash = 0x521f59;
        stmt_yield_t* yield_ = (stmt_yield_t*) AllocNewStatement_(stmt_yield, sizeof(stmt_yield_t), ((void**)(&result)));
        (MetaCParser_Match_((self), (tok_kw_yield), "../metac_parser.c", 3026));
        if (MetaCParser_PeekMatch(self, tok_semicolon,

                                                      1

                                                          ))
        {
            yield_->YieldExp = (metac_expression_t*)_emptyPointer;
        }
        else
        {
            yield_->YieldExp = MetaCParser_ParseExpression(self, expr_flags_none, 0);
            hash = (crc32c_nozero(hash, &(yield_->YieldExp->Hash), sizeof(yield_->YieldExp->Hash)));
        }
        yield_->Hash = hash;
    }
    else if (tokenType == tok_lBrace)
    {
        result = (metac_statement_t*)MetaCParser_ParseBlockStatement(self, parent, prev);
    }
    else if (IsDeclarationFirstToken(tokenType))
    {
        metac_token_t* peek2 = (MetaCParser_PeekToken_(self, 2, 3044));
        metac_token_t* peek3 = (MetaCParser_PeekToken_(self, 3, 3045));
        if (peek2 && IsDeclarationToken(peek2->TokenType)
                                                          )
        {
            metac_declaration_t* decl = MetaCParser_ParseDeclaration(self, 0);
            stmt_decl_t* declStmt = (stmt_decl_t*) AllocNewStatement_(stmt_decl, sizeof(stmt_decl_t), ((void**)(&result)));


            declStmt->Declaration = decl;

            declStmt->Hash = decl->Hash;
        }
    }

    if (result && !result->Hash)
    {
        printf("Hash for %s unimplemented\n", StatementKind_toChars(result->StmtKind));
    }


    if (!result || result == ((void*)0x1))
    {
        metac_expression_t* exp = MetaCParser_ParseExpression(self, expr_flags_none, 0);
        stmt_exp_t* expStmt = (stmt_exp_t*) AllocNewStatement_(stmt_exp, sizeof(stmt_exp_t), ((void**)(&result)));
        expStmt->Expression = exp;
        result->Hash = exp->Hash;

    }
LdoneWithStatement:
    if (prev)
        prev->Next = result;

    MetaCPrinter_Reset(&self->DebugPrinter);

    if(tokenType != tok_lBrace && MetaCParser_PeekMatch(self, tok_semicolon,

                                                                            1

                                                                                ))
    {


        (MetaCParser_Match_((self), (tok_semicolon), "../metac_parser.c", 3082));
    }


    return result;
}

static inline void MetaCParser_PushBlockStatement(metac_parser_t* self,
                                                  stmt_block_t* stmt)
{
    self->CurrentBlockStatement =
        self->BlockStatementStack[self->BlockStatementStackCount++] = stmt;
}

static inline void MetaCParser_PopBlockStatement(metac_parser_t* self,
                                                 stmt_block_t* stmt)
{



    if (--self->BlockStatementStackCount > 0)
    {
        self->CurrentBlockStatement =
            self->BlockStatementStack[self->BlockStatementStackCount - 1];
    }
    else
        self->CurrentBlockStatement = 0;
}

static stmt_block_t* MetaCParser_ParseBlockStatement(metac_parser_t* self,
                                                     metac_statement_t* parent,
                                                     metac_statement_t* prev)
{
    metac_token_t* lBrace = (MetaCParser_Match_((self), (tok_lBrace), "../metac_parser.c", 3114));
    metac_location_t loc = LocationFromToken(self, lBrace);

    metac_statement_t* firstStatement = 0;
    metac_statement_t* nextStatement = 0;
    stmt_block_t* result;
    (stmt_block_t*) AllocNewStatement_(stmt_block, sizeof(stmt_block_t), ((void**)(&result)));
    result->Hash = ~0;
    uint32_t hash = ~0;

    MetaCParser_PushBlockStatement(self, result);

    for (;;)
    {
        metac_token_t* peekToken = (MetaCParser_PeekToken_(self, 1, 3128));
        metac_location_t loc = LocationFromToken(self, peekToken);

        if (peekToken && peekToken->TokenType == tok_rBrace)
        {
            if (!firstStatement)
            {
                firstStatement = (metac_statement_t*)_emptyPointer;
            }
            break;
        }

        if (!firstStatement)
        {
            firstStatement = MetaCParser_ParseStatement(self, (metac_statement_t*)result, firstStatement);
            nextStatement = firstStatement;
            if (nextStatement)
            {


                                          ;
                hash = (crc32c_nozero(hash, &(nextStatement->Hash), sizeof(nextStatement->Hash)));
            }
            else
            {
                fprintf(

               stderr

               , "ParseError[%s:%u]: {%u:%u}" "Statement expected" "\n", "../metac_parser.c", 3151, (loc.StartLine), (loc.StartColumn)); ParseErrorBreak();;
            }
        }
        else
        {
            MetaCParser_ParseStatement(self, (metac_statement_t*)result, nextStatement);
            result->Hash = (crc32c_nozero(result->Hash, &(nextStatement->Hash), sizeof(nextStatement->Hash)));
            if (nextStatement->Next && nextStatement->Next != ((void*)0x1))
            {
                nextStatement = nextStatement->Next;
            }
        }
    }

    result->Body = firstStatement;
    result->Hash = hash;

    metac_token_t* rBrace = (MetaCParser_Match_((self), (tok_rBrace), "../metac_parser.c", 3168));
    MetaCLocation_Expand(&loc, LocationFromToken(self, rBrace));
    result->LocationIdx = MetaCLocationStorage_Store(
        &self->LocationStorage, loc);

    MetaCParser_PopBlockStatement(self, result);

    return result;
}


metac_lexer_t g_lineLexer = {
    g_lineLexer.inlineTokens, 0, ((unsigned int)(sizeof((g_lineLexer.inlineTokens)) / sizeof((g_lineLexer.inlineTokens)[0]))),
    {g_lineLexer.inlineLocations, 0, ((unsigned int)(sizeof((g_lineLexer.inlineLocations)) / sizeof((g_lineLexer.inlineLocations)[0])))}
};

const char* MetaCExpressionKind_toChars(metac_expression_kind_t type)
{
    const char* result = 0;




    switch(type)
    {
        case exp_invalid : {result = "exp_invalid";} break; case exp_identifier : {result = "exp_identifier";} break; case exp_string : {result = "exp_string";} break; case exp_char : {result = "exp_char";} break; case exp_signed_integer : {result = "exp_signed_integer";} break; case exp_increment : {result = "exp_increment";} break; case exp_decrement : {result = "exp_decrement";} break; case exp_post_increment : {result = "exp_post_increment";} break; case exp_post_decrement : {result = "exp_post_decrement";} break; case exp_typeof : {result = "exp_typeof";} break; case exp_sizeof : {result = "exp_sizeof";} break; case exp_inject : {result = "exp_inject";} break; case exp_eject : {result = "exp_eject";} break; case exp_assert : {result = "exp_assert";} break; case exp_outer : {result = "exp_outer";} break; case exp_unary_dot : {result = "exp_unary_dot";} break; case exp_addr : {result = "exp_addr";} break; case exp_ptr : {result = "exp_ptr";} break; case exp_not : {result = "exp_not";} break; case exp_compl : {result = "exp_compl";} break; case exp_umin : {result = "exp_umin";} break; case exp_paren : {result = "exp_paren";} break; case exp_tuple : {result = "exp_tuple";} break; case exp_ternary : {result = "exp_ternary";} break; case exp_cast : {result = "exp_cast";} break; case exp_comma : {result = "exp_comma";} break; case exp_dot : {result = "exp_dot";} break; case exp_add : {result = "exp_add";} break; case exp_sub : {result = "exp_sub";} break; case exp_mul : {result = "exp_mul";} break; case exp_div : {result = "exp_div";} break; case exp_rem : {result = "exp_rem";} break; case exp_xor : {result = "exp_xor";} break; case exp_or : {result = "exp_or";} break; case exp_and : {result = "exp_and";} break; case exp_lsh : {result = "exp_lsh";} break; case exp_rsh : {result = "exp_rsh";} break; case exp_oror : {result = "exp_oror";} break; case exp_andand : {result = "exp_andand";} break; case exp_arrow : {result = "exp_arrow";} break; case exp_dotdot : {result = "exp_dotdot";} break; case exp_assign : {result = "exp_assign";} break; case exp_add_ass : {result = "exp_add_ass";} break; case exp_sub_ass : {result = "exp_sub_ass";} break; case exp_mul_ass : {result = "exp_mul_ass";} break; case exp_div_ass : {result = "exp_div_ass";} break; case exp_rem_ass : {result = "exp_rem_ass";} break; case exp_xor_ass : {result = "exp_xor_ass";} break; case exp_or_ass : {result = "exp_or_ass";} break; case exp_and_ass : {result = "exp_and_ass";} break; case exp_lsh_ass : {result = "exp_lsh_ass";} break; case exp_rsh_ass : {result = "exp_rsh_ass";} break; case exp_eq : {result = "exp_eq";} break; case exp_neq : {result = "exp_neq";} break; case exp_lt : {result = "exp_lt";} break; case exp_le : {result = "exp_le";} break; case exp_gt : {result = "exp_gt";} break; case exp_ge : {result = "exp_ge";} break; case exp_spaceship : {result = "exp_spaceship";} break; case exp_full_slice : {result = "exp_full_slice";} break; case exp_slice : {result = "exp_slice";} break; case exp_index : {result = "exp_index";} break; case exp_call : {result = "exp_call";} break; case exp_argument : {result = "exp_argument";} break; case exp_type : {result = "exp_type";} break; case exp_variable : {result = "exp_variable";} break; case exp_addr_or_and : {result = "exp_addr_or_and";} break; case exp_ptr_or_mul : {result = "exp_ptr_or_mul";} break; case exp_dot_compiler : {result = "exp_dot_compiler";} break; case exp_dot_context : {result = "exp_dot_context";} break; case exp_dot_target : {result = "exp_dot_target";} break; case exp_max : {result = "exp_max";} break;
    }

    return result;


}

const char* MetaCNodeKind_toChars(metac_node_kind_t type)
{
    const char* result = 0;




    switch(type)
    {
        case exp_invalid : {result = "exp_invalid";} break; case exp_identifier : {result = "exp_identifier";} break; case exp_string : {result = "exp_string";} break; case exp_char : {result = "exp_char";} break; case exp_signed_integer : {result = "exp_signed_integer";} break; case exp_increment : {result = "exp_increment";} break; case exp_decrement : {result = "exp_decrement";} break; case exp_post_increment : {result = "exp_post_increment";} break; case exp_post_decrement : {result = "exp_post_decrement";} break; case exp_typeof : {result = "exp_typeof";} break; case exp_sizeof : {result = "exp_sizeof";} break; case exp_inject : {result = "exp_inject";} break; case exp_eject : {result = "exp_eject";} break; case exp_assert : {result = "exp_assert";} break; case exp_outer : {result = "exp_outer";} break; case exp_unary_dot : {result = "exp_unary_dot";} break; case exp_addr : {result = "exp_addr";} break; case exp_ptr : {result = "exp_ptr";} break; case exp_not : {result = "exp_not";} break; case exp_compl : {result = "exp_compl";} break; case exp_umin : {result = "exp_umin";} break; case exp_paren : {result = "exp_paren";} break; case exp_tuple : {result = "exp_tuple";} break; case exp_ternary : {result = "exp_ternary";} break; case exp_cast : {result = "exp_cast";} break; case exp_comma : {result = "exp_comma";} break; case exp_dot : {result = "exp_dot";} break; case exp_add : {result = "exp_add";} break; case exp_sub : {result = "exp_sub";} break; case exp_mul : {result = "exp_mul";} break; case exp_div : {result = "exp_div";} break; case exp_rem : {result = "exp_rem";} break; case exp_xor : {result = "exp_xor";} break; case exp_or : {result = "exp_or";} break; case exp_and : {result = "exp_and";} break; case exp_lsh : {result = "exp_lsh";} break; case exp_rsh : {result = "exp_rsh";} break; case exp_oror : {result = "exp_oror";} break; case exp_andand : {result = "exp_andand";} break; case exp_arrow : {result = "exp_arrow";} break; case exp_dotdot : {result = "exp_dotdot";} break; case exp_assign : {result = "exp_assign";} break; case exp_add_ass : {result = "exp_add_ass";} break; case exp_sub_ass : {result = "exp_sub_ass";} break; case exp_mul_ass : {result = "exp_mul_ass";} break; case exp_div_ass : {result = "exp_div_ass";} break; case exp_rem_ass : {result = "exp_rem_ass";} break; case exp_xor_ass : {result = "exp_xor_ass";} break; case exp_or_ass : {result = "exp_or_ass";} break; case exp_and_ass : {result = "exp_and_ass";} break; case exp_lsh_ass : {result = "exp_lsh_ass";} break; case exp_rsh_ass : {result = "exp_rsh_ass";} break; case exp_eq : {result = "exp_eq";} break; case exp_neq : {result = "exp_neq";} break; case exp_lt : {result = "exp_lt";} break; case exp_le : {result = "exp_le";} break; case exp_gt : {result = "exp_gt";} break; case exp_ge : {result = "exp_ge";} break; case exp_spaceship : {result = "exp_spaceship";} break; case exp_full_slice : {result = "exp_full_slice";} break; case exp_slice : {result = "exp_slice";} break; case exp_index : {result = "exp_index";} break; case exp_call : {result = "exp_call";} break; case exp_argument : {result = "exp_argument";} break; case exp_type : {result = "exp_type";} break; case exp_variable : {result = "exp_variable";} break; case exp_addr_or_and : {result = "exp_addr_or_and";} break; case exp_ptr_or_mul : {result = "exp_ptr_or_mul";} break; case exp_dot_compiler : {result = "exp_dot_compiler";} break; case exp_dot_context : {result = "exp_dot_context";} break; case exp_dot_target : {result = "exp_dot_target";} break; case exp_max : {result = "exp_max";} break; case stmt_block : {result = "stmt_block";} break; case stmt_if : {result = "stmt_if";} break; case stmt_switch : {result = "stmt_switch";} break; case stmt_while : {result = "stmt_while";} break; case stmt_for : {result = "stmt_for";} break; case stmt_do_while : {result = "stmt_do_while";} break; case stmt_label : {result = "stmt_label";} break; case stmt_case : {result = "stmt_case";} break; case stmt_break : {result = "stmt_break";} break; case stmt_yield : {result = "stmt_yield";} break; case stmt_scope : {result = "stmt_scope";} break; case stmt_continue : {result = "stmt_continue";} break; case stmt_goto : {result = "stmt_goto";} break; case stmt_return : {result = "stmt_return";} break; case stmt_exp : {result = "stmt_exp";} break; case stmt_decl : {result = "stmt_decl";} break; case stmt_comment : {result = "stmt_comment";} break; case decl_variable : {result = "decl_variable";} break; case decl_field : {result = "decl_field";} break; case decl_parameter : {result = "decl_parameter";} break; case decl_enum_member : {result = "decl_enum_member";} break; case decl_type : {result = "decl_type";} break; case decl_type_struct : {result = "decl_type_struct";} break; case decl_type_union : {result = "decl_type_union";} break; case decl_type_enum : {result = "decl_type_enum";} break; case decl_type_array : {result = "decl_type_array";} break; case decl_type_ptr : {result = "decl_type_ptr";} break; case decl_type_functiontype : {result = "decl_type_functiontype";} break; case decl_type_tuple : {result = "decl_type_tuple";} break; case decl_type_typedef : {result = "decl_type_typedef";} break; case decl_function : {result = "decl_function";} break; case decl_label : {result = "decl_label";} break; case decl_comment : {result = "decl_comment";} break;
    }

    return result;


}













static inline char* u64tostr(uint64_t v, char buffer[21])
{
    int i;
    buffer[20] = '\0';

    if (!v)
    {
        buffer[19] = '0';
        return &buffer[19];
    }

    for(i = 19; v > 0; --i)
    {
        buffer[i] = (v % 10) + '0';
        v /= 10;
    }
    return buffer + i + 1;
}

static inline char* i64tostr(int64_t v, char buffer[22])
{
    int wasNegative = (v & (1ULL << 63ULL)) != 0;

    if (wasNegative) v = ~v + 1;

    char* result = u64tostr(v, buffer + wasNegative);

    if (wasNegative) {
        (--result)[0] = '-';
    }

    return result;
}

static inline char* u64tohexstr(uint64_t v, char buffer[17])
{
    buffer[16] = '\0';
    for(int i = 0; i < 16; i++)
        buffer[i] = '0';

    if (!v)
    {
        buffer[15] = '0';
        return &buffer[15];
    }

    int i;
    for(i = 15; v > 0; --i)
    {
        uint8_t nibble = v & 0xF;
        buffer[i] = nibble + ((nibble > 9) ? ('A' - 10) : '0');
        v = (v & ~0xFUL) >> 4;
    }
    return buffer + i + 1;
}







static inline void PrintExpression(metac_printer_t* self, metac_expression_t* exp);

static inline void PrintSpace(metac_printer_t* self)
{
    self->StringMemorySize++;
    self->CurrentColumn++;
}

static inline void PrintChar(metac_printer_t* self, char c)
{



                                ;
    self->StringMemory[self->StringMemorySize++] = c;
}

static inline void PrintNewline(metac_printer_t* self)
{
    self->StringMemory[self->StringMemorySize++] = '\n';
    self->CurrentColumn = 0;
}

void static inline PrintIndent(metac_printer_t* self)
{
    const uint32_t indent = self->IndentLevel;

    self->StringMemorySize += 4 * indent;
    self->CurrentColumn += 4 * indent;

    if (self->StartColumn > self->CurrentColumn)
    {
        int32_t columnAdvance =
            self->StartColumn - self->CurrentColumn;
        self->StringMemorySize += columnAdvance;
        self->CurrentColumn += columnAdvance;
    }
}

static inline void CheckAndRellocIfNeeded(metac_printer_t* self,
                                   uint32_t length)
{
    int32_t underflow =
        self->StringMemoryCapacity -
        (self->StringMemorySize + length + 1024);
    if (underflow < 0)
    {
        uint32_t newCapa = (uint32_t)((self->StringMemoryCapacity) * 1.3);
        newCapa = ((newCapa + 4095) & ~4095);
        self->StringMemory = (char*)realloc(self->StringMemory, newCapa);
        if (self->StringMemory == 0)
        {
            ;
        }
    }
}

static inline void PrintString(metac_printer_t* self,
                 const char* string, uint32_t length)
{
    char c;

    while((c = *string++))
    {
        self->StringMemory[self->StringMemorySize++] = c;
    }
    self->CurrentColumn += length;
}

static inline void PrintIdentifier(metac_printer_t* self,
                                   metac_identifier_ptr_t idPtr)
{
    if (idPtr.v == empty_identifier.v)



                ;
    const char* ident = IdentifierPtrToCharPtr(self->IdentifierTable, idPtr);

    PrintString(self, ident, strlen(ident));
}

static void PrintKeyword(metac_printer_t* self,
                         metac_token_enum_t keyword)
{
    const char * str =
        MetaCTokenEnum_toChars(keyword) + sizeof("tok_kw");

    PrintString(self, str, strlen(str));
}

static inline void PrintToken(metac_printer_t* self,
                              metac_token_enum_t tokenType)
{



    switch(tokenType)
    {
        case tok_kw_struct : case tok_kw_union : case tok_kw_enum : case tok_kw_typedef : case tok_kw_auto : case tok_kw_void : case tok_kw_bool : case tok_kw_char : case tok_kw_short : case tok_kw_int : case tok_kw_long : case tok_kw_uint32_t : case tok_kw_float : case tok_kw_double : case tok_kw_signed : case tok_kw_unsigned : case tok_kw_const : case tok_kw_volatile : case tok_kw___shared : case tok_kw_extern : case tok_kw_for : case tok_kw_sizeof : case tok_kw_return : case tok_kw_switch : case tok_kw_while : case tok_kw_do : case tok_kw_typeof : case tok_kw_inject : case tok_kw_eject : case tok_kw_assert : case tok_kw_case : case tok_kw_default : case tok_kw_goto : case tok_kw_static : case tok_kw_inline : case tok_kw_if : case tok_kw_else : case tok_kw_break : case tok_kw_continue : case tok_kw_until : case tok_kw_yield : case tok_kw___attribute__ :



        case tok_semicolon:
            PrintChar(self, ';');
        break;
        case tok_lBrace :
            PrintChar(self, '{');
        break;

        case tok_rBrace :
            PrintChar(self, '}');
        break;

        case tok_lParen:
            PrintChar(self, '(');
        break;

        case tok_rParen:
            PrintChar(self, ')');
        break;

        case tok_lBracket:
            PrintChar(self, '[');
        break;
        case tok_rBracket:
            PrintChar(self, ']');
        break;

        case tok_assign:
            PrintChar(self, '=');
        break;
    }
}

static inline void PrintU64(metac_printer_t* self, uint64_t value)
{
    char u64Buffer[21];

    const char* result = u64tostr(value, u64Buffer);
    int32_t length = (u64Buffer + 20) - result;

    PrintString(self, result, length);
}

static inline void PrintI64(metac_printer_t* self, int64_t value)
{
    char i64Buffer[22];

    const char* result = i64tostr(value, i64Buffer);
    int32_t length = (i64Buffer + 20) - result;

    PrintString(self, result, length);
}

static inline void PrintType(metac_printer_t* self, decl_type_t* type);
static inline void PrintParameterList(metac_printer_t* self,
                                     decl_parameter_t* Parameters);

static inline void PrintVariable(metac_printer_t* self,
                                 decl_variable_t* variable)
{
    if (variable->VarType->DeclKind == decl_type_functiontype)
    {
        decl_type_functiontype_t *funcType =
            (decl_type_functiontype_t*) variable->VarType;

        PrintType(self, funcType->ReturnType);
        PrintSpace(self);
        PrintChar(self, '(');
        PrintChar(self, '*');
        PrintIdentifier(self, variable->VarIdentifier);
        PrintChar(self, ')');
        PrintSpace(self);
        PrintParameterList(self, funcType->Parameters);
    }
    else
    {
        PrintType(self, variable->VarType);
        PrintSpace(self);
        PrintIdentifier(self, variable->VarIdentifier);
    }

    if (variable->VarInitExpression != ((void*)0x1))
    {
        PrintSpace(self);
        PrintToken(self, tok_assign);
        PrintSpace(self);
        PrintExpression(self, variable->VarInitExpression);
    }
}

static inline void PrintParameterList(metac_printer_t* self,
                                      decl_parameter_t* Parameters)
{
    PrintToken(self, tok_lParen);

    for(decl_parameter_t* param = Parameters;
        param != ((void*)0x1);
        param = param->Next)
    {
        if (param->Parameter->VarIdentifier.v != empty_identifier.v)
        {
            PrintVariable(self, param->Parameter);
        }
        else
        {
            PrintType(self, param->Parameter->VarType);
        }
        if (param->Next != ((void*)0x1))
                PrintString(self, ", ", 2);
    }

    PrintToken(self, tok_rParen);
}


static inline void PrintType(metac_printer_t* self, decl_type_t* type)
{
    switch(type->DeclKind)
    {
        case decl_type_array:
        {
            decl_type_array_t *arrayType = (decl_type_array_t*) type;

            PrintType(self, arrayType->ElementType);
            PrintChar(self, '[');
            PrintExpression(self, arrayType->Dim);
            PrintChar(self, ']');
        } break;

        case decl_type_ptr:
        {
            decl_type_ptr_t *ptrType = (decl_type_ptr_t*) type;

            PrintType(self, ptrType->ElementType);
            PrintChar(self, '*');
        } break;

        case decl_type:
        {

            if (type->TypeModifiers)
            {
                uint32_t modifiers = type->TypeModifiers;
                if (modifiers & typemod_const)
                {
                    PrintString(self, "const ", sizeof("const"));
                }
                if (modifiers & typemod_unsigned)
                {
                    PrintString(self, "unsigned ", sizeof("unsigned"));
                }
            }
            if (type->TypeKind >= type_auto && type->TypeKind <= type_double)
            {
                metac_token_enum_t tok = (metac_token_enum_t)
                    ((type->TypeKind - type_auto) + tok_kw_auto);
                PrintKeyword(self, tok);
            }
            else if (type->TypeKind == type_long_long)
            {
                PrintString(self, "long long", sizeof("long long") - 1);
            }
            else if (type->TypeKind == type_long_double)
            {
                PrintString(self, "long double", sizeof("long double") - 1);
            }
            else if (type->TypeKind == type_type)
            {
                PrintString(self, "type", sizeof("type") - 1);
            }
            else if (type->TypeKind == type_identifier)
            {
                PrintIdentifier(self, type->TypeIdentifier);


            }
        } break;
        case decl_type_struct :
        {
            decl_type_struct_t* structType = (decl_type_struct_t*) type;
            PrintIdentifier(self, structType->Identifier);
        }
        break;
        case decl_type_functiontype:
        {
            decl_type_functiontype_t *funcType = (decl_type_functiontype_t*) type;
            PrintType(self, funcType->ReturnType);
            PrintSpace(self);
            PrintChar(self, '(');
            PrintChar(self, '*');
            PrintString(self, "function) ", sizeof("function) ") - 1);
            PrintParameterList(self, funcType->Parameters);
        } break;
        default : break;

    }
}

static inline void PrintDeclaration(metac_printer_t* self,
                                    metac_declaration_t* decl,
                                    uint32_t level);




const char* StatementKind_toChars(metac_statement_kind_t kind)
{
    const char* result = 0;

    switch(kind)
    {
        case stmt_block : {result = "stmt_block";} break; case stmt_if : {result = "stmt_if";} break; case stmt_switch : {result = "stmt_switch";} break; case stmt_while : {result = "stmt_while";} break; case stmt_for : {result = "stmt_for";} break; case stmt_do_while : {result = "stmt_do_while";} break; case stmt_label : {result = "stmt_label";} break; case stmt_case : {result = "stmt_case";} break; case stmt_break : {result = "stmt_break";} break; case stmt_yield : {result = "stmt_yield";} break; case stmt_scope : {result = "stmt_scope";} break; case stmt_continue : {result = "stmt_continue";} break; case stmt_goto : {result = "stmt_goto";} break; case stmt_return : {result = "stmt_return";} break; case stmt_exp : {result = "stmt_exp";} break; case stmt_decl : {result = "stmt_decl";} break; case stmt_comment : {result = "stmt_comment";} break;
    }

    return result;
}



static inline void PrintComment(metac_printer_t* self,
                                const char* Text, uint32_t Length)
{

    PrintString(self, "/*", 2);
    PrintString(self, Text, Length);
    PrintString(self, "*/", 2);

}
static inline void PrintStatement(metac_printer_t* self, metac_statement_t* stmt)
{

    switch(stmt->StmtKind)
    {
        case stmt_return :
        {
            stmt_return_t* stmt_return = (stmt_return_t*) stmt;

            PrintKeyword(self, tok_kw_return);
            PrintSpace(self);
            if (stmt_return->ReturnExp != ((void*)0x1))
                PrintExpression(self, stmt_return->ReturnExp);
            PrintChar(self, ';');
        } break;
        case stmt_yield :
        {
            stmt_yield_t* stmt_yield = (stmt_yield_t*) stmt;

            PrintKeyword(self, tok_kw_yield);
            PrintSpace(self);
            if (stmt_yield->YieldExp != ((void*)0x1))
                PrintExpression(self, stmt_yield->YieldExp);
            PrintChar(self, ';');
        } break;
        case stmt_block :
        {
            stmt_block_t* stmt_block = (stmt_block_t*) stmt;

            PrintToken(self, tok_lBrace);
            ++self->IndentLevel;
            PrintNewline(self);
            PrintIndent(self);

            for(metac_statement_t* nextStmt = stmt_block->Body;
                nextStmt != ((void*)0x1);
                nextStmt = nextStmt->Next)
            {
                PrintStatement(self, nextStmt);
                if(nextStmt->Next)
                {
                    PrintNewline(self);
                    PrintIndent(self);
                }
            }

            --self->IndentLevel;
            if (stmt->Next)
            {
                PrintNewline(self);
                PrintIndent(self);
            }
            PrintToken(self, tok_rBrace);
        } break;
        case stmt_if :
        {
            stmt_if_t* stmt_if_ = (stmt_if_t*) stmt;

            PrintKeyword(self, tok_kw_if);
            PrintSpace(self);
            PrintChar(self, '(');
            PrintExpression(self, stmt_if_->IfCond);
            PrintChar(self, ')');

            if (stmt_if_->IfBody->StmtKind != stmt_block)
                ++self->IndentLevel;
            PrintNewline(self);
            PrintIndent(self);
            PrintStatement(self, stmt_if_->IfBody);
            if (stmt_if_->IfBody->StmtKind != stmt_block)
            {

                --self->IndentLevel;
            }
            PrintNewline(self);
            PrintIndent(self);

            if (stmt_if_->ElseBody != ((void*)0x1))
            {
                PrintKeyword(self, tok_kw_else);
                if (stmt_if_->ElseBody->StmtKind == stmt_if)
                {
                    PrintSpace(self);
                }
                else
                {
                    if (stmt_if_->ElseBody->StmtKind != stmt_block)
                        ++self->IndentLevel;

                    PrintNewline(self);
                    PrintIndent(self);
                }
                PrintStatement(self, stmt_if_->ElseBody);

                if (stmt_if_->ElseBody->StmtKind != stmt_if)
                {
                    if (stmt_if_->ElseBody->StmtKind != stmt_block)
                        --self->IndentLevel;
                }

                PrintNewline(self);
                PrintIndent(self);
            }
        } break;
        case stmt_exp :
        {
            stmt_exp_t* exp_stmt = (stmt_exp_t*) stmt;
            PrintExpression(self, exp_stmt->Expression);
            PrintToken(self, tok_semicolon);
        } break;
        case stmt_decl:
        {
            stmt_decl_t* decl_stmt = (stmt_decl_t*) stmt;
            PrintDeclaration(self, decl_stmt->Declaration, 0);
            PrintToken(self, tok_semicolon);
        } break;
        case stmt_for:
        {
            stmt_for_t* stmt_for = (stmt_for_t*) stmt;
            PrintKeyword(self, tok_kw_for);
            PrintChar(self, '(');
            if (stmt_for->ForInit != (metac_node_t) ((void*)0x1))
            {
                MetaCPrinter_PrintNode(self, stmt_for->ForInit, 0);
            }
            else
            {
                PrintChar(self, ';');
            }

            if (stmt_for->ForCond != (metac_expression_t*) ((void*)0x1))
            {
                PrintExpression(self, stmt_for->ForCond);
            }
            PrintChar(self, ';');
            if (stmt_for->ForPostLoop != (metac_expression_t*) ((void*)0x1))
            {
                PrintExpression(self, stmt_for->ForPostLoop);
            }
            PrintChar(self, ')');
            PrintNewline(self);
            PrintIndent(self);
            PrintStatement(self, stmt_for->ForBody);
        } break;
        case stmt_break:
        {
            stmt_break_t* stmt_break = (stmt_break_t*) stmt;
            PrintKeyword(self, tok_kw_break);
        } break;
        case stmt_continue:
        {
            stmt_continue_t* stmt_continue = (stmt_continue_t*) stmt;
            PrintKeyword(self, tok_kw_continue);
        } break;
        case stmt_case:
        {
            stmt_case_t* stmt_case = (stmt_case_t*) stmt;
            if (stmt_case->CaseExp == (metac_expression_t*) ((void*)0x1))
            {
                PrintKeyword(self, tok_kw_default);
            }
            else
            {
                PrintKeyword(self, tok_kw_case);
                PrintSpace(self);
                PrintExpression(self, stmt_case->CaseExp);
            }
            PrintChar(self, ':');
            if (stmt_case->CaseBody != (metac_statement_t*) ((void*)0x1))
            {
                if (stmt_case->CaseBody->StmtKind != stmt_block)
                {
                    ++self->IndentLevel;
                    PrintNewline(self);
                    PrintIndent(self);
                    metac_statement_t* stmt = stmt_case->CaseBody;
                    while(stmt && stmt != ((void*)0x1))
                    {
                        PrintStatement(self, stmt);
                        stmt = stmt->Next;
                        if (stmt)
                        {
                            PrintNewline(self);
                            PrintIndent(self);
                        }
                    }
                }
                else
                {
                    PrintStatement(self, stmt_case->CaseBody);
                }
                if (stmt_case->CaseBody->StmtKind != stmt_block)
                    --self->IndentLevel;
            }
        } break;
        case stmt_label:
        {
            stmt_label_t* stmt_label = (stmt_label_t*) stmt;
            PrintIdentifier(self, stmt_label->Label);
            PrintChar(self, ':');
        } break;
        case stmt_goto:
        {
            stmt_goto_t* stmt_goto = (stmt_goto_t*) stmt;
            PrintKeyword(self, tok_kw_goto);
            PrintSpace(self);
            PrintIdentifier(self, stmt_goto->GotoLabel);
            PrintChar(self, ';');
        } break;
        case stmt_switch:
        {
            stmt_switch_t* stmt_switch = (stmt_switch_t*) stmt;
            PrintKeyword(self, tok_kw_switch);
            PrintSpace(self);
            PrintChar(self, '(');
            PrintExpression(self, stmt_switch->SwitchExp);
            PrintChar(self, ')');
            PrintNewline(self);
            PrintIndent(self);
            PrintStatement(self, stmt_switch->SwitchBody);
        } break;
        case stmt_while:
        {
            stmt_while_t* stmt_while = (stmt_while_t*)stmt;
            PrintKeyword(self, tok_kw_while);
            PrintSpace(self);
            PrintChar(self, '(');
            PrintExpression(self, stmt_while->WhileExp);
            PrintChar(self, ')');
            PrintNewline(self);
            PrintIndent(self);
            PrintStatement(self, stmt_while->WhileBody);
        } break;
        case stmt_comment:
        {
            stmt_comment_t* comment = (stmt_comment_t*)stmt;
            PrintString(self, "/*", 2);
            PrintString(self, comment->Text, comment->Length);
            PrintString(self, "*/", 2);
        } break;

        default : {
            fprintf(

                   stderr

                         ,
                "Statement Kind: not handled by printer %s\n",
                    StatementKind_toChars(stmt->StmtKind));


        }
    }
}

static inline metac_token_enum_t AggToken(metac_declaration_kind_t declKind)
{
    metac_token_enum_t result;

    if (declKind == decl_type_struct)
    {
        result = tok_kw_struct;
    } else if (declKind == decl_type_union)
    {
        result = tok_kw_union;
    }



    else
    {



    }

    return result;
}

static inline void PrintDeclaration(metac_printer_t* self,
                                    metac_declaration_t* decl,
                                    uint32_t level)
{


   _Bool

        printSemicolon =

                         1

                             ;

    switch (decl->DeclKind)
    {
        case decl_type_enum:
        {
            decl_type_enum_t* enum_ = (decl_type_enum_t*) decl;
            PrintString(self, "enum", sizeof("enum") - 1);
            PrintSpace(self);
            if (enum_->Identifier.v != empty_identifier.v)
            {
                PrintIdentifier(self, enum_->Identifier);
                PrintSpace(self);
            }
            PrintSpace(self);
            PrintToken(self, tok_lBrace);
            ++level;
            ++self->IndentLevel;
            decl_enum_member_t* member = enum_->Members;
            if (enum_->MemberCount)
            {
                PrintNewline(self);
                PrintIndent(self);
            }
            else
            {
                PrintSpace(self);
            }
            for(uint32_t memberIndex = 0;
                memberIndex < enum_->MemberCount;
                memberIndex++)
            {
                PrintIdentifier(self, member->Name);
                if (member->Value != ((void*)0x1))
                {
                    PrintSpace(self);
                    PrintChar(self, '=');
                    PrintSpace(self);
                    PrintExpression(self, member->Value);
                }
                if (member->Next != ((void*)0x1))
                {
                    PrintChar(self, ',');
                    PrintNewline(self);
                    PrintIndent(self);
                }
                member = member->Next;
            }
            --self->IndentLevel;
            --level;
            PrintIndent(self);
            PrintNewline(self);
            PrintToken(self, tok_rBrace);
        } break;
        case decl_type_typedef:
        {
            decl_type_typedef_t* typdef = (decl_type_typedef_t*) decl;
            PrintString(self, "typedef ", sizeof("typedef ") - 1);
            level++;
            PrintDeclaration(self, (metac_declaration_t*)typdef->Type, level);
            if (typdef->Identifier.v != empty_identifier.v)
            {
                PrintIdentifier(self, typdef->Identifier);
            }
            level--;
        } break;
        case decl_type:
        {
            PrintType(self, (decl_type_t*) decl);
        } break;
        case decl_type_union :
        case decl_type_struct :
        {
            decl_type_struct_t* struct_ = (decl_type_struct_t*) decl;
            PrintKeyword(self, AggToken(decl->DeclKind));
            if (struct_->Identifier.v != empty_identifier.v)
            {
                PrintSpace(self);
                PrintIdentifier(self, struct_->Identifier);
            }
            PrintSpace(self);
            PrintToken(self, tok_lBrace);
            ++level;
            ++self->IndentLevel;
            PrintNewline(self);
            PrintIndent(self);
            decl_field_t* f = struct_->Fields;
            for(uint32_t memberIndex = 0;
                memberIndex < struct_->FieldCount;
                memberIndex++)
            {
                PrintDeclaration(self, (metac_declaration_t*)f, level);

                if (f->Next && f->Next != ((void*)0x1))
                    PrintIndent(self);
                f = f->Next;
            }
            --self->IndentLevel;
            --level;

            PrintIndent(self);
            PrintToken(self, tok_rBrace);
            if (self->IndentLevel)
                PrintNewline(self);
            else
                PrintSpace(self);
        } break;
        case decl_type_array:
        {
            PrintType(self, (decl_type_t*)decl);
        } break;
        case decl_field :
        {
            decl_field_t* field = (decl_field_t*) decl;
            PrintVariable(self, field->Field);
        } break;
        case decl_variable:
        {
            decl_variable_t* variable = (decl_variable_t*) decl;
            PrintVariable(self, variable);
        } break;
        case decl_function:
        {
            decl_function_t* function_ = (decl_function_t*) decl;
            PrintType(self, function_->ReturnType);
            PrintSpace(self);
            PrintIdentifier(self, function_->Identifier);
            PrintParameterList(self, function_->Parameters);

            if (function_->FunctionBody != ((void*)0x1))
            {
                PrintStatement(self, (metac_statement_t*)function_->FunctionBody);
                printSemicolon =

                                0

                                     ;
            }
        } break;
        case decl_comment:
        {
            decl_comment_t* comment = (decl_comment_t*) decl;
            PrintComment(self, comment->Text, comment->Length);
        } break;
        case decl_label:
        {
            decl_label_t* label = (decl_label_t*) decl;
            PrintIdentifier(self, label->Identifier);
            PrintChar(self, ':');
        } break;
    }
    if (!!printSemicolon) PrintChar(self, ';');
    PrintNewline(self);
}

static inline void PrintExpression(metac_printer_t* self, metac_expression_t* exp)
{
    if (exp->Kind == exp_paren)
    {
        if (!IsBinaryExp(exp->E1->Kind))
            PrintChar(self, '(');

        PrintExpression(self, exp->E1);

        if (!IsBinaryExp(exp->E1->Kind))
            PrintChar(self, ')');
    }
    else if (exp->Kind == exp_ternary)
    {
        PrintChar(self, '(');
        PrintExpression(self, exp->Econd);
        PrintSpace(self);
        PrintChar(self, '?');
        PrintSpace(self);
        PrintExpression(self, exp->E1);
        PrintSpace(self);
        PrintChar(self, ':');
        PrintSpace(self);
        PrintExpression(self, exp->E2);
        PrintChar(self, ')');
    }
    else if (exp->Kind == exp_tuple)
    {
        PrintChar(self, '{');
        exp_tuple_t* tupleElement =
            exp->TupleExpressionList;
        for(uint32_t i = 0;
            i < exp->TupleExpressionCount;
            i++)
        {
            PrintExpression(self, tupleElement->Expression);
            if (i != (exp->TupleExpressionCount - 1))
            {
                PrintChar(self, ',');
                PrintSpace(self);
            }
            tupleElement = tupleElement->Next;
        }
        PrintChar(self, '}');
    }
    else if (exp->Kind == exp_type)
    {
        PrintChar(self, '(');
        PrintType(self, exp->TypeExp);
        PrintChar(self, ')');
    }
    else if (exp->Kind == exp_identifier)
    {
        PrintIdentifier(self, exp->IdentifierPtr);
    }
    else if (exp->Kind == exp_string)
    {
        uint32_t stringLength = ( (exp->StringKey) >> 12 );
        PrintChar(self, '"');
        PrintString(self,
            IdentifierPtrToCharPtr(self->StringTable, exp->StringPtr),
            stringLength);
        PrintChar(self, '"');
    }
    else if (exp->Kind == exp_signed_integer)
    {
        PrintI64(self, exp->ValueI64);
    }
    else if (exp->Kind == exp_char)
    {
        PrintChar(self, '\'');
        PrintString(self, exp->Chars, ( (exp->CharKey) >> 28 ));
        PrintChar(self, '\'');
    }
    else if (IsBinaryExp(exp->Kind))
    {
        PrintChar(self, '(');
        PrintExpression(self, exp->E1);

        PrintSpace(self);
        const char* op = BinExpTypeToChars((metac_binary_expression_kind_t)exp->Kind);
        PrintString(self, op, strlen(op));
        PrintSpace(self);

        PrintExpression(self, exp->E2);
        PrintChar(self, ')');
    }
    else if (exp->Kind == exp_cast)
    {
        PrintString(self, "cast", 4);
        PrintChar(self, '(');
        PrintType(self, exp->CastType);
        PrintChar(self, ')');

        PrintExpression(self, exp->CastExp);
    }
    else if (exp->Kind == exp_call)
    {
        PrintExpression(self, exp->E1);
        PrintChar(self, '(');

        PrintExpression(self, exp->E2);
        PrintChar(self, ')');
    }
    else if (exp->Kind == exp_argument)
    {
        for(exp_argument_t* arg = (exp_argument_t*)exp;
            arg != ((void*)0x1);
            arg = arg->Next)
        {
            PrintExpression(self, arg->Expression);
            if (arg->Next != ((void*)0x1))
                PrintString(self, ", ", 2);
        }
    }
    else if (exp->Kind == exp_index)
    {
        PrintExpression(self, exp->E1);
        PrintToken(self, tok_lBracket);
        PrintExpression(self, exp->E2);
        PrintToken(self, tok_rBracket);
    }
    else if (exp->Kind == exp_sizeof)
    {
        PrintKeyword(self, tok_kw_sizeof);
        PrintToken(self, tok_lParen);
        PrintExpression(self, exp->E1);
        PrintToken(self, tok_rParen);
    }
    else if (exp->Kind == exp_addr || exp->Kind == exp_ptr
          || exp->Kind == exp_not || exp->Kind == exp_compl
          || exp->Kind == exp_umin)
    {
        {
            const char* op = 0;
            if (exp->Kind == exp_addr)
                op = "&";
            else if (exp->Kind == exp_ptr)
                op = "*";
            else if (exp->Kind == exp_not)
                op = "!";
            else if (exp->Kind == exp_compl)
                op = "~";
            else if (exp->Kind == exp_umin)
                op = "-";

            PrintString(self, op, strlen(op));
        }

        if (!IsBinaryExp(exp->E1->Kind))
            PrintChar(self, '(');

        PrintExpression(self, exp->E1);

        if (!IsBinaryExp(exp->E1->Kind))
            PrintChar(self, ')');
    }
    else if (exp->Kind == exp_post_increment || exp->Kind == exp_post_decrement)
    {
        const char* op = 0;
        if (exp->Kind == exp_post_increment)
            op = "++";
        else if (exp->Kind == exp_post_decrement)
            op = "--";




        if (!IsBinaryExp(exp->E1->Kind))
            PrintChar(self, '(');

        PrintExpression(self, exp->E1);

        if (!IsBinaryExp(exp->E1->Kind))
            PrintChar(self, ')');

        PrintString(self, op, strlen(op));
    }
    else if (exp->Kind == exp_inject || exp->Kind == exp_eject
          || exp->Kind == exp_typeof || exp->Kind == exp_assert
          || exp->Kind == exp_unary_dot)
    {
        {
            const char* op = 0;
            if (exp->Kind == exp_inject)
                op = "inject";
            else if (exp->Kind == exp_eject)
                op = "eject";
            else if (exp->Kind == exp_typeof)
                op = "typeof";
            else if (exp->Kind == exp_assert)
                op = "assert";
            else if (exp->Kind == exp_unary_dot)
                op = ".";



                     ;

            PrintString(self, op, strlen(op));
        }

        if (!IsBinaryExp(exp->E1->Kind))
           PrintChar(self, '(');

        PrintExpression(self, exp->E1);

        if (!IsBinaryExp(exp->E1->Kind))
            PrintChar(self, ')');
    }
    else
    {
        printf("don't know how to print %s\n", (MetaCExpressionKind_toChars(exp->Kind)));
    }
}








static inline void aco_abort(void)
{
    ;
}

typedef struct {
    void* ptr;
    uint32_t sz;
    uint32_t valid_sz;

    uint32_t max_cpsz;

    uint32_t ct_save;

    uint32_t ct_restore;
} aco_save_stack_t;

struct aco_s;
typedef struct aco_s aco_t;

typedef struct {
    void* ptr;
    uint32_t sz;
    void* align_highptr;
    void* align_retptr;
    uint32_t align_validsz;
    uint32_t align_limit;
    aco_t* owner;

    char guard_page_enabled;
    void* real_ptr;
    uint32_t real_sz;




} aco_share_stack_t;

typedef void (*aco_cofuncp_t)(void);

struct aco_s{

        void* reg[16];




    aco_t* main_co;
    void* arg;
    char is_end;

    aco_cofuncp_t fp;

    aco_save_stack_t save_stack;
    aco_share_stack_t* share_stack;
};

extern void aco_runtime_test(void);

extern void aco_thread_init(aco_cofuncp_t last_word_co_fp);

extern void* acosw(aco_t* from_co, aco_t* to_co);

extern void aco_save_fpucw_mxcsr(void* p);

extern void aco_funcp_protector_asm(void);

extern void aco_funcp_protector(void);

extern aco_share_stack_t* aco_share_stack_new(uint32_t sz);

aco_share_stack_t* aco_share_stack_new2(uint32_t sz, char guard_page_enabled);

extern void aco_share_stack_destroy(aco_share_stack_t* sstk);

extern aco_t* aco_create(
        aco_t* main_co,
        aco_share_stack_t* share_stack,
        uint32_t save_stack_sz,
        aco_cofuncp_t fp, void* arg
    );



extern __thread aco_t* aco_gtls_co_;

extern void aco_resume(aco_t* resume_co);

extern void aco_destroy(aco_t* co);















typedef struct RWLock
{



    volatile int32_t _rwctr;

} RWLock;


static inline

             _Bool

                  RWLock_TryReadLock(RWLock *self)
{


 _Bool

      result = 1;

  if ( (__sync_add_and_fetch((&self->_rwctr), 1)) <= 0 )
  {
    (__sync_sub_and_fetch((&self->_rwctr), 1));
    result = 0;
  }

  return result;
}

static inline void RWLock_ReleaseReadLock(RWLock *self)
{
  if ( (__sync_sub_and_fetch((&self->_rwctr), 1)) < 0 )
  {




            ;
  }
}

static inline

             _Bool

                  RWLock_TryWriteLock(RWLock* self)
{
  return (__sync_val_compare_and_swap((&self->_rwctr), (0), (0x80000000))) == 0;
}

static inline void RWLock_ReleaseWriteLock(RWLock *self)
{
  if ( (__sync_fetch_and_add((&self->_rwctr), 0x80000000)) != 0x80000000 )
  {



            ;
  }
}


__attribute__ ((noinline)) void _newMemRealloc(void** memP, uint32_t* capacityP, const uint32_t elementSize);

typedef struct metac_semantic_waiter_t
{
    uint32_t FuncHash;
    uint32_t NodeHash;

 aco_t* Continuation;

} metac_semantic_waiter_t;

typedef struct metac_semantic_waitlist_t
{
    RWLock WaiterLock;
    metac_semantic_waiter_t* Waiters;
    uint32_t WaiterCount;
    uint32_t WaiterCapacity;
} metac_semantic_waitlist_t;

typedef struct metac_sema_decl_state_t
{
    sema_decl_variable_t* CurrentVariables;
    uint32_t CurrentVariableSize;
    uint32_t CurrentVariableCapacity;


} metac_sema_decl_state_t;

typedef struct int16x8_t
{
    union {
      uint16_t E[8];
      uint64_t EX[2];



    };
} int16x8_t;



typedef struct metac_semantic_lru_slot_t
{
    metac_identifier_ptr_t Ptr;

    metac_node_header_t* Node;
} metac_semantic_lru_slot_t;



typedef struct metac_semantic_lru_t
{
    int16x8_t LRUContentHashes;
    metac_semantic_lru_slot_t Slots[8];
} metac_semantic_lru_t ;


typedef struct metac_semantic_state_t
{


   _Bool

        initialized;
    metac_identifier_table_t SemanticIdentifierTable;
    metac_identifier_table_t* ParserIdentifierTable;
    metac_identifier_table_t* ParserStringTable;

    metac_semantic_waitlist_t Waiters;



    metac_sema_decl_state_t* CurrentDeclarationState;

    uint32_t TemporaryScopeDepth;

    metac_scope_t* CurrentScope;

    metac_semantic_lru_t LRU;


    metac_type_table_enum_t EnumTypeTable; metac_type_table_array_t ArrayTypeTable; metac_type_table_aggregate_t StructTypeTable; metac_type_table_ptr_t PtrTypeTable; metac_type_table_aggregate_t UnionTypeTable; metac_type_table_typedef_t TypedefTypeTable; metac_type_table_functiontype_t FunctionTypeTable; metac_type_table_tuple_t TupleTypeTable;

    metac_sema_expression_t* Expressions; uint32_t Expressions_size; uint32_t Expressions_capacity; sema_decl_variable_t* Variables; uint32_t Variables_size; uint32_t Variables_capacity; sema_decl_function_t* Functions; uint32_t Functions_size; uint32_t Functions_capacity; metac_scope_t* Scopes; uint32_t Scopes_size; uint32_t Scopes_capacity; sema_stmt_block_t* BlockStatements; uint32_t BlockStatements_size; uint32_t BlockStatements_capacity; metac_sema_statement_t* Statements; uint32_t Statements_size; uint32_t Statements_capacity;

    metac_sema_expression_t* ExpressionStack;
    uint32_t ExpressionStackSize;
    uint32_t ExpressionStackCapacity;

    metac_type_aggregate_t* CompilerInterface;
} metac_semantic_state_t;




typedef struct MetaCSemantic_doTypeSemantic_Fiber_t
{
    metac_semantic_state_t* Sema;
    decl_type_t* Type;
    metac_type_index_t Result;
} MetaCSemantic_doTypeSemantic_Fiber_t;

metac_type_index_t MetaCSemantic_GetTypeIndex(metac_semantic_state_t* state,
                                              metac_type_kind_t typeKind,
                                              decl_type_t* type);

metac_type_index_t MetaCSemantic_CommonSubtype(metac_semantic_state_t* state,
                                                metac_type_index_t a, metac_type_index_t b);

metac_type_index_t MetaCSemantic_GetArrayTypeOf(metac_semantic_state_t* state,
                                                metac_type_index_t elementTypeIndex,
                                                uint32_t dimension);

metac_type_index_t MetaCSemantic_GetPtrTypeOf(metac_semantic_state_t* self,
                                              metac_type_index_t elementTypeIndex);



_Bool

    MetaCSemantic_ComputeStructLayoutPopulateScope(metac_semantic_state_t* self,
                                                    decl_type_struct_t* agg,
                                                    metac_type_aggregate_t* semaAgg);




typedef struct ticket_t
{
    uint32_t v;
} ticket_t;

typedef struct ticket_lock_t
{
    volatile uint32_t currentlyServing;
    volatile uint32_t nextTicket;




} ticket_lock_t;

typedef struct taskcontext_t
{
    uint32_t ContextCrc;

    void* ContextMem;
    uint32_t ContextMemSize;

    void* TaskMemory;
    uint32_t BytesAllocated;
    uint32_t BytesUsed;

    const char* CallerFile;
    uint32_t CallerLine;
} taskcontext_t;

typedef void (*task_fn_t)(struct task_t*);

typedef enum task_flags_t
{
    Task_Halted,

    Task_Running = (1 << 0),
    Task_Resumable = (1 << 1),
    Task_Complete = (1 << 2),
    Task_Waiting = Task_Resumable | Task_Running,

    Task_Continuation_JumpToLabel = (1 << 4),
    Task_Continuation_Task = (1 << 5),
} task_flags_t;


typedef struct task_origin_t
{
    const char* File;
    const char* Func;
    uint32_t Line;
} task_origin_t;







typedef struct task_inline_ctx_t
{
    uint8_t data[32];
} task_inline_ctx_t;

typedef struct task_t
{
    void (*TaskFunction)(struct task_t* task);
    void* Context;
    struct task_t* Parent;
    aco_t* Fiber;
    union {
        struct task_t* Continuation;
        struct {
            void* ResultPtr;
            aco_t* Caller;
        };
    };
    union {
        uint8_t _inlineContext[32];
        task_inline_ctx_t inlineContext;
    };

    task_origin_t Origin;

    volatile task_flags_t TaskFlags;

    uint16_t QueueId;
    uint16_t CompletionAttempts;

    uint16_t ContextSize;
    uint16_t ContextCapacity;

    uint32_t ChildCount;
    uint32_t ChildrenCompleted;

    struct task_t* Children;
} task_t;

typedef struct taskqueue_t
{
    ticket_lock_t QueueLock;
    uint8_t padding[sizeof(ticket_lock_t) % 16];

    uint16_t readPointer;
    uint16_t writePointer;
    uint16_t writePointerEnd;

    // task_t (*QueueMemory)[1024];

    void* ContextStorage;
    uint32_t ContextStorageCapacity;
} taskqueue_t;

typedef struct fiber_pool_t
{
    uint32_t FreeBitfield;

    aco_t MainCos[sizeof(uint32_t) * 8];
    aco_share_stack_t ShareStacks[sizeof(uint32_t) * 8];
    task_t Tasks[sizeof(uint32_t) * 8];

} fiber_pool_t;

typedef enum worker_flags_t
{
    None = 0,
    Worker_YieldOnTaskCreation = (1 << 0),

    Worker_Max = (1 << 0)
} worker_flags_t;

typedef struct worker_context_t
{
    taskqueue_t Queue;

    uint32_t WorkerId;
    uint32_t KillWorker;



    volatile uint32_t Flags;

    thrd_t Thread;
    aco_t* WorkerMain;
    fiber_pool_t* FiberPool;
} worker_context_t;




typedef struct tasksystem_t
{
    struct worker_context_t* workerContexts;
    uint32_t nWorkers;
} tasksystem_t;

void TaskSystem_Init(tasksystem_t* self, uint32_t workerThreads, void (*workerFunction)(worker_context_t* worker));


_Bool

    AddTask(task_t* task);
worker_context_t* CurrentWorker(void);
void* CurrentFiber(void);
extern task_t* CurrentTask(void);




_Bool

    TaskQueue_Push(taskqueue_t* self, task_t* taskP);





_Bool

    TaskQueue_Pull(taskqueue_t* self, task_t* taskP);

typedef struct MetaCSemantic_doExprSemantic_task_context_t
{
    metac_semantic_state_t* Sema;
    metac_expression_t* Expr;
    metac_sema_expression_t* Result;
} MetaCSemantic_doExprSemantic_task_context_t;

void MetaCSemantic_doExprSemantic_Task(task_t* task);






metac_sema_expression_t* MetaCSemantic_doExprSemantic_(metac_semantic_state_t* self,
                                                       metac_expression_t* expr,
                                                       metac_sema_expression_t* result,
                                                       const char* callFun,
                                                       uint32_t callLine);

void MetaCSemantic_PushExpr(metac_semantic_state_t* self, metac_sema_expression_t* expr);
void MetaCSemantic_PopExpr(metac_semantic_state_t* self, metac_sema_expression_t* expr);



_Bool

    MetaCSemantic_CanHaveAddress(metac_semantic_state_t* self, metac_expression_t* expr);





void RegisterType(metac_semantic_state_t* state, decl_type_t* type);
const char* TypeToChars(metac_semantic_state_t* self, metac_type_index_t typeIndex);
void MetaCSemantic_Init(metac_semantic_state_t* self,
                        metac_parser_t* parser,
                        metac_type_aggregate_t* compilerStruct);





metac_sema_expression_t* MetaCSemantic_doIndexSemantic_(metac_semantic_state_t* self,
                                                        metac_expression_t* expr,
                                                        const char* callFile,
                                                        uint32_t callLine);




metac_sema_statement_t* MetaCSemantic_doStatementSemantic_(metac_semantic_state_t* self,
                                                           metac_statement_t* stmt,
                                                           const char* callFile,
                                                           uint32_t callLine);





metac_sema_declaration_t* MetaCSemantic_doDeclSemantic_(metac_semantic_state_t* self,
                                                        metac_declaration_t* decl,
                                                        const char* callFile,
                                                        uint32_t callLine);





metac_type_index_t MetaCSemantic_doTypeSemantic_(metac_semantic_state_t* self,
                                                decl_type_t* type,
                                                const char* callFile, uint32_t callLine);



metac_node_header_t* MetaCSemantic_LookupIdentifier(metac_semantic_state_t* self,
                                                    metac_identifier_ptr_t identifierPtr);








_Bool

    Expression_IsEqual_(const metac_sema_expression_t* a,
                         const metac_sema_expression_t* b);


metac_sema_expression_t* AllocNewSemaExpression(metac_semantic_state_t* self, metac_expression_t* expr);

sema_decl_function_t* AllocNewSemaFunction(metac_semantic_state_t* self,decl_function_t* func);

sema_decl_variable_t* AllocNewSemaVariable(metac_semantic_state_t* self, decl_variable_t *decl, metac_sema_declaration_t ** result_ptr);

sema_decl_variable_t* AllocFunctionParameters(metac_semantic_state_t* self, sema_decl_function_t* func,
                                              uint32_t parameterCount);

sema_decl_type_t* AllocNewSemaType(metac_semantic_state_t* self, metac_type_index_t typeIndex);



metac_type_aggregate_t* AllocNewAggregate_(metac_semantic_state_t* self, metac_declaration_kind_t kind, uint32_t line, const char* file);

metac_type_aggregate_field_t* AllocAggregateFields(metac_semantic_state_t* self,
                                                   metac_type_aggregate_t* aggregate,
                                                   metac_declaration_kind_t kind,
                                                   uint32_t fieldCount);



metac_sema_statement_t* AllocNewSemaStatement_(metac_semantic_state_t* self,
                                               metac_statement_kind_t kind,
                                               uint32_t nodeSize, void** result_ptr);

sema_stmt_block_t* AllocNewSemaBlockStatement(metac_semantic_state_t* self,
                                              sema_stmt_block_t* Parent, uint32_t statementCount,
                                              void** result_ptr);

metac_scope_t* AllocNewScope(metac_semantic_state_t* self, metac_scope_t* parent, metac_scope_parent_t owner);
metac_type_array_t* AllocNewSemaArrayType(metac_semantic_state_t* self, metac_type_index_t elementTypeIndex, uint32_t dim);

void MetaCSemantic_Handoff(metac_semantic_state_t* self, metac_sema_declaration_t** declP,
                           metac_semantic_state_t* newOwner);



uint32_t FunctionIndex(metac_semantic_state_t* self, sema_decl_function_t* func);
uint32_t StructIndex(metac_semantic_state_t* self, metac_type_aggregate_t* struct_);
uint32_t StatementIndex_(metac_semantic_state_t* self, metac_sema_statement_t* stmt);
uint32_t BlockStatementIndex(metac_semantic_state_t* self, sema_stmt_block_t* blockstmt);
uint32_t UnionIndex(metac_semantic_state_t* self, metac_type_aggregate_t* union_);
uint32_t TypedefIndex(metac_semantic_state_t* self, metac_type_typedef_t* typedef_);
uint32_t PtrTypeIndex(metac_semantic_state_t* self, metac_type_ptr_t* ptr);
uint32_t ArrayTypeIndex(metac_semantic_state_t* self, metac_type_array_t* array);
uint32_t EnumTypeIndex(metac_sema_decl_state_t* self, metac_type_enum_t* enum_);
uint32_t FunctiontypeIndex(metac_semantic_state_t* self, metac_type_functiontype_t* functiontype);
uint32_t TupleTypeIndex(metac_semantic_state_t* self, metac_type_tuple_t* typeType);

sema_decl_function_t* FunctionPtr(metac_semantic_state_t* self, uint32_t index);
metac_type_aggregate_t* StructPtr(metac_semantic_state_t* self, uint32_t index);
metac_sema_statement_t* StatementPtr(metac_semantic_state_t* self, uint32_t index);
sema_stmt_block_t* BlockStatementPtr(metac_semantic_state_t* self, uint32_t index);
metac_type_aggregate_t* UnionPtr(metac_semantic_state_t* self, uint32_t index);
metac_type_typedef_t* TypedefPtr(metac_semantic_state_t* self, uint32_t index);
metac_type_ptr_t* PtrTypePtr(metac_semantic_state_t* self, uint32_t index);
metac_type_array_t* ArrayTypePtr(metac_semantic_state_t* self, uint32_t index);
metac_type_enum_t* EnumTypePtr(metac_semantic_state_t* self, uint32_t index);
metac_type_functiontype_t* FunctiontypePtr(metac_semantic_state_t* self, uint32_t index);
metac_type_tuple_t* TupleTypePtr(metac_semantic_state_t* self, uint32_t index);

metac_scope_t* MetaCScope_PushNewScope(metac_semantic_state_t* sema,
                                       metac_scope_t* parent,
                                       metac_scope_parent_t owner);

scope_insert_error_t MetaCSemantic_RegisterInScope(metac_semantic_state_t* self,
                                                   metac_identifier_ptr_t idPtr,
                                                   metac_node_t node);



metac_scope_t* MetaCSemantic_PushTemporaryScope_(metac_semantic_state_t* self,
                                                 metac_scope_t* tmpScope,
                                                 uint32_t line,
                                                 const char* file);




void MetaCSemantic_PopTemporaryScope_(metac_semantic_state_t* self,

                                      uint32_t line,
                                      const char* file);


static inline void PrintSemaType(metac_printer_t* self,
                                 metac_semantic_state_t* sema,
                                 metac_type_index_t typeIndex)
{
    switch(((metac_type_index_kind_t)((typeIndex).v >> 28)))
    {
        case type_index_basic:
        {
            decl_type_t basicType = {(metac_declaration_kind_t)0};
            basicType.DeclKind = decl_type;
            metac_type_kind_t Kind =
                (metac_type_kind_t) ((typeIndex).v & 0xFFFFFfF);
            basicType.TypeKind = Kind;
            PrintType(self, &basicType);
        } break;
        case type_index_struct:
        {
            uint32_t structIdx = ((typeIndex).v & 0xFFFFFfF);
            metac_identifier_ptr_t structName =
                StructPtr(sema, structIdx)->Identifier;
            PrintString(self, "struct ", sizeof("struct"));
            if (structName.v != empty_identifier.v)
            {
                PrintIdentifier(self, structName);
            }
        } break;
        case type_index_ptr:
        {

        } break;
    }
}

static inline void PrintSemaExpression(metac_printer_t* self,
                                       metac_semantic_state_t* sema,
                                       metac_sema_expression_t* semaExp)
{
    if (semaExp->Kind == exp_paren)
    {
        if (!IsBinaryExp(semaExp->E1->Kind))
            PrintChar(self, '(');

        PrintSemaExpression(self, sema, semaExp->E1);

        if (!IsBinaryExp(semaExp->E1->Kind))
            PrintChar(self, ')');
    }
    else if (semaExp->Kind == exp_tuple)
    {
        PrintChar(self, '{');
        metac_sema_expression_t* tupleElement =
            semaExp->TupleExpressions;
        for(uint32_t i = 0;
            i < semaExp->TupleExpressionCount;
            i++)
        {
            PrintSemaExpression(self, sema, tupleElement + i);
            if (i != (semaExp->TupleExpressionCount - 1))
            {
                PrintChar(self, ',');
                PrintSpace(self);
            }
        }
        PrintChar(self, '}');
    }
    else if (semaExp->Kind == exp_type)
    {
        PrintChar(self, '(');
        PrintSemaType(self, sema, semaExp->TypeExp);
        PrintChar(self, ')');
    }
    else if (semaExp->Kind == exp_identifier)
    {
        PrintIdentifier(self, semaExp->IdentifierPtr);
    }
    else if (semaExp->Kind == exp_string)
    {
        uint32_t stringLength = ( (semaExp->StringKey) >> 12 );
        PrintChar(self, '"');
        PrintString(self,
            IdentifierPtrToCharPtr(self->StringTable, semaExp->StringPtr),
            stringLength);
        PrintChar(self, '"');
    }
    else if (semaExp->Kind == exp_signed_integer)
    {
        PrintI64(self, semaExp->ValueI64);
    }
    else if (semaExp->Kind == exp_char)
    {
        PrintChar(self, '\'');
        PrintString(self, semaExp->Chars, ( (semaExp->CharKey) >> 28 ));
        PrintChar(self, '\'');
    }
    else if (IsBinaryExp(semaExp->Kind))
    {
        PrintChar(self, '(');
        PrintSemaExpression(self, sema, semaExp->E1);

        PrintSpace(self);
        const char* op = BinExpTypeToChars((metac_binary_expression_kind_t)semaExp->Kind);
        PrintString(self, op, strlen(op));
        PrintSpace(self);

        PrintSemaExpression(self, sema, semaExp->E2);
        PrintChar(self, ')');
    }
    else if (semaExp->Kind == exp_cast)
    {
        PrintString(self, "cast", 4);
        PrintChar(self, '(');
        PrintSemaType(self, sema,semaExp->CastType);
        PrintChar(self, ')');

        PrintSemaExpression(self, sema, semaExp->CastExp);
    }
    else if (semaExp->Kind == exp_call)
    {
        PrintSemaExpression(self, sema, semaExp->E1);
        PrintChar(self, '(');
        sema_exp_argument_list_t* argList =
            semaExp->E2->ArgumentList;
        const metac_sema_expression_t* onePastLastArg =
            argList->Arguments + argList->ArgumentCount;
        for(metac_sema_expression_t* arg = argList->Arguments;
            arg < onePastLastArg;
            arg++)
        {
            PrintSemaExpression(self, sema, arg);
            if (arg != (onePastLastArg - 1))
                PrintString(self, ", ", 2);
        }
        PrintChar(self, ')');
    }
    else if (semaExp->Kind == exp_index)
    {
        PrintSemaExpression(self, sema, semaExp->E1);
        PrintToken(self, tok_lBracket);
        PrintSemaExpression(self, sema, semaExp->E2);
        PrintToken(self, tok_rBracket);
    }
    else if (semaExp->Kind == exp_sizeof)
    {
        PrintKeyword(self, tok_kw_sizeof);
        PrintToken(self, tok_lParen);
        PrintSemaExpression(self, sema, semaExp->E1);
        PrintToken(self, tok_rParen);
    }
    else if (semaExp->Kind == exp_addr || semaExp->Kind == exp_ptr
          || semaExp->Kind == exp_not || semaExp->Kind == exp_compl
          || semaExp->Kind == exp_umin)
    {
        {
            const char* op = 0;
            if (semaExp->Kind == exp_addr)
                op = "&";
            else if (semaExp->Kind == exp_ptr)
                op = "*";
            else if (semaExp->Kind == exp_not)
                op = "!";
            else if (semaExp->Kind == exp_compl)
                op = "~";
            else if (semaExp->Kind == exp_umin)
                op = "-";

            PrintString(self, op, strlen(op));
        }

        if (!IsBinaryExp(semaExp->E1->Kind))
            PrintChar(self, '(');

        PrintSemaExpression(self, sema, semaExp->E1);

        if (!IsBinaryExp(semaExp->E1->Kind))
            PrintChar(self, ')');
    }
    else if (semaExp->Kind == exp_post_increment || semaExp->Kind == exp_post_decrement)
    {
        const char* op = 0;
        if (semaExp->Kind == exp_post_increment)
            op = "++";
        else if (semaExp->Kind == exp_post_decrement)
            op = "--";



                 ;

        if (!IsBinaryExp(semaExp->E1->Kind))
            PrintChar(self, '(');

        PrintSemaExpression(self, sema, semaExp->E1);

        if (!IsBinaryExp(semaExp->E1->Kind))
            PrintChar(self, ')');

        PrintString(self, op, strlen(op));
    }
    else if (semaExp->Kind == exp_inject || semaExp->Kind == exp_eject
          || semaExp->Kind == exp_typeof || semaExp->Kind == exp_assert
          || semaExp->Kind == exp_unary_dot)
    {
        {
            const char* op = 0;
            if (semaExp->Kind == exp_inject)
                op = "inject";
            else if (semaExp->Kind == exp_eject)
                op = "eject";
            else if (semaExp->Kind == exp_typeof)
                op = "typeof";
            else if (semaExp->Kind == exp_assert)
                op = "assert";
            else if (semaExp->Kind == exp_unary_dot)
                op = ".";




            PrintString(self, op, strlen(op));
        }

        if (!IsBinaryExp(semaExp->E1->Kind))
           PrintChar(self, '(');

        PrintSemaExpression(self, sema, semaExp->E1);

        if (!IsBinaryExp(semaExp->E1->Kind))
            PrintChar(self, ')');
    }
    else
    {
        printf("don't know how to print %s\n", (MetaCExpressionKind_toChars(semaExp->Kind)));
    }
}

static inline void PrintSemaVariable(metac_printer_t* self,
                                     metac_semantic_state_t* sema,
                                     sema_decl_variable_t* variable)
{
    if (((metac_type_index_kind_t)((variable->TypeIndex).v >> 28)) == type_index_functiontype)
    {
        metac_type_functiontype_t* funcType =
            FunctiontypePtr(sema, ((variable->TypeIndex).v & 0xFFFFFfF));

        PrintSemaType(self, sema, funcType->ReturnType);
        PrintSpace(self);
        PrintChar(self, '(');
        PrintChar(self, '*');
        PrintIdentifier(self, variable->VarIdentifier);
        PrintChar(self, ')');
        PrintSpace(self);
        PrintChar(self, '(');
        PrintChar(self, '{');
        const uint32_t paramCount = funcType->ParameterTypeCount;
        for(uint32_t i = 0;
            i < paramCount;
            i++)
        {
            PrintSemaType(self, sema, funcType->ParameterTypes[i]);
            if (i != paramCount - 1)
            {
                PrintChar(self, ',');
                PrintSpace(self);
            }
        }
        PrintChar(self, '}');
        PrintChar(self, ')');
    }
    else
    {
        PrintSemaType(self, sema, variable->TypeIndex);
        PrintSpace(self);
        PrintIdentifier(self, variable->VarIdentifier);
    }

    if (variable->VarInitExpression != ((void*)0x1))
    {
        PrintSpace(self);
        PrintToken(self, tok_assign);
        PrintSpace(self);
        PrintSemaExpression(self, sema, variable->VarInitExpression);
    }
}


static inline void PrintSemaDeclaration(metac_printer_t* self,
                                        metac_semantic_state_t* sema,
                                        metac_sema_declaration_t* semaDecl,
                                        uint32_t level)
{


   _Bool

        printSemicolon =

                         1

                             ;

    switch (semaDecl->DeclKind)
    {
        case decl_type_enum:
        {
            PrintIdentifier(self, semaDecl->sema_decl_type_enum.Name);
        } break;
        case decl_type_typedef:
        case decl_type:



        case decl_type_union :
        case decl_type_struct :
        {
            metac_type_aggregate_t* struct_ = (metac_type_aggregate_t*) semaDecl;
            PrintKeyword(self, AggToken(semaDecl->DeclKind));
            if (struct_->Identifier.v != empty_identifier.v)
            {
                PrintSpace(self);
                PrintIdentifier(self, struct_->Identifier);
            }
            PrintSpace(self);
            PrintToken(self, tok_lBrace);
            ++level;
            ++self->IndentLevel;
            PrintNewline(self);
            PrintIndent(self);
            metac_type_aggregate_field_t* f = struct_->Fields;
            for(uint32_t memberIndex = 0;
                memberIndex < struct_->FieldCount;
                memberIndex++)
            {

                sema_decl_variable_t synVar;
                synVar.TypeIndex = (f + memberIndex)->Type;
                synVar.VarIdentifier = (f + memberIndex)->Identifier;
                PrintSemaVariable(self, sema, &synVar);

                if (memberIndex && memberIndex != (struct_->FieldCount - 1))
                    PrintIndent(self);
            }
            --self->IndentLevel;
            --level;

            PrintIndent(self);
            PrintToken(self, tok_rBrace);
            if (self->IndentLevel)
                PrintNewline(self);
            else
                PrintSpace(self);
        } break;
        case decl_type_array:
        {



        } break;
        case decl_field :
        {




        } break;
        case decl_variable:
        {
            sema_decl_variable_t* variable = (sema_decl_variable_t*) semaDecl;
            PrintSemaVariable(self, sema, variable);
        } break;
        case decl_function:
        {
            sema_decl_function_t* function_ = (sema_decl_function_t*) semaDecl;
            metac_type_functiontype_t* functionType =
                FunctiontypePtr(sema, ((function_->TypeIndex).v & 0xFFFFFfF));

            PrintSemaType(self, sema, functionType->ReturnType);
            PrintSpace(self);
            PrintIdentifier(self, function_->Identifier);

            PrintChar(self, '(');
            const uint32_t paramCount = functionType->ParameterTypeCount;
            for(uint32_t i = 0;
                i < paramCount;
                i++
            )
            {
                PrintSemaVariable(self, sema, function_->Parameters + i);
                if (i != (paramCount - 1))
                {
                    PrintChar(self, ',');
                    PrintSpace(self);
                }
            }
            PrintChar(self, ')');
            PrintSpace(self);
            if (function_->FunctionBody != ((void*)0x1))
            {
                PrintStatement(self, (metac_statement_t*)function_->FunctionBody);
                printSemicolon =

                                0

                                     ;
            }
        } break;
    }
    if (!!printSemicolon) PrintChar(self, ';');
    PrintNewline(self);
}

const char* MetaCPrinter_PrintSemaNode(metac_printer_t* self,
                                       metac_semantic_state_t* sema,
                                       metac_node_t node)
{
    const char* result = self->StringMemory + self->StringMemorySize;
    uint32_t posBegin = self->StringMemorySize;

    if (node->Kind > node_exp_invalid && node->Kind < node_exp_max)
    {
        PrintSemaExpression(self, sema, (metac_sema_expression_t*) node);
    }
    else if (node->Kind > stmt_min && node->Kind < stmt_max)
    {

    }
    else if (node->Kind > decl_min && node->Kind < decl_max)
    {
        PrintSemaDeclaration(self, sema, (metac_sema_declaration_t*) node, 0);
    }
    else {}



    int advancement = self->StringMemorySize - posBegin;
    self->StringMemory[self->StringMemorySize++] = '\0';
    return result;
}



void MetaCPrinter_Reset(metac_printer_t* self)
{
    memset(self->StringMemory, ' ', self->StringMemorySize);

    self->StringMemorySize = 0;
    self->IndentLevel = 0;
    self->CurrentColumn = 0;
    self->StartColumn = 0;
}

void MetaCPrinter_Init(metac_printer_t* self,
                       metac_identifier_table_t* identifierTable,
                       metac_identifier_table_t* stringTable)
{
    MetaCPrinter_InitSz(self,
                        identifierTable,
                        stringTable,
                        (16 * 4096));
}

void MetaCPrinter_InitSz(metac_printer_t* self,
                         metac_identifier_table_t* identifierTable,
                         metac_identifier_table_t* stringTable,
                         uint32_t initialSize)
{
    self->StringMemoryCapacity = initialSize;
    self->StringMemory = (char*)malloc(self->StringMemoryCapacity);
    self->StringMemorySize = self->StringMemoryCapacity;

    MetaCPrinter_Reset(self);

    self->IdentifierTable = identifierTable;
    self->StringTable = stringTable;
}

void MetacPrinter_PrintStringLiteral(metac_printer_t* self, const char* str)
{
    PrintString(self, str, strlen(str));
}

void MetacPrinter_PrintI64(metac_printer_t* self, int64_t val)
{
    PrintI64(self, val);
}

const char* MetaCPrinter_PrintExpression(metac_printer_t* self, metac_expression_t* exp)
{
    const char* result = self->StringMemory + self->StringMemorySize;

    PrintExpression(self, exp);

    self->StringMemory[self->StringMemorySize++] = '\0';

    return result;
}

const char* MetaCPrinter_PrintDeclaration(metac_printer_t* self, metac_declaration_t* decl)
{
    const char* result = self->StringMemory + self->StringMemorySize;

    PrintDeclaration(self, decl, 0);

    self->StringMemory[self->StringMemorySize++] = '\0';

    return result;
}

const char* MetaCPrinter_PrintStatement(metac_printer_t* self, metac_statement_t* exp)
{
    const char* result = self->StringMemory + self->StringMemorySize;
    uint32_t posBegin = self->StringMemorySize;
    PrintStatement(self, exp);
    int advancement = self->StringMemorySize - posBegin;

    self->StringMemory[self->StringMemorySize++] = '\0';

    return result;
}

const char* MetaCPrinter_PrintNode(metac_printer_t* self, metac_node_t node, uint32_t level)
{
    const char* result = self->StringMemory + self->StringMemorySize;
    uint32_t posBegin = self->StringMemorySize;

    if (node->Kind > node_exp_invalid && node->Kind < node_exp_max)
    {
        PrintExpression(self, (metac_expression_t*) node);
    }
    else if (node->Kind > stmt_min && node->Kind < stmt_max)
    {
        PrintStatement(self, (metac_statement_t*) node);
    }
    else if (node->Kind > decl_min && node->Kind < decl_max)
    {
        PrintDeclaration(self, (metac_declaration_t*) node, level);
    }
    else
    {}



    int advancement = self->StringMemorySize - posBegin;
    self->StringMemory[self->StringMemorySize++] = '\0';
    return result;

}





void _StackArrayRealloc(void** arrayPtr, uint32_t* arrayCapacityPtr,
                        const uint32_t elementSize)
{
    const uint32_t Capacity = *arrayCapacityPtr;

    (*arrayCapacityPtr) *= 2;
    void* newMem = calloc(elementSize, *arrayCapacityPtr);
    memcpy(newMem, *arrayPtr, Capacity * elementSize);
    (*((void**)&arrayPtr)) = newMem;
}




















static const uint32_t skipFn =

                              (4294967295U)

                                        ;
static const uint32_t nodeFromName =

                                    (4294967295U)

                                               - 1;
static const uint32_t currentScope =

                                    (4294967295U)

                                               - 2;



typedef struct HeapAddr
{
    uint32_t addr;
} HeapAddr;

typedef struct StackAddr
{
    uint16_t addr;
} StackAddr;

typedef struct Imm32
{
    uint32_t imm32;


   _Bool

        signed_;
} Imm32;

typedef struct Imm64
{
    uint64_t imm64;


   _Bool

        signed_;
} Imm64;

typedef void (*ReadI32_cb_t)(uint32_t value, void* userCtx);

typedef struct ReadI32_ctx_t
{
    void* userCtx;
    ReadI32_cb_t cb;
} ReadI32_ctx_t;

typedef enum BCTypeEnum
{
    BCTypeEnum_Undef,

    BCTypeEnum_Null,
    BCTypeEnum_Void,

    BCTypeEnum_c8,
    BCTypeEnum_c16,
    BCTypeEnum_c32,


    BCTypeEnum_i8,

    BCTypeEnum_i16,

    BCTypeEnum_i32,

    BCTypeEnum_i64,

    BCTypeEnum_u8,
    BCTypeEnum_u16,
    BCTypeEnum_u32,
    BCTypeEnum_u64,

    BCTypeEnum_f23,
    BCTypeEnum_f52,
    BCTypeEnum_f106,

    BCTypeEnum_string8,
    BCTypeEnum_string16,
    BCTypeEnum_string32,

    BCTypeEnum_Function,
    BCTypeEnum_Delegate,


    BCTypeEnum_Array,
    BCTypeEnum_AArray,
    BCTypeEnum_Struct,
    BCTypeEnum_Class,
    BCTypeEnum_Ptr,
    BCTypeEnum_Slice,
} BCTypeEnum;

extern const char* BCTypeEnum_toChars(const BCTypeEnum* self);

typedef struct BCAddr
{
    uint32_t addr;
} BCAddr;

typedef enum BCTypeFlags
{
    BCTypeFlags_None = 0,
    BCTypeFlags_Const = 1 << 0,
} BCTypeFlags;

extern const char* BCTypeFlags_toChars(BCTypeFlags* self);


typedef struct BCType
{
    BCTypeEnum type;
    uint32_t typeIndex;


    BCTypeFlags flags;
} BCType;

extern const char* BCType_toChars(BCType* self);



typedef enum BCValueType
{
    BCValueType_Unknown = 0,

    BCValueType_Temporary = 1,
    BCValueType_Parameter = 2,
    BCValueType_Local = 3,

    BCValueType_StackValue = 1 << 3,
    BCValueType_Immediate = 2 << 3,
    BCValueType_HeapValue = 3 << 3,

    BCValueType_LastCond = 0xFB,
    BCValueType_Bailout = 0xFC,
    BCValueType_Exception = 0xFD,
    BCValueType_ErrorWithMessage = 0xFE,
    BCValueType_Error = 0xFF,



} BCValueType;

extern const char* BCValueType_toChars(const BCValueType* vTypePtr);

typedef struct BCHeapRef
{

    BCValueType vType;
    union
    {
        uint16_t tmpIndex;
        uint16_t localIndex;
    };

    union
    {
        HeapAddr heapAddr;
        StackAddr stackAddr;
        Imm32 imm32;
    };

    const char* name;





} BCHeapRef;


typedef struct BCValue
{

    BCValueType vType;
    BCType type;
    const char* name;
    union
    {
        int8_t parameterIndex;
        uint16_t temporaryIndex;
        uint16_t localIndex;
    };

    BCHeapRef heapRef;


   _Bool

        couldBeVoid;


    union
    {
        StackAddr stackAddr;
        HeapAddr heapAddr;
        Imm32 imm32;
        Imm64 imm64;





        void* voidStar;
    };

} BCValue;

extern void BCValue_Init(BCValue* self);
extern

        _Bool

             BCValue_eq(const BCValue* lhs, const BCValue* rhs);



typedef struct CndJmpBegin
{
    const BCAddr at;
    const BCValue* cond;
    const

         _Bool

              ifTrue;
} CndJmpBegin;

static inline

             _Bool

                  isStackAddress(uint32_t unrealPointer)
{

    return (unrealPointer & ((1 << 31) | (1 << 30) | (1 << 29) | (1 << 28) | (1 << 27) | (1 << 26))) == ((1 << 31) | (1 << 30) | (1 << 29) | (1 << 28) | (1 << 27) | (1 << 26));
}

static inline

             _Bool

                  isHeapAddress(uint32_t unrealPointer)
{

    return (unrealPointer & ((1 << 31) | (1 << 30) | (1 << 29) | (1 << 28) | (1 << 27) | (1 << 26))) != ((1 << 31) | (1 << 30) | (1 << 29) | (1 << 28) | (1 << 27) | (1 << 26));
}

static inline uint32_t toStackOffset(uint32_t unrealPointer)
{



                                        ;
    return (unrealPointer & ~((1 << 31) | (1 << 30) | (1 << 29) | (1 << 28) | (1 << 27) | (1 << 26)));
}


 static inline const uint32_t align4(const uint32_t val)
{
    return ((val + 3) & ~3);
}

 static inline uint32_t align16(const uint32_t val)
{
    return ((val + 15) & ~15);
}

extern const uint32_t BCTypeEnum_basicTypeSize(const BCTypeEnum bct);

extern

        _Bool

             BCTypeEnum_anyOf(BCTypeEnum type, const BCTypeEnum acceptedTypes[], uint32_t n_types);

extern

        _Bool

             BCType_isFloat(BCType bct);

extern

        _Bool

             BCType_isBasicBCType(BCType bct);

extern

        _Bool

             BCValue_isStackValueOrParameter(const BCValue* val);

static const int BCHeap_initHeapMax = (1 << 15);

typedef struct BCHeap
{
    uint32_t heapMax;
    uint32_t heapSize;
    uint8_t* heapData;
} BCHeap;

static const int heapSizeOffset = ((uint32_t)((char *)&((BCHeap *)0)->heapSize - (char *)0));
static const int heapMaxOffset = ((uint32_t)((char *)&((BCHeap *)0)->heapMax - (char *)0));
static const int heapDataOffset = ((uint32_t)((char *)&((BCHeap *)0)->heapData - (char *)0));

static const int heapDataPtrOffset = ((uint32_t)((char *)&((BCHeap *)0)->heapData - (char *)0)) + sizeof(uint8_t*);
static const int heapDataLengthOffset = ((uint32_t)((char *)&((BCHeap *)0)->heapData - (char *)0)) + sizeof(uint8_t*) + sizeof(void*);

typedef struct BCLabel
{
    BCAddr addr;
} BCLabel;


typedef struct BCLocal
{
    uint16_t idx;
    BCType type;
    StackAddr addr;
    const char* name;
} BCLocal;

typedef struct BCParameter
{
    uint8_t idx;
    BCType type;
    StackAddr pOffset;
    const char* name;
} BCParameter;




extern BCValue imm32_(uint32_t value,

                                       _Bool

                                            signed_);



extern BCValue imm64_(uint64_t value,

                                       _Bool

                                            signed_);

typedef struct Imm23f
{
    float imm23f;
} Imm23f;

typedef struct Imm52f
{
    double imm52f;
} Imm52f;

typedef struct BCBlock
{
    BCLabel begin;
    BCLabel end;
} BCBlock;

typedef struct BCBranch
{
    BCLabel ifTrue;
    BCLabel ifFalse;
} BCBranch;

static const BCTypeEnum smallIntegerTypes[] = {BCTypeEnum_u16, BCTypeEnum_u8,
                                      BCTypeEnum_i16, BCTypeEnum_i8,
                                      BCTypeEnum_c32, BCTypeEnum_c16, BCTypeEnum_c8};


extern BCTypeEnum BCTypeEnum_commonTypeEnum(BCTypeEnum lhs, BCTypeEnum rhs);


static const BCType BCType_i32 = {BCTypeEnum_i32};
static const BCType BCType_u32 = {BCTypeEnum_u32};












typedef void (*Initialize_t) (void* ctx, uint32_t n_args, ...);
typedef void (*InitializeV_t) (void* ctx, uint32_t n_args, va_list args);
typedef void (*Finalize_t) (void* ctx);

typedef uint32_t (*beginFunction_t) (void* ctx, uint32_t fnId, const void* fd);
typedef void* (*endFunction_t) (void* ctx, uint32_t fnIdx);

typedef BCValue (*genTemporary_t) (void* ctx, BCType bct);
typedef void (*destroyTemporary_t) (void* ctx, BCValue* tmp);

typedef BCValue (*genLocal_t) (void* ctx, BCType bct, const char* name);
typedef BCValue (*genParameter_t) (void* ctx, BCType bct, const char* name);
typedef void (*emitFlag_t) (void* ctx, BCValue* lhs);

typedef void (*Alloc_t) (void* ctx, BCValue *heapPtr, const BCValue* size);
typedef void (*Assert_t) (void* ctx, const BCValue* value, const BCValue* err);
typedef void (*MemCpy_t) (void* ctx, const BCValue* dst, const BCValue* src, const BCValue* size);

typedef void (*File_t) (void* ctx, const char* filename);
typedef void (*Line_t) (void* ctx, uint32_t line);
typedef void (*Comment_t) (void* ctx, const char* comment);
typedef void (*Prt_t) (void* ctx, const BCValue* value,

                                                       _Bool

                                                            isString);

typedef void (*Set_t) (void* ctx, BCValue *lhs, const BCValue* rhs);
typedef void (*Ult3_t) (void* ctx, BCValue *result, const BCValue* lhs, const BCValue* rhs);
typedef void (*Ule3_t) (void* ctx, BCValue *result, const BCValue* lhs, const BCValue* rhs);
typedef void (*Lt3_t) (void* ctx, BCValue *result, const BCValue* lhs, const BCValue* rhs);
typedef void (*Le3_t) (void* ctx, BCValue *result, const BCValue* lhs, const BCValue* rhs);
typedef void (*Ugt3_t) (void* ctx, BCValue *result, const BCValue* lhs, const BCValue* rhs);
typedef void (*Uge3_t) (void* ctx, BCValue *result, const BCValue* lhs, const BCValue* rhs);
typedef void (*Gt3_t) (void* ctx, BCValue *result, const BCValue* lhs, const BCValue* rhs);
typedef void (*Ge3_t) (void* ctx, BCValue *result, const BCValue* lhs, const BCValue* rhs);
typedef void (*Eq3_t) (void* ctx, BCValue *result, const BCValue* lhs, const BCValue* rhs);
typedef void (*Neq3_t) (void* ctx, BCValue *result, const BCValue* lhs, const BCValue* rhs);
typedef void (*Add3_t) (void* ctx, BCValue *result, const BCValue* lhs, const BCValue* rhs);
typedef void (*Sub3_t) (void* ctx, BCValue *result, const BCValue* lhs, const BCValue* rhs);
typedef void (*Mul3_t) (void* ctx, BCValue *result, const BCValue* lhs, const BCValue* rhs);
typedef void (*Div3_t) (void* ctx, BCValue *result, const BCValue* lhs, const BCValue* rhs);
typedef void (*Udiv3_t) (void* ctx, BCValue *result, const BCValue* lhs, const BCValue* rhs);
typedef void (*And3_t) (void* ctx, BCValue *result, const BCValue* lhs, const BCValue* rhs);
typedef void (*Or3_t) (void* ctx, BCValue *result, const BCValue* lhs, const BCValue* rhs);
typedef void (*Xor3_t) (void* ctx, BCValue *result, const BCValue* lhs, const BCValue* rhs);
typedef void (*Lsh3_t) (void* ctx, BCValue *result, const BCValue* lhs, const BCValue* rhs);
typedef void (*Rsh3_t) (void* ctx, BCValue *result, const BCValue* lhs, const BCValue* rhs);
typedef void (*Mod3_t) (void* ctx, BCValue *result, const BCValue* lhs, const BCValue* rhs);
typedef void (*Umod3_t) (void* ctx, BCValue *result, const BCValue* lhs, const BCValue* rhs);
typedef void (*Not_t) (void* ctx, BCValue *result, const BCValue* val);

typedef void (*LoadFramePointer_t) (void* ctx, BCValue *result, const int32_t offset);

typedef void (*Call_t) (void* ctx, BCValue *result, const BCValue* fn, const BCValue* args, uint32_t n_args);
typedef BCLabel (*genLabel_t) (void* ctx);
typedef void (*Jmp_t) (void* ctx, BCLabel target);
typedef uint32_t (*beginJmp_t) (void* ctx);
typedef void (*endJmp_t) (void* ctx, BCAddr atIp, BCLabel target);
typedef CndJmpBegin (*beginCndJmp_t) (void* ctx, const BCValue* cond, _Bool ifTrue);
typedef void (*endCndJmp_t) (void* ctx, const CndJmpBegin *jmp, BCLabel target);

typedef void (*Load8_t) (void* ctx, BCValue *dest, const BCValue* from);
typedef void (*Store8_t) (void* ctx, BCValue *dest, const BCValue* value);
typedef void (*Load16_t) (void* ctx, BCValue *dest, const BCValue* from);
typedef void (*Store16_t) (void* ctx, BCValue *dest, const BCValue* value);
typedef void (*Load32_t) (void* ctx, BCValue *dest, const BCValue* from);
typedef void (*Store32_t) (void* ctx, BCValue *dest, const BCValue* value);
typedef void (*Load64_t) (void* ctx, BCValue *dest, const BCValue* from);
typedef void (*Store64_t) (void* ctx, BCValue *dest, const BCValue* value);

typedef void (*Throw_t) (void* ctx, const BCValue* e);
typedef void (*PushCatch_t) (void* ctx);
typedef void (*PopCatch_t) (void* ctx);
typedef void (*Ret_t) (void* ctx, const BCValue* val);

typedef void (*IToF32_t) (void* ctx, BCValue *result, const BCValue* rhs);
typedef void (*IToF64_t) (void* ctx, BCValue *result, const BCValue* rhs);
typedef void (*F32ToI_t) (void* ctx, BCValue *result, const BCValue* rhs);
typedef void (*F64ToI_t) (void* ctx, BCValue *result, const BCValue* rhs);
typedef void (*F32ToF64_t) (void* ctx, BCValue *result, const BCValue* rhs);
typedef void (*F64ToF32_t) (void* ctx, BCValue *result, const BCValue* rhs);

typedef void (*Memcmp_t) (void* ctx, BCValue *result, const BCValue* lhs, const BCValue* rhs);
typedef void (*Realloc_t) (void* ctx, BCValue *result, const BCValue* lhs, const BCValue* rhs, const uint32_t size);

typedef BCValue (*run_t) (void* ctx, uint32_t fnIdx, const BCValue* args, uint32_t n_args);
typedef void (*destroy_instance_t) (void* ctx);
typedef void (*new_instance_t) (void ** result_p);
typedef uint32_t (*sizeof_instance_t) (void);

typedef void (*ReadI32_t) (void* ctx, const BCValue* val, const ReadI32_cb_t readCb, void* userCtx);
typedef void (*ReadI32_cb_t)(uint32_t value, void* userCtx);

typedef struct BackendInterface
{
    const char* name;

    void (*const Initialize) (void* ctx, uint32_t n_args, ...);
    void (*const InitializeV) (void* ctx, uint32_t n_args, va_list args);
    void (*const Finalize) (void* ctx);

    uint32_t (*const beginFunction) (void* ctx, uint32_t fnId, const void* fd);
    void* (*const endFunction) (void* ctx, uint32_t fnIdx);

    BCValue (*const genTemporary) (void* ctx, BCType bct);
    void (*const destroyTemporary) (void* ctx, BCValue* tmp);

    BCValue (*const genLocal) (void* ctx, BCType bct, const char* name);
    BCValue (*const genParameter) (void* ctx, BCType bct, const char* name);
    void (*const emitFlag) (void* ctx, BCValue* lhs);

    void (*const Alloc) (void* ctx, BCValue *heapPtr, const BCValue* size);
    void (*const Assert) (void* ctx, const BCValue* value, const BCValue* err);
    void (*const MemCpy) (void* ctx, const BCValue* dst, const BCValue* src, const BCValue* size);

    void (*const File) (void* ctx, const char* filename);
    void (*const Line) (void* ctx, uint32_t line);
    void (*const Comment) (void* ctx, const char* comment);
    void (*const Prt) (void* ctx, const BCValue* value,

                                                       _Bool

                                                            isString);

    void (*const Set) (void* ctx, BCValue *lhs, const BCValue* rhs);
    void (*const Ult3) (void* ctx, BCValue *result, const BCValue* lhs, const BCValue* rhs);
    void (*const Ule3) (void* ctx, BCValue *result, const BCValue* lhs, const BCValue* rhs);
    void (*const Lt3) (void* ctx, BCValue *result, const BCValue* lhs, const BCValue* rhs);
    void (*const Le3) (void* ctx, BCValue *result, const BCValue* lhs, const BCValue* rhs);
    void (*const Ugt3) (void* ctx, BCValue *result, const BCValue* lhs, const BCValue* rhs);
    void (*const Uge3) (void* ctx, BCValue *result, const BCValue* lhs, const BCValue* rhs);
    void (*const Gt3) (void* ctx, BCValue *result, const BCValue* lhs, const BCValue* rhs);
    void (*const Ge3) (void* ctx, BCValue *result, const BCValue* lhs, const BCValue* rhs);
    void (*const Eq3) (void* ctx, BCValue *result, const BCValue* lhs, const BCValue* rhs);
    void (*const Neq3) (void* ctx, BCValue *result, const BCValue* lhs, const BCValue* rhs);
    void (*const Add3) (void* ctx, BCValue *result, const BCValue* lhs, const BCValue* rhs);
    void (*const Sub3) (void* ctx, BCValue *result, const BCValue* lhs, const BCValue* rhs);
    void (*const Mul3) (void* ctx, BCValue *result, const BCValue* lhs, const BCValue* rhs);
    void (*const Div3) (void* ctx, BCValue *result, const BCValue* lhs, const BCValue* rhs);
    void (*const Udiv3) (void* ctx, BCValue *result, const BCValue* lhs, const BCValue* rhs);
    void (*const And3) (void* ctx, BCValue *result, const BCValue* lhs, const BCValue* rhs);
    void (*const Or3) (void* ctx, BCValue *result, const BCValue* lhs, const BCValue* rhs);
    void (*const Xor3) (void* ctx, BCValue *result, const BCValue* lhs, const BCValue* rhs);
    void (*const Lsh3) (void* ctx, BCValue *result, const BCValue* lhs, const BCValue* rhs);
    void (*const Rsh3) (void* ctx, BCValue *result, const BCValue* lhs, const BCValue* rhs);
    void (*const Mod3) (void* ctx, BCValue *result, const BCValue* lhs, const BCValue* rhs);
    void (*const Umod3) (void* ctx, BCValue *result, const BCValue* lhs, const BCValue* rhs);
    void (*const Not) (void* ctx, BCValue *result, const BCValue* val);

    void (*const LoadFramePointer) (void* ctx, BCValue *result, const int32_t offset);

    void (*const Call) (void* ctx, BCValue *result, const BCValue* fn, const BCValue* args, uint32_t n_args);
    BCLabel (*const genLabel) (void* ctx);
    void (*const Jmp) (void* ctx, BCLabel target);
    uint32_t (*const beginJmp) (void* ctx);
    void (*const endJmp) (void* ctx, BCAddr atIp, BCLabel target);
    CndJmpBegin (*const beginCndJmp) (void* ctx, const BCValue* cond,

                                                                     _Bool

                                                                          ifTrue);
    void (*const endCndJmp) (void* ctx, const CndJmpBegin *jmp, BCLabel target);

    void (*const Load8) (void* ctx, BCValue *dest, const BCValue* from);
    void (*const Store8) (void* ctx, BCValue *dest, const BCValue* value);
    void (*const Load16) (void* ctx, BCValue *dest, const BCValue* from);
    void (*const Store16) (void* ctx, BCValue *dest, const BCValue* value);
    void (*const Load32) (void* ctx, BCValue *dest, const BCValue* from);
    void (*const Store32) (void* ctx, BCValue *dest, const BCValue* value);
    void (*const Load64) (void* ctx, BCValue *dest, const BCValue* from);
    void (*const Store64) (void* ctx, BCValue *dest, const BCValue* value);

    void (*const Throw) (void* ctx, const BCValue* e);
    void (*const PushCatch) (void* ctx);
    void (*const PopCatch) (void* ctx);
    void (*const Ret) (void* ctx, const BCValue* val);

    void (*const IToF32) (void* ctx, BCValue *result, const BCValue* rhs);
    void (*const IToF64) (void* ctx, BCValue *result, const BCValue* rhs);
    void (*const F32ToI) (void* ctx, BCValue *result, const BCValue* rhs);
    void (*const F64ToI) (void* ctx, BCValue *result, const BCValue* rhs);
    void (*const F32ToF64) (void* ctx, BCValue *result, const BCValue* rhs);
    void (*const F64ToF32) (void* ctx, BCValue *result, const BCValue* rhs);

    void (*const Memcmp) (void* ctx, BCValue *result, const BCValue* lhs, const BCValue* rhs);
    void (*const Realloc) (void* ctx, BCValue *result, const BCValue* lhs, const BCValue* rhs, const uint32_t size);

    BCValue (*const run) (void* ctx, uint32_t fnIdx, const BCValue* args, uint32_t n_args);
    void (*const destroy_instance) (void* ctx);
    void (*const new_instance) (void ** result_p);
    uint32_t (*const sizeof_instance) (void);

    void (*const ReadI32) (void* ctx, const BCValue* val, const ReadI32_cb_t readCb, void* userCtx);
} BackendInterface;







uint32_t MetaCPreProcessor_PushDefine(metac_preprocessor_t* self,
                                      metac_preprocessor_define_t* define,
                                      metac_token_t_array_array parameters)
{
    uint32_t result = 0;

    metac_token_t _tokens[64] = {}; metac_token_t_array tokens = { _tokens, 0, 64 };;

    if (parameters.Count != define->ParameterCount &&
        ((define->ParameterCount >= parameters.Count) && !define->IsVariadic))
    {
        printf("Macro takes %u arguments but %u are given\n",
              define->ParameterCount, parameters.Count);
        goto Lret;
    }




    for(uint32_t tokIdx = 0;
        tokIdx < define->TokenCount;
        tokIdx++)
    {
        metac_token_t tok =
            self->DefineTable.TokenMemory[define->TokensOffset + tokIdx];

        printf("Seeing tok: %s\n", MetaCTokenEnum_toChars(tok.TokenType));

        if (tok.TokenType == tok_macro_parameter)
        {
            metac_token_t_array parameter =
                parameters.Ptr[tok.MacroParameterIndex];

            for(uint32_t paramTokIdx = 0;
                paramTokIdx < parameter.Count;
                paramTokIdx++)
            {
                tok = parameter.Ptr[paramTokIdx];

                if (tokens.Count >= tokens.Capacity) { if ((sizeof(_tokens) / sizeof(_tokens[0])) == tokens.Capacity) _StackArrayRealloc(((void**)&tokens.Ptr), &tokens.Capacity, sizeof(_tokens[0])); else _newMemRealloc(((void**)&tokens.Ptr), &tokens.Capacity, sizeof(_tokens[0])); } tokens.Ptr[tokens.Count++] = tok;;
            }
        }
        else if (tok.TokenType == tok_identifier
              && tok.IdentifierKey == 0xbc18fc)
        {
            metac_token_t_array va_args = (*(parameters.Ptr + parameters.Count - 1));
            printf("saw __VA_ARGS__\n");
            for(uint32_t va_args_idx = 0;
                va_args_idx < va_args.Count;
                va_args_idx)
            {

                if (tokens.Count >= tokens.Capacity) { if ((sizeof(_tokens) / sizeof(_tokens[0])) == tokens.Capacity) _StackArrayRealloc(((void**)&tokens.Ptr), &tokens.Capacity, sizeof(_tokens[0])); else _newMemRealloc(((void**)&tokens.Ptr), &tokens.Capacity, sizeof(_tokens[0])); } tokens.Ptr[tokens.Count++] = va_args.Ptr[va_args_idx];;
            }
            continue;
        }
        else
        {

            if (tokens.Count >= tokens.Capacity) { if ((sizeof(_tokens) / sizeof(_tokens[0])) == tokens.Capacity) _StackArrayRealloc(((void**)&tokens.Ptr), &tokens.Capacity, sizeof(_tokens[0])); else _newMemRealloc(((void**)&tokens.Ptr), &tokens.Capacity, sizeof(_tokens[0])); } tokens.Ptr[tokens.Count++] = tok;;
        }
    }

    if (tokens.Count < (sizeof(_tokens) / sizeof(_tokens[0]))) { void* ptr = malloc(sizeof(_tokens[0]) * tokens.Count); memcpy(ptr, tokens.Ptr, sizeof(_tokens[0]) * tokens.Count); (*(void**)&tokens.Ptr) = ptr; tokens.Capacity = tokens.Count; };

Lret:
    self->DefineTokenStack[self->DefineTokenStackCount++] = tokens;
    printf("PushedDefine: [%u]\n", self->DefineTokenStackCount);
    return result;
}

metac_expression_t* MetaCPreProcessor_ResolveDefineToExp(metac_preprocessor_t* self,
                                                         metac_preprocessor_define_ptr_t definePtr,
                                                         metac_token_t_array_array parameters)
{


    metac_expression_t* result = 0;

    return result;
}

void MetaCPreprocessor_DefineTable_Init(metac_define_table_t* self, metac_preprocessor_t* preproc)
{
    self->DefineMemorySize = 0;
    self->DefineMemoryCapacity = 256;
    self->DefineMemory = (metac_preprocessor_define_t*)
        calloc(sizeof(metac_preprocessor_define_t), self->DefineMemoryCapacity);
    self->LengthShift = 20;

    self->SlotCount_Log2 = 9;
    self->Slots = (metac_define_table_slot_t*)
        calloc(sizeof(metac_define_table_slot_t), 1 << self->SlotCount_Log2);

    self->TokenMemoryCapacity = 1024;
    self->TokenMemorySize = 0;
    self->TokenMemory = (metac_token_t*)
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



                                        ;
            metac_preprocessor_define_t def = table->DefineMemory[slot.DefinePtr.v - 4];

            const char* defineIdChars =
                IdentifierPtrToCharPtr(&table->Preproc->DefineIdentifierTable, def.DefineName);



           _Bool

                matches =
                !memcmp(defineIdChars, idChars,
                        ( (key) >> 20 ));
            if (matches)
            {
                result = slot.DefinePtr;
                goto Lret;
            }
        }
    }



Lret:
    return result;
}

static inline metac_token_t_array ResolveDefine()
{
    metac_token_t_array Result = {0};
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
                return

                      1

                          ;
        } else if (op == exp_andand)
        {
            if (!e1)
                return

                      0

                           ;
        }
        e2 = MetaCPreProcessor_EvalExp(self, e->E2, parser);
    }


    printf("op: %s\n", MetaCExpressionKind_toChars(op));
    metac_identifier_ptr_t definedIdPtr;
    switch(op)
    {
        default : {
            fprintf(

                   stderr

                         ,
                "Evaluator doesn't know how to eval: %s\n",
                MetaCExpressionKind_toChars(e->Kind)
            );


        } break;
        case exp_string:
        {



        }


        case exp_signed_integer:
        {
            result = (int32_t)(e->ValueU64);
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
            const char* identifierChars =
                IdentifierPtrToCharPtr(&parser->IdentifierTable, e->IdentifierPtr);
            metac_preprocessor_define_ptr_t definePtr;
            definePtr =
                MetaCPreProcessor_GetDefine(self, e->IdentifierKey, identifierChars);
            metac_expression_t* resolved = 0;
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
        case exp_variable:
        {



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


        } break;

        case exp_call:
        {
            if (e->E1->Kind == exp_identifier)
            {
                if (e->E1->IdentifierKey == 0x7d9260)
                {
                    exp_argument_t* args = (exp_argument_t*)e->E2;
                    metac_expression_t* e2 = args->Expression;

                    if (args->Next != ((void*)0x1) || e2->Kind != exp_identifier)
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
                        uint32_t key = ( ((uint32_t)(definedHash & 0xFFFFF)) | (((uint32_t)(length)) << 20) );

                        result = IsDefineInTable(&self->DefineTable, key, definedChars).v != 0;
                    }
                }
            }
            else
            {
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

    MetaCPreprocessor_DefineTable_Init(&self->DefineTable, self);
    IdentifierTable_Init(&self->DefineIdentifierTable, 20, 7);

    IdentifierTable_Init(&self->IdentifierTable, 20, 8);
    IdentifierTable_Init(&self->StringTable, 12, 7);

    self->DefineTokenStackCount = 0;
    self->Parent = 0;

    self->TokenMemoryCapacity = 256;
    self->TokenMemorySize = 0;
    self->TokenMemory = (metac_token_t*)
        calloc(sizeof(metac_token_t), self->TokenMemoryCapacity);

    printf("Initialized preproc\n");
}




void _StackArrayRealloc(void** arrayPtr, uint32_t* arrayCapacityPtr,
                        const uint32_t elementSize);


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





            uint32_t TokenArraryOffset = (table->TokenMemorySize += tokens.Count, table->TokenMemorySize - tokens.Count);
            memcpy((uint8_t*)table->TokenMemory + TokenArraryOffset,
                   (void*)tokens.Ptr,
                   tokens.Count * sizeof(*tokens.Ptr));
            define.TokensOffset = TokenArraryOffset;

            uint32_t defineIdx = (table->DefineMemorySize++);
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

metac_preprocessor_define_ptr_t
MetaCPreProcessor_ParseDefine(metac_preprocessor_t *self, metac_parser_t* parser)
{
    metac_token_t* defineName = (MetaCParser_Match_((parser), (tok_identifier), "../metac_preproc.c", 501));


   _Bool

        isMacro = MetaCParser_PeekMatch(parser, tok_lParen, 1);



   _Bool

        isVariadic =

                     0

                          ;



   _Bool

        hasPaste =

                   0

                        ;

    metac_identifier_ptr_t _macroParameter[16] = {}; metac_identifier_ptr_t_array macroParameter = { _macroParameter, 0, 16 };;

    metac_token_t _defineBodyTokens[64] = {}; metac_token_t_array defineBodyTokens = { _defineBodyTokens, 0, 64 };;

    if (isMacro)
    {
        (MetaCParser_Match_((parser), (tok_lParen), "../metac_preproc.c", 514));
        while(MetaCParser_PeekMatch(parser, tok_identifier, 1))
        {

            if (macroParameter.Count >= macroParameter.Capacity) { if ((sizeof(_macroParameter) / sizeof(_macroParameter[0])) == macroParameter.Capacity) _StackArrayRealloc(((void**)&macroParameter.Ptr), &macroParameter.Capacity, sizeof(_macroParameter[0])); else _newMemRealloc(((void**)&macroParameter.Ptr), &macroParameter.Capacity, sizeof(_macroParameter[0])); } macroParameter.Ptr[macroParameter.Count++] = (MetaCParser_Match_((parser), (tok_identifier), "../metac_preproc.c", 519))->IdentifierPtr;
                                                                         ;

            if (MetaCParser_PeekMatch(parser, tok_comma, 1))
            {
                (MetaCParser_Match_((parser), (tok_comma), "../metac_preproc.c", 523));
            }
        }
        if (MetaCParser_PeekMatch(parser, tok_dotdotdot, 1))
        {
            isVariadic |=

                         1

                             ;
            (MetaCParser_Match_((parser), (tok_dotdotdot), "../metac_preproc.c", 529));
        }
        (MetaCParser_Match_((parser), (tok_rParen), "../metac_preproc.c", 531));

    }

    uint32_t definedNameLength = defineName->IdentifierKey >> 20;

    const char* defineNameChars =
        IdentifierPtrToCharPtr(&parser->Lexer->IdentifierTable,
                               defineName->IdentifierPtr);

    printf("define %s with %u parameters\n", defineNameChars,
                                             macroParameter.Count);
    MetaCPrinter_Reset(&parser->DebugPrinter);

    uint32_t hash = crc32c(~0, defineNameChars, definedNameLength);
    uint32_t defineKey = ( ((uint32_t)(hash & 0xFFFFF)) | (((uint32_t)(definedNameLength)) << 20) );

    metac_identifier_ptr_t defineIdPtr =
        GetOrAddIdentifier(&self->DefineIdentifierTable, defineKey, defineNameChars);

    metac_token_t* currentToken;
    uint32_t peek1 = 1;

    for(;;)
    {
        const char* s = 0;

        currentToken = (MetaCParser_PeekToken_(parser, peek1++, 558));
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
                    if (defineBodyTokens.Count >= defineBodyTokens.Capacity) { if ((sizeof(_defineBodyTokens) / sizeof(_defineBodyTokens[0])) == defineBodyTokens.Capacity) _StackArrayRealloc(((void**)&defineBodyTokens.Ptr), &defineBodyTokens.Capacity, sizeof(_defineBodyTokens[0])); else _newMemRealloc(((void**)&defineBodyTokens.Ptr), &defineBodyTokens.Capacity, sizeof(_defineBodyTokens[0])); } defineBodyTokens.Ptr[defineBodyTokens.Count++] = tok;;
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
            uint32_t length = ( (currentToken->StringKey) >> 12 );

            GetOrAddIdentifier(&self->StringTable, ( (uint32_t)((hash) & 0xFFF) | (((uint32_t)(length)) << 12) ), stringChars);
            goto LaddToken;
        }
        else
        {
            if (currentToken->TokenType == tok_hashhash)
                hasPaste |= 1;
        LaddToken:
            if (defineBodyTokens.Count >= defineBodyTokens.Capacity) { if ((sizeof(_defineBodyTokens) / sizeof(_defineBodyTokens[0])) == defineBodyTokens.Capacity) _StackArrayRealloc(((void**)&defineBodyTokens.Ptr), &defineBodyTokens.Capacity, sizeof(_defineBodyTokens[0])); else _newMemRealloc(((void**)&defineBodyTokens.Ptr), &defineBodyTokens.Capacity, sizeof(_defineBodyTokens[0])); } defineBodyTokens.Ptr[defineBodyTokens.Count++] = *currentToken;;
        }
        MetaCPrinter_Reset(&parser->DebugPrinter);
    }

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




    uint32_t HashOffsets[512];
    uint32_t HashOffsetsCount = 0;
    uint32_t HashOffsetsCapacity = (sizeof(HashOffsets) / sizeof(HashOffsets[0]));

    {
        uint32_t lastNewlinePos = 0;
        uint32_t scanPos = 0;
        uint32_t scanLeft = fileBuffer.Length;

        for(;;) {
            const char* result =
                (const char*) memchr(fileBuffer.Data + lastNewlinePos, '#', scanLeft);
            if (result != 0)
            {




                uint32_t hashOffset = (uint32_t)(result - fileBuffer.Data);
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

uint32_t MetaCPreProcessor_Eval(metac_preprocessor_t* self, struct metac_parser_t* parser)
{
    uint32_t result = 0;

    for(;;)
    {
        metac_token_t* tok =
            (MetaCParser_PeekToken_(parser, 1, 683));
        if (!tok || tok->TokenType == tok_eof)
            return result;

        metac_expression_t* exp = MetaCParser_ParseExpression(parser, expr_flags_pp, 0);
        MetaCPrinter_Reset(&parser->DebugPrinter);
        const char* exp_string = MetaCPrinter_PrintExpression(&parser->DebugPrinter, exp);
        printf("#eval '%s'\n", exp_string);

        return MetaCPreProcessor_EvalExp(self, exp, parser);

     }

}







uint32_t crc32c_nozero(uint32_t crc, const void* s, const uint32_t len_p);
uint32_t crc32c_byte(uint32_t crc, uint8_t byte);








static inline

             _Bool

                  MetaCNode_IsExpression(metac_node_t node)
{
    return (node->Kind > node_exp_invalid && node->Kind < node_exp_max);
}

static inline

             _Bool

                  MetaCNode_IsStatement(metac_node_t node)
{
    return (node->Kind > stmt_min && node->Kind < stmt_max);
}

static inline

             _Bool

                  MetaCNode_IsDeclaration(metac_node_t node)
{
    return (node->Kind > decl_min && node->Kind < decl_max);
}






int MetaCNode_TreeWalk_Real(metac_node_t node, walker_function_t walker_fn, void* ctx)
{
    int result;





    if (node == ((metac_node_t) 0x1))
    {
        return -1;
    }

    if (result = walker_fn((metac_node_t)node, ctx))
    {
        return result;
    }

    switch(node->Kind)
    {
        default:



        case node_decl_type_tuple:



        case node_decl_variable:
        {
            decl_variable_t* decl_variable = (decl_variable_t*) node;
            if ((metac_node_t)decl_variable->VarType != ((metac_node_t) 0x1))
                result = walker_fn((metac_node_t)decl_variable->VarType, ctx);
            if(result)
                 return result;
            if ((metac_node_t)decl_variable->VarInitExpression != ((metac_node_t) 0x1))
                result = walker_fn((metac_node_t)decl_variable->VarInitExpression, ctx);
            if(result)
                 return result;
        } break;

        case node_decl_field:
        {
            decl_field_t* field = (decl_field_t*) node;
            while(((metac_node_t)field) != ((metac_node_t) 0x1))
            {
                result = MetaCNode_TreeWalk_Real((metac_node_t)field->Field, walker_fn, ctx);
                if (result)
                    return result;
                field = field->Next;
            }
        } break;

        case node_decl_parameter:
        {
            decl_parameter_t* parameter = (decl_parameter_t*) node;
            result = MetaCNode_TreeWalk_Real((metac_node_t)parameter->Parameter, walker_fn, ctx);
            if (result)
                return result;

            while(((metac_node_t)parameter->Next) != ((metac_node_t) 0x1))
            {
                result = MetaCNode_TreeWalk_Real((metac_node_t)parameter->Next->Parameter, walker_fn, ctx);
                if (result)
                    return result;
                parameter = parameter->Next;
            }
        } break;

        case node_decl_enum_member:
        {
            decl_enum_member_t* enum_member = (decl_enum_member_t*) node;
            while((metac_node_t)(enum_member->Next) != ((metac_node_t) 0x1))
            {
                result = walker_fn((metac_node_t)enum_member->Next, ctx);
                if (result)
                    return result;
                enum_member = enum_member->Next;
            }
        } break;
        case node_decl_type:
        {
            decl_type_t* type = (decl_type_t*) node;
            if (result)
                return result;
        } break;
        case node_decl_type_struct:
        {
            decl_type_struct_t* type_struct = (decl_type_struct_t*) node;
            if (type_struct->Fields && (metac_node_t)type_struct->Fields != ((metac_node_t) 0x1))
                result = MetaCNode_TreeWalk_Real((metac_node_t)type_struct->Fields, walker_fn, ctx);
            if (result)
                return result;
        } break;
        case node_decl_type_union:
        {
            decl_type_union_t* type_union = (decl_type_union_t*) node;
            result = MetaCNode_TreeWalk_Real((metac_node_t)type_union->Fields, walker_fn, ctx);
            if (result)
                return result;
        } break;
        case node_decl_type_enum:
        {
            decl_type_enum_t* type_enum = (decl_type_enum_t*) node;
            result = MetaCNode_TreeWalk_Real((metac_node_t)type_enum->Members, walker_fn, ctx);
            if (result)
                return result;
        } break;
        case node_decl_type_array:
        {
            decl_type_array_t* type_array = (decl_type_array_t*) node;
            result = MetaCNode_TreeWalk_Real((metac_node_t)type_array->ElementType, walker_fn, ctx);
            if (result)
                return result;
            result = walker_fn((metac_node_t)type_array->Dim, ctx);
            if (result)
                return result;
        } break;
        case node_decl_type_ptr:
        {
            decl_type_ptr_t* type_ptr = (decl_type_ptr_t*) node;
            result = MetaCNode_TreeWalk_Real((metac_node_t)type_ptr->ElementType, walker_fn, ctx);
            if (result)
                return result;
        } break;
        case node_decl_type_functiontype:
        {
            decl_type_functiontype_t* type_functiontype = (decl_type_functiontype_t*) node;
            result = MetaCNode_TreeWalk_Real((metac_node_t)type_functiontype->ReturnType, walker_fn, ctx);
            if (result)
                return result;
            result = MetaCNode_TreeWalk_Real((metac_node_t)type_functiontype->Parameters, walker_fn, ctx);
            if (result)
                return result;
        } break;
        case node_decl_type_typedef:
        {
            decl_type_typedef_t* typedef_ = (decl_type_typedef_t*) node;
            result = MetaCNode_TreeWalk_Real((metac_node_t)typedef_->Type, walker_fn, ctx);
            if (result)
                return result;
        } break;
        case node_decl_function:
        {
            decl_function_t* decl_function = (decl_function_t*) node;
            if ((metac_node_t)decl_function->ReturnType != ((metac_node_t) 0x1))
                result = walker_fn((metac_node_t)decl_function->ReturnType, ctx);
            if(result)
                 return result;
            if ((metac_node_t)decl_function->Parameters != ((metac_node_t) 0x1))
                result = walker_fn((metac_node_t)decl_function->Parameters, ctx);
            if(result)
                 return result;
            if ((metac_node_t)decl_function->FunctionBody != ((metac_node_t) 0x1))
                result = walker_fn((metac_node_t)decl_function->FunctionBody, ctx);
            if(result)
                 return result;
        } break;

        case node_decl_label:
        case node_decl_comment:
            break;

        case node_stmt_block:
        {
            stmt_block_t* stmt_block = (stmt_block_t*) node;
            const uint32_t statementCount = stmt_block->StatementCount;
            metac_statement_t* firstStatement = stmt_block->Body;
            for(uint32_t i = 0; i < statementCount; i++)
            {
                metac_statement_t* stmt = firstStatement + i;
                result = walker_fn((metac_node_t)stmt, ctx);
                if (result)
                    break;
            }
            if (result)
                return result;
        }
        break;

        case node_stmt_break:
        case node_stmt_continue:
                break;

        case node_stmt_yield:
        {
            stmt_yield_t* stmt_yield = (stmt_yield_t*) node;
            if ((metac_node_t)stmt_yield->YieldExp != ((metac_node_t) 0x1))
                result = walker_fn((metac_node_t)stmt_yield->YieldExp, ctx);
            if(result)
                 return result;
        } break;

        case node_stmt_scope:



        case node_stmt_for:
        {
            stmt_for_t* stmt_for = (stmt_for_t*) node;
            if (stmt_for->ForInit != ((metac_node_t) 0x1))
                result = walker_fn((metac_node_t)stmt_for->ForInit, ctx);
            if (result)
                return result;

            if ((metac_node_t)stmt_for->ForCond != ((metac_node_t) 0x1))
                result = walker_fn((metac_node_t)stmt_for->ForCond, ctx);
            if(result)
                 return result;

            if ((metac_node_t)stmt_for->ForPostLoop != ((metac_node_t) 0x1))
                result = walker_fn((metac_node_t)stmt_for->ForPostLoop, ctx);
            if(result)
                 return result;
        } break;

        case node_stmt_while:
        {
            stmt_while_t* stmt_while = (stmt_while_t*) node;
            if ((metac_node_t)stmt_while->WhileExp != ((metac_node_t) 0x1))
                result = walker_fn((metac_node_t)stmt_while->WhileExp, ctx);
            if(result)
                 return result;

            if ((metac_node_t)stmt_while->WhileBody != ((metac_node_t) 0x1))
                result = MetaCNode_TreeWalk_Real((metac_node_t)stmt_while->WhileBody, walker_fn, ctx);
            if (result)
                return result;
        } break;

        case node_stmt_case:
        {
            stmt_case_t* stmt_case = (stmt_case_t*) node;
            if ((metac_node_t)stmt_case->CaseExp != ((metac_node_t) 0x1))
                result = walker_fn((metac_node_t)stmt_case->CaseExp, ctx);
            if(result)
                 return result;
            if ((metac_node_t)stmt_case->CaseBody != ((metac_node_t) 0x1))
                result = MetaCNode_TreeWalk_Real((metac_node_t)stmt_case->CaseBody, walker_fn, ctx);
            if (result)
                return result;
        } break;

        case node_stmt_goto:
            break;

        case node_stmt_exp:
        {
            stmt_exp_t* stmt_exp = (stmt_exp_t*) node;
            if ((metac_node_t)stmt_exp->Expression != ((metac_node_t) 0x1))
                result = walker_fn((metac_node_t)stmt_exp->Expression, ctx);
            if(result)
                 return result;
        } break;

        case node_stmt_decl:
        {
            stmt_decl_t* stmt_decl = (stmt_decl_t*) node;
        } break;

        case node_stmt_if:
        {
            stmt_if_t* stmt_if = (stmt_if_t*) node;
        } break;

        case node_stmt_label:
            break;

        case node_stmt_return:
        {
            stmt_return_t* stmt_return = (stmt_return_t*) node;
            if ((metac_node_t)stmt_return->ReturnExp != ((metac_node_t) 0x1))
                result = walker_fn((metac_node_t)stmt_return->ReturnExp, ctx);
            if(result)
                 return result;
        } break;

        case node_stmt_switch:
        {
            stmt_switch_t* stmt_switch = (stmt_switch_t*) node;
            if ((metac_node_t)stmt_switch->SwitchExp != ((metac_node_t) 0x1))
                result = walker_fn((metac_node_t)stmt_switch->SwitchExp, ctx);
            if(result)
                 return result;
            if ((metac_node_t)stmt_switch->SwitchBody != ((metac_node_t) 0x1))
                result = MetaCNode_TreeWalk_Real((metac_node_t)stmt_switch->SwitchBody, walker_fn, ctx);
            if (result)
                return result;
        } break;

        case node_stmt_do_while:
        {
            stmt_do_while_t* stmt_do_while = (stmt_do_while_t*) node;
            if ((metac_node_t)stmt_do_while->DoWhileExp != ((metac_node_t) 0x1))
                result = walker_fn((metac_node_t)stmt_do_while->DoWhileExp, ctx);
            if(result)
                 return result;
        } break;

        case node_stmt_comment:
            break;
    }
}




int MetaCTree_Walk_Debug(metac_node_t node, const char* fn_name, walker_function_t walker_fn, void* ctx)
{




    return MetaCNode_TreeWalk_Real(node, walker_fn, ctx);
}
















metac_filesystem_ctx* MetaCNative_Init(const char* config)
{
    return (metac_filesystem_ctx*) 0;
}



metac_filehandle_t MetaCNative_Open(void* dummy, const char* path, const char* file)
{
    metac_filehandle_t result;

    char _pathBuffer[1024];
    char* pathBufferP = _pathBuffer;




    if (path)
    {
        snprintf(pathBufferP, sizeof(_pathBuffer), "%s/%s", path, file);
    }
    else
    {
        pathBufferP = (char*) file;
    }

    const char* absPath = realpath(pathBufferP, 0);



    if (!absPath)
    {
        perror("Path Invalid");
    }
    printf("Opening: %s -- %s\n", absPath, pathBufferP);
    result.p = (void*)fopen(absPath, "rb");

    return result;
}

metac_buffer_t MetaCNative_ReadEntireFileAndZeroTerminate(void* dummy, metac_filehandle_t handle)
{
    FILE* fd = (FILE*)handle.p;
    metac_buffer_t result = {0};

    if(!fd)
    {
        perror("Error Reading File: ");
    }
    else
    {
        fseek(fd, 0,

                    2

                            );
        result.Length = ftell(fd);
        fseek(fd, 0,

                    0

                            );
        uint32_t aligned_size =
            (((result.Length + 1) + 3) & ~3);

        result.Data = (char*) malloc(aligned_size);

        uint32_t read_size = fread((void*)result.Data, 1, result.Length, fd);

        for(uint32_t p = result.Length;
            p < aligned_size;
            p++)
        {
            ((char*)result.Data)[p] = '\0';
        }
    }

    return result;
}

void MetaCNative_Close(void* dummy, metac_filehandle_t handle)
{
    fclose((FILE*) handle.p);
}

static const metac_filesystem_functions_t native_functions =
{
    (metac_filesystem_init_t) MetaCNative_Init,
    (metac_filesystem_open_t) MetaCNative_Open,
    (metac_filesystem_read_entire_file_and_zero_terminate_t)
        MetaCNative_ReadEntireFileAndZeroTerminate,
    (metac_filesystem_close_t) MetaCNative_Close,


} ;


static const metac_filesystem_t NativeFileSystem = {
    0, &native_functions
};


void FileStorage_Init(metac_file_storage_t* self, metac_filesystem_t* fs)
{


    IdentifierTable_Init(&self->Filenames, 20, 9);
    IdentifierTable_Init(&self->Paths, 20, 9);

    if (!fs)
    {
        self->FS = (metac_filesystem_t*)&NativeFileSystem;
    }
}

metac_buffer_t MetaCFileStorage_GetEntireFileBuffer(metac_file_storage_t* self, metac_file_ptr_t file)
{
    metac_buffer_t result = {0};

    return result;
}

int32_t findSlash(const char* path, const uint32_t pathLen)
{
    int32_t slashPosition = -1;

    const int32_t pathLen_s = (int32_t) pathLen;





    for(int32_t i = 0; i < pathLen_s; i++)
    {
        char c = path[pathLen_s - 1 - i];
        if (c == '/')
        {
            slashPosition = pathLen_s - 1 - i;
            break;
        }
    }

    return slashPosition;
}

typedef struct MetaCFileStorage_LoadTask_ctx_t
{
    metac_file_storage_t* FileStorage;
    metac_file_ptr_t FilePtr;
    metac_filehandle_t Result;
} MetaCFileStorage_LoadTask_ctx_t;


void MetaCFileStorage_LoadTask(task_t* task)
{
    MetaCFileStorage_LoadTask_ctx_t* ctxP =
        ((MetaCFileStorage_LoadTask_ctx_t*)task->Context);
    const MetaCFileStorage_LoadTask_ctx_t ctx = *ctxP;

    char filePath[256];

    uint16_t PathIdx = ctx.FilePtr.PathIdx;
    uint16_t FileIdx = ctx.FilePtr.FilenameIdx;

    const char* filename =
        ctx.FileStorage->Filenames.StringMemory + (FileIdx - 4);

    const char* path = (PathIdx ?
        ctx.FileStorage->Paths.StringMemory + (PathIdx - 4) :
        0);

    metac_filesystem_t fs = *ctx.FileStorage->FS;
    const metac_filesystem_functions_t* funcs = fs.functions;

    ctxP->Result = funcs->Open(fs.ctx, path, filename);
}

metac_file_ptr_t MetaCFileStorage_LoadFile(metac_file_storage_t* self, const char* path)
{


    metac_file_ptr_t result = {0};

    uint32_t pathLen = strlen(path);
    int32_t slashPosition = findSlash(path, pathLen);

    const char* baseName = path + slashPosition + 1;
    uint32_t baseNameLength =
        (slashPosition == -1 ? pathLen : pathLen - slashPosition - 1);



    uint32_t fileNameHash = crc32c(~0, baseName, baseNameLength);
    uint32_t fileNameKey = ( ((uint32_t)(fileNameHash & 0xFFFFF)) | (((uint32_t)(baseNameLength)) << 20) );

    uint32_t filePathHash = 0;
    uint32_t filePathKey = 0;

    if (slashPosition != -1)
    {
        filePathHash = crc32c(~0, path, slashPosition);
        filePathKey = ( ((uint32_t)(filePathHash & 0xFFFFF)) | (((uint32_t)(slashPosition)) << 20) );
    }

    metac_identifier_ptr_t fileNamePtr =
        GetOrAddIdentifier(&self->Filenames, fileNameKey, baseName);
    metac_identifier_ptr_t filePathPtr = {0};

    if (slashPosition != -1)
    {
        filePathPtr = GetOrAddIdentifier(&self->Paths, filePathKey, path);
    }

    printf("fileNamePtr: %u\n", fileNamePtr.v);
    printf("filePathPtr: %u\n", filePathPtr.v);



                                     ;

    result.FilenameIdx = fileNamePtr.v;
    result.PathIdx = filePathPtr.v;

    MetaCFileStorage_LoadTask_ctx_t taskCtx = {
        self,
        result
    };


    task_t loadTask = {0};
    loadTask.TaskFunction = MetaCFileStorage_LoadTask;
    loadTask.ContextSize = sizeof(taskCtx);
    *((MetaCFileStorage_LoadTask_ctx_t*)loadTask._inlineContext) =
        taskCtx;
    loadTask.Context = loadTask._inlineContext;
    loadTask.Parent = CurrentTask();
    ( loadTask.Origin.File = "../metac_file.c", loadTask.Origin.Func = __FUNCTION__, loadTask.Origin.Line = 237 );

    AddTaskToQueue(&loadTask);




    return result;
}













static inline void Dot_PrintSpace(metac_dot_printer_t* self)
{
    self->StringMemorySize++;
}

static inline void Dot_PrintChar(metac_dot_printer_t* self, char c)
{


    self->StringMemory[self->StringMemorySize++] = c;
}

static inline void Dot_PrintNewline(metac_dot_printer_t* self)
{
    self->StringMemory[self->StringMemorySize++] = '\n';
}

static inline void Dot_CheckAndRellocIfNeeded(metac_dot_printer_t* self,
                                              uint32_t length)
{
    int32_t underflow =
        self->StringMemoryCapacity -
        (self->StringMemorySize + length + 1024);
    if (underflow < 0)
    {
        uint32_t newCapa = (uint32_t)((self->StringMemoryCapacity) * 1.3);
        newCapa = ((newCapa + 4095) & ~4095);
        self->StringMemory = (char*)realloc(self->StringMemory, newCapa);
        if (self->StringMemory == 0)
        {
            ;
        }
    }
}

metac_dot_printer_label_t* MetaCDotPrinter_BeginLabel(metac_dot_printer_t* self)
{


                                  ;
    self->CurrentLabel = (metac_dot_printer_label_t*)
        malloc(sizeof(metac_dot_printer_label_t) + 128);
    self->CurrentLabel->LabelMemoryCapacity = 128;
    self->CurrentLabel->LabelMemory =
        ((char*) self->CurrentLabel) + sizeof(metac_dot_printer_label_t);

    return self->CurrentLabel;
}

static inline

             _Bool

                  NeedsEscape(char c)
{
    switch (c)
    {
        case '\n':
        case '\t':
        case '\'':
        case '\"':
            return

                  1

                      ;
    }
    return

          0

               ;
}

static inline char AfterEscape(char c)
{
    switch (c)
    {
        case '\n':
            return 'n';
        case '\t':
            return 't';
    }

    return c;
}

void MetaCDotPrinterLabel_PrintChar(metac_dot_printer_label_t* self, char c)
{
    if (NeedsEscape(c))
    {
        self->LabelMemory[self->LabelMemorySize++] = '\\';
        self->LabelMemory[self->LabelMemorySize++] = AfterEscape(c);
    }
    else
    {
        self->LabelMemory[self->LabelMemorySize++] = c;
    }
}






void MetaCDotPrinter_EndLabel(metac_dot_printer_t* self, metac_dot_printer_label_t* label)
{


                                      ;
    uint32_t neededSize = self->CurrentLabel->LabelMemorySize + sizeof("label=\"\"");
    uint32_t newCapa = (((self->StringMemorySize + neededSize) + 3) & ~3);

    if (newCapa > self->StringMemoryCapacity)
    {
        self->StringMemory = (char*)realloc(self->StringMemory, newCapa);
        self->StringMemoryCapacity = newCapa;
    }
    Dot_PrintString(self, "label=\"");

    memcpy(self->StringMemory + self->StringMemorySize,
        self->CurrentLabel->LabelMemory,
        self->CurrentLabel->LabelMemorySize);

    Dot_PrintChar(self, '\"');
}

void Dot_PrintString(metac_dot_printer_t* self,
                     const char* string)
{
    char c;

    while((c = *string++))
    {
        self->StringMemory[self->StringMemorySize++] = c;
    }
}

static inline void Dot_PrintIdentifier(metac_dot_printer_t* self,
                                       metac_identifier_ptr_t idPtr)
{
    if (idPtr.v == empty_identifier.v)



                ;
    const char* ident = IdentifierPtrToCharPtr(self->IdTable, idPtr);

    Dot_PrintString(self, ident);
}


static inline void Dot_PrintU64(metac_dot_printer_t* self, uint64_t value)
{
    char u64Buffer[21];

    char* result = u64tostr(value, u64Buffer);
    int32_t length = (u64Buffer + 20) - result;

    result[length] = '\0';
    Dot_PrintString(self, result);
}

static inline void Dot_PrintI64(metac_dot_printer_t* self, int64_t value)
{
    char i64Buffer[22];

    char* result = i64tostr(value, i64Buffer);
    int32_t length = (i64Buffer + 20) - result;

    result[length] = '\0';
    Dot_PrintString(self, result);
}


void MetaCDotPrinter_Reset(metac_dot_printer_t* self)
{
    memset(self->StringMemory, ' ', self->StringMemorySize);

    self->StringMemorySize = 0;
}

const char* MetaCDotPrinter_Snapshot(metac_dot_printer_t* self)
{
    if (self->StringMemorySize > self->SnapshotMemoryCapacity)
    {
        uint32_t newCapa = (((self->StringMemorySize + 1) + 3) & ~3);
        self->SnapshotMemory = (char*)realloc(self->SnapshotMemory, newCapa);
        self->StringMemoryCapacity = newCapa;
    }

    memcpy(self->SnapshotMemory, self->StringMemory, self->StringMemorySize);
    self->SnapshotMemory[self->StringMemorySize] = '\0';
    return self->SnapshotMemory;
}

void MetaCDotPrinter_Init(metac_dot_printer_t* self,
                          metac_identifier_table_t* idTable)
{
    self->StringMemoryCapacity = (16 * 4096);
    self->StringMemory = (char*)malloc(self->StringMemoryCapacity);
    self->StringMemorySize = self->StringMemoryCapacity;

    self->SnapshotMemoryCapacity = (16 * 4096);
    self->SnapshotMemory = (char*)malloc(self->SnapshotMemoryCapacity);
    self->IdTable = idTable;

    MetaCDotPrinter_Reset(self);
}










void TypeTableInitImpl(metac_type_table_t* table, const uint32_t sizeof_slot, metac_type_index_kind_t kind)
{
    table->Kind = kind;
    table->SlotCount_Log2 = 12;
    const uint32_t maxSlots = (1 << table->SlotCount_Log2);
    table->Slots = (metac_type_table_slot_t*) calloc(maxSlots, sizeof_slot);
    table->SlotsUsed = 0;
    table->MaxDisplacement = 0;
}




metac_type_index_t MetaCTypeTable_GetOrEmptyImpl(metac_type_table_t* table,
                                                 metac_type_table_slot_t* key,
                                                 const uint32_t slotTrailingSize,
                                                 const

                                                      _Bool

                                                           (*cmpSlot)(const metac_type_table_slot_t*,
                                                                       const metac_type_table_slot_t*))
{
    metac_type_index_t result = {0};

    const uint32_t hash = key->Hash;
    const uint32_t slotSize = slotTrailingSize + sizeof(metac_type_table_slot_t);
    const uint32_t slotIndexMask = ((1 << table->SlotCount_Log2) - 1);
    const uint32_t initialSlotIndex = (hash & slotIndexMask);

    for(
        uint32_t slotIndex = initialSlotIndex;
        (++slotIndex & slotIndexMask) != initialSlotIndex;
    )
    {
        metac_type_table_slot_t* slot = (metac_type_table_slot_t*)
        (((char*)table->Slots) +
            ((slotIndex - 1) & slotIndexMask) * slotSize);

        if (slot->Hash == hash)
        {
            if (cmpSlot(slot, key))
            {
                uint32_t slotIndex = (((char*)slot) - (char*)table->Slots) / slotSize;



                                            ;
                result.v = (((table->Kind) << 28) | (slotIndex));
                break;
            }
            else
            {

            }
        }
        else if (slot->Hash == 0)
        {
            break;
        }
        continue;
    }
    return result;
}

metac_type_index_t MetaCTypeTable_GetOrEmptyEnumType (const metac_type_table_enum_t* table, metac_type_enum_t* key) { metac_type_index_t result = MetaCTypeTable_GetOrEmptyImpl((metac_type_table_t*)table, (metac_type_table_slot_t*)key, (sizeof(*key) - sizeof(metac_type_table_slot_t)), EnumSlotsEqual ); return result; } metac_type_index_t MetaCTypeTable_GetOrEmptyArrayType (const metac_type_table_array_t* table, metac_type_array_t* key) { metac_type_index_t result = MetaCTypeTable_GetOrEmptyImpl((metac_type_table_t*)table, (metac_type_table_slot_t*)key, (sizeof(*key) - sizeof(metac_type_table_slot_t)), ArraySlotsEqual ); return result; } metac_type_index_t MetaCTypeTable_GetOrEmptyStructType (const metac_type_table_aggregate_t* table, metac_type_aggregate_t* key) { metac_type_index_t result = MetaCTypeTable_GetOrEmptyImpl((metac_type_table_t*)table, (metac_type_table_slot_t*)key, (sizeof(*key) - sizeof(metac_type_table_slot_t)), AggregateSlotsEqual ); return result; } metac_type_index_t MetaCTypeTable_GetOrEmptyUnionType (const metac_type_table_aggregate_t* table, metac_type_aggregate_t* key) { metac_type_index_t result = MetaCTypeTable_GetOrEmptyImpl((metac_type_table_t*)table, (metac_type_table_slot_t*)key, (sizeof(*key) - sizeof(metac_type_table_slot_t)), AggregateSlotsEqual ); return result; } metac_type_index_t MetaCTypeTable_GetOrEmptyPtrType (const metac_type_table_ptr_t* table, metac_type_ptr_t* key) { metac_type_index_t result = MetaCTypeTable_GetOrEmptyImpl((metac_type_table_t*)table, (metac_type_table_slot_t*)key, (sizeof(*key) - sizeof(metac_type_table_slot_t)), PtrSlotsEqual ); return result; } metac_type_index_t MetaCTypeTable_GetOrEmptyFunctionType (const metac_type_table_functiontype_t* table, metac_type_functiontype_t* key) { metac_type_index_t result = MetaCTypeTable_GetOrEmptyImpl((metac_type_table_t*)table, (metac_type_table_slot_t*)key, (sizeof(*key) - sizeof(metac_type_table_slot_t)), FunctiontypeSlotsEqual ); return result; } metac_type_index_t MetaCTypeTable_GetOrEmptyTypedefType (const metac_type_table_typedef_t* table, metac_type_typedef_t* key) { metac_type_index_t result = MetaCTypeTable_GetOrEmptyImpl((metac_type_table_t*)table, (metac_type_table_slot_t*)key, (sizeof(*key) - sizeof(metac_type_table_slot_t)), TypedefSlotsEqual ); return result; } metac_type_index_t MetaCTypeTable_GetOrEmptyTupleType (const metac_type_table_tuple_t* table, metac_type_tuple_t* key) { metac_type_index_t result = MetaCTypeTable_GetOrEmptyImpl((metac_type_table_t*)table, (metac_type_table_slot_t*)key, (sizeof(*key) - sizeof(metac_type_table_slot_t)), TupleSlotsEqual ); return result; };

metac_type_index_t MetaCTypeTable_AddImpl(metac_type_table_t* self,
                                          const metac_type_table_slot_t *entry,
                                          const uint32_t trailingSize,
                                          const

                                               _Bool

                                                    (*cmpSlot)(const metac_type_table_slot_t*,
                                                          const metac_type_table_slot_t*))
{
    metac_type_index_t result = {0};





    const uint32_t hash = entry->Hash;
    const uint32_t keySize = trailingSize + sizeof(metac_type_table_slot_t);

    const uint32_t slotIndexMask = ((1 << self->SlotCount_Log2) - 1);
    const uint32_t initialSlotIndex = (hash & slotIndexMask);

    for(
        uint32_t slotIndex = initialSlotIndex;
        (++slotIndex & slotIndexMask) != initialSlotIndex;
    )
    {
        slotIndex = ((slotIndex - 1) & slotIndexMask);
        metac_type_table_slot_t* slot = (metac_type_table_slot_t*)
        (((char*)self->Slots) + (slotIndex * keySize));

        if (slot->Hash == 0)
        {

            memcpy(slot, entry, keySize);
            result.v = (((self->Kind) << 28) | (slotIndex));
        }
        else if (slot->Hash == hash)
        {



                                         ;
            result.v = (((self->Kind) << 28) | (slotIndex));

        } else
        {
            continue;
        }

        return result;
    }


}

metac_type_index_t MetaCTypeTable_AddEnumType (metac_type_table_enum_t* table, const metac_type_enum_t* key) { return MetaCTypeTable_AddImpl((metac_type_table_t*)table, (metac_type_table_slot_t*)key, (sizeof(*key) - sizeof(metac_type_table_slot_t)), EnumSlotsEqual); } metac_type_index_t MetaCTypeTable_AddArrayType (metac_type_table_array_t* table, const metac_type_array_t* key) { return MetaCTypeTable_AddImpl((metac_type_table_t*)table, (metac_type_table_slot_t*)key, (sizeof(*key) - sizeof(metac_type_table_slot_t)), ArraySlotsEqual); } metac_type_index_t MetaCTypeTable_AddStructType (metac_type_table_aggregate_t* table, const metac_type_aggregate_t* key) { return MetaCTypeTable_AddImpl((metac_type_table_t*)table, (metac_type_table_slot_t*)key, (sizeof(*key) - sizeof(metac_type_table_slot_t)), AggregateSlotsEqual); } metac_type_index_t MetaCTypeTable_AddUnionType (metac_type_table_aggregate_t* table, const metac_type_aggregate_t* key) { return MetaCTypeTable_AddImpl((metac_type_table_t*)table, (metac_type_table_slot_t*)key, (sizeof(*key) - sizeof(metac_type_table_slot_t)), AggregateSlotsEqual); } metac_type_index_t MetaCTypeTable_AddPtrType (metac_type_table_ptr_t* table, const metac_type_ptr_t* key) { return MetaCTypeTable_AddImpl((metac_type_table_t*)table, (metac_type_table_slot_t*)key, (sizeof(*key) - sizeof(metac_type_table_slot_t)), PtrSlotsEqual); } metac_type_index_t MetaCTypeTable_AddFunctionType (metac_type_table_functiontype_t* table, const metac_type_functiontype_t* key) { return MetaCTypeTable_AddImpl((metac_type_table_t*)table, (metac_type_table_slot_t*)key, (sizeof(*key) - sizeof(metac_type_table_slot_t)), FunctiontypeSlotsEqual); } metac_type_index_t MetaCTypeTable_AddTypedefType (metac_type_table_typedef_t* table, const metac_type_typedef_t* key) { return MetaCTypeTable_AddImpl((metac_type_table_t*)table, (metac_type_table_slot_t*)key, (sizeof(*key) - sizeof(metac_type_table_slot_t)), TypedefSlotsEqual); } metac_type_index_t MetaCTypeTable_AddTupleType (metac_type_table_tuple_t* table, const metac_type_tuple_t* key) { return MetaCTypeTable_AddImpl((metac_type_table_t*)table, (metac_type_table_slot_t*)key, (sizeof(*key) - sizeof(metac_type_table_slot_t)), TupleSlotsEqual); };

uint32_t EntangleInts(uint32_t a, uint32_t b)
{
    uint32_t max_n = (a > b ? a : b);
    uint32_t result = 0;
    uint32_t srcPos = 0;
    uint32_t dstPos = 0;

    if (max_n > (1 << 15))
    {
        max_n = (1 << 15);
        result |= (1 << 31);
    }

    while(max_n)
    {
        {


           _Bool

                bit = (a & (1 << srcPos)) >> srcPos;
            result |= (bit << dstPos++);
        }

        {


           _Bool

                bit = (b & (1 << srcPos)) >> srcPos;
            result |= (bit << dstPos++);
        }
        srcPos++;
        max_n >>= 1;
    }

    return result;
}

uint32_t UntangleInts(uint32_t tangled)
{



                                     ;
    uint16_t a = 0;
    uint16_t b = 0;
    uint32_t binpos = 0;

    while(tangled)
    {
        a |= ((tangled & 1) << binpos);
        tangled >>= 1;

        b |= ((tangled & 1) << binpos);
        tangled >>= 1;
        binpos++;
    }

    uint32_t result = a | (b << 16);

    return result;
}







uint32_t crc32c_nozero(uint32_t crc, const void* s, const uint32_t len_p);
uint32_t crc32c_byte(uint32_t crc, uint8_t byte);







metac_expression_t* AllocNewExpression(metac_expression_kind_t kind);

metac_declaration_t* AllocNewDeclaration_(metac_declaration_kind_t kind, uint32_t nodeSize, void** result_ptr, uint32_t line);





metac_statement_t* AllocNewStatement_(metac_statement_kind_t kind, uint32_t nodeSize, void** result_ptr);








void MetaCScopeTable_InitN(metac_scope_table_t* self, uint32_t nMembers)
{
    self->SlotCount_Log2 = ((__builtin_clz(nMembers) ^ 31) + 1);
    const uint32_t maxSlots = (1 << self->SlotCount_Log2);
    self->Slots = (metac_scope_table_slot_t*) calloc(maxSlots, sizeof(metac_scope_table_slot_t));
    self->SlotsUsed = 0;
}

void MetaCScopeTable_Init(metac_scope_table_t* self)
{
    MetaCScopeTable_InitN(self, 255);
}

void MetaCScopeTable_Free(metac_scope_table_t* self)
{
    free(self->Slots);
    self->Slots = 0;
    self->SlotCount_Log2 = 0;
    self->SlotsUsed = 0;
}

metac_scope_table_slot_t* MetaCScopeTable_Lookup(metac_scope_table_t* self,
                                                 const uint32_t idPtrHash,
                                                 metac_identifier_ptr_t idPtr)
{
   ;

    const uint32_t slotIndexMask = ((1 << self->SlotCount_Log2) - 1);
    const uint32_t initialSlotIndex = (idPtrHash & slotIndexMask);

    for(
        uint32_t slotIndex = initialSlotIndex;
        (++slotIndex & slotIndexMask) != initialSlotIndex;
    )
    {
        metac_scope_table_slot_t* slot =
            &self->Slots[(slotIndex - 1) & slotIndexMask];
        if (slot->Hash == idPtrHash && idPtr.v == slot->Ptr.v)
        {
            ;
            return slot;
        }
        else if (slot->Hash == 0)
        {
            break;
        }
    }
    ;
    return 0;
}



metac_scope_table_slot_t* MetaCScopeTable_Insert(metac_scope_table_t* self,
                                                 metac_identifier_ptr_t idPtr)
{
    uint32_t hash = crc32c_nozero(~0, &idPtr.v, sizeof(idPtr.v));
    const uint32_t slotIndexMask = ((1 << self->SlotCount_Log2) - 1);
    const uint32_t initialSlotIndex = (hash & slotIndexMask);


    for(
        uint32_t slotIndex = initialSlotIndex;
        (++slotIndex & slotIndexMask) != initialSlotIndex;

    )
    {
        metac_scope_table_slot_t* slot =
            &self->Slots[(slotIndex - 1) & slotIndexMask];
        if (slot->Hash == 0)
        {
            self->SlotsUsed++;
            slot->Hash = hash;
            slot->Ptr = idPtr;

            return slot;
        }
        else if (slot->Hash == hash && slot->Ptr.v == idPtr.v)
        {
            return ((metac_scope_table_slot_t*) 2);
        }
    }

    return ((metac_scope_table_slot_t*) 1);
}


scope_insert_error_t MetaCScope_RegisterIdentifier(metac_scope_t* self,
                                                   metac_identifier_ptr_t idPtr,
                                                   metac_node_header_t* node)
{
    metac_scope_table_slot_t* slot =
        MetaCScopeTable_Insert(&self->ScopeTable, idPtr);

    if (slot == ((metac_scope_table_slot_t*) 2))
    {
        return identifier_exists_already;
    }
    else if (slot == ((metac_scope_table_slot_t*) 1))
    {
        return table_full;
    }
    else
    {
        slot->Node = node;

        return success;
    }
}

metac_node_header_t* MetaCScope_LookupIdentifier(metac_scope_t* self,
                                                 uint32_t idPtrHash,
                                                 metac_identifier_ptr_t identifierPtr)
{
    metac_node_header_t* result = 0;
    {
        metac_scope_table_slot_t * slot =
            MetaCScopeTable_Lookup(&self->ScopeTable, idPtrHash, identifierPtr);
        if (slot != 0)
        {
            result = slot->Node;


                              ;
        }
    }
    return result;
}









typedef enum basic_type_kind_t
{


    basic_bool = type_bool,
    basic_char,
    basic_short,
    basic_int,
    basic_long,
    basic_uint32_t,

    basic_float,
    basic_double,

    basic_long_long,
    basic_long_double
} basic_type_kind_t;

typedef struct metac_target_info_t
{
    const uint8_t SizeBool;
    const uint8_t SizeShort;
    const uint8_t SizeInt;
    const uint8_t SizeLong;
    const uint8_t SizeSizeT;
    const uint8_t SizeFloat;
    const uint8_t SizeDouble;
    const uint8_t SizeLongLong;
    const uint8_t SizeLongDouble;

    const uint8_t AlignmentBool;
    const uint8_t AlignmentShort;
    const uint8_t AlignmentInt;
    const uint8_t AlignmentLong;
    const uint8_t AlignmentSizeT;
    const uint8_t AlignmentFloat;
    const uint8_t AlignmentDouble;
    const uint8_t AlignmentLongLong;
    const uint8_t AlignmentLongDouble;

    const uint8_t PtrSize;
    const uint8_t AlignmentChar;
} metac_target_info_t;


uint32_t MetaCTargetInfo_GetBasicSize(const metac_target_info_t* targetInfo,
                                      basic_type_kind_t Kind);

uint32_t MetaCTargetInfo_GetBasicAlign(const metac_target_info_t* targetInfo,
                                       basic_type_kind_t Kind);


uint32_t MetaCTargetInfo_GetBasicSize(const metac_target_info_t* targetInfo,
                                      basic_type_kind_t Kind)
{
    uint32_t result = -1;

    switch(Kind)
    {
        case basic_bool:
            result = targetInfo->SizeBool;
        break;
        case basic_char:
            result = 1;
        break;
        case basic_short:
            result = targetInfo->SizeShort;
        break;
        case basic_int:
            result = targetInfo->SizeInt;
        break;
        case basic_long:
            result = targetInfo->SizeLong;
        break;
        case basic_long_long:
            result = targetInfo->SizeLongLong;
        break;
        case basic_uint32_t:
            result = targetInfo->SizeSizeT;
        break;
        case basic_float:
            result = targetInfo->SizeFloat;
        break;
        case basic_double:
            result = targetInfo->SizeDouble;
        break;
        case basic_long_double:
            result = targetInfo->SizeLongDouble;
        break;
    }

    return result;
}

uint32_t MetaCTargetInfo_GetBasicAlign(const metac_target_info_t* targetInfo,
                                       basic_type_kind_t Kind)
{
    uint32_t result = -1;

    switch(Kind)
    {
        case basic_bool:
            result = targetInfo->AlignmentBool;
        break;
        case basic_char:
            result = targetInfo->AlignmentChar;
        break;
        case basic_short:
            result = targetInfo->AlignmentShort;
        break;
        case basic_int:
            result = targetInfo->AlignmentInt;
        break;
        case basic_long:
            result = targetInfo->AlignmentLong;
        break;
        case basic_long_long:
            result = targetInfo->AlignmentLongLong;
        break;
        case basic_uint32_t:
            result = targetInfo->AlignmentSizeT;
        break;
        case basic_float:
            result = targetInfo->AlignmentFloat;
        break;
        case basic_double:
            result = targetInfo->AlignmentDouble;
        break;
        case basic_long_double:
            result = targetInfo->AlignmentLongDouble;
        break;
    }

    return result;
}











metac_expression_t* AllocNewExpression(metac_expression_kind_t kind);

metac_declaration_t* AllocNewDeclaration_(metac_declaration_kind_t kind, uint32_t nodeSize, void** result_ptr, uint32_t line);





metac_statement_t* AllocNewStatement_(metac_statement_kind_t kind, uint32_t nodeSize, void** result_ptr);









static const metac_target_info_t default_target_info =
{
                    sizeof(

                          _Bool

                              ),
                     sizeof(short),
                   sizeof(int),
                    sizeof(long),
                     sizeof(uint32_t),
                     sizeof(float),
                      sizeof(double),
                        sizeof(long long),
                          sizeof(long double),

                         _Alignof(

                                 _Bool

                                     ),
                          _Alignof(short),
                        _Alignof(int),
                         _Alignof(long),
                          _Alignof(uint32_t),
                          _Alignof(float),
                           _Alignof(double),
                             _Alignof(long long),
                               _Alignof(long double),

                   sizeof(void*),
                         _Alignof(char)
};








uint32_t crc32c_nozero(uint32_t crc, const void* s, const uint32_t len_p);
uint32_t crc32c_byte(uint32_t crc, uint8_t byte);

















void aco_runtime_test(void){



    _Static_assert(sizeof(void*) == 8, "require 'sizeof(void*) == 8'");
    _Static_assert(sizeof(__uint128_t) == 16, "require 'sizeof(__uint128_t) == 16'");



    _Static_assert(sizeof(int) >= 4, "require 'sizeof(int) >= 4'");
    (((__builtin_expect(!!(sizeof(int) >= 4), 1)))? (void)0 :aco_abort());
    _Static_assert(sizeof(int) <= sizeof(uint32_t),
        "require 'sizeof(int) <= sizeof(uint32_t)'");
    (((__builtin_expect(!!(sizeof(int) <= sizeof(uint32_t)), 1)))? (void)0 :aco_abort());
}

__thread aco_t* aco_gtls_co_;







static void aco_default_protector_last_word(void){
    aco_t* co = (aco_gtls_co_);

    fprintf(

           stderr

                 ,"error: aco_default_protector_last_word triggered\n");
    fprintf(

           stderr

                 , "error: co:%p should call `aco_exit()` instead of direct "
        "`return` in co_fp:%p to finish its execution\n", co, (void*)co->fp);
    (((__builtin_expect(!!(0), 1)))? (void)0 :aco_abort());
}

static aco_cofuncp_t aco_gtls_last_word_fp = aco_default_protector_last_word;




    static __thread void* aco_gtls_fpucw_mxcsr[1];

void aco_global_init(void)
{



}

void aco_thread_init(aco_cofuncp_t last_word_co_fp){
    aco_save_fpucw_mxcsr(aco_gtls_fpucw_mxcsr);

    if((void*)last_word_co_fp !=

                                ((void *)0)

                                    )
        aco_gtls_last_word_fp = last_word_co_fp;
}





void aco_funcp_protector(void){
    if((void*)(aco_gtls_last_word_fp) !=

                                        ((void *)0)

                                            ){
        aco_gtls_last_word_fp();
    }else{
        aco_default_protector_last_word();
    }
    (((__builtin_expect(!!(0), 1)))? (void)0 :aco_abort());
}

aco_share_stack_t* aco_share_stack_new(uint32_t sz){
    return aco_share_stack_new2(sz, 1);
}





aco_share_stack_t* aco_share_stack_new2(uint32_t sz, char guard_page_enabled){
    if(sz == 0){
        sz = 1024 * 1024 * 2;
    }
    if(sz < 4096){
        sz = 4096;
    }
    (((__builtin_expect(!!(sz > 0), 1)))? (void)0 :aco_abort());

    uint32_t u_pgsz = 0;
    if(guard_page_enabled != 0){


        long pgsz = sysconf(

                           _SC_PAGESIZE

                                       );

        (((__builtin_expect(!!(pgsz > 0 && (((pgsz - 1) & pgsz) == 0)), 1)))? (void)0 :aco_abort());
        u_pgsz = (uint32_t)((unsigned long)pgsz);

        (((__builtin_expect(!!(u_pgsz == (unsigned long)pgsz && ((u_pgsz << 1) >> 1) == u_pgsz), 1)))? (void)0 :aco_abort());
        if(sz <= u_pgsz){
            sz = u_pgsz << 1;
        } else {
            uint32_t new_sz;
            if((sz & (u_pgsz - 1)) != 0){
                new_sz = (sz & (~(u_pgsz - 1)));
                (((__builtin_expect(!!(new_sz >= u_pgsz), 1)))? (void)0 :aco_abort());
                do { (((__builtin_expect(!!((new_sz)+((u_pgsz << 1)) >= (new_sz)), 1)))? (void)0 :aco_abort()); }while(0);
                new_sz = new_sz + (u_pgsz << 1);
                (((__builtin_expect(!!(sz / u_pgsz + 2 == new_sz / u_pgsz), 1)))? (void)0 :aco_abort());
            } else {
                do { (((__builtin_expect(!!((sz)+(u_pgsz) >= (sz)), 1)))? (void)0 :aco_abort()); }while(0);
                new_sz = sz + u_pgsz;
                (((__builtin_expect(!!(sz / u_pgsz + 1 == new_sz / u_pgsz), 1)))? (void)0 :aco_abort());
            }
            sz = new_sz;
            (((__builtin_expect(!!((sz / u_pgsz > 1) && ((sz & (u_pgsz - 1)) == 0)), 1)))? (void)0 :aco_abort());
        }
    }

    aco_share_stack_t* p = (aco_share_stack_t*)malloc(sizeof(aco_share_stack_t));
    do { if((__builtin_expect(!!((p) ==

   ((void *)0)

   ), 0))){ fprintf(

   stderr

   , "Aborting: failed to allocate memory: %s:%d:%s\n", "../3rd_party/libaco/aco.c", 272, __PRETTY_FUNCTION__); ; } } while(0);
    memset(p, 0, sizeof(aco_share_stack_t));

    if(guard_page_enabled != 0){
        p->real_ptr = mmap(


           ((void *)0)

               , sz,

                     0x1

                              |

                               0x2

                                         ,

                                           0x02

                                                      |

                                                       0x20

                                                                    , -1, 0
        );
        do { if((__builtin_expect(!!(!(p->real_ptr !=

       ((void *) -1)

       )), 0))){ fprintf(

       stderr

       , "Aborting: failed to allocate memory: %s:%d:%s\n", "../3rd_party/libaco/aco.c", 279, __PRETTY_FUNCTION__); ; } } while(0);
        p->guard_page_enabled = 1;
        (((__builtin_expect(!!(0 == mprotect(p->real_ptr, u_pgsz,

       0x1

       )), 1)))? (void)0 :aco_abort());

        p->ptr = (void*)(((uintptr_t)p->real_ptr) + u_pgsz);
        p->real_sz = sz;
        (((__builtin_expect(!!(sz >= (u_pgsz << 1)), 1)))? (void)0 :aco_abort());
        p->sz = sz - u_pgsz;
    } else {

        p->sz = sz;
        p->ptr = malloc(sz);
        do { if((__builtin_expect(!!((p->ptr) ==

       ((void *)0)

       ), 0))){ fprintf(

       stderr

       , "Aborting: failed to allocate memory: %s:%d:%s\n", "../3rd_party/libaco/aco.c", 291, __PRETTY_FUNCTION__); ; } } while(0);
    }

    p->owner =

              ((void *)0)

                  ;






    uintptr_t u_p = (uintptr_t)(p->sz - (sizeof(void*) << 1) + (uintptr_t)p->ptr);
    u_p = (u_p >> 4) << 4;
    p->align_highptr = (void*)u_p;

    p->align_retptr = (void*)(u_p - 16);

    *((void**)(p->align_retptr)) = (void*)(aco_funcp_protector);
    (((__builtin_expect(!!(p->sz > (16 + (sizeof(void*) << 1) + sizeof(void*))), 1)))? (void)0 :aco_abort());
    p->align_limit = p->sz - 16 - (sizeof(void*) << 1);



    return p;
}

void aco_share_stack_destroy(aco_share_stack_t* sstk){
    (((__builtin_expect(!!(sstk !=

   ((void *)0)

   && sstk->ptr !=

   ((void *)0)

   ), 1)))? (void)0 :aco_abort());



    if(sstk->guard_page_enabled){
        (((__builtin_expect(!!(0 == munmap(sstk->real_ptr, sstk->real_sz)), 1)))? (void)0 :aco_abort());
        sstk->real_ptr =

                        ((void *)0)

                            ;
        sstk->ptr =

                   ((void *)0)

                       ;
    } else {
        free(sstk->ptr);
        sstk->ptr =

                   ((void *)0)

                       ;
    }
    free(sstk);
}

aco_t* aco_create(
        aco_t* main_co, aco_share_stack_t* share_stack,
        uint32_t save_stack_sz, aco_cofuncp_t fp, void* arg
    ){

    aco_t* p = (aco_t*)malloc(sizeof(aco_t));
    do { if((__builtin_expect(!!((p) ==

   ((void *)0)

   ), 0))){ fprintf(

   stderr

   , "Aborting: failed to allocate memory: %s:%d:%s\n", "../3rd_party/libaco/aco.c", 338, __PRETTY_FUNCTION__); ; } } while(0);
    memset(p, 0, sizeof(aco_t));

    if(main_co !=

                 ((void *)0)

                     ){
        (((__builtin_expect(!!((share_stack) !=

       ((void *)0)

       ), 1)))?((void)0):(aco_abort()));
        p->share_stack = share_stack;

        p->reg[13] = (void*)fp;
        p->reg[14] = p->share_stack->align_retptr;

            p->reg[15] = aco_gtls_fpucw_mxcsr[0];




        p->main_co = main_co;
        p->arg = arg;
        p->fp = fp;
        if(save_stack_sz == 0){
            save_stack_sz = 64;
        }
        p->save_stack.ptr = malloc(save_stack_sz);
        do { if((__builtin_expect(!!((p->save_stack.ptr) ==

       ((void *)0)

       ), 0))){ fprintf(

       stderr

       , "Aborting: failed to allocate memory: %s:%d:%s\n", "../3rd_party/libaco/aco.c", 370, __PRETTY_FUNCTION__); ; } } while(0);
        p->save_stack.sz = save_stack_sz;

        p->save_stack.valid_sz = 0;



        return p;
    } else {
        p->main_co =

                    ((void *)0)

                        ;
        p->arg = arg;
        p->fp = fp;
        p->share_stack =

                        ((void *)0)

                            ;
        p->save_stack.ptr =

                           ((void *)0)

                               ;
        return p;
    }
    (((__builtin_expect(!!(0), 1)))? (void)0 :aco_abort());
}


void aco_resume(aco_t* resume_co){
    (((__builtin_expect(!!(resume_co !=

   ((void *)0)

   && resume_co->main_co !=

   ((void *)0)

   && resume_co->is_end == 0), 1)))? (void)0 :aco_abort())

     ;
    if(resume_co->share_stack->owner != resume_co){
        if(resume_co->share_stack->owner !=

                                           ((void *)0)

                                               ){
            aco_t* owner_co = resume_co->share_stack->owner;
            (((__builtin_expect(!!(owner_co->share_stack == resume_co->share_stack), 1)))? (void)0 :aco_abort());

            (((__builtin_expect(!!(( (uintptr_t)(owner_co->share_stack->align_retptr) >= (uintptr_t)(owner_co->reg[14]) ) && ( (uintptr_t)(owner_co->share_stack->align_highptr) - (uintptr_t)(owner_co->share_stack->align_limit) <= (uintptr_t)(owner_co->reg[14]) )), 1)))? (void)0 :aco_abort())

             ;
            owner_co->save_stack.valid_sz =
                (uintptr_t)(owner_co->share_stack->align_retptr)
                -
                (uintptr_t)(owner_co->reg[14]);
            if(owner_co->save_stack.sz < owner_co->save_stack.valid_sz){
                free(owner_co->save_stack.ptr);
                owner_co->save_stack.ptr =

                                          ((void *)0)

                                              ;
                while(1){
                    owner_co->save_stack.sz = owner_co->save_stack.sz << 1;
                    (((__builtin_expect(!!(owner_co->save_stack.sz > 0), 1)))? (void)0 :aco_abort());
                    if(owner_co->save_stack.sz >= owner_co->save_stack.valid_sz){
                        break;
                    }
                }
                owner_co->save_stack.ptr = malloc(owner_co->save_stack.sz);
                do { if((__builtin_expect(!!((owner_co->save_stack.ptr) ==

               ((void *)0)

               ), 0))){ fprintf(

               stderr

               , "Aborting: failed to allocate memory: %s:%d:%s\n", "../3rd_party/libaco/aco.c", 429, __PRETTY_FUNCTION__); ; } } while(0);
            }


            if(owner_co->save_stack.valid_sz > 0) {







                memcpy(
                    owner_co->save_stack.ptr,
                    owner_co->reg[14],
                    owner_co->save_stack.valid_sz
                );

                owner_co->save_stack.ct_save++;
            }
            if(owner_co->save_stack.valid_sz > owner_co->save_stack.max_cpsz){
                owner_co->save_stack.max_cpsz = owner_co->save_stack.valid_sz;
            }
            owner_co->share_stack->owner =

                                          ((void *)0)

                                              ;
            owner_co->share_stack->align_validsz = 0;



        }
        (((__builtin_expect(!!(resume_co->share_stack->owner ==

       ((void *)0)

       ), 1)))? (void)0 :aco_abort());

        (((__builtin_expect(!!(resume_co->save_stack.valid_sz <= resume_co->share_stack->align_limit - sizeof(void*)), 1)))? (void)0 :aco_abort())



         ;


        if(resume_co->save_stack.valid_sz > 0) {

            memcpy(
                (void*)(
                    (uintptr_t)(resume_co->share_stack->align_retptr)
                    -
                    resume_co->save_stack.valid_sz
                ),
                resume_co->save_stack.ptr,
                resume_co->save_stack.valid_sz
            );

            resume_co->save_stack.ct_restore++;
        }
        if(resume_co->save_stack.valid_sz > resume_co->save_stack.max_cpsz){
            resume_co->save_stack.max_cpsz = resume_co->save_stack.valid_sz;
        }
        resume_co->share_stack->align_validsz = resume_co->save_stack.valid_sz + sizeof(void*);
        resume_co->share_stack->owner = resume_co;



    }
    (aco_gtls_co_ = (resume_co));
    acosw(resume_co->main_co, resume_co);
    (aco_gtls_co_ = (resume_co->main_co));
}

void aco_destroy(aco_t* co){
        if(co->share_stack->owner == co){
            co->share_stack->owner =

                                    ((void *)0)

                                        ;
            co->share_stack->align_validsz = 0;
        }
        free(co->save_stack.ptr);
        co->save_stack.ptr =

                            ((void *)0)

                                ;
        free(co);
}


/*



__asm__ ( ".text" );



__asm__ (".globl acosw");



__asm__ ("acosw:");

__asm__ (
    "mov     x2,  lr;"
    "stp    x16, x17, [x0, 0x00];"
    "stp    x19, x20, [x0, 0x10];"
    "stp    x21, x22, [x0, 0x20];"
    "stp    x23, x24, [x0, 0x30];"
    "stp    x25, x26, [x0, 0x40];"
    "stp    x27, x28, [x0, 0x50];"
    "stp    x29, lr,  [x0, 0x60];"
    "mov     x5,  sp;"
    "str     x5,  [x0, 0x70];"
);

__asm__ (
    "mrs     x5,  fpcr;"
    "str     x5,  [x1, 0x78];"
);

__asm__ (
    "ldp     x16, x17, [x1, 0x00];"
    "ldp     x19, x20, [x1, 0x10];"
    "ldp     x21, x22, [x1, 0x20];"
    "ldp     x23, x24, [x1, 0x30];"
    "ldp     x25, x26, [x1, 0x40];"
    "ldp     x27, x28, [x1, 0x50];"
    "ldp     x29, x30, [x1, 0x60];"
    "ldr     x3,  [x1, 0x70];"
    "mov     sp, x3;"
);

__asm__ (
    "ldr     x3,  [x1, 0x78];"
    "msr     fpcr,x3;"
);

__asm__ (
    "br x30;"
);

__asm__ (".globl aco_save_fpucw_mxcsr");




__asm__ ("aco_save_fpucw_mxcsr:");

__asm__ (
    "mrs x1, fpcr;"
    "str x1, [x0];"
    "ret;"
);



*/







metac_type_index_t MetaCSemantic_GetPtrTypeOf(metac_semantic_state_t* self,
                                              metac_type_index_t elementTypeIndex)
{
    uint32_t hash = elementTypeIndex.v;
    metac_type_ptr_t key =
            {{decl_type_typedef, 0, hash, 0},
             elementTypeIndex};

    metac_type_index_t result =
        MetaCTypeTable_GetOrEmptyPtrType(&self->PtrTypeTable, &key);
    if (result.v == 0)
    {
        result = MetaCTypeTable_AddPtrType(&self->PtrTypeTable, &key);
    }
    return result;
}

static inline

             _Bool

                  isBasicType(metac_type_kind_t typeKind)
{
    if ((typeKind >= type_void) & (typeKind <= type_unsigned_long_long))
    {
        return

              1

                  ;
    }
    return

          0

               ;
}


metac_type_index_t MetaCSemantic_GetTypeIndex(metac_semantic_state_t* state,
                                              metac_type_kind_t typeKind,
                                              decl_type_t* type)
{
    metac_type_index_t result = {0};

    if (isBasicType(typeKind))
    {
        result.v = (((type_index_basic) << 28) | ((uint32_t) typeKind));

        (((__builtin_expect(!!((type == ((void*)0x1)) || type->TypeKind == typeKind), 1)))? (void)0 :aco_abort());
        if ((type != ((void*)0x1)) && (type->TypeModifiers & typemod_unsigned))
        {
            if((typeKind >= type_char) & (typeKind <= type_long))
            {
                result.v +=
                    (((uint32_t)type_unsigned_char) - ((uint32_t)type_char));
            }
            else if (typeKind == type_long_long)
            {
                result.v +=
                    (((uint32_t)type_unsigned_long_long) - ((uint32_t)type_long_long));
            }
            else
            {

                fprintf(

                       stderr

                             , "modifier unsigned cannot be applied to: %s\n", TypeToChars(state, result));
            }
        }
    }

    return result;
}

static inline

             _Bool

                  IsAggregateTypeDecl(metac_declaration_kind_t declKind)
{
    if (declKind == decl_type_struct || declKind == decl_type_union)
    {
        return

              1

                  ;
    }
    return

          0

               ;
}

static inline

             _Bool

                  IsPointerType(metac_declaration_kind_t declKind)
{
    if (declKind == decl_type_ptr)
    {
        return

              1

                  ;
    }
    return

          0

               ;
}


static inline const char* BasicTypeToChars(metac_type_index_t typeIndex)
{
    (((__builtin_expect(!!(((metac_type_index_kind_t)((typeIndex).v >> 28)) == type_index_basic), 1)))? (void)0 :aco_abort());
    switch((metac_type_kind_t) ((typeIndex).v & 0xFFFFFfF))
    {
        case type_invalid :
            (((__builtin_expect(!!(0), 1)))? (void)0 :aco_abort());

        case type_type:
            return "type";

        case type_void :
            return "void";

        case type_bool :
            return "bool";
        case type_char:
            return "char";
        case type_short:
            return "short";
        case type_int :
            return "int";
        case type_long :
            return "long";
        case type_uint32_t:
            return "uint32_t";
        case type_long_long:
            return "long long";

        case type_float :
            return "float";
        case type_double :
            return "double";

        case type_unsigned_char:
            return "unsigned char";
        case type_unsigned_short:
            return "unsigned short";
        case type_unsigned_int:
            return "unsigned int";
        case type_unsigned_long :
            return "unsigned long";
        case type_unsigned_long_long:
            return "unsigned long long";

        default: (((__builtin_expect(!!(0), 1)))? (void)0 :aco_abort());
    }
    return 0;
}

static inline metac_type_index_t CommonBasicType(const metac_type_index_t a,
                                                 const metac_type_index_t b)
{
    metac_type_index_t result;
    result.v = (((type_index_basic) << 28) | (basic_int));

    if (a.v == (((type_index_basic) << 28) | (basic_long_double)) ||
        b.v == (((type_index_basic) << 28) | (basic_long_double)))
    {
        result.v = (((type_index_basic) << 28) | (basic_long_double));
    } else if (a.v == (((type_index_basic) << 28) | (basic_double)) ||
               b.v == (((type_index_basic) << 28) | (basic_double)))
    {
        result.v = (((type_index_basic) << 28) | (basic_double));
    } else if (a.v == (((type_index_basic) << 28) | (basic_float)) ||
               b.v == (((type_index_basic) << 28) | (basic_float)))
    {
        result.v = (((type_index_basic) << 28) | (basic_float));
    } else if (a.v == (((type_index_basic) << 28) | (type_unsigned_long_long)) ||
               b.v == (((type_index_basic) << 28) | (type_unsigned_long_long)))
    {
        result.v = (((type_index_basic) << 28) | (type_unsigned_long_long));
    } else if (a.v == (((type_index_basic) << 28) | (type_long_long)) ||
               b.v == (((type_index_basic) << 28) | (type_long_long)))
    {
        result.v = (((type_index_basic) << 28) | (type_long_long));
    } else if (a.v == (((type_index_basic) << 28) | (type_unsigned_long)) ||
               b.v == (((type_index_basic) << 28) | (type_unsigned_long)))
    {
        result.v = (((type_index_basic) << 28) | (type_unsigned_long));
    } else if (a.v == (((type_index_basic) << 28) | (type_long)) ||
               b.v == (((type_index_basic) << 28) | (type_long)))
    {
        result.v = (((type_index_basic) << 28) | (type_long));
    } else if (a.v == (((type_index_basic) << 28) | (type_unsigned_int)) ||
               b.v == (((type_index_basic) << 28) | (type_unsigned_int)))
    {
        result.v = (((type_index_basic) << 28) | (type_unsigned_int));
    }

    return result;
}





metac_type_index_t MetaCSemantic_CommonSubtype(metac_semantic_state_t* self,
                                               const metac_type_index_t a,
                                               const metac_type_index_t b)
{
        metac_type_index_t result = {-1};
    if (a.v == b.v)
        result = a;
    else
    {
        if (((metac_type_index_kind_t)((a).v >> 28)) == ((metac_type_index_kind_t)((b).v >> 28)))
        {
            switch(((metac_type_index_kind_t)((a).v >> 28)))
            {
                case type_index_basic:
                    result = CommonBasicType(a, b);
            }
        }
    }
        return result;
}

uint32_t FieldHash(metac_type_aggregate_field_t* field)
{




    uint32_t hash = ~0;
    hash = (crc32c_nozero(hash, &(field->Type), sizeof(field->Type)));
    hash = (crc32c_nozero(hash, &(field->Offset), sizeof(field->Offset)));
    (((__builtin_expect(!!(!field->Header.Hash || field->Header.Hash == hash), 1)))? (void)0 :aco_abort());
    return hash;
}

uint32_t AggregateHash(metac_type_aggregate_t* agg)
{
    if (agg->Header.Hash)
        return agg->Header.Hash;

    uint32_t hash = ~0;
    for(int i = 0; i < agg->FieldCount; i++)
    {
        metac_type_aggregate_field_t* field = agg->Fields +i;
        if (!field->Header.Hash)
        {
            field->Header.Hash = FieldHash(field);
        }
        hash = (crc32c_nozero(hash, &(field->Header.Hash), sizeof(field->Header.Hash)));
    }

    return hash;
}
static inline uint32_t Align(uint32_t size, uint32_t alignment)
{
    (((__builtin_expect(!!(alignment >= 1), 1)))? (void)0 :aco_abort());
    uint32_t alignmentMinusOne = (alignment - 1);
    uint32_t alignSize = (size + alignmentMinusOne);
    uint32_t alignMask = ~(alignmentMinusOne);
    return (alignSize & alignMask);
}


uint32_t MetaCSemantic_GetTypeAlignment(metac_semantic_state_t* self,
                                        metac_type_index_t typeIndex)
{
    uint32_t result = ((uint32_t)-1);

    if (((metac_type_index_kind_t)((typeIndex).v >> 28)) == type_index_basic)
    {
        uint32_t idx = ((typeIndex).v & 0xFFFFFfF);

        if ((idx >= type_unsigned_char)
         && (idx <= type_unsigned_long))
        {
            idx -= ((uint32_t)type_unsigned_char - (uint32_t)type_char);
        }
        else if (idx == type_unsigned_long_long)
        {
            idx = type_long_long;
        }
        result =
            MetaCTargetInfo_GetBasicAlign(&default_target_info, (basic_type_kind_t) idx);
    }
    else if (((metac_type_index_kind_t)((typeIndex).v >> 28)) == type_index_ptr
        || ((metac_type_index_kind_t)((typeIndex).v >> 28)) == type_index_functiontype)
    {
        result = default_target_info.AlignmentSizeT;
    }
    else if (((metac_type_index_kind_t)((typeIndex).v >> 28)) == type_index_typedef)
    {
        uint32_t idx = ((typeIndex).v & 0xFFFFFfF);
        metac_type_index_t elementTypeIndex =
            self->TypedefTypeTable.Slots[idx].Type;

        result = MetaCSemantic_GetTypeAlignment(self,
                                           elementTypeIndex);
    }
    else if (((metac_type_index_kind_t)((typeIndex).v >> 28)) == type_index_struct)
    {
        metac_type_aggregate_t* struct_ = StructPtr(self, ((typeIndex).v & 0xFFFFFfF));
        result = struct_->Alignment;
    }
    else if (((metac_type_index_kind_t)((typeIndex).v >> 28)) == type_index_array)
    {
        uint32_t idx = ((typeIndex).v & 0xFFFFFfF);
        metac_type_array_t* arrayType_ = ArrayTypePtr(self, idx);
        metac_type_index_t elementTypeIndex = arrayType_->ElementType;
        result = MetaCSemantic_GetTypeAlignment(self, elementTypeIndex);
    }

    else
    {
        (((__builtin_expect(!!(0), 1)))? (void)0 :aco_abort());
    }

    return result;
}


uint32_t MetaCSemantic_GetTypeSize(metac_semantic_state_t* self,
                                   metac_type_index_t typeIndex)
{
    uint32_t result = ((uint32_t)-1);

    if (((metac_type_index_kind_t)((typeIndex).v >> 28)) == type_index_basic)
    {
        uint32_t idx = ((typeIndex).v & 0xFFFFFfF);

        if ((idx >= type_unsigned_char)
         && (idx <= type_unsigned_long))
        {
            idx -= ((uint32_t)type_unsigned_char - (uint32_t)type_char);
        }
        else if (idx == type_unsigned_long_long)
        {
            idx = type_long_long;
        }
        result =
            MetaCTargetInfo_GetBasicSize(&default_target_info, (basic_type_kind_t) idx);
    }
    else if (((metac_type_index_kind_t)((typeIndex).v >> 28)) == type_index_ptr
        || ((metac_type_index_kind_t)((typeIndex).v >> 28)) == type_index_functiontype)
    {
        result = default_target_info.PtrSize;
    }
    else if (((metac_type_index_kind_t)((typeIndex).v >> 28)) == type_index_typedef)
    {
        uint32_t idx = ((typeIndex).v & 0xFFFFFfF);
        metac_type_index_t elementTypeIndex =
            self->TypedefTypeTable.Slots[idx].Type;

        result = MetaCSemantic_GetTypeSize(self,
                                           elementTypeIndex);
    }
    else if (((metac_type_index_kind_t)((typeIndex).v >> 28)) == type_index_struct)
    {
        metac_type_aggregate_t* struct_ = StructPtr(self, ((typeIndex).v & 0xFFFFFfF));
        result = struct_->Size;
    }
    else if (((metac_type_index_kind_t)((typeIndex).v >> 28)) == type_index_array)
    {
        uint32_t idx = ((typeIndex).v & 0xFFFFFfF);
        metac_type_array_t* arrayType = ArrayTypePtr(self, idx);
        metac_type_index_t elementTypeIndex = arrayType->ElementType;

        uint32_t baseSize = MetaCSemantic_GetTypeSize(self,
                                                      elementTypeIndex);
        uint32_t alignedSize = Align(baseSize,
                                     MetaCSemantic_GetTypeAlignment(self,
                                                                    elementTypeIndex));
        result = alignedSize * arrayType->Dim;
    }
    else
    {
        (((__builtin_expect(!!(0), 1)))? (void)0 :aco_abort());
    }

    return result;
}

metac_type_aggregate_t* MetaCSemantic_PersistTemporaryAggregate(metac_semantic_state_t* self,
                                                                metac_type_aggregate_t* tmpAgg)
{


    metac_type_index_t typeIndex =
        MetaCTypeTable_AddStructType(&self->StructTypeTable, tmpAgg);
    metac_type_aggregate_t* semaAgg = StructPtr(self, ((typeIndex).v & 0xFFFFFfF));
    metac_type_aggregate_field_t* semaFields = (metac_type_aggregate_field_t*)
            calloc(sizeof(metac_type_aggregate_field_t), tmpAgg->FieldCount);




    memcpy(semaFields, tmpAgg->Fields,
        sizeof(metac_type_aggregate_field_t) * tmpAgg->FieldCount);
    semaAgg->Fields = semaFields;

    return semaAgg;
}

metac_type_index_t MetaCSemantic_TypeSemantic(metac_semantic_state_t* self,
                                              decl_type_t* type)
{
    metac_type_index_t result = {0};

    metac_type_kind_t typeKind = type->TypeKind;

    if (type->DeclKind == decl_type && isBasicType(typeKind))
    {
        result = MetaCSemantic_GetTypeIndex(self, typeKind, type);
    }
    else if (type->DeclKind == decl_type_array)
    {
        decl_type_array_t* arrayType =
            (decl_type_array_t*)type;
        metac_type_index_t elementType =
            MetaCSemantic_doTypeSemantic_(self, ((decl_type_t*)(arrayType->ElementType)), "../metac_type_semantic.c", 412);
        metac_sema_expression_t* dim =
            MetaCSemantic_doExprSemantic_(self, ((metac_expression_t*)(arrayType->Dim)), 0, "../metac_type_semantic.c", 414);
        if (dim->Kind != exp_signed_integer)
        {
            printf("Array dimension should eval to integer but it is: %s\n",
                MetaCExpressionKind_toChars(dim->Kind));
        }
        result =
            MetaCSemantic_GetArrayTypeOf(self,
                                        elementType,
                                        (uint32_t)dim->ValueU64);
    }
    else if (type->DeclKind == decl_type_typedef)
    {
        decl_type_typedef_t* typedef_ = (decl_type_typedef_t*) type;
        metac_type_index_t elementTypeIndex =
            MetaCSemantic_doTypeSemantic_(self, ((decl_type_t*)(typedef_->Type)), "../metac_type_semantic.c", 429);

        uint32_t hash = elementTypeIndex.v;

        metac_type_typedef_t key = {
            {decl_type_typedef, 0, hash},
            elementTypeIndex, typedef_->Identifier
        };

        result =
            MetaCTypeTable_GetOrEmptyTypedefType(&self->TypedefTypeTable, &key);
        if (result.v == 0)
        {
            result = MetaCTypeTable_AddTypedefType(&self->TypedefTypeTable, &key);
        }
        metac_type_typedef_t* semaTypedef = TypedefPtr(self, ((result).v & 0xFFFFFfF));


        scope_insert_error_t scopeInsertError =
            MetaCSemantic_RegisterInScope(self, typedef_->Identifier, (metac_node_t)semaTypedef);
    }
    else if (IsAggregateTypeDecl(type->DeclKind))
    {
        decl_type_struct_t* agg = (decl_type_struct_t*) type;
        if (type->DeclKind == decl_type_struct)
            typeKind = type_struct;
        else if (type->DeclKind == decl_type_union)
            typeKind = type_union;
        else
            (((__builtin_expect(!!(0), 1)))? (void)0 :aco_abort());

       metac_type_aggregate_field_t* tmpFields =
            (metac_type_aggregate_field_t*)
                                malloc(sizeof(metac_type_aggregate_field_t)
                                * agg->FieldCount);

        memset(tmpFields, 0x0, sizeof(*tmpFields) * agg->FieldCount);

        metac_type_aggregate_t tmpSemaAggMem = {(metac_declaration_kind_t)0};
        tmpSemaAggMem.Header.Kind = agg->DeclKind;
        metac_type_aggregate_t* tmpSemaAgg = &tmpSemaAggMem;
        tmpSemaAgg->Fields = tmpFields;
        tmpSemaAgg->FieldCount = agg->FieldCount;
        metac_scope_t tmpScopeMem = { scope_flag_temporary };
        MetaCScopeTable_Init(&tmpScopeMem.ScopeTable);

        metac_scope_parent_kind_t scopeKind = scope_parent_invalid;

        switch(typeKind)
        {
            case type_struct:
                scopeKind = scope_parent_struct;
            break;
            case type_union:
                scopeKind = scope_parent_union;
            break;
            default: (((__builtin_expect(!!(0), 1)))? (void)0 :aco_abort());
        }


        tmpSemaAgg->Scope = MetaCSemantic_PushTemporaryScope_(self, &tmpScopeMem, 500, "../metac_type_semantic.c");

        switch(typeKind)
        {
            case type_struct:
            {
                MetaCSemantic_ComputeStructLayoutPopulateScope(self, agg, tmpSemaAgg);

                uint32_t hash = AggregateHash(tmpSemaAgg);
                tmpSemaAgg->Header.Hash = hash;

                result =
                    MetaCTypeTable_GetOrEmptyStructType(&self->StructTypeTable, tmpSemaAgg);
                if (result.v == 0)
                {
                    metac_type_aggregate_t* semaAgg;
                    semaAgg =
                        MetaCSemantic_PersistTemporaryAggregate(self, tmpSemaAgg);

                    result.v = (((type_index_struct) << 28) | (semaAgg - self->StructTypeTable.Slots));
                }

            } break;

            case type_union:
            {

            } break;

            case type_class:
            {
                (((__builtin_expect(!!(0), 1)))? (void)0 :aco_abort());

            } break;

            default: (((__builtin_expect(!!(0), 1)))? (void)0 :aco_abort());

        }
        MetaCScopeTable_Free(&tmpSemaAgg->Scope->ScopeTable);
        free(tmpFields);
        MetaCSemantic_PopTemporaryScope_(self, 540, "../metac_type_semantic.c");
    }
    else if (IsPointerType(type->DeclKind))
    {
        metac_type_index_t elementTypeIndex = {0};
        decl_type_ptr_t* typePtr = (decl_type_ptr_t*) type;
        elementTypeIndex =
            MetaCSemantic_doTypeSemantic_(self, ((decl_type_t*)(typePtr->ElementType)), "../metac_type_semantic.c", 547);
        (((__builtin_expect(!!(elementTypeIndex.v && elementTypeIndex.v != -1), 1)))? (void)0 :aco_abort());
        result = MetaCSemantic_GetPtrTypeOf(self, elementTypeIndex);
    }
    else if (type->DeclKind == decl_type_functiontype)
    {
        decl_type_functiontype_t* functionType =
            (decl_type_functiontype_t*) type;
        metac_scope_t tmpScope = { scope_flag_temporary };
        MetaCScopeTable_Init(&tmpScope.ScopeTable);

        MetaCSemantic_PushTemporaryScope_(self, &tmpScope, 558, "../metac_type_semantic.c");

        metac_type_index_t returnType =
            MetaCSemantic_doTypeSemantic_(self, ((decl_type_t*)(functionType->ReturnType)),
                                        "../metac_type_semantic.c"

            ,
                                        562

            )
                                         ;

        uint32_t hash = crc32c_nozero(~0, &returnType, sizeof(returnType));
        decl_parameter_t* currentParam = functionType->Parameters;

        const uint32_t nParams = functionType->ParameterCount;
        metac_type_index_t* parameterTypes = (metac_type_index_t*)
            malloc(sizeof(metac_type_index_t) * nParams);

        for(uint32_t i = 0;
            i < nParams;
            i++)
        {
            parameterTypes[i] =
                MetaCSemantic_doTypeSemantic_(self, ((decl_type_t*)(currentParam->Parameter->VarType)), "../metac_type_semantic.c", 576);
            currentParam = currentParam->Next;
        }

        hash = crc32c_nozero(hash, &parameterTypes, sizeof(*parameterTypes) * nParams);
        metac_type_functiontype_t key = {
            {decl_type_functiontype, hash, 0, },
            returnType, parameterTypes, nParams
        };

        MetaCSemantic_PopTemporaryScope_(self, 586, "../metac_type_semantic.c");

        result = MetaCTypeTable_GetOrEmptyFunctionType(&self->FunctionTypeTable, &key);
        if (result.v == 0)
        {
            result =
                MetaCTypeTable_AddFunctionType(&self->FunctionTypeTable, &key);
        }
    }
    else if (type->DeclKind == decl_type && type->TypeKind == type_identifier)
    {


LtryAgian: {}
        metac_node_t node =
            MetaCSemantic_LookupIdentifier(self, type->TypeIdentifier);
        {
            printf("Lookup: %s\n", ((node != (metac_node_header_t*) 0x1) ?
                                       MetaCNodeKind_toChars(node->Kind) :
                                       "empty"));
            if (node == (metac_node_header_t*) 0x1)
            {





                aco_t* me = (aco_t*)CurrentFiber();
                task_t* task = CurrentTask();
                printf("Yield!\n");
                metac_semantic_waiter_t* meWaiter = &self->Waiters.Waiters[(self->Waiters.WaiterCount++)];
                meWaiter->FuncHash = (crc32c_nozero(~(uint32_t)0, "MetaCSemantic_LookupIdentifier", sizeof("MetaCSemantic_LookupIdentifier") - 1));
                meWaiter->NodeHash = (crc32c_nozero(~0, &(type->TypeIdentifier), sizeof(type->TypeIdentifier)));
                meWaiter->Continuation = me;
                task->TaskFlags |= Task_Waiting;
                do { do { do { (((__builtin_expect(!!((((aco_gtls_co_))) !=

               ((void *)0)

               ), 1)))?((void)0):(aco_abort())); (((__builtin_expect(!!((((aco_gtls_co_))->main_co) !=

               ((void *)0)

               ), 1)))?((void)0):(aco_abort())); acosw((aco_gtls_co_), (aco_gtls_co_)->main_co); } while(0); } while(0); } while(0);
                printf("Trying agian after yielding\n");
                goto LtryAgian;



            }
        }
        if (node != (metac_node_header_t*) 0x1 &&
            node->Kind == node_decl_type_typedef)
        {
            metac_type_typedef_t* typedef_ = (metac_type_typedef_t*)node;
            result = typedef_->Type;
        }
        if (node != (metac_node_header_t*) 0x1 &&
            node->Kind == node_decl_type_struct)
        {
            metac_type_aggregate_t* struct_ = (metac_type_aggregate_t*)node;
            result.v = (((type_index_struct) << 28) | (StructIndex(self, (struct_))));
        }







    }
    else
    {
        (((__builtin_expect(!!(0), 1)))? (void)0 :aco_abort());
    }

    const uint32_t funcHash = crc32c(~0, "MetaCSemantic_doTypeSemantic",
                                  sizeof("MetaCSemantic_doTypeSemantic") -1);

    uint32_t nodeHash = type->Hash;

    for(uint32_t waiterIdx = 0;
        waiterIdx < self->Waiters.WaiterCount;
        waiterIdx++)
    {
        metac_semantic_waiter_t waiter = self->Waiters.Waiters[waiterIdx];
        if (waiter.FuncHash == funcHash
         && waiter.NodeHash == nodeHash)
        {
            printf("Found someone waiting for me\n");
            do { aco_resume(waiter.Continuation); } while(0);;
        }
    }

    (((__builtin_expect(!!(result.v != 0), 1)))? (void)0 :aco_abort());
    return result;
}


void MetaCSemantic_doTypeSemantic_Task(task_t* task)
{
    MetaCSemantic_doTypeSemantic_Fiber_t* ctx =
        (MetaCSemantic_doTypeSemantic_Fiber_t*) task->Context;
    metac_semantic_state_t* sema = ctx->Sema;
    decl_type_t* type = ctx->Type;

    ctx->Result = MetaCSemantic_TypeSemantic(sema, type);

}


void MetaCSemantic_doTypeSemantic_Fiber(void* caller, void* arg)
{
    MetaCSemantic_doTypeSemantic_Fiber_t* ctx =
        (MetaCSemantic_doTypeSemantic_Fiber_t*) arg;
    metac_semantic_state_t* sema = ctx->Sema;
    decl_type_t* type = ctx->Type;

    ctx->Result = MetaCSemantic_TypeSemantic(sema, type);

}


metac_type_index_t MetaCSemantic_doTypeSemantic_(metac_semantic_state_t* self,
                                                 decl_type_t* type,
                                                 const char* callFile,
                                                 uint32_t callLine)
{
    metac_type_index_t result = {0};







    result = MetaCSemantic_TypeSemantic(self, type);
    if (!result.v)
    {

        worker_context_t currentContext = *CurrentWorker();

        MetaCSemantic_doTypeSemantic_Fiber_t arg = {
                self, type
        };
        MetaCSemantic_doTypeSemantic_Fiber_t* argPtr = &arg;

        do { uint8_t* ctxMem = (uint8_t*)

       __builtin_alloca (

       sizeof(taskcontext_t) + sizeof(*(argPtr))

       )

       ; taskcontext_t* taskCtx = (taskcontext_t*) ctxMem; uint8_t* ctxPtr = ctxMem + sizeof(taskcontext_t); memcpy(ctxPtr, argPtr, sizeof(*(argPtr))); taskcontext_t ctx = { crc32c(~0, "MetaCSemantic_doTypeSemantic", sizeof("MetaCSemantic_doTypeSemantic") - 1), ctxPtr, sizeof(*(argPtr)), "../metac_type_semantic.c", 725 }; *taskCtx = ctx; (*(void**)(&argPtr)) = (void*)ctxPtr; } while (0);;
        task_t typeSemTask = {0};

        typeSemTask.Context = argPtr;
        typeSemTask.ContextSize = sizeof(arg);
        typeSemTask.TaskFunction = MetaCSemantic_doTypeSemantic_Task;

        while (!TaskQueue_Push(&currentContext.Queue, &typeSemTask))
        {

            do { do { do { (((__builtin_expect(!!((((aco_gtls_co_))) !=

           ((void *)0)

           ), 1)))?((void)0):(aco_abort())); (((__builtin_expect(!!((((aco_gtls_co_))->main_co) !=

           ((void *)0)

           ), 1)))?((void)0):(aco_abort())); acosw((aco_gtls_co_), (aco_gtls_co_)->main_co); } while(0); } while(0); } while(0);
        }

        uint32_t funcHash = (crc32c_nozero(~(uint32_t)0, "MetaCSemantic_doTypeSemantic", sizeof("MetaCSemantic_doTypeSemantic") - 1));
        uint32_t nodeHash = type->TypeHeader.Hash;

        for(uint32_t waiterIdx = 0;
            waiterIdx < self->Waiters.WaiterCount;
            waiterIdx++)
        {
            metac_semantic_waiter_t waiter = self->Waiters.Waiters[waiterIdx];
            if (funcHash == waiter.FuncHash && nodeHash == waiter.NodeHash)
            {
                do { acosw((aco_gtls_co_), waiter.Continuation); } while(0);;
            }
        }

    result = arg.Result;



    }
    return result;
}

metac_type_index_t MetaCSemantic_GetArrayTypeOf(metac_semantic_state_t* state,
                                                metac_type_index_t elementTypeIndex,
                                                uint32_t dimension)
{
    uint32_t hash = EntangleInts(((elementTypeIndex).v & 0xFFFFFfF), dimension);
    metac_type_array_t key = {
                {decl_type_array, 0, hash}, elementTypeIndex, dimension};

    metac_type_index_t result =
        MetaCTypeTable_GetOrEmptyArrayType(&state->ArrayTypeTable, &key);

    if (result.v == 0)
    {
        result =
            MetaCTypeTable_AddArrayType(&state->ArrayTypeTable, &key);
    }

    return result;
}



_Bool

    ComputeStructLayout(metac_semantic_state_t* self, decl_type_t* type)
{
    return

          0

               ;
}



_Bool

    MetaCSemantic_ComputeStructLayoutPopulateScope(metac_semantic_state_t* self,
                                                    decl_type_struct_t* agg,
                                                    metac_type_aggregate_t* semaAgg)
{


   _Bool

        result =

                 1

                     ;

    (((__builtin_expect(!!(semaAgg->Fields && semaAgg->Fields != ((void*)0x1)), 1)))? (void)0 :aco_abort());

    uint32_t currentFieldOffset = 0;
    uint32_t alignment = 1;

    metac_type_aggregate_field_t* onePastLast =
        semaAgg->Fields + semaAgg->FieldCount;
    decl_field_t* declField = agg->Fields;


    for(metac_type_aggregate_field_t* semaField = semaAgg->Fields;
        semaField < onePastLast;
        semaField++)
    {
        semaField->Identifier = declField->Field->VarIdentifier;

        semaField->Type =
            MetaCSemantic_doTypeSemantic_(self, ((decl_type_t*)(declField->Field->VarType)), "../metac_type_semantic.c", 808);

        if (!semaField->Type.v)
        {
            printf("FieldType couldn't be resolved\n yielding fiber\n");
            aco_t* me = (aco_t*)CurrentFiber();
            if (me != 0)
            {
                metac_semantic_waiter_t waiter;
                waiter.FuncHash = (crc32c_nozero(~(uint32_t)0, "MetaCSemantic_doTypeSemantic", sizeof("MetaCSemantic_doTypeSemantic") - 1));
                waiter.NodeHash = declField->Field->VarType->TypeHeader.Hash;
                waiter.Continuation = me;

                (((__builtin_expect(!!(self->Waiters.WaiterCount < self->Waiters.WaiterCapacity), 1)))? (void)0 :aco_abort());
                self->Waiters.Waiters[self->Waiters.WaiterCount++] = waiter;
                printf("We should Yield\n");
                ((task_t*)(me->arg))->TaskFlags |= Task_Waiting;
                do { do { do { (((__builtin_expect(!!((((aco_gtls_co_))) !=

               ((void *)0)

               ), 1)))?((void)0):(aco_abort())); (((__builtin_expect(!!((((aco_gtls_co_))->main_co) !=

               ((void *)0)

               ), 1)))?((void)0):(aco_abort())); acosw((aco_gtls_co_), (aco_gtls_co_)->main_co); } while(0); } while(0); } while(0);
                printf("Now we should be able to resolve\n");
                semaField->Type =
                    MetaCSemantic_doTypeSemantic_(self, ((decl_type_t*)(declField->Field->VarType)), "../metac_type_semantic.c", 828);
            }

        }

        declField = declField->Next;
    }

    uint32_t maxAlignment = semaAgg->FieldCount ?
        MetaCSemantic_GetTypeAlignment(self, semaAgg->Fields->Type) :
        ~0;

    for(metac_type_aggregate_field_t* semaField = semaAgg->Fields;
        semaField < onePastLast;
        semaField++)
    {
        uint32_t alignedSize = MetaCSemantic_GetTypeSize(self, semaField->Type);
        (((__builtin_expect(!!(alignedSize != ((uint32_t)-1)), 1)))? (void)0 :aco_abort());
        if (semaField < (onePastLast - 1))
        {
            metac_type_aggregate_field_t *nextField = semaField + 1;
            uint32_t requestedAligment =
                MetaCSemantic_GetTypeAlignment(self, nextField->Type);
            (((__builtin_expect(!!(requestedAligment != -1), 1)))? (void)0 :aco_abort());
            if (requestedAligment > maxAlignment)
                maxAlignment = requestedAligment;
            alignedSize = Align(alignedSize, requestedAligment);
            (((__builtin_expect(!!(((currentFieldOffset + alignedSize) % requestedAligment) == 0), 1)))? (void)0 :aco_abort());
        }
        semaField->Offset = currentFieldOffset;
        MetaCScope_RegisterIdentifier(semaAgg->Scope,
                                      semaField->Identifier,
                                      (metac_node_t)semaField);
        currentFieldOffset += alignedSize;
    }


    semaAgg->Size = currentFieldOffset;
    semaAgg->Alignment = maxAlignment;

    fprintf(

           stderr

                 , "sizeof(struct) = %u\n", semaAgg->Size);
    fprintf(

           stderr

                 , "Alignof(struct) = %u\n", semaAgg->Alignment);

    return result;
}








void MetaCSemantic_doExprSemantic_Task(task_t* task)
{
    MetaCSemantic_doExprSemantic_task_context_t* ctx
        = (MetaCSemantic_doExprSemantic_task_context_t*) task->Context;

    ctx->Result = MetaCSemantic_doExprSemantic_(ctx->Sema, ctx->Expr, ctx->Result,
                                                task->Origin.Func, task->Origin.File);
}


static

      _Bool

           IsAggregateType(metac_type_index_kind_t typeKind)
{
    switch(typeKind)
    {
        case type_index_struct:
        case type_index_union:
        case type_index_class:
            return

                  1

                      ;
    }

    return

          0

               ;
}

metac_sema_expression_t* MetaCSemantic_doExprSemantic_(metac_semantic_state_t* self,
                                                       metac_expression_t* expr,
                                                       metac_sema_expression_t* result,
                                                       const char* callFun,
                                                       uint32_t callLine)
{
    if (!result)
    {
        result = AllocNewSemaExpression(self, expr);
    }

    if (IsBinaryExp(expr->Kind)
        && (expr->Kind != exp_arrow && expr->Kind != exp_dot))
    {
        MetaCSemantic_PushExpr(self, result);

        result->E1 = MetaCSemantic_doExprSemantic_(self, ((metac_expression_t*)(expr->E1)), 0, "../metac_expr_semantic.c", 52);
        result->E2 = MetaCSemantic_doExprSemantic_(self, ((metac_expression_t*)(expr->E2)), 0, "../metac_expr_semantic.c", 53);

        MetaCSemantic_PopExpr(self, result);
    }

    switch(expr->Kind)
    {
        case exp_invalid:
            (((__builtin_expect(!!(0), 1)))? (void)0 :aco_abort());

        case exp_arrow:
        case exp_dot:
        {
            result->E1 = MetaCSemantic_doExprSemantic_(self, ((metac_expression_t*)(expr->E1)), 0, "../metac_expr_semantic.c", 66);


            const metac_type_index_kind_t typeIndexKind
                = ((metac_type_index_kind_t)((result->E1->TypeIndex).v >> 28));
            uint32_t typeIndexIndex = ((result->E1->TypeIndex).v & 0xFFFFFfF);

            if (IsAggregateType(typeIndexKind))
            {

                metac_type_aggregate_t* agg = 0;
                switch(typeIndexKind)
                {
                case type_index_struct:
                    agg = StructPtr(self, typeIndexIndex);
                break;
                case type_index_union:
                    agg = UnionPtr(self, typeIndexIndex);
                break;
                case type_index_class:
                    (((__builtin_expect(!!(0), 1)))? (void)0 :aco_abort());
                }
                (((__builtin_expect(!!(agg != 0), 1)))? (void)0 :aco_abort());

                (((__builtin_expect(!!(expr->E2->Kind == exp_identifier), 1)))? (void)0 :aco_abort());

                result->AggExp =
                    MetaCSemantic_doExprSemantic_(self, ((metac_expression_t*)(expr->E1)), 0, "../metac_expr_semantic.c", 108);

                metac_type_aggregate_field_t* fields = agg->Fields;
                for(uint32_t i = 0;
                    i < agg->FieldCount;
                    i++)
                {
                    metac_type_aggregate_field_t field = fields[i];
                    if (field.Identifier.v == expr->E2->IdentifierPtr.v)
                    {


                        result->TypeIndex = field.Type;
                        result->AggOffset = field.Offset;
                    }
                }

            }
        } break;

        case exp_typeof:
        {
            metac_sema_expression_t* E1 =
                MetaCSemantic_doExprSemantic_(self, ((metac_expression_t*)(expr->E1)), 0, "../metac_expr_semantic.c", 131);

            result->TypeIndex.v =
                (((type_index_basic) << 28) | (type_type));
            result->TypeExp = E1->TypeIndex;
        } break;

        case exp_compl:
        case exp_not:
        case exp_umin:
        case exp_paren:
        {
            metac_sema_expression_t* E1 =
                MetaCSemantic_doExprSemantic_(self, ((metac_expression_t*)(expr->E1)), 0, "../metac_expr_semantic.c", 144);

            result->TypeIndex = E1->TypeIndex;
            result->E1 = E1;
        } break;



        case exp_add: case exp_sub: case exp_mul: case exp_div: case exp_rem: case exp_xor: case exp_or: case exp_and: case exp_lsh: case exp_rsh:
        case exp_ternary:
            result->TypeIndex =
                MetaCSemantic_CommonSubtype(self, result->E1->TypeIndex, result->E2->TypeIndex);
        break;
        case exp_add_ass: case exp_sub_ass: case exp_mul_ass: case exp_div_ass: case exp_rem_ass: case exp_xor_ass: case exp_or_ass: case exp_and_ass: case exp_lsh_ass: case exp_rsh_ass:
            result->TypeIndex = result->E1->TypeIndex;
        break;
        case exp_index:
            result = MetaCSemantic_doIndexSemantic_(self, expr, "../metac_expr_semantic.c", 161);
        break;
        case exp_char :
            result->TypeIndex = MetaCSemantic_GetTypeIndex(self, type_char, (decl_type_t*)((void*)0x1));
        break;
        case exp_string :
            result->TypeIndex = MetaCSemantic_GetArrayTypeOf(self,
                MetaCSemantic_GetTypeIndex(self, type_char, (decl_type_t*)((void*)0x1)),
                ( (expr->StringKey) >> 12 ) + 1);
        break;

        case exp_signed_integer :
            result->TypeIndex = MetaCSemantic_GetTypeIndex(self, type_int, (decl_type_t*)((void*)0x1));
        break;
        case exp_tuple:
        {
            exp_tuple_t* tupleElement = expr->TupleExpressionList;
            const uint32_t tupleExpressionCount =
                expr->TupleExpressionCount;
            if (expr->TupleExpressionCount > 128)
            {
                fprintf(

               stderr

               , "SemanticError[%s:%u]: " "Tuples with more than 128 elements are currently not supported, given %u\n" "\n",

                                        "../metac_expr_semantic.c"

                ,

                                        184

                , tupleExpressionCount)

                                         ;
                return 0;
            }
            result->TupleExpressionCount = tupleExpressionCount;
            for(uint32_t i = 0;
                i < expr->TupleExpressionCount;
                i++)
            {
                metac_expression_t *e = tupleElement->Expression;
                tupleElement = tupleElement->Next;
                metac_sema_expression_t* resultElem = result->TupleExpressions + i;
                MetaCSemantic_doExprSemantic_(self, ((metac_expression_t*)(e)), resultElem, "../metac_expr_semantic.c", 195);
            }

            metac_type_index_t typeIndicies[128];

            for(uint32_t i = 0; i < tupleExpressionCount; i++)
            {
                typeIndicies[i] = (result->TupleExpressions + i)->TypeIndex;
            }

           uint32_t hash = crc32c(~0, typeIndicies,
                sizeof(metac_type_index_t) * expr->TupleExpressionCount);

            metac_type_tuple_t typeTuple;
            typeTuple.Header.Kind = decl_type_tuple;
            typeTuple.Header.Hash = hash;
            typeTuple.typeCount = tupleExpressionCount;
            typeTuple.typeIndicies = typeIndicies;


            metac_type_index_t tupleIdx =
                MetaCTypeTable_GetOrEmptyTupleType(&self->TupleTypeTable, &typeTuple);
            if (tupleIdx.v == 0)
            {
                metac_type_index_t* newIndicies = (metac_type_index_t*)
                    malloc(expr->TupleExpressionCount * sizeof(metac_type_index_t));
                memcpy(newIndicies, typeIndicies,
                    expr->TupleExpressionCount * sizeof(metac_type_index_t));
                typeTuple.typeIndicies = newIndicies;
                tupleIdx =
                    MetaCTypeTable_AddTupleType(&self->TupleTypeTable, &typeTuple);
            }
            result->TypeIndex = tupleIdx;
        }
        break;
        case exp_dot_compiler:
        {
            if (expr->E1->Kind != exp_call)
            {
                fprintf(

                       stderr

                             , "Only calls are supported not %s\n",
                    MetaCExpressionKind_toChars(expr->E1->Kind));
                break;
            }
            metac_expression_t* call = expr->E1;
            metac_expression_t* fn = call->E1;
            exp_argument_t* args = ((*(metac_node_t*)(&call->E2)) != (metac_node_header_t*) 0x1 ?
                call->E2->ArgumentList : (exp_argument_t*)(metac_node_header_t*) 0x1);

            printf("Type(fn) %s\n", MetaCExpressionKind_toChars(fn->Kind));

            int callIdx = -1;

            for(int memberIdx = 0;
                memberIdx < self->CompilerInterface->FieldCount;
                memberIdx++)
            {
                metac_identifier_ptr_t id =
                    self->CompilerInterface->Fields[memberIdx].Identifier;
                if (id.v == fn->IdentifierPtr.v)
                {
                    printf("Field: %s\n",
                        IdentifierPtrToCharPtr(self->ParserIdentifierTable, id));

                    printf("Found\n");
                    callIdx = memberIdx;
                    break;
                }
            }

            if (callIdx == -1)
            {
                printf("CallNotFound\n");
                result->Kind = exp_signed_integer;
                result->ValueI64 = 0;
                result->TypeIndex.v = 0;
            }
            else
            {

            }

        } break;
        case exp_type:
        {
            metac_type_index_t TypeIndex
                = MetaCSemantic_doTypeSemantic_(self, ((decl_type_t*)(expr->TypeExp)), "../metac_expr_semantic.c", 280);
            result->TypeExp = TypeIndex;
            result->TypeIndex.v = (((type_index_basic) << 28) | (type_type));
        } break;
        case exp_sizeof:
        {
            uint32_t size = -1;
            metac_sema_expression_t* e1 =
                MetaCSemantic_doExprSemantic_(self, ((metac_expression_t*)(expr->E1)), 0, "../metac_expr_semantic.c", 288);
            metac_type_index_t type = e1->TypeIndex;
            if (type.v == (((type_index_basic) << 28) | (type_type)))
            {
                type = e1->TypeExp;
            }

            if (e1->TypeIndex.v != 0 && e1->TypeIndex.v != -1)
                size = MetaCSemantic_GetTypeSize(self, type);

            result->TypeIndex.v = (((type_index_basic) << 28) | (type_uint32_t));
            result->Kind = exp_signed_integer;
            result->ValueU64 = size;
        } break;
        case exp_identifier:
        {
            metac_node_t node =
                MetaCSemantic_LookupIdentifier(self,
                                               result->IdentifierPtr);
            if (node == ((void*)0x1))
            {
                fprintf(

                       stderr

                             , "Identifier lookup failed\n");
            }
            else
            {
                if (node->Kind == (metac_node_kind_t)exp_identifier)
                {
                    fprintf(

                           stderr

                                 , "we should not be retured an identifier\n");
                }
                if (node->Kind == node_decl_variable)
                {
                    sema_decl_variable_t* v = (sema_decl_variable_t*)node;
                    result->Kind = exp_variable;
                    result->Variable = v;
                    result->TypeIndex = v->TypeIndex;
                }
            }
        }
        break;
        case exp_addr:
            MetaCSemantic_PushExpr(self, result);
            result->E1 = MetaCSemantic_doExprSemantic_(self, ((metac_expression_t*)(expr->E1)), 0, "../metac_expr_semantic.c", 329);
            MetaCSemantic_PopExpr(self, result);
            (((__builtin_expect(!!(result->E1->TypeIndex.v != 0 && result->E1->TypeIndex.v != -1), 1)))? (void)0 :aco_abort());
            if (!MetaCSemantic_CanHaveAddress(self, expr->E1))
            {
                result->TypeIndex.v = -1;
                const char* e1String = "E1";



                fprintf(

               stderr

               , "SemanticError[%s:%u]: " "cannot take the address of %s" "\n", "../metac_expr_semantic.c", 339, e1String);
            }
            else
            {
                result->TypeIndex = MetaCSemantic_GetPtrTypeOf(self, result->E1->TypeIndex);
            }
        break;
    }

    return result;
}

void MetaCSemantic_PushExpr(metac_semantic_state_t* self, metac_sema_expression_t* expr)
{
    if (self->ExpressionStackCapacity < self->ExpressionStackSize)
    {
        (((__builtin_expect(!!(0), 1)))? (void)0 :aco_abort());

    }
}

void MetaCSemantic_PopExpr(metac_semantic_state_t* self, metac_sema_expression_t* expr)
{

}



_Bool

    MetaCSemantic_CanHaveAddress(metac_semantic_state_t* self,
                                  metac_expression_t* expr)
{
    switch (expr->Kind)
    {
        case exp_identifier:
            return

                  1

                      ;
        default: return

                       0

                            ;
    }
}



const char* MetaCExpressionKind_toChars(metac_expression_kind_t);
extern void* CurrentFiber(void);
extern task_t* CurrentTask(void);




_Bool

    IsExpressionNode(metac_node_kind_t);



_Bool

    Expression_IsEqual_(const metac_sema_expression_t* a,
                         const metac_sema_expression_t* b)
{


   _Bool

        result =

                 1

                     ;
    if (a == b)
        (((__builtin_expect(!!(0), 1)))? (void)0 :aco_abort());


    if (a->Kind == b->Kind)
    {
        switch(a->Kind)
        {
            case exp_signed_integer:
               result = a->ValueI64 == b->ValueI64;
            break;

            case exp_argument:
            {
                if (a->ArgumentList->ArgumentCount
                    == b->ArgumentList->ArgumentCount)
                {
                    const uint32_t ArgumentCount =
                        a->ArgumentList->ArgumentCount;
                    const metac_sema_expression_t* ExpsA
                            = a->ArgumentList->Arguments;
                    const metac_sema_expression_t* ExpsB
                            = b->ArgumentList->Arguments;

                    for(uint32_t i = 0;
                        i < ArgumentCount;
                        i++)
                    {
                        result &= (ExpsA + i == ExpsB + i ?

                                 1

                                 : Expression_IsEqual_(ExpsA + i, ExpsB + i));
                    }
                }
                else
                {
                    result =

                            0

                                 ;
                }
            } break;

            default: (((__builtin_expect(!!(0), 1)))? (void)0 :aco_abort());
        }
    }
    else
        result =

                0

                     ;
Lret:
    return result;
}

typedef struct handoff_walker_context_t
{
    uint32_t FunctionKey;

    const metac_semantic_state_t* Origin;
    metac_semantic_state_t* NewOwner;
    metac_sema_declaration_t* decl;
    metac_node_t result;
} handoff_walker_context_t;

metac_sema_expression_t* AllocNewSemaExpression(metac_semantic_state_t* self, metac_expression_t* expr)
{
    metac_sema_expression_t* result = 0;

    if (self->Expressions_capacity <= self->Expressions_size) { _newMemRealloc( ((void**)&self->Expressions), &self->Expressions_capacity, sizeof(*self->Expressions) ); }

    {
        result = self->Expressions + (self->Expressions_size++);
        (*(metac_expression_header_t*) result) = (*(metac_expression_header_t*) expr);

        result->TypeIndex.v = 0;
        result->Serial = (_nodeCounter++);
    }

    if (expr->Kind == exp_tuple)
    {
        const uint32_t tupleExpCount = expr->TupleExpressionCount;
        if (self->Expressions_capacity <= (self->Expressions_size + (tupleExpCount))) { _newMemRealloc( ((void**)&self->Expressions), &self->Expressions_capacity, sizeof(*self->Expressions) ); };

        uint32_t allocPos = (self->Expressions_size += tupleExpCount, self->Expressions_size - tupleExpCount);
        metac_sema_expression_t* elements =
            self->Expressions + allocPos;
        exp_tuple_t* expList = expr->TupleExpressionList;

        metac_expression_t* elemExpr;
        for(uint32_t i = 0;
            i < tupleExpCount;
            i++)
        {
            elemExpr = expList->Expression;
            metac_sema_expression_t* semaElem = elements + i;
            semaElem->Serial = (_nodeCounter++);

            (*(metac_expression_header_t*) semaElem) = (*(metac_expression_header_t*) elemExpr);

            memcpy(
                ((char*)semaElem) + sizeof(metac_sema_expression_header_t),
                ((char*)elemExpr) + sizeof(metac_expression_header_t),
                sizeof(metac_expression_t) - sizeof(metac_expression_header_t)
            );

            expList = expList->Next;

        }
        result->TupleExpressions = elements;
    }
    else
    {
        memcpy(
            ((char*)result) + sizeof(metac_sema_expression_header_t),
            ((char*)expr) + sizeof(metac_expression_header_t),
            sizeof(metac_expression_t) - sizeof(metac_expression_header_t)
        );
    }

    return result;
}


uint32_t StructIndex(metac_semantic_state_t* self, metac_type_aggregate_t* struct_)
{
    uint32_t result = (struct_ - self->StructTypeTable.Slots);
    return result;
}

uint32_t UnionIndex(metac_semantic_state_t* self, metac_type_aggregate_t* union_)
{
    uint32_t result = (union_ - self->UnionTypeTable.Slots);
    return result;
}


uint32_t FunctionIndex(metac_semantic_state_t* self, sema_decl_function_t* func)
{
    uint32_t result = (func - self->Functions);
    return result;
}

uint32_t StatementIndex_(metac_semantic_state_t* self, metac_sema_statement_t* stmt)
{
    uint32_t result = (stmt - self->Statements);
    return result;
}

uint32_t TypedefIndex(metac_semantic_state_t* self, metac_type_typedef_t* typedef_)
{
    uint32_t result = (typedef_ - self->TypedefTypeTable.Slots);
    return result;
}

uint32_t ArrayTypeIndex(metac_semantic_state_t* self, metac_type_array_t* array)
{
    uint32_t result = (array - self->ArrayTypeTable.Slots);
    return result;
}

uint32_t PtrTypeIndex(metac_semantic_state_t* self, metac_type_ptr_t* ptr)
{
    uint32_t result = (ptr - self->PtrTypeTable.Slots);
    return result;
}

uint32_t FunctiontypeIndex(metac_semantic_state_t* self, metac_type_functiontype_t* functiontype)
{
    uint32_t result = (functiontype - self->FunctionTypeTable.Slots);
    return result;
}

uint32_t EnumIndex(metac_semantic_state_t* self, metac_type_enum_t* enumtype)
{
    uint32_t result = (enumtype - self->EnumTypeTable.Slots);
    return result;
}


uint32_t TupleTypeIndex(metac_semantic_state_t* self, metac_type_tuple_t* tupletype)
{
    uint32_t result = (tupletype - self->TupleTypeTable.Slots);
    return result;
}

metac_type_aggregate_t* StructPtr(metac_semantic_state_t* self, uint32_t index)
{
    metac_type_aggregate_t* result = (self->StructTypeTable.Slots + index);
    return result;
}

metac_type_aggregate_t* UnionPtr(metac_semantic_state_t* self, uint32_t index)
{
    metac_type_aggregate_t* result = (self->UnionTypeTable.Slots + index);
    return result;
}

sema_decl_function_t* FunctionPtr(metac_semantic_state_t* self, uint32_t index)
{
    sema_decl_function_t* result = (self->Functions + index);
    return result;
}

metac_sema_statement_t* StatementPtr(metac_semantic_state_t* self, uint32_t index)
{
    metac_sema_statement_t* result = (self->Statements + index);
    return result;
}

metac_type_typedef_t* TypedefPtr(metac_semantic_state_t* self, uint32_t index)
{
    metac_type_typedef_t* result = (self->TypedefTypeTable.Slots + index);
    return result;
}

metac_type_ptr_t* PtrTypePtr(metac_semantic_state_t* self, uint32_t index)
{
    metac_type_ptr_t* result = (self->PtrTypeTable.Slots + index);
    return result;
}

metac_type_array_t* ArrayTypePtr(metac_semantic_state_t* self, uint32_t index)
{
    metac_type_array_t* result = (self->ArrayTypeTable.Slots + index);
    return result;
}

metac_type_functiontype_t* FunctiontypePtr(metac_semantic_state_t* self, uint32_t index)
{
    metac_type_functiontype_t* result = (self->FunctionTypeTable.Slots + index);
    return result;
}

metac_type_enum_t* EnumTypePtr(metac_semantic_state_t* self, uint32_t index)
{
    metac_type_enum_t* result = (self->EnumTypeTable.Slots + index);
    return result;
}


metac_type_tuple_t* TupleTypePtr(metac_semantic_state_t* self, uint32_t index)
{
    metac_type_tuple_t* result = (self->TupleTypeTable.Slots + index);
    return result;
}

metac_scope_t* MetaCScope_PushNewScope(metac_semantic_state_t* sema,
                                       metac_scope_t *parent,
                                       metac_scope_parent_t scopeOwner)
{
    metac_scope_t* result = AllocNewScope(sema, parent, scopeOwner);

    MetaCScopeTable_Init(&result->ScopeTable);

    return result;
}


metac_scope_t* AllocNewScope(metac_semantic_state_t* self,
                             metac_scope_t* parent, metac_scope_parent_t owner)
{
    metac_scope_t* result;

    if (self->Scopes_capacity <= self->Scopes_size) { _newMemRealloc( ((void**)&self->Scopes), &self->Scopes_capacity, sizeof(*self->Scopes) ); }

    {
        result = self->Scopes + (self->Scopes_size++);

        result->Serial = (_nodeCounter++);
        result->Owner = owner;
        result->Parent = parent;
    }

    return result;
}


sema_decl_function_t* AllocNewSemaFunction(metac_semantic_state_t* self,
                                           decl_function_t* func)
{
    sema_decl_function_t* result = 0;

    if (self->Functions_capacity <= self->Functions_size) { _newMemRealloc( ((void**)&self->Functions), &self->Functions_capacity, sizeof(*self->Functions) ); }

    {
        result = self->Functions + (self->Functions_size++);
        (*(metac_node_header_t*) result) = (*(metac_node_header_t*) func);

        result->Serial = (_nodeCounter++);
        result->TypeIndex.v = 0;
    }

    return result;
}

sema_decl_variable_t* AllocNewSemaVariable(metac_semantic_state_t* self, decl_variable_t* decl, metac_sema_declaration_t** result_ptr)
{
    sema_decl_variable_t* result = 0;
    if (self->Variables_capacity <= self->Variables_size) { _newMemRealloc( ((void**)&self->Variables), &self->Variables_capacity, sizeof(*self->Variables) ); }

    result = self->Variables + (self->Variables_size++);
    (*result_ptr) = (metac_sema_declaration_t*)result;

    result->DeclKind = decl_variable;
    result->Serial = (_nodeCounter++);
    decl->LocationIdx = result->LocationIdx;


    return result;
}

sema_decl_variable_t* AllocFunctionParameters(metac_semantic_state_t* self,
                                              sema_decl_function_t* func,
                                              uint32_t parameterCount)
{
    sema_decl_variable_t* result = 0;

    if (self->Variables_capacity <= (self->Variables_size + (parameterCount))) { _newMemRealloc( ((void**)&self->Variables), &self->Variables_capacity, sizeof(*self->Variables) ); }

    {
        result = self->Variables + (self->Variables_size += parameterCount, self->Variables_size - parameterCount);
        for(uint32_t i = 0;
            i < parameterCount;
            i++)
        {
            (result + i)->DeclKind = decl_parameter;
            (result + i)->Serial = (_nodeCounter++);
        }

    }

    return result;
}

metac_sema_statement_t* AllocNewSemaStatement_(metac_semantic_state_t* self,
                                               metac_statement_kind_t kind,
                                               uint32_t nodeSize, void** result_ptr)
{
    metac_sema_statement_t* result = 0;

    if (self->Statements_capacity <= self->Statements_size) { _newMemRealloc( ((void**)&self->Statements), &self->Statements_capacity, sizeof(*self->Statements) ); }

    {
        result = self->Statements + (self->Statements_size++);


        result->Serial = (_nodeCounter++);

    }

    *result_ptr = result;

    return result;
}

sema_stmt_block_t* AllocNewSemaBlockStatement(metac_semantic_state_t* self,
                                              sema_stmt_block_t* Parent, uint32_t statementCount,
                                              void** result_ptr)
{
    sema_stmt_block_t* result = 0;

    if (self->BlockStatements_capacity <= self->BlockStatements_size) { _newMemRealloc( ((void**)&self->BlockStatements), &self->BlockStatements_capacity, sizeof(*self->BlockStatements) ); }

    {
        uint32_t pointersSize = statementCount * sizeof(sema_stmt_block_t*);
        uint32_t sizeInBlockStatements =
            (pointersSize + sizeof(*self->BlockStatements)) /
            sizeof(*self->BlockStatements);

        result = self->BlockStatements + (self->BlockStatements_size += sizeInBlockStatements, self->BlockStatements_size - sizeInBlockStatements)
                                                                              ;
        result->Serial = (_nodeCounter++);
        result->Body = (metac_sema_statement_t*)(result + 1);
    }
    (*result_ptr) = result;

    return result;
}

uint32_t BlockStatementIndex(metac_semantic_state_t* self,
                             sema_stmt_block_t* blockstmt)
{
    return blockstmt - self->BlockStatements;
}



static inline void HandoffIdentifier(metac_identifier_table_t* dstTable,
                                     const metac_identifier_table_t* srcTable,
                                     metac_identifier_ptr_t* idPtrP)
{
    (((__builtin_expect(!!(idPtrP->v), 1)))? (void)0 :aco_abort());
    const char* idChars =
        IdentifierPtrToCharPtr((metac_identifier_table_t*)srcTable, *idPtrP);
    const uint32_t idLen = strlen(idChars);
    const uint32_t idHash = crc32c_nozero(~0, idChars, idLen);
    const uint32_t idKey = ( ((uint32_t)(idHash & 0xFFFFF)) | (((uint32_t)(idLen)) << 20) );

    metac_identifier_ptr_t newPtr =
        GetOrAddIdentifier(dstTable, idKey, idChars);

    idPtrP->v = newPtr.v;
}

static void HandoffField(metac_semantic_state_t* dstState,
                          const metac_semantic_state_t* srcState,
                          metac_type_aggregate_field_t* field)
{

}
static inline void HandoffType(metac_semantic_state_t* dstState,
                               const metac_semantic_state_t* srcState,
                               metac_type_index_t* typeIndexP)
{
    (((__builtin_expect(!!(typeIndexP->v), 1)))? (void)0 :aco_abort());
    metac_type_index_t result = {0};

    metac_type_index_t idx = *typeIndexP;
    switch(((metac_type_index_kind_t)((idx).v >> 28)))
    {

        case type_index_basic:
            result = *typeIndexP;
        break;
        case type_index_unknown: (((__builtin_expect(!!(0), 1)))? (void)0 :aco_abort());
        case type_index_functiontype:
        {
            metac_type_functiontype_t* oldSlot
                = srcState->FunctionTypeTable.Slots + ((idx).v & 0xFFFFFfF);

            metac_type_functiontype_t* newSlot;
            metac_type_functiontype_t tmpSlot = *oldSlot;
            const uint32_t paramTypeCount = oldSlot->ParameterTypeCount;
            metac_type_index_t* newParams = (metac_type_index_t*)
                calloc(sizeof(metac_type_index_t), oldSlot->ParameterTypeCount);
            memcpy(newParams, oldSlot->ParameterTypes,
                sizeof(metac_type_index_t) * paramTypeCount);
            for(uint32_t i = 0;
                i < paramTypeCount;
                i++)
            {
                HandoffType(dstState, srcState, newParams + i);
            }
            if (tmpSlot.ParameterTypeCount != 0)
                tmpSlot.ParameterTypes = newParams;
            else
                tmpSlot.ParameterTypes = (metac_type_index_t*)(metac_node_header_t*) 0x1;

            result =
                MetaCTypeTable_GetOrEmptyFunctionType(&dstState->FunctionTypeTable,
                                                      &tmpSlot);
            if (result.v == 0)
            {
                result =
                    MetaCTypeTable_AddFunctionType(&dstState->FunctionTypeTable, &tmpSlot);
            }
            (((__builtin_expect(!!(result.v != -1 && result.v != 0), 1)))? (void)0 :aco_abort());
        } break;

        case type_index_struct:
        {
            metac_type_aggregate_t* oldSlot =
                srcState->StructTypeTable.Slots + ((idx).v & 0xFFFFFfF);

            metac_type_aggregate_t tmpSlot = *oldSlot;

            const uint32_t fieldCount = oldSlot->FieldCount;

            metac_type_aggregate_field_t* newFields =
                (metac_type_aggregate_field_t*)
                    malloc(sizeof(metac_type_aggregate_field_t) * fieldCount);
            for(uint32_t i = 0; i < oldSlot->FieldCount; i++)
            {
                metac_type_aggregate_field_t field = oldSlot->Fields[i];
                HandoffIdentifier(dstState->ParserIdentifierTable,
                                  srcState->ParserIdentifierTable,
                                  &field.Identifier);
                HandoffType(dstState, srcState, &field.Type);
                newFields[i] = field;
            }
            tmpSlot.Fields = newFields;
            tmpSlot.Header.Hash = 0;

            uint32_t newHash = AggregateHash(&tmpSlot);
            tmpSlot.Header.Hash = newHash;

            result =
                MetaCTypeTable_GetOrEmptyStructType(&dstState->StructTypeTable,
                                                    &tmpSlot);
            (((__builtin_expect(!!(result.v == 0), 1)))? (void)0 :aco_abort());
            result =
                MetaCTypeTable_AddStructType(&dstState->StructTypeTable, &tmpSlot);
            (((__builtin_expect(!!(result.v != -1 && result.v != 0), 1)))? (void)0 :aco_abort());
        } break;
        case type_index_union:
        {
            (((__builtin_expect(!!(0), 1)))? (void)0 :aco_abort());

        } break;
        LhandoffAggregate:
        {

        } break;
    }

    (((__builtin_expect(!!(result.v), 1)))? (void)0 :aco_abort());
    (*typeIndexP) = result;
}


static inline int HandoffWalker(metac_node_t node, void* ctx)
{
    handoff_walker_context_t* context =
        (handoff_walker_context_t*) ctx;
    (((__builtin_expect(!!(crc32c_nozero(~0, __FUNCTION__, strlen(__FUNCTION__)) == context->FunctionKey), 1)))? (void)0 :aco_abort())
                                ;

    const metac_semantic_state_t* srcState = context->Origin;
    metac_semantic_state_t* dstState = context->NewOwner;

    switch(node->Kind)
    {
        case node_decl_type_union:
        case node_decl_type_struct:
        {
            metac_type_aggregate_t* struct_ =
                (metac_type_aggregate_t*) node;
            uint32_t hash = struct_->Header.Hash;
            (((__builtin_expect(!!(hash), 1)))? (void)0 :aco_abort());

            metac_type_index_t typeIndex =
                MetaCTypeTable_GetOrEmptyStructType(&srcState->StructTypeTable,
                                                    struct_);
            (((__builtin_expect(!!(typeIndex.v != 0 && typeIndex.v != -1), 1)))? (void)0 :aco_abort());
            HandoffType(dstState, srcState, &typeIndex);

            context->result =
                (metac_node_t)StructPtr(dstState, ((typeIndex).v & 0xFFFFFfF));
            return 1;
        }
        case node_decl_type_functiontype:
        {
            metac_type_functiontype_t* functiontype =
                (metac_type_functiontype_t*) node;
            uint32_t hash = functiontype->Header.Hash;
            (((__builtin_expect(!!(hash), 1)))? (void)0 :aco_abort());


            metac_type_index_t typeIndex =
                MetaCTypeTable_GetOrEmptyFunctionType(&srcState->FunctionTypeTable,
                                                      functiontype);
            (((__builtin_expect(!!(typeIndex.v != 0 && typeIndex.v != -1), 1)))? (void)0 :aco_abort());
            HandoffType(dstState, srcState, &typeIndex);

            context->result =
                (metac_node_t)FunctiontypePtr(dstState, ((typeIndex).v & 0xFFFFFfF));
        } break;
        case node_decl_field:
        {

        } break;
    }

    return 0;
}


void MetaCSemantic_Handoff(metac_semantic_state_t* self, metac_sema_declaration_t** declP,
                           metac_semantic_state_t* newOwner)
{
    printf("Handoff\n");
    metac_sema_declaration_t* decl = *declP;

    handoff_walker_context_t handoff_context = {
        crc32c_nozero(~0, "HandoffWalker", sizeof("HandoffWalker") -1),
        self, newOwner, decl, 0
    };




    *declP = (metac_sema_declaration_t*)handoff_context.result;
}

void MetaCSemantic_Init(metac_semantic_state_t* self, metac_parser_t* parser,
                        metac_type_aggregate_t* compilerStruct)
{





    TypeTableInitImpl((metac_type_table_t*)&self->EnumTypeTable, sizeof(metac_type_enum_t), type_index_enum); TypeTableInitImpl((metac_type_table_t*)&self->ArrayTypeTable, sizeof(metac_type_array_t), type_index_array); TypeTableInitImpl((metac_type_table_t*)&self->StructTypeTable, sizeof(metac_type_aggregate_t), type_index_struct); TypeTableInitImpl((metac_type_table_t*)&self->PtrTypeTable, sizeof(metac_type_ptr_t), type_index_ptr); TypeTableInitImpl((metac_type_table_t*)&self->UnionTypeTable, sizeof(metac_type_aggregate_t), type_index_union); TypeTableInitImpl((metac_type_table_t*)&self->TypedefTypeTable, sizeof(metac_type_typedef_t), type_index_typedef); TypeTableInitImpl((metac_type_table_t*)&self->FunctionTypeTable, sizeof(metac_type_functiontype_t), type_index_functiontype); TypeTableInitImpl((metac_type_table_t*)&self->TupleTypeTable, sizeof(metac_type_tuple_t), type_index_tuple);

    self->Expressions = (metac_sema_expression_t*)0; self->Expressions_size = 0; self->Expressions_capacity = 0; self->Variables = (sema_decl_variable_t*)0; self->Variables_size = 0; self->Variables_capacity = 0; self->Functions = (sema_decl_function_t*)0; self->Functions_size = 0; self->Functions_capacity = 0; self->Scopes = (metac_scope_t*)0; self->Scopes_size = 0; self->Scopes_capacity = 0; self->BlockStatements = (sema_stmt_block_t*)0; self->BlockStatements_size = 0; self->BlockStatements_capacity = 0; self->Statements = (metac_sema_statement_t*)0; self->Statements_size = 0; self->Statements_capacity = 0;

    self->TemporaryScopeDepth = 0;

    self->ExpressionStackCapacity = 64;
    self->ExpressionStackSize = 0;
    self->ExpressionStack = (metac_sema_expression_t*)
        calloc(sizeof(metac_sema_expression_t), self->ExpressionStackCapacity);

    self->Waiters.WaiterLock._rwctr = 0;
    self->Waiters.WaiterCapacity = 64;
    self->Waiters.WaiterCount = 0;
    self->Waiters.Waiters = (metac_semantic_waiter_t*)
                calloc(sizeof(*self->Waiters.Waiters), self->Waiters.WaiterCapacity);

    IdentifierTable_Init(&self->SemanticIdentifierTable, 20, 13);
    self->ParserIdentifierTable = &parser->IdentifierTable;
    self->ParserStringTable = &parser->StringTable;

    self->CurrentScope = 0;
    self->CurrentDeclarationState = 0;

    memset(&self->LRU, 0, sizeof(self->LRU));

    if (compilerStruct && ((metac_node_t)compilerStruct) != (metac_node_header_t*) 0x1)
    {
        self->CompilerInterface = compilerStruct;
    }
    else
    {
        self->CompilerInterface = 0;
    }

    self->initialized =

                       1

                           ;
}

void MetaCSemantic_PopScope(metac_semantic_state_t* self)
{
    (((__builtin_expect(!!(self->CurrentScope), 1)))? (void)0 :aco_abort());
    self->CurrentScope = self->CurrentScope->Parent;
}

metac_scope_parent_t ScopeParent(metac_semantic_state_t* sema,
                                 metac_scope_parent_kind_t parentKind,
                                 metac_node_t parentNode)
{
    metac_scope_parent_t scopeParent;

    uint32_t scopeParentIndex = 0;
    switch(parentKind)
    {
    case scope_parent_invalid :
    case scope_parent_extended :
    case scope_parent_unknown :
        (((__builtin_expect(!!(0), 1)))? (void)0 :aco_abort());

    case scope_parent_module :


        scopeParentIndex = (uint32_t) parentNode;
    break;
    case scope_parent_function :
        scopeParentIndex = FunctionIndex(sema, (sema_decl_function_t*)parentNode);
    break;
    case scope_parent_struct :
        scopeParentIndex = StructIndex(sema, (metac_type_aggregate_t*)parentNode);
    break;
    case scope_parent_statement :
        scopeParentIndex = StatementIndex_(sema, (metac_sema_statement_t*)(metac_sema_statement_t*)parentNode);
    break;
    case scope_parent_block :
        scopeParentIndex = BlockStatementIndex(sema, (sema_stmt_block_t*)parentNode);
    break;
    case scope_parent_union :
        scopeParentIndex = UnionIndex(sema, (metac_type_aggregate_t*)parentNode);
    break;
    }
    scopeParent.v = ((uint32_t)(((parentKind) << 28) | (scopeParentIndex)));

    return scopeParent;
}



_Bool

    IsTemporaryScope(metac_scope_t* scope_)
{
    return (scope_->scopeFlags & scope_flag_temporary) != 0;
}



metac_scope_t* MetaCSemantic_PushTemporaryScope_(metac_semantic_state_t* self,
                                                 metac_scope_t* tmpScope,
                                                 uint32_t line,
                                                 const char* file)
{
    printf("[%u]Pushing tmpScope {%s:%u}\n", self->TemporaryScopeDepth, file, line);
    (((__builtin_expect(!!((tmpScope->scopeFlags & scope_flag_temporary) != 0), 1)))? (void)0 :aco_abort());

    (((__builtin_expect(!!((!self->TemporaryScopeDepth) || IsTemporaryScope(self->CurrentScope)), 1)))? (void)0 :aco_abort());
    self->TemporaryScopeDepth++;

    tmpScope->Parent = self->CurrentScope;
    self->CurrentScope = tmpScope;

    return tmpScope;
}

void MetaCSemantic_PopTemporaryScope_(metac_semantic_state_t* self,

                                      uint32_t line,
                                      const char* file)
{
    (((__builtin_expect(!!(self->TemporaryScopeDepth != 0 && IsTemporaryScope(self->CurrentScope)), 1)))? (void)0 :aco_abort());
    --self->TemporaryScopeDepth;
    printf("[%u] Popping tmpScope {%s:%u}\n", self->TemporaryScopeDepth, file, line);
    self->CurrentScope = self->CurrentScope->Parent;
}

metac_scope_t* MetaCSemantic_PushNewScope(metac_semantic_state_t* self,
                                          metac_scope_parent_kind_t parentKind,
                                          metac_node_t parentNode)
{
    metac_scope_parent_t scopeParent = ScopeParent(self, parentKind, parentNode);
    self->CurrentScope = MetaCScope_PushNewScope(self,
                                                 self->CurrentScope,
                                                 scopeParent);
    return self->CurrentScope;
}

metac_sema_statement_t* MetaCSemantic_doStatementSemantic_(metac_semantic_state_t* self,
                                                           metac_statement_t* stmt,
                                                           const char* callFile,
                                                           uint32_t callLine)
{
    metac_sema_statement_t* result;
    switch (stmt->StmtKind)
    {
        case stmt_exp:
        {
            sema_stmt_exp_t* sse = (sema_stmt_exp_t*) AllocNewSemaStatement_(self, stmt_exp, sizeof(sema_stmt_exp_t), ((void**)(&result)));
        } break;

        default: (((__builtin_expect(!!(0), 1)))? (void)0 :aco_abort());

        case stmt_block:
        {
            stmt_block_t* blockStatement = (stmt_block_t*) stmt;
            uint32_t statementCount = blockStatement->StatementCount;
            sema_stmt_block_t* semaBlockStatement =
                (sema_stmt_block_t*) AllocNewSemaStatement_(self, stmt_block, sizeof(sema_stmt_block_t), ((void**)(&result)));

            metac_scope_parent_t parent = {((uint32_t)(((scope_parent_statement) << 28) | (BlockStatementIndex(self, semaBlockStatement))))
                                                                                         };

            MetaCSemantic_PushNewScope(self,
                                       scope_parent_block,
                                       (metac_node_t)semaBlockStatement);

            metac_statement_t* currentStatement = blockStatement->Body;
            for(uint32_t i = 0;
                i < statementCount;
                i++)
            {
                ((&semaBlockStatement->Body)[i]) =
                    MetaCSemantic_doStatementSemantic_(self, ((metac_statement_t*)(currentStatement)), "../metac_semantic.c", 896);
                currentStatement = currentStatement->Next;
            }

            MetaCSemantic_PopScope(self);
        } break;

        case stmt_for:
        {
            stmt_for_t* for_ = (stmt_for_t*) stmt;
            sema_stmt_for_t* semaFor =
                (sema_stmt_for_t*) AllocNewSemaStatement_(self, stmt_for, sizeof(sema_stmt_for_t), ((void**)(for_)));

            metac_scope_t* forScope = semaFor->Scope =
                MetaCSemantic_PushNewScope(self,
                                           scope_parent_statement,
                                           (*(metac_node_t*)(&semaFor)));

            metac_sema_declaration_t* ForInit =
                MetaCSemantic_doDeclSemantic_(self, ((metac_declaration_t*)(for_->ForInit)), "../metac_semantic.c", 915);
            metac_sema_expression_t* ForCond =
                MetaCSemantic_doExprSemantic_(self, ((metac_expression_t*)(for_->ForCond)), 0, "../metac_semantic.c", 917);
            metac_sema_expression_t* ForPostLoop =
                MetaCSemantic_doExprSemantic_(self, ((metac_expression_t*)(for_->ForPostLoop)), 0, "../metac_semantic.c", 919);
        } break;



        case stmt_yield:
        {
            stmt_yield_t* yieldStatement = (stmt_yield_t*) stmt;
            sema_stmt_yield_t* semaYieldStatement =
                (sema_stmt_yield_t*) AllocNewSemaStatement_(self, stmt_yield, sizeof(sema_stmt_yield_t), ((void**)(&result)));

            metac_sema_expression_t* yieldValue =
                MetaCSemantic_doExprSemantic_(self, ((metac_expression_t*)(yieldStatement->YieldExp)), 0, "../metac_semantic.c", 931);
            semaYieldStatement->YieldExp = yieldValue;
        } break;

        case stmt_return:
        {
            stmt_return_t* returnStatement = (stmt_return_t*) stmt;
            sema_stmt_return_t* semaReturnStatement =
                (sema_stmt_return_t*) AllocNewSemaStatement_(self, stmt_return, sizeof(sema_stmt_return_t), ((void**)(&result)));

            metac_sema_expression_t* returnValue =
                MetaCSemantic_doExprSemantic_(self, ((metac_expression_t*)(returnStatement->ReturnExp)), 0, "../metac_semantic.c", 942);
            semaReturnStatement->ReturnExp = returnValue;
        } break;
    }

    return result;
}





uint32_t crc32c_nozero(uint32_t crc, const void* s, const uint32_t len_p);
uint32_t crc32c_byte(uint32_t crc, uint8_t byte);

scope_insert_error_t MetaCSemantic_RegisterInScope(metac_semantic_state_t* self,
                                                   metac_identifier_ptr_t idPtr,
                                                   metac_node_t node)
{
    scope_insert_error_t result = no_scope;
    uint32_t idPtrHash = crc32c_nozero(~0, &idPtr.v, sizeof(idPtr.v));


    uint16_t hash12 = idPtrHash & 0xFFF0;

    int16x8_t hashes;
    hashes = Load16(&self->LRU.LRUContentHashes);
    const int16x8_t hash12_8 = Set1_16(hash12);
    const int16x8_t hashMask = Set1_16((uint16_t)0xFFF0);
    const int16x8_t maskedHashes = And16(hashes, hashMask);
    const int16x8_t clearMask = Eq16(maskedHashes, hash12_8);
    const int16x8_t cleared = Andnot16(clearMask, hashes);
    Store16(&self->LRU.LRUContentHashes, cleared);

    if (self->CurrentScope != 0)
        result = MetaCScope_RegisterIdentifier(self->CurrentScope, idPtr, node);





    for(uint32_t i = 0; i < self->Waiters.WaiterCount; i++)
    {
        metac_semantic_waiter_t *waiter = &self->Waiters.Waiters[i];
        if (waiter->FuncHash == (crc32c_nozero(~(uint32_t)0, "MetaCSemantic_LookupIdentifier", sizeof("MetaCSemantic_LookupIdentifier") - 1))
         && waiter->NodeHash == (crc32c_nozero(~0, &(idPtr), sizeof(idPtr))))
        {
            task_t* waitingTask = ((task_t*)waiter->Continuation->arg);
            (((__builtin_expect(!!(waitingTask->TaskFlags == Task_Waiting), 1)))? (void)0 :aco_abort());
            printf("Found matching waiter\n");
            waitingTask->TaskFlags &= (~Task_Waiting);
            waitingTask->TaskFlags |= Task_Resumable;

            {
                *waiter = self->Waiters.Waiters[--self->Waiters.WaiterCount];
            }

        }
    }


    return result;
}


sema_decl_function_t* MetaCSemantic_doFunctionSemantic(metac_semantic_state_t* self,
                                                       decl_function_t* func)
{


    metac_sema_decl_state_t declState = {0};
    self->CurrentDeclarationState = &declState;

    sema_decl_function_t* f = AllocNewSemaFunction(self, func);

    sema_decl_variable_t* params =
        f->Parameters =
            AllocFunctionParameters(self, f, func->ParameterCount);

    decl_parameter_t* currentParam = func->Parameters;
    for(uint32_t i = 0;
        i < func->ParameterCount;
        i++)
    {



        f->Parameters[i].VarFlags |= variable_is_parameter;
        f->Parameters[i].TypeIndex =
            MetaCSemantic_doTypeSemantic_(self, ((decl_type_t*)(currentParam->Parameter->VarType)),
                                                                         "../metac_semantic.c"

            ,
                                                                         1045

            )
                                                                          ;
        currentParam = currentParam->Next;
    }

    (((__builtin_expect(!!(currentParam == ((void*)0x1)), 1)))? (void)0 :aco_abort());

    if (func->FunctionBody == ((void*)0x1))
    {
        return f;
    }

    metac_scope_parent_t Parent = {((uint32_t)(((scope_parent_function) << 28) | (FunctionIndex(self, f))))};

    f->Scope = MetaCSemantic_PushNewScope(self, scope_parent_function, (metac_node_t)f);


 uint32_t frameOffset = ((f->ParentFunc != (sema_decl_function_t*)(metac_node_header_t*) 0x1)
                           ? f->ParentFunc->FrameOffset : 0);

    for(uint32_t i = 0;
        i < func->ParameterCount;
        i++)
    {
        metac_node_t ptr = (metac_node_t)(&f->Parameters[i]);
        params[i].Storage.v = (((storage_stack) << 28) | (frameOffset));
        frameOffset += Align(MetaCSemantic_GetTypeSize(self, params[i].TypeIndex), 4);

        scope_insert_error_t result =
            MetaCScope_RegisterIdentifier(f->Scope, params[i].VarIdentifier,
                                          ptr);
    }
    f->FrameOffset = frameOffset;
    f->FunctionBody = (sema_stmt_block_t*)
        MetaCSemantic_doStatementSemantic_(self, ((metac_statement_t*)(func->FunctionBody)), "../metac_semantic.c", 1078);

    MetaCSemantic_PopScope(self);

    return f;
}

metac_node_t NodeFromTypeIndex(metac_semantic_state_t* sema,
                               metac_type_index_t typeIndex)
{
    const uint32_t index = ((typeIndex).v & 0xFFFFFfF);
    switch(((metac_type_index_kind_t)((typeIndex).v >> 28)))
    {
        case type_index_struct:
            return (metac_node_t)StructPtr(sema, index);
        case type_index_union:
            return (metac_node_t)UnionPtr(sema, index);
        case type_index_typedef:
            return (metac_node_t)(TypedefPtr(sema, index));
    }

    return 0;
}

typedef struct MetaCSemantic_doDeclSemantic_task_context_t
{
    metac_semantic_state_t* Sema;
    metac_declaration_t* Decl;
    metac_sema_declaration_t* Result;
} MetaCSemantic_doDeclSemantic_task_context_t;

const char* doDeclSemantic_PrintFunction(task_t* task)
{
    MetaCSemantic_doDeclSemantic_task_context_t* ctx =
         (MetaCSemantic_doDeclSemantic_task_context_t*)
            task->Context;
    char* buffer = (char*)malloc(256);
    metac_printer_t printer;

    const char* declPrint = 0;


    sprintf(buffer, "doDeclSemantic {Sema: %p, Decl: %s}\n",
                    ctx->Sema, declPrint);

    return buffer;
}


metac_sema_declaration_t* MetaCSemantic_declSemantic(metac_semantic_state_t* self,
                                                     metac_declaration_t* decl)
{
    metac_sema_declaration_t* result = (metac_sema_declaration_t*)0xFEFEFEFE;
    metac_identifier_ptr_t declId = {0};

    switch(decl->DeclKind)
    {
        case decl_function:
        {
            decl_function_t* f = (decl_function_t*) decl;
            result = (metac_sema_declaration_t*)
                MetaCSemantic_doFunctionSemantic(self, f);

        } break;
        case decl_parameter:
            (((__builtin_expect(!!(0), 1)))? (void)0 :aco_abort());
        case decl_variable:
        {
            decl_variable_t* v = (decl_variable_t*) decl;
            sema_decl_variable_t* var = AllocNewSemaVariable(self, v, &result);
            var->TypeIndex = MetaCSemantic_doTypeSemantic_(self, ((decl_type_t*)(v->VarType)), "../metac_semantic.c", 1148);
            if ((*(metac_node_t*)(&v->VarInitExpression)) != (metac_node_header_t*) 0x1)
            {
                var->VarInitExpression = MetaCSemantic_doExprSemantic_(self, ((metac_expression_t*)(v->VarInitExpression)), 0, "../metac_semantic.c", 1151);
            }
            else
            {
                (*(metac_node_t*)(&var->VarInitExpression)) = (metac_node_header_t*) 0x1;
            }

            var->VarIdentifier = v->VarIdentifier;
            MetaCSemantic_RegisterInScope(self, var->VarIdentifier, (*(metac_node_t*)(&var)));
        } break;
        case decl_type_struct:
            ((decl_type_t*)decl)->TypeKind = type_struct;
            declId = ((decl_type_struct_t*) decl)->Identifier;
            goto LdoTypeSemantic;
        case decl_type_union:
            ((decl_type_t*)decl)->TypeKind = type_union;
            declId = ((decl_type_union_t*) decl)->Identifier;
            goto LdoTypeSemantic;
        case decl_type_array:
            ((decl_type_t*)decl)->TypeKind = type_array;
            goto LdoTypeSemantic;
        case decl_type_typedef:
            ((decl_type_t*)decl)->TypeKind = type_typedef;
            declId = ((decl_type_typedef_t*) decl)->Identifier;
        goto LdoTypeSemantic;
        case decl_type_ptr:
            ((decl_type_t*)decl)->TypeKind = type_ptr;
        goto LdoTypeSemantic;
    LdoTypeSemantic:
        {
            metac_type_index_t type_index =
                MetaCSemantic_doTypeSemantic_(self, ((decl_type_t*)((decl_type_t*)decl)), "../metac_semantic.c", 1182);
            if (declId.v != 0 && declId.v != -1)
            {
                metac_node_t node =
                    NodeFromTypeIndex(self, type_index);
                MetaCSemantic_RegisterInScope(self, declId, node);
                                result = (metac_sema_declaration_t*)node;
            }
        } break;
    }
    (((__builtin_expect(!!(result != (metac_sema_declaration_t*)0xFEFEFEFE), 1)))? (void)0 :aco_abort());
    return result;
}

void MetaCSemantic_doDeclSemantic_Task(task_t* task)
{
    const char* taskPrint = doDeclSemantic_PrintFunction(task);
    printf("Task: %s\n", taskPrint);
    free(taskPrint);

    MetaCSemantic_doDeclSemantic_task_context_t* ctx =
        (MetaCSemantic_doDeclSemantic_task_context_t*)
            task->Context;
    task_origin_t Origin = task->Origin;
    ctx->Result =
        MetaCSemantic_doDeclSemantic_(ctx->Sema, ctx->Decl, Origin.File, Origin.Line);
}



metac_sema_declaration_t* MetaCSemantic_doDeclSemantic_(metac_semantic_state_t* self,
                                                        metac_declaration_t* decl,
                                                        const char* callFile,
                                                        uint32_t callLine)
{
    metac_sema_declaration_t* result = 0;
    result = MetaCSemantic_declSemantic(self, decl);
    if (!result)
    {

        taskqueue_t* q = &CurrentWorker()->Queue;
        printf("Couldn't do the decl Semantic, yielding to try again\n");
        MetaCSemantic_doDeclSemantic_task_context_t CtxValue = {
            self, decl
        };

        task_t declTask;
        task_t* currentTask = CurrentTask();
        if (currentTask->TaskFunction == MetaCSemantic_doDeclSemantic_Task)
        {
            ;
        }
        declTask.TaskFunction = MetaCSemantic_doDeclSemantic_Task;
                declTask.Origin.File = callFile;
        declTask.Origin.Line = callFile;
        declTask.Context = declTask._inlineContext;
        declTask.ContextSize = sizeof(CtxValue);
        (((__builtin_expect(!!(sizeof(CtxValue) < sizeof(declTask._inlineContext)), 1)))? (void)0 :aco_abort());
        (*(MetaCSemantic_doDeclSemantic_task_context_t*)declTask.Context) =
            CtxValue;
        MetaCSemantic_doDeclSemantic_task_context_t* CtxValuePtr = &CtxValue;
        printf("We should yield now\n");
        declTask.Continuation = currentTask;
        TaskQueue_Push(q, &declTask);
        currentTask->TaskFlags |= Task_Waiting;
        do { do { do { (((__builtin_expect(!!((((aco_gtls_co_))) !=

       ((void *)0)

       ), 1)))?((void)0):(aco_abort())); (((__builtin_expect(!!((((aco_gtls_co_))->main_co) !=

       ((void *)0)

       ), 1)))?((void)0):(aco_abort())); acosw((aco_gtls_co_), (aco_gtls_co_)->main_co); } while(0); } while(0); } while(0);
        printf("We are back\n");
        result = CtxValuePtr->Result;



    }
    return result;
}




metac_node_t MetaCSemantic_LRU_LookupIdentifier(metac_semantic_state_t* self,
                                                 uint32_t idPtrHash,
                                                 metac_identifier_ptr_t idPtr)
{
    uint32_t mask = 0;
    int16x8_t hashes = Load16(&self->LRU.LRUContentHashes);

    metac_node_t result = (metac_node_header_t*) 0x1;
    uint16_t hash12 = idPtrHash & 0xFFF0;

    const int16x8_t hash12_8 = Set1_16(hash12);
    const int16x8_t hashMask = Set1_16(0xFFF0);
    const int16x8_t maskedHashes = And16(hashes, hashMask);
    const int16x8_t matches = Eq16(maskedHashes, hash12_8);
    mask = MoveMask16(matches);

    while(mask)
    {
        const uint32_t i = (__builtin_ctz(mask));

        mask &= ~(i << i);
        if (self->LRU.Slots[i].Ptr.v == idPtr.v)
        {
            result = self->LRU.Slots[i].Node;
            break;
        }
    }

    return result;
}



metac_node_t MetaCSemantic_LookupIdentifier(metac_semantic_state_t* self,
                                            metac_identifier_ptr_t identifierPtr)
{

    metac_node_t result = (metac_node_header_t*) 0x1;
    uint32_t idPtrHash = crc32c_nozero(~0, &identifierPtr.v, sizeof(identifierPtr.v));

    metac_scope_t *currentScope = self->CurrentScope;
    {
        while(currentScope)
        {
          metac_node_t lookupResult =
                MetaCScope_LookupIdentifier(currentScope, idPtrHash, identifierPtr);
            if (lookupResult)
            {
                result = lookupResult;
                break;
            }
            currentScope = currentScope->Parent;
        }
    }

    return result;
}

static inline void TypeToCharsP(metac_semantic_state_t* self,
                                metac_printer_t* printer,
                                metac_type_index_t typeIndex)
{
    uint32_t typeIndexIndex = ((typeIndex).v & 0xFFFFFfF);

    switch (((metac_type_index_kind_t)((typeIndex).v >> 28)))
    {
        case type_index_array:
        {
            metac_type_array_t* arrayType =
                (self->ArrayTypeTable.Slots + ((typeIndex).v & 0xFFFFFfF));
            TypeToCharsP(self, printer, arrayType->ElementType);
            MetacPrinter_PrintStringLiteral(printer, "[");
            MetacPrinter_PrintI64(printer, (int64_t)arrayType->Dim);
            MetacPrinter_PrintStringLiteral(printer, "]");
        } break;
        case type_index_basic:
        {
            const char* typeString = BasicTypeToChars(typeIndex);
            MetacPrinter_PrintStringLiteral(printer, typeString);
        } break;
        case type_index_ptr:
        {
            metac_type_ptr_t* ptrType =
                (self->PtrTypeTable.Slots + ((typeIndex).v & 0xFFFFFfF));
            TypeToCharsP(self, printer, ptrType->ElementType);
            MetacPrinter_PrintStringLiteral(printer, "*");
        } break;
        case type_index_tuple:
        {
            metac_type_tuple_t* tupleType =
                (self->TupleTypeTable.Slots + ((typeIndex).v & 0xFFFFFfF));
            MetacPrinter_PrintStringLiteral(printer, "{");
            const uint32_t typeCount = tupleType->typeCount;
            for(uint32_t i = 0;
                i < tupleType->typeCount;
                i++)
            {
                TypeToCharsP(self, printer, tupleType->typeIndicies[i]);
                if (i != (typeCount - 1))
                {
                    MetacPrinter_PrintStringLiteral(printer, ", ");
                }
            }
            MetacPrinter_PrintStringLiteral(printer, "}");
        } break;
    }
}

const char* TypeToChars(metac_semantic_state_t* self, metac_type_index_t typeIndex)
{
    const char* result = 0;
    static metac_printer_t printer = {0};
    if (!printer.StringMemory)
        MetaCPrinter_InitSz(&printer, self->ParserIdentifierTable, 0, 32);
    else
        MetaCPrinter_Reset(&printer);
    TypeToCharsP(self, &printer, typeIndex);
    printer.StringMemory[printer.StringMemorySize++] = '\0';
    result = printer.StringMemory;

    return result;
}

const metac_scope_t* GetAggregateScope(metac_type_aggregate_t* agg)
{
    return agg->Scope;
}


static inline int32_t GetConstI32(metac_semantic_state_t* self, metac_sema_expression_t* index,

                                                                                               _Bool

                                                                                                    *errored)
{
    int32_t result = ~0;

    if (index->Kind == exp_signed_integer)
    {
        result = (int32_t) index->ValueI64;
    }
    else
    {
        *errored =

                  1

                      ;
    }

    return result;
}

metac_sema_expression_t* MetaCSemantic_doIndexSemantic_(metac_semantic_state_t* self,
                                                        metac_expression_t* expr,
                                                        const char* callFun,
                                                        uint32_t callLine)
{
    metac_sema_expression_t* result = 0;

    metac_sema_expression_t* indexed = MetaCSemantic_doExprSemantic_(self, ((metac_expression_t*)(expr->E1)), 0, "../metac_semantic.c", 1422);
    metac_sema_expression_t* index = MetaCSemantic_doExprSemantic_(self, ((metac_expression_t*)(expr->E2)), 0, "../metac_semantic.c", 1423);
    if (indexed->Kind == exp_tuple)
    {


       _Bool

            errored =

                      0

                           ;
        int32_t idx = GetConstI32(self, index, &errored);
        if ((int32_t)indexed->TupleExpressionCount > idx)
        {
            result = indexed->TupleExpressions + idx;
        }
        else if (!errored)
        {
            fprintf(

                   stderr

                         , "TupleIndex needs to be less than: %u", indexed->TupleExpressionCount);
        }
        else
        {
            fprintf(

                   stderr

                         , "index is not a constant value\n");
        }

    }

    return result;
}












typedef struct DeclarationArray
{
    metac_declaration_t** Ptr;
    uint32_t Length;
    uint32_t Capacity;
} DeclarationArray;

void LexFile(metac_lexer_t* lexer,
             const char* path,
             const char* text, int32_t length);

void ParseFile(metac_parser_t* parser,
               const char* path,
               DeclarationArray* result);

extern

      _Bool

           errored;









uint32_t crc32c_nozero(uint32_t crc, const void* s, const uint32_t len_p);
uint32_t crc32c_byte(uint32_t crc, uint8_t byte);





_Bool

    errored =

              0

                   ;



static inline uint32_t EstimateNumberOfTokens(uint32_t length)
{
    uint32_t aligned_length = (length + 16) & ~15;
    float token_estimate = ( aligned_length / 4.3 );
    return (((uint32_t) token_estimate) + 128) & ~127;
}


void LexFile(metac_lexer_t* lexer,
             const char* path,
             const char* text, int32_t length)
{
    if (length)
    {
        uint32_t estimated = EstimateNumberOfTokens( length );
        if (estimated > 96 && estimated < 768)
            estimated = 1024;

        uint32_t fileHash = ~0;

        metac_lexer_state_t lexer_state =
            MetaCLexerStateFromBuffer(1, text, length);
        if (estimated > 96)
        {
            lexer->Tokens = (metac_token_t*) malloc(estimated * sizeof(metac_token_t));
            lexer->TokenCapacity = estimated;
            lexer->LocationStorage.Locations =
                (metac_location_t*) malloc(estimated * sizeof(metac_location_t));
            lexer->LocationStorage.LocationCapacity = estimated;
        }
        else
        {
            lexer->Tokens = lexer->inlineTokens;
            lexer->TokenCapacity =
                sizeof(lexer->inlineTokens) / sizeof(lexer->Tokens[0]);
        }

        while(length > 0)
        {
            uint32_t initialPosition = lexer_state.Position;

            metac_token_t token =
                *MetaCLexerLexNextToken(lexer, &lexer_state, text, length);
            uint32_t eaten_chars = lexer_state.Position - initialPosition;
            fileHash = crc32c_nozero(fileHash, text, eaten_chars);

            if (eaten_chars == 0)
            {
                printf("No chars consumed ... Token:%s\n", MetaCTokenEnum_toChars(token.TokenType));
                printf("String around the halt: : %.*s\n" ,20 , text - 10);
                errored =

                         1

                             ;
                return ;
            }

            text += eaten_chars;
            length -= eaten_chars;
        }
    }
}


void ParseFile(metac_parser_t* parser,
               const char* path,
               DeclarationArray* result)
{

    uint32_t declarationSize = 0;
    uint32_t declarationCapacity = 1024;

    metac_declaration_t** declarations =
        (metac_declaration_t**)
            calloc(sizeof(metac_declaration_t*),
            declarationCapacity);

    while(parser->CurrentTokenIndex < parser->Lexer->TokenCount)
    {
        declarations[declarationSize++] = MetaCParser_ParseDeclaration(parser, 0);

        metac_token_t* lastToken = (MetaCParser_PeekToken_(parser, 1, 98));

        if (!lastToken || lastToken->TokenType == tok_eof)
            break;
    }

    if (result != 0)
    {
        if (result->Capacity < declarationSize)
        {
            result->Capacity = (((declarationSize) + 3) & ~3);
            result->Ptr = (metac_declaration_t**)calloc(result->Capacity, sizeof(metac_declaration_t*));
        }
        result->Length = declarationSize;
        memcpy(result->Ptr, declarations, declarationSize * sizeof(metac_declaration_t*));
        free(declarations);
    }
}







const char** includePaths = 0;
uint32_t includePathCount = 0;
uint32_t includePathCapacity = 0;

void AddIncludePath(const char* path)
{
    (((__builtin_expect(!!(includePathCount < includePathCapacity), 1)))? (void)0 :aco_abort());

    *(includePaths + includePathCount++) = path;
}

int main(int argc, const char* argv[])
{
    includePathCount = 0;
    includePathCapacity = 256;
    includePaths = (const char**)malloc(sizeof(char**) * includePathCapacity);
    const char* arg = "bigcode.c";

    uint32_t nWorkers = 3;







    for(int arg_idx = 1;
        arg_idx < argc;
        arg_idx++)
    {
        arg = argv[arg_idx];
        if (arg[0] == '-')
        {
            if (arg[1] == 'I')
            {
                AddIncludePath(arg + 2);
            }
            else
            {
                fprintf(

                       stderr

                             , "Unkown option: %s", arg);
            }
            continue;
        }
        printf("arg: %s\n", arg);
        metac_lexer_t lexer;
        MetaCLexer_Init(&lexer);

        read_result_t readResult = ReadFileAndZeroTerminate(arg);

        LexFile(&lexer, arg,
            readResult.FileContent0, readResult.FileLength
        );

        metac_parser_t parser;
        MetaCParser_InitFromLexer(&parser, &lexer);

        ParseFile(&parser, arg, 0);

        metac_identifier_table_slot_t firstEntry = {0};

        metac_identifier_table_slot_t* firstEntryP = findFirstEntry(&lexer.IdentifierTable);
        if (firstEntryP)
            firstEntry = *firstEntryP;

        printf("First Entry = {Hash:%x Value:%u}\n", firstEntry.HashKey, firstEntry.Ptr.v);



        char formatBuffer[512];
        sprintf(formatBuffer, "%s.tokens", arg);
        FILE* tokens_fd = fopen(formatBuffer, "wb");
        fwrite(lexer.Tokens, 1, lexer.TokenCount * sizeof(metac_token_t), tokens_fd);
        fclose(tokens_fd);


        sprintf(formatBuffer, "%s.identifiers", arg);
        WriteTable(&lexer.IdentifierTable, formatBuffer, 20, 0);

        metac_identifier_table_t newIdTable = ReadTable(formatBuffer);
        sprintf(formatBuffer, "%s.identifiers.new", arg);
        WriteTable(&newIdTable, formatBuffer, 20, "new");

        printf("First entry is in read out table: %d\n",
               IsInTable(&newIdTable, firstEntry.HashKey,
                         firstEntry.Ptr)
        );


        sprintf(formatBuffer, "%s.strings", arg);
        WriteTable(&lexer.StringTable, formatBuffer, 12, 0);


    }


    return errored;
}
