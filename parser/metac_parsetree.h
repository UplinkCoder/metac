#ifndef _METAC_PARSETREE_H_
#define _METAC_PARSETREE_H_

#include "../os/compat.h"
#include "metac_identifier_table.h"
// #include "metac_type_table.h"

#ifndef AT
#  define AT(...)
#endif

#include "metac_node.h"
#include "metac_lexer.h"
#ifndef NO_PREPROCESSOR
#  include "metac_preproc.h"
#endif
#pragma pack(push, 1)

// XXX: KEEP COMPATIBLE WITH parse_number_flags_t
typedef enum number_flags_t
{
    number_flag_hex       = (1 << 0),
    number_flag_octal     = (1 << 1),
    number_flag_float     = (1 << 2),

    number_flag_unsigned =  (1 << 3),

    number_flag_long =      (1 << 4),
    number_flag_long_long = (1 << 5)
} number_flags_t;

#define EXPR_HEADER \
    metac_expr_kind_t Kind; \
    metac_location_ptr_t LocationIdx; \
    uint32_t Hash; \
    uint32_t Serial;

typedef struct metac_expr_header_t
{
    EXPR_HEADER
} metac_expr_header_t;

typedef struct expr_function_t
{
    EXPR_HEADER

    struct decl_parameter_t* Parameters;
    uint32_t ParameterCount;

    struct metac_stmt_t* FunctionBody;
} expr_function_t;

typedef struct expr_argument_t
{
    EXPR_HEADER

    struct metac_expr_t* Expr;
    struct expr_argument_t* Next;
} expr_argument_t;

typedef struct expr_argument_list_t
{
    EXPR_HEADER

    expr_argument_t* Arguments;
    uint32_t ArgumentCount;
} expr_argument_list_t;

typedef struct expr_tuple_t
{
    EXPR_HEADER

    struct metac_expr_t* Expr;
    struct expr_tuple_t* Next;
} expr_tuple_t;

#define METAC_MAX_EXP_BODY_SIZE 24

typedef struct _metac_exp_body_t
{
    uint8_t Bytes[METAC_MAX_EXP_BODY_SIZE];
} _metac_exp_body_t;

typedef struct metac_expr_t
{
    EXPR_HEADER

    union // switch(Kind)
    {
        // invalid case expr_max, expr_invalid :

        // case expr_add, expr_sub, expr_mul, expr_div, expr_cat, expr_catAss, expr_assign,
        // expr_lt, expr_gt, expr_le, expr_ge, expr_spaceShip :
        struct {
            struct metac_expr_t* _E1;
            struct metac_expr_t* E2;
        };
        //case expr_ternary:
        struct {
            struct metac_expr_t* _E1_;
            struct metac_expr_t* _E2;
            struct metac_expr_t* Econd;
        };
        // case expr_inject, expr_eject, expr_assert, expr_outerParen, expr_outer,
        // expr_run :
        struct {
            struct metac_expr_t* E1;
        };
        // case expr_eject:
        struct {
            struct metac_stmt_t* EjectedStmt;
        };
        // case expr_sizeof:
        struct {
            struct metac_expr_t* SizeofExp;
        };
        // case expr_tuple:
        struct {
            struct expr_tuple_t* TupleExprList;
            uint32_t TupleExprCount;
        };
        // case expr_cast:
        struct {
            struct metac_expr_t* CastExp;
            struct decl_type_t* CastType;
        };
        // case expr_type:
        struct {
            struct decl_type_t* TypeExp;
        };

        // case expr_argument:
        struct
        {
            struct metac_expr_t* Expr;
            struct expr_argument_t* Next;
        };
        // case identifier_exp :
        struct {
            uint32_t IdentifierKey;
            metac_identifier_ptr_t IdentifierPtr;
        };
        // case expr_string :
        struct {
            uint32_t StringKey;
            metac_identifier_ptr_t StringPtr;
        };
        // case expr_char:
        struct {
            uint32_t CharKey;
            char Chars[8];
        };

        struct {
            union {
                // case expr_signed_integer :
                int64_t ValueI64;
                // case expr_unsigned_integer :
                uint64_t ValueU64;
                // case expr_float:
                float ValueF23;
                // case expr_double:
                double ValueF52;
            };
            number_flags_t NumberFlags;
        };
    };
} metac_expr_t;

#define COPY_EXP_BODY(SRCP, DSTP) do { \
    (DSTP)->Kind = (SRCP)->Kind; \
    (DSTP)->LocationIdx = (SRCP)->LocationIdx; \
} while (0);

//_Static_assert((sizeof(metac_expr_t) - sizeof(metac_expr_header_t)) <= METAC_MAX_EXP_BODY_SIZE, "Dumb");
#define STMT_HEADER \
    metac_stmt_kind_t Kind; \
    metac_location_ptr_t LocationIdx; \
    uint32_t Hash; \
    uint32_t Serial; \
    struct metac_stmt_t* Next;

typedef struct stmt_header_t
{
    STMT_HEADER
} stmt_header_t;

typedef struct stmt_block_t
{
    STMT_HEADER

    struct metac_stmt_t* Body;
    uint32_t StmtCount;
} stmt_block_t;

typedef struct stmt_break_t
{
    STMT_HEADER
} stmt_break_t;

typedef struct stmt_continue_t
{
    STMT_HEADER
} stmt_continue_t;

typedef struct stmt_yield_t
{
    STMT_HEADER

    metac_expr_t* YieldExp;
} stmt_yield_t;

typedef struct stmt_scope_t
{
    STMT_HEADER

    scope_kind_t ScopeKind;
    struct metac_stmt_t* Stmt;
} stmt_scope_t;

typedef struct stmt_defer_t
{
    STMT_HEADER

    struct metac_stmt_t* Stmt;
} stmt_defer_t;

typedef struct stmt_for_t
{
    STMT_HEADER

    /// can be either an expression or a declaration
    metac_node_t ForInit;
    metac_expr_t* ForCond;
    metac_expr_t* ForPostLoop;

    struct metac_stmt_t* ForBody;
} stmt_for_t;

typedef struct stmt_while_t
{
    STMT_HEADER

    metac_expr_t* WhileExp;
    struct metac_stmt_t* WhileBody;
} stmt_while_t;

typedef struct stmt_run_t
{
    STMT_HEADER

    struct metac_stmt_t* RunBody;
} stmt_run_t;


typedef struct stmt_case_t
{
    STMT_HEADER

    metac_expr_t* CaseExp;
    struct metac_stmt_t* CaseBody;
} stmt_case_t;

typedef struct stmt_goto_t
{
    STMT_HEADER

    metac_identifier_ptr_t GotoLabel;
} stmt_goto_t;

typedef struct stmt_expr_t
{
    STMT_HEADER

    metac_expr_t* Expr;
} stmt_expr_t;

typedef struct stmt_decl_t
{
    STMT_HEADER

    struct metac_decl_t* Decl;
} stmt_decl_t;

typedef struct stmt_if_t
{
    STMT_HEADER

    struct metac_expr_t* IfCond;
    struct metac_stmt_t* IfBody;
    struct metac_stmt_t* ElseBody;
} stmt_if_t;

typedef struct stmt_label_t
{
    STMT_HEADER

    metac_identifier_ptr_t Label;
} stmt_label_t;

typedef struct stmt_return_t
{
    STMT_HEADER

    metac_expr_t* ReturnExp;
} stmt_return_t;

typedef struct stmt_switch_t
{
    STMT_HEADER

    metac_expr_t* SwitchExp;
    struct stmt_block_t* SwitchBody;
} stmt_switch_t;

typedef struct stmt_do_while_t
{
    STMT_HEADER

    metac_expr_t* DoWhileExp;
    struct metac_stmt_t* DoWhileBody;
} stmt_do_while_t;

typedef struct stmt_casebody_t
{
    void* invalid_as_parse_node;
} stmt_casebody_t;

typedef struct stmt_empty_t
{
    STMT_HEADER
} stmt_empty_t;

typedef struct stmt_comment_t
{
    STMT_HEADER

    const char* Text;
    uint32_t Length;
} stmt_comment_t;

#define MEMBER(KIND) \
    KIND##_t KIND;

typedef struct decl_type_tuple_t decl_type_tuple_t;

typedef struct metac_stmt_t
{
    union // switch(Kind)
    {
        struct {
            STMT_HEADER
        };

        FOREACH_STMT_KIND(MEMBER)
    };
} metac_stmt_t;

#define DECL_HEADER \
    metac_decl_kind_t Kind; \
    metac_location_ptr_t LocationIdx; \
    uint32_t Hash; \
    uint32_t Serial; \
    metac_storageclasses_t StorageClass;

typedef enum metac_type_modifiers
{
    typemod_none,

    typemod_const    = (1 << 0),
    typemod_unsigned = (1 << 1),
    typemod_signed   = (1 << 2),

} metac_type_modifiers;

typedef enum metac_storageclasses_t
{
    storageclass_none     = 0,
    storageclass_static   = (1 << 1),
    storageclass_inline   = (1 << 2),
    storageclass_extern   = (1 << 3),
    storageclass_volatile = (1 << 4),
    storageclass_thread   = (1 << 5),
    // the global storage class is for the repl mainly
    // it's potentially subject to future removal
    storageclass_global   = (1 << 6),
    storageclass_local    = (1 << 7),

    storageclass_scope    = (1 << 8),
} metac_storageclasses_t;

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
// DO NOT CHANGE THE ORDER FROM HERE
// XXX: Order needs to be in sync with the type tokens in metac_lexer.h
    type_void,
    type_bool,
    type_char,
    type_short,
    type_int,
    type_long,
    type_size_t,

    type_float,
    type_double,
//TO HERE
    type_long_long,
    type_long_double,
// ALSO DON'T CHANGE ANYTHING FROM HERE
    type_unsigned_char,
    type_unsigned_short,
    type_unsigned_int,
    type_unsigned_long,
//TO HERE
    type_unsigned_long_long,

    type_type,
    type_identifier,

    type_ptr,
    type_array,

    type_map,

    type_tuple,

    type_template_instance,

    type_modifiers,

    type_code,

    type_max
} metac_type_kind_t;


#define TYPE_HEADER \
    metac_type_kind_t TypeKind; \
    metac_type_modifiers TypeModifiers;


typedef struct decl_type_t
{
    union {
        struct {
            DECL_HEADER
        };
        // metac_type_header_t TypeHeader;
    };
    TYPE_HEADER

    // only set if TypeKind == type_identifier
    metac_identifier_ptr_t TypeIdentifier;
    //
} decl_type_t;

typedef struct decl_comment_t
{
    DECL_HEADER

    const char* Text;
    uint32_t Length;
} decl_comment_t;

typedef struct decl_label_t
{
    DECL_HEADER

    metac_identifier_ptr_t Identifier;

    struct metac_decl_t* Decl;
} decl_label_t;

typedef struct decl_variable_t
{
    DECL_HEADER

    decl_type_t* VarType;

    metac_identifier_ptr_t VarIdentifier;

    metac_expr_t* VarInitExpr;

} decl_variable_t;


typedef struct decl_field_t
{
    DECL_HEADER

    decl_variable_t* Field;

    struct decl_field_t* Next;
} decl_field_t;


typedef struct decl_parameter_t
{
    DECL_HEADER

    decl_variable_t* Parameter;

    struct decl_parameter_t* Next;
} decl_parameter_t;

typedef struct decl_parameter_list_t
{
    decl_parameter_t* List;
    uint32_t ParameterCount;
    uint32_t Hash;
    bool IsVariadic;
} decl_parameter_list_t;

#ifndef NO_PREPROCESSOR

typedef struct decl_preproc_t
{
    DECL_HEADER

    metac_preprocessor_directive_t DirectiveKind;

    union
    {
        metac_preprocessor_define_ptr_t DefinePtr;
    };
} decl_preproc_t;

#endif

typedef struct decl_function_t
{
    DECL_HEADER

    AT(maybeEmpty) decl_type_t* YieldType;

    decl_type_t* ReturnType;

    decl_parameter_t* Parameters;

    uint32_t ParameterCount;
    metac_identifier_ptr_t Identifier;

    stmt_block_t* FunctionBody;
} decl_function_t;

typedef struct decl_type_ptr_t
{
    DECL_HEADER

    TYPE_HEADER

    decl_type_t* ElementType;
} decl_type_ptr_t;

typedef struct decl_enum_member_t
{
    DECL_HEADER

    metac_identifier_ptr_t Identifier;

    metac_expr_t* Value;

    struct decl_enum_member_t* Next;
} decl_enum_member_t;

typedef struct decl_type_enum_t
{
    DECL_HEADER

    TYPE_HEADER

    metac_identifier_ptr_t Identifier;

    decl_enum_member_t* Members;

    uint32_t MemberCount;

    decl_type_t* BaseType;

} decl_type_enum_t;

typedef struct decl_type_functiontype_t
{
    DECL_HEADER

    TYPE_HEADER

    AT(maybeEmpty) decl_type_t* YieldType;

    decl_type_t* ReturnType;

    //TODO maybe use decl_parameter_list_t here?

    /// this may or may not include identifiers
    AT(maybeEmpty) decl_parameter_t* Parameters;

    uint32_t ParameterCount;
} decl_type_functiontype_t;

typedef struct decl_type_array_t
{
    DECL_HEADER

    TYPE_HEADER

    decl_type_t* ElementType;

    metac_expr_t* Dim;
} decl_type_array_t;

typedef struct decl_type_struct_t
{
    DECL_HEADER

    TYPE_HEADER

    metac_identifier_ptr_t Identifier;

    metac_identifier_ptr_t BaseIdentifier;

    AT(maybeEmpty) decl_parameter_t* Parameters;

    AT(maybeEmpty) struct decl_field_t* Fields;

    uint32_t ParameterCount;
    uint32_t FieldCount;
} decl_type_struct_t;

/// this is not used, it only exits
/// for tooling purposes
typedef struct decl_type_union_t
{
    DECL_HEADER

    TYPE_HEADER

    metac_identifier_ptr_t Identifier;

    metac_identifier_ptr_t BaseIdentifier;

    AT(maybeEmpty) decl_parameter_t* Parameters;

    AT(maybeEmpty) struct decl_field_t* Fields;

    uint32_t ParameterCount;
    uint32_t FieldCount;
} decl_type_union_t;

typedef struct decl_type_typedef_t
{
    DECL_HEADER

    TYPE_HEADER

    decl_type_t* Type;

    metac_identifier_ptr_t Identifier;
} decl_type_typedef_t;


typedef struct decl_type_typeof_t
{
    DECL_HEADER

    TYPE_HEADER

    struct metac_expr_t* Expr;

    metac_identifier_ptr_t Identifier;
} decl_type_typeof_t;

typedef struct decl_type_tuple_t
{
    DECL_HEADER

    TYPE_HEADER

    decl_type_t** Types;
    uint32_t TypeCount;
} decl_type_tuple_t;

typedef struct decl_type_template_instance_t
{
    DECL_HEADER

    TYPE_HEADER

    metac_identifier_ptr_t Identifier;

    expr_argument_t* Arguments;

    uint32_t ArgumentCount;
} decl_type_template_instance_t;


typedef struct decl_type_modifier_t
{
    DECL_HEADER

    TYPE_HEADER
} decl_type_modifies_t;

typedef struct metac_decl_t
{
    union {
        struct {
            DECL_HEADER
        };

        FOREACH_DECL_KIND(MEMBER)
    };
#undef MEMBER
} metac_decl_t;
#pragma pack(pop)

typedef int (*walker_function_t) (metac_node_t node, void * ctx);

#ifdef NDEBUG
#  define MetaCDecl_Walk(DECL, FUNC, CTX) \
      MetaCDecl_Walk_Real(DECL, FUNC, (void*)CTX)
#else
  int MetaCDecl_Walk_Debug(metac_decl_t* decl, const char* fn_name, walker_function_t walker_fn, void* ctx);
#  define MetaCDecl_Walk(DECL, FUNC, CTX) \
      MetaCDecl_Walk_Debug(DECL, #FUNC, FUNC, (void*)CTX)
#endif
int MetaCDecl_Walk_Real(metac_decl_t* decl, walker_function_t walker_fn, void* ctx);

#endif
