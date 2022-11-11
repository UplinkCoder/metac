#ifndef _METAC_PARSETREE_H_
#define _METAC_PARSETREE_H_

#include "../os/compat.h"
#include "metac_identifier_table.h"
// #include "metac_type_table.h"

#include "metac_node.h"

#pragma pack(push, 1)

#define EXPRESSION_HEADER \
    metac_expression_kind_t Kind; \
    uint32_t LocationIdx; \
    uint32_t Hash; \
    uint32_t Serial;

typedef struct metac_expression_header_t
{
    EXPRESSION_HEADER
} metac_expression_header_t;

typedef struct exp_argument_t
{
    EXPRESSION_HEADER

    struct metac_expression_t* Expression;
    struct exp_argument_t* Next;
} exp_argument_t;

typedef struct exp_tuple_t
{
    EXPRESSION_HEADER

    struct metac_expression_t* Expression;
    struct exp_tuple_t* Next;
} exp_tuple_t;

#define METAC_MAX_EXP_BODY_SIZE 24

typedef struct _metac_exp_body_t
{
    uint8_t Bytes[METAC_MAX_EXP_BODY_SIZE];
} _metac_exp_body_t;

typedef struct metac_expression_t
{
    EXPRESSION_HEADER

    union // switch(Kind)
    {
        // invalid case exp_max, exp_invalid :

        // case exp_add, exp_sub, exp_mul, exp_div, exp_cat, exp_catAss, exp_assign,
        // exp_lt, exp_gt, exp_le, exp_ge, exp_spaceShip :
        struct {
            struct metac_expression_t* _E1;
            struct metac_expression_t* E2;
        };
        //case exp_ternary:
        struct {
            struct metac_expression_t* _E1_;
            struct metac_expression_t* _E2;
            struct metac_expression_t* Econd;
        };
        // case  exp_inject, exp_eject, exp_assert, exp_outerParen, exp_outer :
        struct {
            struct metac_expression_t* E1;
        };
        // case exp_sizeof:
        struct {
            struct metac_expression_t* SizeofExp;
        };
        // case exp_tuple:
        struct {
            struct exp_tuple_t* TupleExpressionList;
            uint32_t TupleExpressionCount;
        };
        // case exp_cast:
        struct {
            struct metac_expression_t* CastExp;
            struct decl_type_t* CastType;
        };
        // case exp_type:
        struct {
            struct decl_type_t* TypeExp;
        };

        // case exp_argument:
        struct
        {
            struct metac_expression_t* Expression;
            struct exp_argument_t* Next;
        };
        // case identifier_exp :
        struct {
            uint32_t IdentifierKey;
            metac_identifier_ptr_t IdentifierPtr;
        };
        // case exp_string :
        struct {
            uint32_t StringKey;
            metac_identifier_ptr_t StringPtr;
        };
        // case exp_char:
        struct {
            uint32_t CharKey;
            char Chars[8];
        };

        // case exp_signed_integer :
        int64_t ValueI64;
        // case exp_unsigned_integer :
        uint64_t ValueU64;
        // case exp_float:
        float ValueF23;
    };
} metac_expression_t;

#define COPY_EXP_BODY(SRCP, DSTP) do { \
    (DSTP)->Kind = (SRCP)->Kind; \
    (DSTP)->LocationIdx = (SRCP)->LocationIdx; \
} while (0);

//_Static_assert((sizeof(metac_expression_t) - sizeof(metac_expression_header_t)) <= METAC_MAX_EXP_BODY_SIZE, "Dumb");
#define STATEMENT_HEADER \
    metac_statement_kind_t Kind; \
    uint32_t LocationIdx; \
    uint32_t Hash; \
    uint32_t Serial; \
    struct metac_statement_t* Next;

typedef struct statement_header_t
{
    STATEMENT_HEADER
} statement_header_t;

typedef struct stmt_block_t
{
    STATEMENT_HEADER

    struct metac_statement_t* Body;
    uint32_t StatementCount;
} stmt_block_t;

typedef struct stmt_break_t
{
    STATEMENT_HEADER
} stmt_break_t;

typedef struct stmt_continue_t
{
    STATEMENT_HEADER
} stmt_continue_t;

typedef struct stmt_yield_t
{
    STATEMENT_HEADER

    metac_expression_t* YieldExp;
} stmt_yield_t;

typedef struct stmt_scope_t
{
    STATEMENT_HEADER

    scope_kind_t ScopeKind;
    struct metac_statement_t* Stmt;
} stmt_scope_t;

typedef struct stmt_defer_t
{
    STATEMENT_HEADER

    struct metac_statement_t* Stmt;
} stmt_defer_t;

typedef struct stmt_for_t
{
    STATEMENT_HEADER

    /// can be either an expression or a declaration
    metac_node_t ForInit;
    metac_expression_t* ForCond;
    metac_expression_t* ForPostLoop;

    struct metac_statement_t* ForBody;
} stmt_for_t;

typedef struct stmt_while_t
{
    STATEMENT_HEADER

    metac_expression_t* WhileExp;
    struct metac_statement_t* WhileBody;
} stmt_while_t;

typedef struct stmt_case_t
{
    STATEMENT_HEADER

    metac_expression_t* CaseExp;
    struct metac_statement_t* CaseBody;
} stmt_case_t;

typedef struct stmt_goto_t
{
    STATEMENT_HEADER

    metac_identifier_ptr_t GotoLabel;
} stmt_goto_t;

typedef struct stmt_exp_t
{
    STATEMENT_HEADER

    metac_expression_t* Expression;
} stmt_exp_t;

typedef struct stmt_decl_t
{
    STATEMENT_HEADER

    struct metac_declaration_t* Declaration;
} stmt_decl_t;

typedef struct stmt_if_t
{
    STATEMENT_HEADER

    struct metac_expression_t* IfCond;
    struct metac_statement_t* IfBody;
    struct metac_statement_t* ElseBody;
} stmt_if_t;

typedef struct stmt_label_t
{
    STATEMENT_HEADER

    metac_identifier_ptr_t Label;
} stmt_label_t;

typedef struct stmt_return_t
{
    STATEMENT_HEADER

    metac_expression_t* ReturnExp;
} stmt_return_t;

typedef struct stmt_switch_t
{
    STATEMENT_HEADER

    metac_expression_t* SwitchExp;
    struct stmt_block_t* SwitchBody;
} stmt_switch_t;

typedef struct stmt_do_while_t
{
    STATEMENT_HEADER

    metac_expression_t* DoWhileExp;
    struct metac_statement_t* DoWhileBody;
} stmt_do_while_t;

typedef struct stmt_casebody_t
{
    void* invalid_as_parse_node;
} stmt_casebody_t;

typedef struct stmt_empty_t
{
    STATEMENT_HEADER
} stmt_empty_t;

typedef struct stmt_comment_t
{
    STATEMENT_HEADER

    const char* Text;
    uint32_t Length;
} stmt_comment_t;

#define MEMBER(KIND) \
    KIND##_t KIND;

typedef struct decl_type_tuple_t decl_type_tuple_t;

typedef struct metac_statement_t
{
    union // switch(Kind)
    {
        struct {
            STATEMENT_HEADER
        };

        FOREACH_STMT_KIND(MEMBER)
    };
} metac_statement_t;

#define DECLARATION_HEADER \
    metac_declaration_kind_t Kind; \
    uint32_t LocationIdx; \
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

    type_modifiers,

    type_max
} metac_type_kind_t;


#define TYPE_HEADER \
    metac_type_kind_t TypeKind; \
    metac_type_modifiers TypeModifiers;


typedef struct decl_type_t
{
    union {
        struct {
            DECLARATION_HEADER
        };
        // metac_type_header_t TypeHeader;
    };
    TYPE_HEADER

    // only set if TypeKind == type_identifier
    metac_identifier_ptr_t TypeIdentifier;
} decl_type_t;

typedef struct decl_comment_t
{
    DECLARATION_HEADER

    const char* Text;
    uint32_t Length;
} decl_comment_t;

typedef struct decl_label_t
{
    DECLARATION_HEADER

    metac_identifier_ptr_t Identifier;

    struct metac_declaration_t* Decl;
} decl_label_t;

typedef struct decl_variable_t
{
    DECLARATION_HEADER

    decl_type_t* VarType;

    metac_identifier_ptr_t VarIdentifier;

    metac_expression_t* VarInitExpression;

} decl_variable_t;


typedef struct decl_field_t
{
    DECLARATION_HEADER

    decl_variable_t* Field;

    struct decl_field_t* Next;
} decl_field_t;


typedef struct decl_parameter_t
{
    DECLARATION_HEADER

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

typedef struct decl_function_t
{
    DECLARATION_HEADER

    decl_type_t* ReturnType;

    decl_parameter_t* Parameters;

    uint32_t ParameterCount;
    metac_identifier_ptr_t Identifier;

    stmt_block_t* FunctionBody;
} decl_function_t;

typedef struct decl_type_ptr_t
{
    DECLARATION_HEADER

    TYPE_HEADER

    decl_type_t* ElementType;
} decl_type_ptr_t;

typedef struct decl_enum_member_t
{
    DECLARATION_HEADER

    metac_identifier_ptr_t Name;

    metac_expression_t* Value;

    struct decl_enum_member_t* Next;
} decl_enum_member_t;

typedef struct decl_type_enum_t
{
    DECLARATION_HEADER

    TYPE_HEADER

    metac_identifier_ptr_t Identifier;

    decl_enum_member_t* Members;

    uint32_t MemberCount;

    decl_type_t* BaseType;

} decl_type_enum_t;

typedef struct decl_type_functiontype_t
{
    DECLARATION_HEADER

    TYPE_HEADER

    decl_type_t* ReturnType;

    //TODO maybe use decl_parameter_list_t here?

    /// this may or may not include identifiers
    decl_parameter_t* Parameters;

    uint32_t ParameterCount;
} decl_type_functiontype_t;

typedef struct decl_type_array_t
{
    DECLARATION_HEADER

    TYPE_HEADER

    decl_type_t* ElementType;

    metac_expression_t* Dim;
} decl_type_array_t;

typedef struct decl_type_struct_t
{
    DECLARATION_HEADER

    TYPE_HEADER

    metac_identifier_ptr_t Identifier;

    metac_identifier_ptr_t BaseIdentifier;

    struct decl_field_t* Fields;

    uint32_t FieldCount;
} decl_type_struct_t;

/// this is not used, it only exits
/// for tooling purposes
typedef struct decl_type_union_t
{
    DECLARATION_HEADER

    TYPE_HEADER

    metac_identifier_ptr_t Identifier;

    metac_identifier_ptr_t BaseIdentifier;

    struct decl_field_t* Fields;

    uint32_t FieldCount;
} decl_type_union_t;

typedef struct decl_type_typedef_t
{
    DECLARATION_HEADER

    TYPE_HEADER

    decl_type_t* Type;

    metac_identifier_ptr_t Identifier;
} decl_type_typedef_t;


typedef struct decl_type_typeof_t
{
    DECLARATION_HEADER

    TYPE_HEADER

    struct metac_expression_t* Exp;

    metac_identifier_ptr_t Identifier;
} decl_type_typeof_t;

typedef struct decl_type_tuple_t
{
    DECLARATION_HEADER

    TYPE_HEADER

    decl_type_t** Types;
    uint32_t TypeCount;
} decl_type_tuple_t;

typedef struct decl_type_modifier_t
{
    DECLARATION_HEADER

    TYPE_HEADER
} decl_type_modifies_t;

typedef struct metac_declaration_t
{
    union {
        struct {
            DECLARATION_HEADER
        };

        FOREACH_DECL_KIND(MEMBER)
    };
#undef MEMBER
} metac_declaration_t;
#pragma pack(pop)

typedef int (*walker_function_t) (metac_node_t node, void * ctx);

#ifdef NDEBUG
#  define MetaCDeclaration_Walk(DECL, FUNC, CTX) \
      MetaCDeclaration_Walk_Real(DECL, FUNC, (void*)CTX)
#else
  int MetaCDeclaration_Walk_Debug(metac_declaration_t* decl, const char* fn_name, walker_function_t walker_fn, void* ctx);
#  define MetaCDeclaration_Walk(DECL, FUNC, CTX) \
      MetaCDeclaration_Walk_Debug(DECL, #FUNC, FUNC, (void*)CTX)
#endif
int MetaCDeclaration_Walk_Real(metac_declaration_t* decl, walker_function_t walker_fn, void* ctx);

#endif
