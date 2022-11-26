#ifndef _METAC_SEMATREE_H_
#define _METAC_SEMATREE_H_

#include "../os/compat.h"
#include "../parser/metac_identifier_table.h"
#include "metac_type_table.h"
#include "metac_scope.h"
#include "../parser/metac_parsetree.h"

#define ERROR_PARENT_INDEX_V -1

#define SEMA_EXPRESSION_HEADER \
    EXPRESSION_HEADER \
    metac_type_index_t TypeIndex;

typedef enum metac_storage_kind_t
{
    storage_unknown = 0,

    storage_stack,               // 1
    storage_register,            // 2

    storage_thread_local,        // 3
    storage_task_local,          // 4

    storage_static,              // 5
    storage_static_thread_local, // 6
    storage_static_task_local,   // 7

    storage_parameter,           // 8
    storage_local,               // 9

    storage_global,              // A

    storage_external,            // B

    storage_invalid = 0xE,
} metac_storage_kind_t;

typedef struct metac_storage_location_t
{
    union{
        uint32_t v;
        struct {
            uint32_t Offset : 28;
            metac_storage_kind_t Kind : 4;
        };
    };
} metac_storage_location_t;

#define STORAGE_V(KIND, OFFSET) \
    (((KIND) << 28) | (OFFSET))

#define STORAGE_KIND(STORAGE) \
    ((metac_storage_kind_t) ((STORAGE).v >> 28) & 0xF)

typedef struct sema_exp_argument_list_t
{
    struct metac_sema_expression_t** Arguments;
    uint32_t ArgumentCount;
} sema_exp_argument_list_t;

typedef struct metac_sema_expression_header_t
{
    SEMA_EXPRESSION_HEADER
} metac_sema_expression_header_t;

#pragma pack(push, 1)
typedef struct sema_exp_call_t
{
    struct metac_sema_expression_t* Function;
    struct metac_sema_expression_t** Arguments;
    uint32_t ArgumentCount;
} sema_exp_call_t;


typedef struct metac_sema_expression_t
{
    SEMA_EXPRESSION_HEADER

    union // switch(Kind)
    {
        // invalid case exp_max, exp_invalid :

        // case exp_add, exp_sub, exp_mul, exp_div, exp_cat, exp_catAss, exp_assign,
        // exp_lt, exp_gt, exp_le, exp_ge, exp_spaceShip :
        struct {
            struct metac_sema_expression_t* _E1;
            struct metac_sema_expression_t* E2;
        };
        // case exp_ternary:
        struct {
            struct metac_sema_expression_t* _E1_;
            struct metac_sema_expression_t* _E2;
            struct metac_sema_expression_t* Econd;
        };
        // case exp_sizeof:
        // case  exp_inject, exp_eject, exp_assert, exp_outerParen, exp_outer :
        struct metac_sema_expression_t* E1;
        // case exp_cast:
        struct {
            struct metac_sema_expression_t* CastExp;
            struct metac_type_index_t CastType;
        };
        // case exp_dot, exp_arrow:
        struct {
            struct metac_sema_expression_t* AggExp;
            uint32_t AggMemberIndex;
            struct metac_sema_expression_t* DotE2;
        };
        // case exp_type
        struct metac_type_index_t TypeExp;
        // case exp_tuple
        struct {
            struct metac_sema_expression_t** TupleExpressions;
            uint32_t TupleExpressionCount;
        };
        // case exp_argument:
        sema_exp_argument_list_t* ArgumentList;
        // case exp_function:
        struct sema_decl_function_t* Function;
        // case exp_call:
        struct sema_exp_call_t Call;

        // case unknown_value_exp:
        metac_expression_t* Expression;
        // case variable_exp:
        struct sema_decl_variable_t* Variable;
        // case field_exp:
        struct metac_type_aggregate_field_t* Field;
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
        // case exp_float :
        float ValueF23;

        uint8_t _Body[METAC_MAX_EXP_BODY_SIZE];
    };
} metac_sema_expression_t;

#ifndef _MSC_VER
_Static_assert(sizeof(metac_sema_expression_t) - sizeof(metac_sema_expression_header_t) <= METAC_MAX_EXP_BODY_SIZE,
    "METAC_MAX_EXP_BODY_SIZE is less than the actual expression body size");
#endif

#define SEMA_STATEMENT_HEADER \
    metac_stmt_kind_t Kind; \
    uint32_t LocationIdx; \
    uint32_t Hash; \
    uint32_t Serial;


typedef struct sema_statement_header_t
{
    SEMA_STATEMENT_HEADER
} sema_statement_header_t;

typedef struct sema_stmt_block_t
{
    SEMA_STATEMENT_HEADER

    ARENA_ARRAY(struct metac_sema_statement_t*, Body)
    uint32_t StatementCount;
} sema_stmt_block_t;

typedef struct sema_stmt_break_t
{
    SEMA_STATEMENT_HEADER
} sema_stmt_break_t;

typedef struct sema_stmt_continue_t
{
    SEMA_STATEMENT_HEADER
} sema_stmt_continue_t;

typedef struct sema_stmt_yield_t
{
    SEMA_STATEMENT_HEADER

    metac_sema_expression_t* YieldExp;
} sema_stmt_yield_t;

typedef struct sema_stmt_scope_t
{
    SEMA_STATEMENT_HEADER

    scope_kind_t ScopeKind;
    struct metac_sema_statement_t* Stmt;
} sema_stmt_scope_t;

typedef struct sema_stmt_defer_t
{
    SEMA_STATEMENT_HEADER

    struct metac_sema_statement_t* DeferStmt;
} sema_stmt_defer_t;

typedef struct sema_stmt_for_t
{
    SEMA_STATEMENT_HEADER

    metac_node_t ForInit;
    metac_sema_expression_t* ForCond;
    metac_sema_expression_t* ForPostLoop;

    struct metac_sema_statement_t* ForBody;

    metac_scope_t* Scope;
} sema_stmt_for_t;

typedef struct sema_stmt_while_t
{
    SEMA_STATEMENT_HEADER

    metac_sema_expression_t* WhileExp;

    struct metac_sema_statement_t* WhileBody;
} sema_stmt_while_t;

typedef struct sema_stmt_casebody_t
{
    SEMA_STATEMENT_HEADER

    ARENA_ARRAY(struct metac_sema_statement_t*, Statements)

    uint32_t StatementCount;
} sema_stmt_casebody_t;

typedef struct sema_stmt_case_t
{
    SEMA_STATEMENT_HEADER

    metac_sema_expression_t* CaseExp;

    sema_stmt_casebody_t* CaseBody;
} sema_stmt_case_t;

typedef struct sema_stmt_goto_t
{
    SEMA_STATEMENT_HEADER

    metac_identifier_ptr_t GotoLabel;
} sema_stmt_goto_t;

typedef struct sema_stmt_exp_t
{
    SEMA_STATEMENT_HEADER

    metac_sema_expression_t* Expression;
} sema_stmt_exp_t;

typedef struct sema_stmt_decl_t
{
    SEMA_STATEMENT_HEADER

    struct metac_sema_declaration_t* Declaration;
} sema_stmt_decl_t;

typedef struct sema_stmt_if_t
{
    SEMA_STATEMENT_HEADER

    struct metac_sema_expression_t* IfCond;
    struct metac_sema_statement_t* IfBody;
    struct metac_sema_statement_t* ElseBody;
} sema_stmt_if_t;

typedef struct sema_stmt_label_t
{
    SEMA_STATEMENT_HEADER

    metac_identifier_ptr_t Label;
} sema_stmt_label_t;

typedef struct sema_stmt_return_t
{
    SEMA_STATEMENT_HEADER

    metac_sema_expression_t* ReturnExp;
} sema_stmt_return_t;

typedef struct sema_stmt_switch_t
{
    SEMA_STATEMENT_HEADER

    metac_sema_expression_t* SwitchExp;
    struct sema_stmt_block_t* SwitchBody;
} sema_stmt_switch_t;

typedef struct sema_stmt_do_while_t
{
    SEMA_STATEMENT_HEADER

    metac_sema_expression_t* WhileExp;
    struct metac_sema_statement_t* WhileBody;
} sema_stmt_do_while_t;

typedef struct metac_sema_statement_t
{
    union // switch(Kind)
    {
        struct {
            SEMA_STATEMENT_HEADER
        };

        // invalid case stmt_max, stmt_invalid :
        // case stmt_if :
        sema_stmt_if_t sema_stmt_if;
        // case stmt_exp :
        sema_stmt_exp_t sema_stmt_exp;
        // case stmt_block :
        sema_stmt_block_t sema_stmt_block;
        // case stmt_label :
        sema_stmt_label_t sema_stmt_label;
        // case stmt_goto :
        sema_stmt_goto_t sema_stmt_goto;
        // case stmt_yield :
        sema_stmt_yield_t sema_stmt_yield;
        // case stmt_return :
        sema_stmt_return_t sema_stmt_return;
        // case stmt_while :
        sema_stmt_while_t sema_stmt_while;
        // case stmt_decl :
        sema_stmt_decl_t sema_stmt_decl;
        // case stmt_switch :
        sema_stmt_switch_t sema_stmt_switch;
        // case stmt_case :
        sema_stmt_case_t sema_stmt_case;
        // case stmt_casbody :
        sema_stmt_casebody_t sema_stmt_casebody;
        // case stmt_for :
        sema_stmt_for_t sema_stmt_for;
    };
} metac_sema_statement_t;


#define SEMA_DECLARATION_HEADER \
    DECLARATION_HEADER \
    metac_node_header_t* Parent;

typedef struct sema_declaration_header_t
{
    SEMA_DECLARATION_HEADER
} sema_declaration_header_t;

#define SEMA_TYPE_HEADER \
    TYPE_HEADER \
    uint32_t StructuralHash;

typedef struct sema_decl_type_t
{
    SEMA_DECLARATION_HEADER

    // only set if TypeKind == type_identifier
    metac_type_index_t typeIndex;
} sema_decl_type_t;

enum metac_variable_flags_t
{
    variable_none          = (1 <<  0),
    variable_is_parameter  = (1 <<  1),
    variable_is_local      = (1 <<  2),
    variable_address_taken = (1 <<  3),
} metac_variable_flags_t;

typedef struct sema_decl_variable_t
{
    SEMA_DECLARATION_HEADER

    uint32_t VarFlags;

    metac_type_index_t TypeIndex;

    metac_identifier_ptr_t VarIdentifier;

    metac_sema_expression_t* VarInitExpression;

    metac_storage_location_t Storage;
} sema_decl_variable_t;

/// ParameterCount is gotten from TypeIndex;
typedef struct sema_decl_function_t
{
    SEMA_DECLARATION_HEADER

    metac_type_index_t TypeIndex;

    metac_identifier_ptr_t Identifier;

    uint32_t FrameOffset;

    struct metac_scope_t* Scope;

    sema_decl_variable_t* Parameters;

    sema_stmt_block_t* FunctionBody;

    uint32_t IsExternal;
} sema_decl_function_t;

typedef struct sema_decl_type_ptr_t
{
    SEMA_DECLARATION_HEADER

    SEMA_TYPE_HEADER

    metac_type_index_t ElementType;
} sema_decl_type_ptr_t;

typedef struct sema_decl_enum_member_t
{
    SEMA_DECLARATION_HEADER

    metac_identifier_ptr_t Name;

    metac_sema_expression_t* Value;
} sema_decl_enum_member_t;

typedef struct sema_decl_type_enum_t
{
    SEMA_DECLARATION_HEADER

    SEMA_TYPE_HEADER

    metac_identifier_ptr_t Name;

    sema_decl_enum_member_t* Members;

    uint32_t MemberCount;
} sema_decl_type_enum_t;

typedef struct sema_decl_type_functiontype_t
{
    SEMA_DECLARATION_HEADER

    SEMA_TYPE_HEADER

    metac_type_index_t ReturnType;

    metac_type_index_t* ParameterTypes;

    uint32_t ParameterTypeCount;
} sema_decl_type_functiontype_t;

typedef struct sema_decl_type_array_t
{
    SEMA_DECLARATION_HEADER

    SEMA_TYPE_HEADER

    metac_type_index_t* ElementType;

    metac_sema_expression_t* Dim;
} sema_decl_type_array_t;

/*
typedef struct sema_type_aggregate_t
{
    SEMA_DECLARATION_HEADER

    SEMA_TYPE_HEADER

    metac_identifier_ptr_t Identifier;

    metac_scope_t* Scope;

    metac_type_aggregate_field_t* Fields;

    uint32_t FieldCount;

    uint32_t Size;

    uint32_t Alignment;
} sema_type_aggregate_t;
*/
typedef struct sema_decl_type_union_t
{
    SEMA_DECLARATION_HEADER

    SEMA_TYPE_HEADER

    metac_identifier_ptr_t Identifier;

    struct sema_decl_field_t* Fields;

    uint32_t FieldCount;
} sema_decl_type_union_t;

typedef struct sema_decl_type_typedef_t
{
    SEMA_DECLARATION_HEADER

    metac_type_index_t Type;

    metac_identifier_ptr_t Identifier;
} sema_decl_type_typedef_t;

typedef struct metac_sema_declaration_t
{
    union {
        struct {
            SEMA_DECLARATION_HEADER
        };
        sema_decl_variable_t sema_decl_variable;
        metac_type_typedef_t sema_decl_typedef;
        sema_decl_type_t sema_decl_type;
        metac_enum_member_t sema_decl_enum_member;
        metac_type_enum_t sema_decl_type_enum;
        metac_type_ptr_t sema_decl_type_ptr;
        sema_decl_function_t sema_decl_function;
        metac_type_array_t sema_decl_type_array;
        metac_type_aggregate_t sema_type_aggergate;
    };

} metac_sema_declaration_t;
#pragma pack(pop)

#endif // _METAC_SEMATREE_H_
