#include "metac_alloc_node.h"
#include <string.h>
#include <stdlib.h>

#ifndef ALIGN4
#  define ALIGN4(N) (((N) + 3) & ~3)
#endif

#ifndef EMPTY_POINTER_VALUE
#  define EMPTY_POINTER_VALUE (0x1)
#endif

#ifndef offsetof
  #define offsetof(st, m) \
      ((size_t)((char *)&((st *)0)->m - (char *)0))
#endif
/*
#define CHUNK_MAX_IN_BYTES ((65536 * 4) - 1);

typedef struct memory_t
{
    void* MostEmptyChunkFreeArea;
    uint16_t MostEmptyChunkBytesUsed4;

    uint16_t chunksUsed;
    uint16_t chunksAllocated;


    uint16_t MostEmptyChunk;
    uint16_t SecondMostEmptyChunk;
    uint16_t SecondMostEmptyChunkBytesUsed4;

    void* SecondMostEmptyChunkFreeArea;
} memory_t;

typedef struct move_target_t
{
    uint16_t TargetChunk;
    uint16_t Offset4;
} move_target_t;

typedef struct chunk_header_t
{
    uint16_t BytesFree4;
    /// 0xFFFF means there is no move target;


} chunk_header_t;

typedef struct chunk_ptr_t
{
    union
    {
        uint32_t v;
        struct
        {
            uint8_t  Kind : 4;
            /// Chunk 0xFFFF is reserved for extension
            uint16_t Chunk;
            /// offset from chunk start in multiples of 4
            /// i.e. ptr = ((char*)chunks[index].Memory) + (offset4 * 4);
            uint16_t Offset4 : 12;
        }
    }
} chunk_ptr_t;
*/
#pragma pack(push, 2)
typedef struct node_ptr_t
{
    uint32_t          Ptr;
    metac_node_kind_t Kind : 7;
} ndoe_ptr_t;
#pragma pack(pop)

noinline void _newMemRealloc(void** memP, uint32_t* capacityP, const uint32_t elementSize)
{
    uint32_t capacity;
    if (!*memP)
    {
        capacity = cast(int)(1024 / 1.6f);
    }
    else
    {
        capacity = *capacityP;
    }

    {
        capacity = ALIGN4(cast(uint32_t) ((capacity - 1) * 1.6f));
        *memP = realloc(*memP, ((capacity) * elementSize));
    }

    *capacityP = capacity;
}


#define FOREACH_ALLOCATED_TYPE(M) \
    M(metac_expression_t, _newExp) \
    M(metac_declaration_t, _newDecl) \
    M(metac_statement_t, _newStmt) \
    M(metac_sema_expression_t, _newSemaExp) \
    M(sema_decl_function_t, _newSemaFunc) \
    M(sema_decl_variable_t, _newSemaVariables) \
    M(sema_decl_type_enum_t, _newSemaEnums) \
    M(metac_type_aggregate_t, _newSemaStructs) \
    M(metac_type_aggregate_field_t, _newSemaStructFields) \
    M(metac_type_aggregate_t, _newSemaUnions) \
    M(metac_type_aggregate_field_t, _newSemaUnionFields) \
    M(metac_sema_statement_t, _newSemaStatements) \
    M(sema_stmt_block_t, _newSemaBlockStatements) \
    M(metac_scope_t, _newScopes) \
    M(metac_type_typedef_t, _newSemaTypedefs) \
    M(metac_type_functiontype_t, _newSemaFunctiontypes) \
    M(metac_type_ptr_t, _newSemaPtrTypes)


#define FREELIST(PREFIX) \
    struct PREFIX ## _freelist_t

#define DEF_FREELIST_T(TYPE_NAME, PREFIX) \
    FREELIST(PREFIX) { \
        TYPE_NAME* Element; \
        FREELIST(PREFIX)* Next; \
        uint8_t nElements; \
    };

/** makes code of the form
static uint32_t _newExp_size = 0;
static uint32_t _newExp_capacity = 0;
static metac_expression_t* _newExp_mem = 0;
*/

#define DECLARE_STATIC_POOL(TYPE_NAME, PREFIX) \
    static uint32_t PREFIX ##_size = 0; \
    static uint32_t PREFIX ##_capacity = 0; \
    static TYPE_NAME* PREFIX ##_mem = (TYPE_NAME*)0;
/*
    DEF_FREELIST_T(TYPE_NAME, PREFIX) \
    static FREELIST(PREFIX)* PREFIX ##_freelist = (FREELIST(PREFIX)*)0;
*/
FOREACH_ALLOCATED_TYPE(DECLARE_STATIC_POOL)

static uint32_t _nodeCounter = 1;


#ifndef ATOMIC
#define INC(v) \
    (v++)
#else
#define INC(v)
    (__builtin_atomic_fetch_add(&v, __ATOMIC_RELEASE))
#endif

/// TODO: lock during realloc
#define REALLOC_BOILERPLATE(PREFIX) \
if (PREFIX ## _capacity <= PREFIX ## _size) \
    { \
        _newMemRealloc( \
            (void**)&  PREFIX ## _mem, \
            &PREFIX## _capacity, \
            sizeof(* PREFIX ## _mem) \
        ); \
    }

/// TODO: lock during realloc
#define REALLOC_N_BOILERPLATE(PREFIX, N) \
if (PREFIX ## _capacity <= (PREFIX ## _size + (N))) \
    { \
        _newMemRealloc( \
            (void**)&  PREFIX ## _mem, \
            &PREFIX## _capacity, \
            sizeof(* PREFIX ## _mem) \
        ); \
    }
metac_expression_t* AllocNewExpression(metac_expression_kind_t kind)
{
    metac_expression_t* result = 0;

    REALLOC_BOILERPLATE(_newExp)

    {
        result = _newExp_mem + INC(_newExp_size);
        result->Kind = kind;
        result->Serial = INC(_nodeCounter);
    }

    return result;
}

uint32_t StructIndex(metac_type_aggregate_t* struct_)
{
    uint32_t result = (struct_ - _newSemaStructs_mem);
    return result;
}

uint32_t UnionIndex(metac_type_aggregate_t* union_)
{
    uint32_t result = (union_ - _newSemaUnions_mem);
    return result;
}


uint32_t FunctionIndex(sema_decl_function_t* func)
{
    uint32_t result = (func - _newSemaFunc_mem);
    return result;
}

uint32_t StatementIndex_(metac_sema_statement_t* stmt)
{
    uint32_t result = (stmt - _newSemaStatements_mem);
    return result;
}

uint32_t TypedefIndex(metac_type_typedef_t* typedef_)
{
    uint32_t result = (typedef_ - _newSemaTypedefs_mem);
    return result;
}

uint32_t PtrTypeIndex(metac_type_ptr_t* ptr)
{
    uint32_t result = (ptr - _newSemaPtrTypes_mem);
    return result;
}

uint32_t FunctiontypeIndex(metac_type_functiontype_t* functiontype)
{
    uint32_t result = (functiontype  - _newSemaFunctiontypes_mem);
    return result;
}

metac_type_aggregate_t* StructPtr(uint32_t index)
{
    metac_type_aggregate_t* result = (_newSemaStructs_mem + index);
    return result;
}

metac_type_aggregate_t* UnionPtr(uint32_t index)
{
    metac_type_aggregate_t* result = (_newSemaUnions_mem + index);
    return result;
}

sema_decl_function_t* FunctionPtr(uint32_t index)
{
    sema_decl_function_t* result = (_newSemaFunc_mem + index);
    return result;
}

metac_sema_statement_t* StatementPtr(uint32_t index)
{
    metac_sema_statement_t* result = (_newSemaStatements_mem + index);
    return result;
}

metac_type_typedef_t* TypedefPtr(uint32_t index)
{
    metac_type_typedef_t* result = (_newSemaTypedefs_mem + index);
    return result;
}

metac_type_ptr_t* PtrTypePtr(uint32_t index)
{
    metac_type_typedef_t* result = (_newSemaPtrTypes_mem + index);
    return result;
}

metac_declaration_t* AllocNewDeclaration_(metac_declaration_kind_t kind, size_t nodeSize, void** result_ptr, uint32_t line)
{
    metac_declaration_t* result = 0;

    REALLOC_BOILERPLATE(_newDecl)

    {
        (*result_ptr) = result = _newDecl_mem + INC(_newDecl_size);
        result->DeclKind = kind;
        result->Serial = INC(_nodeCounter);
        memset(&result->Serial + 1, 0, nodeSize - offsetof(metac_declaration_t, Serial));
    }
    result->AllocLine = line;

    return result;
}

metac_statement_t* AllocNewStatement_(metac_statement_kind_t kind, size_t nodeSize, void** result_ptr)
{
    metac_statement_t* result = 0;

    REALLOC_BOILERPLATE(_newStmt)

    {
        (*result_ptr) = result = _newStmt_mem + INC(_newStmt_size);
        result->StmtKind = kind;
        result->Serial = INC(_nodeCounter);
        result->Next = (metac_statement_t*)EMPTY_POINTER_VALUE;
    }

    return result;
}

metac_sema_expression_t* AllocNewSemaExpression(metac_expression_t* expr)
{
    metac_sema_expression_t* result = 0;

    REALLOC_BOILERPLATE(_newSemaExp)

    {
        result = _newSemaExp_mem + INC(_newSemaExp_size);
        (*(metac_expression_header_t*) result) = (*(metac_expression_header_t*) expr);

        result->TypeIndex.v = 0;
        memcpy(
               ((char*)result) + sizeof(metac_sema_expression_header_t),
               ((char*)expr) + sizeof(metac_expression_header_t),
               sizeof(metac_expression_t) - sizeof(metac_expression_header_t));
        result->Serial = INC(_nodeCounter);
    }

    return result;
}
// ---------------------------------------------- sema -----------------------------

metac_scope_t* AllocNewScope(metac_scope_t* parent, metac_scope_parent_t owner)
{
    metac_scope_t* result;

    REALLOC_BOILERPLATE(_newScopes)

    {
        result = _newScopes_mem + INC(_newScopes_size);
        result->Serial = INC(_nodeCounter);
        result->Owner = owner;
        result->Parent = parent;
    }

    return result;
}


sema_decl_function_t* AllocNewSemaFunction(decl_function_t* func)
{
    sema_decl_function_t* result = 0;

    REALLOC_BOILERPLATE(_newSemaFunc)

    {
        result = _newSemaFunc_mem + INC(_newSemaFunc_size);
        (*(metac_node_header_t*) result) = (*(metac_node_header_t*) func);

        result->Serial = INC(_nodeCounter);
        result->TypeIndex.v = 0;
    }

    return result;
}

#ifndef ATOMIC
#define POST_ADD(v, b) \
    (v += b, v - b)
#else
#define POST_ADD(v, b)
    (__builtin_atomic_fetch_add(&v, b))
#endif

sema_decl_variable_t* AllocNewSemaVariable(decl_variable_t* decl, metac_sema_declaration_t** result_ptr)
{
    sema_decl_variable_t* result = 0;
    REALLOC_BOILERPLATE(_newSemaVariables)

    result = _newSemaVariables_mem + INC(_newSemaVariables_size);
    (*result_ptr) = (metac_sema_declaration_t*)result;

    result->DeclKind = decl_variable;
    result->Serial = INC(_nodeCounter);
    decl->LocationIdx = result->LocationIdx;


    return result;
}

sema_decl_variable_t* AllocFunctionParameters(sema_decl_function_t* func,
                                              uint32_t parameterCount)
{
    sema_decl_variable_t* result = 0;

    REALLOC_N_BOILERPLATE(_newSemaVariables, parameterCount)

    {
        result = _newSemaVariables_mem + POST_ADD(_newSemaVariables_size, parameterCount);
        for(uint32_t i = 0;
            i < parameterCount;
            i++)
        {
            (result + i)->DeclKind = decl_parameter;
            (result + i)->Serial = INC(_nodeCounter);
        }

    }

    return result;
}

metac_type_aggregate_t* AllocNewAggregate(metac_declaration_kind_t kind)
{
    metac_type_aggregate_t* result = 0;

    switch(kind)
    {
        case decl_type_struct:
        {
            REALLOC_BOILERPLATE(_newSemaStructs)
            result = _newSemaStructs_mem + INC(_newSemaStructs_size);
        } break;
        case decl_type_union:
        {
            REALLOC_BOILERPLATE(_newSemaUnions)
            result = _newSemaUnions_mem + INC(_newSemaUnions_size);
        } break;
        case type_class:
        {
            assert(0);
        } break;
        default: assert(0);
    }

    result->Header.Kind = kind;
    //result->Header.LocationIdx = agg->LocationIdx;
    //result->TypeKind = kind;
    result->Header.Serial = INC(_nodeCounter);
    //assert(result->Header.Serial != 120);
    return result;
}

metac_type_typedef_t* AllocNewSemaTypedef(metac_type_index_t typeIndex)
{
    metac_type_typedef_t* result = 0;

    REALLOC_BOILERPLATE(_newSemaTypedefs);
    result = _newSemaTypedefs_mem + INC(_newSemaTypedefs_size);

    result->Header.Kind = decl_type_typedef;
    result->Type = typeIndex;
    result->Header.Serial = INC(_nodeCounter);

    return result;
}

metac_type_typedef_t* AllocNewSemaPtrType(metac_type_index_t elementTypeIndex)
{
    metac_type_typedef_t* result = 0;

    REALLOC_BOILERPLATE(_newSemaTypedefs);
    result = _newSemaTypedefs_mem + INC(_newSemaTypedefs_size);

    result->Header.Kind = decl_type_typedef;
    result->Type = elementTypeIndex;
    result->Header.Serial = INC(_nodeCounter);

    return result;
}


metac_type_functiontype_t* AllocNewSemaFunctionype(metac_type_index_t returnType,
                                                   metac_type_index_t* parameterTypes,
                                                   uint32_t parameterTypeCount)
{
    metac_type_functiontype_t* result = 0;

    REALLOC_BOILERPLATE(_newSemaFunctiontypes);
    result = _newSemaFunctiontypes_mem + INC(_newSemaFunctiontypes_size);

    result->Header.Kind = decl_type_functiontype;
    result->Header.Serial = INC(_nodeCounter);

    result->ReturnType = returnType;
    result->ParameterTypes = parameterTypes;
    result->ParameterTypeCount = parameterTypeCount;


    return result;
}


metac_type_aggregate_field_t* AllocAggregateFields(metac_type_aggregate_t* aggregate,
                                                   metac_type_kind_t kind,
                                                   uint32_t fieldCount)
{
    uint32_t aggregateIndex = 0;
    metac_type_aggregate_field_t* result = 0;
    switch(kind)
    {
        case type_struct:
        {
            REALLOC_BOILERPLATE(_newSemaStructFields)
            result = _newSemaStructFields_mem +
                POST_ADD(_newSemaStructFields_size, fieldCount);
            aggregateIndex = aggregate - _newSemaStructs_mem;
        } break;
        case type_union:
        {
            REALLOC_BOILERPLATE(_newSemaUnionFields)
            result = _newSemaUnionFields_mem +
                POST_ADD(_newSemaUnionFields_size, fieldCount);
            aggregateIndex = aggregate - _newSemaUnions_mem;
        } break;
        case type_class:
        {
            assert(0);
        } break;
    }

    {
        for(uint32_t i = 0;
            i < fieldCount;
            i++)
        {
            (result + i)->Header.Serial = INC(_nodeCounter);
            (result + i)->AggregateIndex = aggregateIndex;
        }

    }

    aggregate->Fields = result;

    return result;
}

metac_sema_statement_t* AllocNewSemaStatement_(metac_statement_kind_t kind,
                                               size_t nodeSize, void** result_ptr)
{
    metac_sema_statement_t* result = 0;

    REALLOC_BOILERPLATE(_newSemaStatements)

    {
        result = _newSemaStatements_mem + INC(_newSemaStatements_size);
        // result->Parent = 0;

        result->Serial = INC(_nodeCounter);
        // result->TypeIndex.v = 0;
    }

    *result_ptr = result;

    return result;
}

sema_stmt_block_t* AllocNewSemaBlockStatement(sema_stmt_block_t* Parent, uint32_t statementCount,
                                              void** result_ptr)
{
    sema_stmt_block_t* result = 0;

    REALLOC_BOILERPLATE(_newSemaBlockStatements)

    {
        uint32_t pointersSize = statementCount * sizeof(sema_stmt_block_t*);
        uint32_t sizeInBlockStatements =
            (pointersSize + sizeof(*_newSemaBlockStatements_mem)) /
            sizeof(*_newSemaBlockStatements_mem);

        result = _newSemaBlockStatements_mem + POST_ADD(_newSemaBlockStatements_size,
                                                        sizeInBlockStatements);
        // result->Parent = 0;

        result->Serial = INC(_nodeCounter);
        result->Body = (metac_sema_statement_t*)(result + 1);
        // result->TypeIndex.v = 0;
    }
    (*result_ptr) = result;

    return result;
}

uint32_t BlockStatementIndex(sema_stmt_block_t* blockstmt)
{
    return blockstmt - _newSemaBlockStatements_mem;
}
#undef offsetof
