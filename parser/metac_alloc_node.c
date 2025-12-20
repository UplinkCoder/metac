#include "../os/metac_atomic.h"
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
#  define offsetof(st, m) \
      ((size_t)((char *)&((st *)0)->m - (char *)0))
#endif

metac_noinline void _newMemRealloc(void** memP, uint32_t* capacityP, const uint32_t elementSize)
{
    uint32_t capacity;
    if (!*memP)
    {
        capacity = cast(int)((8192 * 12) / 1.6f);
    }
    else
    {
        capacity = *capacityP;
    }

    {
        capacity = ALIGN4(cast(uint32_t) ((capacity - 1) * 1.6f));
        ALIGN_STACK();
        *memP = realloc(*memP, ((capacity) * elementSize));
        RESTORE_STACK();
    }

    *capacityP = capacity;
}


#define FOREACH_ALLOCATED_PARSER_TYPE(M) \
    M(metac_expr_t, _newExp) \
    M(metac_decl_t, _newDecl) \
    M(metac_stmt_t, _newStmt)

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
static metac_expr_t* _newExp_mem = 0;
*/

#define DECLARE_STATIC_ARRAY(TYPE_NAME, PREFIX) \
    static uint32_t PREFIX ##_size = 0; \
    static uint32_t PREFIX ##_capacity = 0; \
    static TYPE_NAME* PREFIX ##_mem = (TYPE_NAME*)0;
/*
    DEF_FREELIST_T(TYPE_NAME, PREFIX) \
    static FREELIST(PREFIX)* PREFIX ##_freelist = (FREELIST(PREFIX)*)0;
*/
FOREACH_ALLOCATED_PARSER_TYPE(DECLARE_STATIC_ARRAY)

static uint32_t _nodeCounter = 1;


/// TODO: lock during realloc
#define REALLOC_BOILERPLATE(PREFIX) \
if (PREFIX ## _capacity <= PREFIX ## _size) \
    { \
    /*printf("[%s]Preforming realloc from:%u\n", __FUNCTION__, PREFIX ##_capacity);*/ \
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
metac_expr_t* AllocNewExpr(metac_expr_kind_t kind)
{
    metac_expr_t* result = 0;

    REALLOC_BOILERPLATE(_newExp)

    {
        result = _newExp_mem + INC(_newExp_size);
        result->Kind = kind;
        result->Serial = INC(_nodeCounter);
        memset(&result->Serial + 1, 0, sizeof(metac_expr_t) - offsetof(metac_expr_t, Serial));
    }

    return result;
}

metac_decl_t* AllocNewDecl_(metac_decl_kind_t kind, uint32_t nodeSize, void** result_ptr, uint32_t line)
{
    metac_decl_t* result = 0;

    REALLOC_BOILERPLATE(_newDecl)

    {
        (*result_ptr) = result = _newDecl_mem + INC(_newDecl_size);
        result->Kind = kind;
        result->Serial = INC(_nodeCounter);
        memset(&result->Serial + 1, 0, nodeSize - offsetof(metac_decl_t, Serial));
    }

    // result->AllocLine = line;

    return result;
}

metac_stmt_t* AllocNewStmt_(metac_stmt_kind_t kind, uint32_t nodeSize, void** result_ptr)
{
    metac_stmt_t* result = 0;

    REALLOC_BOILERPLATE(_newStmt)

    {
        (*result_ptr) = result = _newStmt_mem + INC(_newStmt_size);
        result->Kind = kind;
        result->Serial = INC(_nodeCounter);
        result->Next = (metac_stmt_t*)EMPTY_POINTER_VALUE;
    }

    return result;
}

#undef offsetof
#undef REALLOC_BOILERPLATE
#undef REALLOC_N_BOILERPLATE
