#include "metac_alloc_node.h"
#include <string.h>
#include <stdlib.h>

#ifndef ALIGN4
#  define ALIGN4(N) (((N) + 3) & ~3)
#endif

#ifndef EMPTY_POINTER_VALUE
#  define EMPTY_POINTER_VALUE (0x1)
#endif
#define offsetof(st, m) \
    ((size_t)((char *)&((st *)0)->m - (char *)0))

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

static uint32_t _newExp_size = 0;
static uint32_t _newExp_capacity = 0;
static metac_expression_t* _newExp_mem = 0;

static uint32_t _newStmt_size = 0;
static uint32_t _newStmt_capacity = 0;
static metac_statement_t* _newStmt_mem = 0;

static uint32_t _newDecl_size = 0;
static uint32_t _newDecl_capacity = 0;
static metac_declaration_t* _newDecl_mem = 0;

static uint32_t _nodeCounter = 1;


#ifndef ATOMIC
#define INC(v) \
    (v++)
#else
#define INC(v)
    (__builtin_atomic_fetch_add(&v, __ATOMIC_RELEASE))
#endif

metac_expression_t* AllocNewExpression(metac_expression_kind_t kind)
{
    metac_expression_t* result = 0;

    if (_newExp_capacity <= _newExp_size)
    {
        _newMemRealloc(
            (void**)&_newExp_mem,
            &_newExp_capacity,
            sizeof(metac_expression_t)
        );
    }

    {
        result = _newExp_mem + INC(_newExp_size);
        result->Kind = kind;
        result->Serial = INC(_nodeCounter);
    }

    return result;
}


metac_declaration_t* AllocNewDeclaration_(metac_declaration_kind_t kind, size_t nodeSize, void** result_ptr, uint32_t line)
{
    metac_declaration_t* result = 0;

    if (_newDecl_capacity <= _newDecl_size)
    {
        _newMemRealloc(
            (void**)&_newDecl_mem,
            &_newDecl_capacity,
            sizeof(metac_declaration_t)
        );
    }

    {
        (*result_ptr) = result = _newDecl_mem + INC(_newDecl_size);
        result->DeclKind = kind;
        result->Serial = INC(_nodeCounter);
        memset(&result->Serial, 0, nodeSize - offsetof(metac_declaration_t, Serial));
    }

    return result;
}

metac_statement_t* AllocNewStatement_(metac_statement_kind_t kind, size_t nodeSize, void** result_ptr)
{
    metac_statement_t* result = 0;

    if (_newStmt_capacity <= _newStmt_size)
    {
        _newMemRealloc(
            (void**)&_newStmt_mem,
            &_newStmt_capacity,
            sizeof(metac_statement_t)
        );
    }

    {
        (*result_ptr) = result = _newStmt_mem + INC(_newStmt_size);
        result->StmtKind = kind;
        result->Serial = INC(_nodeCounter);
        result->Next = (metac_statement_t*)EMPTY_POINTER_VALUE;
    }

    return result;
}
