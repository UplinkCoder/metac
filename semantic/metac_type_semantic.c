#include "metac_type_semantic.h"
#include "metac_expr_semantic.h"
#include "../os/metac_alloc.h"

#ifndef NO_FIBERS
#  include "../os/metac_task.h"
#endif

#ifndef INVALID_SIZE
#  define INVALID_SIZE \
      ((uint32_t)-1)
#endif

#ifndef U32
#  define U32(VAR) \
      (*(uint32_t*)(&VAR))
#endif

#ifndef _emptyPointer
#  define _emptyPointer (void*)0x1
#endif

#ifndef emptyNode
#  define emptyNode (metac_node_t) _emptyPointer
#endif

#define fatalf(format, ...) do { \
    xprintf(format, __VA_ARGS__); \
    assert(0); \
} while (0)


static const metac_type_index_t zeroIdx = {0};
metac_type_index_t MetaCSemantic_GetPtrTypeOf(metac_sema_state_t* self,
                                              metac_type_index_t elementTypeIndex)
{
    uint32_t hash = elementTypeIndex.v;
    metac_type_ptr_t key =
            {{decl_type_typedef, 0, hash, 0},
             zeroIdx, elementTypeIndex};

    metac_type_index_t result =
        MetaCTypeTable_GetOrEmptyPtrType(&self->PtrTypeTable, &key);
    if (result.v == 0)
    {
        result = MetaCTypeTable_AddPtrType(&self->PtrTypeTable, &key);
    }
    return result;
}

static inline bool isBasicType(metac_type_kind_t typeKind)
{
    if ((typeKind >= type_void) & (typeKind <= type_unsigned_long_long))
    {
        return true;
    } else if (typeKind == type_type)
    {
        return true;
    } else
    return false;
}

bool IsUnknownType(metac_sema_state_t* self,
                   metac_type_index_t typeIndex)
{
    bool result = false;

    if (TYPE_INDEX_KIND(typeIndex) == type_index_unknown)
    {
        result = true;
    }
    if (TYPE_INDEX_KIND(typeIndex) == type_index_array)
    {
        result = IsUnknownType(self, self->ArrayTypeTable.Slots[TYPE_INDEX_INDEX(typeIndex)].TypeIndex);
    }

    return result;
}

sema_decl_type_t* MetaCSemantic_GetTypeNode(metac_sema_state_t* self,
                                            metac_type_index_t typeIndex)
{
    return cast(sema_decl_type_t*) NodeFromTypeIndex(self, typeIndex);
}

metac_type_index_t MetaCSemantic_GetTypeIndex(metac_sema_state_t* state,
                                              metac_type_kind_t typeKind,
                                              decl_type_t* type)
{
    metac_type_index_t result = {0};

    if (isBasicType(typeKind))
    {
        result.v = TYPE_INDEX_V(type_index_basic, (uint32_t) typeKind);

        assert((type == emptyPointer) || type->TypeKind == typeKind);
        if ((type != emptyPointer) && (type->TypeModifiers & typemod_unsigned))
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
                //TODO Real error macro
                fprintf(stderr, "modifier unsigned cannot be applied to: %s\n", TypeToChars(state, result));
            }
        }
    }
    else if (typeKind == type_auto)
    {
        result.v = TYPE_INDEX_V(type_index_basic, type_auto);
    }
    return result;
}

static inline bool IsAggregateTypeDecl(metac_decl_kind_t declKind)
{
    if (declKind == decl_type_struct || declKind == decl_type_union)
    {
        return true;
    }
    return false;
}

static inline bool IsPointerType(metac_decl_kind_t declKind)
{
    if (declKind == decl_type_ptr)
    {
        return true;
    }
    return false;
}


static inline const char* BasicTypeToChars(metac_type_index_t typeIndex)
{
    assert(TYPE_INDEX_KIND(typeIndex) == type_index_basic);
    switch((metac_type_kind_t) TYPE_INDEX_INDEX(typeIndex))
    {
        case type_invalid :
            assert(0);

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
        case type_size_t:
            return "size_t";
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

        default: assert(0);
    }
    return 0;
}

static inline metac_type_index_t CommonBasicType(const metac_type_index_t a,
                                                 const metac_type_index_t b)
{
    metac_type_index_t result;
    result.v = TYPE_INDEX_V(type_index_basic, basic_int);

    if (a.v == TYPE_INDEX_V(type_index_basic, basic_long_double) ||
        b.v == TYPE_INDEX_V(type_index_basic, basic_long_double))
    {
        result.v = TYPE_INDEX_V(type_index_basic, basic_long_double);
    } else if (a.v == TYPE_INDEX_V(type_index_basic, basic_double) ||
               b.v == TYPE_INDEX_V(type_index_basic, basic_double))
    {
        result.v = TYPE_INDEX_V(type_index_basic, basic_double);
    } else if (a.v == TYPE_INDEX_V(type_index_basic, basic_float) ||
               b.v == TYPE_INDEX_V(type_index_basic, basic_float))
    {
        result.v = TYPE_INDEX_V(type_index_basic, basic_float);
    } else if (a.v == TYPE_INDEX_V(type_index_basic, type_unsigned_long_long) ||
               b.v == TYPE_INDEX_V(type_index_basic, type_unsigned_long_long))
    {
        result.v = TYPE_INDEX_V(type_index_basic, type_unsigned_long_long);
    } else if (a.v == TYPE_INDEX_V(type_index_basic, type_long_long) ||
               b.v == TYPE_INDEX_V(type_index_basic, type_long_long))
    {
        result.v = TYPE_INDEX_V(type_index_basic, type_long_long);
    } else if (a.v == TYPE_INDEX_V(type_index_basic, type_unsigned_long) ||
               b.v == TYPE_INDEX_V(type_index_basic, type_unsigned_long))
    {
        result.v = TYPE_INDEX_V(type_index_basic, type_unsigned_long);
    } else if (a.v == TYPE_INDEX_V(type_index_basic, type_long) ||
               b.v == TYPE_INDEX_V(type_index_basic, type_long))
    {
        result.v = TYPE_INDEX_V(type_index_basic, type_long);
    } else if (a.v == TYPE_INDEX_V(type_index_basic, type_unsigned_int) ||
               b.v == TYPE_INDEX_V(type_index_basic, type_unsigned_int))
    {
        result.v = TYPE_INDEX_V(type_index_basic, type_unsigned_int);
    }

    return result;
}

static inline bool TypeConvertsToPointer(const metac_type_index_t a)
{
    bool result;

    switch(TYPE_INDEX_KIND(a))
    {
        case type_index_functiontype:
        // case type_index_struct: // why was that here ???
        case type_index_array:
        case type_index_ptr:
            result = true;
        break;
        case type_index_basic:
        {
            basic_type_kind_t basicKind = (basic_type_kind_t)
                TYPE_INDEX_INDEX(a);
            result = (basicKind == basic_int);
        }
        default :
            result = false;
    }

    return result;
}


bool MetaCSemantic_TypeConvertsTo(metac_sema_state_t* self,
                                  const metac_type_index_t a,
                                  const metac_type_index_t b)
{
    if (a.v == b.v)
    {
        return true;
    }
    else
    {
        //TODO this is not nearly complete.
        return false;
    }
}
///TODO FIXME
/// this is not nearly complete!
metac_type_index_t MetaCSemantic_CommonSubtype(metac_sema_state_t* self,
                                               const metac_type_index_t a,
                                               const metac_type_index_t b)
{
    metac_type_index_t result = {0};

    if (a.v == b.v)
        result = a;
    else
    {
        if (TYPE_INDEX_KIND(a) == TYPE_INDEX_KIND(b))
        {
            switch(TYPE_INDEX_KIND(a))
            {
                case type_index_basic:
                    result = CommonBasicType(a, b);
                break;
                case type_index_functiontype:
                    result.v = TYPE_INDEX_V(type_index_functiontype, 0);
                break;
                default:
                    assert(!"CommonType not implemented for this type_kind");
            }
        }
        else if (TypeConvertsToPointer(a) && TypeConvertsToPointer(b))
        {
            result.v = TYPE_INDEX_V(type_index_ptr, 0);
        }
    }

    assert(result.v != -1);
    return result;
}

uint32_t FieldHash(metac_type_aggregate_field_t* field)
{
    uint32_t hash = ~0;
    hash = CRC32C_VALUE(hash, field->Type);
    hash = CRC32C_VALUE(hash, field->Offset);
//    assert(!field->Header.Hash || field->Header.Hash == hash);
//   TODO this assert should be here and it should pass
    return hash;
}

uint32_t AggregateHash(metac_type_aggregate_t* agg)
{
#ifdef NDEBUG
    if (agg->Header.Hash)
        return agg->Header.Hash;
#endif
    uint32_t hash = ~0;
    for(int i = 0; i < agg->FieldCount; i++)
    {
        metac_type_aggregate_field_t* field = agg->Fields + i;
        // ideally we would want to make sure that field->Header.Hash is 0
        // if it's not set to a vaild value so we can cache it
        field->Header.Hash = FieldHash(field);
        hash = CRC32C_VALUE(hash, field->Header.Hash);
    }

    return hash;
}
static inline uint32_t Align(uint32_t size, uint32_t alignment)
{
    assert(alignment >= 1);
    uint32_t alignmentMinusOne = (alignment - 1);
    uint32_t alignSize = (size + alignmentMinusOne);
    uint32_t alignMask = ~(alignmentMinusOne);
    return (alignSize & alignMask);
}

metac_type_index_t MetaCSemantic_GetElementType(metac_sema_state_t* self,
                                                metac_type_index_t typeIndex)
{
    metac_type_index_t result = {0};

    switch(TYPE_INDEX_KIND(typeIndex))
    {
        case type_index_array:
        {
            metac_type_array_t* arrayType = ArrayTypePtr(self, TYPE_INDEX_INDEX(typeIndex));
            result = arrayType->ElementType;
        } break;
        case type_index_ptr:
        {
            metac_type_ptr_t* ptrType = PtrTypePtr(self, TYPE_INDEX_INDEX(typeIndex));
            result = ptrType->ElementType;
        } break;
        default: break;
    }

    return result;
}

uint32_t MetaCSemantic_GetTypeAlignment(metac_sema_state_t* self,
                                        metac_type_index_t typeIndex)
{
    uint32_t result = INVALID_SIZE;
    metac_type_index_kind_t typeIndexKind = TYPE_INDEX_KIND(typeIndex);

    if (typeIndexKind == type_index_basic)
    {
        uint32_t idx = TYPE_INDEX_INDEX(typeIndex);

        if ((idx >= type_unsigned_char)
         && (idx <= type_unsigned_long))
        {
            idx -= ((uint32_t)type_unsigned_char - (uint32_t)type_char);
        }
        else if (idx == type_unsigned_long_long)
        {
            idx = type_long_long;
        }

        if (idx != type_type)
        {
            result =
                MetaCTargetInfo_GetBasicAlign(&default_target_info, (basic_type_kind_t) idx);
        }
        else
        {
            result = 4;
        }
    }
    else if (typeIndexKind == type_index_ptr
        ||   typeIndexKind == type_index_functiontype)
    {
        result = default_target_info.AlignmentSizeT;
    }
    else if (typeIndexKind == type_index_typedef)
    {
        uint32_t idx = TYPE_INDEX_INDEX(typeIndex);
        metac_type_index_t elementTypeIndex =
            self->TypedefTypeTable.Slots[idx].Type;

        result = MetaCSemantic_GetTypeAlignment(self, elementTypeIndex);
    }
    else if (typeIndexKind == type_index_struct)
    {
        metac_type_aggregate_t* struct_ = StructPtr(self, TYPE_INDEX_INDEX(typeIndex));
        result = struct_->Alignment;
    }
    else if (typeIndexKind == type_index_enum)
    {
        metac_type_enum_t* enum_ = EnumTypePtr(self, TYPE_INDEX_INDEX(typeIndex));
        //TODO use an enum basetype
        result = MetaCTargetInfo_GetBasicAlign(&default_target_info, basic_int);
    }
    else if (typeIndexKind == type_index_array)
    {
        uint32_t idx = TYPE_INDEX_INDEX(typeIndex);
        metac_type_array_t* arrayType_ = ArrayTypePtr(self, idx);
        metac_type_index_t elementTypeIndex = arrayType_->ElementType;
        result = MetaCSemantic_GetTypeAlignment(self, elementTypeIndex);
    }
    else if (typeIndexKind == type_index_tuple)
    {
        uint32_t idx = TYPE_INDEX_INDEX(typeIndex);
        metac_type_tuple_t* tupleType_ = TupleTypePtr(self, idx);
        uint32_t maxAlign = 0;
        for(uint32_t i = 0; i < tupleType_->TypeCount; i++)
        {
            metac_type_index_t mType = tupleType_->TypeIndicies[i];
            uint32_t align = MetaCSemantic_GetTypeAlignment(self, mType);
            //TODO we might not want to assume that we will always be able
            // to get the alignment and we should suspend here ...
            // however for now just assert.
            assert(align != INVALID_SIZE);

            if (align > maxAlign)
            {
                maxAlign = align;
            }
        }
        result = maxAlign;
    }
    else
    {
        assert(0);
    }

    return result;
}

/// Returns size in byte or INVALID_SIZE on error
uint32_t MetaCSemantic_GetTypeSize(metac_sema_state_t* self,
                                   metac_type_index_t typeIndex)
{
    uint32_t result = INVALID_SIZE;

    if (TYPE_INDEX_KIND(typeIndex) == type_index_basic)
    {
        uint32_t idx = TYPE_INDEX_INDEX(typeIndex);
        if (idx == type_type)
        {
            return sizeof(metac_type_index_t);
        }
        if ((idx >= type_unsigned_char)
         && (idx <= type_unsigned_long))
        {
            idx -= ((uint32_t)type_unsigned_char - (uint32_t)type_char);
        }
        else if (idx == type_unsigned_long_long)
        {
            idx = type_long_long;
        }
        if (idx != type_type)
        {
            result =
                MetaCTargetInfo_GetBasicSize(&default_target_info, (basic_type_kind_t) idx);
        }
        else
        {
            result = 4;
        }
    }
    else if (TYPE_INDEX_KIND(typeIndex) == type_index_ptr
        ||   TYPE_INDEX_KIND(typeIndex) == type_index_functiontype)
    {
        result = default_target_info.PtrSize;
    }
    else if (TYPE_INDEX_KIND(typeIndex) == type_index_typedef)
    {
        uint32_t idx = TYPE_INDEX_INDEX(typeIndex);
        metac_type_index_t elementTypeIndex =
            self->TypedefTypeTable.Slots[idx].Type;

        result = MetaCSemantic_GetTypeSize(self,
                                           elementTypeIndex);
    }
    else if (TYPE_INDEX_KIND(typeIndex) == type_index_struct)
    {
        metac_type_aggregate_t* struct_ = StructPtr(self, TYPE_INDEX_INDEX(typeIndex));
        result = struct_->Size;
    }
    else if (TYPE_INDEX_KIND(typeIndex) == type_index_enum)
    {
        metac_type_enum_t* enum_ = EnumTypePtr(self, TYPE_INDEX_INDEX(typeIndex));
        //TODO use an enum basetype
        result = MetaCTargetInfo_GetBasicSize(&default_target_info, basic_int);
    }
    else if (TYPE_INDEX_KIND(typeIndex) == type_index_array)
    {
        uint32_t idx = TYPE_INDEX_INDEX(typeIndex);
        metac_type_array_t* arrayType = ArrayTypePtr(self, idx);
        metac_type_index_t elementTypeIndex = arrayType->ElementType;

        uint32_t baseSize = MetaCSemantic_GetTypeSize(self,
                                                      elementTypeIndex);
        uint32_t alignedSize = Align(baseSize,
                                     MetaCSemantic_GetTypeAlignment(self,
                                                                    elementTypeIndex));
        result = alignedSize * arrayType->Dim;
    }
    else if (TYPE_INDEX_KIND(typeIndex) == type_index_tuple)
    {
        metac_type_tuple_t* tuple = TupleTypePtr(self, TYPE_INDEX_INDEX(typeIndex));
        metac_size_computer_t sizeComputer;
        MetaCSizeComputer_Init(&sizeComputer);
        MetaCSizeComputer_BeginSizeOf(&sizeComputer);
        {
            uint32_t i;
            for(i = 0; i < tuple->TypeCount; i++)
            {
                MetaCSizeComputer_MemberType(&sizeComputer, self, tuple->TypeIndicies[i]);
            }
        }
        result = MetaCSizeComputer_FinishSizeOf(&sizeComputer);
    }
    else
    {
        assert(0);
    }

    return result;
}

typedef struct scope_and_fields_t
{
    metac_scope_t* Scope;
    metac_type_aggregate_field_t* Fields;
    uint32_t FieldCount;
} scope_and_fields_t;

scope_and_fields_t AllocateAggregateScopeAndFields(metac_alloc_t* alloc, uint32_t nFields)
{

    uint32_t nSlots = nFields + ((nFields < 16) ? 4 : (nFields >> 2));

    const uint32_t scopeTableSize =
        ALIGN16(NEXTPOW2(nSlots) * sizeof(metac_scope_table_slot_t))
        + ALIGN16(sizeof(metac_scope_t));

    const uint32_t aggregateMemorySize =
        ALIGN16(nFields * sizeof(metac_type_aggregate_field_t))
        + scopeTableSize;

    arena_ptr_t arenaPtr = AllocateArena(alloc, aggregateMemorySize);
    tagged_arena_t* aggregateArena = &alloc->Arenas[arenaPtr.Index];
    metac_type_aggregate_field_t* aggFields = cast(metac_type_aggregate_field_t*)
        aggregateArena->Memory;

    metac_scope_t* scope_ = cast(metac_scope_t*) (aggFields + nFields);
    metac_scope_table_slot_t* slots = (metac_scope_table_slot_t*) (scope_ + 1);
    scope_and_fields_t result = {scope_, aggFields, nFields};

    memset(scope_, 0, scopeTableSize);
    memset(aggFields, 0, nFields * sizeof(metac_type_aggregate_field_t));

    scope_->ScopeTable.Slots = slots;
    scope_->ScopeTable.SlotCount_Log2 = LOG2(nSlots);

    return result;
}

void PopulateTemporaryAggregateScope(metac_sema_state_t* self,
                                     metac_type_aggregate_t* tmpSemaAgg,
                                     decl_type_struct_t* agg)
{
    uint32_t nFields = agg->FieldCount;
    scope_and_fields_t scopeAndFields =
        AllocateAggregateScopeAndFields(&self->TempAlloc, nFields);

    metac_type_aggregate_field_t* fields = scopeAndFields.Fields;

    // ??? do we want to the the parent like this?
    // scopeAndFields.Scope->Parent = self->CurrentScope;
    // or do we want the aggregate scope to exist in isolation ...
    // I think we want it in isolation.

    tmpSemaAgg->Scope = scopeAndFields.Scope;
    tmpSemaAgg->Fields = fields;
    tmpSemaAgg->FieldCount = nFields;
    decl_field_t* aggField = agg->Fields;

    MetaCSemantic_MountScope(self, tmpSemaAgg->Scope);

    for(uint32_t i = 0; i < nFields; i++)
    {
        metac_type_aggregate_field_t* semaField = &fields[i];
        metac_identifier_ptr_t fieldName = aggField->Field->VarIdentifier;
        // ??? should we Register this identifier in the semanticIdentifierTable
        // and using a pointer to that in the sema node?

        uint32_t declNodePtr = 42;
        semaField->Header.Kind = node_decl_field;

        semaField->Identifier = fieldName;
        // placeholder while we don't have hash-table for decl_node pointers
        semaField->Index = cast(uint16_t) i;
        semaField->Type.v = TYPE_INDEX_V(type_index_unknown, declNodePtr);
        //metac_sema_identifier_t
        //    semaId = MetaCSemantic_RegisterIdentifier(agg->Fields[i].Field.VarIdentifier);
        scope_insert_error_t inserted =
            MetaCSemantic_RegisterInScope(self, fieldName,
                                          cast(metac_node_t)semaField);
        if (inserted != success)
        {
            SemanticError(0, "%s could not be inserted",
                IdentifierPtrToCharPtr(self->ParserIdentifierTable, fieldName));
        }

        aggField = aggField->Next;
    }

    MetaCSemantic_UnmountScope(self);
}

/// this is where we also populate the scope
metac_type_aggregate_t* MetaCSemantic_PersistTemporaryAggregateAndPopulateScope(metac_sema_state_t* self,
                                                                                metac_type_aggregate_t* tmpAgg)
{
    uint32_t nFields = tmpAgg->FieldCount;
    scope_and_fields_t scopeAndFields = AllocateAggregateScopeAndFields(&self->Allocator, nFields);

    metac_type_aggregate_field_t* aggFields = scopeAndFields.Fields;
    metac_scope_t* scope_ = scopeAndFields.Scope;

    metac_type_index_t typeIndex;
    metac_type_aggregate_t* semaAgg = 0;

    // Determine the type of the aggregate and set the appropriate owner in the scope
    if (tmpAgg->Header.Kind == decl_type_struct)
    {
        typeIndex = MetaCTypeTable_AddStructType(&self->StructTypeTable, tmpAgg);
        semaAgg = StructPtr(self, TYPE_INDEX_INDEX(typeIndex));
        scope_->Owner.v = SCOPE_OWNER_V(scope_owner_struct, StructIndex(self, semaAgg));
    }
    else if (tmpAgg->Header.Kind == decl_type_union)
    {
        typeIndex = MetaCTypeTable_AddUnionType(&self->UnionTypeTable, tmpAgg);
        semaAgg = UnionPtr(self, TYPE_INDEX_INDEX(typeIndex));
        scope_->Owner.v = SCOPE_OWNER_V(scope_owner_union, UnionIndex(self, semaAgg));
    }
    else
    {
        assert(!"aggregate type not supported or it's not an aggregate type");
    }

    scope_->Parent = self->CurrentScope;

    // Mount the scope to register fields
    MetaCSemantic_MountScope(self, scope_);

    // Initialize the printer for semantic nodes
/*
    metac_printer_t printer;
    MetaCPrinter_Init(&printer,
                      self->ParserIdentifierTable, self->ParserStringTable,
                      &self->Allocator);
I
 * */
    semaAgg->Scope = scope_;

    // Copy fields from the temporary aggregate to the new persistent memory location and register them in the scope
    for (uint32_t i = 0; i < nFields; i++)
    {
        aggFields[i] = tmpAgg->Fields[i];
        //MetaCPrinter_PrintSemaNode(&printer, self, cast(metac_node_t) &aggFields[i]);

        scope_insert_error_t inserted =
            MetaCSemantic_RegisterInScope(self, aggFields[i].Identifier, cast(metac_node_t)&aggFields[i]);
        if (inserted != success)
        {
            SemanticError(0, "%s could not be inserted",
                IdentifierPtrToCharPtr(self->ParserIdentifierTable, aggFields[i].Identifier));
        }
    }

    // Unmount the scope after all fields are registered
    MetaCSemantic_UnmountScope(self);

    semaAgg->Fields = aggFields;
    // XXX there was a comment here to walk the fields
    // suggesting that there might be more work to do here.

    return semaAgg;
}

#define BeginTaskBarrier()
#define EndTaskBarrier()

void MetaCSemantic_ComputeEnumValues(metac_sema_state_t* self,
                                     decl_type_enum_t* enum_,
                                     metac_type_enum_t* semaEnum)
{
    decl_enum_member_t* member = enum_->Members;
    int64_t nextValue = 0;
    const uint32_t memberCount = enum_->MemberCount;
#if !DEBUG_MEMORY
    STACK_ARENA_ARRAY(metac_sema_expr_t, memberPlaceholders, 32, &self->TempAlloc)
#else
    metac_sema_expr_t* memberPlaceholders = (metac_sema_state_t*)
        calloc(memberCount, sizeof(metac_sema_expr_t));
    uint32_t memberPlaceholdersCount = 0;
#endif
    metac_printer_t debugPrinter;

    uint32_t hash = enum_key;
    hash = CRC32C_VALUE(hash, enum_->Identifier.v);
    assert(SCOPE_OWNER_KIND(self->CurrentScope->Owner) == scope_owner_enum);
#if !DEBUG_MEMORY
    ARENA_ARRAY_ENSURE_SIZE(memberPlaceholders, memberCount);
#endif
    MetaCPrinter_Init(&debugPrinter,
        self->ParserIdentifierTable, self->ParserStringTable, 0);

    memberPlaceholdersCount = memberCount;
#if DEBUG_MEMORY
    memset(memberPlaceholders, 0, memberCount * sizeof(*memberPlaceholders));
#endif

    //TODO you want to make CurrentValue a metac_sema_expr_t
    // such that you can interpret the increment operator
    // MetaCSemantic_SetInProgress(self, semaEnum, "Members");

    semaEnum->Identifier = MetaCIdentifierTable_CopyIdentifier(self->ParserIdentifierTable, &self->SemanticIdentifierTable, enum_->Identifier);
    semaEnum->Header.Kind = decl_type_enum;
    if (METAC_NODE(enum_->BaseType) == emptyNode)
    {
        semaEnum->BaseType.v = TYPE_INDEX_V(type_index_basic, type_int);
    }
    else
    {
        semaEnum->BaseType = MetaCSemantic_TypeSemantic(self, enum_->BaseType);
    }
    // first register dummy members
    // such that we can type forward reference expressions
    {
        member = enum_->Members;
        for(uint32_t memberIdx = 0;
            memberIdx < memberCount;
            memberIdx++, member = member->Next)
        {
            metac_sema_expr_t* placeHolder = memberPlaceholders + memberIdx;

            placeHolder->Kind = expr_unknown_value;
            placeHolder->TypeIndex = semaEnum->BaseType;
            placeHolder->Expr = member->Value;

            MetaCSemantic_RegisterInScope(self, member->Name,
                                          METAC_NODE(placeHolder));
        }
    }
/*
    macro ScopeTable_AllowOverrideDo(metac_scope_table_t* self, __code code)
    {
        bool oldAllowOverride = self->CurrentScope->ScopeTable.AllowOverride;
        self->AllowOverride = true;
        inject code;
        self->AllowOverride = oldAllowOverride;
    }
 */   
    // Set the currentScope to be an override scope
    // Since we are inserting members
    bool oldAllowOverride = self->CurrentScope->ScopeTable.AllowOverride;
    self->CurrentScope->ScopeTable.AllowOverride = true;
    {
        uint32_t lastUnresolvedMembers = 0;
        uint32_t unresolvedMembers = 0;

        const char* eName = IdentifierPtrToCharPtr(self->ParserIdentifierTable, enum_->Identifier);
        do
        {
            MetaCSemantic_PushOnResolveFail(self, OnResolveFail_ReturnNull);
            lastUnresolvedMembers = unresolvedMembers;

            member = enum_->Members;
            for(uint32_t memberIdx = 0;
                memberIdx < memberCount;
                memberIdx++, member = member->Next)
            {
                const char* mName = IdentifierPtrToCharPtr(self->ParserIdentifierTable, member->Name);

                if (METAC_NODE(member->Value) != emptyNode)
                {
                    metac_sema_expr_t* semaValue =
                        MetaCSemantic_doExprSemantic(self, member->Value, 0);
                    semaEnum->Members[memberIdx].Value = semaValue;

                    semaEnum->Members[memberIdx].Header.Kind = decl_enum_member;

                    if (IsUnresolved(METAC_NODE(semaValue)))
                    {
                        unresolvedMembers++;
                    }
                    else
                    {
                        metac_enum_member_t* semaMember = semaEnum->Members + memberIdx;
                        MetaCSemantic_RegisterInScope(self, member->Name, METAC_NODE(semaMember));
                    }
                }
            }
            assert(METAC_NODE(member) == emptyNode);
            MetaCSemantic_PopOnResolveFail(self);

            if (unresolvedMembers == lastUnresolvedMembers)
            {
                // we have a the same number of unresolved members as last round
                // which indicates a lack of local progress.
                YIELD("Yielding because we are waiting for more enum members to become resolvable");
            }
        } while (unresolvedMembers);
#if DEBUG_MEMORY
        free(memberPlaceholders);
#endif
        member = enum_->Members;
        for(uint32_t memberIdx = 0;
            memberIdx < memberCount;
            memberIdx++, member = member->Next)
        {

            semaEnum->Members[memberIdx].Identifier = member->Name;
            semaEnum->Members[memberIdx].Header.Kind = decl_enum_member;

            if (METAC_NODE(member->Value) != emptyNode)
            {
                metac_sema_expr_t semaValue =
                    *semaEnum->Members[memberIdx].Value;

                if (semaValue.Kind != expr_signed_integer)
                {
                    semaValue.TypeIndex.v = semaEnum->BaseType.v;
                    semaValue = EvaluateExpr(self, &semaValue, 0);
                }
                if (semaValue.Kind != expr_signed_integer)
                {
                    fatalf("Value of %s could not be constant folded\n", IdentifierPtrToCharPtr(self->ParserIdentifierTable, member->Name));
                }
                //assert(semaValue->Kind == expr_signed_integer);
                nextValue = semaValue.ValueI64 + 1;
            }
            else
            {
                // let's construct a metac_expression from currentValue
                metac_expr_t Value = {expr_signed_integer, member->LocationIdx, 0, 0};
                Value.ValueI64 = nextValue++;

                semaEnum->Members[memberIdx].Value =
                    MetaCSemantic_doExprSemantic(self, &Value, 0);

            }

            semaEnum->Members[memberIdx].Header.LocationIdx = member->LocationIdx;
            hash = CRC32C_VALUE(hash, semaEnum->Members[memberIdx].Identifier);
            hash = CRC32C_VALUE(hash, semaEnum->Members[memberIdx].Value->ValueI64);

            MetaCSemantic_RegisterInScope(self,
                                          semaEnum->Members[memberIdx].Identifier,
                                          METAC_NODE(semaEnum->Members[memberIdx].Value));
        }
    }
    self->CurrentScope->ScopeTable.AllowOverride = oldAllowOverride;
    semaEnum->Header.Hash = hash;

    return ;
}

static bool TypeIsInteger(metac_type_index_t typeIdx)
{
    bool result = false;

    uint32_t typeIdxIndex = TYPE_INDEX_INDEX(typeIdx);

    if (TYPE_INDEX_KIND(typeIdx) == type_index_basic)
    {
        result = ((typeIdxIndex >= type_bool) & (typeIdxIndex <= type_size_t)) |
                 ((typeIdxIndex >= type_unsigned_char) & (typeIdxIndex <= type_unsigned_long)) |
                 ((typeIdxIndex == type_long_long) | (typeIdxIndex == type_unsigned_long_long));
    }

    return result;
}

static metac_type_index_t TypeTupleSemantic(metac_sema_state_t* self,
                                            decl_type_t* type_)
{
    metac_type_index_t result = {0};

    uint32_t i;
#define tuple_key 0x55ee11
    uint32_t hash = tuple_key;
    decl_type_tuple_t* tupleType = cast(decl_type_tuple_t*) type_;

    STACK_ARENA_ARRAY(metac_type_index_t, typeIndicies, 16, &self->Allocator)

    for(i = 0; i < tupleType->TypeCount; i++)
    {
        metac_type_index_t typ =
                 MetaCSemantic_TypeSemantic(self, tupleType->Types[i]);
        ARENA_ARRAY_ADD(typeIndicies, typ);
        hash = CRC32C_VALUE(hash, typ);
    }

    metac_type_header_t header =
        {decl_type_tuple, 0, hash};

    metac_type_tuple_t key = {
        header,
        zeroIdx,
        typeIndicies,
        typeIndiciesCount
    };

    result =
        MetaCTypeTable_GetOrEmptyTupleType(&self->TupleTypeTable, &key);

    if (result.v == 0)
    {
        STACK_ARENA_ARRAY_TO_HEAP(typeIndicies, &self->Allocator);
        key.TypeIndicies = typeIndicies;
        result = MetaCTypeTable_AddTupleType(&self->TupleTypeTable, &key);
    }

        // metac_type_tuple_t* semaTypeTuple = TupleTypePtr(self, TYPE_INDEX_INDEX(result));
    return result;
}
metac_type_index_t TypeArraySemantic(metac_sema_state_t* self,
                                     decl_type_t* type_)
{
    metac_type_index_t result = {0};

       decl_type_array_t* arrayType =
        cast(decl_type_array_t*)type_;
    metac_type_index_t elementType =
        MetaCSemantic_doTypeSemantic(self, arrayType->ElementType);
    // it could be an array win inferred dimensions in which case
    // arrayType->Dim is the empty pointer

    metac_sema_expr_t* dim = (METAC_NODE(arrayType->Dim) == emptyNode ?
        cast(metac_sema_expr_t*)emptyNode :
        MetaCSemantic_doExprSemantic(self, arrayType->Dim, 0)
    );

    if (METAC_NODE(dim) != emptyNode)
    {
        if (dim->Kind != expr_signed_integer)
        {
            xprintf("Array dimension should eval to integer but it is: %s\n",
                MetaCExprKind_toChars(dim->Kind));
            //TODO register this decl_type_t as an error so we do not reevaluate
            static const metac_type_index_t invalidTypeIndex = {0};
            return invalidTypeIndex;
        }
    }
    uint32_t dimValue = (
        METAC_NODE(dim) != emptyNode ?
            (uint32_t)dim->ValueU64 : -1);

    result =
        MetaCSemantic_GetArrayTypeOf(self, elementType, dimValue);

    return result;
}

metac_type_index_t TypeEnumSemantic(metac_sema_state_t* self,
                                    decl_type_t* type_)
{
    metac_type_index_t result = {0};

    decl_type_enum_t* enm = cast(decl_type_enum_t*) type_;

    metac_type_enum_t tmpSemaEnum = {(metac_decl_kind_t)0};

    metac_scope_t enumScope = { scope_flag_temporary };
    STACK_ARENA_ARRAY(metac_enum_member_t, semaMembers, 64, &self->TempAlloc)
    bool keepEnumScope = false;

    enumScope.Owner.v = SCOPE_OWNER_V(scope_owner_enum, 0);
    tmpSemaEnum.MemberCount = enm->MemberCount;
    tmpSemaEnum.Identifier = enm->Identifier;

    MetaCScopeTable_InitN(&enumScope.ScopeTable, tmpSemaEnum.MemberCount, &self->TempAlloc);

    MetaCSemantic_PushTemporaryScope(self, &enumScope);

    ARENA_ARRAY_ENSURE_SIZE(semaMembers, tmpSemaEnum.MemberCount);

    semaMembersCount = tmpSemaEnum.MemberCount;
    tmpSemaEnum.Members = semaMembers;

    MetaCSemantic_ComputeEnumValues(self, enm, &tmpSemaEnum);

    MetaCSemantic_PopTemporaryScope(self);
    {
/*
        metac_printer_t debugPrinter;
        MetaCPrinter_Init(&debugPrinter, self->ParserIdentifierTable, self->ParserStringTable, 0);


        for(uint32_t i = 0; i < tmpSemaEnum.MemberCount; i++)
        {
            const char* valueString = MetaCPrinter_PrintSemaNode(&debugPrinter,
                self, tmpSemaEnum.Members[i].Value);
            printf("Member %s = %s\n", IdentifierPtrToCharPtr(self->ParserIdentifierTable,
                                                              tmpSemaEnum.Members[i].Identifier),
                                                              valueString
            );
            MetaCPrinter_Reset(&debugPrinter);
        }
*/
    }

    enumScope.Closed = true;

    result = MetaCTypeTable_GetOrEmptyEnumType(&self->EnumTypeTable, &tmpSemaEnum);
    if (result.v == 0)
    {
        STACK_ARENA_ARRAY_TO_HEAP(semaMembers, &self->Allocator);

        tmpSemaEnum.Members = semaMembers;
        result = MetaCTypeTable_AddEnumType(&self->EnumTypeTable, &tmpSemaEnum);

        keepEnumScope = true;
        MetaCSemantic_RegisterInScope(self, tmpSemaEnum.Identifier, cast(metac_node_t)EnumTypePtr(self, result.Index));
        for(uint32_t i = 0; i < semaMembersCount; i++)
        {
            semaMembers[i].Value->TypeIndex.v = result.v;
        }
    }

    Allocator_FreeArena(&self->TempAlloc, semaMembersArenaPtr);
#define ISOLATED_ENUM_SCOPE 0
    if (!ISOLATED_ENUM_SCOPE)
    {
        assert(self->CurrentScope->Owner.Kind == scope_owner_module);

        for(uint32_t memberIndex = 0;
            memberIndex < tmpSemaEnum.MemberCount;
            memberIndex++)
        {
            metac_enum_member_t* member = tmpSemaEnum.Members + memberIndex;

            scope_insert_error_t inserted =
                MetaCSemantic_RegisterInScope(self, member->Identifier, METAC_NODE(member));
            if(inserted != success)
            {
                SemanticError(member->Header.LocationIdx, "%s couldn't be inserted into the scope",
                              IdentifierPtrToCharPtr(self->ParserIdentifierTable, member->Identifier));
            }
        }
    }

    if (result.v != 0)
    {
        Allocator_FreeArena(&self->Allocator, enumScope.ScopeTable.Arena);
    }

    // STACK_ARENA_FREE(self->Allocator, members);
    // STACK_ARENA_FREE(self->Allocator, semaMembers);
    return result;
}

metac_type_index_t MetaCSemantic_TypeSemantic(metac_sema_state_t* self,
                                              decl_type_t* type)
{
    metac_type_index_t result = {0};

    metac_type_kind_t typeKind = type->TypeKind;

    if (type->Kind == decl_type && isBasicType(typeKind))
    {
        result = MetaCSemantic_GetTypeIndex(self, typeKind, type);
    }
    else if (typeKind == type_type)
    {
        result = MetaCSemantic_GetTypeIndex(self, type_type, type);
    }
    else if (typeKind == type_auto)
    {
        result = MetaCSemantic_GetTypeIndex(self, type_auto, type);
    }
    else if (type->Kind == decl_type_tuple)
    {
        result = TypeTupleSemantic(self, type);
    }
    else if (type->Kind == decl_type_array)
    {
        result = TypeArraySemantic(self, type);
    }
    else if (type->Kind == decl_type_typedef)
    {
        decl_type_typedef_t* typedef_ = cast(decl_type_typedef_t*) type;
        metac_identifier_ptr_t semaId = 
            MetaCIdentifierTable_CopyIdentifier(self->ParserIdentifierTable,
                                                &self->SemanticIdentifierTable,
                                                typedef_->Identifier);

        self->CurrentLocIdx = typedef_->LocationIdx;
        metac_type_index_t elementTypeIndex =
            MetaCSemantic_doTypeSemantic(self, typedef_->Type);

        uint32_t hash = elementTypeIndex.v;

        metac_type_typedef_t key = {
            {decl_type_typedef, 0, hash},
            zeroIdx, elementTypeIndex, typedef_->Identifier
        };

        result =
            MetaCTypeTable_GetOrEmptyTypedefType(&self->TypedefTypeTable, &key);
        if (result.v == 0)
        {
            result = MetaCTypeTable_AddTypedefType(&self->TypedefTypeTable, &key);
        }
        metac_type_typedef_t* semaTypedef = TypedefPtr(self, TYPE_INDEX_INDEX(result));
        //semaTypedef->Type = elementTypeIndex;

        scope_insert_error_t scopeInsertError =
            MetaCSemantic_RegisterInScope(self, typedef_->Identifier, (metac_node_t)semaTypedef);
    }
    else if (type->Kind == decl_type_typeof)
    {
        decl_type_typeof_t* type_typeof = cast(decl_type_typeof_t*) type;

        metac_sema_expr_t* se =
            MetaCSemantic_doExprSemantic(self, type_typeof->Expr, 0);
        result = se->TypeIndex;
    }
    else if (type->Kind == decl_type_enum)
    {
        result = TypeEnumSemantic(self, type);
    }
    else if (IsAggregateTypeDecl(type->Kind))
    {
        STACK_ARENA_ARRAY(metac_type_aggregate_field_t, tmpFields, 128, &self->TempAlloc)

        decl_type_struct_t* agg = (decl_type_struct_t*) type;
        metac_type_aggregate_t tmpSemaAggMem = {(metac_decl_kind_t)0};
        metac_type_aggregate_t* tmpSemaAgg = &tmpSemaAggMem;
        metac_scope_t tmpTemplateScope = {0};


        if (type->Kind == decl_type_struct)
            typeKind = type_struct;
        else if (type->Kind == decl_type_union)
            typeKind = type_union;
        else
            assert(0);

        ARENA_ARRAY_ENSURE_SIZE(tmpFields, agg->FieldCount);

        tmpSemaAgg->Header.Kind = agg->Kind;
        tmpSemaAgg->Identifier = agg->Identifier;
        tmpSemaAgg->Fields = tmpFields;
        tmpSemaAgg->FieldCount = agg->FieldCount;

        PopulateTemporaryAggregateScope(self, tmpSemaAgg, agg);
        MetaCSemantic_MountScope(self, tmpSemaAgg->Scope);

        if (agg->ParameterCount != 0)
        {
            uint32_t parameterCount = agg->ParameterCount;
            decl_parameter_t* param = agg->Parameters;
            U32(tmpTemplateScope.ScopeFlags) |= scope_flag_temporary;
            MetaCSemantic_PushTemporaryScope(self, &tmpTemplateScope);
         
            for(uint32_t paramIdx = 0; paramIdx < parameterCount; paramIdx++)
            {
                metac_identifier_ptr_t paramIdent = param->Parameter->VarIdentifier;
                metac_location_ptr_t locIdx = param->Parameter->LocationIdx;
                metac_expr_t placeholderExpr;
                metac_sema_expr_t* placeholder = AllocNewSemaExpr(self, &placeholderExpr);

                placeholder->Kind = expr_unknown_value;
                placeholder->TypeIndex = MetaCSemantic_TypeSemantic(self, param->Parameter->VarType);
                METAC_NODE(placeholder->Expr) = emptyNode;
                placeholder->LocationIdx = locIdx;


                MetaCSemantic_RegisterInScope(self, paramIdent, METAC_NODE(placeholder));

                param = param->Next;
            }
        }

        switch(typeKind)
        {
            case type_struct:
            {
                uint32_t hash;
                MetaCSemantic_ComputeStructLayout(self, agg, tmpSemaAgg);

                if (agg->ParameterCount != 0)
                {
                    MetaCSemantic_PopTemporaryScope(self);
                }
                tmpSemaAgg->Header.Hash = AggregateHash(tmpSemaAgg);

                result =
                    MetaCTypeTable_GetOrEmptyStructType(&self->StructTypeTable, tmpSemaAgg);
                if (result.v == 0)
                {
                    metac_type_aggregate_t* semaAgg;
                    semaAgg =
                        MetaCSemantic_PersistTemporaryAggregateAndPopulateScope(self, tmpSemaAgg);

                    result.v = TYPE_INDEX_V(type_index_struct, StructIndex(self, semaAgg));
                }
                // MetaCSemantic_RegisterInScopeStructNamespace()
            } break;

            case type_union:
            {

            } break;

            case type_class:
            {
                assert(0);
                // Not implemented yet
            } break;

            default: assert(0);

        }

        MetaCSemantic_UnmountScope(self);
    }
    else if (IsPointerType(type->Kind))
    {
        metac_type_index_t elementTypeIndex = {0};
        decl_type_ptr_t* typePtr = (decl_type_ptr_t*) type;
        elementTypeIndex =
            MetaCSemantic_doTypeSemantic(self, typePtr->ElementType);

        if (elementTypeIndex.v == 0 || elementTypeIndex.v == -1)
        {
            metac_location_t currentLoc = {0};
            SemanticError(currentLoc, "Cannot resolve ptr element type {%s}", "");
            return result;
        }
        result = MetaCSemantic_GetPtrTypeOf(self, elementTypeIndex);
    }
    else if (type->Kind == decl_type_functiontype)
    {
        decl_type_functiontype_t* functionType =
            (decl_type_functiontype_t*) type;

        metac_type_index_t returnType =
            MetaCSemantic_doTypeSemantic(self,
                functionType->ReturnType);

        uint32_t hash = crc32c_nozero(~0, &returnType, sizeof(returnType));
        decl_parameter_t* currentParam = functionType->Parameters;

        const uint32_t nParams = functionType->ParameterCount;
        STACK_ARENA_ARRAY(metac_type_index_t, parameterTypes, 64, &self->TempAlloc);
        ARENA_ARRAY_ENSURE_SIZE(parameterTypes, nParams);

        for(uint32_t i = 0;
            i < nParams;
            i++)
        {
            ARENA_ARRAY_ADD(parameterTypes,
                MetaCSemantic_doTypeSemantic(self, currentParam->Parameter->VarType));
            currentParam = currentParam->Next;
        }

        hash = crc32c_nozero(hash, parameterTypes, sizeof(*parameterTypes) * nParams);
        metac_type_header_t header = {decl_type_functiontype, 0, hash, 0};

        metac_type_functiontype_t key = {
            header,
            zeroIdx,
            returnType,
            parameterTypes,
            0, // parameter names
            nParams
        };

        result = MetaCTypeTable_GetOrEmptyFunctionType(&self->FunctionTypeTable, &key);
        if (result.v == 0)
        {
            STACK_ARENA_ARRAY_TO_HEAP(parameterTypes, &self->Allocator);
            key.ParameterTypes = parameterTypes;
            result =
                MetaCTypeTable_AddFunctionType(&self->FunctionTypeTable, &key);
#ifndef NDEBUG
            metac_type_index_t test =
                MetaCTypeTable_GetOrEmptyFunctionType(&self->FunctionTypeTable, &key);
            assert(result.v == test.v);
#endif
        }
    }
    else if (type->Kind == decl_type_template_instance)
    {
        decl_type_template_instance_t* tInst = cast(decl_type_template_instance_t*) type;
        expr_argument_t* args = tInst->Arguments;
        STACK_ARENA_ARRAY(metac_sema_expr_t*, semaArguments, 64, &self->TempAlloc);

#if 0
        metac_printer_t debugPrinter = {0};
        MetaCPrinter_Init(&debugPrinter, self->ParserIdentifierTable, self->ParserStringTable, &self->TempAlloc);
#endif
        ARENA_ARRAY_ENSURE_SIZE(semaArguments, tInst->ArgumentCount);
        // before calling semantic we need to mount the template instance scope.
        metac_scope_t tmpScope = {scope_flag_temporary};
        MetaCSemantic_PushTemporaryScope(self, &tmpScope);

        for(metac_expr_t* e = args->Expr; METAC_NODE(args) != emptyNode; args = args->Next, e = args->Expr)
        {
            ARENA_ARRAY_ADD(semaArguments, MetaCSemantic_doExprSemantic(self, e, 0));
        }

        {
            metac_decl_t* symbol = NULL;
            metac_sema_expr_t** arguments = semaArguments;
            uint32_t hash = symbol ? crc32c_nozero(~0, &symbol->Hash, sizeof(symbol->Hash)) : ~0;
            uint32_t nArguments = tInst->ArgumentCount;
            for(uint32_t argIdx = 0; argIdx < nArguments; argIdx++)
            {
                metac_sema_expr_t* semaArg = arguments[argIdx];
                hash = crc32c_nozero(hash, &semaArg->Hash, sizeof(semaArg->Hash));
            }
            {
                metac_type_header_t header = {decl_type_template_instance, 0, hash, 0};
                metac_type_template_t key = {
                    header,
                    zeroIdx,
                    symbol,
                    arguments,
                    nArguments,
                };
                result = MetaCTypeTable_GetOrEmptyTemplateType(&self->TemplateTypeTable, &key);
                if (result.v == 0)
                {
                    result = MetaCTypeTable_AddTemplateType(&self->TemplateTypeTable, &key);
                }
            }

        }

    }
    else if (type->Kind == decl_type)
    {
        if ((type->TypeKind != type_identifier) && (type->TypeKind != type_template_instance))
        {
            assert(!"Only identifier types and template types are expected to be resovled here");
        }
        xprintf("MetaCNodeKind_toChars: %s\n", MetaCNodeKind_toChars((metac_node_kind_t)type->Kind));
        xprintf("TypeIdentifier: %s\n", IdentifierPtrToCharPtr(self->ParserIdentifierTable, type->TypeIdentifier));
LtryAgian: {}
        metac_node_t node =
            MetaCSemantic_LookupIdentifier(self, type->TypeIdentifier);
        {
            const char* idChars =
                IdentifierPtrToCharPtr(self->ParserIdentifierTable, type->TypeIdentifier);
#if 0
            printf("Lookup: (%s) = %s\n", idChars, ((node != emptyNode) ?
                                       MetaCNodeKind_toChars(node->Kind) :
                                       "empty"));
#endif
            if (node == emptyNode)
            {
                uint32_t stackIdx =
                    (self->OnResolveFailStackCount ? self->OnResolveFailStackCount - 1 : 0);

                if (self->OnResolveFailStack[stackIdx] == OnResolveFail_ReturnNull)
                {
                    // MetaCSemantic_ResolveFail(self, type->TypeIdentifier);
                    metac_type_index_t unresolvedIdx = {0};
                    return unresolvedIdx;
                }
#ifndef NO_FIBERS
#if 0
                WaitForSignal(MetaCSemantic_RegisterInScope, type->TypeIdentifier);
                CancelIf(&self->CurrentScope->Closed);
#endif
                aco_t* me = (aco_t*)CurrentFiber();
                task_t* task = CurrentTask();
                printf("Yield!\n");
                metac_semantic_waiter_t* meWaiter = &self->Waiters.Waiters[INC(self->Waiters.WaiterCount)];
                meWaiter->FuncHash = CRC32C_S("MetaCSemantic_LookupIdentifier");
                meWaiter->NodeHash = CRC32C_VALUE(~0, type->TypeIdentifier);
                meWaiter->Continuation = me;
                U32(task->TaskFlags) |= Task_Waiting;
                U32(task->TaskFlags) &= (~Task_Running);
                YIELD(WaitOnResolve);
                printf("Trying agian after yielding\n");
                goto LtryAgian;
#else
                printf("No fiber support ... cannot deal with deferred lookup\n");
#endif
            }
        }

        if (node != emptyNode)
        {
            switch(node->Kind)
            {
                case decl_type_typedef:
                {
                    metac_type_typedef_t* typedef_ = (metac_type_typedef_t*)node;
                    result = typedef_->Type;
                } break;
                case decl_type_struct:
                {
                    metac_type_aggregate_t* struct_ = (metac_type_aggregate_t*)node;
                    result.v = TYPE_INDEX_V(type_index_struct, StructIndex(self, (struct_)));
                } break;
                case decl_type_enum:
                {
                    metac_type_enum_t* enum_ = (metac_type_enum_t*)node;
                    result.v = TYPE_INDEX_V(type_index_enum, EnumIndex(self, enum_));
                } break;
                case decl_variable:
                {
                    sema_decl_variable_t* var = (sema_decl_variable_t*)node;
                    result.v = var->TypeIndex.v;
                } break;
                case decl_field:
                {
                    sema_decl_variable_t* var = (sema_decl_variable_t*)node;
                    result.v = var->TypeIndex.v;
                } break;
            }
        }
        else
        {
            printf("Empty node during lookup\n");
            assert(0);
        }
    }
    else
    {
        assert(0); // me not know what do do.
    }
#ifndef NO_FIBERS
    const uint32_t funcHash = crc32c(~0, "MetaCSemantic_doTypeSemantic",
                                  sizeof("MetaCSemantic_doTypeSemantic") -1);

    uint32_t nodeHash = type->Hash;
    // check if someone waited for the node we just resolved
    for(uint32_t waiterIdx = 0;
        waiterIdx < self->Waiters.WaiterCount;
        waiterIdx++)
    {
        metac_semantic_waiter_t waiter = self->Waiters.Waiters[waiterIdx];
        if (waiter.FuncHash == funcHash
         && waiter.NodeHash == nodeHash)
        {
            printf("Found someone waiting for me\n");
            RESUME(waiter.Continuation);
        }
    }
#endif
    assert(result.v != 0);
    return result;
}

#ifndef NO_FIBERS
void MetaCSemantic_doTypeSemantic_Task(task_t* task)
{
    MetaCSemantic_doTypeSemantic_Fiber_t* ctx =
        cast(MetaCSemantic_doTypeSemantic_Fiber_t*) task->Context;
    metac_sema_state_t* sema = ctx->Sema;
    decl_type_t* type = ctx->Type;

    ctx->Result = MetaCSemantic_TypeSemantic(sema, type);
}


void MetaCSemantic_doTypeSemantic_Fiber(void* caller, void* arg)
{
    MetaCSemantic_doTypeSemantic_Fiber_t* ctx =
        cast(MetaCSemantic_doTypeSemantic_Fiber_t*) arg;
    metac_sema_state_t* sema = ctx->Sema;
    decl_type_t* type = ctx->Type;

    ctx->Result = MetaCSemantic_TypeSemantic(sema, type);
}
#endif

metac_type_index_t MetaCSemantic_doTypeSemantic_(metac_sema_state_t* self,
                                                 decl_type_t* type,
                                                 const char* callFile,
                                                 uint32_t callLine)
{
    metac_type_index_t result = {0};

//   result =
//        DO_TASK(MetaCSemantic_TypeSemantic, self, type);

    result = MetaCSemantic_TypeSemantic(self, type);

    if (!result.v)
    {
#ifndef NO_FIBERS
        worker_context_t currentContext = *CurrentWorker();

        MetaCSemantic_doTypeSemantic_Fiber_t arg = {
                self, type
        };
        MetaCSemantic_doTypeSemantic_Fiber_t* argPtr = &arg;

        CALL_TASK_FN(MetaCSemantic_doTypeSemantic, argPtr);
        task_t typeSemTask = {0};

        typeSemTask.Context = argPtr;
        typeSemTask.ContextSize = sizeof(arg);
        typeSemTask.TaskFunction = MetaCSemantic_doTypeSemantic_Task;

        while (!TaskQueue_Push(&currentContext.Queue, &typeSemTask))
        {
            // yielding because queue is full;
            YIELD(QueueFull);
        }

        uint32_t funcHash = CRC32C_S("MetaCSemantic_doTypeSemantic");
        uint32_t nodeHash = type->Hash;

        for(uint32_t waiterIdx = 0;
            waiterIdx < self->Waiters.WaiterCount;
            waiterIdx++)
        {
            metac_semantic_waiter_t waiter = self->Waiters.Waiters[waiterIdx];
            if (funcHash == waiter.FuncHash && nodeHash == waiter.NodeHash)
            {
                aco_yield2(waiter.Continuation);
            }
        }

    result = arg.Result;
#else
    printf("Without fiber no deferral\n");
#endif
    }

    if (result.Kind != type_index_invalid
     && result.Kind != type_index_basic
     && result.Kind != type_index_unknown)
    {
        NodeFromTypeIndex(self, result)->Origin = type;
    }

    return result;
}

metac_type_index_t MetaCSemantic_GetArrayTypeOf(metac_sema_state_t* state,
                                                metac_type_index_t elementTypeIndex,
                                                uint32_t dimension)
{
    uint32_t hash = EntangleInts(TYPE_INDEX_INDEX(elementTypeIndex), dimension);
    metac_type_array_t key = {
        {decl_type_array, 0, hash, 0},
        zeroIdx, elementTypeIndex, dimension
    };

    metac_type_index_t result =
        MetaCTypeTable_GetOrEmptyArrayType(&state->ArrayTypeTable, &key);

    if (result.v == 0)
    {
        result =
            MetaCTypeTable_AddArrayType(&state->ArrayTypeTable, &key);
    }

    if (1)
    {
        metac_type_array_t* array = ArrayTypePtr(state, TYPE_INDEX_INDEX(result));
        assert(array->Dim == dimension);
        assert(array->ElementType.v == elementTypeIndex.v);
    }

    return result;
}

void MetaCSizeComputer_Init(metac_size_computer_t* self)
{
    self->MaxAlignment = 1;
    self->CurrentSize = 0;
}

void MetaCSizeComputer_BeginSizeOf(metac_size_computer_t* self)
{
    assert(self->MaxAlignment == 1);
    assert(self->CurrentSize == 0);
}

/// returns the offset of the member
uint32_t MetaCSizeComputer_MemberType(metac_size_computer_t* self,
                                      metac_sema_state_t* sema,
                                      metac_type_index_t memberType)
{
    uint32_t requestedAligment =
        MetaCSemantic_GetTypeAlignment(sema, memberType);

    uint32_t memberOffset = Align(self->CurrentSize, requestedAligment);

    assert(requestedAligment != -1);

    if (requestedAligment > self->MaxAlignment)
        self->MaxAlignment = requestedAligment;

    self->CurrentSize = (memberOffset
                         + MetaCSemantic_GetTypeSize(sema, memberType));

    return memberOffset;
}

// returns the size of the type
uint32_t MetaCSizeComputer_FinishSizeOf(metac_size_computer_t* self)
{
    uint32_t result = Align(self->CurrentSize, self->MaxAlignment);

    self->MaxAlignment = 1;
    self->CurrentSize = 0;

    return result;
}

bool MetaCSemantic_ComputeStructLayout(metac_sema_state_t* self,
                                       decl_type_struct_t* agg,
                                       metac_type_aggregate_t* semaAgg)
{
    bool result = true;

    assert(self->CurrentScope == semaAgg->Scope || ((self->CurrentScope->ScopeFlags & scope_flag_temporary) && self->CurrentScope->Parent == semaAgg->Scope) );
    // make sure the scope is mounted.
    assert(semaAgg->Fields && semaAgg->Fields != emptyPointer);

    metac_type_aggregate_field_t* onePastLast =
        semaAgg->Fields + semaAgg->FieldCount;
    decl_field_t* declField = agg->Fields;

    // first determine all the types which will determine the sizes;
    for(metac_type_aggregate_field_t* semaField = semaAgg->Fields;
        semaField < onePastLast;
        semaField++)
    {
        semaField->Identifier  = declField->Field->VarIdentifier;
        semaField->Header.Kind = node_decl_field;

        semaField->Type =
            MetaCSemantic_doTypeSemantic(self, declField->Field->VarType);
        if (IsUnknownType(self, semaField->Type))
        {
            xprintf("Found unknown type \n");
            return false;
        }
#ifndef NO_FIBERS
        while (!semaField->Type.v)
        {
            xprintf("FieldType couldn't be resolved ... yielding fiber\n");
            aco_t* me = (aco_t*)CurrentFiber();
            if (me != 0)
            {
                metac_semantic_waiter_t waiter;
                waiter.FuncHash = CRC32C_S("MetaCSemantic_doTypeSemantic");
                waiter.NodeHash = declField->Field->VarType->Hash;
                waiter.Continuation = me;

                assert(self->Waiters.WaiterCount < self->Waiters.WaiterCapacity);
                self->Waiters.Waiters[self->Waiters.WaiterCount++] = waiter;
                xprintf("We should Yield\n");
                (cast(task_t*)(me->arg))->TaskFlags = Task_Waiting;
                YIELD(watingOnTypeSemantic);
                (cast(task_t*)(me->arg))->TaskFlags = Task_Running;
                xprintf("Now we should be able to resolve\n");
                semaField->Type =
                    MetaCSemantic_doTypeSemantic(self, declField->Field->VarType);
            }
            // YIELD_ON(declField->Field->VarType, MetaCSemantic_doTypeSemantic);
        }
#endif
        if (!semaField->Type.v)
        {
            return false;
        }
        declField = declField->Next;
    }

    {
        metac_size_computer_t sizeComputer;
        MetaCSizeComputer_Init(&sizeComputer);
        MetaCSizeComputer_BeginSizeOf(&sizeComputer);

        for(metac_type_aggregate_field_t* semaField = semaAgg->Fields;
            semaField < onePastLast;
            semaField++)
        {
            semaField->Offset = MetaCSizeComputer_MemberType(&sizeComputer, self, semaField->Type);
        }

        semaAgg->Alignment = sizeComputer.MaxAlignment;
        semaAgg->Size = MetaCSizeComputer_FinishSizeOf(&sizeComputer);
    }
    //fprintf(stderr, "sizeof(struct) = %u\n", semaAgg->Size);//DEBUG
    //fprintf(stderr, "Alignof(struct) = %u\n", semaAgg->Alignment);//DEBUG

    return result;
}
