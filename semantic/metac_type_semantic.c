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

static const metac_type_index_t zeroIdx = {0};
metac_type_index_t MetaCSemantic_GetPtrTypeOf(metac_semantic_state_t* self,
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

sema_decl_type_t* MetaCSemantic_GetTypeNode(metac_semantic_state_t* self,
                                            metac_type_index_t typeIndex)
{
    return cast(sema_decl_type_t*) NodeFromTypeIndex(self, typeIndex);
}
metac_type_index_t MetaCSemantic_GetTypeIndex(metac_semantic_state_t* state,
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

static inline bool IsAggregateTypeDecl(metac_declaration_kind_t declKind)
{
    if (declKind == decl_type_struct || declKind == decl_type_union)
    {
        return true;
    }
    return false;
}

static inline bool IsPointerType(metac_declaration_kind_t declKind)
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
    bool result = false;

    switch(TYPE_INDEX_KIND(a))
    {
        case type_index_functiontype:
        case type_index_struct:
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

    }

    return result;
}


///TODO FIXME
/// this is not nearly complete!
metac_type_index_t MetaCSemantic_CommonSubtype(metac_semantic_state_t* self,
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

metac_type_index_t MetaCSemantic_GetElementType(metac_semantic_state_t* self,
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
    }

    return result;
}

uint32_t MetaCSemantic_GetTypeAlignment(metac_semantic_state_t* self,
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
    else
    {
        assert(0);
    }

    return result;
}

static metac_type_index_t* NextTypeTupleElem(metac_type_index_t* typeIndexP)
{
    return typeIndexP + 1;
}

/// Returns size in byte or INVALID_SIZE on error
uint32_t MetaCSemantic_GetTypeSize(metac_semantic_state_t* self,
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
        uint32_t sz = ComputeStructSize(self, tuple->TypeIndicies, tuple->TypeCount, NextTypeTupleElem);
        // we now create a struct temporarily so we can use the same
        // logic we use to determine the size of structs
        result = sz;
    }
    else
    {
        assert(0);
    }

    return result;
}


/// this is where we also populate the scope
metac_type_aggregate_t* MetaCSemantic_PersistTemporaryAggregateAndPopulateScope(metac_semantic_state_t* self,
                                                                                metac_type_aggregate_t* tmpAgg)
{
    // memcpy(semaAgg, tmpAgg, sizeof(metac_type_aggregate_t));
    uint32_t nFields = tmpAgg->FieldCount;
    uint32_t nSlots = nFields + ((nFields < 16) ? 4 : (nFields >> 2));

    const uint32_t scopeTableSize =
        ALIGN16(NEXTPOW2(nSlots) * sizeof(metac_scope_table_slot_t))
        + ALIGN16(sizeof(metac_scope_t));

    const uint32_t aggregateMemorySize =
        ALIGN16(nFields * sizeof(metac_type_aggregate_field_t))
        + scopeTableSize;

    arena_ptr_t arenaPtr = AllocateArena(&self->Allocator, aggregateMemorySize);
    tagged_arena_t* aggregateArena = &self->Allocator.Arenas[arenaPtr.Index];
    metac_type_aggregate_field_t* aggFields = cast(metac_type_aggregate_field_t*)
        aggregateArena->Memory;

    metac_scope_t* scope_ = cast(metac_scope_t*) (aggFields + nFields);
    metac_scope_table_slot_t* slots = (metac_scope_table_slot_t*) (scope_ + 1);

    metac_type_index_t typeIndex;
    metac_type_aggregate_t* semaAgg = 0;

    memset(scope_, 0, scopeTableSize);

    if (tmpAgg->Header.Kind == decl_type_struct)
    {
        typeIndex = MetaCTypeTable_AddStructType(&self->StructTypeTable, tmpAgg);
        semaAgg = StructPtr(self, TYPE_INDEX_INDEX(typeIndex));
        scope_->Owner.v = SCOPE_OWNER_V(scope_owner_struct, StructIndex(self, semaAgg));
    } else if (tmpAgg->Header.Kind == decl_type_union)
    {
        typeIndex = MetaCTypeTable_AddUnionType(&self->UnionTypeTable, tmpAgg);
        semaAgg = UnionPtr(self, TYPE_INDEX_INDEX(typeIndex));
        scope_->Owner.v = SCOPE_OWNER_V(scope_owner_union, UnionIndex(self, semaAgg));
    }
    else
    {
        assert(!"aggregate type not supported or it's not an aggregate type");
    }


    scope_->ScopeTable.Slots = slots;
    scope_->ScopeTable.SlotCount_Log2 = LOG2(nSlots);
    scope_->Parent = self->CurrentScope;

    MetaCSemantic_MountScope(self, scope_);
    metac_printer_t printer;
    MetaCPrinter_Init(&printer,
        self->ParserIdentifierTable, self->ParserStringTable
    );

    semaAgg->Scope = scope_;
    for(uint32_t i = 0; i < nFields; i++)
    {
        aggFields[i] = tmpAgg->Fields[i];
        MetaCPrinter_PrintSemaNode(&printer, self, cast(metac_node_t) &aggFields[i]);

        scope_insert_error_t inserted =
            MetaCSemantic_RegisterInScope(self, aggFields[i].Identifier, cast(metac_node_t)&aggFields[i]);
        if (inserted != success)
        {
            SemanticError(0, "%s could not be inserted",
                IdentifierPtrToCharPtr(self->ParserIdentifierTable, aggFields[i].Identifier));
        }
    }

    MetaCSemantic_UnmountScope(self);

    //TODO FIXME this should use a deep walk through the fields
    //MetaCDeclaration_Walk(tmpAgg,)

    semaAgg->Fields = aggFields;

    return semaAgg;
}
#define BeginTaskBarrier()
#define EndTaskBarrier()

void MetaCSemantic_ComputeEnumValues(metac_semantic_state_t* self,
                                     decl_type_enum_t* enum_,
                                     metac_type_enum_t* semaEnum)
{
    uint32_t lastUnresolvedMembers = 0;
    uint32_t unresolvedMembers = 0;
    decl_enum_member_t* member = enum_->Members;
    int64_t nextValue = 0;
    const uint32_t memberCount = enum_->MemberCount;
    STACK_ARENA_ARRAY(metac_sema_expression_t, memberPlaceholders, 32, &self->TempAlloc);

    uint32_t hash = enum_key;
    hash = CRC32C_VALUE(hash, enum_->Identifier.v);
    assert(SCOPE_OWNER_KIND(self->CurrentScope->Owner) == scope_owner_enum);
    ARENA_ARRAY_ENSURE_SIZE(memberPlaceholders, memberCount);
    memberPlaceholdersCount = memberCount;

    //TODO you want to make CurrentValue a metac_sema_expression_t
    // such that you can interpret the increment operator
    //SetInProgress(semaEnum, "Members");
    // semaEnum->Name = MetaCSemantic_RegisterIdentifier(self, enum->Identifier);

    semaEnum->Name = enum_->Identifier;
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
            memberPlaceholders[memberIdx].Kind = exp_unknown_value;
            memberPlaceholders[memberIdx].TypeIndex = semaEnum->BaseType;
            memberPlaceholders[memberIdx].Expression = member->Value;

            MetaCSemantic_RegisterInScope(self, member->Name, memberPlaceholders + memberIdx);
        }
    }

    self->CurrentScope->ScopeTable.AllowOverride = true;
    {
        MetaCSemantic_PushOnResolveFail(self, OnResolveFail_ReturnNull);
        do
        {
            lastUnresolvedMembers = unresolvedMembers;

            member = enum_->Members;
            for(uint32_t memberIdx = 0;
                memberIdx < memberCount;
                memberIdx++, member = member->Next)
            {
                if (METAC_NODE(member->Value) != emptyNode)
                {
                    metac_sema_expression_t* semaValue =
                        MetaCSemantic_doExprSemantic(self, member->Value, 0);
                    semaEnum->Members[memberIdx].Value = semaValue;
                    semaEnum->Members[memberIdx].Header.Kind = node_decl_enum_member;

                    if (IsUnresolved(METAC_NODE(semaValue)))
                    {
                        unresolvedMembers++;
                    }
                    else
                    {
                        MetaCSemantic_RegisterInScope(self, member->Name, semaEnum->Members + memberIdx);
                    }
                }
            }
            if (!unresolvedMembers)
            {
                MetaCSemantic_PopOnResolveFail(self);
            }
            else if (unresolvedMembers == lastUnresolvedMembers)
            {
                // we have a the same number of unresolved members as last round
                // which indicates a lack of local progress.
                MetaCSemantic_PopOnResolveFail(self);
                YIELD("Yielding because we are waiting for more enum members to become resolvable");
            }
        } while (unresolvedMembers);

        member = enum_->Members;
        for(uint32_t memberIdx = 0;
            memberIdx < memberCount;
            memberIdx++, member = member->Next)
        {

            semaEnum->Members[memberIdx].Identifier = member->Name;
            semaEnum->Members[memberIdx].Header.Kind = decl_enum_member;

            if (member->Value != cast(metac_expression_t*)emptyPointer)
            {
                metac_sema_expression_t* semaValue =
                    semaEnum->Members[memberIdx].Value;
                assert(member->Value);
                if (semaValue->Kind != exp_signed_integer)
                {
                    semaValue->TypeIndex.v = semaEnum->BaseType.v;
                    MetaCSemantic_ConstantFold(self, semaValue);
                }
                assert(semaValue->Kind == exp_signed_integer);
                nextValue = semaValue->ValueI64 + 1;
            }
            else
            {
                // let's construct a metac_expression from currentValue
                metac_expression_t Value = {exp_signed_integer, member->LocationIdx, 0, 0};
                Value.ValueI64 = nextValue++;

                semaEnum->Members[memberIdx].Value =
                    MetaCSemantic_doExprSemantic(self, &Value, 0);
                //TODO fix up the type of the value to be the enum type
    //            semaEnum->Members[memberIdx].Value->TypeIndex =
            }

            semaEnum->Members[memberIdx].Header.LocationIdx = member->LocationIdx;
            hash = CRC32C_VALUE(hash, semaEnum->Members[memberIdx].Identifier);
            hash = CRC32C_VALUE(hash, semaEnum->Members[memberIdx].Value->ValueI64);
            MetaCSemantic_RegisterInScope(self, member->Name, semaEnum->Members + memberIdx);
        }
    }
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

static metac_type_index_t TypeTupleSemantic(metac_semantic_state_t* self,
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
        zeroIdx, typeIndicies, typeIndiciesCount
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
metac_type_index_t TypeArraySemantic(metac_semantic_state_t* self,
                                     decl_type_t* type_)
{
    metac_type_index_t result = {0};

       decl_type_array_t* arrayType =
        cast(decl_type_array_t*)type_;
    metac_type_index_t elementType =
        MetaCSemantic_doTypeSemantic(self, arrayType->ElementType);
    // it could be an array win inferred dimensions in which case
    // arrayType->Dim is the empty pointer

    metac_sema_expression_t* dim = (METAC_NODE(arrayType->Dim) == emptyNode ?
        cast(metac_sema_expression_t*)emptyNode :
        MetaCSemantic_doExprSemantic(self, arrayType->Dim, 0)
    );

    if (METAC_NODE(dim) != emptyNode)
    {
        if (dim->Kind != exp_signed_integer)
        {
            fprintf(stderr, "Array dimension should eval to integer but it is: %s\n",
                MetaCExpressionKind_toChars(dim->Kind));
        }
    }
    uint32_t dimValue = (
        METAC_NODE(dim) != emptyNode ?
            (uint32_t)dim->ValueU64 : -1);

    result =
        MetaCSemantic_GetArrayTypeOf(self, elementType, dimValue);

    return result;
}

metac_type_index_t TypeEnumSemantic(metac_semantic_state_t* self,
                                    decl_type_t* type_)
{
    metac_type_index_t result = {0};

    decl_type_enum_t* enm = cast(decl_type_enum_t*) type_;

    metac_type_enum_t tmpSemaEnum = {0};

    metac_scope_t enumScope = { scope_flag_temporary };
    enumScope.Owner.v = SCOPE_OWNER_V(scope_owner_enum, 0);
    tmpSemaEnum.MemberCount = enm->MemberCount;
    tmpSemaEnum.Name = enm->Identifier;

    MetaCScopeTable_InitN(&enumScope.ScopeTable, tmpSemaEnum.MemberCount, &self->TempAlloc);

    MetaCSemantic_PushTemporaryScope(self, &enumScope);
    bool keepEnumScope = false;
    STACK_ARENA_ARRAY(metac_enum_member_t, semaMembers, 64, &self->TempAlloc);

    ARENA_ARRAY_ENSURE_SIZE(semaMembers, tmpSemaEnum.MemberCount);

    semaMembersCount = tmpSemaEnum.MemberCount;
    tmpSemaEnum.Members = semaMembers;

    MetaCSemantic_ComputeEnumValues(self, enm, &tmpSemaEnum);

    MetaCSemantic_PopTemporaryScope(self);

    enumScope.Closed = true;

    result = MetaCTypeTable_GetOrEmptyEnumType(&self->EnumTypeTable, &tmpSemaEnum);
    if (result.v == 0)
    {
        STACK_ARENA_ARRAY_TO_HEAP(semaMembers, &self->Allocator);

        tmpSemaEnum.Members = semaMembers;
        result = MetaCTypeTable_AddEnumType(&self->EnumTypeTable, &tmpSemaEnum);

        keepEnumScope = true;
        MetaCSemantic_RegisterInScope(self, tmpSemaEnum.Name, cast(metac_node_t)EnumTypePtr(self, result.Index));
    }

    Allocator_FreeArena(&self->TempAlloc, semaMembersArenaPtr);
#define ISOLATED_ENUM_SCOPE 0
    if (!ISOLATED_ENUM_SCOPE)
    {
        assert(TYPE_INDEX_KIND(self->CurrentScope->Owner) == scope_owner_module);

        for(uint32_t memberIndex = 0;
            memberIndex < tmpSemaEnum.MemberCount;
            memberIndex++)
        {
            metac_enum_member_t* member = tmpSemaEnum.Members + memberIndex;

            scope_insert_error_t inserted =
                MetaCSemantic_RegisterInScope(self, member->Identifier, METAC_NODE(member));
            if(inserted != success)
            {
                SemanticError(member->Header.LocationIdx, "Couldn't be inserted into the scope", 0);
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

metac_type_index_t MetaCSemantic_TypeSemantic(metac_semantic_state_t* self,
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

        metac_sema_expression_t* se =
            MetaCSemantic_doExprSemantic(self, type_typeof->Exp, 0);
        result = se->TypeIndex;
    }
    else if (type->Kind == decl_type_enum)
    {
        result = TypeEnumSemantic(self, type);
    }
    else if (IsAggregateTypeDecl(type->Kind))
    {
        decl_type_struct_t* agg = (decl_type_struct_t*) type;
        if (type->Kind == decl_type_struct)
            typeKind = type_struct;
        else if (type->Kind == decl_type_union)
            typeKind = type_union;
        else
            assert(0);

        STACK_ARENA_ARRAY(metac_type_aggregate_field_t, tmpFields, 128, &self->TempAlloc);
        ARENA_ARRAY_ENSURE_SIZE(tmpFields, agg->FieldCount);

        metac_type_aggregate_t tmpSemaAggMem = {(metac_declaration_kind_t)0};
        metac_type_aggregate_t* tmpSemaAgg = &tmpSemaAggMem;
        tmpSemaAgg->Header.Kind = agg->Kind;
        tmpSemaAgg->Identifier = agg->Identifier;
        tmpSemaAgg->Fields = tmpFields;
        tmpSemaAgg->FieldCount = agg->FieldCount;

        switch(typeKind)
        {
            case type_struct:
            {
                MetaCSemantic_ComputeStructLayout(self, agg, tmpSemaAgg);

                uint32_t hash = AggregateHash(tmpSemaAgg);
                tmpSemaAgg->Header.Hash = hash;

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
    }
    else if (IsPointerType(type->Kind))
    {
        metac_type_index_t elementTypeIndex = {0};
        decl_type_ptr_t* typePtr = (decl_type_ptr_t*) type;
        elementTypeIndex =
            MetaCSemantic_doTypeSemantic(self, typePtr->ElementType);
        assert(elementTypeIndex.v && elementTypeIndex.v != -1);
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
            nParams,
            0
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
    else if (type->Kind == decl_type && type->TypeKind == type_identifier)
    {
        //printf("MetaCNodeKind_toChars: %s\n", MetaCNodeKind_toChars((metac_node_kind_t)type->Kind));
        //printf("TypeIdentifier: %s\n", IdentifierPtrToCharPtr(self->ParserIdentifierTable, type->TypeIdentifier));
LtryAgian: {}
        metac_node_t node =
            MetaCSemantic_LookupIdentifier(self, type->TypeIdentifier);
        {
            const char* idChars =
                IdentifierPtrToCharPtr(self->ParserIdentifierTable, type->TypeIdentifier);

            printf("Lookup: (%s) = %s\n", idChars, ((node != emptyNode) ?
                                       MetaCNodeKind_toChars(node->Kind) :
                                       "empty"));
            if (node == emptyNode)
            {
                if (self->OnResolveFailStack[self->OnResolveFailStackCount - 1] == OnResolveFail_ReturnNull)
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
                task->TaskFlags |= Task_Waiting;
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
        assert(0); // me not no what do do.
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
     metac_semantic_state_t* sema = ctx->Sema;
    decl_type_t* type = ctx->Type;

    ctx->Result = MetaCSemantic_TypeSemantic(sema, type);

}


void MetaCSemantic_doTypeSemantic_Fiber(void* caller, void* arg)
{
    MetaCSemantic_doTypeSemantic_Fiber_t* ctx =
        cast(MetaCSemantic_doTypeSemantic_Fiber_t*) arg;
    metac_semantic_state_t* sema = ctx->Sema;
    decl_type_t* type = ctx->Type;

    ctx->Result = MetaCSemantic_TypeSemantic(sema, type);

}
#endif

metac_type_index_t MetaCSemantic_doTypeSemantic_(metac_semantic_state_t* self,
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

    return result;
}

metac_type_index_t MetaCSemantic_GetArrayTypeOf(metac_semantic_state_t* state,
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
/// Given a list of types compute how a struct would be layed out
/// if it had a fields comprised of those types in the same order
uint32_t ComputeStructSize(metac_semantic_state_t* self, metac_type_index_t* typeBegin,
    uint32_t nTypes, metac_type_index_t * (*Next) (metac_type_index_t*))
{
    uint32_t currentFieldOffset = 0;
    uint32_t maxAlignment = 0;

    {
        metac_type_index_t* typ = typeBegin;
        metac_type_index_t* nextType = Next(typeBegin);
        uint32_t i;

        for(i = 0; i < nTypes; i++)
        {
            uint32_t alignedSize = MetaCSemantic_GetTypeSize(self, *typ);
            assert(alignedSize != INVALID_SIZE);
            if (i < nTypes - 1)
            {
                uint32_t requestedAligment =
                    MetaCSemantic_GetTypeAlignment(self, *nextType);
                assert(requestedAligment != -1);
                if (requestedAligment > maxAlignment)
                    maxAlignment = requestedAligment;
                alignedSize = Align(alignedSize, requestedAligment);
                assert(((currentFieldOffset + alignedSize) % requestedAligment) == 0);
            }
            currentFieldOffset += alignedSize;
        }
        typ = Next(typ);
        nextType = Next(nextType);
    }

    return Align(currentFieldOffset, maxAlignment);
}

bool MetaCSemantic_ComputeStructLayout(metac_semantic_state_t* self,
                                       decl_type_struct_t* agg,
                                       metac_type_aggregate_t* semaAgg)
{
    // XXX make this use the compute structSize above
    bool result = true;

    assert(semaAgg->Fields && semaAgg->Fields != emptyPointer);

    uint32_t currentFieldOffset = 0;

    metac_type_aggregate_field_t* onePastLast =
        semaAgg->Fields + semaAgg->FieldCount;
    decl_field_t* declField = agg->Fields;

    // first determine all the types which will determine the sizes;
    for(metac_type_aggregate_field_t* semaField = semaAgg->Fields;
        semaField < onePastLast;
        semaField++)
    {
        semaField->Identifier  = declField->Field->VarIdentifier;
        semaField->Header.Kind = decl_field;

        semaField->Type =
            MetaCSemantic_doTypeSemantic(self, declField->Field->VarType);
#ifndef NO_FIBERS
        if (!semaField->Type.v)
        {
            printf("FieldType couldn't be resolved ... yielding fiber\n");
            aco_t* me = (aco_t*)CurrentFiber();
            if (me != 0)
            {
                metac_semantic_waiter_t waiter;
                waiter.FuncHash = CRC32C_S("MetaCSemantic_doTypeSemantic");
                waiter.NodeHash = declField->Field->VarType->Hash;
                waiter.Continuation = me;

                assert(self->Waiters.WaiterCount < self->Waiters.WaiterCapacity);
                self->Waiters.Waiters[self->Waiters.WaiterCount++] = waiter;
                printf("We should Yield\n");
                (cast(task_t*)(me->arg))->TaskFlags |= Task_Waiting;
                YIELD(watingOnTypeSemantic);
                printf("Now we should be able to resolve\n");
                semaField->Type =
                    MetaCSemantic_doTypeSemantic(self, declField->Field->VarType);
            }
            // YIELD_ON(declField->Field->VarType, MetaCSemantic_doTypeSemantic);
        }
#endif
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
        assert(alignedSize != INVALID_SIZE);
        if (semaField < (onePastLast - 1))
        {
            metac_type_aggregate_field_t *nextField = semaField + 1;
            uint32_t requestedAligment =
                MetaCSemantic_GetTypeAlignment(self, nextField->Type);
            assert(requestedAligment != -1);
            if (requestedAligment > maxAlignment)
                maxAlignment = requestedAligment;
            alignedSize = Align(alignedSize, requestedAligment);
            assert(((currentFieldOffset + alignedSize) % requestedAligment) == 0);
        }
        semaField->Offset = currentFieldOffset;
        currentFieldOffset += alignedSize;
    }

    // result =
    semaAgg->Size = currentFieldOffset;
    semaAgg->Alignment = maxAlignment;

    fprintf(stderr, "sizeof(struct) = %u\n", semaAgg->Size);//DEBUG
    fprintf(stderr, "Alignof(struct) = %u\n", semaAgg->Alignment);//DEBUG

    return result;
}
