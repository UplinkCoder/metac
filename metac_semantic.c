#include "metac_semantic.h"
#include "compat.h"
#include <assert.h>

static inline bool isBasicType(metac_type_kind_t typeKind)
{
    if ((typeKind >= type_void) & (typeKind <= type_unsigned_long_long))
    {
        return true;
    }
    return false;
}

void MetaCSemantic_Init(metac_semantic_state_t* self)
{
    TypeTableInitImpl(&self->ArrayTypeTable,
                      sizeof(metac_type_array_slot_t),
                      type_index_array);

    TypeTableInitImpl(&self->StructTypeTable,
                      sizeof(metac_type_struct_slot_t),
                      type_index_struct);
}

void MetaCSemantic_doDeclSemantic(metac_semantic_state_t* state,
                                  metac_declaration_t* decl)
{
    switch(decl->DeclKind)
    {
        case decl_function:
        {
            decl_function_t* f = cast(decl_function_t*) decl;
        } break;
        case decl_variable:
        {
            decl_variable_t* v = cast(decl_variable_t*) decl;
        } break;
    }
}

metac_type_index_t MetaCSemantic_GetTypeIndex(metac_semantic_state_t* state,
                                              metac_type_kind_t typeKind,
                                              decl_type_t* type)
{
    if (isBasicType(typeKind))
    {
        return (metac_type_index_t) {
            TYPE_INDEX_V(type_index_basic, (uint32_t) typeKind)
        };
    }

    return (metac_type_index_t) {0};
}

metac_type_index_t MetaCSemantic_GetArrayTypeOf(metac_semantic_state_t* state,
                                                metac_type_index_t elementTypeIndex,
                                                uint32_t dimension)
{
    uint32_t hash = EntangleInts(TYPE_INDEX_INDEX(elementTypeIndex), dimension);
    metac_type_array_slot_t key = {hash, elementTypeIndex, dimension};

    metac_type_index_t result =
        MetaCTypeTable_GetOrAddArrayType(&state->ArrayTypeTable, hash, &key);

    return result;
}

#include "metac_printer.h"
static inline const char* BasicTypeToChars(metac_type_index_t typeIndex)
{
    assert(TYPE_INDEX_KIND(typeIndex) == type_index_basic);
    switch((metac_type_kind_t) TYPE_INDEX_INDEX(typeIndex))
    {
        case type_invalid :
            assert(0);

        case type_void :
            return "void";

        case type_bool :
            return "bool";
        case type_char:
            return "char";

        case type_unsigned_int:
            return "unsigned int";
        case type_unsigned_long :
            return "long";
        case type_unsigned_long_long:
            return "unsigned long long";

        case type_long :
            return "long";
        case type_long_long:
            return "long long";
        case type_int :
            return "int";
        case type_float :
            return "float";
    }
    return 0;
}

static inline void TypeToCharsP(metac_semantic_state_t* self,
                                metac_printer_t* printer,
                                metac_type_index_t typeIndex)
{
    uint32_t typeIndexIndex = TYPE_INDEX_INDEX(typeIndex);

    switch (TYPE_INDEX_KIND(typeIndex))
    {
        case type_index_array:
        {
            metac_type_array_slot_t (*slots)[4096] = (metac_type_array_slot_t(*)[4096])self->ArrayTypeTable.Slots;

            metac_type_array_slot_t* arrayType =
                (self->ArrayTypeTable.Slots + TYPE_INDEX_INDEX(typeIndex));
            TypeToCharsP(self, printer, arrayType->ElementTypeIndex);
            MetacPrinter_PrintStringLiteral(printer, "[");
            MetacPrinter_PrintI64(printer, (int64_t)arrayType->Dimension);
            MetacPrinter_PrintStringLiteral(printer, "]");
        } break;
        case type_index_basic:
        {
            const char* typeString = BasicTypeToChars(typeIndex);
            MetacPrinter_PrintStringLiteral(printer, typeString);
        } break;
    }
}

const char* TypeToChars(metac_semantic_state_t* self, metac_type_index_t typeIndex)
{
    const char* result = 0;
    static metac_printer_t printer = {0};
    if (!printer.IdentifierTable)
        MetaCPrinter_Init(&printer, self->IdentifierTable, 0);
    else
        MetaCPrinter_Reset(&printer);
    TypeToCharsP(self, &printer, typeIndex);
    printer.StringMemory[printer.StringMemorySize++] = '\0';
    result = printer.StringMemory;
}


void MetaCSemantic_doExprSemantic(metac_semantic_state_t* state,
                                  metac_expression_t* expr)
{
    switch(expr->Kind)
    {
        case exp_invalid:
            assert(0);

        case exp_char :
            expr->TypeIndex = MetaCSemantic_GetTypeIndex(state, type_char, 0);
        break;
        case exp_string :
            expr->TypeIndex = MetaCSemantic_GetArrayTypeOf(state,
                MetaCSemantic_GetTypeIndex(state, type_char, 0),
                LENGTH_FROM_STRING_KEY(expr->StringKey));
        break;
        case exp_signed_integer :
            expr->TypeIndex = MetaCSemantic_GetTypeIndex(state, type_int, 0);
        break;
    }
}
