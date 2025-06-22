#ifndef _METAC_COMPILER_INTERFACE_H_
#define _METAC_COMPILER_INTERFACE_H_

typedef int int32_t;
typedef unsigned int uint32_t;
typedef unsigned long int uint64_t;

#ifndef __METAC__
typedef void* type;
#endif

typedef enum type_kind_t
{
    TypeKind_Invalid,
    TypeKind_Enum,
    TypeKind_Struct = 5,
    TypeKind_Class = TypeKind_Struct + 2,
    TypeKind_Max
} type_kind_t;

typedef struct  metac_enum_members_t {
    const char** Names;
    uint32_t* Values;
    uint32_t Count;
} metac_enum_members_t;

typedef struct metac_compiler_t
{
    void* semanticState;

    uint32_t StartTimeStamp;
    void (*CurrentTimeStamp) (uint32_t* outResult);
    void (*BuiltinCount) (uint32_t* outResult);

    const char* (*PrintType) (struct metac_compiler_t* compilerP, type* T);

    const char** (*FieldNames) (type* T);

    const char* (*BuiltinName) (uint32_t* builtinNumber);
    const char* (*Help) ();

    void (*Message) (struct metac_compiler_t* compilerP,
                     const char* str,
                     void* context);

    void (*Error) (struct metac_compiler_t* compilerP,
                   const char* str,
                   void* context);

    type_kind_t* (*GetTypeKind) (struct metac_compiler_t* compilerP, type* T);

    void (*RegisterLogCallback) (struct metac_compiler_t* compilerP,
                                 void (*LogCb)(const char* msg, void* context),
                                 void* context);

    void (*RegisterIdentifierCallback) (struct metac_compiler_t* compilerP,
                                        void (*IdentfierCb)(const char* idChars, uint32_t idKey, void* context),
                                        void* context);
    void (*PrintInt) (int32_t* value);

    metac_enum_members_t* (*GetEnumMembers) (struct metac_compiler_t* compilerP, type* T);

    metac_node_t (*ResolveNode)(struct metac_compiler_t* compilerP, const char* name);

} metac_compiler_t;

/* Proposed Interface:
typedef struct {
    // Other members...

    // Function to get the type of a variable or expression
    type_t* (*GetType)(struct Compiler* compiler, const char* identifier);

    // Function to get the kind of a type (e.g., struct, union, enum)
    type_kind_t* (*GetTypeKind)(struct Compiler* compiler, type_t type);

    // Function to get the members of a struct or union type
    metac_aggregate_members_t* (*GetMembers)(struct Compiler* compiler, type_t structOrUnionType);

    // Function to assert a condition and generate a compiler error if false
    void (*Assert)(struct Compiler* compiler, int condition, const char* message);

    // Function to report a compiler error
    void (*Error)(struct Compiler* compiler, const char* message);

    // Function to register a callback for logging
    void (*RegisterLogCallback)(struct Compiler* compiler, void (*LogCb)(const char* msg, void* context), void* context);

    // Function to register a callback for handling identifiers
    void (*RegisterIdentifierCallback)(struct Compiler* compiler, void (*IdentifierCb)(const char* idChars, uint32_t idKey, void* context), void* context);

    // Function to register a callback for associating member names and types of struct types
    void (*RegisterStructMembersCallback)(struct Compiler* compiler, void (*StructMembersCb)(type_t structType, const char* memberName, type_t memberType, void* context), void* context);

    // Function to resolve identifiers in the current context
    type_t* (*ResolveInCurrentContext)(struct Compiler* compiler, const char* identifier);

    // Function to resolve a list of identifiers in the form "." identifier "." identifier "..."
    type_t* (*ResolveIdentifierList)(struct Compiler* compiler, const char* identifierList);

    // Function to get the type of a member from a struct or union type
    type_t* (*GetTypeFromMember)(struct Compiler* compiler, type_t structOrUnionType, const char* memberName);

    // Other compiler-related functions...

} Compiler;

type_t GetTypeFromMember(struct Compiler* compiler, type_t structOrUnionType, const char* memberName) {
    // Resolve the type of the member from the struct or union type
    metac_aggregate_members_t* members = compiler->GetMembers(compiler, structOrUnionType);

    for (uint32_t i = 0; i < members->Count; i++) {
        metac_aggregate_member_t member = members->Members[i];
        if (strcmp(member.name, memberName) == 0) {
            // Member found, return its type
            return member.type;
        }
    }

    // Member not found, return an invalid type or handle the error as needed
    return INVALID_TYPE;
}
*/
#endif

