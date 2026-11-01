# X-Macros in Metac

## Overview
This document explains the use of X-Macros (specifically those prefixed with `FOREACH_`) within this codebase.

## Why X-Macros?
We use X-Macros to adhere to the DRY (Don't Repeat Yourself) principle. This pattern allows us to define a single "source of truth" for a set of related data (like enum members, string representations, or function signatures) and then expand them into multiple different constructs.

This prevents bugs where an item is added to an enum but forgotten in a `switch` statement, a string conversion function, or a table initialization.

## The Pattern

A typical X-Macro implementation follows these steps:

1.  **Definition**: A `FOREACH_...` macro is defined. It takes a single argument, which is itself a macro (usually named `M`).
2.  **Expansion**: The `FOREACH_...` macro calls `M(...)` for every item in the list.

### Example

Suppose we have a list of error codes:

```c
// 1. The Definition
#define FOREACH_ERROR_CODE(M) \
    M(ERR_SUCCESS) \
    M(ERR_NOT_FOUND) \
    M(ERR_PERMISSION_DENIED)
```

We can then use this to generate an `enum`:

```c
// 2. Expansion for Enum
typedef enum error_code_t {
    #define DEFINE_ENUM(NAME) NAME,
    FOREACH_ERROR_CODE(DEFINE_ENUM)
    #undef DEFINE_ENUM
} error_code_t;
```

**What the preprocessor produces:**
```c
typedef enum error_code_t {
    ERR_SUCCESS,
    ERR_NOT_FOUND,
    ERR_PERMISSION_DENIED,
} error_code_t;
```

And the same list to generate a `switch` statement for string conversion:

```c
// 3. Expansion for String Conversion
const char* error_to_string(error_code_t err) {
    switch(err) {
        #define DEFINE_CASE(NAME) case NAME: return #NAME;
        FOREACH_ERROR_CODE(DEFINE_CASE)
        #undef DEFINE_CASE
        default: return "UNKNOWN_ERROR";
    }
}
```

**What the preprocessor produces:**
```c
const char* error_to_string(error_code_t err) {
    switch(err) {
        case ERR_SUCCESS: return "ERR_SUCCESS";
        case ERR_NOT_FOUND: return "ERR_NOT_FOUND";
        case ERR_PERMISSION_DENIED: return "ERR_PERMISSION_DENIED";
        default: return "UNKNOWN_ERROR";
    }
}
```

## Categories in This Codebase

### 1. Parser & Lexer
Used for defining tokens, expression kinds, and statement kinds. This ensures that when a new token or expression is added, the parser and the lexer stay in sync.
- **Common Files**: `parser/metac_lexer.h`, `parser/metac_node.h`
- **Key Macros**: `FOREACH_TOKEN`, `FOREACH_EXP`, `FOREACH_STMT_KIND`

### 2. Semantic Analysis
Used to manage semantic state, scopes, and error types.
- **Common Files**: `semantic/metac_scope.h`, `semantic/metac_semantic.h`, `semantic/metac_type.h`
- **Key Macros**: `FOREACH_SCOPE_OWNER`, `FOREACH_SEMA_STATE_ARRAY`, `FOREACH_TYPE_INDEX_KIND`

### 3. Type Table Implementation
Used to automate the generation of repetitive table management functions (like `Add` and `Get` functions for different table types).
- **Common Files**: `semantic/metac_type_table.h`
- **Key Macros**: `FOREACH_TABLE_MEMBER`

## Guidelines for Contributors

### Adding a New Entity
If you are adding a new set of constants (like a new category of tokens or expression kinds):
1.  **Find the existing `FOREACH_` macro** that governs that category.
2.  **Add your new item** to that macro definition.
3.  **Check all expansions**. Verify that you've also updated any `switch` statements, `enum` definitions, or table functions that use that macro.

### Creating a New X-Macro
If you find yourself repeating the same list of items in three or more different ways (e.g., an enum, a string array, and a validation function):
1.  **Define a new `FOREACH_...` macro**.
2.  **Use a descriptive name** (e.g., `FOREACH_MY_NEW_ENTITY`).
3.  **Document its expected argument structure** if it's non-standard (e.g., if it requires `M(name, value)` instead of just `M(name)`).

## Warnings
- **Debugging**: X-Macros can make compiler error messages harder to read because the error often points to the line where the macro is *expanded*, not where the item was *defined*.
- **Complexity**: Do not use X-Macros for everything. They should only be used when there is a clear, repetitive relationship between multiple code constructs.
