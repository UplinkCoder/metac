#include "../os/metac_alloc.h"
#include "../libinterpret/bc_common.h"
#include "../libinterpret/backend_interface_funcs.h"
#include "../libinterpret/bc_common.c"

#include "../libinterpret/bc_interpreter_backend.c"
#include "../libinterpret/printer_backend.c"

#ifdef _WIN32
#  define INT32_MAX 0x7fffffff
#  define INT32_MIN (~INT32_MAX)
#endif

#include "metac_vstore.c"

#include "metac_codegen.h"

#include <stdarg.h>

#ifdef METAC_COMPILER_INTERFACE
#include "../compiler_intrinsics/metac_compiler_interface.c"
#endif

uint32_t MetaCCodegen_GetTypeABISize(metac_bytecode_ctx_t* ctx,
                                     metac_type_index_t type)
{
    return 0;
}

BCType MetaCCodegen_GetBCType(metac_bytecode_ctx_t* ctx, metac_type_index_t typeIdx)
{
    BCType result = {BCTypeEnum_Undef};
    metac_type_index_kind_t typeKind = TYPE_INDEX_KIND(typeIdx);

    if (typeKind == type_index_basic)
    {
        switch(typeIdx.Index)
        {
            case type_void:
                result.type = BCTypeEnum_Void;
            break;
            case type_bool:
                result.type = BCTypeEnum_u32;
            break;
            case type_char:
                result.type = BCTypeEnum_i8;
            break;
            case type_short:
                result.type = BCTypeEnum_i16;
            break;
            case type_int:
                result.type = BCTypeEnum_i32;
            break;
            case type_long:
                result.type = BCTypeEnum_i64;
            break;
            case type_size_t:
                // TODO: we will likely want to vary what size_t is
                result.type = BCTypeEnum_u64;
            break;

            case type_float:
                result.type = BCTypeEnum_f23;
            break;
            case type_double:
                result.type = BCTypeEnum_f52;
            break;

            case type_long_long:
                result.type = BCTypeEnum_i64;
            break;
            case type_long_double:
                result.type = BCTypeEnum_f106;
            break;

            case type_unsigned_char:
                result.type = BCTypeEnum_u8;
            break;
            case type_unsigned_short:
                result.type = BCTypeEnum_u16;
            break;
            case type_unsigned_int:
                result.type = BCTypeEnum_u32;
            break;
            case type_unsigned_long:
                result.type = BCTypeEnum_u64;
            break;
            case type_unsigned_long_long:
                result.type = BCTypeEnum_u64;
            break;
            case type_type:
                // maybe type should become a BCTypeEnum_Ptr?
                result.type = BCTypeEnum_u32;
            break;
            default : assert(0);
        }
    }
    else if (typeKind == type_index_tuple)
    {
        result.type = BCTypeEnum_Tuple;
        result.typeIndex = typeIdx.Index;
    }
    else if (typeKind == type_index_functiontype)
    {
        result.type = BCTypeEnum_Function;
        result.typeIndex = typeIdx.Index;
    }
    else if (typeKind == type_index_struct)
    {
        result.type = BCTypeEnum_Struct;
        result.typeIndex = typeIdx.Index;
    }
    else if (typeKind == type_index_ptr)
    {
        result.type = BCTypeEnum_Ptr;
        result.typeIndex = typeIdx.Index;
    }
    else if (typeKind == type_index_enum)
    {
#if 0
        result.type = BCTypeEnum_Enum;
        result.typeIndex = typeIdx.Index;
#else
        result.type = BCTypeEnum_i32;
#endif
    }
    else
    {
        printf("cannot convert typekind: {%s, %u}\n",
            type_index_kind_toChars(TYPE_INDEX_KIND(typeIdx)),
                                    TYPE_INDEX_INDEX(typeIdx));
        assert(0);
    }

    return  result;
}
extern const BackendInterface Lightning_interface;
extern const BackendInterface BCGen_interface;

static metac_type_index_t GetTypeIndex(BCType bcType)
{
    metac_type_index_t result;
    metac_type_index_kind_t kind = type_index_invalid;

    switch (bcType.type)
    {
        case BCTypeEnum_Tuple:
            kind = type_index_tuple;
        break;
        case BCTypeEnum_Struct:
            kind = type_index_struct;
        break;

        case BCTypeEnum_Void:
            result.v = TYPE_INDEX_V(type_index_basic, type_void);
        break;
        case BCTypeEnum_i8:
            result.v = TYPE_INDEX_V(type_index_basic, type_char);
        break;
        case BCTypeEnum_i16:
            result.v = TYPE_INDEX_V(type_index_basic, type_short);
        break;
        case BCTypeEnum_i32:
            result.v = TYPE_INDEX_V(type_index_basic, type_int);
        break;
        case BCTypeEnum_i64:
            result.v = TYPE_INDEX_V(type_index_basic, type_long_long);
        break;

        case BCTypeEnum_f23:
            result.v = TYPE_INDEX_V(type_index_basic, type_float);
        break;
        case BCTypeEnum_f52:
            result.v = TYPE_INDEX_V(type_index_basic, type_double);
        break;

        case BCTypeEnum_f106:
            result.v = TYPE_INDEX_V(type_index_basic, type_long_double);
        break;

        case BCTypeEnum_u8:
            result.v = TYPE_INDEX_V(type_index_basic, type_unsigned_char);
        break;
        case BCTypeEnum_u16:
            result.v = TYPE_INDEX_V(type_index_basic, type_unsigned_short);
        break;
        case BCTypeEnum_u32:
            result.v = TYPE_INDEX_V(type_index_basic, type_unsigned_int);
        break;
        case BCTypeEnum_u64:
            result.v = TYPE_INDEX_V(type_index_basic, type_unsigned_long_long);
        break;

        default: assert(0);
    }

    if (kind != type_index_invalid)
        result.v = TYPE_INDEX_V(kind, bcType.typeIndex);

    return result;
}

uint32_t MetaCCodegen_GetStorageSize(metac_bytecode_ctx_t* ctx, BCType bcType)
{
    uint32_t sz = 0;
    if (BCType_isBasicBCType(bcType))
    {
        sz = BCTypeEnum_basicTypeSize(bcType.type);
    }
    else
    {
         metac_type_index_t typeIdx = GetTypeIndex(bcType);
         sz = MetaCSemantic_GetTypeSize(ctx->Sema, typeIdx);
    }

    assert(sz);
    return sz;
}

void MetaCCodegen_doType(metac_bytecode_ctx_t* ctx, metac_type_index_t typeIdx)
{

}

BCTypeInfo MetaCCodegen_GetTypeInfo(metac_bytecode_ctx_t* ctx, BCType* bcType)
{
    BCTypeInfo func = {
      BCTypeInfofKind_Function,
    };
    return func;
}

void MetaCCodegen_doGlobal(metac_bytecode_ctx_t* ctx, metac_sema_declaration_t* decl, uint32_t idx)
{
    BCValue result = {BCValueType_HeapValue};

    metac_type_index_t typeIdx = MetaCSemantic_GetType(ctx->Sema, METAC_NODE(decl));
    metac_type_index_kind_t typeIdxKind = TYPE_INDEX_KIND(typeIdx);

    result.heapAddr.addr = ctx->GlobalMemoryOffset;
    if (typeIdxKind != type_index_invalid)
    {
        BCType bcType = MetaCCodegen_GetBCType(ctx, typeIdx);
        uint32_t sz = MetaCCodegen_GetStorageSize(ctx, bcType);
        result.type = bcType;

        assert(result.vType == BCValueType_HeapValue);
        ARENA_ARRAY_ADD(ctx->Globals, result);
        assert(ctx->Globals[ctx->GlobalsCount - 1].vType == BCValueType_HeapValue);

        if (decl->Kind == decl_variable)
        {
            sema_decl_variable_t var = decl->sema_decl_variable;
            if (METAC_NODE(var.VarInitExpression) != emptyNode)
            {
                BCValue initVal;
                MetaCCodegen_doExpression(ctx, var.VarInitExpression, &initVal, _Rvalue);
            }
            ctx->GlobalMemoryOffset += sz;
        }

/*
        printf("offset = %d sz = %d\n", ctx->GlobalMemoryOffset, sz);
                ctx->GlobalMemoryOffset += align4(sz);
*/
        //printf("Doing da global\n");
        // if (decl.)
    }
    else
    {
        assert(0);
    }

}


long MetaCCodegen_RunFunction(metac_bytecode_ctx_t* self,
                              metac_bytecode_function_t f,
                              metac_alloc_t* interpAlloc,
                              BCHeap* heap,
                              const char* fargs, ...)
{
    va_list l;
    va_start(l, fargs);

    STACK_ARENA_ARRAY(BCValue, args, 8, interpAlloc);
    const BackendInterface gen = *self->gen;

    const char* farg = fargs;
    uint32_t nArgs = 0;
    if (fargs) for(char c = *farg++;c; c = *farg++)
    {
        switch(c)
        {
            case 'd' :
            {
                nArgs++;
                int32_t a = va_arg(l, int32_t);
                ARENA_ARRAY_ADD(args, imm32_(a, true));
            }
            break;
            case 'u':
            {
                nArgs++;
                uint32_t a = va_arg(l, uint32_t);
                ARENA_ARRAY_ADD(args, imm32(a));
            }
            break;
            case 's':
            {
                nArgs++;
                const char* a = va_arg(l, const char*);
                uint32_t sz = strlen(a);
                HeapAddr ptr = {heap->heapSize};
                heap->heapSize += align4(sz + 1);
                BCValue stringPtr;
                stringPtr.vType = BCValueType_Immediate;
                BCType bcType = {BCTypeEnum_Ptr, 10};
                stringPtr.type = bcType;
                stringPtr.heapAddr = ptr;
                ARENA_ARRAY_ADD(args, stringPtr);
            } break;
            default:
            {
                fprintf(stderr, "arg specifier, '%c' unsupported\n", c);
                assert(!"Value format unsupported");
            }

        }
    }
    va_end(l);

    BCValue result = gen.Run(self->c, f.FunctionIndex, args, nArgs, heap);
    return result.imm32.imm32;
}

void MetaCCodegen_End(metac_bytecode_ctx_t* self)
{

    self->gen->Finalize(self->c);

    if (self->gen == &Printer_interface)
    {
        Printer* printer = (Printer*)self->c;
        printf("%s\n\n", printer->BufferStart);
    }
}
void* MetaCCodegen_AllocMemory(metac_bytecode_ctx_t* self, uint32_t size, sema_decl_function_t* func)
{
    tagged_arena_t* arena = 0;
    arena_ptr_t arenaPtr;
    if (size == FREE_SIZE)
    {
        /*arena = MetaCCodegen_FindArena(self, cast(void*)func)*/
    }
    else
    {
        // printf("Allocating for %s \n", (func ? "function" : "startup"));
        arenaPtr = Allocate_(&self->Allocator, size, __FILE__, __LINE__, false);
        if (arenaPtr.Index == -1)
        {
            return 0;
        }

        arena = &self->Allocator.Arenas[arenaPtr.Index];
        return arena->Memory;
    }

    return 0;
}

void MetaCCodegen_Init(metac_bytecode_ctx_t* self, metac_alloc_t* parentAlloc)
{
    //TODO we might want a cheaper initialisation?
    static const metac_bytecode_ctx_t initValue = {0};
    tagged_arena_t* arena = 0;

    (*self) = initValue;
    //TODO take BC as a parameter
    // bc = &Lightning_interface;
#if BC_PRINTER
    self->gen = &Printer_interface;
#elif BC_LIGHTNING
    self->gen = &Lightning_interface;
#else
    self->gen = &BCGen_interface;
#endif

    const BackendInterface gen = *self->gen;

    Allocator_Init(&self->Allocator, parentAlloc, 0);

    arena_ptr_t arenaPtr =
        AllocateArena(&self->Allocator, gen.sizeof_instance());
    arena = &self->Allocator.Arenas[arenaPtr.Index];
    self->c = arena->Memory;

    if (gen.set_alloc_memory)
    {
        gen.set_alloc_memory(self->c, cast(alloc_fn_t)MetaCCodegen_AllocMemory, cast(void*)self);
    }

    if (gen.set_get_typeinfo)
    {
        gen.set_get_typeinfo(self->c, cast(get_typeinfo_fn_t)MetaCCodegen_GetTypeInfo, cast(void*)self);
    }

    gen.init_instance(self->c);

    gen.Initialize(self->c, 0);
    ARENA_ARRAY_INIT(metac_bytecode_function_t, self->Functions, &self->Allocator);

    ARENA_ARRAY_INIT(metac_external_entry_t, self->Externals, &self->Allocator);

    // ARENA_ARRAY_INIT(sema_decl_variable_t, self->Locals, &self->Allocator);
    ARENA_ARRAY_INIT(BCValue, self->Globals, &self->Allocator);
    self->GlobalMemoryOffset = 4;

#if METAC_COMPILER_INTERFACE
    metac_external_entry_t* extCompP = self->Externals + 0;
    extCompP->externalAddress = &compiler;
    extCompP->externalSize = sizeof(compiler);
    self->ExternalsCount = 1;
    compiler.Help = compiler_help;
#endif
}

void MetaCCodegen_Free(metac_bytecode_ctx_t* self)
{
    self->gen->fini_instance(self->c);
    // Allocator_Remove(self->Allocator)
    Debug_RemoveAllocator(g_DebugServer, &self->Allocator);
}

void MetaCCodegen_Begin(metac_bytecode_ctx_t* self, metac_identifier_table_t* idTable, metac_semantic_state_t* sema)
{
    assert(self->c != 0);
    self->IdentifierTable = idTable;
    self->Sema = sema;
    VariableStore_Init(&self->Vstore, idTable, &self->Allocator);
    ARENA_ARRAY_INIT(metac_bytecode_switch_t, self->SwitchStack, &self->Allocator);
}

static BCValue MetaCCodegen_doFunction(metac_bytecode_ctx_t* ctx,
                                       sema_decl_function_t* fn)
{
    metac_bytecode_function_t f = MetaCCodegen_GenerateFunction(ctx, fn);
    return imm32(f.FunctionIndex);
}

static void InitCompilerInterface(metac_bytecode_ctx_t* ctx)
{
#ifdef METAC_COMPILER_INTERFACE
    void* c = ctx->c;
    const BackendInterface gen = *ctx->gen;

    if (ctx->Sema->CompilerInterface)
    {
        BCType compilerInterfaceType;
        compilerInterfaceType.type = BCTypeEnum_Struct;
        compilerInterfaceType.typeIndex =
            ctx->Sema->CompilerInterface->TypeIndex.Index;

        ctx->CompilerInterfaceValue = gen.GenExternal(c, compilerInterfaceType, ".compiler");
        gen.MapExternal(c, &ctx->CompilerInterfaceValue, &compiler, sizeof(compiler));
        ctx->Externals[0].ExtValue = ctx->CompilerInterfaceValue;
    }
#endif
}

bool IsExternal(metac_sema_expression_t* expr)
{
    bool result = false;

    switch(expr->Kind)
    {
        case exp_variable:
            result = (STORAGE_KIND(expr->Variable->Storage) == storage_external);
        break;
        case exp_dot:
            result = IsExternal(expr->E1);
        break;
        case exp_function:
            result = false;
        break;
        default: assert(0);
    }

    return result;
}

BCValue
MetacCodegen_ExternalFunction(metac_bytecode_ctx_t* ctx, metac_sema_expression_t* func)
{
    void* c = ctx->c;
    const BackendInterface gen = *ctx->gen;
    BCType fType = MetaCCodegen_GetBCType(ctx, func->TypeIndex);
    BCValue extValue = gen.GenExternalFunc(c, fType, "externalFunc");
    BCValue funcPtr = gen.GenTemporary(c, fType);

    assert(TYPE_INDEX_KIND(func->TypeIndex) == type_index_functiontype);

    MetaCCodegen_doExpression(ctx, func, &funcPtr, _Rvalue);

    gen.MapExternalFunc(c, &extValue, &funcPtr);

    return extValue;
}


metac_bytecode_function_t MetaCCodegen_GenerateFunctionFromExp(metac_bytecode_ctx_t* ctx,
                                                               metac_sema_expression_t* expr)
{
    void* c = ctx->c;
    BCValue resultVal = {BCValueType_Unknown};

    metac_bytecode_function_t func;
    metac_bytecode_function_t calledF = {0};

    const BackendInterface gen = *ctx->gen;

    bool compilerInterfaceInited = false;

    if (expr->Kind == exp_call)
    {
        if(expr->E1->Kind == exp_function)
        {
            calledF = MetaCCodegen_GenerateFunction(ctx, expr->E1->Function);
        }
    }

    func.FunctionIndex =
        gen.BeginFunction(c, 0, "dummy_eval_func");

    if (!compilerInterfaceInited)
    {
        InitCompilerInterface(ctx);
    }

    // we need to introduce all resolvable variables from the outer context here.
    // .compiler.help - .compiler
    MetaCCodegen_doExpression(ctx, expr, &resultVal, _Rvalue);

    gen.Ret(c, &resultVal);

    gen.EndFunction(c, func.FunctionIndex);

#ifdef PRINT_BYTECODE
    if (ctx->gen == &BCGen_interface)
    {
        BCGen_PrintCode((BCGen*)c, 0, 48);
    }
#endif

    return func;
}

void MetaCCodegen_doStatement(metac_bytecode_ctx_t* ctx,
                              metac_sema_stmt_t* stmt);

metac_bytecode_function_t MetaCCodegen_GenerateFunction(metac_bytecode_ctx_t* ctx,
                                                        sema_decl_function_t* function)
{
    void* c = ctx->c;
    const BackendInterface gen = *ctx->gen;

    for(uint32_t i = 0; i < ctx->FunctionsCount; i++)
    {
        metac_bytecode_function_t* f = ctx->Functions + i;
        if (f->FuncDeclPtr == cast(void*) function)
        {
            return *f;
        }
    }

    metac_type_functiontype_t* functionType =
        FunctiontypePtr(ctx->Sema, TYPE_INDEX_INDEX(function->TypeIndex));
    uint32_t frameSize = 0;
    uint32_t functionParameterCount = functionType->ParameterTypeCount;

    STACK_ARENA_ARRAY(BCValue, parameters, 16, &ctx->Allocator)
    STACK_ARENA_ARRAY(BCValue, locals, 16, &ctx->Allocator)
    STACK_ARENA_ARRAY(BCAddr, breaks, 32, &ctx->Allocator)

    const char* fName =
        IdentifierPtrToCharPtr(ctx->IdentifierTable, function->Identifier);

    uint32_t functionId =
        gen.BeginFunction(c, 0, fName);

    // gen.Comment(c, "Function Begin.");

    metac_bytecode_function_t result;

    result.FunctionIndex = functionId;
    result.FuncDeclPtr = (void*) function;

    for(uint32_t i = 0;
        i < functionParameterCount;
        i++)
    {
        sema_decl_variable_t* paramVar = function->Parameters + i;
        BCType paramType = MetaCCodegen_GetBCType(ctx, paramVar->TypeIndex);
        uint32_t paramSize = MetaCCodegen_GetStorageSize(ctx, paramType);

        const char* paramName = IdentifierPtrToCharPtr(ctx->IdentifierTable,
                                                       paramVar->VarIdentifier);
        BCValue param = gen.GenParameter(c, paramType, paramName);
        ARENA_ARRAY_ADD(parameters, param);
        VariableStore_AddVariable(&ctx->Vstore, paramVar, &parameters[parametersCount - 1]);

        frameSize += paramSize;
    }

    assert(parametersCount == functionParameterCount);


#ifdef METAC_COMPILER_INTERFACE
    BCType compilerInterfaceType;

    if (ctx->Sema->CompilerInterface)
    {
        compilerInterfaceType.type = BCTypeEnum_Struct;
        compilerInterfaceType.typeIndex =
            ctx->Sema->CompilerInterface->TypeIndex.Index;

        ctx->CompilerInterfaceValue = gen.GenExternal(c, compilerInterfaceType, ".compiler");
        gen.MapExternal(c, &ctx->CompilerInterfaceValue, &compiler, sizeof(compiler));
        ctx->Externals[0].ExtValue = ctx->CompilerInterfaceValue;
        frameSize += sizeof(void*);
    }
#endif

    ctx->Locals = locals;
    ctx->LocalsArena = localsArena;
    ctx->LocalsAlloc = localsAlloc;
    ctx->LocalsCount = localsCount;

    ctx->Parameters = parameters;
    ctx->ParametersArena = parametersArena;
    ctx->ParametersAlloc = parametersAlloc;
    ctx->ParametersCount = parametersCount;

    ctx->Breaks = breaks;
    ctx->BreaksArena = breaksArena;
    ctx->BreaksAlloc = breaksAlloc;
    ctx->BreaksCount = breaksCount;

    for (uint32_t i = 0;
         i < function->FunctionBody->StatementCount;
         i++)
    {
        MetaCCodegen_doStatement(ctx, function->FunctionBody->Body[i]);
    }

    // gen.Comment(c, "Function body end");

    if (ctx->BreaksAlloc)
    {
        Allocator_FreeArena(&ctx->Allocator, ctx->BreaksArenaPtr);
    }
    if (ctx->LocalsAlloc)
    {
        Allocator_FreeArena(&ctx->Allocator, ctx->LocalsArenaPtr);
    }

    gen.EndFunction(c, result.FunctionIndex);
#ifdef PRINT_BYTECODE
    if (ctx->gen == &BCGen_interface)
    {
        BCGen_PrintCode((BCGen*)c, 0, 64);
    }
#endif
    ARENA_ARRAY_ADD(ctx->Functions, result);
    return result;
}


static bool IsUnaryExp(metac_expression_kind_t kind)
{
    switch(kind)
    {
        case exp_umin:
        case exp_not:
        case exp_compl:
        case exp_decrement:
        case exp_increment:
        case exp_post_decrement:
        case exp_post_increment:
        case exp_paren:
        case exp_cast:
        case exp_deref:
        case exp_addr:
        case exp_sizeof:
        case exp_typeof:
            return true;
        default:
            return false;
    }
}
/*
     storage_unknown = 0,

    storage_stack,
    storage_register,

    storage_thread_local,
    storage_task_local,

    storage_static,
    storage_static_thread_local,
    storage_static_task_local,


    storage_invalid = 0xE,
*/

// Returns wheter the access was indirected i.e. we need to
// store the result of the operation to the heap when we are done
static bool MetaCCodegen_AccessVariable(metac_bytecode_ctx_t* ctx,
                                        sema_decl_variable_t* var,
                                        BCValue* result,
                                        metac_value_type_t lValue)
{
    void* c = ctx->c;
    const BackendInterface gen = *ctx->gen;

    switch(var->Storage.Kind)
    {
        default:
            assert(0);

        case storage_external:
        {
            (*result) = ctx->Externals[var->Storage.Offset].ExtValue;
            return false;
        } break;
        case storage_parameter:
        {
            assert(ctx->ParametersCount >= var->Storage.Offset);
            (*result) = ctx->Parameters[var->Storage.Offset];
            return false;
        } break;
        case storage_local:
        {
            (*result) = ctx->Locals[var->Storage.Offset];
            return false;
        } break;
        case storage_global:
        {
            BCValue globalHeapRef = ctx->Globals[var->Storage.Offset];

            assert(globalHeapRef.vType == BCValueType_HeapValue);
            BCValue resTmp = gen.GenTemporary(c, globalHeapRef.type);

            resTmp.heapRef.vType = BCValueType_HeapValue;
            resTmp.heapRef.heapAddr = globalHeapRef.heapAddr;

            (*result) = resTmp;
            return true;
        } break;
    }
}

static void LoadFromHeapRef(metac_bytecode_ctx_t* ctx, BCValue* hrv, uint32_t abiSize)
{
    // import std.stdio; writeln("Calling LoadHeapRef from: ", line); //DEBUGLINE
    const BackendInterface gen = *ctx->gen;
    void* c = ctx->c;
    {
        BCTypeEnum types[] = {BCTypeEnum_u64, BCTypeEnum_i64, BCTypeEnum_f52};
        if(BCTypeEnum_anyOf(hrv->type.type, types, ARRAY_SIZE(types)))
        {
Lload64: {}
            BCValue hr = BCValue_fromHeapref(hrv->heapRef);
            gen.Load64(c, hrv, &hr);
            return ;
        }
    }

    {
        BCTypeEnum types[] = {
            BCTypeEnum_u8, BCTypeEnum_u16, BCTypeEnum_u32,
            BCTypeEnum_i8, BCTypeEnum_i16, BCTypeEnum_i32,
            BCTypeEnum_c8, BCTypeEnum_c16, BCTypeEnum_c32,
            BCTypeEnum_f23
        };
        if(BCTypeEnum_anyOf(hrv->type.type, types, ARRAY_SIZE(types)))
        {
Lload32: {}
            BCValue hr = BCValue_fromHeapref(hrv->heapRef);
            gen.Load32(c, hrv, &hr);
            return ;
        }
    }
    // since the stuff below are heapValues we may not want to do this ??
    {
        BCTypeEnum types[] =
            { BCTypeEnum_Struct, BCTypeEnum_Slice, BCTypeEnum_Array, BCTypeEnum_Tuple };
        if (BCTypeEnum_anyOf(hrv->type.type, types, ARRAY_SIZE(types)))
        {
            BCValue sz = imm32(abiSize);
            BCValue hrv_i32 = *hrv;
            BCValue hr = BCValue_fromHeapref(hrv->heapRef);
            hr.type = BCType_i32;
            hrv_i32.type = BCType_i32;

            gen.MemCpy(c, &hrv_i32, &hr, &sz);
        }
        else if (abiSize == 4)
        {
            goto Lload32;
        }
        else if (abiSize == 8)
        {
            goto Lload64;
        }
        else
        {
            assert(!"is not supported in LoadFromHeapRef");
        }
    }
}

static void StructMemberInit(void *c, BCValue* result, uint32_t offset, BCValue* initValue, uint32_t memberSz)
{
    assert(0);
}

static void StoreToHeapRef(metac_bytecode_ctx_t* ctx, BCValue* hrv, uint32_t abiSize)
{
    const BackendInterface gen = *ctx->gen;
    void* c = ctx->c;
    // import std.stdio; writeln("Calling LoadHeapRef from: ", line); //DEBUGLINE
    {
        BCTypeEnum types[] = {BCTypeEnum_i64, BCTypeEnum_f52};
        if(BCTypeEnum_anyOf(hrv->type.type, types, ARRAY_SIZE(types)))
        {
            BCValue hr = BCValue_fromHeapref(hrv->heapRef);
            gen.Store64(c, &hr, hrv);
            return ;
        }
    }

    {
        BCTypeEnum types[] = {
            BCTypeEnum_u8, BCTypeEnum_u16, BCTypeEnum_u32,
            BCTypeEnum_i8, BCTypeEnum_i16, BCTypeEnum_i32,
            BCTypeEnum_c8, BCTypeEnum_c16, BCTypeEnum_c32,
            BCTypeEnum_f23
        };

        if(BCTypeEnum_anyOf(hrv->type.type, types, ARRAY_SIZE(types)))
        {
            BCValue hr = BCValue_fromHeapref(hrv->heapRef);
            gen.Store32(c, &hr, hrv);
            return ;
        }
    }
    // since the stuff below are heapValues we may not want to do this ??
    {
        BCTypeEnum types[] =
            { BCTypeEnum_Struct, BCTypeEnum_Slice, BCTypeEnum_Array, BCTypeEnum_Tuple };
        if (BCTypeEnum_anyOf(hrv->type.type, types, ARRAY_SIZE(types)))
        {
            BCValue hr = BCValue_fromHeapref(hrv->heapRef);
            hr.type = BCType_i32;
            BCValue sz = imm32(abiSize);
            BCValue hrv_i32 = *hrv;
            hrv_i32.type = BCType_i32;
            gen.MemCpy(c, &hr, &hrv_i32, &sz);
        }
        else
        {
            assert(!"is not supported in StoreToHeapRef");
        }
    }
}

static void doArithExp(metac_bytecode_ctx_t* ctx,
                       metac_sema_expression_t* result,
                       metac_sema_expression_t* lhs,
                       metac_sema_expression_t* rhs)
{

}

static bool HasSideEffect(metac_sema_expression_t* exp)
{
    return exp->Kind != exp_signed_integer;
}

static BCValue PtrValue(metac_bytecode_ctx_t* ctx, void* ptrV)
{
    size_t value = (size_t)ptrV;

    if (sizeof(void*) == 4)
    {
        return imm32(value);
    }
    else if (sizeof(void*) == 8)
    {
        return imm64(value);
    }
}

static void MetaCCodegen_ComputeAddress(metac_bytecode_ctx_t* ctx,
                                        metac_sema_expression_t* exp,
                                        BCValue* result)
{
    sema_decl_variable_t* var = exp->Variable;
    assert(exp->Kind == exp_variable);
    switch(var->Storage.Kind)
    {
        case storage_external:
        {
            *result = PtrValue(ctx, ctx->Externals[var->Storage.Offset].externalAddress);
        } break;
        case storage_global:
        {
            *result = PtrValue(ctx, (char*)ctx->GlobalsArena.Memory + var->Storage.Offset);
        } break;
        default: assert(!"this type of storage is not supported");
    }

}

void MetaCCodegen_doDeref(metac_bytecode_ctx_t* ctx,
                          BCValue* addr,
                          metac_type_index_t varType,
                          BCValue* result)
{
    if (TYPE_INDEX_KIND(varType) == type_index_functiontype
     || TYPE_INDEX_KIND(varType) == type_index_ptr)
    {
        BCType bct = MetaCCodegen_GetBCType(ctx, varType);
        BCHeapRef heapRef;
        heapRef.vType = addr->vType;
        heapRef.localIndex = addr->localIndex;
        heapRef.stackAddr = addr->stackAddr;
        result->heapRef = heapRef;
        LoadFromHeapRef(ctx, result, sizeof(void*));
    }
    else if (TYPE_INDEX_KIND(varType) == type_index_basic)
    {
        BCType bct = MetaCCodegen_GetBCType(ctx, varType);
        BCHeapRef heapRef;
        heapRef.vType = addr->vType;
        heapRef.localIndex = addr->localIndex;
        heapRef.stackAddr = addr->stackAddr;
        result->heapRef = heapRef;
        LoadFromHeapRef(ctx, result, BCTypeEnum_basicTypeSize(bct.type));
    }
    else
    {
        assert(!"doDeref not implemented right now");
    }
}

static void MetaCCodegen_doCastExpression(metac_bytecode_ctx_t* ctx,
                                          metac_sema_expression_t* exp,
                                          BCValue* result)

{
    metac_type_index_t castToType = exp->CastType;
    metac_sema_expression_t* castExpr = exp->CastExp;
    metac_type_index_t castFromType = castExpr->TypeIndex;
    BackendInterface gen = *ctx->gen;
    void* c = ctx->c;

    BCValue rhs = { BCValueType_Unknown };
    MetaCCodegen_doExpression(ctx, castExpr, &rhs, _Rvalue);

    assert(exp->Kind == exp_cast);

    if (castToType.v == TYPE_INDEX_V(type_index_basic, type_float))
    {
        if (castFromType.v == TYPE_INDEX_V(type_index_basic, type_int))
        {
            if (rhs.vType == BCValueType_Immediate)
            {
                BCValue tmp = gen.GenTemporary(c, BCType_i32);
                gen.Set(c, &tmp, &rhs);
                rhs = tmp;
            }
            gen.IToF32(c, result, &rhs);
            return;
        }
    }
    else if (castToType.v == TYPE_INDEX_V(type_index_basic, type_int))
    {
        if (castFromType.v == TYPE_INDEX_V(type_index_basic, type_float))
        {
            if (rhs.vType == BCValueType_Immediate)
            {
                BCType BCType_f23 = {BCTypeEnum_f23};
                BCValue tmp = gen.GenTemporary(c, BCType_f23);
                gen.Set(c, &tmp, &rhs);
                rhs = tmp;
            }
            gen.F32ToI(c, result, &rhs);
            return;
        }
        else if (TYPE_INDEX_KIND(castFromType) == type_index_enum)
        {
            gen.Set(c, result, &rhs);
            return ;
        }
    }

    assert(!"Handling for exp_cast not implemented at the moment");
}

static void MetaCCodegen_doDotExpression(metac_bytecode_ctx_t* ctx,
                                         metac_sema_expression_t* exp,
                                         BCValue* result)
{
    const BackendInterface gen = *ctx->gen;
    void* c = ctx->c;

    metac_sema_expression_t* e1 = exp->E1;
    metac_sema_expression_t* e2 = exp->DotE2;

    metac_type_index_t expTypeIndex = e1->TypeIndex;
    metac_type_index_kind_t idxKind = TYPE_INDEX_KIND(expTypeIndex);
    metac_type_aggregate_field_t* field = 0;
    BCType addrType = {BCTypeEnum_Ptr};
    BCValue addr;
    BCValue e1Value = {BCValueType_Unknown};
    BCValue offsetVal = {BCValueType_Unknown};

    assert(exp->Kind == exp_dot);
    assert(idxKind == type_index_struct);
    assert(e2->Kind == exp_field || e2->Kind == exp_call);

    if (TYPE_INDEX_KIND(e2->TypeIndex) == type_index_functiontype)
    {
        BCType type_ = {BCTypeEnum_Function, TYPE_INDEX_INDEX(e2->TypeIndex)};
        addrType = type_;
    }

    addr = gen.GenTemporary(c, addrType);
    MetaCCodegen_doExpression(ctx, e1, &e1Value, _Rvalue);

    field = e2->Field;
    offsetVal = imm32(field->Offset);

    // HACK remove!
/*
    if (e1Value.vType == BCValueType_HeapValue || e1Value.vType == BCValueType_External)
    {
        e1Value.vType = BCValueType_StackValue;
    }
*/
    gen.Add3(c, &addr, &e1Value, &offsetVal);

    MetaCCodegen_doDeref(ctx, &addr, field->Type, result);
}

static void MetaCCodegen_doExpression(metac_bytecode_ctx_t* ctx,
                                      metac_sema_expression_t* exp,
                                      BCValue* result,
                                      metac_value_type_t lValue)
{
    const BackendInterface gen = *ctx->gen;
    void* c = ctx->c;
    bool doBinAss = false;
    BCValue lhs = { BCValueType_Unknown };
    BCValue rhs = { BCValueType_Unknown };

    metac_printer_t printer;
    MetaCPrinter_Init(&printer, ctx->Sema->ParserIdentifierTable, ctx->Sema->ParserStringTable);

    metac_expression_kind_t op = exp->Kind;
    BCType expType = MetaCCodegen_GetBCType(ctx, exp->TypeIndex);

    if (lValue == _Discard && !HasSideEffect(exp))
    {
        return ;
    }
    else if (lValue == _Cond && exp->Kind == exp_signed_integer)
    {
        int32_t truthval = exp->ValueI64 & 0xffffffff
                         | exp->ValueI64 << 32;
        BCValue val = imm32(truthval);
        gen.Set(c, result, &val);
        return ;
    }
    else
    {
        if (!result)
        {
            // XXX: this is super dangerous
            // FIXME remove stackref as soon as possible!
            BCValue tmp = {BCValueType_Unknown};
            result = &tmp;
        }

        if (op == exp_signed_integer)
        {
            (*result) = imm32_(cast(int32_t)exp->ValueI64, true);
            goto Lret;
        }
        else if (op == exp_char)
        {
            //TODO this is wrong!
            (*result) = imm32(exp->Chars[0]);
            goto Lret;
        }
        else if (op == exp_float)
        {
            (*result) = imm32(*(uint32_t*)&exp->ValueF23);
            result->type.type = BCTypeEnum_f23;
            goto Lret;
        }


        if (result->vType == BCValueType_Unknown)
        {
            (*result) = gen.GenTemporary(c, expType);
        }
    }

    if (IsBinaryAssignExp(op))
    {
        doBinAss = true;
        U32(op) -= (exp_add_ass - exp_add);
    }



    if (op == exp_assign)
    {
        MetaCCodegen_doExpression(ctx, exp->E1, result, _Lvalue);
        MetaCCodegen_doExpression(ctx, exp->E2, &rhs, _Rvalue);
    }
    else if (op == exp_addr)
    {
    }
    else if (op == exp_dot)
    {
    }
    else if (IsUnaryExp(op))
    {
        if (exp->E1->Kind == exp_signed_integer
           && (exp->E1->ValueI64 >= INT32_MIN && exp->E1->ValueI64 <= INT32_MAX))
        {
            lhs = imm32_(cast(int32_t)exp->E1->ValueI64, true);
        }
        else
        {
            bool opIsPostIncDec =
                (op == exp_post_decrement || op == exp_post_increment);
            lhs = gen.GenTemporary(c, expType);
            MetaCCodegen_doExpression(ctx, exp->E1, &lhs, (opIsPostIncDec ? _Lvalue : _Rvalue));
        }
    }
    else if (IsBinaryExp(op) && op != exp_comma)
    {
        if (!doBinAss && exp->E1->Kind != exp_signed_integer)
        {
            lhs = gen.GenTemporary(c, expType);
        }

        if (doBinAss)
        {
            MetaCCodegen_doExpression(ctx, exp->E1, result, _Lvalue);
            lhs = *result;
        }
        else
        {
            MetaCCodegen_doExpression(ctx, exp->E1, &lhs, _Rvalue);
        }

        if (exp->E2->Kind == exp_signed_integer
           && (exp->E2->ValueI64 >= INT32_MIN && exp->E2->ValueI64 <= INT32_MAX))
        {
            rhs = imm32_(cast(int32_t)exp->E2->ValueI64, true);
        }
        else
        {
            rhs = gen.GenTemporary(c, expType);
            MetaCCodegen_doExpression(ctx, exp->E2, &rhs, _Rvalue);
        }
    }

    switch(op)
    {
        case exp_dot_compiler:
        {
            printf("ignoring unprocessed .compiler expression\n");
        } break;
        default : {
            fprintf(stderr,
                "Evaluator doesn't know how to eval: %s\n",
                MetaCExpressionKind_toChars(exp->Kind)
            );
            assert(0);
        } break;

        case exp_unknown_value:
        {
            assert(0);
            BCValue unknown = gen.GenTemporary(c, expType);
            gen.Set(c, result, &unknown);
        } break;

        case exp_cast:
        {
            MetaCCodegen_doCastExpression(ctx, exp, result);
        } break;

        case exp_dot:
        {
            MetaCCodegen_doDotExpression(ctx, exp, result);
        } break;

        case exp_function:
        {
            *result = MetaCCodegen_doFunction(ctx, exp->Function);
        } break;

        case decl_enum_member:
        {
            metac_enum_member_t* enumMember = cast(metac_enum_member_t*) exp;
            MetaCCodegen_doExpression(ctx, enumMember->Value, result, _Rvalue);
        } break;

        case exp_ternary:
        {
            BCValue cond = gen.GenTemporary(c, BCType_i32);

            MetaCCodegen_doExpression(ctx, exp->E1, &lhs, _Rvalue);
            MetaCCodegen_doExpression(ctx, exp->E2, &rhs, _Rvalue);
            MetaCCodegen_doExpression(ctx, exp->Econd, &cond, _Cond);

            CndJmpBegin condExpJmp = gen.BeginCndJmp(c, &cond, false);
            gen.Set(c, result, &lhs);
            BCAddr toEnd = gen.BeginJmp(c);
            BCLabel falseBranch = gen.GenLabel(c);
            gen.Set(c, result, &rhs);
            BCLabel endLabel = gen.GenLabel(c);
            gen.EndJmp(c, toEnd, endLabel);
            gen.EndCndJmp(c, &condExpJmp, falseBranch);
        } break;

        case exp_comma:
        {
            metac_sema_expression_t* r = exp;
            while(r->Kind == exp_comma)
            {
                // skip the generation of integers we'll never see
                if (r->E1->Kind != exp_signed_integer)
                {
                    MetaCCodegen_doExpression(ctx, r->E1, 0, _Discard);
                }
                r = r->E2;
            }
            MetaCCodegen_doExpression(ctx, r, result, lValue);
        } break;

        case exp_addr:
        {
            MetaCCodegen_ComputeAddress(ctx, exp->E1, result);
        } break;

        case exp_string:
        {
            // this should not happen, we should have made it into a pointer I think
            assert(0);
        }
        case exp_assert:
        {
            /*
            BCValue errVal = imm32(0);
            BCValue cond = gen.GenTemporary(c, (BCType){BCTypeEnum_u32});
            WalkTree(c, &cond, e->E1, vstore);
            gen.Assert(c, &cond, &errVal);
             */
        } break;
        case exp_assign:
        {
            if (exp->E1->Kind == exp_tuple)
            {
                if (exp->E2->Kind == exp_tuple)
                {
                    //
                }
            }
            else if (exp->E1->Kind != exp_variable)
            {
                fprintf(stderr, "left hand side of assignment is not a variable but a %s ..."
                                " this should not happen; we are past semantic analysis\n", "");
                assert(0);
            }


            //metac_identifier_ptr_t idPtr = e->E1->Variable->VarIdentifier;
            //metac_identifier_ptr_t vStorePtr = GetVStoreID(vstore, e->E1);
            if (lValue != _Discard)
            {
                gen.Set(c, result, &rhs);
            }

            if (exp->E1->Variable->Storage.Kind == storage_global)
            {
                StoreToHeapRef(ctx, result, MetaCCodegen_GetTypeABISize(ctx, exp->E1->TypeIndex));
            }
            //gen.Set(c, result, &lhs);
        } break;

        case exp_tuple:
        {
            if (result)
            {
                metac_type_index_t typeIndex = exp->TypeIndex;
                // (*result) = gen.GenTemporary(c, MetaCCodegen_GetBCType(ctx, typeIndex));
                // result = TupleToValue(c, exp);
                STACK_ARENA_ARRAY(metac_type_index_t, types, 32, &ctx->Allocator);
                STACK_ARENA_ARRAY(uint32_t, offsets, 32, &ctx->Allocator);
                STACK_ARENA_ARRAY(BCType, bcTypes, 32, &ctx->Allocator);
                STACK_ARENA_ARRAY(BCValue, bcValues, 32, &ctx->Allocator);
                uint32_t currentOffset = 0;
                BCValue sz;


                for(uint32_t i = 0; i < exp->TupleExpressionCount; i++)
                {
                    metac_sema_expression_t te = *exp->TupleExpressions[i];
                    metac_type_index_t typeIdx = te.TypeIndex;
                    BCType bcType = MetaCCodegen_GetBCType(ctx, typeIdx);
                    BCValue bcValue = {BCValueType_Unknown};

                    MetaCCodegen_doExpression(ctx, exp->TupleExpressions[i], &bcValue, _Rvalue);
                    ARENA_ARRAY_ADD(types, typeIdx);
                    ARENA_ARRAY_ADD(offsets, currentOffset);
                    ARENA_ARRAY_ADD(bcTypes, bcType);
                    ARENA_ARRAY_ADD(bcValues, bcValue);

                    currentOffset += MetaCCodegen_GetStorageSize(ctx, bcType);
                }
                sz = imm32(currentOffset);

                gen.Alloc(c, result, &sz);
                BCValue address = gen.GenTemporary(c, BCType_i32);
                gen.Set(c, &address, result);
                for(uint32_t i = 0; i < exp->TupleExpressionCount; i++)
                {
                    metac_sema_expression_t te = *exp->TupleExpressions[i];
                    uint32_t memberSz = MetaCCodegen_GetStorageSize(ctx, bcTypes[i]);

                    if (te.Kind == exp_signed_integer)
                    {
                        BCValue val = imm32(cast(int32_t)te.ValueI64);
                        gen.Store32(c, &address, &val);
                    }
                    else if (te.TypeIndex.v == TYPE_INDEX_V(type_index_basic, type_int))
                    {
                        gen.Store32(c, &address, bcValues + i);
                    }
                    else if (te.TypeIndex.v == TYPE_INDEX_V(type_index_basic, type_type))
                    {
                        gen.Store32(c, &address, bcValues + i);
                    }
                    else if (TYPE_INDEX_KIND(te.TypeIndex) == type_index_struct)
                    {
                        gen.Store32(c, &address, bcValues + i);
                    }
                    else
                    {
                        StructMemberInit(c, result, offsets[i], bcValues + i, memberSz);
                    }

                    BCValue addToOffset = imm32(memberSz);
                    gen.Add3(c, &address, &address, &addToOffset);
                }

                ARENA_ARRAY_FREE(bcTypes);
                ARENA_ARRAY_FREE(types);
                ARENA_ARRAY_FREE(offsets);
                ARENA_ARRAY_FREE(bcValues);
            }
        } break;

        case exp_type:
        {
            BCValue imm = imm32(exp->TypeExp.v);
            gen.Set(c, result, &imm);
        } break;
/*
        case exp_char:
        {
            // BCValue imm = imm32()
        } break;
*/
        case exp_signed_integer:
        {
            // BCValue imm = imm32((int32_t)exp->ValueU64);
            // gen.Set(c, result, &imm);
            assert(!"this should have been taken care of already");
        } break;

        case exp_eq:
        {
            gen.Eq3(c, result, &lhs, &rhs);
        } break;

        case exp_neq:
        {
            gen.Neq3(c, result, &lhs, &rhs);
        } break;

        case exp_lt:
        {
            gen.Lt3(c, result, &lhs, &rhs);
        } break;

        case exp_le:
        {
            gen.Le3(c, result, &lhs, &rhs);
        } break;

        case exp_ge:
        {
            gen.Ge3(c, result, &lhs, &rhs);
        } break;

        case exp_gt:
        {
            gen.Gt3(c, result, &lhs, &rhs);
        } break;

        case exp_add:
        {
            gen.Add3(c, result, &lhs, &rhs);
        } break;
        case exp_sub:
        {
            gen.Sub3(c, result, &lhs, &rhs);
        } break;
        case exp_mul:
        {
            gen.Mul3(c, result, &lhs, &rhs);
        } break;
        case exp_div:
        {
            gen.Div3(c, result, &lhs, &rhs);
        } break;
        case exp_rem:
        {
            gen.Mod3(c, result, &lhs, &rhs);
        } break;
        case exp_andand:
        case exp_and:
        {
            gen.And3(c, result, &lhs, &rhs);
        } break;

        case exp_oror:
        case exp_or:
        {
            gen.Or3(c, result, &lhs, &rhs);
        } break;
        case exp_xor:
        {
            gen.Xor3(c, result, &lhs, &rhs);
        } break;
        case exp_lsh:
        {
            gen.Lsh3(c, result, &lhs, &rhs);
        } break;
        case exp_rsh:
        {
            gen.Rsh3(c, result, &lhs, &rhs);
        } break;
        case exp_identifier:
        {
            fprintf(stderr, "There have been unresolved identifiers ... this should not happen\n");
        } break;
        case exp_variable:
        {
            if (MetaCCodegen_AccessVariable(ctx, exp->Variable, result, lValue))
            {
                BCType bcType = MetaCCodegen_GetBCType(ctx, exp->Variable->TypeIndex);
                uint32_t sz = MetaCCodegen_GetStorageSize(ctx, bcType);
                LoadFromHeapRef(ctx, result, sz);
                // Info("Indirected access");
            }
        } break;
        case exp_paren:
        {
            MetaCCodegen_doExpression(ctx, exp->E1, result, lValue);
        } break;
        case exp_compl:
        {
            gen.Not(c, result, &lhs);
        } break;
        case exp_not:
        {
            BCValue zero = imm32(0);
            gen.Eq3(c, result, &lhs, &zero);
        } break;
        case exp_umin:
        {
            BCValue zero = imm32(0);
            gen.Sub3(c, result, &zero, &lhs);
        } break;

        case exp_post_increment:
        case exp_post_decrement:
        {
            gen.Set(c, result, &lhs);
            BCValue one = imm32(1);
            (op == exp_post_increment ? gen.Add3(c, &lhs, &lhs, &one)
                                      : gen.Sub3(c, &lhs, &lhs, &one));
        } break;

        case exp_increment:
        case exp_decrement:
        {
            BCValue one = imm32(1);
            (op == exp_increment ? gen.Add3(c, &lhs, &lhs, &one)
                                 : gen.Sub3(c, &lhs, &lhs, &one));
            gen.Set(c, result, &lhs);
        } break;

        case exp_call:
        {
            sema_exp_call_t call = exp->Call;
            bool isExternal = IsExternal(call.Function);

            BCValue resultVal =
                gen.GenTemporary(c, MetaCCodegen_GetBCType(ctx, exp->TypeIndex));

            if (call.Function->Kind == exp_function)
            {
                assert(call.Function->Function);
            }

            BCValue fn = {BCValueType_Unknown};

            STACK_ARENA_ARRAY(BCValue, args, 16, &ctx->Allocator);
            const static BCValue nullValue = {BCValueType_Unknown};

            // printf("argCount: %u\n", call.ArgumentCount);
            for(uint32_t i = 0; i < call.ArgumentCount; i++)
            {
                ARENA_ARRAY_ADD(args, nullValue);
                BCValue* argP = &args[i];
                MetaCCodegen_doExpression(ctx, call.Arguments[i], argP, _Rvalue);
            }
            if (IsExternal)
            {
                fn =
                    MetacCodegen_ExternalFunction(ctx, call.Function);
                gen.Call(c, &resultVal, &fn, args, call.ArgumentCount);
                // gen.ExternalCall(c, &resultVal, &fn, args, call.ArgumentCount);
            }
            else
            {
                MetaCCodegen_doExpression(ctx, call.Function, &fn, _Rvalue);

                gen.Call(c, &resultVal, &fn, args, call.ArgumentCount);
            }
            *result = resultVal;
        } break;
    }

    // binary assignment that use heap-ref values need that mirrored back
    if (doBinAss)
    {
        if (exp->E1->Variable->Storage.Kind == storage_global)
        {
            StoreToHeapRef(ctx, result, MetaCCodegen_GetTypeABISize(ctx, exp->E1->TypeIndex));
        }
    }

    if (rhs.vType == BCValueType_Temporary)
        gen.DestroyTemporary(c, &rhs);

    if (lhs.vType == BCValueType_Temporary)
        gen.DestroyTemporary(c, &lhs);
Lret: {}
}


static metac_bytecode_switch_t* MetaCCodegen_PushSwitch(metac_bytecode_ctx_t* ctx,
                                                        BCValue exp)
{
    metac_bytecode_switch_t swtch = {BCValueType_Unknown};
    swtch.Exp = exp;
    METAC_NODE(swtch.DefaultBody) = emptyNode;
    ARENA_ARRAY_INIT(metac_bytecode_casejmp_t, swtch.PrevCaseJumps, &ctx->Allocator);

    ARENA_ARRAY_ADD(ctx->SwitchStack, swtch);

    return ctx->SwitchStack + (ctx->SwitchStackCount - 1);
}

static void MetaCCodegen_PopSwitch(metac_bytecode_ctx_t* ctx, BCValue exp)
{
    assert(ctx->SwitchStackCount > 0);

    metac_bytecode_switch_t* swtch =
        ctx->SwitchStack + (ctx->SwitchStackCount - 1);
    assert(BCValue_eq(&exp, &swtch->Exp));
    //FreeArena(ctx->Sw)
    --ctx->SwitchStackCount;
}

static void FixupBreaks(metac_bytecode_ctx_t* ctx, uint32_t breakCount,
                        BCLabel breakLabel)
{
    const BackendInterface gen = *ctx->gen;
    void* c = ctx->c;

    if (ctx->BreaksCount > breakCount)
    {
        for(uint32_t i = breakCount;
            i < ctx->BreaksCount; i++)
        {
            gen.EndJmp(c, (BCAddr)ctx->Breaks[i], breakLabel);
        }
        ctx->BreaksCount = breakCount;
    }
}

static void MetaCCodegen_doBlockStmt(metac_bytecode_ctx_t* ctx,
                                     sema_stmt_block_t* stmt)
{
    assert(stmt->Kind == stmt_block);
    for(uint32_t i = 0; i < stmt->StatementCount; i++)
    {
        MetaCCodegen_doStatement(ctx, stmt->Body[i]);
    }
}

static inline void MetaCCodegen_doCaseStmt(metac_bytecode_ctx_t* ctx,
                                           sema_stmt_case_t* caseStmt)
{
    const BackendInterface gen = *ctx->gen;
    void* c = ctx->c;

    assert(ctx->SwitchStackCount > 0);

    metac_bytecode_switch_t* swtch = &ctx->SwitchStack[ctx->SwitchStackCount - 1];
    metac_sema_expression_t* caseExp = caseStmt->CaseExp;
    metac_sema_stmt_t*  caseBody = cast(metac_sema_stmt_t*)caseStmt->CaseBody;

    // if there's no exp it's the default case
    if (METAC_NODE(caseExp) == emptyNode)
    {
        // make sure we haven't yet seen a default
        assert(METAC_NODE(swtch->DefaultBody) == emptyNode);
        swtch->DefaultBody = caseBody;
        return ;
    }
    BCValue* switchExp = &swtch->Exp;
    BCValue caseExpr;
    MetaCCodegen_doExpression(ctx, caseExp, &caseExpr, _Lvalue);
    gen.Eq3(c, 0, switchExp, &caseExpr);

    bool hasBody =
        !(caseBody && caseBody->Kind == stmt_case);
    // if we have our own body we want to jump is the cnd is false
    // otherwise we want to jump if it's true
    CndJmpBegin cndJmp = gen.BeginCndJmp(c, 0, !hasBody);
    if (hasBody)
    {
        BCLabel caseLabel;
        const uint32_t caseCount = swtch->PrevCaseJumpsCount;

        if (caseCount > 0)
        {
            caseLabel = gen.GenLabel(c);
            for(uint32_t i = 0;
                i < caseCount;
                i++)
            {
                metac_bytecode_casejmp_t caseJmp = swtch->PrevCaseJumps[i];
                switch(caseJmp.Kind)
                {
                    case casejmp_condJmp:
                    {
                       gen.EndCndJmp(c, &caseJmp.cndJmp, caseLabel);
                    } break;
                    case casejmp_jmp:
                    {
                        gen.EndJmp(c, caseJmp.jmp, caseLabel);
                    } break;
                }
            }
            swtch->PrevCaseJumpsCount = 0;
        }
        if (caseStmt->CaseBody->Kind == stmt_casebody)
        {
            const uint32_t stmtCount =
                caseStmt->CaseBody->StatementCount;

            for(uint32_t i = 0;
                i < stmtCount;
                i++)
            {
                MetaCCodegen_doStatement(ctx, caseStmt->CaseBody->Statements[i]);
            }
        }
        else
        {
            MetaCCodegen_doStatement(ctx, cast(metac_sema_stmt_t*)caseStmt->CaseBody);
        }
        BCAddr nextCaseJmp = gen.BeginJmp(c);
        metac_bytecode_casejmp_t caseJmp;
        caseJmp.Kind = casejmp_jmp;
        caseJmp.jmp = nextCaseJmp;
        ARENA_ARRAY_ADD(swtch->PrevCaseJumps, caseJmp);

        gen.EndCndJmp(c, &cndJmp, gen.GenLabel(c));
    }
    else
    {
        metac_bytecode_casejmp_t caseJmp;
        caseJmp.Kind = casejmp_condJmp;
        caseJmp.cndJmp = cndJmp;

        ARENA_ARRAY_ADD(swtch->PrevCaseJumps, caseJmp);
        if (caseBody)
            MetaCCodegen_doStatement(ctx, caseBody);
    }
}

static inline bool IsEmpty(metac_stmt_t* stmt)
{
    return false;
}

void MetaCCodegen_doLocalVar(metac_bytecode_ctx_t* ctx,
                             sema_decl_variable_t* localVar)
{
    const BackendInterface gen = *ctx->gen;
    void* c = ctx->c;

    const char* localName = IdentifierPtrToCharPtr(ctx->IdentifierTable,
                                                   localVar->VarIdentifier);
    BCType localType = MetaCCodegen_GetBCType(ctx, localVar->TypeIndex);
    BCValue local = gen.GenLocal(c, localType, localName);
    ARENA_ARRAY_ADD(ctx->Locals, local);
    VariableStore_AddVariable(&ctx->Vstore, localVar, &ctx->Locals[(ctx->LocalsCount) - 1]);
    if (METAC_NODE(localVar->VarInitExpression) != emptyNode)
    {
        BCValue initVal;
        MetaCCodegen_doExpression(ctx, localVar->VarInitExpression, &initVal, _Rvalue);
        gen.Set(ctx->c, &local, &initVal);
    }
}

void MetaCCodegen_doStatement(metac_bytecode_ctx_t* ctx,
                              metac_sema_stmt_t* stmt)
{
    const BackendInterface gen = *ctx->gen;
    void* c = ctx->c;

    switch(stmt->Kind)
    {
        case stmt_switch:
        {
            uint32_t currentBreakCount = ctx->BreaksCount;

            sema_stmt_switch_t* switchStatement = cast(sema_stmt_switch_t*) stmt;
            BCValue switchExp;
            MetaCCodegen_doExpression(ctx, switchStatement->SwitchExp, &switchExp, _Rvalue);
            metac_bytecode_switch_t* swtch =
                MetaCCodegen_PushSwitch(ctx, switchExp);
            MetaCCodegen_doBlockStmt(ctx, switchStatement->SwitchBody);
            // gen default case if there is one.
            if (METAC_NODE(swtch->DefaultBody) != emptyPointer)
            {
                MetaCCodegen_doStatement(ctx, swtch->DefaultBody);
            }
            MetaCCodegen_PopSwitch(ctx, switchExp);
            BCLabel breakLabel = gen.GenLabel(c);
            FixupBreaks(ctx, currentBreakCount, breakLabel);
        } break;

        case stmt_block:
        {
            MetaCCodegen_doBlockStmt(ctx, cast(sema_stmt_block_t*)stmt);
        } break;

        case stmt_break:
        {
            ARENA_ARRAY_ADD(ctx->Breaks, gen.BeginJmp(c));
        } break;

        case stmt_decl:
        {
            sema_stmt_decl_t* declStmt = cast(sema_stmt_decl_t*) stmt;
            sema_decl_variable_t* localVar = (sema_decl_variable_t*) declStmt->Declaration;
            MetaCCodegen_doLocalVar(ctx, localVar);
            // MetaCCodegen_doStatement(ctx, decl)
        } break;

        case stmt_case:
        {
            sema_stmt_case_t* caseStmt = cast(sema_stmt_case_t*) stmt;
            assert(ctx->SwitchStackCount > 0);
            MetaCCodegen_doCaseStmt(ctx, caseStmt);
        } break;

        case stmt_return:
        {
            sema_stmt_return_t* returnStmt = cast(sema_stmt_return_t*) stmt;
            BCValue retVal = {BCValueType_Unknown};
            MetaCCodegen_doExpression(ctx, returnStmt->ReturnExp, &retVal, _Rvalue);
            gen.Ret(c, &retVal);
        } break;

        case stmt_exp:
        {
            sema_stmt_exp_t* expStmt = cast(sema_stmt_exp_t*) stmt;
            BCValue dontCare = {BCValueType_Unknown};
            MetaCCodegen_doExpression(ctx, expStmt->Expression, &dontCare, _Discard);
        } break;

        case stmt_if:
        {
            sema_stmt_if_t* ifStmt = cast(sema_stmt_if_t*) stmt;

            BCValue cond = {BCValueType_Unknown};
            MetaCCodegen_doExpression(ctx, ifStmt->IfCond, &cond, _Cond);
            CndJmpBegin cj = gen.BeginCndJmp(c, &cond, false);
            {
                MetaCCodegen_doStatement(ctx, ifStmt->IfBody);
            }
            BCAddr skipElse;
            if (METAC_NODE(ifStmt->ElseBody) != emptyNode)
                skipElse =  gen.BeginJmp(c);
            gen.EndCndJmp(c, &cj, gen.GenLabel(c));

            if (METAC_NODE(ifStmt->ElseBody) != emptyNode)
            {
                {
                    MetaCCodegen_doStatement(ctx, ifStmt->ElseBody);
                }
                gen.EndJmp(c, skipElse, gen.GenLabel(c));
            }
        } break;

        case stmt_do_while:
        {
            sema_stmt_while_t* whileStatement = cast(sema_stmt_while_t*) stmt;
            BCLabel beginLoop = gen.GenLabel(c);

            MetaCCodegen_doStatement(ctx, whileStatement->WhileBody);

            // BCLabel evalCond = gen.GenLabel(c);
            BCValue cond = {BCValueType_Unknown};
            MetaCCodegen_doExpression(ctx, whileStatement->WhileExp, &cond, _Cond);
            CndJmpBegin condExpJmp = gen.BeginCndJmp(c, &cond, true);
            gen.EndCndJmp(c, &condExpJmp, beginLoop);
        } break;

        case stmt_while:
        {
            sema_stmt_while_t* whileStatement = cast(sema_stmt_while_t*) stmt;
            BCLabel evalCond = gen.GenLabel(c);
            BCValue cond = {BCValueType_Unknown};
            MetaCCodegen_doExpression(ctx, whileStatement->WhileExp, &cond, _Cond);

            CndJmpBegin condExpJmp = gen.BeginCndJmp(c, &cond, false);
            MetaCCodegen_doStatement(ctx, whileStatement->WhileBody);
            gen.Jmp(c, evalCond);
            gen.EndCndJmp(c, &condExpJmp, gen.GenLabel(c));
        } break;

        case stmt_for:
        {
            sema_stmt_for_t* forStatement = cast(sema_stmt_for_t*) stmt;

            if (forStatement->ForInit != emptyNode)
            {
                if (IsExpressionNode(forStatement->ForInit->Kind))
                {
                    BCValue dontCare;
                    MetaCCodegen_doExpression(ctx,
                        (metac_sema_expression_t*)forStatement->ForInit, &dontCare, _Discard);
                }
                else
                {
                    MetaCCodegen_doLocalVar(ctx,
                        (sema_decl_variable_t*)forStatement->ForInit);
                }
            }

            CndJmpBegin cndJmpToCondEval;
            BCLabel loopBegin = gen.GenLabel(c);

            if (METAC_NODE(forStatement->ForCond) != emptyNode)
            {
                BCValue cond = {BCValueType_Unknown};
                MetaCCodegen_doExpression(ctx, forStatement->ForCond, &cond, _Cond);
                cndJmpToCondEval = gen.BeginCndJmp(c, &cond, false);
            }

            MetaCCodegen_doStatement(ctx, forStatement->ForBody);
            gen.Jmp(c, loopBegin);

            if (METAC_NODE(forStatement->ForCond) != emptyNode)
            {
                gen.EndCndJmp(c, &cndJmpToCondEval, gen.GenLabel(c));
            }
        } break;

        // (?) Maybe we want to actually output the comment in a Comment directive?
        case stmt_comment:
        {
            stmt_comment_t* comment = (stmt_comment_t*) stmt;
            gen.Comment(c, comment->Text);
        } break;

        default:
        {
            printf("Statement unsupported %s\n", StatementKind_toChars(stmt->Kind));
            assert(0);
        } break;
    }
}
