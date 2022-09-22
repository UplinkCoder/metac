#include "metac_alloc.h"
#include "libinterpret/bc_common.h"
#include "libinterpret/backend_interface_funcs.h"
#include "libinterpret/bc_common.c"
#include "libinterpret/bc_interpreter_backend.c"
#include "libinterpret/printer_backend.c"
#include  "metac_vstore.c"

#include "metac_codegen.h"

#include <stdarg.h>
uint32_t MetaCCodegen_GetTypeABISize(metac_bytecode_ctx_t* ctx, metac_type_index_t type)
{
    return 0;
}

void MetaCCodegen_doStatement(metac_bytecode_ctx_t* ctx,
                              metac_sema_statement_t* stmt);

BCType MetaCCodegen_GetBCType(metac_bytecode_ctx_t* ctx, metac_type_index_t type)
{
    BCType result = {0};

    if (type.Kind == type_index_enum)
        result.type = BCTypeEnum_i32;

    if (type.Kind == type_index_basic)
    {
        switch(type.Index)
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

    return  result;
}

extern const BackendInterface BCGen_interface;
const BackendInterface* bc;

uint32_t MetaCCodegen_GetStorageSize(metac_bytecode_ctx_t* ctx, BCType bcType)
{
    if (BCType_isBasicBCType(bcType))
    {
        return BCTypeEnum_basicTypeSize(bcType.type);
    }
}

void MetaCCodegen_doType(metac_bytecode_ctx_t* ctx, metac_type_index_t typeIdx)
{

}

void MetaCCodegen_doGlobal(metac_bytecode_ctx_t* ctx, metac_sema_declaration_t* decl, uint32_t idx)
{
    BCValue result = {BCValueType_HeapValue};
    result.heapAddr = (HeapAddr){ctx->GlobalMemoryOffset};

    metac_type_index_t typeIdx = MetaCSemantic_GetType(ctx->Sema, decl);
    if (typeIdx.Kind != type_index_invalid)
    {
        BCType bcType = MetaCCodegen_GetBCType(ctx, typeIdx);
        uint32_t sz = MetaCCodegen_GetStorageSize(ctx, bcType);
        result.type = bcType;

        ARENA_ARRAY_ADD(ctx->Globals, result);
        if (decl->Kind == decl_variable)
        {
            sema_decl_variable_t var = decl->sema_decl_variable;
            if (METAC_NODE(var.VarInitExpression) != emptyNode)
            {
                BCValue initVal;
                MetaCCodegen_doExpression(ctx, var.VarInitExpression, &initVal, _Rvalue);
            }
        }
        // printf("offset = %d sz = %d\n", ctx->GlobalMemoryOffset, sz);
        ctx->GlobalMemoryOffset += align4(sz);
        //printf("Doing da global\n");
        // if (decl.)
    }
    else
    {
        assert(0);
    }
    //ctx->GlobalMemoryOffset += MetaCCodegen_GetTypeABISize(ctx, decl->TypeIndex);

}


long MetaCCodegen_RunFunction(metac_bytecode_ctx_t* self,
                             metac_bytecode_function_t f,
                             metac_alloc_t* interpAlloc,
                             const char* fargs, ...)
{
    va_list l;
    va_start(l, fargs);
    STACK_ARENA_ARRAY(BCValue, args, 8, interpAlloc);

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
            default:
            {
                fprintf(stderr, "arg specifier, '%c' unsupported\n", c);
                assert(!"Value format unsupported");
            }

        }
    }
    va_end(l);

    BCValue result = bc->Run(self->c, f.FunctionIndex, args, nArgs);
    return result.imm32.imm32;
}

void MetaCCodegen_End(metac_bytecode_ctx_t* self)
{
    bc->Finalize(self->c);

    if (bc == &Printer_interface)
    {
        Printer* printer = (Printer*)self->c;
        printf("%s\n\n", printer->BufferStart);
    }

}

void MetaCCodegen_Init(metac_bytecode_ctx_t* self, metac_alloc_t* parentAlloc)
{
    //TODO take BC as a parameter
    if (bc == 0)
    {
#if BC_PRINTER
        bc = &Printer_interface;
#else
        bc = &BCGen_interface;
#endif
    }

    (*self) = (metac_bytecode_ctx_t) {};
    Allocator_Init(&self->Allocator, parentAlloc, 0);

#ifndef BC_PRINTER
    tagged_arena_t* arena =
        AllocateArena(&self->Allocator, bc->sizeof_instance());
    self->c = arena->Memory;
    bc->init_instance(self->c);
#else
    bc->new_instance(&self->c);
    Printer* printer = (Printer*) self->c;
#endif
    bc->Initialize(self->c, 0);

    ARENA_ARRAY_INIT(metac_bytecode_function_t, self->Functions, &self->Allocator);

    // ARENA_ARRAY_INIT(sema_decl_variable_t, self->Locals, &self->Allocator);
    ARENA_ARRAY_INIT(BCValue, self->Globals, &self->Allocator);
    self->GlobalMemoryOffset = 4;
}

void MetaCCodegen_Begin(metac_bytecode_ctx_t* self, metac_identifier_table_t* idTable, metac_semantic_state_t* sema)
{
    assert(self->c != 0);
    self->IdentifierTable = idTable;
    self->Sema = sema;
    VariableStore_Init(&self->Vstore, idTable);
    ARENA_ARRAY_INIT(metac_bytecode_switch_t, self->SwitchStack, &self->Allocator);
}

metac_bytecode_function_t MetaCCodegen_GenerateFunctionFromExp(metac_bytecode_ctx_t* ctx,
                                                               metac_sema_expression_t* expr)
{
    void* c = ctx->c;
    BCValue resultVal = {};

    metac_bytecode_function_t func;

    func.FunctionIndex =
        bc->BeginFunction(c, 0, "dummy_eval_func");

    // we need to introduce all resolvable variables from the outer context here.

    MetaCCodegen_doExpression(ctx, expr, &resultVal, 0);

    bc->Ret(c, &resultVal);

    bc->EndFunction(c, func.FunctionIndex);

    if (bc == &Printer_interface)
    {
        Printer* printer = (Printer*)ctx->c;
        printf("%s\n\n", printer->BufferStart);
    }

#ifdef PRINT_BYTECODE
    if (bc == &BCGen_interface)
    {
        BCGen_PrintCode(c, 0, 48);
    }
#endif

    return func;
}

metac_bytecode_function_t MetaCCodegen_GenerateFunction(metac_bytecode_ctx_t* ctx,
                                                        sema_decl_function_t* function)
{
    void* c = ctx->c;

    uint32_t frameSize = 0;
    uint32_t functionParameterCount = 1;
    STACK_ARENA_ARRAY(BCValue, parameters, 16, &ctx->Allocator);
    STACK_ARENA_ARRAY(BCValue, locals, 16, &ctx->Allocator);
    STACK_ARENA_ARRAY(BCAddr, breaks, 32, &ctx->Allocator);

    const char* fName =
        IdentifierPtrToCharPtr(ctx->IdentifierTable, function->Identifier);

    uint32_t functionId =
        bc->BeginFunction(c, 0, fName);

    bc->Comment(c, "Function Begin.");

    metac_bytecode_function_t result;
    result.FunctionIndex = functionId;

    for(uint32_t i = 0;
        i < functionParameterCount;
        i++)
    {
        sema_decl_variable_t* paramVar = function->Parameters + i;
        uint32_t paramSize = MetaCCodegen_GetTypeABISize(ctx, paramVar->TypeIndex);

        const char* paramName = IdentifierPtrToCharPtr(ctx->IdentifierTable,
                                                       paramVar->VarIdentifier);
        BCType paramType = MetaCCodegen_GetBCType(ctx, paramVar->TypeIndex);
        BCValue param = bc->GenParameter(c, paramType, paramName);
        ARENA_ARRAY_ADD(parameters, param);
        VariableStore_AddVariable(&ctx->Vstore, paramVar, &parameters[parametersCount - 1]);

        frameSize += paramSize;
    }
    assert(parametersCount == functionParameterCount);

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
    bc->Comment(c, "Function body end");

    if (ctx->BreaksAlloc)
    {
        FreeArena(&ctx->BreaksArena);
    }
    if (ctx->LocalsAlloc)
    {
        FreeArena(&ctx->LocalsArena);
    }

    bc->EndFunction(c, result.FunctionIndex);
#ifdef PRINT_BYTECODE
    if (bc == &BCGen_interface)
    {
        BCGen_PrintCode(c, 0, 600);
    }
#endif
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
        case exp_ptr:
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
    switch(var->Storage.Kind)
    {
        default:
            assert(0);

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
            BCValue resTmp = bc->GenTemporary(c, globalHeapRef.type);

            resTmp.heapRef.vType = BCValueType_HeapValue;
            resTmp.heapRef.heapAddr = globalHeapRef.heapAddr;

            (*result) = resTmp;
            return true;
        } break;
    }
}

static void LoadFromHeapRef(void* c, BCValue* hrv, uint32_t abiSize)
{
    // import std.stdio; writeln("Calling LoadHeapRef from: ", line); //DEBUGLINE
    {
        BCTypeEnum types[] = {BCTypeEnum_i64, BCTypeEnum_f52};
        if(BCTypeEnum_anyOf(hrv->type.type, types, ARRAY_SIZE(types)))
        {
            BCValue hr = BCValue_fromHeapref(hrv->heapRef);
            bc->Load64(c, hrv, &hr);
            return ;
        }
    }

    {
        BCTypeEnum types[] = {
            BCTypeEnum_i8, BCTypeEnum_i16, BCTypeEnum_i32,
            BCTypeEnum_c8, BCTypeEnum_c16, BCTypeEnum_c32,
            BCTypeEnum_f23
        };
        if(BCTypeEnum_anyOf(hrv->type.type, types, ARRAY_SIZE(types)))
        {
            BCValue hr = BCValue_fromHeapref(hrv->heapRef);
            bc->Load32(c, hrv, &hr);
            return ;
        }
    }
    // since the stuff below are heapValues we may not want to do this ??
    {
        BCTypeEnum types[] =
            { BCTypeEnum_Struct, BCTypeEnum_Slice, BCTypeEnum_Array };
        if (BCTypeEnum_anyOf(hrv->type.type, types, ARRAY_SIZE(types)))
        {
            BCValue sz = imm32(abiSize);
            BCValue hrv_i32 = *hrv;
            BCValue hr = BCValue_fromHeapref(hrv->heapRef);
            hr.type = BCType_i32;
            hrv_i32.type = BCType_i32;

            bc->MemCpy(c, &hrv_i32, &hr, &sz);
        }
        else
        {
            assert(!"is not supported in LoadFromHeapRef");
        }
    }
}


static void StoreToHeapRef(void* c, BCValue* hrv, uint32_t abiSize)
{
    // import std.stdio; writeln("Calling LoadHeapRef from: ", line); //DEBUGLINE
    {
        BCTypeEnum types[] = {BCTypeEnum_i64, BCTypeEnum_f52};
        if(BCTypeEnum_anyOf(hrv->type.type, types, ARRAY_SIZE(types)))
        {
            BCValue hr = BCValue_fromHeapref(hrv->heapRef);
            bc->Store64(c, &hr, hrv);
            return ;
        }
    }

    {
        BCTypeEnum types[] = {
            BCTypeEnum_i8, BCTypeEnum_i16, BCTypeEnum_i32,
            BCTypeEnum_c8, BCTypeEnum_c16, BCTypeEnum_c32,
            BCTypeEnum_f23
        };
        if(BCTypeEnum_anyOf(hrv->type.type, types, ARRAY_SIZE(types)))
        {
            BCValue hr = BCValue_fromHeapref(hrv->heapRef);
            bc->Store32(c, &hr, hrv);
            return ;
        }
    }
    // since the stuff below are heapValues we may not want to do this ??
    {
        BCTypeEnum types[] =
            { BCTypeEnum_Struct, BCTypeEnum_Slice, BCTypeEnum_Array };
        if (BCTypeEnum_anyOf(hrv->type.type, types, ARRAY_SIZE(types)))
        {
            BCValue hr = BCValue_fromHeapref(hrv->heapRef);
            hr.type = BCType_i32;
            BCValue sz = imm32(abiSize);
            BCValue hrv_i32 = *hrv;
            hrv_i32.type = BCType_i32;
            bc->MemCpy(c, &hr, &hrv_i32, &sz);
        }
        else
        {
            assert(!"is not supported in StoreToHeapRef");
        }
    }
}


static void MetaCCodegen_doExpression(metac_bytecode_ctx_t* ctx,
                                      metac_sema_expression_t* exp,
                                      BCValue* result,
                                      metac_value_type_t lValue)
{
    metac_printer_t printer;
    MetaCPrinter_Init(&printer, ctx->IdentifierTable, 0);

    void* c = ctx->c;

    metac_expression_kind_t op = exp->Kind;
    BCType expType = MetaCCodegen_GetBCType(ctx, exp->TypeIndex);

    if (lValue != _Discard)
    {
        if (!result)
        {
            // XXX: this is super dangerous
            // FIXME remove stackref as soon as possible!
            BCValue tmp;
            result = &tmp;
        }

        if (op == exp_signed_integer)
        {
                (*result) = imm32(cast(int32_t)exp->ValueI64);
            goto Lret;
        }


        if (result->vType == BCValueType_Unknown)
        {
            (*result) = bc->GenTemporary(c, expType);
        }
    }

    bool doBinAss = false;
    if (IsBinaryAssignExp(op))
    {
        doBinAss = true;
        op -= (exp_add_ass - exp_add);
    }

    BCValue lhs;
    BCValue rhs;


    if (op == exp_assign)
    {
        MetaCCodegen_doExpression(ctx, exp->E1, result, _Lvalue);
        MetaCCodegen_doExpression(ctx, exp->E2, &rhs, _Rvalue);
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
            lhs = bc->GenTemporary(c, expType);
            MetaCCodegen_doExpression(ctx, exp->E1, &lhs, (opIsPostIncDec ? _Lvalue : _Rvalue));
        }
    }
    else if (IsBinaryExp(op))
    {
        if (!doBinAss)
            lhs = bc->GenTemporary(c, expType);
        MetaCCodegen_doExpression(ctx, exp->E1, (doBinAss ? result : &lhs), (doBinAss ? _Lvalue: _Rvalue));
        if (doBinAss)
            lhs = *result;

        if (exp->E2->Kind == exp_signed_integer
           && (exp->E2->ValueI64 >= INT32_MIN && exp->E2->ValueI64 <= INT32_MAX))
        {
            rhs = imm32_(cast(int32_t)exp->E2->ValueI64, true);
        }
        else
        {
            rhs = bc->GenTemporary(c, expType);
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

        case decl_enum_member:
        {
            metac_enum_member_t* enumMember = cast(metac_enum_member_t*) exp;
            MetaCCodegen_doExpression(ctx, enumMember->Value, result, _Rvalue);
        } break;

        case exp_comma:
        {
            metac_sema_expression_t* r = exp;
            while(r->Kind == exp_comma)
            {
                if (r->E1->Kind != exp_signed_integer)
                {
                    // skip the generation of integers we'll never see
                    MetaCCodegen_doExpression(ctx, r->E1, 0, lValue);
                }
                r = r->E2;
            }
            MetaCCodegen_doExpression(ctx, r, result, lValue);
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
            BCValue cond = bc->GenTemporary(c, (BCType){BCTypeEnum_u32});
            WalkTree(c, &cond, e->E1, vstore);
            bc->Assert(c, &cond, &errVal);
             */
        } break;
        case exp_assign:
        {
            if (exp->E1->Kind != exp_variable)
            {
                fprintf(stderr, "left hand side of assignment is not a variable but a %s ..."
                                " this should not happen; we are past semantic analysis\n", "");
                assert(0);
            }


            //metac_identifier_ptr_t idPtr = e->E1->Variable->VarIdentifier;
            //metac_identifier_ptr_t vStorePtr = GetVStoreID(vstore, e->E1);
            if (lValue != _Discard)
            {
                bc->Set(c, result, &rhs);
            }

            if (exp->E1->Variable->Storage.Kind == storage_global)
            {
                StoreToHeapRef(c, result, MetaCCodegen_GetTypeABISize(ctx, exp->E1->TypeIndex));
            }
            //bc->Set(c, result, &lhs);
        } break;

        case exp_tuple:
        {
            // result = TupleToValue(c, exp);
            assert(0);
        } break;

        case exp_type:
        {
            BCValue imm = imm32(exp->TypeExp.v);
            bc->Set(c, result, &imm);
        } break;

        case exp_signed_integer:
        {
            // BCValue imm = imm32((int32_t)exp->ValueU64);
            // bc->Set(c, result, &imm);
            assert(!"this should have been taken care of already");
        } break;

        case exp_eq:
        {
            bc->Eq3(c, result, &lhs, &rhs);
        } break;

        case exp_neq:
        {
            bc->Neq3(c, result, &lhs, &rhs);
        } break;

        case exp_lt:
        {
            bc->Lt3(c, result, &lhs, &rhs);
        } break;

        case exp_le:
        {
            bc->Le3(c, result, &lhs, &rhs);
        } break;

        case exp_ge:
        {
            bc->Ge3(c, result, &lhs, &rhs);
        } break;

        case exp_gt:
        {
            bc->Gt3(c, result, &lhs, &rhs);
        } break;

        case exp_add:
        {
            bc->Add3(c, result, &lhs, &rhs);
        } break;
        case exp_sub:
        {
            bc->Sub3(c, result, &lhs, &rhs);
        } break;
        case exp_mul:
        {
            bc->Mul3(c, result, &lhs, &rhs);
        } break;
        case exp_div:
        {
            bc->Div3(c, result, &lhs, &rhs);
        } break;
        case exp_rem:
        {
            bc->Mod3(c, result, &lhs, &rhs);
        } break;
        case exp_andand:
        case exp_and:
        {
            bc->And3(c, result, &lhs, &rhs);
        } break;

        case exp_oror:
        case exp_or:
        {
            bc->Or3(c, result, &lhs, &rhs);
        } break;
        case exp_xor:
        {
            bc->Xor3(c, result, &lhs, &rhs);
        } break;
        case exp_identifier:
        {
            fprintf(stderr, "There have been unresolved identifiers ... this should not happen\n");
        } break;
        case exp_variable:
        {
            if (MetaCCodegen_AccessVariable(ctx, exp->Variable, result, lValue))
            {
                LoadFromHeapRef(ctx->c, result, MetaCCodegen_GetTypeABISize(ctx, exp->TypeIndex));
                // Info("Indirected access");
            }
        } break;
        case exp_paren:
        {
            MetaCCodegen_doExpression(ctx, exp->E1, result, lValue);
        } break;
        case exp_compl:
        {
            bc->Not(c, result, &lhs);
        } break;
        case exp_not:
        {
            BCValue zero = imm32(0);
            bc->Eq3(c, result, &lhs, &zero);
        } break;
        case exp_umin:
        {
            BCValue zero = imm32(0);
            bc->Sub3(c, result, &zero, &lhs);
        } break;

        case exp_post_increment:
        case exp_post_decrement:
        {
            bc->Set(c, result, &lhs);
            BCValue one = imm32(1);
            (op == exp_post_increment ? bc->Sub3(c, &lhs, &lhs, &one)
                                      : bc->Add3(c, &lhs, &lhs, &one));
        } break;

        case exp_increment:
        case exp_decrement:
        {
            BCValue one = imm32(1);
            (op == exp_increment ? bc->Sub3(c, &lhs, &lhs, &one)
                                 : bc->Add3(c, &lhs, &lhs, &one));
            bc->Set(c, result, &lhs);
        } break;

        case exp_call:
        {
            sema_exp_call_t call = exp->Call;
            BCValue fn = bc->GenTemporary(c, MetaCCodegen_GetBCType(ctx, exp->TypeIndex));

            assert(call.Function->Kind == exp_function);
            assert(call.Function->Function);
            STACK_ARENA_ARRAY(BCValue, args, 16, &ctx->Allocator);
            const static BCValue nullValue = {BCValueType_Unknown};

            for(uint32_t i = 0; i < call.ArgumentCount; i++)
            {
                ARENA_ARRAY_ADD(args, nullValue);
                BCValue* argP = &args[i];
                MetaCCodegen_doExpression(ctx, call.Arguments[i], argP, _Rvalue);
            }
            bc->Call(c, result, &fn, args, call.ArgumentCount);
        } break;
    }

    if (rhs.vType == BCValueType_Temporary)
        bc->DestroyTemporary(c, &rhs);

    if (lhs.vType == BCValueType_Temporary)
        bc->DestroyTemporary(c, &lhs);
Lret: {}

}


static metac_bytecode_switch_t* MetaCCodegen_PushSwitch(metac_bytecode_ctx_t* ctx,
                                                        BCValue exp)
{
    metac_bytecode_switch_t swtch = {0};
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
    void* c = ctx->c;
    if (ctx->BreaksCount > breakCount)
    {
        for(uint32_t i = breakCount;
            i < ctx->BreaksCount; i++)
        {
            bc->EndJmp(c, (BCAddr)ctx->Breaks[i], breakLabel);
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
    void* c = ctx->c;
    assert(ctx->SwitchStackCount > 0);

    metac_bytecode_switch_t* swtch = &ctx->SwitchStack[ctx->SwitchStackCount - 1];
    metac_sema_expression_t* caseExp = caseStmt->CaseExp;
    metac_sema_statement_t*  caseBody = cast(metac_sema_statement_t*)caseStmt->CaseBody;

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
    bc->Eq3(c, 0, switchExp, &caseExpr);

    bool hasBody =
        !(caseBody && caseBody->Kind == stmt_case);
    // if we have our own body we want to jump is the cnd is false
    // otherwise we want to jump if it's true
    CndJmpBegin cndJmp = bc->BeginCndJmp(c, 0, !hasBody);
    if (hasBody)
    {
        BCLabel caseLabel;
        const uint32_t caseCount = swtch->PrevCaseJumpsCount;

        if (caseCount > 0)
        {
            caseLabel = bc->GenLabel(c);
            for(uint32_t i = 0;
                i < caseCount;
                i++)
            {
                metac_bytecode_casejmp_t caseJmp = swtch->PrevCaseJumps[i];
                switch(caseJmp.Kind)
                {
                    case casejmp_condJmp:
                    {
                       bc->EndCndJmp(c, &caseJmp.cndJmp, caseLabel);
                    } break;
                    case casejmp_jmp:
                    {
                        bc->EndJmp(c, caseJmp.jmp, caseLabel);
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
            MetaCCodegen_doStatement(ctx, cast(metac_sema_statement_t*)caseStmt->CaseBody);
        }
        BCAddr nextCaseJmp = bc->BeginJmp(c);
        metac_bytecode_casejmp_t caseJmp;
        caseJmp.Kind = casejmp_jmp;
        caseJmp.jmp = nextCaseJmp;
        ARENA_ARRAY_ADD(swtch->PrevCaseJumps, caseJmp);

        bc->EndCndJmp(c, &cndJmp, bc->GenLabel(c));
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

static inline bool IsEmpty(metac_statement_t* stmt)
{
    return false;
}

void MetaCCodegen_doLocalVar(metac_bytecode_ctx_t* ctx,
                             sema_decl_variable_t* localVar)
{
    const char* localName = IdentifierPtrToCharPtr(ctx->IdentifierTable,
                                                   localVar->VarIdentifier);
    BCType localType = MetaCCodegen_GetBCType(ctx, localVar->TypeIndex);
    BCValue local = bc->GenLocal(ctx->c, localType, localName);
    ARENA_ARRAY_ADD(ctx->Locals, local);
    VariableStore_AddVariable(&ctx->Vstore, localVar, &ctx->Locals[(ctx->LocalsCount) - 1]);
    if (METAC_NODE(localVar->VarInitExpression) != emptyNode)
    {
        BCValue initVal;
        MetaCCodegen_doExpression(ctx, localVar->VarInitExpression, &initVal, _Rvalue);
        bc->Set(ctx->c, &local, &initVal);
    }
}

void MetaCCodegen_doStatement(metac_bytecode_ctx_t* ctx,
                              metac_sema_statement_t* stmt)
{
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
            BCLabel breakLabel = bc->GenLabel(c);
            FixupBreaks(ctx, currentBreakCount, breakLabel);
        } break;

        case stmt_block:
        {
            MetaCCodegen_doBlockStmt(ctx, cast(sema_stmt_block_t*)stmt);
        } break;

        case stmt_break:
        {
            ARENA_ARRAY_ADD(ctx->Breaks, bc->BeginJmp(c));
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
            BCValue retVal = {0};
            MetaCCodegen_doExpression(ctx, returnStmt->ReturnExp, &retVal, _Rvalue);
            bc->Ret(c, &retVal);
        } break;

        case stmt_exp:
        {
            sema_stmt_exp_t* expStmt = cast(sema_stmt_exp_t*) stmt;
            BCValue dontCare = {0};
            MetaCCodegen_doExpression(ctx, expStmt->Expression, &dontCare, _Discard);
        } break;

        case stmt_if:
        {
            sema_stmt_if_t* ifStmt = cast(sema_stmt_if_t*) stmt;

            BCValue cond = {0};
            MetaCCodegen_doExpression(ctx, ifStmt->IfCond, &cond, _Cond);
            CndJmpBegin cj = bc->BeginCndJmp(c, &cond, false);
            {
                MetaCCodegen_doStatement(ctx, ifStmt->IfBody);
            }
            BCAddr skipElse;
            if (METAC_NODE(ifStmt->ElseBody) != emptyNode)
                skipElse =  bc->BeginJmp(c);
            bc->EndCndJmp(c, &cj, bc->GenLabel(c));

            if (METAC_NODE(ifStmt->ElseBody) != emptyNode)
            {
                {
                    MetaCCodegen_doStatement(ctx, ifStmt->ElseBody);
                }
                bc->EndJmp(c, skipElse, bc->GenLabel(c));
            }
        } break;

        case stmt_do_while:
        {
            sema_stmt_while_t* whileStatement = cast(sema_stmt_while_t*) stmt;
            BCLabel beginLoop = bc->GenLabel(c);

            MetaCCodegen_doStatement(ctx, whileStatement->WhileBody);

            // BCLabel evalCond = bc->GenLabel(c);
            BCValue cond = {0};
            MetaCCodegen_doExpression(ctx, whileStatement->WhileExp, &cond, _Cond);
            CndJmpBegin condExpJmp = bc->BeginCndJmp(c, &cond, true);
            bc->EndCndJmp(c, &condExpJmp, beginLoop);
        } break;

        case stmt_while:
        {
            sema_stmt_while_t* whileStatement = cast(sema_stmt_while_t*) stmt;
            BCLabel evalCond = bc->GenLabel(c);
            BCValue cond = {0};
            MetaCCodegen_doExpression(ctx, whileStatement->WhileExp, &cond, _Cond);

            CndJmpBegin condExpJmp = bc->BeginCndJmp(c, &cond, false);
            MetaCCodegen_doStatement(ctx, whileStatement->WhileBody);
            bc->Jmp(c, evalCond);
            bc->EndCndJmp(c, &condExpJmp, bc->GenLabel(c));
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
            BCLabel loopBegin = bc->GenLabel(c);

            if (METAC_NODE(forStatement->ForCond) != emptyNode)
            {
                BCValue cond = {0};
                MetaCCodegen_doExpression(ctx, forStatement->ForCond, &cond, _Cond);
                cndJmpToCondEval = bc->BeginCndJmp(c, &cond, false);
            }

            MetaCCodegen_doStatement(ctx, forStatement->ForBody);
            bc->Jmp(c, loopBegin);

            if (METAC_NODE(forStatement->ForCond) != emptyNode)
            {
                bc->EndCndJmp(c, &cndJmpToCondEval, bc->GenLabel(c));
            }
        } break;

        // (?) Maybe we want to actually output the comment in a Comment directive?
        case stmt_comment:
        {
        } break;

        default:
        {
            printf("Statement unsupported %s\n", StatementKind_toChars(stmt->Kind));
            assert(0);
        } break;
    }
}
