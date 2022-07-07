#if defined(__TINYC__) && defined(__aarch64__)
acosw:

.long 0xaa1e03e2  //    mov x2, x30
.long 0xa9004410  //    stp x16, x17, [x0]
.long 0xa9015013  //    stp x19, x20, [x0, #16]
.long 0xa9025815  //    stp x21, x22, [x0, #32]
.long 0xa9036017 //     stp x23, x24, [x0, #48]);
.long 0xa9046819 //     x25, x26, [x0, #64]
.long 0xa905701b //     stp x27, x28, [x0, #80]
.long 0xa906781d //     stp x29, x30, [x0, #96]
.long 0x910003e5 //     mov x5, sp
.long 0xf9003805 //     str x5, [x0, #112]
.long 0xd53b4405 //     mrs x5, fpcr
.long 0xf9003c25 //     str x5, [x1, #120]
.long 0xa9404430 //     ldp x16, x17, [x1]
.long 0xa9415033 //     ldp x19, x20, [x1, #16]
.long 0xa9425835 //     ldp x21, x22, [x1, #32]
.long 0xa9436037 //     ldp x23, x24, [x1, #48]
.long 0xa9446839//  ldp x25, x26, [x1, #64]
.long 0xa945703b //     ldp x27, x28, [x1, #80]
.long 0xa946783d//  ldp x29, x30, [x1, #96]
.long 0xf9403823 //     ldr x3, [x1, #112]
.long 0x9100007f //     mov sp, x3
.long 0xf9403c23 //     ldr x3, [x1, #120]
.long 0xd51b4403 //     msr fpcr, x3
.long 0xd61f03c0 //     br  x30

// ------------------------------------------------------------------------------
aco_save_fpucw_mxcsr:

.long 0xd53b4401 //  mrs x1, fpcr
.long 0xf9000001 //  str x1, [x0]
.long 0xd65f03c0 //  ret

// --------------------------------------------------------------------------------
aco_funcp_protector_asm:

.long 0x910003e9 //  mov x9, sp
.long 0x927ced29 //  and x9, x9, #0xfffffffffffffff0
.long 0x9100013f //  mov sp, x9
.long 0x94000000 //  bl  0 <aco_funcp_protector>
.long 0x94000000 //  bl  0 <abort>
.long 0xd65f03c0 //  ret

#else
#ifndef NO_FIBERS
.text
.globl acosw
#if defined(__APPLE__)
#else
.type  acosw, @function
#endif
#ifndef __aarch64__
.intel_syntax noprefix
#endif
acosw:
/*
    extern void acosw(aco_t* from_co, aco_t* to_co);

    struct aco_t {
        void*  reg[X];
        // ...
    }

    reference:
        https://github.com/hjl-tools/x86-psABI/wiki/X86-psABI

    pitfall:
        http://man7.org/linux/man-pages/man7/signal.7.html
        http://man7.org/linux/man-pages/man2/sigaltstack.2.html

        > $ man 7 signal
        > ...
        > By default, the signal handler is invoked on the normal process 
        > stack.   It  is  possible  to arrange that the signal handler 
        > uses an alternate stack; see sigaltstack(2) for a discussion of 
        > how to do this and when it might be useful.
        > ...

        This is a BUG example:
            https://github.com/Tencent/libco/blob/v1.0/coctx_swap.S#L27

    proof of correctness:
        https://github.com/hnes/libaco

    mxcsr & fpu:
        fnstcw * m2byte
            Store FPU control word to m2byte without checking for 
            pending unmasked floating-point exceptions.

        fldcw m2byte
            Load FPU control word from m2byte.

        stmxcsr m32
            Store contents of MXCSR register to m32

        ldmxcsr m32
            Load MXCSR register from m32.
*/
/*
    0x00             -->               0xff
    eip esp ebp edi esi ebx fpucw16 mxcsr32
    0   4   8   c   10  14  18      1c
*/
#ifdef __i386__
    mov     eax,DWORD PTR [esp+0x4]     // from_co
    mov     edx,DWORD PTR [esp]         // retaddr
    lea     ecx,[esp+0x4]               // esp
    mov     DWORD PTR [eax+0x8],ebp     //<ebp
    mov     DWORD PTR [eax+0x4],ecx     //<esp
    mov     DWORD PTR [eax+0x0],edx     //<retaddr
    mov     DWORD PTR [eax+0xc],edi     //<edi
    mov     ecx,DWORD PTR [esp+0x8]     // to_co
    mov     DWORD PTR [eax+0x10],esi    //<esi
    mov     DWORD PTR [eax+0x14],ebx    //<ebx
#ifndef ACO_CONFIG_SHARE_FPU_MXCSR_ENV
    fnstcw  WORD  PTR [eax+0x18]        //<fpucw
    stmxcsr DWORD PTR [eax+0x1c]        //<mxcsr
#endif
    mov     edx,DWORD PTR [ecx+0x4]     //>esp
    mov     ebp,DWORD PTR [ecx+0x8]     //>ebp
    mov     eax,DWORD PTR [ecx+0x0]     //>retaddr
    mov     edi,DWORD PTR [ecx+0xc]     //>edi
    mov     esi,DWORD PTR [ecx+0x10]    //>esi
    mov     ebx,DWORD PTR [ecx+0x14]    //>ebx
#ifndef ACO_CONFIG_SHARE_FPU_MXCSR_ENV
    fldcw   WORD  PTR     [ecx+0x18]        //>fpucw
    ldmxcsr DWORD PTR     [ecx+0x1c]        //>mxcsr
#endif
    xor     ecx,ecx
    mov     esp,edx
    xor     edx,edx
    jmp     eax
#elif __x86_64__
/*
    0x00                  -->                  0xff
    r12 r13 r14 r15 rip rsp rbx rbp fpucw16 mxcsr32
    0   8   10  18  20  28  30  38  40      44
*/
    // rdi - from_co | rsi - to_co
    mov     rdx,QWORD PTR [rsp]      // retaddr
    lea     rcx,[rsp+0x8]            // rsp
    mov     QWORD PTR [rdi+0x0], r12
    mov     QWORD PTR [rdi+0x8], r13
    mov     QWORD PTR [rdi+0x10],r14
    mov     QWORD PTR [rdi+0x18],r15
    mov     QWORD PTR [rdi+0x20],rdx // retaddr
    mov     QWORD PTR [rdi+0x28],rcx // rsp
    mov     QWORD PTR [rdi+0x30],rbx
    mov     QWORD PTR [rdi+0x38],rbp
#ifndef ACO_CONFIG_SHARE_FPU_MXCSR_ENV
    fnstcw  WORD PTR  [rdi+0x40]
    stmxcsr DWORD PTR [rdi+0x44]
#endif
    mov     r12,QWORD PTR [rsi+0x0]
    mov     r13,QWORD PTR [rsi+0x8]
    mov     r14,QWORD PTR [rsi+0x10]
    mov     r15,QWORD PTR [rsi+0x18]
    mov     rax,QWORD PTR [rsi+0x20] // retaddr
    mov     rcx,QWORD PTR [rsi+0x28] // rsp
    mov     rbx,QWORD PTR [rsi+0x30]
    mov     rbp,QWORD PTR [rsi+0x38]
#ifndef ACO_CONFIG_SHARE_FPU_MXCSR_ENV
    fldcw   WORD PTR      [rsi+0x40]
    ldmxcsr DWORD PTR     [rsi+0x44]
#endif
    mov     rsp,rcx
    jmp     rax
#elif __aarch64__
/*
    0x00         -->            0xff
    x16 x17 x19  ...  x29 lr sp fpcr
    0   8   10   ...  60  68 70 78
*/
    // r0 - from_co | r1 - to_co
    mov     x2,  lr
    stp    x16, x17, [x0, 0x00]
    stp    x19, x20, [x0, 0x10]
    stp    x21, x22, [x0, 0x20]
    stp    x23, x24, [x0, 0x30]
    stp    x25, x26, [x0, 0x40]
    stp    x27, x28, [x0, 0x50]
    stp    x29, lr,  [x0, 0x60]
    mov     x5,  sp
    str     x5,  [x0, 0x70]
#ifndef ACO_CONFIG_SHARE_FPU_MXCSR_ENV
    mrs     x5,  fpcr
    str     x5,  [x1, 0x78]
#endif

    ldp     x16, x17, [x1, 0x00]
    ldp     x19, x20, [x1, 0x10]
    ldp     x21, x22, [x1, 0x20]
    ldp     x23, x24, [x1, 0x30]
    ldp     x25, x26, [x1, 0x40]
    ldp     x27, x28, [x1, 0x50]
    ldp     x29, x30, [x1, 0x60]
    ldr     x3,  [x1, 0x70]
    mov     sp,  x3
#ifndef ACO_CONFIG_SHARE_FPU_MXCSR_ENV
    ldr     x3,  [x1, 0x78]
    msr     fpcr,x3
#endif

    br      x30
#else
    #error "platform not support"
#endif

.globl aco_save_fpucw_mxcsr
#if defined(__APPLE__)
#else
.type  aco_save_fpucw_mxcsr, @function
#endif
#ifndef __aarch64__
.intel_syntax noprefix
#endif
aco_save_fpucw_mxcsr:
#ifdef __i386__
    mov     eax,DWORD PTR [esp+0x4]     // ptr
    fnstcw  WORD PTR  [eax]
    stmxcsr DWORD PTR [eax+0x4] 
    ret
#elif __x86_64__
    fnstcw  WORD PTR  [rdi]
    stmxcsr DWORD PTR [rdi+0x4]
    ret
#elif __aarch64__
    mrs x1, fpcr
    str x1, [x0]
    ret
#else
    #error "platform not support"
#endif

#if defined(__APPLE__)
.globl _abort
.globl _aco_funcp_protector
#else
.globl abort
.globl aco_funcp_protector
#endif

.globl aco_funcp_protector_asm
#if defined(__APPLE__)
#else
.type  aco_funcp_protector_asm, @function
#endif
#ifndef __aarch64__
.intel_syntax noprefix
#endif
aco_funcp_protector_asm:
#ifdef __i386__
            and     esp,0xfffffff0
    #if defined(__APPLE__)
            call    _aco_funcp_protector
            call    _abort
    #else
        #if defined(__pic__) || defined(__PIC__)
            call    aco_funcp_protector@PLT
            call    abort@PLT
        #else
            call    aco_funcp_protector
            call    abort
        #endif
    #endif
            ret
#elif __x86_64__
            and     rsp,0xfffffffffffffff0
    #if defined(__APPLE__)
            call    _aco_funcp_protector
            call    _abort
    #else
        #if defined(__pic__) || defined(__PIC__)
            call    aco_funcp_protector@PLT
            call    abort@PLT
        #else
            call    aco_funcp_protector
            call    abort
        #endif
    #endif
            ret
#elif __aarch64__
            mov     x9, sp
            and     x9, x9, #0xfffffffffffffff0
            mov     sp, x9
            bl      aco_funcp_protector
            bl      abort
            ret
#else
#    error "platform not support"
#endif
#endif // NO_FIBERS
#endif
