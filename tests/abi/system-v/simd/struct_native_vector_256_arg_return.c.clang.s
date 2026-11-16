	.att_syntax
	.file	"struct_native_vector_256_arg_return.c"
	.text
	.globl	identity                        # -- Begin function identity
	.prefalign	4, .Lfunc_end0, nop
	.type	identity,@function
identity:                               # @identity
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	andq	$-32, %rsp
	subq	$128, %rsp
	movq	%fs:40, %rax
	movq	%rax, 104(%rsp)
	vmovaps	%ymm0, 32(%rsp)
	vmovaps	32(%rsp), %ymm0
	vmovaps	%ymm0, 64(%rsp)
	vmovaps	64(%rsp), %ymm0
	vmovaps	%ymm0, (%rsp)                   # 32-byte Spill
	movq	%fs:40, %rax
	movq	104(%rsp), %rcx
	cmpq	%rcx, %rax
	jne	.LBB0_2
# %bb.1:
	vmovaps	(%rsp), %ymm0                   # 32-byte Reload
	movq	%rbp, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.LBB0_2:
	.cfi_def_cfa %rbp, 16
	vzeroupper
	callq	__stack_chk_fail@PLT
.Lfunc_end0:
	.size	identity, .Lfunc_end0-identity
	.cfi_endproc
                                        # -- End function
	.ident	"clang version 23.1.5"
	.section	".note.GNU-stack","",@progbits
	.addrsig
	.addrsig_sym __stack_chk_fail
