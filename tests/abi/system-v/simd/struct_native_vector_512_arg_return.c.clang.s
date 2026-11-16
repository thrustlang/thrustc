	.att_syntax
	.file	"struct_native_vector_512_arg_return.c"
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
	andq	$-64, %rsp
	subq	$256, %rsp                      # imm = 0x100
	movq	%fs:40, %rax
	movq	%rax, 232(%rsp)
	vmovaps	%zmm0, 64(%rsp)
	vmovdqa64	64(%rsp), %zmm0
	vmovdqa64	%zmm0, 128(%rsp)
	vmovaps	128(%rsp), %zmm0
	vmovaps	%zmm0, (%rsp)                   # 64-byte Spill
	movq	%fs:40, %rax
	movq	232(%rsp), %rcx
	cmpq	%rcx, %rax
	jne	.LBB0_2
# %bb.1:
	vmovaps	(%rsp), %zmm0                   # 64-byte Reload
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
