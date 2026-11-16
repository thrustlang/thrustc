	.att_syntax
	.file	"float_combine_four_f32_arg_return.c"
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
	subq	$80, %rsp
	movq	%fs:40, %rax
	movq	%rax, -8(%rbp)
	movlpd	%xmm0, -48(%rbp)
	movlpd	%xmm1, -40(%rbp)
	movups	-48(%rbp), %xmm0
	movaps	%xmm0, -32(%rbp)
	movsd	-32(%rbp), %xmm0                # xmm0 = mem[0],zero
	movaps	%xmm0, -80(%rbp)                # 16-byte Spill
	movsd	-24(%rbp), %xmm0                # xmm0 = mem[0],zero
	movaps	%xmm0, -64(%rbp)                # 16-byte Spill
	movq	%fs:40, %rax
	movq	-8(%rbp), %rcx
	cmpq	%rcx, %rax
	jne	.LBB0_2
# %bb.1:
	movaps	-64(%rbp), %xmm1                # 16-byte Reload
	movaps	-80(%rbp), %xmm0                # 16-byte Reload
	addq	$80, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.LBB0_2:
	.cfi_def_cfa %rbp, 16
	callq	__stack_chk_fail@PLT
.Lfunc_end0:
	.size	identity, .Lfunc_end0-identity
	.cfi_endproc
                                        # -- End function
	.ident	"clang version 23.1.5"
	.section	".note.GNU-stack","",@progbits
	.addrsig
	.addrsig_sym __stack_chk_fail
