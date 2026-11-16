	.att_syntax
	.file	"complex_x87_f80_vector_arg_return.c"
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
	subq	$64, %rsp
	leaq	16(%rbp), %rax
	fldt	(%rax)
	fldt	10(%rax)
	fstpt	10(%rsp)
	fstpt	(%rsp)
	fldt	10(%rsp)
	fldt	(%rsp)
	movq	%rbp, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Lfunc_end0:
	.size	identity, .Lfunc_end0-identity
	.cfi_endproc
                                        # -- End function
	.ident	"clang version 23.1.5"
	.section	".note.GNU-stack","",@progbits
	.addrsig
