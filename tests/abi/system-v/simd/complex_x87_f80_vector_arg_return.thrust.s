	.text
	.file	"complex_x87_f80_vector_arg_return.thrust"
	.globl	complex_x87_f80_vector_arg_return.identity
	.p2align	4, 0x90
	.type	complex_x87_f80_vector_arg_return.identity,@function
complex_x87_f80_vector_arg_return.identity:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	movq	%rdi, %rax
	leaq	16(%rbp), %rcx
	fldt	(%rcx)
	fldt	10(%rcx)
	fstpt	10(%rdi)
	fstpt	(%rdi)
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Lfunc_end0:
	.size	complex_x87_f80_vector_arg_return.identity, .Lfunc_end0-complex_x87_f80_vector_arg_return.identity
	.cfi_endproc

	.globl	main
	.p2align	4, 0x90
	.type	main,@function
main:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	xorl	%eax, %eax
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Lfunc_end1:
	.size	main, .Lfunc_end1-main
	.cfi_endproc

	.ident	"thrustc version 0.2.6"
	.section	".note.GNU-stack","",@progbits
