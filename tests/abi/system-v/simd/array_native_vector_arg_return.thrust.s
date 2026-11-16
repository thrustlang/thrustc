	.text
	.file	"array_native_vector_arg_return.thrust"
	.globl	array_native_vector_arg_return.identity
	.p2align	4, 0x90
	.type	array_native_vector_arg_return.identity,@function
array_native_vector_arg_return.identity:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	andq	$-32, %rsp
	subq	$32, %rsp
	movq	%rdi, %rax
	leaq	16(%rbp), %rcx
	vmovaps	(%rcx), %ymm0
	vmovaps	32(%rcx), %ymm1
	vmovaps	%ymm1, 32(%rdi)
	vmovaps	%ymm0, (%rdi)
	movq	%rbp, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	vzeroupper
	retq
.Lfunc_end0:
	.size	array_native_vector_arg_return.identity, .Lfunc_end0-array_native_vector_arg_return.identity
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
	andq	$-32, %rsp
	subq	$352, %rsp
	movq	%fs:40, %rax
	movq	%rax, 328(%rsp)
	vmovaps	256(%rsp), %ymm0
	vmovaps	288(%rsp), %ymm1
	vmovaps	%ymm1, 96(%rsp)
	vmovaps	%ymm0, 64(%rsp)
	vmovaps	64(%rsp), %ymm0
	vmovaps	96(%rsp), %ymm1
	movq	%rsp, %rax
	vmovaps	%ymm1, 32(%rax)
	vmovaps	%ymm0, (%rax)
	leaq	128(%rsp), %rdi
	vzeroupper
	callq	array_native_vector_arg_return.identity@PLT
	vmovaps	128(%rsp), %ymm0
	vmovaps	160(%rsp), %ymm1
	vmovaps	%ymm1, 224(%rsp)
	vmovaps	%ymm0, 192(%rsp)
	xorl	%eax, %eax
	testb	$1, %al
	jne	.LBB1_1
	jmp	.LBB1_3
.LBB1_1:
	movq	%fs:40, %rax
	movq	328(%rsp), %rcx
	cmpq	%rcx, %rax
	jne	.LBB1_5
	movl	$1, %eax
	movq	%rbp, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	vzeroupper
	retq
.LBB1_3:
	.cfi_def_cfa %rbp, 16
	movq	%fs:40, %rax
	movq	328(%rsp), %rcx
	cmpq	%rcx, %rax
	jne	.LBB1_5
	xorl	%eax, %eax
	movq	%rbp, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	vzeroupper
	retq
.LBB1_5:
	.cfi_def_cfa %rbp, 16
	vzeroupper
	callq	__stack_chk_fail@PLT
.Lfunc_end1:
	.size	main, .Lfunc_end1-main
	.cfi_endproc

	.ident	"thrustc version 0.2.6"
	.section	".note.GNU-stack","",@progbits
