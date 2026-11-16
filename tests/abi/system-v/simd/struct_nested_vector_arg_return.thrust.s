	.text
	.file	"struct_nested_vector_arg_return.thrust"
	.globl	struct_nested_vector_arg_return.identity
	.p2align	4, 0x90
	.type	struct_nested_vector_arg_return.identity,@function
struct_nested_vector_arg_return.identity:
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
	movl	32(%rcx), %ecx
	movl	%ecx, 32(%rdi)
	vmovaps	%ymm0, (%rdi)
	movq	%rbp, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	vzeroupper
	retq
.Lfunc_end0:
	.size	struct_nested_vector_arg_return.identity, .Lfunc_end0-struct_nested_vector_arg_return.identity
	.cfi_endproc

	.section	.rodata.cst4,"aM",@progbits,4
	.p2align	2, 0x0
.LCPI1_0:
	.long	0x3f800000
.LCPI1_2:
	.long	0x41000000
	.section	.rodata.cst32,"aM",@progbits,32
	.p2align	5, 0x0
.LCPI1_1:
	.long	0x3f800000
	.long	0x40000000
	.long	0x40400000
	.long	0x40800000
	.long	0x40a00000
	.long	0x40c00000
	.long	0x40e00000
	.long	0x41000000
	.text
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
	subq	$416, %rsp
	movq	%fs:40, %rax
	movq	%rax, 392(%rsp)
	vmovaps	.LCPI1_1(%rip), %ymm0
	vmovaps	%ymm0, 160(%rsp)
	vmovaps	160(%rsp), %ymm0
	vmovaps	%ymm0, 192(%rsp)
	movl	$7, 224(%rsp)
	vmovaps	192(%rsp), %ymm0
	movl	224(%rsp), %eax
	movl	%eax, 288(%rsp)
	vmovaps	%ymm0, 256(%rsp)
	vmovaps	256(%rsp), %ymm0
	vmovaps	288(%rsp), %ymm1
	movq	%rsp, %rax
	vmovaps	%ymm1, 32(%rax)
	vmovaps	%ymm0, (%rax)
	leaq	320(%rsp), %rdi
	vzeroupper
	callq	struct_nested_vector_arg_return.identity@PLT
	vmovaps	320(%rsp), %ymm0
	movl	352(%rsp), %eax
	movl	%eax, 128(%rsp)
	vmovaps	%ymm0, 96(%rsp)
	vmovaps	96(%rsp), %ymm0
	vmovaps	%ymm0, 64(%rsp)
	vmovss	64(%rsp), %xmm0
	vmovss	.LCPI1_0(%rip), %xmm1
	vucomiss	%xmm1, %xmm0
	jne	.LBB1_2
	vmovss	92(%rsp), %xmm0
	vmovss	.LCPI1_2(%rip), %xmm1
	vucomiss	%xmm1, %xmm0
	jne	.LBB1_5
	jmp	.LBB1_4
.LBB1_2:
	movq	%fs:40, %rax
	movq	392(%rsp), %rcx
	cmpq	%rcx, %rax
	jne	.LBB1_11
	movl	$1, %eax
	movq	%rbp, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	vzeroupper
	retq
.LBB1_4:
	.cfi_def_cfa %rbp, 16
	cmpl	$7, 128(%rsp)
	jne	.LBB1_7
	jmp	.LBB1_9
.LBB1_5:
	movq	%fs:40, %rax
	movq	392(%rsp), %rcx
	cmpq	%rcx, %rax
	jne	.LBB1_11
	movl	$2, %eax
	movq	%rbp, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	vzeroupper
	retq
.LBB1_7:
	.cfi_def_cfa %rbp, 16
	movq	%fs:40, %rax
	movq	392(%rsp), %rcx
	cmpq	%rcx, %rax
	jne	.LBB1_11
	movl	$3, %eax
	movq	%rbp, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	vzeroupper
	retq
.LBB1_9:
	.cfi_def_cfa %rbp, 16
	movq	%fs:40, %rax
	movq	392(%rsp), %rcx
	cmpq	%rcx, %rax
	jne	.LBB1_11
	xorl	%eax, %eax
	movq	%rbp, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	vzeroupper
	retq
.LBB1_11:
	.cfi_def_cfa %rbp, 16
	vzeroupper
	callq	__stack_chk_fail@PLT
.Lfunc_end1:
	.size	main, .Lfunc_end1-main
	.cfi_endproc

	.ident	"thrustc version 0.2.6"
	.section	".note.GNU-stack","",@progbits
