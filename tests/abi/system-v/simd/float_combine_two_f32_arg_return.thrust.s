	.text
	.file	"float_combine_two_f32_arg_return.thrust"
	.globl	float_combine_two_f32_arg_return.identity
	.p2align	4, 0x90
	.type	float_combine_two_f32_arg_return.identity,@function
float_combine_two_f32_arg_return.identity:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	vmovlpd	%xmm0, -8(%rbp)
	vmovss	-8(%rbp), %xmm0
	vmovss	-4(%rbp), %xmm1
	vmovss	%xmm1, -12(%rbp)
	vmovss	%xmm0, -16(%rbp)
	vmovsd	-16(%rbp), %xmm0
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Lfunc_end0:
	.size	float_combine_two_f32_arg_return.identity, .Lfunc_end0-float_combine_two_f32_arg_return.identity
	.cfi_endproc

	.section	.rodata.cst4,"aM",@progbits,4
	.p2align	2, 0x0
.LCPI1_0:
	.long	0x3fc00000
.LCPI1_1:
	.long	0x40200000
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
	subq	$32, %rsp
	movl	$1069547520, -8(%rbp)
	movl	$1075838976, -4(%rbp)
	vmovss	-8(%rbp), %xmm0
	vmovss	-4(%rbp), %xmm1
	vmovss	%xmm1, -20(%rbp)
	vmovss	%xmm0, -24(%rbp)
	vmovsd	-24(%rbp), %xmm0
	callq	float_combine_two_f32_arg_return.identity@PLT
	vmovlpd	%xmm0, -16(%rbp)
	vmovss	-16(%rbp), %xmm0
	vmovss	.LCPI1_0(%rip), %xmm1
	vucomiss	%xmm1, %xmm0
	jne	.LBB1_2
	vmovss	-12(%rbp), %xmm0
	vmovss	.LCPI1_1(%rip), %xmm1
	vucomiss	%xmm1, %xmm0
	jne	.LBB1_3
	jmp	.LBB1_4
.LBB1_2:
	movl	$1, %eax
	addq	$32, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.LBB1_3:
	.cfi_def_cfa %rbp, 16
	movl	$2, %eax
	addq	$32, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.LBB1_4:
	.cfi_def_cfa %rbp, 16
	xorl	%eax, %eax
	addq	$32, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Lfunc_end1:
	.size	main, .Lfunc_end1-main
	.cfi_endproc

	.ident	"thrustc version 0.2.6"
	.section	".note.GNU-stack","",@progbits
