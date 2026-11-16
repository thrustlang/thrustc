	.text
	.file	"float_combine_four_f32_arg_return.thrust"
	.globl	float_combine_four_f32_arg_return.identity
	.p2align	4, 0x90
	.type	float_combine_four_f32_arg_return.identity,@function
float_combine_four_f32_arg_return.identity:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	vmovlpd	%xmm0, -16(%rbp)
	vmovlpd	%xmm1, -8(%rbp)
	vmovss	-16(%rbp), %xmm0
	vmovss	-12(%rbp), %xmm1
	vmovss	-8(%rbp), %xmm2
	vmovss	-4(%rbp), %xmm3
	vmovss	%xmm3, -20(%rbp)
	vmovss	%xmm2, -24(%rbp)
	vmovss	%xmm1, -28(%rbp)
	vmovss	%xmm0, -32(%rbp)
	vmovsd	-32(%rbp), %xmm0
	vmovsd	-24(%rbp), %xmm1
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Lfunc_end0:
	.size	float_combine_four_f32_arg_return.identity, .Lfunc_end0-float_combine_four_f32_arg_return.identity
	.cfi_endproc

	.section	.rodata.cst4,"aM",@progbits,4
	.p2align	2, 0x0
.LCPI1_0:
	.long	0x3f800000
.LCPI1_1:
	.long	0x40800000
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
	subq	$48, %rsp
	movl	$1065353216, -16(%rbp)
	movl	$1073741824, -12(%rbp)
	movl	$1077936128, -8(%rbp)
	movl	$1082130432, -4(%rbp)
	vmovss	-16(%rbp), %xmm0
	vmovss	-12(%rbp), %xmm1
	vmovss	-8(%rbp), %xmm2
	vmovss	-4(%rbp), %xmm3
	vmovss	%xmm3, -36(%rbp)
	vmovss	%xmm2, -40(%rbp)
	vmovss	%xmm1, -44(%rbp)
	vmovss	%xmm0, -48(%rbp)
	vmovsd	-48(%rbp), %xmm0
	vmovsd	-40(%rbp), %xmm1
	callq	float_combine_four_f32_arg_return.identity@PLT
	vmovlpd	%xmm1, -24(%rbp)
	vmovlpd	%xmm0, -32(%rbp)
	vmovss	-32(%rbp), %xmm0
	vmovss	.LCPI1_0(%rip), %xmm1
	vucomiss	%xmm1, %xmm0
	jne	.LBB1_2
	vmovss	-20(%rbp), %xmm0
	vmovss	.LCPI1_1(%rip), %xmm1
	vucomiss	%xmm1, %xmm0
	jne	.LBB1_3
	jmp	.LBB1_4
.LBB1_2:
	movl	$1, %eax
	addq	$48, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.LBB1_3:
	.cfi_def_cfa %rbp, 16
	movl	$2, %eax
	addq	$48, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.LBB1_4:
	.cfi_def_cfa %rbp, 16
	xorl	%eax, %eax
	addq	$48, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Lfunc_end1:
	.size	main, .Lfunc_end1-main
	.cfi_endproc

	.ident	"thrustc version 0.2.6"
	.section	".note.GNU-stack","",@progbits
