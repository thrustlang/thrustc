	.text
	.file	"native_vector_256_arg_return.thrust"
	.globl	native_vector_256_arg_return.addVectors
	.p2align	4, 0x90
	.type	native_vector_256_arg_return.addVectors,@function
native_vector_256_arg_return.addVectors:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	vaddps	%ymm1, %ymm0, %ymm0
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Lfunc_end0:
	.size	native_vector_256_arg_return.addVectors, .Lfunc_end0-native_vector_256_arg_return.addVectors
	.cfi_endproc

	.section	.rodata.cst4,"aM",@progbits,4
	.p2align	2, 0x0
.LCPI1_0:
	.long	0x41100000
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
.LCPI1_2:
	.long	0x41000000
	.long	0x40e00000
	.long	0x40c00000
	.long	0x40a00000
	.long	0x40800000
	.long	0x40400000
	.long	0x40000000
	.long	0x3f800000
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
	subq	$128, %rsp
	vmovaps	.LCPI1_1(%rip), %ymm0
	vmovaps	%ymm0, 64(%rsp)
	vmovaps	.LCPI1_2(%rip), %ymm0
	vmovaps	%ymm0, 32(%rsp)
	vmovaps	64(%rsp), %ymm0
	vmovaps	32(%rsp), %ymm1
	callq	native_vector_256_arg_return.addVectors@PLT
	vmovaps	%ymm0, (%rsp)
	vmovss	(%rsp), %xmm0
	vmovss	.LCPI1_0(%rip), %xmm1
	vucomiss	%xmm1, %xmm0
	jne	.LBB1_2
	vmovss	28(%rsp), %xmm0
	vmovss	.LCPI1_0(%rip), %xmm1
	vucomiss	%xmm1, %xmm0
	jne	.LBB1_3
	jmp	.LBB1_4
.LBB1_2:
	movl	$1, %eax
	movq	%rbp, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	vzeroupper
	retq
.LBB1_3:
	.cfi_def_cfa %rbp, 16
	movl	$2, %eax
	movq	%rbp, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	vzeroupper
	retq
.LBB1_4:
	.cfi_def_cfa %rbp, 16
	xorl	%eax, %eax
	movq	%rbp, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	vzeroupper
	retq
.Lfunc_end1:
	.size	main, .Lfunc_end1-main
	.cfi_endproc

	.ident	"thrustc version 0.2.6"
	.section	".note.GNU-stack","",@progbits
