	.text
	.file	"integer_per_element_bool16_arg_return.thrust"
	.globl	integer_per_element_bool16_arg_return.identity
	.p2align	4, 0x90
	.type	integer_per_element_bool16_arg_return.identity,@function
integer_per_element_bool16_arg_return.identity:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	pushq	%r15
	pushq	%r14
	pushq	%r13
	pushq	%r12
	pushq	%rbx
	.cfi_offset %rbx, -56
	.cfi_offset %r12, -48
	.cfi_offset %r13, -40
	.cfi_offset %r14, -32
	.cfi_offset %r15, -24
	movb	%r9b, %al
	movb	%al, -49(%rbp)
	movb	%r8b, %al
	movb	%al, -50(%rbp)
	movb	%cl, %al
	movb	%al, -51(%rbp)
	movb	%dl, %cl
	movb	%sil, %al
	movb	%al, -57(%rbp)
	movq	%rdi, %rax
	movq	%rax, -48(%rbp)
	movb	96(%rbp), %dl
	movb	88(%rbp), %sil
	movb	80(%rbp), %r8b
	movb	72(%rbp), %r9b
	movb	64(%rbp), %r10b
	movb	56(%rbp), %r11b
	movb	48(%rbp), %bl
	movb	40(%rbp), %r14b
	movb	32(%rbp), %r15b
	movb	24(%rbp), %r12b
	movb	16(%rbp), %r13b
	movb	%cl, %al
	movb	-57(%rbp), %cl
	movl	%eax, -56(%rbp)
	movb	%cl, %al
	movl	-56(%rbp), %ecx
	vmovd	%eax, %xmm0
	movb	-51(%rbp), %al
	vpinsrb	$1, %ecx, %xmm0, %xmm0
	movb	%al, %cl
	movb	-50(%rbp), %al
	vpinsrb	$2, %ecx, %xmm0, %xmm0
	movb	%al, %cl
	movb	-49(%rbp), %al
	vpinsrb	$3, %ecx, %xmm0, %xmm0
	movb	%al, %cl
	movq	-48(%rbp), %rax
	vpinsrb	$4, %ecx, %xmm0, %xmm0
	movb	%r13b, %cl
	vpinsrb	$5, %ecx, %xmm0, %xmm0
	movb	%r12b, %cl
	vpinsrb	$6, %ecx, %xmm0, %xmm0
	movb	%r15b, %cl
	vpinsrb	$7, %ecx, %xmm0, %xmm0
	movb	%r14b, %cl
	vpinsrb	$8, %ecx, %xmm0, %xmm0
	movb	%bl, %cl
	vpinsrb	$9, %ecx, %xmm0, %xmm0
	movb	%r11b, %cl
	vpinsrb	$10, %ecx, %xmm0, %xmm0
	movb	%r10b, %cl
	vpinsrb	$11, %ecx, %xmm0, %xmm0
	movb	%r9b, %cl
	vpinsrb	$12, %ecx, %xmm0, %xmm0
	movb	%r8b, %cl
	vpinsrb	$13, %ecx, %xmm0, %xmm0
	movb	%sil, %cl
	vpinsrb	$14, %ecx, %xmm0, %xmm0
	movb	%dl, %cl
	vpinsrb	$15, %ecx, %xmm0, %xmm0
	vpsllw	$7, %xmm0, %xmm0
	vpmovmskb	%xmm0, %ecx
	movw	%cx, (%rdi)
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Lfunc_end0:
	.size	integer_per_element_bool16_arg_return.identity, .Lfunc_end0-integer_per_element_bool16_arg_return.identity
	.cfi_endproc

	.section	.rodata.cst16,"aM",@progbits,16
	.p2align	4, 0x0
.LCPI1_0:
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	1
	.byte	1
	.byte	1
	.byte	1
	.byte	1
	.byte	1
	.byte	1
	.byte	1
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	3, 0x0
.LCPI1_1:
	.byte	1
	.byte	2
	.byte	4
	.byte	8
	.byte	16
	.byte	32
	.byte	64
	.byte	128
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
	pushq	%r15
	pushq	%r14
	pushq	%r13
	pushq	%r12
	pushq	%rbx
	subq	$136, %rsp
	.cfi_offset %rbx, -56
	.cfi_offset %r12, -48
	.cfi_offset %r13, -40
	.cfi_offset %r14, -32
	.cfi_offset %r15, -24
	movq	%fs:40, %rax
	movq	%rax, -48(%rbp)
	movw	$21845, -52(%rbp)
	movzwl	-52(%rbp), %eax
	vmovd	%eax, %xmm0
	vmovdqa	.LCPI1_0(%rip), %xmm1
	vpshufb	%xmm1, %xmm0, %xmm0
	vpbroadcastq	.LCPI1_1(%rip), %xmm1
	vpand	%xmm1, %xmm0, %xmm0
	vpcmpeqb	%xmm1, %xmm0, %xmm0
	vpmovmskb	%xmm0, %ecx
	movl	%ecx, -64(%rbp)
	movl	%ecx, %edx
	andl	$2, %edx
	shrl	%edx
	movl	%ecx, %eax
	andl	$4, %eax
	shrl	$2, %eax
	movl	%eax, -68(%rbp)
	movl	%ecx, %r8d
	andl	$8, %r8d
	shrl	$3, %r8d
	movl	%ecx, %r9d
	andl	$16, %r9d
	shrl	$4, %r9d
	movb	%cl, %al
	testb	%al, %al
	sets	%dil
	movl	%ecx, %r10d
	andl	$32, %r10d
	shrl	$5, %r10d
	movl	%ecx, %r11d
	andl	$64, %r11d
	shrl	$6, %r11d
	movl	%ecx, %ebx
	shrl	$8, %ebx
	andl	$1, %ebx
	movl	%ecx, %r14d
	shrl	$9, %r14d
	andl	$1, %r14d
	movl	%ecx, %r15d
	shrl	$10, %r15d
	andl	$1, %r15d
	movl	%ecx, %r12d
	shrl	$11, %r12d
	andl	$1, %r12d
	movl	%ecx, %r13d
	shrl	$12, %r13d
	andl	$1, %r13d
	movl	%ecx, %esi
	shrl	$13, %esi
	andl	$1, %esi
	movl	%ecx, %eax
	shrl	$14, %eax
	andl	$1, %eax
	movl	%eax, -72(%rbp)
	shrl	$15, %ecx
	movq	%rsp, %rax
	movq	%rax, -80(%rbp)
	movl	%ecx, 80(%rax)
	movl	-72(%rbp), %ecx
	movl	%ecx, 72(%rax)
	movl	-68(%rbp), %ecx
	movl	%esi, 64(%rax)
	movl	-64(%rbp), %esi
	movl	%r13d, 56(%rax)
	movl	%r12d, 48(%rax)
	movl	%r15d, 40(%rax)
	movl	%r14d, 32(%rax)
	movl	%ebx, 24(%rax)
	movl	%r11d, 8(%rax)
	movl	%r10d, (%rax)
	movzbl	%dil, %edi
	movl	%edi, 16(%rax)
	leaq	-50(%rbp), %rdi
	callq	integer_per_element_bool16_arg_return.identity@PLT
	movw	-50(%rbp), %ax
	movw	%ax, -54(%rbp)
	movb	-54(%rbp), %al
	testb	$1, %al
	je	.LBB1_2
	jmp	.LBB1_1
.LBB1_1:
	movw	-54(%rbp), %ax
	movw	%ax, -56(%rbp)
	movzwl	-56(%rbp), %eax
	movl	%eax, %ecx
	shrl	%ecx
	andl	$1, %ecx
	btl	$1, %eax
	jb	.LBB1_5
	jmp	.LBB1_4
.LBB1_2:
	movq	%fs:40, %rax
	movq	-48(%rbp), %rcx
	cmpq	%rcx, %rax
	jne	.LBB1_11
	movl	$1, %eax
	addq	$136, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.LBB1_4:
	.cfi_def_cfa %rbp, 16
	movw	-54(%rbp), %ax
	movw	%ax, -58(%rbp)
	movzwl	-58(%rbp), %eax
	shrl	$15, %eax
	testl	%eax, %eax
	jne	.LBB1_7
	jmp	.LBB1_9
.LBB1_5:
	movq	%fs:40, %rax
	movq	-48(%rbp), %rcx
	cmpq	%rcx, %rax
	jne	.LBB1_11
	movl	$2, %eax
	addq	$136, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.LBB1_7:
	.cfi_def_cfa %rbp, 16
	movq	%fs:40, %rax
	movq	-48(%rbp), %rcx
	cmpq	%rcx, %rax
	jne	.LBB1_11
	movl	$3, %eax
	addq	$136, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.LBB1_9:
	.cfi_def_cfa %rbp, 16
	movq	%fs:40, %rax
	movq	-48(%rbp), %rcx
	cmpq	%rcx, %rax
	jne	.LBB1_11
	xorl	%eax, %eax
	addq	$136, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.LBB1_11:
	.cfi_def_cfa %rbp, 16
	callq	__stack_chk_fail@PLT
.Lfunc_end1:
	.size	main, .Lfunc_end1-main
	.cfi_endproc

	.ident	"thrustc version 0.2.6"
	.section	".note.GNU-stack","",@progbits
