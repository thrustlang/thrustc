	.att_syntax
	.file	"native_vector_512_arg_return.c"
	.text
	.globl	add_vectors                     # -- Begin function add_vectors
	.prefalign	4, .Lfunc_end0, nop
	.type	add_vectors,@function
add_vectors:                            # @add_vectors
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	andq	$-64, %rsp
	subq	$192, %rsp
	vmovaps	%zmm0, 64(%rsp)
	vmovaps	%zmm1, (%rsp)
	vmovaps	64(%rsp), %zmm0
	vaddps	(%rsp), %zmm0, %zmm0
	movq	%rbp, %rsp
	popq	%rbp
	.cfi_def_cfa %rsp, 8
	retq
.Lfunc_end0:
	.size	add_vectors, .Lfunc_end0-add_vectors
	.cfi_endproc
                                        # -- End function
	.ident	"clang version 23.1.5"
	.section	".note.GNU-stack","",@progbits
	.addrsig
