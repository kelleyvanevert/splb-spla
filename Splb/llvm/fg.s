	.file	"fg.ll"
	.text
	.globl	main
	.align	16, 0x90
	.type	main,@function
main:                                   # @main
	.cfi_startproc
# BB#0:
	pushl	%esi
.Ltmp2:
	.cfi_def_cfa_offset 8
	subl	$24, %esp
.Ltmp3:
	.cfi_def_cfa_offset 32
.Ltmp4:
	.cfi_offset %esi, -8
	movl	$3, 4(%esp)
	movl	$2, (%esp)
	calll	f
	movl	%eax, %esi
	movl	%esi, 20(%esp)
	movl	%esi, 4(%esp)
	movl	$.L.str, (%esp)
	calll	printf
	movl	%esi, %eax
	addl	$24, %esp
	popl	%esi
	ret
.Ltmp5:
	.size	main, .Ltmp5-main
	.cfi_endproc

	.globl	f
	.align	16, 0x90
	.type	f,@function
f:                                      # @f
	.cfi_startproc
# BB#0:
	subl	$12, %esp
.Ltmp7:
	.cfi_def_cfa_offset 16
	movl	20(%esp), %eax
	movl	%eax, (%esp)
	calll	g
	addl	16(%esp), %eax
	addl	$12, %esp
	ret
.Ltmp8:
	.size	f, .Ltmp8-f
	.cfi_endproc

	.globl	g
	.align	16, 0x90
	.type	g,@function
g:                                      # @g
	.cfi_startproc
# BB#0:
	subl	$12, %esp
.Ltmp10:
	.cfi_def_cfa_offset 16
	calll	h
	addl	16(%esp), %eax
	addl	$12, %esp
	ret
.Ltmp11:
	.size	g, .Ltmp11-g
	.cfi_endproc

	.globl	h
	.align	16, 0x90
	.type	h,@function
h:                                      # @h
	.cfi_startproc
# BB#0:
	movl	$1, %eax
	ret
.Ltmp12:
	.size	h, .Ltmp12-h
	.cfi_endproc

	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"%d\n"
	.size	.L.str, 4


	.section	".note.GNU-stack","",@progbits
