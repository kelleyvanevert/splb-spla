	.file	"hello.ll"
	.text
	.globl	main
	.align	16, 0x90
	.type	main,@function
main:                                   # @main
	.cfi_startproc
# BB#0:
	subl	$12, %esp
.Ltmp1:
	.cfi_def_cfa_offset 16
	movl	$.L.hello, (%esp)
	calll	puts
	xorl	%eax, %eax
	addl	$12, %esp
	ret
.Ltmp2:
	.size	main, .Ltmp2-main
	.cfi_endproc

	.type	.L.hello,@object        # @.hello
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.hello:
	.asciz	"hello world\n"
	.size	.L.hello, 13


	.section	".note.GNU-stack","",@progbits
