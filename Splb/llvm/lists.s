	.file	"lists.ll"
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
	movl	$5, (%esp)
	calll	malloc
	movl	$1, %eax
	addl	$12, %esp
	ret
.Ltmp2:
	.size	main, .Ltmp2-main
	.cfi_endproc

	.type	.L.print_int_v_str,@object # @.print_int_v_str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.print_int_v_str:
	.asciz	"%d\n"
	.size	.L.print_int_v_str, 4

	.type	.L.print_string_v_str,@object # @.print_string_v_str
.L.print_string_v_str:
	.asciz	"%s\n"
	.size	.L.print_string_v_str, 4

	.type	.L.true_v_str,@object   # @.true_v_str
.L.true_v_str:
	.asciz	"true"
	.size	.L.true_v_str, 5

	.type	.L.false_v_str,@object  # @.false_v_str
.L.false_v_str:
	.asciz	"false"
	.size	.L.false_v_str, 6

	.type	.L.unit_v_str,@object   # @.unit_v_str
.L.unit_v_str:
	.asciz	"()"
	.size	.L.unit_v_str, 3


	.section	".note.GNU-stack","",@progbits
