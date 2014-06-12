	.file	"print.ll"
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
	movl	$1, (%esp)
	calll	print_int
	movl	$2, (%esp)
	calll	print_int
	movl	$3, (%esp)
	calll	print_int
	movl	$4, (%esp)
	calll	print_int
	movl	$5, (%esp)
	calll	print_int
	movl	$1, (%esp)
	calll	print_bool
	movl	$0, (%esp)
	calll	print_bool
	movl	$1, %eax
	addl	$12, %esp
	ret
.Ltmp2:
	.size	main, .Ltmp2-main
	.cfi_endproc

	.globl	print_int
	.align	16, 0x90
	.type	print_int,@function
print_int:                              # @print_int
	.cfi_startproc
# BB#0:
	subl	$12, %esp
.Ltmp4:
	.cfi_def_cfa_offset 16
	movl	16(%esp), %eax
	movl	%eax, 4(%esp)
	movl	$.L.print_int_v_str, (%esp)
	calll	printf
	addl	$12, %esp
	ret
.Ltmp5:
	.size	print_int, .Ltmp5-print_int
	.cfi_endproc

	.globl	print_bool
	.align	16, 0x90
	.type	print_bool,@function
print_bool:                             # @print_bool
	.cfi_startproc
# BB#0:
	subl	$12, %esp
.Ltmp7:
	.cfi_def_cfa_offset 16
	testb	$1, 16(%esp)
	je	.LBB2_3
# BB#1:                                 # %printTrue
	movl	$.L.true_v_str, 4(%esp)
	jmp	.LBB2_2
.LBB2_3:                                # %printFalse
	movl	$.L.false_v_str, 4(%esp)
.LBB2_2:                                # %printTrue
	movl	$.L.print_string_v_str, (%esp)
	calll	printf
	addl	$12, %esp
	ret
.Ltmp8:
	.size	print_bool, .Ltmp8-print_bool
	.cfi_endproc

	.globl	print_tuple
	.align	16, 0x90
	.type	print_tuple,@function
print_tuple:                            # @print_tuple
	.cfi_startproc
# BB#0:
	ret
.Ltmp9:
	.size	print_tuple, .Ltmp9-print_tuple
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
