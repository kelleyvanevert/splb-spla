
.text
  .global main

main: 
  call _main

  push $msg          # printf("%s\n", "Hola!")
  push $s_format
  call printf

  # [int] x = 6 : [];
    # 1. expr [] in %eax
    movl $0, %eax
    # 2. expr 5 : _ in %eax
    movl %eax, %ecx # prev expr in %ecx
    movl $8, %edi # malloc 8 bytes = 2 int32's
    call malloc # address is in %eax
    movl $6, (%eax)
    movl %ecx, 4(%eax)
  # print(x.hd);
    # 1. expr x.hd in %eax
    movl (%eax), %eax
    call _print


#          mov $4, %edi
#          call malloc        # malloc(40); // result is in %eax
#          mov $7, %ecx
#          mov %ecx, (%eax) # // move 7 into allocated space
#          mov (%eax), %edx # // read back
#          mov %edx, %eax   # // put into return register
#          call _print

  push $0            # exit(0);
  call exit
  ret

_main:
  enter $0, $0

                     # f(2, 3);
  push $3            #   arg 2
  push $2            #   arg 1
  call _f
  call _print
  leave
  ret

_f:
  enter $0, $0
  mov 12(%ebp), %ebx # access arg 2
  mov 8(%ebp), %eax  # access arg 1
  add %ebx, %eax
  leave
  ret

# int g (int c) {
#   return c + h();
# }
_g:
  enter $0, $0
  leave
  ret

# int h () {
#   return 1;
# }
_h:
  enter $0, $0
  movl $1, %eax
  leave
  ret

_print:
  enter $0, $0
  push %eax
  push $d_format
  call printf
  leave
  ret

.data

s_format:
  .asciz "%s\n"
d_format:
  .asciz "%d\n"
msg:
  .asciz "Hola!"
