
.text
  .global main

main:
  push $msg          # printf("%s\n", "Hola!")
  push $s_format
  call printf

  call _main

  push $0            # exit(0);
  call exit
  ret

# int main () {
#   [[int]] x;
#   x = [] : [];
#   x.hd = 5 : [];
#   f(x);
#   // x = [[5, 5]]
#   print (x.hd.hd);           // 5
#   print (x.hd.tl.hd);        // 5
#   return 1;                  // 1
# }
_main:
  enter $4, $0 # 4 bytes = 1 x int32 var x

  # [int] x;
  # x = 23 : []';
    # 1. expr []' on stack
    pushl $0
    # 2. expr 23 on stack
    pushl $23
    # 3. expr 23 : []' on stack
    movl $8, %edi # malloc 8 bytes = 2 int32's
    call malloc # address is in %eax
    popl %edx # expr 23
    movl %edx, (%eax)
    popl %edx # expr []'
    movl %edx, 4(%eax)
    pushl %eax
    # 4. assign to var x
    popl %edx # expr 23 : []' in %edx
    movl %edx, -4(%ebp) # move into first local (x)
  # print(x.hd);
    # 1. expr x on stack
    pushl -4(%ebp) # expr x on stack
    # 2. expr x.hd on stack
    popl %edx # expr x in %edx
    pushl 0(%edx) # expr x.hd on stack
    # 3. print
    popl %eax
    call _print


  leave
  ret

# unit f ([[a]] x) {
#   x.hd := x.hd.hd : x.hd;
#   return ();
# }
_f:
  enter $0, $0
  mov 12(%ebp), %ebx # access arg 2
  mov 8(%ebp), %eax  # access arg 1
  add %ebx, %eax
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
