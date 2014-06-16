
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
#   x.hd = 23 : [];
#   f(x);
#   // x = [[23, 23]]
#   print (x.hd.hd);           // 23
#   print (x.hd.tl.hd);        // 23
#   return 1;                  // 1
# }
_main:
  enter $4, $0 # 4 bytes = 1 x int32 var x

  # 1. [[int]] x;
  # 2. x = [] : []';
    # 2.1. put expr []' on stack
    pushl $0
    # 2.2. put expr [] on stack
    pushl $0
    # 2.3. put expr [] : []' on stack
    movl $8, %edi # malloc 8 bytes = 2 int32's
    call malloc # address is in %eax
    popl %edx # expr []
    movl %edx, (%eax)
    popl %edx # expr []'
    movl %edx, 4(%eax)
    pushl %eax
    # 2.4. assign to var x
    popl %edx # expr [] : []' in %edx
    movl %edx, -4(%ebp) # move into first local (x)
  # 3. x.hd = 23 : [];
    # 3.1. put expr [] on stack
    pushl $0
    # 3.2. put expr 23 on stack
    pushl $23
    # 3.3. put expr 23 : [] on stack
    movl $8, %edi # malloc 8 bytes = 2 int32's
    call malloc # address is in %eax
    popl %edx # expr []
    movl %edx, (%eax)
    popl %edx # expr []'
    movl %edx, 4(%eax)
    pushl %eax
    # 3.4. assign to x.hd
    popl %edx # expr in %edx
    movl -4(%ebp), %eax # expr x in %eax
    movl %edx, (%eax)
  # 4. f(x);
    # 4.1. put expr x on stack
    pushl -4(%ebp)
    # 4.2. call f
    call _f
  # 5. print(x.hd.hd);
    # 5.1. put expr x on stack
    pushl -4(%ebp)
    # 5.2. replace with expr x.hd on stack
    popl %edx # expr x in %edx
    pushl (%edx) # expr x.hd on stack
    # 5.3. replace with expr x.hd.hd on stack
    popl %edx # expr x.hd in %edx
    pushl (%edx) # expr x.hd.hd on stack
    # 5.4. print
    popl %eax
    call _print
  # 6. print(x.hd.tl.hd);
    # 6.1. put expr x on stack
    pushl -4(%ebp)
    # 6.2. replace with expr x.hd on stack
    popl %edx # expr x in %edx
    pushl (%edx) # expr x.hd on stack
    # 6.3. replace with expr x.hd.tl on stack
    popl %edx # expr x.hd in %edx
    pushl 4(%edx) # expr x.hd.tl on stack
    # 6.4. replace with expr x.hd.tl.hd on stack
    popl %edx # expr x.hd.tl in %edx
    pushl (%edx) # expr x.hd.tl.hd on stack
    # 6.5. print
    popl %eax
    call _print
  # 7. return 1;
  mov $1, %eax
  leave
  ret


# unit f ([[a]] x) {
#   x.hd = x.hd.hd : x.hd;
#   return ();
# }
_f:
  enter $0, $0 # no locals

  # 1. x.hd = x.hd.hd : x.hd;
    # 1.1. put expr x.hd on stack
    mov 8(%ebp), %edx # access arg 1 = x
    pushl (%edx)
    # 1.2. put expr x.hd.hd on stack
    mov 8(%ebp), %edx # access arg 1 = x
    movl (%edx), %edx
    pushl (%edx)
    # 1.3. replace with expr x.hd.hd : x.hd on stack
    movl $8, %edi # malloc 8 bytes = 2 int32's
    call malloc # address is in %eax
    popl %edx # expr x.hd.hd
    movl %edx, (%eax)
    popl %edx # expr x.hd
    movl %edx, 4(%eax)
    pushl %eax # expr x.hd.hd : x.hd on stack
    # 1.4. assign to x.hd
    popl %edx # value expr in %edx
    movl 8(%ebp), %eax # expr x (= arg 1) in %eax
    movl %edx, (%eax)
  # 2. return ();
  movl $0, %eax
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
