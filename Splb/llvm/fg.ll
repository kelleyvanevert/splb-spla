
;int main () {
;  int x;
;  x := f(2, 3);
;  print(x);
;  return x;             // 6
;}
;
;int f (int a, int b) {
;  return a + g(b);
;}
;
;int g (int c) {
;  return c + h();
;}
;
;int h () {
;  return 1;
;}

declare i32 @printf(i8*, ...) #1
@.str = private unnamed_addr constant [4 x i8] c"%d\0A\00", align 1 ; used for printing integers with printf


define i32 @main(i32 %argc, i8** %argv) {
  %x_ptr = alloca i32            ; int x;

  %1 = call i32 @f(i32 2, i32 3) ; x := f(2, 3);
  store i32 %1, i32* %x_ptr
  
  %2 = load i32* %x_ptr          ; <load x value>
  call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([4 x i8]* @.str, i32 0, i32 0), i32 %2) ; print(x);
  ret i32 %2                     ; return <x value>;
}

define i32 @f (i32 %a, i32 %b) {
  %1 = call i32 @g(i32 %b)
  %2 = add i32 %a, %1
  ret i32 %2
}

define i32 @g (i32 %c) {
  %1 = call i32 @h()
  %2 = add i32 %c, %1
  ret i32 %2
}

define i32 @h () {
  ret i32 1
}