;
;int main () {
;  print(1);
;  print(2);
;  print(3);
;  print(4);
;  print(5);
;  print(true);
;  print(false);
;  print(());
;  print((4, 5));
;  print((4, (5, false)));
;  print(5 : []);
;}

declare i32 @printf(i8*, ...) #1

@.print_int_v_str    = private unnamed_addr constant [4 x i8] c"%d\0A\00", align 1
@.print_string_v_str = private unnamed_addr constant [4 x i8] c"%s\0A\00", align 1

@.true_v_str   = private unnamed_addr constant [5 x i8] c"true\00",  align 1
@.false_v_str  = private unnamed_addr constant [6 x i8] c"false\00", align 1
@.unit_v_str   = private unnamed_addr constant [3 x i8] c"()\00",    align 1


define i32 @main(i32 %argc, i8** %argv) {
  call void @print_int(i32 1)
  call void @print_int(i32 2)
  call void @print_int(i32 3)
  call void @print_int(i32 4)
  call void @print_int(i32 5)

  call void @print_bool(i1 1)
  call void @print_bool(i1 0)

  ret i32 1 ; return 1
}

define void @print_int(i32 %x) {
  call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([4 x i8]* @.print_int_v_str, i32 0, i32 0), i32 %x)
  ret void
}

define void @print_bool(i1 %x) {
  br i1 %x, label %printTrue, label %printFalse
printTrue:
  call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([4 x i8]* @.print_string_v_str, i32 0, i32 0),
    i8* getelementptr inbounds ([5 x i8]* @.true_v_str, i32 0, i32 0))
  ret void
printFalse:
  call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([4 x i8]* @.print_string_v_str, i32 0, i32 0),
    i8* getelementptr inbounds ([6 x i8]* @.false_v_str, i32 0, i32 0))
  ret void
}

define void @print_tuple() {
  ret void
}