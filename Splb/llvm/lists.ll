;
; int main () {
;   [[int]] x;
;   x := [] : [];
;   x.hd := 5 : [];
;   f(x);
;   // x = [[5, 5]]
;   print (x.hd.hd);           // 5
;   print (x.hd.tl.hd);        // 5
;   return 1;                  // 1
; }
; 
; unit f ([[a]] x) {
;   x.hd := x.hd.hd : x.hd;
;   return ();
; }
;

declare i32 @printf(i8*, ...) #1
declare noalias i8* @malloc(i32)
declare void @free(i8* nocapture)

@.print_int_v_str    = private unnamed_addr constant [4 x i8] c"%d\0A\00", align 1
@.print_string_v_str = private unnamed_addr constant [4 x i8] c"%s\0A\00", align 1

@.true_v_str   = private unnamed_addr constant [5 x i8] c"true\00",  align 1
@.false_v_str  = private unnamed_addr constant [6 x i8] c"false\00", align 1
@.unit_v_str   = private unnamed_addr constant [3 x i8] c"()\00",    align 1

%list = type <{ i32, %list* }>

define i32 @main(i32 %argc, i8** %argv) {
  %1 = call noalias i8* @malloc(i32 5)
  ret i32 1 ; return 1
}
