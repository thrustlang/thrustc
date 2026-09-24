; ModuleID = 'large_nested_struct_arg.c'
source_filename = "large_nested_struct_arg.c"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

%struct.Outer = type { %struct.Inner, i64 }
%struct.Inner = type { i64, i64 }

; Function Attrs: noinline nounwind optnone sspstrong uwtable
define dso_local i32 @check_outer(ptr noundef byval(%struct.Outer) align 8 %0) #0 {
  %2 = alloca i32, align 4
  %3 = getelementptr inbounds nuw %struct.Outer, ptr %0, i32 0, i32 0
  %4 = getelementptr inbounds nuw %struct.Inner, ptr %3, i32 0, i32 0
  %5 = load i64, ptr %4, align 8
  %6 = icmp ne i64 %5, 1
  br i1 %6, label %7, label %8

7:                                                ; preds = %1
  store i32 1, ptr %2, align 4
  br label %20

8:                                                ; preds = %1
  %9 = getelementptr inbounds nuw %struct.Outer, ptr %0, i32 0, i32 0
  %10 = getelementptr inbounds nuw %struct.Inner, ptr %9, i32 0, i32 1
  %11 = load i64, ptr %10, align 8
  %12 = icmp ne i64 %11, 2
  br i1 %12, label %13, label %14

13:                                               ; preds = %8
  store i32 2, ptr %2, align 4
  br label %20

14:                                               ; preds = %8
  %15 = getelementptr inbounds nuw %struct.Outer, ptr %0, i32 0, i32 1
  %16 = load i64, ptr %15, align 8
  %17 = icmp ne i64 %16, 3
  br i1 %17, label %18, label %19

18:                                               ; preds = %14
  store i32 3, ptr %2, align 4
  br label %20

19:                                               ; preds = %14
  store i32 0, ptr %2, align 4
  br label %20

20:                                               ; preds = %19, %18, %13, %7
  %21 = load i32, ptr %2, align 4
  ret i32 %21
}

attributes #0 = { noinline nounwind optnone sspstrong uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }

!llvm.module.flags = !{!0, !1, !2, !3, !4}
!llvm.ident = !{!5}

!0 = !{i32 1, !"wchar_size", i32 4}
!1 = !{i32 8, !"PIC Level", i32 2}
!2 = !{i32 7, !"PIE Level", i32 2}
!3 = !{i32 7, !"uwtable", i32 2}
!4 = !{i32 7, !"frame-pointer", i32 2}
!5 = !{!"clang version 22.1.8"}
