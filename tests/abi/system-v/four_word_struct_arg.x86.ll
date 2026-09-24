; ModuleID = 'four_word_struct_arg.c'
source_filename = "four_word_struct_arg.c"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

%struct.Wide = type { i64, i64, i64, i64 }

; Function Attrs: noinline nounwind optnone sspstrong uwtable
define dso_local i32 @check_wide(ptr noundef byval(%struct.Wide) align 8 %0) #0 {
  %2 = alloca i32, align 4
  %3 = getelementptr inbounds nuw %struct.Wide, ptr %0, i32 0, i32 0
  %4 = load i64, ptr %3, align 8
  %5 = icmp ne i64 %4, 1
  br i1 %5, label %6, label %7

6:                                                ; preds = %1
  store i32 1, ptr %2, align 4
  br label %23

7:                                                ; preds = %1
  %8 = getelementptr inbounds nuw %struct.Wide, ptr %0, i32 0, i32 1
  %9 = load i64, ptr %8, align 8
  %10 = icmp ne i64 %9, 2
  br i1 %10, label %11, label %12

11:                                               ; preds = %7
  store i32 2, ptr %2, align 4
  br label %23

12:                                               ; preds = %7
  %13 = getelementptr inbounds nuw %struct.Wide, ptr %0, i32 0, i32 2
  %14 = load i64, ptr %13, align 8
  %15 = icmp ne i64 %14, 3
  br i1 %15, label %16, label %17

16:                                               ; preds = %12
  store i32 3, ptr %2, align 4
  br label %23

17:                                               ; preds = %12
  %18 = getelementptr inbounds nuw %struct.Wide, ptr %0, i32 0, i32 3
  %19 = load i64, ptr %18, align 8
  %20 = icmp ne i64 %19, 4
  br i1 %20, label %21, label %22

21:                                               ; preds = %17
  store i32 4, ptr %2, align 4
  br label %23

22:                                               ; preds = %17
  store i32 0, ptr %2, align 4
  br label %23

23:                                               ; preds = %22, %21, %16, %11, %6
  %24 = load i32, ptr %2, align 4
  ret i32 %24
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
