; ModuleID = 'small_large_struct_mix.c'
source_filename = "small_large_struct_mix.c"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

%struct.Large = type { i64, i64, i64 }
%struct.Small = type { i16, i16 }

; Function Attrs: noinline nounwind optnone sspstrong uwtable
define dso_local i32 @combine(i32 %0, ptr noundef byval(%struct.Large) align 8 %1) #0 {
  %3 = alloca i32, align 4
  %4 = alloca %struct.Small, align 2
  store i32 %0, ptr %4, align 2
  %5 = getelementptr inbounds nuw %struct.Small, ptr %4, i32 0, i32 0
  %6 = load i16, ptr %5, align 2
  %7 = zext i16 %6 to i32
  %8 = icmp ne i32 %7, 3
  br i1 %8, label %9, label %10

9:                                                ; preds = %2
  store i32 1, ptr %3, align 4
  br label %32

10:                                               ; preds = %2
  %11 = getelementptr inbounds nuw %struct.Small, ptr %4, i32 0, i32 1
  %12 = load i16, ptr %11, align 2
  %13 = zext i16 %12 to i32
  %14 = icmp ne i32 %13, 4
  br i1 %14, label %15, label %16

15:                                               ; preds = %10
  store i32 2, ptr %3, align 4
  br label %32

16:                                               ; preds = %10
  %17 = getelementptr inbounds nuw %struct.Large, ptr %1, i32 0, i32 0
  %18 = load i64, ptr %17, align 8
  %19 = icmp ne i64 %18, 5
  br i1 %19, label %20, label %21

20:                                               ; preds = %16
  store i32 3, ptr %3, align 4
  br label %32

21:                                               ; preds = %16
  %22 = getelementptr inbounds nuw %struct.Large, ptr %1, i32 0, i32 1
  %23 = load i64, ptr %22, align 8
  %24 = icmp ne i64 %23, 6
  br i1 %24, label %25, label %26

25:                                               ; preds = %21
  store i32 4, ptr %3, align 4
  br label %32

26:                                               ; preds = %21
  %27 = getelementptr inbounds nuw %struct.Large, ptr %1, i32 0, i32 2
  %28 = load i64, ptr %27, align 8
  %29 = icmp ne i64 %28, 7
  br i1 %29, label %30, label %31

30:                                               ; preds = %26
  store i32 5, ptr %3, align 4
  br label %32

31:                                               ; preds = %26
  store i32 0, ptr %3, align 4
  br label %32

32:                                               ; preds = %31, %30, %25, %20, %15, %9
  %33 = load i32, ptr %3, align 4
  ret i32 %33
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
