; ModuleID = 'mixed_width_struct_arg.c'
source_filename = "mixed_width_struct_arg.c"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

%struct.Mixed = type { i8, i16, i32 }

; Function Attrs: noinline nounwind optnone sspstrong uwtable
define dso_local i32 @check_mixed(i64 %0) #0 {
  %2 = alloca i32, align 4
  %3 = alloca %struct.Mixed, align 4
  store i64 %0, ptr %3, align 4
  %4 = getelementptr inbounds nuw %struct.Mixed, ptr %3, i32 0, i32 0
  %5 = load i8, ptr %4, align 4
  %6 = zext i8 %5 to i32
  %7 = icmp ne i32 %6, 1
  br i1 %7, label %8, label %9

8:                                                ; preds = %1
  store i32 1, ptr %2, align 4
  br label %21

9:                                                ; preds = %1
  %10 = getelementptr inbounds nuw %struct.Mixed, ptr %3, i32 0, i32 1
  %11 = load i16, ptr %10, align 2
  %12 = zext i16 %11 to i32
  %13 = icmp ne i32 %12, 2
  br i1 %13, label %14, label %15

14:                                               ; preds = %9
  store i32 2, ptr %2, align 4
  br label %21

15:                                               ; preds = %9
  %16 = getelementptr inbounds nuw %struct.Mixed, ptr %3, i32 0, i32 2
  %17 = load i32, ptr %16, align 4
  %18 = icmp ne i32 %17, 3
  br i1 %18, label %19, label %20

19:                                               ; preds = %15
  store i32 3, ptr %2, align 4
  br label %21

20:                                               ; preds = %15
  store i32 0, ptr %2, align 4
  br label %21

21:                                               ; preds = %20, %19, %14, %8
  %22 = load i32, ptr %2, align 4
  ret i32 %22
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
