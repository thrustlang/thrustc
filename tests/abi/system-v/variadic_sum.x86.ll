; ModuleID = 'variadic_sum.c'
source_filename = "variadic_sum.c"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

%struct.__va_list_tag = type { i32, i32, ptr, ptr }

; Function Attrs: noinline nounwind optnone sspstrong uwtable
define dso_local i32 @sum(i32 noundef %0, ...) #0 {
  %2 = alloca i32, align 4
  %3 = alloca [1 x %struct.__va_list_tag], align 16
  %4 = alloca i32, align 4
  %5 = alloca i32, align 4
  store i32 %0, ptr %2, align 4
  store i32 0, ptr %4, align 4
  %6 = getelementptr inbounds [1 x %struct.__va_list_tag], ptr %3, i64 0, i64 0
  call void @llvm.va_start.p0(ptr %6)
  store i32 0, ptr %5, align 4
  br label %7

7:                                                ; preds = %30, %1
  %8 = load i32, ptr %5, align 4
  %9 = load i32, ptr %2, align 4
  %10 = icmp slt i32 %8, %9
  br i1 %10, label %11, label %33

11:                                               ; preds = %7
  %12 = getelementptr inbounds [1 x %struct.__va_list_tag], ptr %3, i64 0, i64 0
  %13 = getelementptr inbounds nuw %struct.__va_list_tag, ptr %12, i32 0, i32 0
  %14 = load i32, ptr %13, align 16
  %15 = icmp ule i32 %14, 40
  br i1 %15, label %16, label %21

16:                                               ; preds = %11
  %17 = getelementptr inbounds nuw %struct.__va_list_tag, ptr %12, i32 0, i32 3
  %18 = load ptr, ptr %17, align 16
  %19 = getelementptr i8, ptr %18, i32 %14
  %20 = add i32 %14, 8
  store i32 %20, ptr %13, align 16
  br label %25

21:                                               ; preds = %11
  %22 = getelementptr inbounds nuw %struct.__va_list_tag, ptr %12, i32 0, i32 2
  %23 = load ptr, ptr %22, align 8
  %24 = getelementptr i8, ptr %23, i32 8
  store ptr %24, ptr %22, align 8
  br label %25

25:                                               ; preds = %21, %16
  %26 = phi ptr [ %19, %16 ], [ %23, %21 ]
  %27 = load i32, ptr %26, align 4
  %28 = load i32, ptr %4, align 4
  %29 = add nsw i32 %28, %27
  store i32 %29, ptr %4, align 4
  br label %30

30:                                               ; preds = %25
  %31 = load i32, ptr %5, align 4
  %32 = add nsw i32 %31, 1
  store i32 %32, ptr %5, align 4
  br label %7, !llvm.loop !6

33:                                               ; preds = %7
  %34 = getelementptr inbounds [1 x %struct.__va_list_tag], ptr %3, i64 0, i64 0
  call void @llvm.va_end.p0(ptr %34)
  %35 = load i32, ptr %4, align 4
  ret i32 %35
}

; Function Attrs: nocallback nofree nosync nounwind willreturn
declare void @llvm.va_start.p0(ptr) #1

; Function Attrs: nocallback nofree nosync nounwind willreturn
declare void @llvm.va_end.p0(ptr) #1

attributes #0 = { noinline nounwind optnone sspstrong uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #1 = { nocallback nofree nosync nounwind willreturn }

!llvm.module.flags = !{!0, !1, !2, !3, !4}
!llvm.ident = !{!5}

!0 = !{i32 1, !"wchar_size", i32 4}
!1 = !{i32 8, !"PIC Level", i32 2}
!2 = !{i32 7, !"PIE Level", i32 2}
!3 = !{i32 7, !"uwtable", i32 2}
!4 = !{i32 7, !"frame-pointer", i32 2}
!5 = !{!"clang version 22.1.8"}
!6 = distinct !{!6, !7}
!7 = !{!"llvm.loop.mustprogress"}
