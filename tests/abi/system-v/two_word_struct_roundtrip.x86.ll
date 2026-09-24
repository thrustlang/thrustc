; ModuleID = 'two_word_struct_roundtrip.c'
source_filename = "two_word_struct_roundtrip.c"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

%struct.Pair64 = type { i64, i64 }

; Function Attrs: noinline nounwind optnone sspstrong uwtable
define dso_local i32 @check_pair64(i64 %0, i64 %1) #0 {
  %3 = alloca i32, align 4
  %4 = alloca %struct.Pair64, align 8
  %5 = getelementptr inbounds nuw { i64, i64 }, ptr %4, i32 0, i32 0
  store i64 %0, ptr %5, align 8
  %6 = getelementptr inbounds nuw { i64, i64 }, ptr %4, i32 0, i32 1
  store i64 %1, ptr %6, align 8
  %7 = getelementptr inbounds nuw %struct.Pair64, ptr %4, i32 0, i32 0
  %8 = load i64, ptr %7, align 8
  %9 = icmp ne i64 %8, 7
  br i1 %9, label %10, label %11

10:                                               ; preds = %2
  store i32 1, ptr %3, align 4
  br label %17

11:                                               ; preds = %2
  %12 = getelementptr inbounds nuw %struct.Pair64, ptr %4, i32 0, i32 1
  %13 = load i64, ptr %12, align 8
  %14 = icmp ne i64 %13, 9
  br i1 %14, label %15, label %16

15:                                               ; preds = %11
  store i32 2, ptr %3, align 4
  br label %17

16:                                               ; preds = %11
  store i32 0, ptr %3, align 4
  br label %17

17:                                               ; preds = %16, %15, %10
  %18 = load i32, ptr %3, align 4
  ret i32 %18
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
