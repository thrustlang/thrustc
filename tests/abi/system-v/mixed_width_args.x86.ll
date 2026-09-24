; ModuleID = 'mixed_width_args.c'
source_filename = "mixed_width_args.c"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

; Function Attrs: noinline nounwind optnone sspstrong uwtable
define dso_local i32 @pick(i8 noundef signext %0, i8 noundef zeroext %1, i16 noundef signext %2, i16 noundef zeroext %3, i32 noundef %4, i32 noundef %5) #0 {
  %7 = alloca i8, align 1
  %8 = alloca i8, align 1
  %9 = alloca i16, align 2
  %10 = alloca i16, align 2
  %11 = alloca i32, align 4
  %12 = alloca i32, align 4
  store i8 %0, ptr %7, align 1
  store i8 %1, ptr %8, align 1
  store i16 %2, ptr %9, align 2
  store i16 %3, ptr %10, align 2
  store i32 %4, ptr %11, align 4
  store i32 %5, ptr %12, align 4
  %13 = load i8, ptr %7, align 1
  %14 = sext i8 %13 to i32
  %15 = load i8, ptr %8, align 1
  %16 = zext i8 %15 to i32
  %17 = add nsw i32 %14, %16
  %18 = load i16, ptr %9, align 2
  %19 = sext i16 %18 to i32
  %20 = add nsw i32 %17, %19
  %21 = load i16, ptr %10, align 2
  %22 = zext i16 %21 to i32
  %23 = add nsw i32 %20, %22
  %24 = load i32, ptr %11, align 4
  %25 = add nsw i32 %23, %24
  %26 = load i32, ptr %12, align 4
  %27 = add nsw i32 %25, %26
  ret i32 %27
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
