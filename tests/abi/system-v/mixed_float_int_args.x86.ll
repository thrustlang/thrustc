; ModuleID = 'mixed_float_int_args.c'
source_filename = "mixed_float_int_args.c"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

; Function Attrs: noinline nounwind optnone sspstrong uwtable
define dso_local double @pack(float noundef %0, i32 noundef %1, double noundef %2, i64 noundef %3) #0 {
  %5 = alloca float, align 4
  %6 = alloca i32, align 4
  %7 = alloca double, align 8
  %8 = alloca i64, align 8
  store float %0, ptr %5, align 4
  store i32 %1, ptr %6, align 4
  store double %2, ptr %7, align 8
  store i64 %3, ptr %8, align 8
  %9 = load float, ptr %5, align 4
  %10 = fpext float %9 to double
  %11 = load i32, ptr %6, align 4
  %12 = sitofp i32 %11 to double
  %13 = fadd double %10, %12
  %14 = load double, ptr %7, align 8
  %15 = fadd double %13, %14
  %16 = load i64, ptr %8, align 8
  %17 = uitofp i64 %16 to double
  %18 = fadd double %15, %17
  ret double %18
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
