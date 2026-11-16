; ModuleID = 'complex_x87_f80_vector_arg_return.c'
source_filename = "complex_x87_f80_vector_arg_return.c"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

; Function Attrs: noinline nounwind optnone sspstrong uwtable
define dso_local <2 x x86_fp80> @identity(ptr noundef byval(<2 x x86_fp80>) align 32 %0) #0 {
  %2 = alloca <2 x x86_fp80>, align 32
  %3 = load <2 x x86_fp80>, ptr %0, align 32
  store <2 x x86_fp80> %3, ptr %2, align 32
  %4 = load <2 x x86_fp80>, ptr %2, align 32
  ret <2 x x86_fp80> %4
}

attributes #0 = { noinline nounwind optnone sspstrong uwtable "frame-pointer"="all" "min-legal-vector-width"="160" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }

!llvm.module.flags = !{!0, !1, !2, !3}
!llvm.ident = !{!4}

!0 = !{i32 8, !"PIC Level", i32 2}
!1 = !{i32 7, !"PIE Level", i32 2}
!2 = !{i32 7, !"uwtable", i32 2}
!3 = !{i32 7, !"frame-pointer", i32 2}
!4 = !{!"clang version 23.1.5"}
