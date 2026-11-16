; ModuleID = 'struct_nested_vector_arg_return.thrust'
source_filename = "struct_nested_vector_arg_return.thrust"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

; Function Attrs: inlinehint norecurse nounwind sspstrong uwtable
define void @struct_nested_vector_arg_return.identity(ptr noundef writeonly sret({ { <8 x float> }, i32 }) align 32 dereferenceable(1) %0, ptr noundef readonly byval({ { <8 x float> }, i32 }) align 32 dereferenceable(1) %1) #0 {
LKjuyqKrqPo:
  %2 = load { { <8 x float> }, i32 }, ptr %1, align 32
  store { { <8 x float> }, i32 } %2, ptr %0, align 32
  ret void
}

; Function Attrs: norecurse nounwind sspstrong uwtable
define i32 @main() #1 {
tRKbf:
  %local.input = alloca { { <8 x float> }, i32 }, align 32
  %0 = alloca { <8 x float> }, align 32
  %local.result = alloca { { <8 x float> }, i32 }, align 32
  %local.value = alloca <8 x float>, align 32
  %1 = getelementptr inbounds { <8 x float> }, ptr %0, i32 0, i32 0
  store <8 x float> <float 1.000000e+00, float 2.000000e+00, float 3.000000e+00, float 4.000000e+00, float 5.000000e+00, float 6.000000e+00, float 7.000000e+00, float 8.000000e+00>, ptr %1, align 32
  %2 = load { <8 x float> }, ptr %0, align 32
  %3 = getelementptr inbounds { { <8 x float> }, i32 }, ptr %local.input, i32 0, i32 0
  store { <8 x float> } %2, ptr %3, align 32
  %4 = getelementptr inbounds { { <8 x float> }, i32 }, ptr %local.input, i32 0, i32 1
  store i32 7, ptr %4, align 4
  %5 = load { { <8 x float> }, i32 }, ptr %local.input, align 32
  %6 = alloca { { <8 x float> }, i32 }, align 32
  %7 = alloca { { <8 x float> }, i32 }, align 32
  store { { <8 x float> }, i32 } %5, ptr %7, align 32
  call void @struct_nested_vector_arg_return.identity(ptr %6, ptr %7)
  %8 = load { { <8 x float> }, i32 }, ptr %6, align 32
  store { { <8 x float> }, i32 } %8, ptr %local.result, align 32
  %9 = getelementptr inbounds { { <8 x float> }, i32 }, ptr %local.result, i32 0, i32 0
  %10 = getelementptr inbounds { <8 x float> }, ptr %9, i32 0, i32 0
  %11 = load <8 x float>, ptr %10, align 32
  store <8 x float> %11, ptr %local.value, align 32
  %12 = load <8 x float>, ptr %local.value, align 32
  %13 = extractelement <8 x float> %12, i32 0
  %14 = fcmp one float %13, 1.000000e+00
  br i1 %14, label %xJauUEl, label %lstdP

lstdP:                                            ; preds = %tRKbf
  %15 = load <8 x float>, ptr %local.value, align 32
  %16 = extractelement <8 x float> %15, i32 7
  %17 = fcmp one float %16, 8.000000e+00
  br i1 %17, label %kCrUhVm, label %iBdPd

xJauUEl:                                          ; preds = %tRKbf
  ret i32 1

iBdPd:                                            ; preds = %lstdP
  %18 = getelementptr inbounds { { <8 x float> }, i32 }, ptr %local.result, i32 0, i32 1
  %19 = load i32, ptr %18, align 4
  %20 = icmp ne i32 %19, 7
  br i1 %20, label %DllXwhL, label %RpekVeU

kCrUhVm:                                          ; preds = %lstdP
  ret i32 2

DllXwhL:                                          ; preds = %iBdPd
  ret i32 3

RpekVeU:                                          ; preds = %iBdPd
  ret i32 0
}

attributes #0 = { inlinehint norecurse nounwind sspstrong uwtable "frame-pointer"="all" "no-trapping-math"="true" "target-cpu"="skylake" "target-features"="+sse2,+avx" "tune-cpu"="skylake" }
attributes #1 = { norecurse nounwind sspstrong uwtable "frame-pointer"="all" "no-trapping-math"="true" "target-cpu"="skylake" "target-features"="+sse2,+avx" "tune-cpu"="skylake" }

!llvm.module.flags = !{!0, !1, !2, !3, !4, !5}
!llvm.ident = !{!6}
!build = !{!7}

!0 = !{i32 8, !"PIC Level", i32 1}
!1 = !{i32 7, !"PIE Level", i32 1}
!2 = !{i32 1, !"Code Model", i32 0}
!3 = !{i32 1, !"target-abi", !"gnu"}
!4 = !{i32 7, !"frame-pointer", i32 2}
!5 = !{i32 7, !"uwtable", i32 2}
!6 = !{!"thrustc version 0.2.6"}
!7 = !{!"080668f4-f8e7-4ea1-8fab-a3b7299edf1d", !"LLVM 17.0.6"}
