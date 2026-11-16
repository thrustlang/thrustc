; ModuleID = 'native_vector_512_arg_return.thrust'
source_filename = "native_vector_512_arg_return.thrust"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

; Function Attrs: inlinehint norecurse nounwind sspstrong memory(none) uwtable
define <16 x float> @native_vector_512_arg_return.addVectors(<16 x float> noundef %left, <16 x float> noundef %right) #0 {
plbFBV:
  %0 = fadd <16 x float> %left, %right
  ret <16 x float> %0
}

; Function Attrs: norecurse nounwind sspstrong uwtable
define i32 @main() #1 {
MINLp:
  %local.left = alloca <16 x float>, align 64
  %local.right = alloca <16 x float>, align 64
  %local.result = alloca <16 x float>, align 64
  store <16 x float> <float 1.000000e+00, float 2.000000e+00, float 3.000000e+00, float 4.000000e+00, float 5.000000e+00, float 6.000000e+00, float 7.000000e+00, float 8.000000e+00, float 9.000000e+00, float 1.000000e+01, float 1.100000e+01, float 1.200000e+01, float 1.300000e+01, float 1.400000e+01, float 1.500000e+01, float 1.600000e+01>, ptr %local.left, align 64
  store <16 x float> <float 1.600000e+01, float 1.500000e+01, float 1.400000e+01, float 1.300000e+01, float 1.200000e+01, float 1.100000e+01, float 1.000000e+01, float 9.000000e+00, float 8.000000e+00, float 7.000000e+00, float 6.000000e+00, float 5.000000e+00, float 4.000000e+00, float 3.000000e+00, float 2.000000e+00, float 1.000000e+00>, ptr %local.right, align 64
  %0 = load <16 x float>, ptr %local.left, align 64
  %1 = load <16 x float>, ptr %local.right, align 64
  %2 = call <16 x float> @native_vector_512_arg_return.addVectors(<16 x float> %0, <16 x float> %1)
  store <16 x float> %2, ptr %local.result, align 64
  %3 = load <16 x float>, ptr %local.result, align 64
  %4 = extractelement <16 x float> %3, i32 0
  %5 = fcmp one float %4, 1.700000e+01
  br i1 %5, label %fWITKtBTTsG, label %cCgrUblUFseQ

cCgrUblUFseQ:                                     ; preds = %MINLp
  %6 = load <16 x float>, ptr %local.result, align 64
  %7 = extractelement <16 x float> %6, i32 15
  %8 = fcmp one float %7, 1.700000e+01
  br i1 %8, label %SKXjnFH, label %kAlmJmWvsFZu

fWITKtBTTsG:                                      ; preds = %MINLp
  ret i32 1

SKXjnFH:                                          ; preds = %cCgrUblUFseQ
  ret i32 2

kAlmJmWvsFZu:                                     ; preds = %cCgrUblUFseQ
  ret i32 0
}

attributes #0 = { inlinehint norecurse nounwind sspstrong memory(none) uwtable "frame-pointer"="all" "no-trapping-math"="true" "target-cpu"="skylake" "target-features"="+sse2,+avx512f" "tune-cpu"="skylake" }
attributes #1 = { norecurse nounwind sspstrong uwtable "frame-pointer"="all" "no-trapping-math"="true" "target-cpu"="skylake" "target-features"="+sse2,+avx512f" "tune-cpu"="skylake" }

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
!7 = !{!"2996b6e0-ad66-492b-b10d-a3ea386dded3", !"LLVM 17.0.6"}
