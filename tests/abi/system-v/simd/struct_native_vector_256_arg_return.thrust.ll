; ModuleID = 'struct_native_vector_256_arg_return.thrust'
source_filename = "struct_native_vector_256_arg_return.thrust"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

; Function Attrs: inlinehint norecurse nounwind sspstrong memory(none) uwtable
define { <8 x float> } @struct_native_vector_256_arg_return.identity({ <8 x float> } noundef %value) #0 {
gkTiCRadtHIe:
  ret { <8 x float> } %value
}

; Function Attrs: norecurse nounwind sspstrong uwtable
define i32 @main() #1 {
hBgVeNU:
  %local.input = alloca { <8 x float> }, align 32
  %local.result = alloca { <8 x float> }, align 32
  %local.resultValue = alloca <8 x float>, align 32
  %0 = getelementptr inbounds { <8 x float> }, ptr %local.input, i32 0, i32 0
  store <8 x float> <float 1.000000e+00, float 2.000000e+00, float 3.000000e+00, float 4.000000e+00, float 5.000000e+00, float 6.000000e+00, float 7.000000e+00, float 8.000000e+00>, ptr %0, align 32
  %1 = load { <8 x float> }, ptr %local.input, align 32
  %2 = call { <8 x float> } @struct_native_vector_256_arg_return.identity({ <8 x float> } %1)
  store { <8 x float> } %2, ptr %local.result, align 32
  %3 = getelementptr inbounds { <8 x float> }, ptr %local.result, i32 0, i32 0
  %4 = load <8 x float>, ptr %3, align 32
  store <8 x float> %4, ptr %local.resultValue, align 32
  %5 = load <8 x float>, ptr %local.resultValue, align 32
  %6 = extractelement <8 x float> %5, i32 0
  %7 = fcmp one float %6, 1.000000e+00
  br i1 %7, label %woVDrTnmlNXt, label %LiSvPbEX

LiSvPbEX:                                         ; preds = %hBgVeNU
  %8 = load <8 x float>, ptr %local.resultValue, align 32
  %9 = extractelement <8 x float> %8, i32 7
  %10 = fcmp one float %9, 8.000000e+00
  br i1 %10, label %Xxmkk, label %snAqBt

woVDrTnmlNXt:                                     ; preds = %hBgVeNU
  ret i32 1

Xxmkk:                                            ; preds = %LiSvPbEX
  ret i32 2

snAqBt:                                           ; preds = %LiSvPbEX
  ret i32 0
}

attributes #0 = { inlinehint norecurse nounwind sspstrong memory(none) uwtable "frame-pointer"="all" "no-trapping-math"="true" "target-cpu"="skylake" "target-features"="+sse2,+avx" "tune-cpu"="skylake" }
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
!7 = !{!"f182a972-8b6f-4a02-b512-960928e159a9", !"LLVM 17.0.6"}
