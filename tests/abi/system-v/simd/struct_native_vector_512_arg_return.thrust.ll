; ModuleID = 'struct_native_vector_512_arg_return.thrust'
source_filename = "struct_native_vector_512_arg_return.thrust"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

; Function Attrs: inlinehint norecurse nounwind sspstrong memory(none) uwtable
define { <16 x float> } @struct_native_vector_512_arg_return.identity({ <16 x float> } noundef %value) #0 {
otKGeoPBTQqD:
  ret { <16 x float> } %value
}

; Function Attrs: norecurse nounwind sspstrong uwtable
define i32 @main() #1 {
mSNkwaLQvR:
  %local.input = alloca { <16 x float> }, align 64
  %local.result = alloca { <16 x float> }, align 64
  %local.resultValue = alloca <16 x float>, align 64
  %0 = getelementptr inbounds { <16 x float> }, ptr %local.input, i32 0, i32 0
  store <16 x float> <float 1.000000e+00, float 2.000000e+00, float 3.000000e+00, float 4.000000e+00, float 5.000000e+00, float 6.000000e+00, float 7.000000e+00, float 8.000000e+00, float 9.000000e+00, float 1.000000e+01, float 1.100000e+01, float 1.200000e+01, float 1.300000e+01, float 1.400000e+01, float 1.500000e+01, float 1.600000e+01>, ptr %0, align 64
  %1 = load { <16 x float> }, ptr %local.input, align 64
  %2 = call { <16 x float> } @struct_native_vector_512_arg_return.identity({ <16 x float> } %1)
  store { <16 x float> } %2, ptr %local.result, align 64
  %3 = getelementptr inbounds { <16 x float> }, ptr %local.result, i32 0, i32 0
  %4 = load <16 x float>, ptr %3, align 64
  store <16 x float> %4, ptr %local.resultValue, align 64
  %5 = load <16 x float>, ptr %local.resultValue, align 64
  %6 = extractelement <16 x float> %5, i32 0
  %7 = fcmp one float %6, 1.000000e+00
  br i1 %7, label %vdklexqP, label %IQMjMRTlH

IQMjMRTlH:                                        ; preds = %mSNkwaLQvR
  %8 = load <16 x float>, ptr %local.resultValue, align 64
  %9 = extractelement <16 x float> %8, i32 15
  %10 = fcmp one float %9, 1.600000e+01
  br i1 %10, label %AuJjpwtWH, label %IgIVcWjAnxc

vdklexqP:                                         ; preds = %mSNkwaLQvR
  ret i32 1

AuJjpwtWH:                                        ; preds = %IQMjMRTlH
  ret i32 2

IgIVcWjAnxc:                                      ; preds = %IQMjMRTlH
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
!7 = !{!"bd9af5e5-d8f8-407d-ba44-b007d2fcdbb4", !"LLVM 17.0.6"}
