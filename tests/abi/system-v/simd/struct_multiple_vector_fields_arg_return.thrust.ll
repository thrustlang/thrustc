; ModuleID = 'struct_multiple_vector_fields_arg_return.thrust'
source_filename = "struct_multiple_vector_fields_arg_return.thrust"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

; Function Attrs: inlinehint norecurse nounwind sspstrong uwtable
define void @struct_multiple_vector_fields_arg_return.identity(ptr noundef writeonly sret({ <8 x float>, <8 x float> }) align 32 dereferenceable(1) %0, ptr noundef readonly byval({ <8 x float>, <8 x float> }) align 32 dereferenceable(1) %1) #0 {
zjsRImRSBexF:
  %2 = load { <8 x float>, <8 x float> }, ptr %1, align 32
  store { <8 x float>, <8 x float> } %2, ptr %0, align 32
  ret void
}

; Function Attrs: norecurse nounwind sspstrong uwtable
define i32 @main() #1 {
agiOMsNYFBD:
  %local.input = alloca { <8 x float>, <8 x float> }, align 32
  %local.result = alloca { <8 x float>, <8 x float> }, align 32
  %local.left = alloca <8 x float>, align 32
  %local.right = alloca <8 x float>, align 32
  %0 = getelementptr inbounds { <8 x float>, <8 x float> }, ptr %local.input, i32 0, i32 0
  store <8 x float> <float 1.000000e+00, float 2.000000e+00, float 3.000000e+00, float 4.000000e+00, float 5.000000e+00, float 6.000000e+00, float 7.000000e+00, float 8.000000e+00>, ptr %0, align 32
  %1 = getelementptr inbounds { <8 x float>, <8 x float> }, ptr %local.input, i32 0, i32 1
  store <8 x float> <float 8.000000e+00, float 7.000000e+00, float 6.000000e+00, float 5.000000e+00, float 4.000000e+00, float 3.000000e+00, float 2.000000e+00, float 1.000000e+00>, ptr %1, align 32
  %2 = load { <8 x float>, <8 x float> }, ptr %local.input, align 32
  %3 = alloca { <8 x float>, <8 x float> }, align 32
  %4 = alloca { <8 x float>, <8 x float> }, align 32
  store { <8 x float>, <8 x float> } %2, ptr %4, align 32
  call void @struct_multiple_vector_fields_arg_return.identity(ptr %3, ptr %4)
  %5 = load { <8 x float>, <8 x float> }, ptr %3, align 32
  store { <8 x float>, <8 x float> } %5, ptr %local.result, align 32
  %6 = getelementptr inbounds { <8 x float>, <8 x float> }, ptr %local.result, i32 0, i32 0
  %7 = load <8 x float>, ptr %6, align 32
  store <8 x float> %7, ptr %local.left, align 32
  %8 = getelementptr inbounds { <8 x float>, <8 x float> }, ptr %local.result, i32 0, i32 1
  %9 = load <8 x float>, ptr %8, align 32
  store <8 x float> %9, ptr %local.right, align 32
  %10 = load <8 x float>, ptr %local.left, align 32
  %11 = extractelement <8 x float> %10, i32 0
  %12 = fcmp one float %11, 1.000000e+00
  br i1 %12, label %bvzdusH, label %rugFJkqpid

rugFJkqpid:                                       ; preds = %agiOMsNYFBD
  %13 = load <8 x float>, ptr %local.right, align 32
  %14 = extractelement <8 x float> %13, i32 7
  %15 = fcmp one float %14, 1.000000e+00
  br i1 %15, label %QwhxZa, label %zVRMjrzri

bvzdusH:                                          ; preds = %agiOMsNYFBD
  ret i32 1

QwhxZa:                                           ; preds = %rugFJkqpid
  ret i32 2

zVRMjrzri:                                        ; preds = %rugFJkqpid
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
!7 = !{!"dc6042a9-5532-4a33-be0e-4712664cc100", !"LLVM 17.0.6"}
