; ModuleID = 'array_native_vector_arg_return.thrust'
source_filename = "array_native_vector_arg_return.thrust"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

; Function Attrs: inlinehint norecurse nounwind sspstrong uwtable
define void @array_native_vector_arg_return.identity(ptr noundef writeonly sret([2 x <8 x float>]) align 32 dereferenceable(1) %0, ptr noundef readonly byval([2 x <8 x float>]) align 32 dereferenceable(1) %1) #0 {
PceWZZlqFzP:
  %2 = load [2 x <8 x float>], ptr %1, align 32
  store [2 x <8 x float>] %2, ptr %0, align 32
  ret void
}

; Function Attrs: norecurse nounwind sspstrong uwtable
define i32 @main() #1 {
ngVKhUU:
  %local.input = alloca [2 x <8 x float>], align 32
  %local.result = alloca [2 x <8 x float>], align 32
  %0 = load [2 x <8 x float>], ptr %local.input, align 32
  %1 = alloca [2 x <8 x float>], align 32
  %2 = alloca [2 x <8 x float>], align 32
  store [2 x <8 x float>] %0, ptr %2, align 32
  call void @array_native_vector_arg_return.identity(ptr %1, ptr %2)
  %3 = load [2 x <8 x float>], ptr %1, align 32
  store [2 x <8 x float>] %3, ptr %local.result, align 32
  br i1 false, label %mPsLWS, label %ndVlR

mPsLWS:                                           ; preds = %ngVKhUU
  ret i32 1

ndVlR:                                            ; preds = %ngVKhUU
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
!7 = !{!"5c4001eb-6554-40b6-8e8b-b8705004f9fc", !"LLVM 17.0.6"}
