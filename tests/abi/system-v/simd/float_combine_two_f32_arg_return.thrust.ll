; ModuleID = 'float_combine_two_f32_arg_return.thrust'
source_filename = "float_combine_two_f32_arg_return.thrust"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

; Function Attrs: norecurse nounwind sspstrong uwtable
define <2 x float> @float_combine_two_f32_arg_return.identity(<2 x float> noundef %0) #0 {
SNfSvanq:
  %1 = alloca { float, float }, align 8
  %2 = getelementptr inbounds <2 x float>, ptr %1, i32 0
  store <2 x float> %0, ptr %2, align 8
  %value = load { float, float }, ptr %1, align 4
  %3 = alloca { float, float }, align 8
  store { float, float } %value, ptr %3, align 4
  %4 = load <2 x float>, ptr %3, align 8
  ret <2 x float> %4
}

; Function Attrs: norecurse nounwind sspstrong uwtable
define i32 @main() #0 {
qHcvtH:
  %local.input = alloca { float, float }, align 8
  %local.result = alloca { float, float }, align 8
  %0 = getelementptr inbounds { float, float }, ptr %local.input, i32 0, i32 0
  store float 1.500000e+00, ptr %0, align 4
  %1 = getelementptr inbounds { float, float }, ptr %local.input, i32 0, i32 1
  store float 2.500000e+00, ptr %1, align 4
  %2 = load { float, float }, ptr %local.input, align 8
  %3 = alloca { float, float }, align 8
  store { float, float } %2, ptr %3, align 4
  %4 = getelementptr inbounds <2 x float>, ptr %3, i32 0
  %5 = load <2 x float>, ptr %4, align 8
  %6 = call <2 x float> @float_combine_two_f32_arg_return.identity(<2 x float> %5)
  store <2 x float> %6, ptr %local.result, align 8
  %7 = getelementptr inbounds { float, float }, ptr %local.result, i32 0, i32 0
  %8 = load float, ptr %7, align 4
  %9 = fcmp one float %8, 1.500000e+00
  br i1 %9, label %YcxBwVZIVT, label %yHuILBMsIrU

yHuILBMsIrU:                                      ; preds = %qHcvtH
  %10 = getelementptr inbounds { float, float }, ptr %local.result, i32 0, i32 1
  %11 = load float, ptr %10, align 4
  %12 = fcmp one float %11, 2.500000e+00
  br i1 %12, label %EGmmgCozXd, label %rxAUKXclkz

YcxBwVZIVT:                                       ; preds = %qHcvtH
  ret i32 1

EGmmgCozXd:                                       ; preds = %yHuILBMsIrU
  ret i32 2

rxAUKXclkz:                                       ; preds = %yHuILBMsIrU
  ret i32 0
}

attributes #0 = { norecurse nounwind sspstrong uwtable "frame-pointer"="all" "no-trapping-math"="true" "target-cpu"="skylake" "target-features"="+prfchw,-cldemote,+avx,+aes,+sahf,+pclmul,-xop,+crc32,+xsaves,-avx512fp16,-sm4,+sse4.1,-avx512ifma,+xsave,-avx512pf,+sse4.2,-tsxldtrk,-ptwrite,-widekl,-sm3,+invpcid,+64bit,+xsavec,-avx512vpopcntdq,+cmov,-avx512vp2intersect,-avx512cd,+movbe,-avxvnniint8,-avx512er,-amx-int8,-kl,-sha512,-avxvnni,-rtm,+adx,+avx2,-hreset,-movdiri,-serialize,-vpclmulqdq,-avx512vl,-uintr,+clflushopt,-raoint,-cmpccxadd,+bmi,-amx-tile,+sse,-gfni,-avxvnniint16,-amx-fp16,+xsaveopt,+rdrnd,-avx512f,-amx-bf16,-avx512bf16,-avx512vnni,+cx8,-avx512bw,+sse3,-pku,+fsgsbase,-clzero,-mwaitx,-lwp,+lzcnt,-sha,-movdir64b,-wbnoinvd,-enqcmd,-prefetchwt1,-avxneconvert,-tbm,-pconfig,-amx-complex,+ssse3,+cx16,+bmi2,+fma,+popcnt,-avxifma,+f16c,-avx512bitalg,-rdpru,-clwb,+mmx,+sse2,+rdseed,-avx512vbmi2,-prefetchi,-rdpid,-fma4,-avx512vbmi,-shstk,-vaes,-waitpkg,+sgx,+fxsr,-avx512dq,-sse4a" "tune-cpu"="skylake" }

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
!7 = !{!"597118fa-9b01-428d-b951-3e4c8acf3103", !"LLVM 17.0.6"}
