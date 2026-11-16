; ModuleID = 'float_combine_four_f32_arg_return.thrust'
source_filename = "float_combine_four_f32_arg_return.thrust"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

; Function Attrs: norecurse nounwind sspstrong uwtable
define { <2 x float>, <2 x float> } @float_combine_four_f32_arg_return.identity(<2 x float> noundef %0, <2 x float> noundef %1) #0 {
Yegpw:
  %2 = alloca { float, float, float, float }, align 8
  %3 = getelementptr inbounds <2 x float>, ptr %2, i32 0
  store <2 x float> %0, ptr %3, align 8
  %4 = getelementptr inbounds <2 x float>, ptr %2, i32 1
  store <2 x float> %1, ptr %4, align 8
  %value = load { float, float, float, float }, ptr %2, align 4
  %5 = alloca { float, float, float, float }, align 8
  store { float, float, float, float } %value, ptr %5, align 4
  %6 = getelementptr inbounds <2 x float>, ptr %5, i32 0
  %7 = load <2 x float>, ptr %6, align 8
  %8 = insertvalue { <2 x float>, <2 x float> } undef, <2 x float> %7, 0
  %9 = getelementptr inbounds <2 x float>, ptr %5, i32 1
  %10 = load <2 x float>, ptr %9, align 8
  %11 = insertvalue { <2 x float>, <2 x float> } %8, <2 x float> %10, 1
  ret { <2 x float>, <2 x float> } %11
}

; Function Attrs: norecurse nounwind sspstrong uwtable
define i32 @main() #0 {
OXZowpnakn:
  %local.input = alloca { float, float, float, float }, align 8
  %local.result = alloca { float, float, float, float }, align 8
  %0 = getelementptr inbounds { float, float, float, float }, ptr %local.input, i32 0, i32 0
  store float 1.000000e+00, ptr %0, align 4
  %1 = getelementptr inbounds { float, float, float, float }, ptr %local.input, i32 0, i32 1
  store float 2.000000e+00, ptr %1, align 4
  %2 = getelementptr inbounds { float, float, float, float }, ptr %local.input, i32 0, i32 2
  store float 3.000000e+00, ptr %2, align 4
  %3 = getelementptr inbounds { float, float, float, float }, ptr %local.input, i32 0, i32 3
  store float 4.000000e+00, ptr %3, align 4
  %4 = load { float, float, float, float }, ptr %local.input, align 8
  %5 = alloca { float, float, float, float }, align 8
  store { float, float, float, float } %4, ptr %5, align 4
  %6 = getelementptr inbounds <2 x float>, ptr %5, i32 0
  %7 = load <2 x float>, ptr %6, align 8
  %8 = getelementptr inbounds <2 x float>, ptr %5, i32 1
  %9 = load <2 x float>, ptr %8, align 8
  %10 = call { <2 x float>, <2 x float> } @float_combine_four_f32_arg_return.identity(<2 x float> %7, <2 x float> %9)
  store { <2 x float>, <2 x float> } %10, ptr %local.result, align 8
  %11 = getelementptr inbounds { float, float, float, float }, ptr %local.result, i32 0, i32 0
  %12 = load float, ptr %11, align 4
  %13 = fcmp one float %12, 1.000000e+00
  br i1 %13, label %nwXYfwrcti, label %hdwJEm

hdwJEm:                                           ; preds = %OXZowpnakn
  %14 = getelementptr inbounds { float, float, float, float }, ptr %local.result, i32 0, i32 3
  %15 = load float, ptr %14, align 4
  %16 = fcmp one float %15, 4.000000e+00
  br i1 %16, label %EhUKCYD, label %RBAkWvWQR

nwXYfwrcti:                                       ; preds = %OXZowpnakn
  ret i32 1

EhUKCYD:                                          ; preds = %hdwJEm
  ret i32 2

RBAkWvWQR:                                        ; preds = %hdwJEm
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
!7 = !{!"99a0499c-18ce-419d-a1b6-38f271f4bd36", !"LLVM 17.0.6"}
