; ModuleID = 'integer_per_element_bool16_arg_return.thrust'
source_filename = "integer_per_element_bool16_arg_return.thrust"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

; Function Attrs: norecurse nounwind sspstrong memory(argmem: write) uwtable
define void @integer_per_element_bool16_arg_return.identity(ptr noundef writeonly sret(<16 x i1>) align 2 dereferenceable(1) %0, i1 noundef %1, i1 noundef %2, i1 noundef %3, i1 noundef %4, i1 noundef %5, i1 noundef %6, i1 noundef %7, i1 noundef %8, i1 noundef %9, i1 noundef %10, i1 noundef %11, i1 noundef %12, i1 noundef %13, i1 noundef %14, i1 noundef %15, i1 noundef %16) #0 {
TVBTCfM:
  %17 = insertelement <16 x i1> undef, i1 %1, i32 0
  %18 = insertelement <16 x i1> %17, i1 %2, i32 1
  %19 = insertelement <16 x i1> %18, i1 %3, i32 2
  %20 = insertelement <16 x i1> %19, i1 %4, i32 3
  %21 = insertelement <16 x i1> %20, i1 %5, i32 4
  %22 = insertelement <16 x i1> %21, i1 %6, i32 5
  %23 = insertelement <16 x i1> %22, i1 %7, i32 6
  %24 = insertelement <16 x i1> %23, i1 %8, i32 7
  %25 = insertelement <16 x i1> %24, i1 %9, i32 8
  %26 = insertelement <16 x i1> %25, i1 %10, i32 9
  %27 = insertelement <16 x i1> %26, i1 %11, i32 10
  %28 = insertelement <16 x i1> %27, i1 %12, i32 11
  %29 = insertelement <16 x i1> %28, i1 %13, i32 12
  %30 = insertelement <16 x i1> %29, i1 %14, i32 13
  %31 = insertelement <16 x i1> %30, i1 %15, i32 14
  %value = insertelement <16 x i1> %31, i1 %16, i32 15
  store <16 x i1> %value, ptr %0, align 16
  ret void
}

; Function Attrs: norecurse nounwind sspstrong uwtable
define i32 @main() #1 {
TuDNwp:
  %local.input = alloca <16 x i1>, align 2
  %local.result = alloca <16 x i1>, align 2
  store <16 x i1> <i1 true, i1 false, i1 true, i1 false, i1 true, i1 false, i1 true, i1 false, i1 true, i1 false, i1 true, i1 false, i1 true, i1 false, i1 true, i1 false>, ptr %local.input, align 2
  %0 = load <16 x i1>, ptr %local.input, align 2
  %1 = alloca <16 x i1>, align 2
  %2 = extractelement <16 x i1> %0, i32 0
  %3 = extractelement <16 x i1> %0, i32 1
  %4 = extractelement <16 x i1> %0, i32 2
  %5 = extractelement <16 x i1> %0, i32 3
  %6 = extractelement <16 x i1> %0, i32 4
  %7 = extractelement <16 x i1> %0, i32 5
  %8 = extractelement <16 x i1> %0, i32 6
  %9 = extractelement <16 x i1> %0, i32 7
  %10 = extractelement <16 x i1> %0, i32 8
  %11 = extractelement <16 x i1> %0, i32 9
  %12 = extractelement <16 x i1> %0, i32 10
  %13 = extractelement <16 x i1> %0, i32 11
  %14 = extractelement <16 x i1> %0, i32 12
  %15 = extractelement <16 x i1> %0, i32 13
  %16 = extractelement <16 x i1> %0, i32 14
  %17 = extractelement <16 x i1> %0, i32 15
  call void @integer_per_element_bool16_arg_return.identity(ptr %1, i1 %2, i1 %3, i1 %4, i1 %5, i1 %6, i1 %7, i1 %8, i1 %9, i1 %10, i1 %11, i1 %12, i1 %13, i1 %14, i1 %15, i1 %16, i1 %17)
  %18 = load <16 x i1>, ptr %1, align 16
  store <16 x i1> %18, ptr %local.result, align 2
  %19 = load <16 x i1>, ptr %local.result, align 2
  %20 = extractelement <16 x i1> %19, i32 0
  %21 = icmp ne i1 %20, true
  br i1 %21, label %CcfFYaDOgG, label %IuZIRyxQOSvj

IuZIRyxQOSvj:                                     ; preds = %TuDNwp
  %22 = load <16 x i1>, ptr %local.result, align 2
  %23 = extractelement <16 x i1> %22, i32 1
  %24 = icmp ne i1 %23, false
  br i1 %24, label %FJRRTU, label %rRGNdKi

CcfFYaDOgG:                                       ; preds = %TuDNwp
  ret i32 1

rRGNdKi:                                          ; preds = %IuZIRyxQOSvj
  %25 = load <16 x i1>, ptr %local.result, align 2
  %26 = extractelement <16 x i1> %25, i32 15
  %27 = icmp ne i1 %26, false
  br i1 %27, label %VWgyyyc, label %wtslyUrpA

FJRRTU:                                           ; preds = %IuZIRyxQOSvj
  ret i32 2

VWgyyyc:                                          ; preds = %rRGNdKi
  ret i32 3

wtslyUrpA:                                        ; preds = %rRGNdKi
  ret i32 0
}

attributes #0 = { norecurse nounwind sspstrong memory(argmem: write) uwtable "frame-pointer"="all" "no-trapping-math"="true" "target-cpu"="skylake" "target-features"="+prfchw,-cldemote,+avx,+aes,+sahf,+pclmul,-xop,+crc32,+xsaves,-avx512fp16,-sm4,+sse4.1,-avx512ifma,+xsave,-avx512pf,+sse4.2,-tsxldtrk,-ptwrite,-widekl,-sm3,+invpcid,+64bit,+xsavec,-avx512vpopcntdq,+cmov,-avx512vp2intersect,-avx512cd,+movbe,-avxvnniint8,-avx512er,-amx-int8,-kl,-sha512,-avxvnni,-rtm,+adx,+avx2,-hreset,-movdiri,-serialize,-vpclmulqdq,-avx512vl,-uintr,+clflushopt,-raoint,-cmpccxadd,+bmi,-amx-tile,+sse,-gfni,-avxvnniint16,-amx-fp16,+xsaveopt,+rdrnd,-avx512f,-amx-bf16,-avx512bf16,-avx512vnni,+cx8,-avx512bw,+sse3,-pku,+fsgsbase,-clzero,-mwaitx,-lwp,+lzcnt,-sha,-movdir64b,-wbnoinvd,-enqcmd,-prefetchwt1,-avxneconvert,-tbm,-pconfig,-amx-complex,+ssse3,+cx16,+bmi2,+fma,+popcnt,-avxifma,+f16c,-avx512bitalg,-rdpru,-clwb,+mmx,+sse2,+rdseed,-avx512vbmi2,-prefetchi,-rdpid,-fma4,-avx512vbmi,-shstk,-vaes,-waitpkg,+sgx,+fxsr,-avx512dq,-sse4a" "tune-cpu"="skylake" }
attributes #1 = { norecurse nounwind sspstrong uwtable "frame-pointer"="all" "no-trapping-math"="true" "target-cpu"="skylake" "target-features"="+prfchw,-cldemote,+avx,+aes,+sahf,+pclmul,-xop,+crc32,+xsaves,-avx512fp16,-sm4,+sse4.1,-avx512ifma,+xsave,-avx512pf,+sse4.2,-tsxldtrk,-ptwrite,-widekl,-sm3,+invpcid,+64bit,+xsavec,-avx512vpopcntdq,+cmov,-avx512vp2intersect,-avx512cd,+movbe,-avxvnniint8,-avx512er,-amx-int8,-kl,-sha512,-avxvnni,-rtm,+adx,+avx2,-hreset,-movdiri,-serialize,-vpclmulqdq,-avx512vl,-uintr,+clflushopt,-raoint,-cmpccxadd,+bmi,-amx-tile,+sse,-gfni,-avxvnniint16,-amx-fp16,+xsaveopt,+rdrnd,-avx512f,-amx-bf16,-avx512bf16,-avx512vnni,+cx8,-avx512bw,+sse3,-pku,+fsgsbase,-clzero,-mwaitx,-lwp,+lzcnt,-sha,-movdir64b,-wbnoinvd,-enqcmd,-prefetchwt1,-avxneconvert,-tbm,-pconfig,-amx-complex,+ssse3,+cx16,+bmi2,+fma,+popcnt,-avxifma,+f16c,-avx512bitalg,-rdpru,-clwb,+mmx,+sse2,+rdseed,-avx512vbmi2,-prefetchi,-rdpid,-fma4,-avx512vbmi,-shstk,-vaes,-waitpkg,+sgx,+fxsr,-avx512dq,-sse4a" "tune-cpu"="skylake" }

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
!7 = !{!"43b20a75-a1db-49f5-998d-e27869bc83c5", !"LLVM 17.0.6"}
