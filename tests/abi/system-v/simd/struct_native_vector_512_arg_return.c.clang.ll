; ModuleID = 'struct_native_vector_512_arg_return.c'
source_filename = "struct_native_vector_512_arg_return.c"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

%struct.V = type { <16 x float> }

; Function Attrs: noinline nounwind optnone sspstrong uwtable
define dso_local <16 x float> @identity(<16 x float> %0) #0 {
  %2 = alloca %struct.V, align 64
  %3 = alloca %struct.V, align 64
  %4 = getelementptr inbounds nuw %struct.V, ptr %3, i32 0, i32 0
  store <16 x float> %0, ptr %4, align 64
  call void @llvm.memcpy.p0.p0.i64(ptr align 64 %2, ptr align 64 %3, i64 64, i1 false)
  %5 = getelementptr inbounds nuw %struct.V, ptr %2, i32 0, i32 0
  %6 = load <16 x float>, ptr %5, align 64
  ret <16 x float> %6
}

; Function Attrs: nocallback nofree nosync nounwind willreturn memory(argmem: readwrite)
declare void @llvm.memcpy.p0.p0.i64(ptr noalias writeonly captures(none), ptr noalias readonly captures(none), i64, i1 immarg) #1

attributes #0 = { noinline nounwind optnone sspstrong uwtable "frame-pointer"="all" "min-legal-vector-width"="512" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+avx,+avx2,+avx512f,+cmov,+crc32,+cx8,+f16c,+fma,+fxsr,+mmx,+popcnt,+sse,+sse2,+sse3,+sse4.1,+sse4.2,+ssse3,+x87,+xsave" "tune-cpu"="generic" }
attributes #1 = { nocallback nofree nosync nounwind willreturn memory(argmem: readwrite) }

!llvm.module.flags = !{!0, !1, !2, !3}
!llvm.ident = !{!4}

!0 = !{i32 8, !"PIC Level", i32 2}
!1 = !{i32 7, !"PIE Level", i32 2}
!2 = !{i32 7, !"uwtable", i32 2}
!3 = !{i32 7, !"frame-pointer", i32 2}
!4 = !{!"clang version 23.1.5"}
