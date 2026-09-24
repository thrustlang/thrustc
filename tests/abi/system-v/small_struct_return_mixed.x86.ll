; ModuleID = 'small_struct_return_mixed.c'
source_filename = "small_struct_return_mixed.c"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

%struct.Triplet = type { i8, i8, i8 }

; Function Attrs: noinline nounwind optnone sspstrong uwtable
define dso_local i24 @make_triplet() #0 {
  %1 = alloca %struct.Triplet, align 1
  %2 = alloca i24, align 4
  %3 = getelementptr inbounds nuw %struct.Triplet, ptr %1, i32 0, i32 0
  store i8 5, ptr %3, align 1
  %4 = getelementptr inbounds nuw %struct.Triplet, ptr %1, i32 0, i32 1
  store i8 6, ptr %4, align 1
  %5 = getelementptr inbounds nuw %struct.Triplet, ptr %1, i32 0, i32 2
  store i8 7, ptr %5, align 1
  call void @llvm.memcpy.p0.p0.i64(ptr align 4 %2, ptr align 1 %1, i64 3, i1 false)
  %6 = load i24, ptr %2, align 4
  ret i24 %6
}

; Function Attrs: nocallback nofree nounwind willreturn memory(argmem: readwrite)
declare void @llvm.memcpy.p0.p0.i64(ptr noalias writeonly captures(none), ptr noalias readonly captures(none), i64, i1 immarg) #1

attributes #0 = { noinline nounwind optnone sspstrong uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #1 = { nocallback nofree nounwind willreturn memory(argmem: readwrite) }

!llvm.module.flags = !{!0, !1, !2, !3, !4}
!llvm.ident = !{!5}

!0 = !{i32 1, !"wchar_size", i32 4}
!1 = !{i32 8, !"PIC Level", i32 2}
!2 = !{i32 7, !"PIE Level", i32 2}
!3 = !{i32 7, !"uwtable", i32 2}
!4 = !{i32 7, !"frame-pointer", i32 2}
!5 = !{!"clang version 22.1.8"}
