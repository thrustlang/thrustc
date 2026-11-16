/*

    Copyright (C) 2026  Stevens Benavides

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>.

*/

use inkwell::targets::TargetMachine;

#[inline]
pub fn print_all_targets() {
    thrustc_logging::write(
        thrustc_logging::OutputIn::Stdout,
        "aarch64     - AArch64 (little endian)
aarch64_32  - AArch64 (little endian ILP32)
aarch64_be  - AArch64 (big endian)
amdgcn      - AMD GCN GPUs
arm         - ARM
arm64       - ARM64 (little endian)
arm64_32    - ARM64 (little endian ILP32)
armeb       - ARM (big endian)
bpf         - BPF (host endian)
bpfeb       - BPF (big endian)
bpfel       - BPF (little endian)
hexagon     - Hexagon
lanai       - Lanai
loongarch32 - 32-bit LoongArch
loongarch64 - 64-bit LoongArch
mips        - MIPS (32-bit big endian)
mips64      - MIPS (64-bit big endian)
mips64el    - MIPS (64-bit little endian)
mipsel      - MIPS (32-bit little endian)
msp430      - MSP430 [experimental]
nvptx       - NVIDIA PTX 32-bit
nvptx64     - NVIDIA PTX 64-bit
ppc32       - PowerPC 32
ppc32le     - PowerPC 32 LE
ppc64       - PowerPC 64
ppc64le     - PowerPC 64 LE
r600        - AMD GPUs HD2XXX-HD6XXX
riscv32     - 32-bit RISC-V
riscv64     - 64-bit RISC-V
sparc       - Sparc
sparcel     - Sparc LE
sparcv9     - Sparc V9
spirv       - SPIR-V Logical
spirv32     - SPIR-V 32-bit
spirv64     - SPIR-V 64-bit
systemz     - SystemZ
wasm32      - WebAssembly 32-bit
wasm64      - WebAssembly 64-bit
x86         - 32-bit X86: Pentium-Pro and above
x86-64      - 64-bit X86: EM64T and AMD64
xcore       - XCore",
    );

    std::process::exit(0);
}

#[inline]
pub fn print_specific_cpu_support(arch: &str) {
    match arch {
        "aarch64" | "aarch64_32" | "aarch64_be" => {
            thrustc_logging::write(
                thrustc_logging::OutputIn::Stdout,
                "a64fx
ampere1
ampere1a
ampere1b
apple-a10
apple-a11
apple-a12
apple-a13
apple-a14
apple-a15
apple-a16
apple-a17
apple-a7
apple-a8
apple-a9
apple-m1
apple-m2
apple-m3
apple-m4
apple-s4
apple-s5
carmel
cobalt-100
cortex-a34
cortex-a35
cortex-a510
cortex-a520
cortex-a520ae
cortex-a53
cortex-a55
cortex-a57
cortex-a65
cortex-a65ae
cortex-a710
cortex-a715
cortex-a72
cortex-a720
cortex-a720ae
cortex-a725
cortex-a73
cortex-a75
cortex-a76
cortex-a76ae
cortex-a77
cortex-a78
cortex-a78ae
cortex-a78c
cortex-r82
cortex-r82ae
cortex-x1
cortex-x1c
cortex-x2
cortex-x3
cortex-x4
cortex-x925
cyclone
exynos-m3
exynos-m4
exynos-m5
falkor
fujitsu-monaka
generic
grace
kryo
neoverse-512tvb
neoverse-e1
neoverse-n1
neoverse-n2
neoverse-n3
neoverse-v1
neoverse-v2
neoverse-v3
neoverse-v3ae
oryon-1
saphira
thunderx
thunderx2t99
thunderx3t110
thunderxt81
thunderxt83
thunderxt88
tsv110",
            );
        }

        "arm" | "arm64" | "arm64_32" | "armeb" => {
            thrustc_logging::write(
                thrustc_logging::OutputIn::Stdout,
                "arm1020e
arm1020t
arm1022e
arm10e
arm10tdmi
arm1136j-s
arm1136jf-s
arm1156t2-s
arm1156t2f-s
arm1176jz-s
arm1176jzf-s
arm710t
arm720t
arm7tdmi
arm7tdmi-s
arm8
arm810
arm9
arm920
arm920t
arm922t
arm926ej-s
arm940t
arm946e-s
arm966e-s
arm968e-s
arm9e
arm9tdmi
cortex-a12
cortex-a15
cortex-a17
cortex-a32
cortex-a35
cortex-a5
cortex-a510
cortex-a53
cortex-a55
cortex-a57
cortex-a7
cortex-a710
cortex-a72
cortex-a73
cortex-a75
cortex-a76
cortex-a76ae
cortex-a77
cortex-a78
cortex-a78ae
cortex-a78c
cortex-a8
cortex-a9
cortex-m0
cortex-m0plus
cortex-m1
cortex-m23
cortex-m3
cortex-m33
cortex-m35p
cortex-m4
cortex-m52
cortex-m55
cortex-m7
cortex-m85
cortex-r4
cortex-r4f
cortex-r5
cortex-r52
cortex-r52plus
cortex-r7
cortex-r8
cortex-x1
cortex-x1c
cyclone
ep9312
exynos-m3
exynos-m4
exynos-m5
generic
iwmmxt
krait
kryo
mpcore
mpcorenovfp
neoverse-n1
neoverse-n2
neoverse-v1
sc000
sc300
star-mc1
strongarm
strongarm110
strongarm1100
strongarm1110
swift
xscale",
            );
        }

        "amdgcn" | "r600" => {
            thrustc_logging::write(
                thrustc_logging::OutputIn::Stdout,
                "bonaire
carrizo
fiji
generic
generic-hsa
gfx10-1-generic
gfx10-3-generic
gfx1010
gfx1011
gfx1012
gfx1013
gfx1030
gfx1031
gfx1032
gfx1033
gfx1034
gfx1035
gfx1036
gfx11-generic
gfx1100
gfx1101
gfx1102
gfx1103
gfx1150
gfx1151
gfx1152
gfx1153
gfx12-generic
gfx1200
gfx1201
gfx600
gfx601
gfx602
gfx700
gfx701
gfx702
gfx703
gfx704
gfx705
gfx801
gfx802
gfx803
gfx805
gfx810
gfx9-4-generic
gfx9-generic
gfx900
gfx902
gfx904
gfx906
gfx908
gfx909
gfx90a
gfx90c
gfx940
gfx941
gfx942
gfx950
hainan
hawaii
iceland
kabini
kaveri
mullins
oland
pitcairn
polaris10
polaris11
stoney
tahiti
tonga
tongapro
verde",
            );
        }

        "bpf" | "bpfeb" | "bpfel" => {
            thrustc_logging::write(
                thrustc_logging::OutputIn::Stdout,
                "generic
probe
v1
v2
v3
v4",
            );
        }

        "loongarch32" | "loongarch64" => {
            thrustc_logging::write(
                thrustc_logging::OutputIn::Stdout,
                "generic
generic-la32
generic-la64
la464
la664
loongarch64",
            );
        }

        "mips" | "mips64" | "mips64el" | "mipsel" => {
            thrustc_logging::write(
                thrustc_logging::OutputIn::Stdout,
                "generic
mips1
mips2
mips3
mips32
mips32r2
mips32r3
mips32r5
mips32r6
mips4
mips5
mips64
mips64r2
mips64r3
mips64r5
mips64r6
octeon
octeon+
p5600",
            );
        }

        "ppc32" | "ppc32le" | "ppc64" | "ppc64le" => {
            thrustc_logging::write(
                thrustc_logging::OutputIn::Stdout,
                "440
450
601
602
603
603e
603ev
604
604e
620
7400
7450
750
970
a2
e500
e500mc
e5500
future
g3
g4
g4+
g5
generic
ppc
ppc32
ppc64
ppc64le
pwr10
pwr11
pwr3
pwr4
pwr5
pwr5x
pwr6
pwr6x
pwr7
pwr8
pwr9",
            );
        }

        "riscv32" | "riscv64" => {
            thrustc_logging::write(
                thrustc_logging::OutputIn::Stdout,
                "generic
generic-rv32
generic-rv64
mips-p8700
rocket
rocket-rv32
rocket-rv64
rp2350-hazard3
sifive-7-series
sifive-e20
sifive-e21
sifive-e24
sifive-e31
sifive-e34
sifive-e76
sifive-p450
sifive-p470
sifive-p550
sifive-p670
sifive-s21
sifive-s51
sifive-s54
sifive-s76
sifive-u54
sifive-u74
sifive-x280
spacemit-x60
syntacore-scr1-base
syntacore-scr1-max
syntacore-scr3-rv32
syntacore-scr3-rv64
syntacore-scr4-rv32
syntacore-scr4-rv64
syntacore-scr5-rv32
syntacore-scr5-rv64
syntacore-scr7
tt-ascalon-d8
veyron-v1
xiangshan-nanhu",
            );
        }

        "sparc" | "sparcel" | "sparcv9" => {
            thrustc_logging::write(
                thrustc_logging::OutputIn::Stdout,
                "at697e
at697f
f934
generic
gr712rc
gr740
hypersparc
leon2
leon3
leon4
ma2080
ma2085
ma2100
ma2150
ma2155
ma2450
ma2455
ma2480
ma2485
ma2x5x
ma2x8x
myriad2
myriad2.1
myriad2.2
myriad2.3
niagara
niagara2
niagara3
niagara4
sparclet
sparclite
sparclite86x
supersparc
tsc701
ultrasparc
ultrasparc3
ut699
v7
v8
v9",
            );
        }

        "wasm32" | "wasm64" => {
            thrustc_logging::write(
                thrustc_logging::OutputIn::Stdout,
                "bleeding-edge
generic
lime1
mvp",
            );
        }

        "hexagon" => {
            thrustc_logging::write(
                thrustc_logging::OutputIn::Stdout,
                "generic
hexagonv5
hexagonv55
hexagonv60
hexagonv62
hexagonv65
hexagonv66
hexagonv67
hexagonv67t
hexagonv68
hexagonv69
hexagonv71
hexagonv71t
hexagonv73
hexagonv75
hexagonv79",
            );
        }

        "msp430" => {
            thrustc_logging::write(
                thrustc_logging::OutputIn::Stdout,
                "generic
msp430
msp430x",
            );
        }

        "nvptx" | "nvptx64" => {
            thrustc_logging::write(
                thrustc_logging::OutputIn::Stdout,
                "sm_100
sm_100a
sm_101
sm_101a
sm_120
sm_120a
sm_20
sm_21
sm_30
sm_32
sm_35
sm_37
sm_50
sm_52
sm_53
sm_60
sm_61
sm_62
sm_70
sm_72
sm_75
sm_80
sm_86
sm_87
sm_89
sm_90",
            );
        }

        "systemz" => {
            thrustc_logging::write(
                thrustc_logging::OutputIn::Stdout,
                "arch10
arch11
arch12
arch13
arch14
arch15
arch8
arch9
generic
z10
z13
z14
z15
z16
z17
z196
zEC12",
            );
        }

        "x86" | "x86_64" => {
            thrustc_logging::write(
                thrustc_logging::OutputIn::Stdout,
                "alderlake
amdfam10
arrowlake
arrowlake-s
arrowlake_s
athlon
athlon-4
athlon-fx
athlon-mp
athlon-tbird
athlon-xp
athlon64
athlon64-sse3
atom
atom_sse4_2
atom_sse4_2_movbe
barcelona
bdver1
bdver2
bdver3
bdver4
bonnell
broadwell
btver1
btver2
c3
c3-2
cannonlake
cascadelake
clearwaterforest
cooperlake
core-avx-i
core-avx2
core2
core_2_duo_sse4_1
core_2_duo_ssse3
core_2nd_gen_avx
core_3rd_gen_avx
core_4th_gen_avx
core_4th_gen_avx_tsx
core_5th_gen_avx
core_5th_gen_avx_tsx
core_aes_pclmulqdq
core_i7_sse4_2
corei7
corei7-avx
diamondrapids
emeraldrapids
generic
geode
goldmont
goldmont-plus
goldmont_plus
gracemont
grandridge
graniterapids
graniterapids-d
graniterapids_d
haswell
i386
i486
i586
i686
icelake-client
icelake-server
icelake_client
icelake_server
ivybridge
k6
k6-2
k6-3
k8
k8-sse3
knl
knm
lakemont
lunarlake
meteorlake
mic_avx512
nehalem
nocona
opteron
opteron-sse3
pantherlake
penryn
pentium
pentium-m
pentium-mmx
pentium2
pentium3
pentium3m
pentium4
pentium4m
pentium_4
pentium_4_sse3
pentium_ii
pentium_iii
pentium_iii_no_xmm_regs
pentium_m
pentium_mmx
pentium_pro
pentiumpro
prescott
raptorlake
rocketlake
sandybridge
sapphirerapids
sierraforest
silvermont
skx
skylake
skylake-avx512
skylake_avx512
slm
tigerlake
tremont
westmere
winchip-c6
winchip2
x86-64
x86-64-v2
x86-64-v3
x86-64-v4
yonah
znver1
znver2
znver3
znver4
znver5",
            );
        }

        what => {
            if what.is_empty() {
                thrustc_logging::print_error(
                    thrustc_logging::LoggingType::Error,
                    "Try to set the target architecture using the '-target' command line flag.",
                );

                std::process::exit(1)
            }

            thrustc_logging::print_error(
                thrustc_logging::LoggingType::Error,
                &format!("CPU support isn't available for '{}' architecture.", what),
            );

            std::process::exit(1)
        }
    }

    std::process::exit(0)
}

// SOURCE: LLVM OPT 17.0.6 -print-passes
#[inline]
pub fn print_all_available_opt_passes() {
    thrustc_logging::write(thrustc_logging::OutputIn::Stdout, "
Module passes:
  always-inline
  attributor
  annotation2metadata
  openmp-opt
  openmp-opt-postlink
  called-value-propagation
  canonicalize-aliases
  cg-profile
  check-debugify
  constmerge
  coro-early
  coro-cleanup
  cross-dso-cfi
  deadargelim
  debugify
  dot-callgraph
  elim-avail-extern
  extract-blocks
  forceattrs
  function-import
  globalopt
  globalsplit
  hotcoldsplit
  inferattrs
  inliner-wrapper
  inliner-ml-advisor-release
  print<inline-advisor>
  inliner-wrapper-no-mandatory-first
  insert-gcov-profiling
  instrorderfile
  instrprof
  internalize
  invalidate<all>
  iroutliner
  print-ir-similarity
  lower-global-dtors
  lower-ifunc
  lowertypetests
  metarenamer
  mergefunc
  name-anon-globals
  no-op-module
  objc-arc-apelim
  partial-inliner
  memprof-context-disambiguation
  pgo-icall-prom
  pgo-instr-gen
  pgo-instr-use
  print-profile-summary
  print-callgraph
  print-callgraph-sccs
  print
  print-lcg
  print-lcg-dot
  print-must-be-executed-contexts
  print-stack-safety
  print<module-debuginfo>
  recompute-globalsaa
  rel-lookup-table-converter
  rewrite-statepoints-for-gc
  rewrite-symbols
  rpo-function-attrs
  sample-profile
  scc-oz-module-inliner
  strip
  strip-dead-debug-info
  pseudo-probe
  strip-dead-prototypes
  strip-debug-declare
  strip-nondebug
  strip-nonlinetable-debuginfo
  synthetic-counts-propagation
  trigger-crash
  verify
  view-callgraph
  wholeprogramdevirt
  dfsan
  module-inline
  tsan-module
  sancov-module
  sanmd-module
  memprof-module
  poison-checking
  pseudo-probe-update
Module passes with params:
  loop-extract<single>
  globaldce<in-lto-post-link>
  hwasan<kernel;recover>
  asan<kernel>
  msan<recover;kernel;eager-checks;track-origins=N>
  ipsccp<no-func-spec;func-spec>
  embed-bitcode<thinlto;emit-summary>
  memprof-use<profile-filename=S>
Module analyses:
  callgraph
  lcg
  module-summary
  no-op-module
  profile-summary
  stack-safety
  verify
  pass-instrumentation
  inline-advisor
  ir-similarity
  globals-aa
Module alias analyses:
  globals-aa
CGSCC passes:
  argpromotion
  invalidate<all>
  attributor-cgscc
  openmp-opt-cgscc
  no-op-cgscc
CGSCC passes with params:
  inline<only-mandatory>
  coro-split<reuse-storage>
  function-attrs<skip-non-recursive>
CGSCC analyses:
  no-op-cgscc
  fam-proxy
  pass-instrumentation
Function passes:
  aa-eval
  adce
  add-discriminators
  aggressive-instcombine
  assume-builder
  assume-simplify
  alignment-from-assumptions
  annotation-remarks
  bdce
  bounds-checking
  break-crit-edges
  callsite-splitting
  consthoist
  count-visits
  constraint-elimination
  chr
  coro-elide
  correlated-propagation
  dce
  dfa-jump-threading
  div-rem-pairs
  dse
  dot-cfg
  dot-cfg-only
  dot-dom
  dot-dom-only
  dot-post-dom
  dot-post-dom-only
  view-dom
  view-dom-only
  view-post-dom
  view-post-dom-only
  fix-irreducible
  flattencfg
  make-guards-explicit
  gvn-hoist
  gvn-sink
  helloworld
  infer-address-spaces
  instcombine
  instcount
  instsimplify
  invalidate<all>
  irce
  float2int
  no-op-function
  libcalls-shrinkwrap
  lint
  inject-tli-mappings
  instnamer
  loweratomic
  lower-expect
  lower-guard-intrinsic
  lower-constant-intrinsics
  lower-widenable-condition
  guard-widening
  load-store-vectorizer
  loop-simplify
  loop-sink
  lowerinvoke
  lowerswitch
  mem2reg
  memcpyopt
  mergeicmps
  mergereturn
  move-auto-init
  nary-reassociate
  newgvn
  jump-threading
  partially-inline-libcalls
  kcfi
  lcssa
  loop-data-prefetch
  loop-load-elim
  loop-fusion
  loop-distribute
  loop-versioning
  objc-arc
  objc-arc-contract
  objc-arc-expand
  pa-eval
  pgo-memop-opt
  place-safepoints
  print
  print<assumptions>
  print<block-freq>
  print<branch-prob>
  print<cost-model>
  print<cycles>
  print<da>
  print<domtree>
  print<postdomtree>
  print<delinearization>
  print<demanded-bits>
  print<domfrontier>
  print<func-properties>
  print<inline-cost>
  print<inliner-size-estimator>
  print<loops>
  print<memoryssa-walker>
  print<phi-values>
  print<regions>
  print<scalar-evolution>
  print<stack-safety-local>
  print<access-info>
  print-alias-sets
  print-cfg-sccs
  print-predicateinfo
  print-mustexecute
  print-memderefs
  print<uniformity>
  reassociate
  redundant-dbg-inst-elim
  reg2mem
  scalarize-masked-mem-intrin
  scalarizer
  separate-const-offset-from-gep
  sccp
  sink
  slp-vectorizer
  slsr
  speculative-execution
  strip-gc-relocates
  structurizecfg
  tailcallelim
  typepromotion
  unify-loop-exits
  vector-combine
  verify
  verify<domtree>
  verify<loops>
  verify<memoryssa>
  verify<regions>
  verify<safepoint-ir>
  verify<scalar-evolution>
  view-cfg
  view-cfg-only
  tlshoist
  transform-warning
  tsan
  memprof
  declare-to-assign
Function passes with params:
  early-cse<memssa>
  ee-instrument<post-inline>
  hardware-loops<force-hardware-loops;force-hardware-loop-phi;force-nested-hardware-loop;force-hardware-loop-guard;hardware-loop-decrement=N;hardware-loop-counter-bitwidth=N>
  lower-matrix-intrinsics<minimal>
  loop-unroll<O0;O1;O2;O3;full-unroll-max=N;no-partial;partial;no-peeling;peeling;no-profile-peeling;profile-peeling;no-runtime;runtime;no-upperbound;upperbound>
  simplifycfg<no-forward-switch-cond;forward-switch-cond;no-switch-range-to-icmp;switch-range-to-icmp;no-switch-to-lookup;switch-to-lookup;no-keep-loops;keep-loops;no-hoist-common-insts;hoist-common-insts;no-sink-common-insts;sink-common-insts;bonus-inst-threshold=N>
  loop-vectorize<no-interleave-forced-only;interleave-forced-only;no-vectorize-forced-only;vectorize-forced-only>
  instcombine<no-use-loop-info;use-loop-info;max-iterations=N>
  mldst-motion<no-split-footer-bb;split-footer-bb>
  gvn<no-pre;pre;no-load-pre;load-pre;no-split-backedge-load-pre;split-backedge-load-pre;no-memdep;memdep>
  sroa<preserve-cfg;modify-cfg>
  print<stack-lifetime><may;must>
  print<da><normalized-results>
  separate-const-offset-from-gep<lower-gep>
  function-simplification<O1;O2;O3;Os;Oz>
  print<memoryssa><no-ensure-optimized-uses>
Function analyses:
  aa
  assumptions
  block-freq
  branch-prob
  cycles
  domtree
  postdomtree
  demanded-bits
  domfrontier
  func-properties
  loops
  access-info
  lazy-value-info
  da
  inliner-size-estimator
  memdep
  memoryssa
  phi-values
  regions
  no-op-function
  opt-remark-emit
  scalar-evolution
  should-not-run-function-passes
  should-run-extra-vector-passes
  stack-safety-local
  targetlibinfo
  targetir
  verify
  pass-instrumentation
  uniformity
  basic-aa
  objc-arc-aa
  scev-aa
  scoped-noalias-aa
  tbaa
Function alias analyses:
  basic-aa
  objc-arc-aa
  scev-aa
  scoped-noalias-aa
  tbaa
LoopNest passes:
  loop-flatten
  loop-interchange
  loop-unroll-and-jam
  no-op-loopnest
Loop passes:
  canon-freeze
  dot-ddg
  invalidate<all>
  loop-idiom
  loop-instsimplify
  no-op-loop
  print
  loop-deletion
  loop-simplifycfg
  loop-reduce
  indvars
  loop-unroll-full
  print<ddg>
  print<iv-users>
  print<loopnest>
  print<loop-cache-cost>
  loop-predication
  guard-widening
  loop-bound-split
  loop-reroll
  loop-versioning-licm
Loop passes with params:
  simple-loop-unswitch<nontrivial;no-nontrivial;trivial;no-trivial>
  licm<allowspeculation>
  lnicm<allowspeculation>
  loop-rotate<no-header-duplication;header-duplication;no-prepare-for-lto;prepare-for-lto>
Loop analyses:
  no-op-loop
  ddg
  iv-users
  pass-instrumentation
");
    std::process::exit(0);
}

pub fn print_host_target_triple() -> ! {
    thrustc_logging::write(
        thrustc_logging::OutputIn::Stdout,
        TargetMachine::get_default_triple()
            .as_str()
            .to_string_lossy()
            .trim(),
    );

    std::process::exit(1)
}

pub fn print_host_cpu_features() -> ! {
    let features: String = TargetMachine::get_host_cpu_features().to_string();

    thrustc_logging::write(
        thrustc_logging::OutputIn::Stdout,
        features.trim(),
    );

    std::process::exit(0)
}
