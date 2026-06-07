" Syntax highlighting for Thrust
" Language: Thrust
" Maintainer: Stevens Benavides

if exists("b:current_syntax")
  finish
endif

" ─── Comments ────────────────────────────────────────────────────────────────
syntax region thrustBlockComment start="/\*" end="\*/" contains=thrustTodo
syntax match  thrustLineComment  "//.*$"     contains=thrustTodo
syntax keyword thrustTodo TODO FIXME HACK NOTE XXX contained

" ─── Strings & Characters ────────────────────────────────────────────────────
syntax region thrustString start='"' skip='\\"' end='"'
syntax match  thrustChar   "'\\.'"
syntax match  thrustChar   "'[^\\]'"

" ─── Numbers ─────────────────────────────────────────────────────────────────
syntax match thrustFloat   "\<[0-9]\+\.[0-9]*\>"
syntax match thrustInt     "\<[0-9]\+\>"
syntax match thrustHex     "\<0x[0-9a-fA-F]\+\>"

" ─── Keywords ────────────────────────────────────────────────────────────────
syntax keyword thrustKeyword
    \ var fn if elif else for while loop
    \ return break continue breakall continueall
    \ defer pass as const struct type enum
    \ alloc address addr load write
    \ ref mut static unreachable intrinsic
    \ embedded import importC new
    \ asm asmfn global_asm fixed

" ─── Literals ────────────────────────────────────────────────────────────────
syntax keyword thrustBoolean true false
syntax keyword thrustNull    nullptr

" ─── Memory / pointer keywords ───────────────────────────────────────────────
syntax keyword thrustMemory deref

" ─── Builtin functions ───────────────────────────────────────────────────────
syntax keyword thrustBuiltin
    \ halloc sizeOf memset memmove memcpy
    \ alignOf abiSizeOf bitSizeOf abiAlignOf

" ─── Types ───────────────────────────────────────────────────────────────────
syntax keyword thrustType
    \ s8 s16 s32 s64 ssize
    \ u8 u16 u32 u64 u128 usize
    \ f32 f64 f128 f80 fppc_128
    \ bool char ptr array void Fn

" ─── Type qualifier ──────────────────────────────────────────────────────────
syntax keyword thrustTypeQual const

" ─── Atomic / threading ──────────────────────────────────────────────────────
syntax keyword thrustAtomic
    \ volatile lazyThread
    \ atomicNone atomicFree atomicRelax atomicGrab
    \ atomicDrop atomicSync atomicStrict
    \ threadInit threadDyn threadExec threadLDyn

" ─── Attributes  (@something) ────────────────────────────────────────────────
syntax match thrustAttribute
    \ "@\(asmAlignStack\|asmSyntax\|asmThrowErrors\|asmSideEffects\)"
syntax match thrustAttribute
    \ "@\(align\|optFuzzing\|noUnwind\|packed\|heap\|public\|linkage\)"
syntax match thrustAttribute
    \ "@\(extern\|arbitraryArgs\|hot\|minSize\|alwaysInline\|noInline\|inline\)"
syntax match thrustAttribute
    \ "@\(safeStack\|weakStack\|strongStack\|preciseFloatingPoint\)"
syntax match thrustAttribute
    \ "@\(convention\|pure\|thunk\|cuda\|constructor\|destructor\)"

" ─── Operators ───────────────────────────────────────────────────────────────
syntax match thrustOperator "[-+*/=<>!&|^%~]"
syntax match thrustOperator "\(==\|!=\|<=\|>=\|&&\|||/\|<<\|>>\)"
syntax match thrustOperator "\(++\|--\)"

" ─── Function definitions and calls ──────────────────────────────────────────
syntax match thrustFuncDef  "\<fn\s\+\zs\w\+"
syntax match thrustFuncCall "\<\w\+\ze\s*("

" ─── Struct names ─────────────────────────────────────────────────────────────
syntax match thrustStructName "\<struct\s\+\zs\w\+"

" ─── Punctuation ──────────────────────────────────────────────────────────────
syntax match thrustPunct "[(){}\[\].,;:]"

" ─── One Dark colors ────────────────────────────────────────────────────────
"
"   bg         #282c34    fg          #abb2bf
"   purple     #c678dd    cyan        #56b6c2
"   blue       #61afef    green       #98c379
"   red        #e06c75    yellow      #e5c07b
"   orange     #d19a66    comment     #5c6370
"   dark_red   #be5046

function! s:hi(group, fg, ...) abort
    let l:bg  = get(a:, 1, 'NONE')
    let l:gui = get(a:, 2, 'NONE')
    exe 'highlight ' . a:group .
        \ ' guifg=' . a:fg .
        \ ' guibg=' . l:bg .
        \ ' gui='   . l:gui .
        \ ' ctermfg=NONE ctermbg=NONE cterm=NONE'
endfunction

call s:hi('thrustBlockComment', '#5c6370', 'NONE', 'italic')
call s:hi('thrustLineComment',  '#5c6370', 'NONE', 'italic')
call s:hi('thrustTodo',         '#e5c07b', 'NONE', 'bold')

call s:hi('thrustString',       '#98c379')
call s:hi('thrustChar',         '#98c379')
call s:hi('thrustFloat',        '#d19a66')
call s:hi('thrustInt',          '#d19a66')
call s:hi('thrustHex',          '#d19a66')

call s:hi('thrustKeyword',      '#c678dd')
call s:hi('thrustBoolean',      '#d19a66')
call s:hi('thrustNull',         '#d19a66')
call s:hi('thrustMemory',       '#c678dd')

call s:hi('thrustBuiltin',      '#56b6c2')
call s:hi('thrustType',         '#e5c07b')
call s:hi('thrustTypeQual',     '#e5c07b')
call s:hi('thrustAtomic',       '#56b6c2')

call s:hi('thrustAttribute',    '#e06c75')

call s:hi('thrustOperator',     '#56b6c2')
call s:hi('thrustFuncDef',      '#61afef')
call s:hi('thrustFuncCall',     '#61afef')
call s:hi('thrustStructName',   '#e5c07b')
call s:hi('thrustPunct',        '#abb2bf')

let b:current_syntax = "thrust"
