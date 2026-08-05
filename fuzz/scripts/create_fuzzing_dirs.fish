set SCRIPT_DIR (dirname (status filename))
set FUZZ_DIR (realpath "$SCRIPT_DIR/..")

set corpus_dirs \
    corpus_stable/llvm-codegen-top-level \
    corpus_stable/llvm-codegen-local \
    corpus_stable/llvm-codegen-local-loops \
    corpus_stable/pipeline \
    corpus_unstable/llvm-codegen-top-level \
    corpus_unstable/llvm-codegen-local \
    corpus_unstable/llvm-codegen-local-loops \
    corpus_unstable/pipeline \
    corpus_universal/lexer

for relative in $corpus_dirs
    set target "$FUZZ_DIR/$relative"
    if test -d "$target"
        echo "exists : $relative"
    else
        mkdir -p "$target"
        echo "created: $relative"
    end
end

echo "All corpus directories are present."