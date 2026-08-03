#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
FUZZ_DIR="$(cd "${SCRIPT_DIR}/.." && pwd)"

corpus_dirs=(
    corpus_stable/llvm-codegen-top-level
    corpus_stable/llvm-codegen-local
    corpus_stable/llvm-codegen-local-loops
    corpus_stable/pipeline
    corpus_unstable/llvm-codegen-top-level
    corpus_unstable/llvm-codegen-local
    corpus_unstable/llvm-codegen-local-loops
    corpus_unstable/pipeline
    corpus_universal/lexer
)

for relative in "${corpus_dirs[@]}"; do
    target="${FUZZ_DIR}/${relative}"
    if [ ! -d "${target}" ]; then
        mkdir -p "${target}"
        echo "created: ${relative}"
    else
        echo "exists : ${relative}"
    fi
done

echo "All corpus directories are present."