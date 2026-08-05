@echo off

setlocal

set "SCRIPT_DIR=%~dp0"
for %%I in ("%SCRIPT_DIR%..") do set "FUZZ_DIR=%%~fI"

set "DIRS=corpus_stable\llvm-codegen-top-level corpus_stable\llvm-codegen-local corpus_stable\llvm-codegen-local-loops corpus_stable\pipeline corpus_unstable\llvm-codegen-top-level corpus_unstable\llvm-codegen-local corpus_unstable\llvm-codegen-local-loops corpus_unstable\pipeline corpus_universal\lexer fuzz_reproduce_logs fuzz_pipeline backlog fuzz_continuous"

for %%D in (%DIRS%) do (
    if not exist "%FUZZ_DIR%\%%D\" (
        mkdir "%FUZZ_DIR%\%%D"
        echo created: %%D
    ) else (
        echo exists : %%D
    )
)

echo All required directories are present.
endlocal