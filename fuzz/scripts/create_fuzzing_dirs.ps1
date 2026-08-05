$ErrorActionPreference = 'Stop'

$scriptDir = $PSScriptRoot
$fuzzDir = Join-Path $scriptDir '..'

$corpusDirs = @(
    'corpus_stable\llvm-codegen-top-level'
    'corpus_stable\llvm-codegen-local'
    'corpus_stable\llvm-codegen-local-loops'
    'corpus_stable\pipeline'
    'corpus_unstable\llvm-codegen-top-level'
    'corpus_unstable\llvm-codegen-local'
    'corpus_unstable\llvm-codegen-local-loops'
    'corpus_unstable\pipeline'
    'corpus_universal\lexer'
    'fuzz_reproduce_logs'
    'fuzz_pipeline'
)

foreach ($relative in $corpusDirs) {
    $target = Join-Path $fuzzDir $relative
    if (-not (Test-Path -LiteralPath $target)) {
        New-Item -ItemType Directory -Path $target -Force | Out-Null
        Write-Output "created: $relative"
    } else {
        Write-Output "exists : $relative"
    }
}

Write-Output 'All required directories are present.'