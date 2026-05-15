Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"

$ScriptDir = $PSScriptRoot

Write-Host "=== Step 1/3: Deploying documentation ==="
& "$ScriptDir\deploy-code-docs.ps1"

Write-Host ""
Write-Host "=== Step 2/3: Generating release changelog ==="
& "$ScriptDir\release-changelog.ps1"

Write-Host ""
Write-Host "=== Step 3/3: Tagging release ==="
& "$ScriptDir\tag-manager.ps1"

Write-Host ""
Write-Host "Done. Version deployed successfully."
