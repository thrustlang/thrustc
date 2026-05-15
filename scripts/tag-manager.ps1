function Main {
    $tagName = Read-Host "Enter tag name"
    
    if ([string]::IsNullOrWhiteSpace($tagName)) {
        Write-Host "Error: Tag name required" -ForegroundColor Red
        exit 1
    }
    
    try {
        git rev-parse --git-dir 2>$null | Out-Null
    }
    catch {
        Write-Host "Error: Not in a Git repository" -ForegroundColor Red
        exit 1
    }
    
    $existingTag = git tag -l | Where-Object { $_ -eq $tagName }
    
    if ($existingTag) {
        Write-Host "Tag '$tagName' exists. Deleting..." -ForegroundColor Yellow
        git tag -d $tagName
        
        $remoteTags = git ls-remote --tags origin 2>$null
        
        if ($remoteTags -match "refs/tags/$tagName$") {
            git push origin --delete $tagName
        }
    }
    
    Write-Host "Creating tag '$tagName'..." -ForegroundColor Green
    git tag $tagName
    
    if ($LASTEXITCODE -ne 0) {
        Write-Host "Error: Failed to create tag" -ForegroundColor Red
        exit 1
    }
    
    $pushConfirm = Read-Host "Push to remote? [Y/n]"
    
    if ($pushConfirm -ne "n" -and $pushConfirm -ne "N") {
        git push origin $tagName
    }
    
    Write-Host "Done. Tag '$tagName' created." -ForegroundColor Green
}

Main
