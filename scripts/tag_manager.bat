@echo off
setlocal enabledelayedexpansion

:main
set /p tag_name="Enter tag name: "

if "%tag_name%"=="" (
    echo Error: Tag name required
    exit /b 1
)

git rev-parse --git-dir >nul 2>&1
if errorlevel 1 (
    echo Error: Not in a Git repository
    exit /b 1
)

git tag -l | findstr /r "^%tag_name%$" >nul 2>&1
if not errorlevel 1 (
    echo Tag '%tag_name%' exists. Deleting...
    git tag -d "%tag_name%"
    
    git ls-remote --tags origin | findstr /r "refs/tags/%tag_name%$" >nul 2>&1
    if not errorlevel 1 (
        git push origin --delete "%tag_name%"
    )
)

echo Creating tag '%tag_name%'...
git tag "%tag_name%"

if errorlevel 1 (
    echo Error: Failed to create tag
    exit /b 1
)

set /p push_confirm="Push to remote? [Y/n]: "

if /i not "%push_confirm%"=="n" (
    git push origin "%tag_name%"
)

echo Done. Tag '%tag_name%' created.
goto :eof