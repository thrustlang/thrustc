@echo off
setlocal enabledelayedexpansion

set SCRIPT_DIR=%~dp0

echo === Step 1/3: Deploying documentation ===
call "%SCRIPT_DIR%deploy-code-docs.bat"
if errorlevel 1 exit /b 1

echo.
echo === Step 2/3: Generating release changelog ===
call "%SCRIPT_DIR%release-changelog.bat"
if errorlevel 1 exit /b 1

echo.
echo === Step 3/3: Tagging release ===
call "%SCRIPT_DIR%tag-manager.bat"
if errorlevel 1 exit /b 1

echo.
echo Done. Version deployed successfully.
