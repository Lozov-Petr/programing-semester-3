@echo off
if "%BuilderStart%"=="" goto :EOF

echo Cloning repo...
echo.

git clone %gitURL% >nul 2>%RepoCloneErrors%

if errorlevel 1 goto :error

echo Cloning completed.
echo.
goto :EOF

:error
set errorInCloning=true
echo Error cloning repo.
echo.


