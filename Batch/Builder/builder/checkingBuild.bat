@echo off
if "%BuilderStart%"=="" goto :EOF

echo Checking for correct building...
echo.

for /F "tokens=*" %%i in (%fileList%) do if not exist "%buildFolder%\%%i" set FileNotFound=%%i& goto :error

echo Build is correct.
echo.
goto :EOF  

:error
echo Not found: %FileNotFound%.
set errorChecking=true
echo.