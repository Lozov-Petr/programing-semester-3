@echo off
if "%BuilderStart%"=="" goto :EOF

echo Removing the old repo...
echo.

if exist %nameRepo% goto :remove

echo Old repo not found.
echo.
goto :EOF

:remove
rd /s /q %nameRepo%
echo Remove completed.
echo.