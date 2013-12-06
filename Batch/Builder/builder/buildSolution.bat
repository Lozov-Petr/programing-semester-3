@echo off
if "%BuilderStart%"=="" goto :EOF

echo Build solution...
echo.

chcp %cyrillicCodepage% > nul
MSBuild %solution%>%logMSBuild%
chcp %standartCodepage% > nul

if errorlevel 1 goto :error

echo Build completed.
echo.
goto :EOF

:error
set errorInBuild=true
echo Error build solution!
echo.