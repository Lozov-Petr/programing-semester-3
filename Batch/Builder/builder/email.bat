@echo off
if "%BuilderStart%"=="" goto :EOF

echo Sending eMail with results of build solutions...
echo.

if %errorInCloning%==true (
  set emailBody=An error occurred when cloning repository.
  set emailSubject=%emailSubject% [Error in cloning repo]
  set emailFail=%RepoCloneErrors%
 )
 
if %errorInBuild%==true (
  set emailBody=An error occurred during the build solutions.
  set emailSubject=%emailSubject% [Error in build solution]
 ) 
 
if %errorChecking%==true (
  set emailBody=File %FileNotFound% not found in the build solutions.
  set emailSubject=%emailSubject% [File not found]
 )  

blat -subject "%emailSubject%" -body "%emailBody%" -tf %emailList% -attach %emailFail% >nul 2>%SendingErrors%

if errorlevel 1 goto :error

echo Sending succeeded.
echo.
goto :EOF

:error

set errorInSending=true
echo Error sending.
echo.