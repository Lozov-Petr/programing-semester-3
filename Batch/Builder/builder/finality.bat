@echo off
if "%BuilderStart%"=="" goto :EOF

if %errorInSending%==false (
  if exist %RepoCloneErrors% del %RepoCloneErrors%
  if exist %logMSBuild% del %logMSBuild%
  if exist %SendingErrors% del %SendingErrors%  
)
