@echo off
set BuilderStart=true

:: �������� �����, � ������� ��������� �� ������� ��������� �����
set folder=builder

:: �������� ���������� �������
call %folder%\decor.bat

:: �������� ����������� ���������� � ����������
call %folder%\setting.bat

:: �������� ������� �����������
call %folder%\removeOldRepo.bat

:: ������������ �����������
call %folder%\repoClone.bat
if "%errorInCloning%"=="true" goto :end

:: ������ �������
call %folder%\buildSolution.bat
if "%errorInBuild%"=="true" goto :end

call %folder%\checkingBuild.bat

:end
:: �������� �����
call %folder%\email.bat

:: �������� ��������� ������
call %folder%\finality.bat

echo Done.

timeout /t 5 > nul