@echo off
if "%BuilderStart%"=="" goto :EOF

:: ������ � PATH
set PathGit="C:\Program Files\Git\bin"
set PathMSBuild="C:\Windows\Microsoft.NET\Framework\v4.0.30319"
set PathBlat="C:\Program Files\Blat\full"
set PATH=%PATH%;%PathGit%;%PathMSBuild%;%PathBlat%

:: �������� �� �����������
set nameRepo=Geometry
set solutionName=IntersectCircles

:: ����� �����������
set gitURL=http://github.com/Lozov-Petr/%nameRepo%

:: ������ � �����������
set buildFolder=%nameRepo%\%solutionName%\bin\Debug
set solution=%nameRepo%\%solutionName%.sln

:: ������� ��������
set standartCodepage=866
set cyrillicCodepage=1251
chcp %standartCodepage% > nul

:: ������ ������, ������� ����� ���������
set fileList=%folder%\fileList.txt

:: ����
set RepoCloneErrors=RepoCloneErrors.log
set logMSBuild=logMSBuild.log
set SendingErrors=SendingErrors.log

:: ���������� � ��������� ������
set errorInCloning=false
set errorInBuild=false
set errorChecking=false
set errorInSending=false

:: �� ��������� ���� � ������
set FileNotFound=

:: ���������� ��� blat
set emailBody=Successful build the solution.
set emailFail=%logMSBuild%
set emailSubject=Auto-building solution: %solutionName%
set emailList=%folder%\emailList.txt
