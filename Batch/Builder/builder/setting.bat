@echo off
if "%BuilderStart%"=="" goto :EOF

:: Работа с PATH
set PathGit="C:\Program Files\Git\bin"
set PathMSBuild="C:\Windows\Microsoft.NET\Framework\v4.0.30319"
set PathBlat="C:\Program Files\Blat\full"
set PATH=%PATH%;%PathGit%;%PathMSBuild%;%PathBlat%

:: Названия из репозитория
set nameRepo=Geometry
set solutionName=IntersectCircles

:: Адрес репозитория
set gitURL=http://github.com/Lozov-Petr/%nameRepo%

:: Адреса в репозитории
set buildFolder=%nameRepo%\%solutionName%\bin\Debug
set solution=%nameRepo%\%solutionName%.sln

:: Кодовые страницы
set standartCodepage=866
set cyrillicCodepage=1251
chcp %standartCodepage% > nul

:: Список файлов, которые нужно проверить
set fileList=%folder%\fileList.txt

:: Логи
set RepoCloneErrors=RepoCloneErrors.log
set logMSBuild=logMSBuild.log
set SendingErrors=SendingErrors.log

:: Информация о появлении ошибок
set errorInCloning=false
set errorInBuild=false
set errorChecking=false
set errorInSending=false

:: Не нейденный файл в сборке
set FileNotFound=

:: Информация для blat
set emailBody=Successful build the solution.
set emailFail=%logMSBuild%
set emailSubject=Auto-building solution: %solutionName%
set emailList=%folder%\emailList.txt
