@echo off
set BuilderStart=true

:: Название папки, в которой находятся не главные командные файлы
set folder=builder

:: Создание оформления консоли
call %folder%\decor.bat

:: Внесение необходимой информации в переменные
call %folder%\setting.bat

:: Удаление старого репозитория
call %folder%\removeOldRepo.bat

:: Клонирование репозитория
call %folder%\repoClone.bat
if "%errorInCloning%"=="true" goto :end

:: Сборка проекта
call %folder%\buildSolution.bat
if "%errorInBuild%"=="true" goto :end

call %folder%\checkingBuild.bat

:end
:: Рассылка писем
call %folder%\email.bat

:: Удаление временных файлов
call %folder%\finality.bat

echo Done.

timeout /t 5 > nul