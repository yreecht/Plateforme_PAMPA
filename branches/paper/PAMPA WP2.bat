@echo off

rem :: Ancienne méthode (ne fonctionne pas sous Vista) :
rem set valeur=reg query "hklm\Software\R-core\R" /v InstallPath
rem set Rpathstr=findstr /I /L /C:"REG_SZ"
rem for /f "tokens=3 delims=	" %%i in ('%valeur%^|%Rpathstr%') do set Rpath="%%i"

setlocal

::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
:: Méthode qui devrait fonctionner sous Vista et Windows 7 également :
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
:: use environment variable R_HOME if defined
:: else current folder if bin\rcmd.exe exists
:: else most current R as determined by registry entry
:: else error
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

rem Test si variable d'environnement définie ou présence de Rgui dans le dossier bin\ :
if not defined R_HOME if exist bin\Rgui.exe set R_HOME=%CD%

rem Registre XP (ou équivalent) :
if not defined R_HOME for /f "tokens=2*" %%a in (
 'reg query hklm\software\R-core\R /v InstallPath 2^>NUL ^| findstr InstallPath'
  ) do set R_HOME=%%~b

rem Registre Vista ou équivalent :
if not defined R_HOME for /f "tokens=2*" %%a in (
 'reg query hklm\software\wow6432Node\r-core\r /v InstallPath 2^>NUL ^| findstr InstallPath'
  ) do set R_HOME=%%~b

rem Erreur si R n'est pas trouvé :
if not defined R_HOME echo "Error: R not found" & goto:eof

:::::::::::::::::::::::::
:: 2.12 ou supérieure ? :
:::::::::::::::::::::::::

call :process_arch %*

rem Registre XP (ou équivalent) :
:: if not defined R32_HOME for /f "tokens=2*" %%a in (
::  'reg query hklm\software\R-core\R32 /v InstallPath 2^>NUL ^| findstr InstallPath'
::  ) do set R32_HOME=%%~b

set cmd=Rgui.exe

:: Look in architecture specific subdirectory of bin. If not there look in bin.
set cmdpath=%R_HOME%\bin\%R_ARCH0%\%cmd%
if exist "%cmdpath%" goto:cmdpathfound
set cmdpath=%R_HOME%\bin\%cmd%
if exist "%cmdpath%" goto:cmdpathfound
echo "Error: %cmd% not found" & goto:eof
:cmdpathfound

echo %cmdpath% trouvé !

::::::::::::::::::::::::::::::::
:: On lance R et la plateforme :
::::::::::::::::::::::::::::::::

rem set R_PROFILE=Rprofile.site
start "PAMPA WP2" "%cmdpath%" R_PROFILE=Exec/Rprofile.site --no-restore --no-save --sdi

goto:finconsole
rem Qitter l'invite de commande :

::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
:: processing of --arch= where value can be 32, 64, i386, x64, /i386, /x64

:: Call it like this: call :process_arch %*
:: On return R_ARCH will be set from --arch or R_ARCH or default
:: and R_ARCH0 will be R_ARCH without the / prefix
:: It will look for the architecture in these places in this order:
:: - first arg if its --arch
:: - environment variable R_ARCH
:: - check if R_HOME\bin\i386 exists
:: - if R_HOME\bin\x64 exists
:: - if none of the above then use R_ARCH=/i386
:: Note that R_HOME should be defined before calling this routine
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

:process_arch
	if defined R_ARCH goto:process_arch_cont
	:: The loop searches for --arch and sets R_ARCH to the next argument
    :process_arch_loop
    (set arg=%~1)
    shift
    if not defined arg goto :process_arch_cont
	if "%arg%"=="--arch" (set R_ARCH=%1) & goto:process_arch_cont
	goto:process_arch_loop
    :process_arch_cont
	if defined process_arg_arch goto:process_arch_defined
	if exist %R_HOME%\bin\i386 (set R_ARCH=/i386) & goto:process_arch_defined
	if exist %R_HOME%\bin\x64 (set R_ARCH=/x64) & goto:process_arch_defined
	(set R_ARCH=/i386)
	:process_arch_defined
	if "%R_ARCH%"=="32" (set R_ARCH=/i386)
	if "%R_ARCH%"=="386" (set R_ARCH=/i386)
	if "%R_ARCH%"=="i386" (set R_ARCH=/i386)
	if "%R_ARCH%"=="64" (set R_ARCH=/x64)
	if "%R_ARCH%"=="x64" (set R_ARCH=/x64)
	:: if R_ARCH does not begin with a slash add one as a prefix
	(set first_char=%R_ARCH:~0,1%)
	if not "%first_char%" == "/" (set R_ARCH=/%R_ARCH%)
	:: R_ARCH0 is like R_ARCH but without the beginning /
	(set R_ARCH0=%R_ARCH:~1%)
	goto:eof

endlocal

:finconsole
exit
