@echo off

rem :: Ancienne méthode (ne fonctionne pas sous Vista) :
rem set valeur=reg query "hklm\Software\R-core\R" /v InstallPath
rem set Rpathstr=findstr /I /L /C:"REG_SZ"
rem for /f "tokens=3 delims=	" %%i in ('%valeur%^|%Rpathstr%') do set Rpath="%%i"

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

::::::::::::::::::::::::::::::::
:: On lance R et la plateforme :
::::::::::::::::::::::::::::::::

rem set R_PROFILE=Rprofile.site
start "PAMPA WP2" "%R_HOME%"\bin\Rgui.exe R_PROFILE=Exec/Rprofile.site --no-restore --sdi

rem Qitter l'invite de commande :
exit
