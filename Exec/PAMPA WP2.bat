@echo off
set valeur=reg query "HKEY_LOCAL_MACHINE\Software\R-core\R" /v InstallPath
set Rpathstr=findstr /I /L /C:"REG_SZ"
for /f "tokens=3 delims=	" %%i in ('%valeur%^|%Rpathstr%') do set Rpath="%%i"

rem set R_PROFILE=Rprofile.site
start "PAMPA WP2" %Rpath%\bin\Rgui.exe R_PROFILE=Exec/Rprofile.site --no-restore --sdi
exit
