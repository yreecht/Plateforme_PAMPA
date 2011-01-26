; Script generated by the Inno Setup Script Wizard.
; SEE THE DOCUMENTATION FOR DETAILS ON CREATING INNO SETUP SCRIPT FILES!

#define MyAppName "PAMPA WP2"
#define MyAppVersion "1.0-alpha-5"
#define MyAppPublisher "Ifremer"
#define MyAppURL "https://www.ifremer.fr/ezprod/index.php/pampa/"
#define MyAppExeName "PAMPA WP2.bat"
#define InstallDir "C:\PAMPA"
#define ExecDir "Exec"

[Setup]
; NOTE: The value of AppId uniquely identifies this application.
; Do not use the same AppId value in installers for other applications.
; (To generate a new GUID, click Tools | Generate GUID inside the IDE.)
AppId={{6F863544-2657-4C1C-8CB5-CD743B198932}
AppName={#MyAppName}
AppVersion={#MyAppVersion}
;AppVerName={#MyAppName} {#MyAppVersion}
AppPublisher={#MyAppPublisher}
AppPublisherURL={#MyAppURL}
AppSupportURL={#MyAppURL}
AppUpdatesURL={#MyAppURL}
DefaultDirName={reg:HKLM\Software\PAMPA WP2,Path|{#InstallDir}}\{#ExecDir}
DefaultGroupName={#MyAppName}
AllowNoIcons=yes
OutputDir=.
OutputBaseFilename=setup-update_PAMPA_WP2-cleaningVersion-{#MyAppVersion}
; SetupIconFile=Y:\tmp\1284538187_bluefish-icon.ico
Compression=lzma
SolidCompression=yes
WizardImageFile=..\Img\pampa2L.bmp
WizardSmallImageFile=..\Img\pampa2.bmp

[Languages]
Name: "french"; MessagesFile: "compiler:Languages\French.isl"

[Tasks]
Name: "desktopicon"; Description: "{cm:CreateDesktopIcon}"; GroupDescription: "{cm:AdditionalIcons}"
Name: "quicklaunchicon"; Description: "{cm:CreateQuickLaunchIcon}"; GroupDescription: "{cm:AdditionalIcons}"; Flags: unchecked; OnlyBelowVersion: 0,6.1

[Files]
Source: ".\cleaningVersion\PAMPA WP2.bat"; DestDir: "{app}"; Flags: ignoreversion
Source: ".\cleaningVersion\img\*"; DestDir: "{app}\img"; Flags: ignoreversion
Source: ".\cleaningVersion\Doc\*"; DestDir: "{app}\Doc"; Flags: ignoreversion
; NOTE: Don't use "Flags: ignoreversion" on any shared system files
Source: ".\cleaningVersion\config.r"; DestDir: "{app}"; Flags: uninsneveruninstall onlyifdoesntexist
Source: "cleaningVersion\arbre_regression.r"; DestDir: "{app}"
Source: "cleaningVersion\barplots_occurrence.R"; DestDir: "{app}"
Source: "cleaningVersion/fonctions_graphiques.R"; DestDir: "{app}"
Source: "cleaningVersion\boxplots_esp_generiques.R"; DestDir: "{app}"
Source: "cleaningVersion\boxplots_unitobs_generiques.R"; DestDir: "{app}"
Source: "cleaningVersion\calcul_simple.r"; DestDir: "{app}"
Source: "cleaningVersion\command.r"; DestDir: "{app}"
Source: "cleaningVersion\corresp-cat-benth.csv"; DestDir: "{app}"
Source: "cleaningVersion\fonctions_base.R"; DestDir: "{app}"
Source: "cleaningVersion\gestionmessages.r"; DestDir: "{app}"
Source: "cleaningVersion\Global.r"; DestDir: "{app}"
Source: "cleaningVersion\import.r"; DestDir: "{app}"
Source: "cleaningVersion\importdefaut.r"; DestDir: "{app}"
Source: "cleaningVersion\interface.r"; DestDir: "{app}"
Source: "cleaningVersion\interface_fonctions.R"; DestDir: "{app}"
Source: "cleaningVersion\load_packages.R"; DestDir: "{app}"
Source: "cleaningVersion\mkfilegroupe.r"; DestDir: "{app}"
Source: "cleaningVersion\modeles_lineaires_esp_generiques.R"; DestDir: "{app}"
Source: "cleaningVersion\modeles_lineaires_unitobs_generiques.R"; DestDir: "{app}"
Source: "cleaningVersion\modeles_lineaires_interface.R"; DestDir: "{app}"
Source: "cleaningVersion\modifinterface.r"; DestDir: "{app}"
Source: "cleaningVersion\NomsVariables.csv"; DestDir: "{app}"
Source: "cleaningVersion\PAMPA WP2.bat"; DestDir: "{app}"
Source: "cleaningVersion\requetes.r"; DestDir: "{app}"
Source: "cleaningVersion\Rprofile.site"; DestDir: "{app}"
Source: "cleaningVersion\selection_variables_fonctions.R"; DestDir: "{app}"
Source: "cleaningVersion\selection_variables_interface.R"; DestDir: "{app}"
Source: "cleaningVersion\testfichier.r"; DestDir: "{app}"
Source: "cleaningVersion\view.r"; DestDir: "{app}"
Source: "cleaningVersion\nombres_SVR.R"; DestDir: "{app}"


[Icons]
;; IconFilename: "{app}\img\Pampa.ico" pour d�finir l'icone d'un raccourci.
Name: "{group}\{#MyAppName}"; Filename: "{app}\{#MyAppExeName}"; WorkingDir: {#InstallDir}; IconFilename: "{app}\img\Pampa.ico"
Name: "{group}\{cm:UninstallProgram,{#MyAppName}}"; Filename: "{app}\{uninstallexe}"
Name: "{commondesktop}\{#MyAppName}"; Filename: "{app}\{#MyAppExeName}"; Tasks: desktopicon; WorkingDir: {#InstallDir}; IconFilename: "{app}\img\Pampa.ico"
Name: "{userappdata}\Microsoft\Internet Explorer\Quick Launch\{#MyAppName}"; Filename: "{app}\{#MyAppExeName}"; Tasks: quicklaunchicon; WorkingDir: {#InstallDir}; IconFilename: "{app}\img\Pampa.ico"

Name: "{group}\Documentation\Guide Utilisateur"; Filename: "{app}\Doc\GuideCalculsIndicateurs-WP2-Meth4-042010-modif.pdf";
Name: "{group}\Documentation\Nouveaut�s de la plateforme PAMPA WP2"; Filename: "{app}\Doc\Annexe_GuideCalculsIndicateurs-WP2-Meth4-092010.pdf";
Name: "{group}\Cr�er un rapport de bug"; Filename: "{app}\Doc\Rapport_bug_PAMPA-WP2.dot";

[Run]
Filename: "{app}\{#MyAppExeName}"; Description: "{cm:LaunchProgram,{#StringChange(MyAppName, "&", "&&")}}"; Flags: shellexec postinstall skipifsilent; WorkingDir: {#InstallDir}

[Dirs]
Name: "{#InstallDir}\Data"; Flags: uninsneveruninstall; Tasks: ; Languages:





































