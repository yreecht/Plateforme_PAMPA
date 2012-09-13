; Script generated by the Inno Setup Script Wizard.
; SEE THE DOCUMENTATION FOR DETAILS ON CREATING INNO SETUP SCRIPT FILES!

#define MyAppVersion "2.0"

#define MyAppName "PAMPA"
#define MyAppNameWP2 "PAMPA Ressources & Biodiversit�"
#define MyAppNameWP3 "PAMPA Usages"
#define MyAppPublisher "Ifremer"
#define MyAppURL "http://wwz.ifremer.fr/pampa/"
#define InstallDir "C:\PAMPA"
#define ExecDir "Scripts_Biodiv"
#define appCommune "Scripts_communs"
#define appUsage "Scripts_Usages"
#define MyAppCommuneExeName "PAMPA.bat"
#define MyAppExeName "PAMPA WP2.bat"
#define MyAppUsageExeName "PAMPA WP3.bat"

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
OutputBaseFilename=setup-update_PAMPA-{#MyAppVersion}
LicenseFile=.\Scripts_Biodiv\LICENCE-GPL-3.0.fr.txt
; SetupIconFile=Y:\tmp\1284538187_bluefish-icon.ico
Compression=lzma
SolidCompression=yes
WizardImageFile=.\Img\pampa2L.bmp
WizardSmallImageFile=.\Img\pampa2.bmp

[Languages]
Name: "french"; MessagesFile: "compiler:Languages\French.isl"

[Tasks]
Name: "desktopicon"; Description: "{cm:CreateDesktopIcon}"; GroupDescription: "{cm:AdditionalIcons}"
Name: "desktopiconAdd"; Description: "Ic�nes sur le bureau pour chaque interface individuelle"; GroupDescription: "{cm:AdditionalIcons}"

Name: "quicklaunchicon"; Description: "{cm:CreateQuickLaunchIcon}"; GroupDescription: "{cm:AdditionalIcons}"; Flags: unchecked; OnlyBelowVersion: 0,6.1

Name: "quicklaunchiconAdd"; Description: "Ic�nes dans la barre de lancement rapide pour chaque interface individuelle"; GroupDescription: "{cm:AdditionalIcons}"; Flags: unchecked; OnlyBelowVersion: 0,6.1

[Files]
;;####################
; Interface commune :
Source: ".\Scripts_communs\interface_PAMPA.R"; DestDir: "{app}\..\{#appCommune}"
Source: ".\Scripts_communs\PAMPA.bat"; DestDir: "{app}\..\{#appCommune}"
Source: ".\Scripts_communs\Rprofile.site"; DestDir: "{app}\..\{#appCommune}"
Source: ".\Scripts_communs\img\biodiv1.gif"; DestDir: "{app}\..\{#appCommune}\img"
Source: ".\Scripts_communs\img\biodiv2.gif"; DestDir: "{app}\..\{#appCommune}\img"
Source: ".\Scripts_communs\img\usage1.gif"; DestDir: "{app}\..\{#appCommune}\img"
Source: ".\Scripts_communs\img\usage2.gif"; DestDir: "{app}\..\{#appCommune}\img"

; Routines WP2 :
Source: ".\Scripts_Biodiv\PAMPA WP2.bat"; DestDir: "{app}"; Flags: ignoreversion
Source: ".\Scripts_Biodiv\img\*"; DestDir: "{app}\img"; Flags: ignoreversion
Source: ".\Scripts_Biodiv\Doc\*"; DestDir: "{app}\Doc"; Flags: ignoreversion
Source: ".\Scripts_Biodiv\Config.R"; DestDir: "{app}"; Flags: uninsneveruninstall onlyifdoesntexist
; NOTE: Don't use "Flags: ignoreversion" on any shared system files

;;####################
;; Scripts R :
Source: "Scripts_Biodiv\Agregations_generiques.R"; DestDir: "{app}"
Source: "Scripts_Biodiv\Arbres_regression_esp_generiques.R"; DestDir: "{app}"
Source: "Scripts_Biodiv\Arbres_regression_unitobs_generiques.R"; DestDir: "{app}"
Source: "Scripts_Biodiv\Barplots_esp_generiques.R"; DestDir: "{app}"
Source: "Scripts_Biodiv\Barplots_occurrence.R"; DestDir: "{app}"
Source: "Scripts_Biodiv\Barplots_occurrence_unitobs.R"; DestDir: "{app}"
Source: "Scripts_Biodiv\Barplots_unitobs_generiques.R"; DestDir: "{app}"
Source: "Scripts_Biodiv\Boxplots_esp_generiques.R"; DestDir: "{app}"
Source: "Scripts_Biodiv\Boxplots_unitobs_generiques.R"; DestDir: "{app}"
Source: "Scripts_Biodiv\Calcul_poids.R"; DestDir: "{app}"
Source: "Scripts_Biodiv\Calcul_tables_metriques.R"; DestDir: "{app}"
Source: "Scripts_Biodiv\Calcul_tables_metriques_LIT.R"; DestDir: "{app}"
Source: "Scripts_Biodiv\Calcul_tables_metriques_SVR.R"; DestDir: "{app}"
Source: "Scripts_Biodiv\Chargement_fichiers.R"; DestDir: "{app}"
Source: "Scripts_Biodiv\Chargement_manuel_fichiers.R"; DestDir: "{app}"
Source: "Scripts_Biodiv\Demo_cartes.R"; DestDir: "{app}"
Source: "Scripts_Biodiv\Fonctions_base.R"; DestDir: "{app}"
Source: "Scripts_Biodiv\Fonctions_graphiques.R"; DestDir: "{app}"
Source: "Scripts_Biodiv\Gestionmessages.R"; DestDir: "{app}"
Source: "Scripts_Biodiv\Initialisation.R"; DestDir: "{app}"
Source: "Scripts_Biodiv\Interface_fonctions.R"; DestDir: "{app}"
Source: "Scripts_Biodiv\Interface_principale.R"; DestDir: "{app}"
Source: "Scripts_Biodiv\Load_packages.R"; DestDir: "{app}"
Source: "Scripts_Biodiv\Main.R"; DestDir: "{app}"
Source: "Scripts_Biodiv\Modeles_lineaires_esp_generiques.R"; DestDir: "{app}"
Source: "Scripts_Biodiv\Modeles_lineaires_interface.R"; DestDir: "{app}"
Source: "Scripts_Biodiv\Modeles_lineaires_unitobs_generiques.R"; DestDir: "{app}"
Source: "Scripts_Biodiv\Nombres_SVR.R"; DestDir: "{app}"
Source: "Scripts_Biodiv\Selection_donnees.R"; DestDir: "{app}"
Source: "Scripts_Biodiv\Selection_variables_fonctions.R"; DestDir: "{app}"
Source: "Scripts_Biodiv\Selection_variables_interface.R"; DestDir: "{app}"
Source: "Scripts_Biodiv\Testfichier.R"; DestDir: "{app}"
Source: "Scripts_Biodiv\View.R"; DestDir: "{app}"

;; Autres fichiers de la plateforme :
Source: "Scripts_Biodiv\corresp-cat-benth.csv"; DestDir: "{app}"
Source: "Scripts_Biodiv\NomsVariables_fr.csv"; DestDir: "{app}"
Source: "Scripts_Biodiv\NomsVariables_en.csv"; DestDir: "{app}"
Source: "Scripts_Biodiv\Rprofile.site"; DestDir: "{app}"

;; Fichiers de licence :
Source: "Scripts_Biodiv\LICENCE-GPL-2.fr.txt"; DestDir: "{app}\.."
Source: "Scripts_Biodiv\LICENCE-GPL-2.txt"; DestDir: "{app}\.."
Source: "Scripts_Biodiv\LICENCE-GPL-3.0.fr.txt"; DestDir: "{app}\.."
Source: "Scripts_Biodiv\LICENCE-GPL-3.0.txt"; DestDir: "{app}\.."

;;####################
;; Routines WP3 :
Source: ".\Scripts_Usages\img\pampa2.GIF"; DestDir: "{app}\..\{#appUsage}\img"; Flags: ignoreversion
Source: ".\Resultats_Usages\*"; DestDir: "{app}\..\Resultats_Usages"; Flags: recursesubdirs createallsubdirs

Source: ".\Scripts_Usages\BootstrapEnquetes.r"; DestDir: "{app}\..\{#appUsage}"
Source: ".\Scripts_Usages\BootstrapFrequentationExtrapolee.r"; DestDir: "{app}\..\{#appUsage}"
Source: ".\Scripts_Usages\CalculIndicateurComposite.r"; DestDir: "{app}\..\{#appUsage}"
Source: ".\Scripts_Usages\FonctionsExtrapolation.r"; DestDir: "{app}\..\{#appUsage}"
Source: ".\Scripts_Usages\FonctionsFreqMeteo.r"; DestDir: "{app}\..\{#appUsage}"
Source: ".\Scripts_Usages\FonctionsGenerales.r"; DestDir: "{app}\..\{#appUsage}"
Source: ".\Scripts_Usages\FonctionsGraphEnquetes.r"; DestDir: "{app}\..\{#appUsage}"
Source: ".\Scripts_Usages\FonctionsGraphFrequentation.r"; DestDir: "{app}\..\{#appUsage}"
Source: ".\Scripts_Usages\ImportDonnees.r"; DestDir: "{app}\..\{#appUsage}"
Source: ".\Scripts_Usages\InfoApropos.r"; DestDir: "{app}\..\{#appUsage}"
Source: ".\Scripts_Usages\InterfaceEnquetes.r"; DestDir: "{app}\..\{#appUsage}"
Source: ".\Scripts_Usages\InterfaceExtrapolation.r"; DestDir: "{app}\..\{#appUsage}"
Source: ".\Scripts_Usages\InterfaceFonctions.r"; DestDir: "{app}\..\{#appUsage}"
Source: ".\Scripts_Usages\InterfaceFrequentation.r"; DestDir: "{app}\..\{#appUsage}"
Source: ".\Scripts_Usages\InterfaceStatsEnquetes.r"; DestDir: "{app}\..\{#appUsage}"
Source: ".\Scripts_Usages\InterfaceStatsFreq.r"; DestDir: "{app}\..\{#appUsage}"
Source: ".\Scripts_Usages\PackagesManquants.r"; DestDir: "{app}\..\{#appUsage}"
Source: ".\Scripts_Usages\TestsStatistiquesEnquetes.r"; DestDir: "{app}\..\{#appUsage}"
Source: ".\Scripts_Usages\TestsStatistiquesFreq.r"; DestDir: "{app}\..\{#appUsage}"
Source: ".\Scripts_Usages\TestsStatsFonctionsBase.r"; DestDir: "{app}\..\{#appUsage}"
Source: ".\Scripts_Usages\TopMenu.r"; DestDir: "{app}\..\{#appUsage}"

;; Scripts de lancement Windows :
Source: ".\Scripts_Usages\PAMPA WP3.bat"; DestDir: "{app}\..\{#appUsage}"
Source: ".\Scripts_Usages\Rprofile.site"; DestDir: "{app}\..\{#appUsage}"

[Icons]
;; Icones principales :
Name: "{group}\{#MyAppName}"; Filename: "{app}\..\{#appCommune}\{#MyAppCommuneExeName}"; WorkingDir: {#InstallDir}; IconFilename: "{app}\img\Pampa.ico"
Name: "{commondesktop}\{#MyAppName}"; Filename: "{app}\..\{#appCommune}\{#MyAppCommuneExeName}"; Tasks: desktopicon; WorkingDir: {#InstallDir}; IconFilename: "{app}\img\Pampa.ico"
Name: "{userappdata}\Microsoft\Internet Explorer\Quick Launch\{#MyAppName}"; Filename: "{app}\..\{#appCommune}\{#MyAppCommuneExeName}"; Tasks: quicklaunchicon; WorkingDir: {#InstallDir}; IconFilename: "{app}\img\Pampa.ico"

;; Icones WP2 :
;; IconFilename: "{app}\img\Pampa.ico" pour d�finir l'icone d'un raccourci.
Name: "{group}\{#MyAppNameWP2}"; Filename: "{app}\{#MyAppExeName}"; WorkingDir: {#InstallDir}; IconFilename: "{app}\img\Pampa.ico"
Name: "{group}\{cm:UninstallProgram,{#MyAppName}}"; Filename: "{app}\{uninstallexe}"
Name: "{commondesktop}\{#MyAppNameWP2}"; Filename: "{app}\{#MyAppExeName}"; Tasks: desktopiconAdd; WorkingDir: {#InstallDir}; IconFilename: "{app}\img\Pampa.ico"
Name: "{userappdata}\Microsoft\Internet Explorer\Quick Launch\{#MyAppNameWP2}"; Filename: "{app}\{#MyAppExeName}"; Tasks: quicklaunchiconAdd; WorkingDir: {#InstallDir}; IconFilename: "{app}\img\Pampa.ico"

Name: "{group}\Documentation\Guide Utilisateur Ressources & Biodiversit�"; Filename: "{app}\Doc\Guide_plateforme_WP2_Meth4.pdf";
;; Name: "{group}\Documentation\Nouveaut�s de la plateforme PAMPA WP2"; Filename: "{app}\Doc\Annexe_GuideCalculsIndicateurs-WP2-Meth4-092010.pdf";
Name: "{group}\Cr�er un rapport de bug"; Filename: "{app}\Doc\Rapport_bug_PAMPA-WP2.dot";

;; Icones WP3 :
Name: "{group}\{#MyAppNameWP3}"; Filename: "{app}\..\{#appUsage}\{#MyAppUsageExeName}"; WorkingDir: {#InstallDir}; IconFilename: "{app}\img\Pampa.ico"
Name: "{commondesktop}\{#MyAppNameWP3}"; Filename: "{app}\..\{#appUsage}\{#MyAppUsageExeName}"; Tasks: desktopiconAdd; WorkingDir: {#InstallDir}; IconFilename: "{app}\img\Pampa.ico"
Name: "{userappdata}\Microsoft\Internet Explorer\Quick Launch\{#MyAppNameWP3}"; Filename: "{app}\..\{#appUsage}\{#MyAppUsageExeName}"; Tasks: quicklaunchiconAdd; WorkingDir: {#InstallDir}; IconFilename: "{app}\img\Pampa.ico"

[Run]
Filename: "{app}\..\{#appCommune}\{#MyAppCommuneExeName}"; Description: "{cm:LaunchProgram,{#StringChange(MyAppName, "&", "&&")}}"; Flags: shellexec postinstall skipifsilent; WorkingDir: {#InstallDir}

[Dirs]
Name: "{#InstallDir}\Data"; Flags: uninsneveruninstall;
Name: "{#InstallDir}\Donnees_Usages"; Flags: uninsneveruninstall;
