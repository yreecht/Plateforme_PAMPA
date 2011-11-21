################################################################################
# Nom               : TopMenu.r
# Type              : Programme
# Objet             : Constitution du menu d'interface général d'où l'on accède à toutes les fonctions de calculs
#                     Rajout de tableaux et de messages d'information sur les données importées
#                     Ce programme commence par l'appel des différents scripts contenant les fonctions de calcul
# Input             : clic souris
# Output            : lancement de fonctions
# Auteur            : Elodie Gamp
# R version         : 2.8.1
# Date de création  : Avril 2011
# Sources
################################################################################

## ** Version **
options(versionPAMPA = "1.0-0")

# Script de chargement des packages
source("C:/PAMPA/SCRIPTS WP3/PackagesManquants.r")

### packages nécessaires pour les scripts
require(tcltk)
#library("maptools")     # pour la catographie (apparemment non validie dans les versions récentes de R, à voir)
library(mvpart)
library(TeachingDemos)
library(plotrix)
# pr les stats
library(tkrplot)
library(MASS)
library(gamlss)
library(nnet)

### Appel des différents scripts pour le calcul des métriques
# Script des fonctions pour l'interface
source("C:/PAMPA/SCRIPTS WP3/InterfaceFonctions.r")
# Script des fonctions graphique d'enquêtes
source("C:/PAMPA/SCRIPTS WP3/fonctionsGraphEnquete.R")
# script des fonctions d'interface des données d'enquêtes
source("C:/PAMPA/SCRIPTS WP3/InterfaceOpinion.r")
# Script des fonctions graphique de fréquentation
source("C:/PAMPA/SCRIPTS WP3/fonctionsGraphFrequentation.r")
# script des fonctions d'interface des données de fréquentation
source("C:/PAMPA/SCRIPTS WP3/InterfacePression.r")
# Script des fonctions génériques d'extrapolation
source("C:/PAMPA/SCRIPTS WP3/ExtrapolationFonctions.r")
# Script des fonctions d'interface d'extrapolation
source("C:/PAMPA/SCRIPTS WP3/InterfaceExtrapolation.r")
# Script des fonctions bootstrap (enquêtes)
source("C:/PAMPA/SCRIPTS WP3/BootstrapOpinionNew.r")

# Script de choix de l'utilisateur
source("C:/PAMPA/SCRIPTS WP3/ChoixUtilisateur.r")
### Scripts des bootstrap opinion et extrapolation à rajouter une fois finalisés

### création du message d'aide
  # Définit les informations à renseigner dans l'interface selon les cas rencontrés 
  # (importation, sélection, restitution, poolage)
gestionMSGaide.f  = function(namemsg) {
  MGS="message à renseigner"
  if (namemsg=="etapeImport")
    MGS="\n Importez vos données pour pouvoir calculer les métriques"  
  if (namemsg=="Selection")
    MGS=paste("Vous avez restreint vos données selon le critère : ",critereSelection, "\n Pour récupérer la totalité de vos données, cliquez sur 'Restitution Données'.")  
  if (namemsg=="traitements")
    MGS="\n Les données sont importées vous pouvez calculer les métriques que vous souhaitez" 
  if (namemsg=="restitution")
    MGS=" \n Les données initiales ont été restituées. Les métriques seront calculées sur l'ensembre de vos données."   
  if (namemsg=="Poolage")
    MGS = "\n Toutes les périodes d'échantillonnage ont été poolées entre elles. \n Les métriques sont calculées sans prendre en considération les différences temporelles."
  tkinsert(helpframe,"end",paste(MGS,"\n"))
  tkyview.moveto(helpframe,1)
}

### mise à jour du tableau d'échantillonnage
  # Résume dans un tableau le nombre d'enregistrements présents dans chacune des tables de données.
  # Mise à jour de ces informations en cas de sélection/restitution de la part de l'utilisateur.
MiseajourTableau.f = function(tclarray) {
  tclarray[[1,1]] <- "freqtot"        # fréquentation
  tclarray[[1,2]] <- dim(freqtot)[1]  # nb d'enregistrements
  tclarray[[1,3]] <- dim(freqtot)[2]  # nb de champs
  tclarray[[2,1]] <- "peche"          # pêche récréative
  tclarray[[2,2]] <- dim(peche)[1]
  tclarray[[2,3]] <- dim(peche)[2]
  tclarray[[3,1]] <- "captures"       # captures du jour
  tclarray[[3,2]] <- dim(captures)[1]
  tclarray[[3,3]] <- dim(captures)[2]
  tclarray[[4,1]] <- "capturesAn"     # captures annuelles (principales espèces pêchées déclarées)
  tclarray[[4,2]] <- dim(capturesAn)[1]
  tclarray[[4,3]] <- dim(capturesAn)[2]
  tclarray[[5,1]] <- "plaisance"       # plaisance
  tclarray[[5,2]] <- dim(plaisance)[1]
  tclarray[[5,3]] <- dim(plaisance)[2]
  tclarray[[6,1]] <- "plongee"         # plongée
  tclarray[[6,2]] <- dim(plongee)[1]
  tclarray[[6,3]] <- dim(plongee)[2]
  tclarray[[7,1]] <- "excursion"       # PMT SSM
  tclarray[[7,2]] <- dim(excursion)[1]
  tclarray[[7,3]] <- dim(excursion)[2]
  tclarray[[8,1]] <- "refSpatial"      # référentiel spatial du site
  tclarray[[8,2]] <- dim(refSpatial)[1]
  tclarray[[8,3]] <- dim(refSpatial)[2]
  tclarray[[9,1]] <- "refEspeces"       # référentiel espèces (Méditerranée ou outremer)
  tclarray[[9,2]] <- dim(refEspeces)[1]
  tclarray[[9,3]] <- dim(refEspeces)[2]
  tclarray[[10,1]] <- "refEngin"       # référentiel engin
  tclarray[[10,2]] <- dim(refEngin)[1]
  tclarray[[10,3]] <- dim(refEngin)[2]
  tclarray[[11,1]] <- "tousQuest"       # table formée par R rassemblant l'ensemble des questionnaires (tous usagers)
  tclarray[[11,2]] <- dim(tousQuest)[1]
  tclarray[[11,3]] <- dim(tousQuest)[2]
}

### mise à jour du tableau d'info enquêtes pêche
  # Pour les enquêtes de pêche récréative, résume dans un tableau le nombre 
  # de refus, de déjà enquêtés et de questionnaires avec captures.
MiseajourTableauInfo.f = function(tclarray2) {
  tclarray2[[1,1]] <- length(peche$refus[which(peche$refus=="oui")])
  tclarray2[[1,2]] <- length(peche$dejaEnq[which(peche$dejaEnq=="oui")])
  tclarray2[[1,3]] <- length(peche$capture[which(peche$capture=="non")])
}

# construction du menu d'interface
tm <- tktoplevel(height=500,width=1500,bg="lightyellow")
winSmartPlace.f(tm)
topMenu <- tkmenu(tm)
tkconfigure(tm,menu=topMenu)
tkwm.title(tm,"INDICATEURS RELATIFS AUX USAGES")
tkwm.maxsize(tm,1500,768) #taille maximale de la fenetre
tkwm.minsize(tm,1500,600) #taille minimale de la fenetre

topFrame <- tkframe(tm,relief="groove",borderwidth=2)  
imageAMP <- tclVar()    #crée un objet Tk image pour l'interface
tcl("image","create","photo",imageAMP,file="C:/PAMPA/SCRIPTS WP3/img/pampa2.GIF")
imgAsLabel <- tklabel(tm,image=imageAMP,bg="white")
helpframe <- tktext(tm,bg="skyblue3",font="arial",width=120,height=3,relief="groove",borderwidth=2)
titreAideContextuelle<-tklabel(tm,width=106,text=" \n Informations données ",background="lightyellow")

# on place les éléments dans un espace en trois colonnes avec tkgrid.configure -column, -columnspan, -in, -ipadx, -ipady, -padx, -pady, -row, -rowspan, et -sticky.
tkgrid(imgAsLabel,titreAideContextuelle)
tkgrid(imgAsLabel,helpframe)
tkgrid.configure(imgAsLabel,sticky="w",rowspan=3)  #L'image fait trois lignes de haut
# puis on place les 3 objets à coté de l'image
tkgrid.configure(titreAideContextuelle,columnspan=3,row=1,column=2,sticky="n")
tkgrid.configure(helpframe,sticky="e",columnspan=3,row=2,column=2,sticky="n")
button.widgetRESTIT <- tkbutton(tm,text="Restituer les données",background="yellow",command=function () RestitutionDonnees.f())
button.widgetOFF <- tkbutton(tm,text="Fermer les graphiques",background="purple",command=graphics.off)
button.widgetQUIT <- tkbutton(tm,text="Quitter",background="deeppink3",command=function() tkdestroy(tm))

espace<-tklabel(tm,width=106,text="  ",background="lightyellow")
tkgrid.configure(espace,columnspan=3,column=2,sticky="n")
tkgrid(button.widgetRESTIT,button.widgetOFF,button.widgetQUIT)
tkgrid.configure(button.widgetRESTIT,columnspan=1,sticky="e")
tkgrid.configure(button.widgetOFF,columnspan=2,column=1,sticky="e")
tkgrid.configure(button.widgetQUIT,columnspan=3,column=2,sticky="e")

#tkgrid.configure(ResumerEspaceTravail<-tklabel(tm,background="lightyellow",text=paste("\n Espace de travail : ","non sélectionné"),width="200"))
tkgrid.configure(ResumerAMP<-tklabel(tm,background="lightyellow",text="\n Actuellement aucun jeu de données n'a été importé. \n \n",width="200"))
#tkgrid.configure(ResumerEspaceTravail,sticky="w",columnspan=4)
tkgrid.configure(ResumerAMP,sticky="w",columnspan=4)

ResumerSituationFreq<-tklabel(tm,text=paste("Fréquentation : ","non sélectionné"))
ResumerSituationPeche<-tklabel(tm,text=paste("Enquêtes Pêche : ","non sélectionné"))
ResumerSituationCaptures<-tklabel(tm,text=paste("Captures associées : ","non sélectionné"))
ResumerSituationCapturesAn<-tklabel(tm,text=paste("Captures annuelles : ","non sélectionné"))
ResumerSituationPlaisance<-tklabel(tm,text=paste("Enquêtes Plaisance : ","non sélectionné"))
ResumerSituationPlongée<-tklabel(tm,text=paste("Enquêtes Plongée : ","non sélectionné"))
ResumerSituationExcursion<-tklabel(tm,text=paste("Enquêtes Excursion : ","non sélectionné"))
ResumerSituationRefSpatial<-tklabel(tm,text=paste("Référentiel Spatial : ","non sélectionné"))
ResumerSituationRefEspeces<-tklabel(tm,text=paste("Référentiel Espèces : ","non sélectionné"))
ResumerSituationRefEngin<-tklabel(tm,text=paste("Référentiel Engin : ","non sélectionné"))

gestionMSGaide.f("etapeImport")
var1=0
var2=0
### tableau d'échantillonnage
ArrayEtatFichier <- c("Jeu_de_données","Nom_du_tableau","Nb_enregistrements","Nb_champs",
              "Comptages_de_la_Fréquentation","à_importer","NA","NA",
              "Enquêtes_Pêche_Récréative","à_importer","NA","NA",
              "Captures_du_jour_associées","à_importer","NA","NA",
              "Captures_annuelles_déclarées","à_importer","NA","NA",
              "Enquêtes_Plaisance","à_importer","NA","NA",
              "Enquêtes_Plongée","à_importer","NA","NA",
              "Enquêtes_PMT_SSM","à_importer","NA","NA",
              "Référentiel_Spatial","à_importer","NA","NA",
              "Référentiel_Espèces","à_importer","NA","NA",
              "Référentiel_Engin","à_importer","NA","NA",
              "Tous_Questionnaires","à_construire_par_R","NA","NA")
tclarray <- tclArray()
dim(ArrayEtatFichier) <- c(4,12)
for (i in (0:11))
  for (j in (0:3))
     tclarray[[i,j]] <- ArrayEtatFichier[j+1,i+1]
tclRequire("Tktable")
table1 <-tkwidget(tm,"table",variable=tclarray,rows=12,cols=4,titlerows=1,selectmode="extended",colwidth=33,background="white")

### info enquêtes
ArrayInfo <- c("Jeu_de_données","Nb_refus","Nb_déjà_enquêté","Nb_sans_capture",
              "Enquêtes_Pêche","NA","NA","NA")
tclarray2 <- tclArray()
dim(ArrayInfo) <- c(4,2)
for (i in (0:1))
  for (j in (0:3))
     tclarray2[[i,j]] <- ArrayInfo[j+1,i+1]
tclRequire("Tktable")
table2 <-tkwidget(tm,"table",variable=tclarray2,rows=2,cols=4,titlerows=1,selectmode="extended",colwidth=20,background="white")

# position des tableaux
tkgrid(table1)
tkgrid.configure(table1,columnspan=4,column=2,sticky="w")
tkgrid.configure(espace,columnspan=4,column=2,row=1,sticky="n")
tkgrid(table2)
tkgrid.configure(table2,columnspan=4,column=2,sticky="w")


#################    création de la barre de menu   ############################

# déclaration de toutes les variables intermédiaires (sous-menus)
ChoixUtilisateur <- tkmenu(topMenu,tearoff=FALSE,bg="lightsalmon")

################################################################################

####      Appel des fonctions
  
# Menu déroulant des choix disponibles pour l'utilisateur
#  tkadd(ChoixUtilisateur,"command",label="Choix de la surface/du linéaire", command = ChoixSurface.f)
#  tkadd(ChoixUtilisateur,"command",label="Choix de l'année en cours", command = ChoixAnnee.f)
  tkadd(ChoixUtilisateur,"command",label="Pooler toutes les périodes d'échantillonnage pour les calculs", command = ChoixPoolerAnnees.f)
  tkadd(ChoixUtilisateur,"command",label="Restreindre les données à certaines période d'échantillonnage", command = ChoixPeriodEchant.f)
  tkadd(ChoixUtilisateur,"command",label="Restreindre les données selon la caractéristique : résident", command = ChoixResident.f)
  tkadd(ChoixUtilisateur,"command",label="Restreindre les données à certais usagers", command = ChoixUsagers.f)
  tkadd(ChoixUtilisateur,"command",label="Restituer les données", command = function () RestitutionDonnees.f())

######################## Premier niveau de menu ################################

  tkadd(topMenu,"command",label="Import des données",command = function() source("C:/PAMPA/SCRIPTS WP3/ImportDonnees.r"))
  tkadd(topMenu,"cascade",label="Etude de la fréquentation", command = interfaceFrequentation.f)
  tkadd(topMenu,"cascade",label="Etude des enquêtes", command = interfaceEnquete.f)
  tkadd(topMenu,"cascade",label="Extrapolation de la fréquentation", command = interfaceExtrapolation.f)
  tkadd(topMenu,"command",label="Indices composites", command = function() tkmessageBox(message="Cette fonction n'est pas encore disponible",icon="warning",type="ok")) #IndiceCompo)   
  tkadd(topMenu,"command",label="Bootstrap", command = function() tkmessageBox(message="Cette fonction n'est pas encore disponible",icon="warning",type="ok")) #Bootstrap)   
#  tkadd(topMenu,"cascade",label="Choix de l'utilisateur", menu = ChoixUtilisateur)
################################################################################

tkfocus(tm)
