################################################################################
## Nom                  : interface.r
## Objet                : interface du Programme de calcul des métriques biodiversité et ressources
## Input                : TXT
## Output               : CSV
## Toutes les fonctions appelées dans ce fichier à partir de widjet devront appartenir au fichier "command.r"
## Auteurs               : Bastien Preuss / Jérémie Habasque / Romain David
## R version            : 2.10.1
## Date de création     : Février 2008
## Date de modification : Avril 2010
################################################################################

test.f = function() {
   tkmessageBox(message="En cours de programation, patience")
}

#################################################################################
## FONCTION PRINCIPALE
## Constitution du menu d'interface
#################################################################################

tm <- tktoplevel(height=600,width=800,background="#FFFFFF")
topMenu <- tkmenu(tm)
tkconfigure(tm,menu=topMenu)
tkwm.title(tm,"CALCULS INDICATEURS PAMPA")
tkwm.maxsize(tm,1000,768) #taille maximale de la fenetre
tkwm.minsize(tm,800,600) #taille minimale de la fenetre

topFrame <- tkframe(tm,relief="groove",borderwidth=2)
imageAMP <- tclVar()  #crée un objet Tk image pour l'interface
tcl("image","create","photo",imageAMP,file=fileimage)
imgAsLabel <- tklabel(tm,image=imageAMP,bg="white")
helpframe <- tktext(tm,bg="yellow",font="arial",width=71,height=3,relief="groove",borderwidth=2)
titreAideContextuelle<-tklabel(tm,width=106,text=" Ci dessous l'aide contextuelle",background="#FFFFFF")
#tklabel(topFrame,text="Bienvenue sur l'interface TCL TK de PAMPA",background="#FFFFFF")

# on place les éléments dans un espace en trois colonnes avec tkgrid.configure -column, -columnspan, -in, -ipadx, -ipady, -padx, -pady, -row, -rowspan, et -sticky.
tkgrid(imgAsLabel,titreAideContextuelle)
tkgrid(imgAsLabel,helpframe)
#tkgrid(imgAsLabel,topFrame)
tkgrid.configure(imgAsLabel,sticky="w",rowspan=3)  #L'image fait trois lignes de haut
# puis on place les 3 objets à coté de l'image
tkgrid.configure(titreAideContextuelle,columnspan=2,row=1,column=1,sticky="n")
tkgrid.configure(helpframe,sticky="e",columnspan=2,row=2,column=1,sticky="n")
TitreSuiviOperation<-tklabel(tm,text="Suivi des opérations")
button.widgetOFF <- tkbutton(tm,text="Fermer les graphiques",command=graphics.off)

tkgrid(TitreSuiviOperation,button.widgetOFF)
tkgrid.configure(TitreSuiviOperation,columnspan=1,sticky="w")
tkgrid.configure(button.widgetOFF,columnspan=2,column=2,sticky="e")

scr <- tkscrollbar(tm, repeatinterval=2, command=function(...)tkyview(txt.w,...))
txt.w <- tktext(tm,bg="white",width=90,height=15,yscrollcommand=function(...)tkset(scr,...))
tkgrid.configure(txt.w,scr)
tkgrid.configure(txt.w,sticky="nsew",columnspan=3)
tkgrid.configure(scr,sticky="nsw",column=3)

tkgrid.configure(ResumerEspaceTravail<-tklabel(tm,text=paste("Espace de travail : ","non sélectionné"),width="134"))
tkgrid.configure(ResumerAMPetType<-tklabel(tm,text="Aucun jeu de donnée sélectionné pour le moment",width="134"))
tkgrid.configure(ResumerEspaceTravail,sticky="w",columnspan=4)
tkgrid.configure(ResumerAMPetType,sticky="w",columnspan=4)

ResumerSituationFichierUnitesObs<-tklabel(tm,text=paste("Fichier d'unités d'observations : ","non sélectionné"))
ResumerSituationFichierObs<-tklabel(tm,text=paste("Fichier d'observations : ","non sélectionné"))
ResumerSituationReferencielEspece<-tklabel(tm,text=paste("Référentiel espèce : ","non sélectionné"))

gestionMSGaide.f("etapeImport")

var1=0
var2=0
ArrayEtatFichier <- c("Type de fichier","Source","Nb Enr","Nb Champs","Sélections",
              "Fichier d'unités d'observations","non sélectionné","NA","NA","NA",
              "Fichier d'observations ","non sélectionnés","NA","NA","NA",
              "Référentiel espèce ","non sélectionné","NA","NA","NA")
tclarray <- tclArray()
dim(ArrayEtatFichier) <- c(5,4)
for (i in (0:3))                        # [!!!] [yr: 11/01/2011]
  for (j in (0:4))
     tclarray[[i,j]] <- ArrayEtatFichier[j+1,i+1]

tclRequire("Tktable")
table1 <-tkwidget(tm,"table",variable=tclarray,rows=4,cols=4,titlerows=1,selectmode="extended",colwidth=25,background="white")
tkgrid(table1)
tkgrid.configure(table1,columnspan=3,sticky="w")

  #déclaration et mise à 0 de toutes les variables de sélection dans les menus
  SelectListEsp <- tclVar(0)
  SelectFam <- tclVar(0)
  SelectBenth <- tclVar(0)
  SelectBiot  <- tclVar(0)
  SelectPhylum  <- tclVar(0)
  SelectOrdre  <- tclVar(0)
  SelectClasse  <- tclVar(0)
  SelectEndem <- tclVar(0)
  SelectEmble <- tclVar(0)
  SelectMenace <- tclVar(0)
  SelectIUCN <- tclVar(0)
  SelectAutreStatut <- tclVar(0)
  SelectCB  <- tclVar(0)
  GraphBiomasse <- tclVar(0)
  GraphDensite <- tclVar(0)
  GraphDensiteEsp  <- tclVar(0)
  GraphDensiteFam  <- tclVar(0)
  GraphHill <- tclVar(0)
  GraphPielou <- tclVar(0)
  GraphL.simpson <- tclVar(0)
  GraphSimpson <- tclVar(0)
  GraphRichesse_specifique <- tclVar(0)

#############initialisation des entrées de menu déchirables (ie qui donne accès à un sous menu)
arbreRegression <- tkmenu(topMenu,tearoff=FALSE)
analyse <- tkmenu(topMenu,tearoff=FALSE)
import <- tkmenu(topMenu,tearoff=FALSE)
selection <- tkmenu(topMenu,tearoff=FALSE)
traitement <- tkmenu(topMenu,tearoff=FALSE)
pampainfos   <- tkmenu(topMenu,tearoff=FALSE)
outils <- tkmenu(topMenu,tearoff=FALSE)
## Ajout [yr: 25/08/2010] :
modelesInferentiels <- tkmenu(topMenu,tearoff=FALSE)
analysesExplo <- tkmenu(topMenu,tearoff=FALSE)

##Troisieme niveau de menu

## Menu deroulant de "arbre de regression"
tkadd(arbreRegression,"command",label="1 facteur",command=arbre1.f)
tkadd(arbreRegression,"command",label="2 facteurs",command=arbre2.f)
tkadd(arbreRegression,"command",label="3 facteurs",command=arbre3.f)

## Graphiques :

## Ajout [yr: 14/10/2010]
tkadd(traitement,"separator")
## Ajout [yr: 11/08/2010]
tkadd(traitement, "command", label="Boxplots métrique /espèce/unité d'observation + Biodiversité...",
      background="#FFFBCF",
      command=function(){selectionVariables.f("boxplot.esp") ; winRaise.f(tm)})
## Ajout [yr: 25/10/2010]
tkadd(traitement, "command", label="Boxplots métrique /unité d'observation...",
      background="#FFFBCF",
      command=function(){selectionVariables.f("boxplot.unitobs") ; winRaise.f(tm)})
## Ajout [yr: 14/10/2010]
tkadd(traitement, "command", label="Fréquences d'occurrence...",
      background="#FFFBCF",
      command=function(){selectionVariables.f("freq_occurrence") ; winRaise.f(tm)})

## Menu deroulant de "Import de donnees"
tkadd(import,"command",label="Choix des dossiers et fichiers de données...", accelerator="CTRL+N", command = {openfile.f})
tkadd(import,"command",label="Dossiers et fichiers par defaut", accelerator="CTRL+A",
      command = function(){opendefault.f()})
tkadd(import,"separator")
tkadd(import,"command",label="Test du référentiel (espèces concernées)",underline=9,accelerator="CTRL+R",
      state="disabled", command = testfileref.f)
## tkadd(import,"command",label="Test des données importées",underline=0, accelerator="CTRL+T", state="disabled")  ## [sup] [yr: 13/01/2011]
tkadd(import,"command",label="Champs de 'TableMetrique' et TableBiodiv",underline=0, accelerator="CTRL+M",
      state="disabled")

## Sélection et recalcul :
tkadd(selection,"command",label="Selon un champs du référentiel espèce...",
      command = function(){SelectionUnCritereEsp.f() ; winRaise.f(tm)})
tkadd(selection,"command",label="Selon un champs des unités d'observation...",
      command = function(){SelectionUnCritereUnitobs.f() ; winRaise.f(tm)})

tkadd(selection,"separator")
tkadd(selection,"checkbutton",label="Par liste d'espèces (fichier)",variable=SelectListEsp,onvalue=1,offvalue=0, command =
      choixespeces.f, state="disabled")

## Analyses :
## Ajout [yr: 25/08/2010] :
tkadd(analyse, "cascade", label="Modèles inférentiels", menu=modelesInferentiels,
      background="#FFFBCF")
tkadd(analyse, "cascade", label="Analyses exploratoires", menu=analysesExplo, state="disabled")
tkadd(analyse,"separator")
tkadd(analyse,"cascade",label="Arbre de regression multivariee",menu = arbreRegression)

tkadd(modelesInferentiels, "command", label="Modèles linéaires métrique /espèce/unité d'observation + Biodiversité...",
      background="#FFFBCF",
      command=function(){selectionVariables.f("modele_lineaire") ; winRaise.f(tm)})
## Ajout [yr: 26/10/2010]
tkadd(modelesInferentiels, "command", label="Modèles linéaires métrique /unité d'observation...",
      background="#FFFBCF",
      command=function(){selectionVariables.f("modele_lineaire.unitobs") ; winRaise.f(tm)})
## Ajout [yr: 13/10/2010]
tkadd(modelesInferentiels, "command", label="Modèles linéaires sur 'présences/absences'...",
      background="#FFFBCF",
      command=function(){selectionVariables.f("pres_abs") ; winRaise.f(tm)})


## Menu deroulant de "Outils"
tkadd(outils,"command",label="Aide", accelerator="CTRL+?", command = test.f, state="disabled")
tkadd(outils,"command",label="mise à jour", state="disabled", command = test.f)
## tkadd(outils,"command",label="Options", state="disabled", command = test.f)
tkadd(outils,"command",label="Langue", state="disabled", command = test.f)
tkadd(outils,"command",label="Export de donnees", state="disabled", command = test.f)

## Menu deroulant de "Infos"
tkadd(pampainfos,"command",label="A propos de Pampa", command = test.f)
tkadd(pampainfos,"command",label="Notice utilisateur", command = test.f)

## [dep] [yr: 11/01/2011]
tkadd(import,"command",label="Voir le plan d'échantillonnage", accelerator="CTRL+P", state="disabled", command = VoirPlanEchantillonnage.f)
tkadd(import,"command",label="Info données par espèces", state="disabled", accelerator="CTRL+E", command = VoirInformationsDonneesEspeces.f)
tkadd(import,"command",label="Info données par unité d'observation", state="disabled", accelerator="CTRL+U", command = VoirInformationsDonneesUnitobs.f)

## Premier niveau de menu
tkadd(topMenu,"cascade",label="Données",menu=import) # Renommé [yr: 10/01/2011]
tkadd(topMenu,"cascade",label="Sélection et recalcul", state="disabled", menu=selection)
tkadd(topMenu,"cascade",label="Graphiques", state="disabled",menu=traitement)

tkadd(topMenu,"cascade",label="Statistiques", state="disabled",menu=analyse)
tkadd(topMenu,"cascade",label="Outils", menu=outils)
tkadd(topMenu,"cascade",label="Aide", menu=pampainfos) # Renommé [yr: 10/01/2011]
tkadd(topMenu,"command",label="Quitter", command=function() tkdestroy(tm))

gestionMSGinfo.f("start")
print("fonction interface activée")
tkfocus(tm)

# Frame des sélections
frameOverall <- tkframe(tm)
tkgrid(tklabel(frameOverall,text="Critères de sélection",relief="groove",borderwidth=2,width=135))
tkgrid.configure(frameOverall,columnspan=5)
frameUpper <- tkframe(frameOverall,borderwidth=2)
MonCritere<-tklabel(frameUpper,text="Aucun")
tkgrid(MonCritere)
frameLower <- tkframe(frameOverall,relief="groove",borderwidth=2)
MesEnregistrements<-tklabel(frameLower,text="NA")
tkgrid(MesEnregistrements)
tkgrid(frameUpper,frameLower)
tkgrid(frameOverall)
ResumerSituationEspecesSelectionnees<-tklabel(frameOverall,text="-> Nombre d'espèces concernées : NA")
tkgrid(ResumerSituationEspecesSelectionnees)
tkgrid.configure(ResumerSituationEspecesSelectionnees,columnspan=3,sticky="w")
ResumerSituationUnitobsSelectionnees<-tklabel(frameOverall,text="-> Nombre d'unités d'observation concernées : NA")
tkgrid(ResumerSituationUnitobsSelectionnees)
tkgrid.configure(ResumerSituationUnitobsSelectionnees,columnspan=3,sticky="w")
button.DataRestore <- tkbutton(tm,text="Restaurer les données",command=RestaurerDonnees.f)
tkgrid(button.DataRestore)
tkconfigure(button.DataRestore, state="disabled")

# Gestion des évènements dans la fenêtre tm (toplevel)

 tkbind(tm, "<Control-a>", function(){opendefault.f()})
 tkbind(tm, "<Control-A>", function(){opendefault.f()})
 tkbind(tm, "<Control-n>", function(){openfile.f()})
 tkbind(tm, "<Control-N>", function(){openfile.f()})
 tkbind(tm, "<Control-r>", testfileref.f)
 tkbind(tm, "<Control-R>", testfileref.f)
 ## tkbind(tm, "<Control-F1>", aide.f)
 ## tkbind(tm, "<Control-?>", aide.f)
 tkbind(tm, "<Control-p>", VoirPlanEchantillonnage.f)
 tkbind(tm, "<Control-P>", VoirPlanEchantillonnage.f)
 tkbind(tm, "<Control-e>", VoirInformationsDonneesEspeces.f)
 tkbind(tm, "<Control-E>", VoirInformationsDonneesEspeces.f)
 tkbind(tm, "<Control-u>", VoirInformationsDonneesUnitobs.f)
 tkbind(tm, "<Control-U>", VoirInformationsDonneesUnitobs.f)
 tkbind(tm, "<Control-o>", test.f)
 tkbind(tm, "<Control-O>", test.f)
 tkbind(tm, "<Control-q>", function() tkdestroy(tm))
 tkbind(tm, "<Control-Q>", function() tkdestroy(tm))

## Placement de la fenêtre :
winSmartPlace.f(tm)
