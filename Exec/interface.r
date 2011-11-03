## ################################################################################
## Nom                  : interface.r
## Objet                : interface du Programme de calcul des métriques biodiversité et ressources
## Input                : TXT
## Output               : CSV
## Toutes les fonctions appelées dans ce fichier à partir de widjet devront appartenir au fichier "command.r"
## Auteurs               : Bastien Preuss / Jérémie Habasque / Romain David
## R version            : 2.10.1
## Date de création     : Février 2008
## Date de modification : Avril 2010
## ################################################################################

test.f <- function ()
{
    tkmessageBox(message="En cours de programation, patience")
}

## #################################################################################
## FENÊTRE PRINCIPALE
## #################################################################################

tm <- tktoplevel(height=600, width=800, background="#FFFFFF")

tkwm.title(tm, "Calcul d'indicateurs PAMPA WP2")

tkwm.maxsize(tm, 1000, 768)             # taille maximale de la fenetre
tkwm.minsize(tm, 800, 550)              # taille minimale de la fenetre

########################################################################################################################
## Menus :

F.menu <- tkframe(tm)

## initialisation des entrées de menu déchirables (i.e. qui donne accès à un sous menu)

## Boutons de menus :
MB.import <- tkmenubutton(F.menu, text="Données", state="normal")
MB.selection <- tkmenubutton(F.menu, text="Sélection et recalcul", state="disabled")
MB.traitement <- tkmenubutton(F.menu, text="Graphiques", state="disabled")
MB.analyse <- tkmenubutton(F.menu, text="Statistiques", state="disabled")
MB.outils <- tkmenubutton(F.menu, text="Outils", state="normal")
MB.pampainfos <- tkmenubutton(F.menu, text="Aide", state="normal")

## Menus associés :
import <- tkmenu(MB.import, tearoff=FALSE)
selection <- tkmenu(MB.selection, tearoff=FALSE)
traitement <- tkmenu(MB.traitement, tearoff=FALSE)
analyse <- tkmenu(MB.analyse, tearoff=FALSE)
outils <- tkmenu(MB.outils, tearoff=FALSE)
pampainfos <- tkmenu(MB.pampainfos, tearoff=FALSE)

## Sous menus :
arbreRegression <- tkmenu(analyse, tearoff=FALSE)
modelesInferentiels <- tkmenu(analyse, tearoff=FALSE)
analysesExplo <- tkmenu(analyse, tearoff=FALSE)

## Association des boutons et menus :
tkconfigure(MB.import, menu=import, activebackground="#81a5dc")
tkconfigure(MB.selection, menu=selection, activebackground="#81a5dc")
tkconfigure(MB.traitement, menu=traitement, activebackground="#81a5dc")
tkconfigure(MB.analyse, menu=analyse, activebackground="#81a5dc")
tkconfigure(MB.outils, menu=outils, activebackground="#81a5dc")
tkconfigure(MB.pampainfos, menu=pampainfos, activebackground="#81a5dc")

## Bouton pour quitter :
B.quit.main <- tkbutton(F.menu, text=" Quitter... ",
                        command=function()
                    {
                        quitConfirm.f(tm)
                    },
                        activebackground="#81a5dc")

## Placement des menus :
tkpack(MB.import, MB.selection, MB.traitement, MB.analyse, MB.outils, MB.pampainfos,
       side="left")
tkpack(B.quit.main, side="right", padx=2, pady=2)

tkgrid(F.menu, sticky="ew", columnspan=4)


## Remplissage des menus :

########################################
## Menu deroulant des Données :

## Imports :
tkadd(import, "command", label="Choix des dossiers et fichiers de données...",
      accelerator="CTRL+N", command = {openfile.f})

tkadd(import, "command", label="Dossiers et fichiers par defaut", accelerator="CTRL+A",
      command = function()
  {
      rm(fileName1, fileName2, fileName3, envir=.GlobalEnv)
      eval(source("./Exec/config.r", encoding="latin1"), envir=.GlobalEnv) # rechargement de la configuration.
      opendefault.f()                                                      # chargement des données.
  })

## Informations sur les données :
tkadd(import, "separator")

tkadd(import, "command", label="Test du référentiel (espèces concernées)", underline=9, accelerator="CTRL+R",
      state="disabled", command = testfileref.f)

## tkadd(import, "command", label="Test des données importées", underline=0,
##       accelerator="CTRL+T", state="disabled")  ## [sup] [yr: 13/01/2011]

## tkadd(import, "command", label="Champs de 'TableMetrique' et TableBiodiv", underline=0, accelerator="CTRL+M",
##       state="disabled")

tkadd(import, "command", label="Voir le plan d'échantillonnage", accelerator="CTRL+P", state="disabled",
      command = VoirPlanEchantillonnage.f)

tkadd(import, "command", label="Info données par espèces", state="disabled", accelerator="CTRL+E",
      command = VoirInformationsDonneesEspeces.f)

tkadd(import, "command", label="Info données par unité d'observation",
      state="disabled", accelerator="CTRL+U",
      command = VoirInformationsDonneesUnitobs.f)

########################################
## Sélection et recalcul :

tkadd(selection, "command", label="Selon un champ du référentiel espèce...",
      command = function ()
  {
      SelectionUnCritereEsp.f()
      winRaise.f(tm)
  })

tkadd(selection, "command", label="Selon un champ des unités d'observation...",
      command = function ()
  {
      SelectionUnCritereUnitobs.f()
      winRaise.f(tm)
  })

tkadd(selection, "separator")
tkadd(selection, "checkbutton", label="Par liste d'espèces (fichier)",
      ## variable=SelectListEsp, # à quoi sert cette variable ? [???]
      ## onvalue=1, offvalue=0,
      command = choixespeces.f, state="disabled")

## Restauration des données :
tkadd(selection, "separator")
tkadd(selection, "command", label="Restaurer les données originales",
      state="disabled",
      command = function ()
  {
      RestaurerDonnees.f()
      winRaise.f(tm)
  })

########################################
## Graphiques :

## Info :
tkadd(traitement, "separator")

tkadd(traitement, "command", label="Représentation par espèce ou  classe de taille d'espèce :",
      foreground="darkred", background="#dadada",
      state="normal")

## Boxplots espèces :
tkadd(traitement, "command", label="Boxplots métrique /espèce/unité d'observation...",
      background="#FFFBCF",
      command=function ()
  {
      selectionVariables.f("boxplot.esp")
      winRaise.f(tm)
  })

## Barplots espèces :
tkadd(traitement, "command", label="Fréquences d'occurrence (/espèce sur des unité d'observation)...",
      background="#FFFBCF",
      command=function ()
  {
      selectionVariables.f("freq_occurrence")
      winRaise.f(tm)
  })

## Info :
tkadd(traitement, "separator")

tkadd(traitement, "command", label="Agrégation de plusieurs espèces ou classes de taille par unité d'observation :",
      foreground="darkred", background="#dadada",
      state="normal")

## Boxplots unitobs :
tkadd(traitement, "command", label="Boxplots métrique /unité d'observation (dont biodiversité)...",
      background="#FFFBCF",
      command=function ()
  {
      selectionVariables.f("boxplot.unitobs")
      winRaise.f(tm)
  })

## Barplots unitobs :
tkadd(traitement, "command", label="Fréquences d'occurrence (/facteurs d'unité d'observation)...",
      background="#FFFBCF",
      command=function ()
  {
      selectionVariables.f("freq_occurrence.unitobs")
      winRaise.f(tm)
  })

########################################
## Analyses :

## Menus déchirables...

## ...modèles inférentiels :
tkadd(analyse, "cascade", label="Modèles inférentiels", menu=modelesInferentiels,
      background="#FFFBCF")

## ...modèles exploratoires (à faire) :
tkadd(analyse, "cascade", label="Analyses exploratoires", menu=analysesExplo, state="disabled")

## Modèles inférentiels :

## Info :
tkadd(modelesInferentiels, "command", label="Analyse par espèce ou classe de taille d'espèce :",
      foreground="darkred", background="#dadada",
      state="normal")

## (G)LMs espèces
tkadd(modelesInferentiels, "command", label="Modèles linéaires métrique /espèce/unité d'observation...",
      background="#FFFBCF",
      command=function ()
  {
      selectionVariables.f("modele_lineaire")
      winRaise.f(tm)
  })

## MRT espèces :
tkadd(modelesInferentiels, "command", label=paste("Arbres de régression multivariée,",
                                      " métrique / espèces / unité d'observation...", sep=""),
      background="#FFFBCF",
      command=function ()
  {
      selectionVariables.f("MRT.esp")
      winRaise.f(tm)
  })

## Info :
tkadd(modelesInferentiels, "separator")

tkadd(modelesInferentiels, "command", label="Agrégation de plusieurs espèces ou classes de taille par unité d'observation :",
      foreground="darkred", background="#dadada",
      state="normal")

## (G)LMs unitobs :
tkadd(modelesInferentiels, "command", label="Modèles linéaires métrique /unité d'observation (dont biodiversité)...",
      background="#FFFBCF",
      command=function ()
  {
      selectionVariables.f("modele_lineaire.unitobs")
      winRaise.f(tm)
  })

## MRT unitobs :
tkadd(modelesInferentiels, "command", label=paste("Arbres de régression multivariée,",
                                      " métrique /unité d'observation (dont biodiversité)...", sep=""),
      background="#FFFBCF",
      command=function ()
  {
      selectionVariables.f("MRT.unitobs")
      winRaise.f(tm)
  })


########################################
## Menu deroulant des outils :

tkadd(outils, "command", label="Options graphiques...", command = choixOptionsGraphiques.f, state="normal")
tkadd(outils, "separator")

tkadd(outils, "command", label="Éditer le fichier de configuration",
      command=function()
  {
      shell.exec(paste(basePath, "/Exec/config.r", sep=""))
  })
tkadd(outils, "separator")

tkadd(outils, "command", label="Créer un rapport de bug", state="normal",
      command = function()
  {
      shell.exec(paste(basePath, "/Exec/Doc/Rapport_bug_PAMPA-WP2.dot", sep=""))
  })
tkadd(outils, "separator")

tkadd(outils, "command", label="mise à jour (site de téléchargement)", state="normal",
      command = function()
  {
      browseURL("http://www.projet-pampa.fr/wiki/doku.php/wp2:telechargement")
  })

## tkadd(outils, "command", label="Langue", state="disabled", command = test.f)

## tkadd(outils, "command", label="Export de donnees", state="disabled", command = test.f)


########################################
## Menu deroulant de l'aide

## Accès aux documentations :
tkadd(pampainfos, "command", label="Documentation en ligne",
      command = function()
  {
      browseURL("http://www.projet-pampa.fr/wiki/doku.php/wp2:wp2#documentation")
  })

tkadd(pampainfos, "command", label="Documentation (locale)",
      command = function()
  {
      shell.exec(dir(paste(basePath, "/Exec/Doc", sep=""),
                     full.names=TRUE)[grep("^Guide",
                                           dir(paste(basePath, "/Exec/Doc", sep="")), perl=TRUE)])
  })

tkadd(pampainfos, "command", label="Forum d'entraide",
      command = function()
  {
      browseURL("http://www.projet-pampa.fr/forum/viewforum.php?id=2")
  })

## tkadd(pampainfos, "command", label="Nouveautés de la plateforme (locale)",
##       command = function()
##   {
##       shell.exec(dir(paste(basePath, "/Exec/Doc", sep=""),
##                      full.names=TRUE)[grep("^Annexe_Guide",
##                                            dir(paste(basePath, "/Exec/Doc", sep="")), perl=TRUE)])
##   })

## À propos... :
tkadd(pampainfos, "separator")
tkadd(pampainfos, "command", label="À propos de la plateforme...", command = apropos.f)


########################################################################################################################
## Ajhout des autres éléments :

## Frame principale :
## topFrame <- tkframe(tm, relief="groove", borderwidth=2)

## Logo :
imageAMP <- tclVar()
tcl("image", "create", "photo", imageAMP, file=fileimage) # crée un objet Tk image pour l'interface.

imgAsLabel <- tklabel(tm, image=imageAMP, bg="white") # -> label avec image.

## Frames d'aide :
titreAideContextuelle <- tklabel(tm, width=106,
                                 text=" Ci dessous l'aide contextuelle",
                                 background="#FFFFFF")

helpframe <- tktext(tm, bg="yellow", font="arial",
                    width=71, height=3, # taille.
                    relief="groove", borderwidth=2)


tkgrid(imgAsLabel, titreAideContextuelle)
tkgrid(imgAsLabel, helpframe)
## tkgrid(imgAsLabel, topFrame)

tkgrid.configure(imgAsLabel, sticky="w", rowspan=3) # L'image fait trois lignes de haut. [!!!]

## puis on place les 3 objets à coté de l'image :
tkgrid.configure(titreAideContextuelle, columnspan=2, row=1, column=1, sticky="n")
tkgrid.configure(helpframe, sticky="e", columnspan=2, row=2, column=1, sticky="n")

## Suivi des opérations :
TitreSuiviOperation <- tklabel(tm, text="Suivi des opérations")

tkgrid(TitreSuiviOperation, columnspan=1, sticky="w")

## ... zone de texte dédiée avec son "ascenceur" :
scr <- tkscrollbar(tm, repeatinterval=2,
                   command=function(...)tkyview(txt.w, ...))

txt.w <- tktext(tm, bg="white", width=90, height=15,
                yscrollcommand=function(...)tkset(scr, ...),
                wrap="word")            # Éviter les coupures de mots.

tkgrid.configure(txt.w, scr)            # [???]

tkgrid.configure(txt.w, sticky="nsew", columnspan=3) # Faire bien coïncider les bords de la zone de texte
tkgrid.configure(scr, sticky="nsw", column=3)        # et de l'ascenceur.

## Zone d'information sur l'espace de travail :
tkgrid.configure(ResumerEspaceTravail <-
                 tklabel(tm,
                         text=paste("Espace de travail : ", "non sélectionné"),
                         width="134"))

tkgrid.configure(ResumerAMPetType <-
                 tklabel(tm,
                         text="Aucun jeu de donnée sélectionné pour le moment",
                         width="134"))

tkgrid.configure(ResumerEspaceTravail, sticky="w", columnspan=4)
tkgrid.configure(ResumerAMPetType, sticky="w", columnspan=4)


gestionMSGaide.f("etapeImport") ## [???]

###############################################################
## Zone d'information sur les données et sélections (tableau) :

## Création d'un tableau tcl :
ArrayEtatFichier <- matrix(c("Type de fichier", "Source", "Nb enregistrements", "Nb champs", "",
                             "Fichier d'unités d'observations", "non sélectionné", "NA", "NA", "NA",
                             "Fichier d'observations ", "non sélectionné", "NA", "NA", "NA",
                             "Référentiel espèce ", "non sélectionné", "NA", "NA", "NA"),
                           nrow=4, ncol=5, byrow=TRUE)

tclarray <- tclArray()

for (i in 0:(nrow(ArrayEtatFichier) - 1))
{
    for (j in 0:(ncol(ArrayEtatFichier) - 1))
    {
        tclarray[[i, j]] <- ArrayEtatFichier[i+1, j+1] # !!! décalage de 1 dans les indices.
    }
}

## Création d'une table tk utilisant ce tableau :
tclRequire("Tktable")

table1 <-tkwidget(tm, "table", variable=tclarray,
                  rows=4, cols=4, titlerows=1, # seulement 4 colonnes au départ => ajout ultérieur.
                  selectmode="extended",
                  colwidth=15, background="white")

## Affichage et placement de la table :
tkgrid(table1)
tkgrid.configure(table1, columnspan=3, sticky="ew")

########################################################################################################################
gestionMSGinfo.f("start")

runLog.f(msg=c("Chargement de l'interface :"))

tkfocus(tm)

####################################
## Frame d'info sur les sélections :
frameOverall <- tkframe(tm)

tkgrid(tklabel(frameOverall, text="Critères de sélection",
               relief="groove", borderwidth=2, width=135))

tkgrid.configure(frameOverall, columnspan=4)

frameUpper <- tkframe(frameOverall, borderwidth=2)

## Info sur les critères de sélection :
MonCritere <- tklabel(frameUpper, text="Tout",
                      wraplength=750, justify="left")
tkgrid(MonCritere)

tkgrid(frameUpper)
tkgrid(frameOverall)

ResumerSituationEspecesSelectionnees <-
    tklabel(frameOverall,
            text="-> Nombre d'espèces dans le fichier d'observation : NA")

tkgrid(ResumerSituationEspecesSelectionnees)
tkgrid.configure(ResumerSituationEspecesSelectionnees, columnspan=3, sticky="w")

ResumerSituationUnitobsSelectionnees <-
    tklabel(frameOverall,
            text="-> Nombre d'unités d'observation dans le fichier d'observation : NA")

tkgrid(ResumerSituationUnitobsSelectionnees)
tkgrid.configure(ResumerSituationUnitobsSelectionnees, columnspan=3, sticky="w")

##############################
## Boutons de bas de fenêtre :

## Restauration des données originales :
button.DataRestore <- tkbutton(tm, text="Restaurer les données", command=RestaurerDonnees.f)
tkgrid(button.DataRestore, pady=5, padx=5, sticky="w")

tkconfigure(button.DataRestore, state="disabled")

## Fermeture de tous les graphiques :
button.widgetOFF <- tkbutton(tm, text="Fermer les graphiques", command=graphics.off)

tkgrid(button.widgetOFF, column=2,
       row=tkObj.gridInfo.f(button.DataRestore)["row"], # même ligne que la restauration des données
       columnspan=2, sticky="e", pady=5, padx=5)

####################################################################################################
## Gestion des évènements dans la fenêtre tm (toplevel) => raccourcis claviers :
tkbind(tm, "<Control-a>",
       function()
   {
       eval(source("./Exec/config.r", encoding="latin1"), envir=.GlobalEnv) # rechargement de la configuration.
       opendefault.f()
   })
tkbind(tm, "<Control-A>", function()
   {
       eval(source("./Exec/config.r", encoding="latin1"), envir=.GlobalEnv) # rechargement de la configuration.
       opendefault.f()
   })

tkbind(tm, "<Control-n>", openfile.f)
tkbind(tm, "<Control-N>", openfile.f)

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

tkbind(tm, "<Control-q>", function()
   {
       quitConfirm.f(tm)
   })
tkbind(tm, "<Control-Q>", function()
   {
       quitConfirm.f(tm)
   })

## Largeur des colonnes:
ColAutoWidth.f(table1)

## Placement de la fenêtre :
winSmartPlace.f(tm)
