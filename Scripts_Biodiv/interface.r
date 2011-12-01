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

tkwm.title(tm, "Indicateurs relatifs à la biodiversité et aux ressources")

tkwm.maxsize(tm,
             tkwinfo("screenwidth", tm),
             as.numeric(tclvalue(tkwinfo("screenheight", tm))) - 30)             # taille maximale de la fenetre
tkwm.minsize(tm, 800, 500)              # taille minimale de la fenetre

## Couleurs :
.MenuBackground <- "#FFFBCF"
.FrameBackground <- "#FFF6BF"
.TopMenueBackground <- "#CBDDED"

########################################################################################################################
## Menus :

F.menu <- tkframe(tm, background=.TopMenueBackground)

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
tkconfigure(MB.import, menu=import, activebackground="#81a5dc", background=.TopMenueBackground)
tkconfigure(MB.selection, menu=selection, activebackground="#81a5dc", background=.TopMenueBackground)
tkconfigure(MB.traitement, menu=traitement, activebackground="#81a5dc", background=.TopMenueBackground)
tkconfigure(MB.analyse, menu=analyse, activebackground="#81a5dc", background=.TopMenueBackground)
tkconfigure(MB.outils, menu=outils, activebackground="#81a5dc", background=.TopMenueBackground)
tkconfigure(MB.pampainfos, menu=pampainfos, activebackground="#81a5dc", background=.TopMenueBackground)

## Bouton pour quitter :
B.quit.main <- tkbutton(F.menu, text=" Quitter... ",
                        command=function()
                    {
                        quitConfirm.f(tm)
                    },
                        activebackground="#81a5dc")


## Fermeture de tous les graphiques :
B.graphicsOFF <- tkbutton(F.menu, text="Fermer les graphiques", command=graphics.off,
                          activebackground="#81a5dc")


## Placement des menus :
tkpack(MB.import, MB.selection, MB.traitement, MB.analyse, MB.outils, MB.pampainfos,
       side="left")
tkpack(B.quit.main, B.graphicsOFF, side="right", padx=5, pady=2)

tkgrid(F.menu, sticky="ew", columnspan=4)


## Remplissage des menus :

########################################
## Menu deroulant des Données :

## Imports :
tkadd(import, "command", label="Choix des dossiers et fichiers de données...",
      accelerator="CTRL+N", command = {openfile.f},
      background=.MenuBackground)

tkadd(import, "command", label="Dossiers et fichiers par defaut", accelerator="CTRL+A",
      command = function()
  {
      rm(fileName1, fileName2, fileName3, envir=.GlobalEnv)
      eval(source("./Scripts_Biodiv/config.r", encoding="latin1"), envir=.GlobalEnv) # rechargement de la configuration.

      testVar.f(requiredVar=get("requiredVar", envir=.GlobalEnv), env = .GlobalEnv)

      ## chargement (conditionnel) des données :
      if (all(sapply(get("requiredVar", envir=.GlobalEnv), exists)))
      {
          opendefault.f()
      }else{}
  },
      background=.MenuBackground)

## Informations sur les données :
tkadd(import, "separator", background=.MenuBackground)

tkadd(import, "command", label="Test du référentiel (espèces concernées)", underline=9, accelerator="CTRL+R",
      state="disabled", command = testfileref.f,
      background=.MenuBackground)

## tkadd(import, "command", label="Test des données importées", underline=0,
##       accelerator="CTRL+T", state="disabled")  ## [sup] [yr: 13/01/2011]

## tkadd(import, "command", label="Champs de 'TableMetrique' et TableBiodiv", underline=0, accelerator="CTRL+M",
##       state="disabled")

tkadd(import, "command", label="Plan d'échantillonnage basique", accelerator="CTRL+P", state="disabled",
      command = VoirPlanEchantillonnage.f,
      background=.MenuBackground)

tkadd(import, "command", label="Info données par espèces", state="disabled", accelerator="CTRL+E",
      command = VoirInformationsDonneesEspeces.f,
      background=.MenuBackground)

tkadd(import, "command", label="Info données par unité d'observation",
      state="disabled", accelerator="CTRL+U",
      command = VoirInformationsDonneesUnitobs.f,
      background=.MenuBackground)

########################################
## Sélection et recalcul :

tkadd(selection, "command", label="Selon un champ du référentiel espèce...",
      background=.MenuBackground,
      command = function ()
  {
      SelectionUnCritereEsp.f()
      winRaise.f(tm)
  })

tkadd(selection, "command", label="Selon un champ des unités d'observation...",
      background=.MenuBackground,
      command = function ()
  {
      SelectionUnCritereUnitobs.f()
      winRaise.f(tm)
  })

tkadd(selection, "separator", background=.MenuBackground)
tkadd(selection, "checkbutton", label="Par liste d'espèces (fichier)",
      background=.MenuBackground,
      ## variable=SelectListEsp, # à quoi sert cette variable ? [???]
      ## onvalue=1, offvalue=0,
      command = choixespeces.f, state="disabled")

## Restauration des données :
tkadd(selection, "separator", background=.MenuBackground)
tkadd(selection, "command", label="Restaurer les données originales",
      background=.MenuBackground,
      state="disabled",
      command = function ()
  {
      RestaurerDonnees.f()
      winRaise.f(tm)
  })

########################################
## Graphiques :

## Info :
tkadd(traitement, "separator", background=.MenuBackground)

tkadd(traitement, "command", label="Par espèce ou classe de taille d'espèce :",
      foreground="darkred", background="#dadada",
      font=tkfont.create(weight="bold", size=8),
      state="normal")

## Boxplots espèces :
tkadd(traitement, "command", label="Boxplots, métrique /espèce/unité d'observation...",
      background=.MenuBackground,
      command=function ()
  {
      selectionVariables.f("boxplot.esp")
      winRaise.f(tm)
  })

## Barplots espèces :
tkadd(traitement, "command", label="Fréquences d'occurrence (/espèce sur des unité d'observation)...",
      background=.MenuBackground,
      command=function ()
  {
      selectionVariables.f("freq_occurrence")
      winRaise.f(tm)
  })

## Info :
tkadd(traitement, "separator", background=.MenuBackground)

tkadd(traitement, "command", label="Plusieurs espèces ou classes de taille, agrégées par unité d'observation :",
      foreground="darkred", background="#dadada",
      font=tkfont.create(weight="bold", size=8),
      state="normal")

## Boxplots unitobs :
tkadd(traitement, "command", label="Boxplots, métrique /unité d'observation (dont biodiversité)...",
      background=.MenuBackground,
      command=function ()
  {
      selectionVariables.f("boxplot.unitobs")
      winRaise.f(tm)
  })

## Barplots unitobs :
tkadd(traitement, "command", label="Fréquences d'occurrence (/facteurs d'unité d'observation)...",
      background=.MenuBackground,
      command=function ()
  {
      selectionVariables.f("freq_occurrence.unitobs")
      winRaise.f(tm)
  })

########################################
## Analyses :

## Menus déchirables...

## ## ...modèles inférentiels :
## tkadd(analyse, "cascade", label="Modèles inférentiels", menu=modelesInferentiels,
##       background=.MenuBackground)

## ## ...modèles exploratoires (à faire) :
## tkadd(analyse, "cascade", label="Analyses exploratoires", menu=analysesExplo, state="disabled")

modelesInferentiels <- analyse

## Modèles inférentiels :

## Info :
tkadd(modelesInferentiels, "command", label="Par espèce ou classe de taille d'espèce :",
      foreground="darkred", background="#dadada",
      font=tkfont.create(weight="bold", size=8),
      state="normal")

## (G)LMs espèces
tkadd(modelesInferentiels, "command", label="Modèles linéaires, métrique /espèce/unité d'observation...",
      background=.MenuBackground,
      command=function ()
  {
      selectionVariables.f("modele_lineaire")
      winRaise.f(tm)
  })

## MRT espèces :
tkadd(modelesInferentiels, "command", label=paste("Arbres de régression multivariée,",
                                      " métrique / espèces / unité d'observation...", sep=""),
      background=.MenuBackground,
      command=function ()
  {
      selectionVariables.f("MRT.esp")
      winRaise.f(tm)
  })

## Info :
tkadd(modelesInferentiels, "separator", background=.MenuBackground)

tkadd(modelesInferentiels, "command", label="Plusieurs espèces ou classes de taille, agrégées par unité d'observation :",
      foreground="darkred", background="#dadada",
      font=tkfont.create(weight="bold", size=8),
      state="normal")

## (G)LMs unitobs :
tkadd(modelesInferentiels, "command", label="Modèles linéaires, métrique /unité d'observation (dont biodiversité)...",
      background=.MenuBackground,
      command=function ()
  {
      selectionVariables.f("modele_lineaire.unitobs")
      winRaise.f(tm)
  })

## MRT unitobs :
tkadd(modelesInferentiels, "command", label=paste("Arbres de régression multivariée,",
                                      " métrique /unité d'observation (dont biodiversité)...", sep=""),
      background=.MenuBackground,
      command=function ()
  {
      selectionVariables.f("MRT.unitobs")
      winRaise.f(tm)
  })


########################################
## Menu deroulant des outils :

tkadd(outils, "command", label="Options graphiques...", command = choixOptionsGraphiques.f, state="normal",
      background=.MenuBackground)
tkadd(outils, "separator", background=.MenuBackground)

tkadd(outils, "command", label="Éditer le fichier de configuration",
      background=.MenuBackground,
      command=function()
  {
      shell.exec(paste(basePath, "/Scripts_Biodiv/config.r", sep=""))
  })
tkadd(outils, "separator", background=.MenuBackground)

tkadd(outils, "command", label="Créer un rapport de bug", state="normal",
      background=.MenuBackground,
      command = function()
  {
      shell.exec(paste(basePath, "/Scripts_Biodiv/Doc/Rapport_bug_PAMPA-WP2.dot", sep=""))
  })
tkadd(outils, "separator", background=.MenuBackground)

tkadd(outils, "command", label="mise à jour (site de téléchargement)", state="normal",
      background=.MenuBackground,
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
      background=.MenuBackground,
      command = function()
  {
      browseURL("http://www.projet-pampa.fr/wiki/doku.php/wp2:wp2#documentation")
  })

tkadd(pampainfos, "command", label="Documentation (locale)",
      background=.MenuBackground,
      command = function()
  {
      shell.exec(dir(paste(basePath, "/Scripts_Biodiv/Doc", sep=""),
                     full.names=TRUE)[grep("^Guide",
                                           dir(paste(basePath, "/Scripts_Biodiv/Doc", sep="")), perl=TRUE)])
  })

tkadd(pampainfos, "command", label="Forum d'entraide",
      background=.MenuBackground,
      command = function()
  {
      browseURL("http://www.projet-pampa.fr/forum/viewforum.php?id=2")
  })


## À propos... :
tkadd(pampainfos, "separator", background=.MenuBackground)
tkadd(pampainfos, "command", label="À propos de la plateforme...", command = apropos.f,
      background=.MenuBackground)


########################################################################################################################
## Ajout des autres éléments :

## Frame principale :
## topFrame <- tkframe(tm, relief="groove", borderwidth=2)

## Logo :
imageAMP <- tclVar()
tcl("image", "create", "photo", imageAMP, file=fileimage) # crée un objet Tk image pour l'interface.

F.aide <- tkframe(tm, background="white")

## Dans la répartition d'espace entre les deux colonne,
## on attribue tout l'espace supplémentaire à la première
tkgrid.columnconfigure(F.aide, 0, minsize=1, weight=0)
tkgrid.columnconfigure(F.aide, 1, weight=9)

imgAsLabel <- tklabel(F.aide, image=imageAMP, bg="white") # -> label avec image.

## Frames d'aide :
titreAideContextuelle <- tklabel(F.aide, #width=106,
                                 text="Vous pouvez...",
                                 background="#FFF07F", justify="left",
                                 font=tkfont.create(family="arial", size=10))

helpframe <- tktext(F.aide, bg=.FrameBackground, font=tkfont.create(family="arial", size=10),
                    ## width=95,
                    height=3, # taille.
                    relief="groove", borderwidth=2)


## tkgrid(imgAsLabel, sticky="w")
tkgrid(imgAsLabel,
       titreAideContextuelle## , ## columnspan=4, sticky="w",
       ## padx=5
       )

tkgrid(imgAsLabel,
       helpframe## , ## columnspan=4, sticky="ew",
       ## padx=5
       )

## tkgrid(tklabel(tm, text="", background="white"))
## tkgrid(imgAsLabel, topFrame)

tkgrid.configure(imgAsLabel, sticky="nsew", rowspan=2,
                 padx=7) # L'image fait trois lignes de haut. [!!!]

## ## puis on place les 3 objets à coté de l'image :
tkgrid.configure(titreAideContextuelle, columnspan=1, row=0, column=1, sticky="ws", padx=5)
tkgrid.configure(helpframe, columnspan=1, row=1, column=1, sticky="ewn", padx=5)

tkgrid(F.aide, columnspan=4, sticky="ew", pady=5)

########################################################################################################################



########################################################################################################################


####################################
## Frame d'info sur les fichiers et sélections :
frameOverall <- tkframe(tm, background=.FrameBackground, borderwidth=2)

## Zone d'information sur l'espace de travail :
tkgrid(ResumerEspaceTravail <-
       tklabel(frameOverall,
               text=paste("Espace de travail : ", "non sélectionné."),
               background=.FrameBackground,
               width=117,
               font=tkfont.create(size=9)))

tkgrid(ResumerAMPetType <-
       tklabel(frameOverall,
               text="Aucun jeu de données chargé pour l'instant !",
               background=.FrameBackground,
               font=tkfont.create(size=9)))

tkgrid.configure(ResumerEspaceTravail, sticky="ew", columnspan=3)
tkgrid.configure(ResumerAMPetType, sticky="ew", columnspan=3)


gestionMSGaide.f("etapeImport") ## [???]

F.titreSelect <- tkframe(frameOverall, relief="groove", borderwidth=0, background="#FFEE70")
## tkgrid.propagate(frameOverall, "1")

## Restauration des données originales :
B.DataRestore <- tkbutton(F.titreSelect, text="Restaurer les données", command=RestaurerDonnees.f)

## Titre des critères de sélection :
L.criteres <- tklabel(F.titreSelect, text="       Critère(s) de sélection :", background="#FFEE70",
                      font=tkfont.create(size=8), foreground="darkred"## , width=110
                      )


## Affichage/placement du titre et du bouton:
tkpack(B.DataRestore, side="right", pady=2, padx=5)
tkpack(L.criteres, pady=2)

tkconfigure(B.DataRestore, state="disabled") # Bouton de restauration désactivé en premier lieu.


## Info sur les critères de sélection :
frameUpper <- tkframe(frameOverall, relief="groove", borderwidth=0)

MonCritere <- tklabel(frameUpper, text="Tout",
                      wraplength=750, justify="left",
                      background=.FrameBackground)



## Placement des éléments d'info sur les sélections :

## Séparateur :
tkgrid(ttkseparator(frameOverall, orient="horizontal"), columnspan=3, sticky="ew")

## Placement du titre de sélection (frame)
tkgrid(F.titreSelect, sticky="ew", columnspan=3)

tkgrid(MonCritere)

tkgrid(frameUpper, columnspan=3, pady=5)

tkgrid(frameOverall)
tkgrid.configure(frameOverall, columnspan=4, padx=5, pady=10, sticky="ew")


###############################################################
## Zone d'information sur les données et sélections (tableau) :

## Création d'un tableau tcl :
ArrayEtatFichier <- matrix(c("Type de fichier", "Nom de fichier", "Nb champs", "Nb enregistrements", "",
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

table1 <-tkwidget(frameOverall, "table", variable=tclarray,
                  rows=4, cols=4, titlerows=1, # seulement 4 colonnes au départ => ajout ultérieur.
                  selectmode="extended",
                  colwidth=15, background="white")

## Affichage et placement de la table :
tkgrid(table1, columnspan=3, sticky="ew")

## Nombres effectifs "d'espèces" et d'unités d'observation :
ResumerSituationEspecesSelectionnees <-
    tklabel(frameOverall,
            text="-> Nombre de codes espèce dans le fichier d'observation : NA",
            background=.FrameBackground, state="disabled")

ResumerSituationUnitobsSelectionnees <-
    tklabel(frameOverall,
            text="-> Nombre d'unités d'observation dans le fichier d'observation : NA",
            background=.FrameBackground, state="disabled")

tkgrid(ResumerSituationEspecesSelectionnees, columnspan=3, sticky="w", padx=5)

tkgrid(ResumerSituationUnitobsSelectionnees, sticky="w", columnspan=3, padx=5)


########################################################################################################################

## Suivi des opérations :

F.log <- tkframe(tm, bg="white")
## tkgrid.propagate(F.log, 1)

tkgrid(F.log, sticky="ew", columnspan=4, padx=5, pady=5)

TitreSuiviOperation <- tklabel(F.log, text="Rapport de chargement/sélection/export :", background="#FFFFFF",
                               anchor="w")

## ... zone de texte dédiée avec son "ascenceur" :
scr <- tkscrollbar(F.log, repeatinterval=2,
                   command=function(...)tkyview(txt.w, ...))

txt.w <- tktext(F.log, bg="white",  height=9,
                yscrollcommand=function(...)tkset(scr, ...),
                wrap="word")            # Éviter les coupures de mots.

## Dans la répartition d'espace entre les deux colonne,
## on attribue tout l'espace supplémentaire à la première
tkgrid.columnconfigure(F.log, 1, minsize=1, weight=0)
tkgrid.columnconfigure(F.log, 0, weight=9)

tkgrid(TitreSuiviOperation, columnspan=2, sticky="ew")

tkgrid(txt.w, scr, sticky="nsew")

tcl("update")
tkfocus(txt.w)

runLog.f(msg=c("Chargement de l'interface :"))

tkfocus(tm)

##############################
## Boutons de bas de fenêtre :

## tkgrid(button.widgetOFF, column=2,
##        row=tkObj.gridInfo.f(button.DataRestore)["row"], # même ligne que la restauration des données
##        columnspan=2, sticky="e", pady=5, padx=5)

####################################################################################################
## Gestion des évènements dans la fenêtre tm (toplevel) => raccourcis claviers :
tkbind(tm, "<Control-a>",
       function()
   {
       eval(source("./Scripts_Biodiv/config.r", encoding="latin1"), envir=.GlobalEnv) # rechargement de la configuration.

       testVar.f(requiredVar=get("requiredVar", envir=.GlobalEnv), env = .GlobalEnv)

       if (all(sapply(get("requiredVar", envir=.GlobalEnv), exists)))
       {
           opendefault.f()
       }else{}
   })
tkbind(tm, "<Control-A>", function()
   {
       eval(source("./Scripts_Biodiv/config.r", encoding="latin1"), envir=.GlobalEnv) # rechargement de la configuration.

       testVar.f(requiredVar=get("requiredVar", envir=.GlobalEnv), env = .GlobalEnv)

       if (all(sapply(get("requiredVar", envir=.GlobalEnv), exists)))
       {
           opendefault.f()
       }else{}
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
