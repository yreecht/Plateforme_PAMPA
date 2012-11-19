#-*- coding: latin-1 -*-

## Plateforme PAMPA de calcul d'indicateurs de ressources & biodiversité
##   Copyright (C) 2008-2012 Ifremer - Tous droits réservés.
##
##   Ce programme est un logiciel libre ; vous pouvez le redistribuer ou le
##   modifier suivant les termes de la "GNU General Public License" telle que
##   publiée par la Free Software Foundation : soit la version 2 de cette
##   licence, soit (à votre gré) toute version ultérieure.
##
##   Ce programme est distribué dans l'espoir qu'il vous sera utile, mais SANS
##   AUCUNE GARANTIE : sans même la garantie implicite de COMMERCIALISABILITÉ
##   ni d'ADÉQUATION À UN OBJECTIF PARTICULIER. Consultez la Licence Générale
##   Publique GNU pour plus de détails.
##
##   Vous devriez avoir reçu une copie de la Licence Générale Publique GNU avec
##   ce programme ; si ce n'est pas le cas, consultez :
##   <http://www.gnu.org/licenses/>.

### File: Options.R
### Time-stamp: <>
###
### Author: Yves Reecht
###
####################################################################################################
### Description:
###
###
####################################################################################################

########################################################################################################################
initialiseGraphOptions.f <- function()
{
    ## Purpose: (Ré-)initialiser les options graphiques (éventuellement
    ##          persistantes) spécifiques aux graphiques PAMPA WP2.
    ## ----------------------------------------------------------------------
    ## Arguments: Aucun.
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date:  6 août 2010, 10:10

    options(GraphPAMPA=TRUE,                    # Sert à savoir si les options ont déjà été définies.
            P.maxExclu = FALSE,                 # Suppressions des données suppérieures à une certaine proportion du
                                        # maximum ?
            P.GraphPartMax = 0.95,              # Proportion du maximum à conserver si P.maxExclu == TRUE
            P.NbObs = TRUE,                     # Afichage sur le graphique du nombre d'observations
                                        # par boite à moustache.
            P.NbObsCol = "orange",              # Couleur d'affichage des nombres d'observations.
            P.pointMoyenne = FALSE,             # Affichage des moyennes (points) sur le graphique.
            P.pointMoyenneCol = "blue",         # Couleur d'affichage des moyennes (points).
            P.valMoyenne = TRUE,                # Affichage des moyennes (valeurs) sur le graphique.
            P.valMoyenneCol = "blue",           # Couleur d'affichage des moyennes (valeurs).
            P.MinNbObs = 1,                     # ??
            P.sepGroupes = TRUE,                # Séparateurs du premier niveau de regroupements sur un même graphique ?
            P.sepGroupesCol = "red",            # Couleur des séparateurs de groupes.
            P.graphPDF = FALSE,                 # Sorties graphiques en pdf ?
            P.graphPNG = FALSE,                 # Sorties graphiques en png ?
            P.plusieursGraphPage = FALSE,       # Plusieurs graphiques par page/fenêtre ?
            P.ncolGraph = 2,                    # Nombres de colonnes de graphiques (si P.plusieursGraphPage est TRUE).
            P.nrowGraph = 2,                    # Nombres de lignes de graphiques (si P.plusieursGraphPage est TRUE).
            P.PDFunFichierPage = FALSE,         # Créer un fichier par page pour les sorties PDF ?
            P.NbDecimal = 2,                    # Nombre de décimales à afficher sur les graphiques
            P.legendeCouleurs = TRUE,           # Afficher la légende pour le facteur identifié par une des couleurs ?
            P.colPalette = "défaut",            # Type de palette de couleur.
            P.statusOrder = c("RI", "RE", "IN", # Ordre des nivaux de protection pour les graphiques et analyses.
                              "Z1", "I1",
                              "PP", "RP",
                              "Z2", "I2",
                              "Z3", "I3",
                              "HR", "OUT",
                              "Z4"),
            P.graphPaper = FALSE,               # Graphiques adaptés pour la publication (pas de titre, format plus
                                                # petit,...) ?
            P.warnings = TRUE,                  # Affichage des avertissement (graph tronqué, petits effectifs) ?
            P.pointMoyenneCex = 1,              # Taille des points pour affichage de la moyenne.
            P.pointMoyennePch = 18,             # Type de point pour affichage de la moyenne.
            P.cex = 1,                          # Taille générale des caractères.
            P.graphWMF = FALSE,                 # Sauvegarde des graphiques affichés à l'écran en WMF (Windows) ?
            P.pdfEmbedFonts = TRUE,             # Inclusion des polices dans les pdfs ?
            P.lang = "fr",                      # Langue des graphiques ("fr" ou "en")... n'affecte que les axes.
            P.barplotStat="moyenne",            # Statistique des barplots ("mean", "moyenne", "médiane" ou "median").
            P.saveData=TRUE,                    # Sauvegarde des données de graphiques et analyses ?
            P.saveStats=TRUE,                   # Sauvegarde des informations sur les données (stats incluses) ?
            P.axesLabels=TRUE,                  # Affichage des noms d'axes ?
            ## ####################################################################################################
            ## Classe des options (pour conversion depuis les variables tcl) :
            P.optionsClass = c(P.maxExclu="logical", P.NbObs="logical", P.NbObsCol="character",
                               P.pointMoyenne="logical", P.pointMoyenneCol="character", P.valMoyenne="logical",
                               P.valMoyenneCol="character", "P.GraphPartMax"="numeric",
                               P.MinNbObs="integer", P.sepGroupes="logical", P.sepGroupesCol="character",
                               P.graphPDF="logical", P.graphPNG="logical", P.plusieursGraphPage="logical",
                               P.ncolGraph="integer",
                               P.nrowGraph="integer", P.PDFunFichierPage="logical", P.NbDecimal="integer",
                               P.legendeCouleurs="logical", P.colPalette="character", P.statusOrder="character",
                               P.graphPaper="logical", P.warnings="logical",
                               P.pointMoyenneCex="numeric", P.pointMoyennePch="integer", P.cex="numeric",
                               P.graphWMF="logical", P.pdfEmbedFonts="logical",
                               P.lang="character", P.barplotStat="character", P.saveData="logical",
                               P.saveStats="logical", P.axesLabels="logical")
            )

    ## On crée les palettes de couleurs :
    makeColorPalettes.f()

    ## Initialisation de la langue des variables de graphiques :
    init.GraphLang.f()
}

########################################################################################################################
changeColor.f <- function(color, colTclVal, widgetName, envBase, env)
{
    ## Purpose: changer une couleur d'option et reconfigurer un widget.
    ## ----------------------------------------------------------------------
    ## Arguments: color : couleur initiale.
    ##            colTclVal : nom de la variable tcl dans la liste P.options.
    ##            widgetName : nom du widget (pour recolorer).
    ##            envBase : environnement où sont définies les options
    ##            env : l'environnement appelant.
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date:  7 nov. 2012, 17:00

    color <- tclvalue(tcl("tk_chooseColor", initialcolor=color, title="Choisir une couleur"))

    if (nchar(color) > 0)
    {
        attach(envBase)
        ## Changement de l'option :
        tclvalue(P.options[[colTclVal]]) <- color#, envir=env)
        detach(envBase)

        attach(env)
        ## Changement de la couleur du widget :
        tkconfigure(get(widgetName, envir=env),
                         bg=color)#, envir=env)
        detach(env)

        tcl("update")
    }else{}


}

########################################################################################################################
addBarplotOccOptFrame.f <- function(env)
{
    ## Purpose: Ajouter une frame avec les options spécifiques
    ##          pour les barplots d'occurrence à une interface existante.
    ## ----------------------------------------------------------------------
    ## Arguments: env : environnement de l'interface existante.
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date:  8 nov. 2012, 15:51

    env2 <- environment()

    attach(env)

    on.exit(detach(env))

    ## Objets pour le choix des options :
    F.barplotOpt <- tkwidget(WinOpt, "labelframe",
                             text="Options des \"graphiques en barres\" sur les occurrences", padx=4, pady=4,
                             height=30,
                             borderwidth=2, relief="groove",
                             font=tkfont.create(weight="bold", size=10),
                             foreground="darkred",
                             background=.BGcolor)

    F.barplot1 <- tkframe(F.barplotOpt, background=.BGcolor) #, borderwidth=4, relief="groove")

    B.NbObs <- tkcheckbutton(F.barplot1, variable=P.options[["P.NbObs"]])
    F.NbObs <- tkframe(F.barplot1, background=.BGcolor)
    ##
    C.NbObsCol <- tkcanvas(F.barplot1, width="20", height="15",
                           bg=tclvalue(P.options[["P.NbObsCol"]]),
                           borderwidth=2, relief="raised")
    ##
    tkbind(C.NbObsCol ,"<ButtonPress-1>",
           function()
       {
           changeColor.f(color=tclvalue(get("P.options", envir=env)[["P.NbObsCol"]]),
                         colTclVal="P.NbObsCol",
                         widgetName="C.NbObsCol",
                         envBase=env,
                         env=env2)
           tcl("update")
       })

    B.warnings <- tkcheckbutton(F.barplot1, variable=P.options[["P.warnings"]])

    ## #### Placement des éléments sur la grille :
    tkgrid(B.NbObs,
           tklabel(F.barplot1,
                   text=" Afficher les nombres d'unités d'observation par barre ?",
                   bg=.BGcolor),
           sticky="w", padx=4, pady=1)
    tkgrid(tklabel(F.NbObs, text=" Couleur de fonte des nombres d'unités d'observations : ", bg=.BGcolor),
           C.NbObsCol)
    tkgrid(F.NbObs,  sticky="w", padx=4, pady=1, columnspan=1, column=1)

    tkgrid(B.warnings,
           tklabel(F.barplot1,
                   text=paste(" Afficher les avertissements ?\n",
                              "    (faibles effectifs)", sep=""),
                   bg=.BGcolor, justify="left"),
           sticky="w", padx=4, pady=2)

    ## Éléments généraux :

    tkgrid(F.barplot1, ## S.barplot2, F.barplot2,
           sticky="nsew")

    ## tkgrid.configure(S.barplot, rowspan=2, sticky="ns", pady=0)

    tkgrid(F.barplotOpt, columnspan=2, sticky="ew", padx=4, pady=6)

    ## On retourne l'environnement (pour accès extérieur aux objets) :
    return(env2)
}

########################################################################################################################
addBarplotOptFrame.f <- function(env)
{
    ## Purpose: Ajouter une frame avec les options spécifiques
    ##          pour les barplots à une interface existante.
    ## ----------------------------------------------------------------------
    ## Arguments: env : environnement de l'interface existante.
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date:  8 nov. 2012, 15:28

    env2 <- environment()

    attach(env)

    on.exit(detach(env))

    ## Objets pour le choix des options :
    F.barplotOpt <- tkwidget(WinOpt, "labelframe",
                             text="Options des \"graphiques en barres\"", padx=4, pady=4,
                             height=30,
                             borderwidth=2, relief="groove",
                             font=tkfont.create(weight="bold", size=10),
                             foreground="darkred",
                             background=.BGcolor)

    F.barplot1 <- tkframe(F.barplotOpt, background=.BGcolor) #, borderwidth=4, relief="groove")

    B.NbObs <- tkcheckbutton(F.barplot1, variable=P.options[["P.NbObs"]])
    F.NbObs <- tkframe(F.barplot1, background=.BGcolor)
    ##
    C.NbObsCol <- tkcanvas(F.barplot1, width="20", height="15",
                           bg=tclvalue(P.options[["P.NbObsCol"]]),
                           borderwidth=2, relief="raised")
    ##
    tkbind(C.NbObsCol ,"<ButtonPress-1>",
           function()
       {
           changeColor.f(color=tclvalue(get("P.options", envir=env)[["P.NbObsCol"]]),
                         colTclVal="P.NbObsCol",
                         widgetName="C.NbObsCol",
                         envBase=env,
                         env=env2)
           tcl("update")
       })

    B.warnings <- tkcheckbutton(F.barplot1, variable=P.options[["P.warnings"]])

    F.stat <- tkframe(F.barplot1, background=.BGcolor, pady=2)
    CB.stat <- ttkcombobox(F.stat, value=c("moyenne", "médiane"),
                           textvariable=P.options[["P.barplotStat"]],
                           state="readonly", width=8, background=.BGcolor)

    ## #### Placement des éléments sur la grille :
    tkgrid(B.NbObs,
           tklabel(F.barplot1,
                   text=" Afficher les nombres d'enregistrement par barre ?",
                   bg=.BGcolor),
           sticky="w", padx=4, pady=1)
    tkgrid(tklabel(F.NbObs, text=" Couleur de fonte des nombres d'enregistrement : ", bg=.BGcolor),
           C.NbObsCol)
    tkgrid(F.NbObs,  sticky="w", padx=4, pady=1, columnspan=1, column=1)

    tkgrid(B.warnings,
           tklabel(F.barplot1,
                   text=paste(" Afficher les avertissements ?\n",
                              "    (faibles effectifs)", sep=""),
                   bg=.BGcolor, justify="left"),
           sticky="w", padx=4, pady=2)

    tkgrid(tklabel(F.stat,
                   text="Type de statistique à représenter sur les barplots : ",
                   bg=.BGcolor, justify="left"),
           CB.stat,
           sticky="w")
    tkgrid(F.stat, sticky="w", padx=4, pady=2, columnspan=2)

    ## Éléments généraux :

    tkgrid(F.barplot1, ## S.barplot2, F.barplot2,
           sticky="nsew")

    ## tkgrid.configure(S.barplot, rowspan=2, sticky="ns", pady=0)

    tkgrid(F.barplotOpt, columnspan=2, sticky="ew", padx=4, pady=6)

    ## On retourne l'environnement (pour accès extérieur aux objets) :
    return(env2)
}

########################################################################################################################
addBoxplotOptFrame.f <- function(env)
{
    ## Purpose: Ajouter une frame avec les options spécifiques
    ##          pour les boxplots à une interface existante.
    ## ----------------------------------------------------------------------
    ## Arguments: env : environnement de l'interface existante.
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date:  7 nov. 2012, 15:59

    env2 <- environment()

    attach(env)

    on.exit(detach(env))

    ## Objets pour le choix des options :
    F.boxplotOpt <- tkwidget(WinOpt, "labelframe",
                             text="Options des graphiques en \"boîtes à moustaches\"", padx=4, pady=4,
                             height=30,
                             borderwidth=2, relief="groove",
                             font=tkfont.create(weight="bold", size=10),
                             foreground="darkred",
                             background=.BGcolor)


    F.boxplot1 <- tkframe(F.boxplotOpt, background=.BGcolor) #, borderwidth=4, relief="groove")
    F.boxplot2 <- tkframe(F.boxplotOpt, background=.BGcolor) #, borderwidth=4, relief="groove")

    S.boxplot <- ttkseparator(F.boxplotOpt, orient="vertical")
    S.boxplot2 <- ttkseparator(F.boxplotOpt, orient="vertical")

    B.maxExclu <- tkcheckbutton(F.boxplot1, variable=P.options[["P.maxExclu"]])
    FrameExclu <- tkframe(F.boxplot1, bg=.BGcolor)
    FrameExclu2 <- tkframe(FrameExclu, bg=.BGcolor)
    E.GraphPartMax <- tkentry(FrameExclu2, width="5", textvariable=P.options[["P.GraphPartMax"]])

    B.NbObs <- tkcheckbutton(F.boxplot1, variable=P.options[["P.NbObs"]])
    F.NbObs <- tkframe(F.boxplot1, background=.BGcolor)
    ##
    C.NbObsCol <- tkcanvas(F.boxplot1, width="20", height="15",
                           bg=tclvalue(P.options[["P.NbObsCol"]]),
                           borderwidth=2, relief="raised")
    ##
    tkbind(C.NbObsCol ,"<ButtonPress-1>",
           function()
       {
           changeColor.f(color=tclvalue(get("P.options", envir=env)[["P.NbObsCol"]]),
                         colTclVal="P.NbObsCol",
                         widgetName="C.NbObsCol",
                         envBase=env,
                         env=env2)
           tcl("update")
       })


    B.sepGroupes <- tkcheckbutton(F.boxplot1, variable=P.options[["P.sepGroupes"]])
    F.sepGroupes <- tkframe(F.boxplot1, background=.BGcolor)
    ##
    C.sepGroupesCol <- tkcanvas(F.boxplot1, width="20", height="15",
                               bg=tclvalue(P.options[["P.sepGroupesCol"]]),
                               borderwidth=2, relief="raised")
    ##
    tkbind(C.sepGroupesCol ,"<ButtonPress-1>",
           function()
       {
           changeColor.f(color=tclvalue(get("P.options", envir=env)[["P.sepGroupesCol"]]),
                         colTclVal="P.sepGroupesCol",
                         widgetName="C.sepGroupesCol",
                         envBase=env,
                         env=env2)
           tcl("update")
       })

    B.warnings <- tkcheckbutton(F.boxplot1, variable=P.options[["P.warnings"]])

    FrameEntry <- tkframe(F.boxplot1, bg=.BGcolor)
    E.minNbObs <- tkentry(FrameEntry, width="3", textvariable=P.options[["P.MinNbObs"]])

    FrameEntry2 <- tkframe(F.boxplot1, bg=.BGcolor)
    E.NbDecimal <- tkentry(FrameEntry2, width="3", textvariable=P.options[["P.NbDecimal"]])

    B.legendeCouleurs <- tkcheckbutton(F.boxplot2, variable=P.options[["P.legendeCouleurs"]])

    B.pointMoyenne <- tkcheckbutton(F.boxplot2, variable=P.options[["P.pointMoyenne"]])
    F.pointMoyenne <- tkframe(F.boxplot2, background=.BGcolor)
    F.PMcol <- tkframe(F.pointMoyenne, background=.BGcolor)
    ##
    C.pointMoyenneCol <- tkcanvas(F.PMcol, width="20", height="15",
                               bg=tclvalue(P.options[["P.pointMoyenneCol"]]),
                               borderwidth=2, relief="raised")
    ##
    tkbind(C.pointMoyenneCol ,"<ButtonPress-1>",
           function()
       {
           changeColor.f(color=tclvalue(get("P.options", envir=env)[["P.pointMoyenneCol"]]),
                         colTclVal="P.pointMoyenneCol",
                         widgetName="C.pointMoyenneCol",
                         envBase=env,
                         env=env2)
           tcl("update")
       })
    ##
    F.PMcex <- tkframe(F.pointMoyenne, background=.BGcolor)
    E.pointMoyenneCex <- tkentry(F.PMcex, width="3", textvariable=P.options[["P.pointMoyenneCex"]])
    ##
    F.PMpch <- tkframe(F.pointMoyenne, background=.BGcolor)
    E.pointMoyennePch <- tkentry(F.PMpch, width="3", textvariable=P.options[["P.pointMoyennePch"]])

    B.valMoyenne <- tkcheckbutton(F.boxplot2, variable=P.options[["P.valMoyenne"]])
    F.valMoyenne <- tkframe(F.boxplot2, background=.BGcolor)
    F.VMcol <- tkframe(F.valMoyenne, background=.BGcolor)
    ##
    C.valMoyenneCol <- tkcanvas(F.VMcol, width="20", height="15",
                               bg=tclvalue(P.options[["P.valMoyenneCol"]]),
                               borderwidth=2, relief="raised")
    ##
    tkbind(C.valMoyenneCol ,"<ButtonPress-1>",
           function()
       {
           changeColor.f(color=tclvalue(get("P.options", envir=env)[["P.valMoyenneCol"]]),
                         colTclVal="P.valMoyenneCol",
                         widgetName="C.valMoyenneCol",
                         envBase=env,
                         env=env2)
           tcl("update")
       })
    ##
    F.VMdec <- tkframe(F.valMoyenne, background=.BGcolor)
    E.NbDecimal <- tkentry(F.VMdec, width="3", textvariable=P.options[["P.NbDecimal"]])


    ## #### Placement des éléments sur la grille :

    tkgrid(tklabel(FrameExclu, text=paste(" Ne pas représenter les valeurs extrêmes",
                           "", sep=" "), bg=.BGcolor), sticky="w")
    tkgrid(tklabel(FrameExclu2, text="    (supérieures à ", bg=.BGcolor, justify="left"),
           E.GraphPartMax,
           tklabel(FrameExclu2, text=" x la valeur max) ? \n", bg=.BGcolor),
           sticky="nw")
    tkgrid(FrameExclu2, sticky="w")

    tkgrid(B.maxExclu, FrameExclu, sticky="nw", padx=4, pady=2)

    tkgrid(B.NbObs,
           tklabel(F.boxplot1,
                   text=" Afficher les nombres d'enregistrement par boîte à moustache ?",
                   bg=.BGcolor),
           sticky="w", padx=4, pady=1)
    tkgrid(tklabel(F.NbObs, text=" Couleur de fonte des nombres d'enregistrement : ", bg=.BGcolor),
           C.NbObsCol)
    tkgrid(F.NbObs,  sticky="w", padx=4, pady=1, columnspan=1, column=1)

    tkgrid(B.warnings,
           tklabel(F.boxplot1,
                   text=paste(" Afficher les avertissements ?\n",
                              "    (faibles effectifs + maximum exclus)", sep=""),
                   bg=.BGcolor, justify="left"),
           sticky="w", padx=4, pady=2)

    tkgrid(B.sepGroupes,
           tklabel(F.boxplot1,
                   text=" Afficher les séparateurs de groupes (facteur de premier niveau) ?",
                   bg=.BGcolor),
           sticky="w", padx=4, pady=1)
    tkgrid(tklabel(F.sepGroupes, text=" Couleur des séparateurs : ", bg=.BGcolor),
           C.sepGroupesCol)
    tkgrid(F.sepGroupes,  sticky="w", padx=4, pady=1, columnspan=1, column=1)

    tkgrid(B.legendeCouleurs,
            tklabel(F.boxplot2,
                   text=" Afficher la légende des couleurs (facteur de second niveau) ?",
                   bg=.BGcolor),
           sticky="w", padx=4, pady=2)

    tkgrid(B.pointMoyenne,
           tklabel(F.boxplot2,
                   text=" Afficher les moyennes (points) sur les boxplot ?",
                   bg=.BGcolor),
           sticky="w", padx=4, pady=2)
    tkgrid(tklabel(F.PMcol, text=" Couleur des points : ", bg=.BGcolor),
           C.pointMoyenneCol)
    tkgrid(F.PMcol, sticky="w")
    tkgrid(tklabel(F.PMcex, text=" Facteur multiplicatif des tailles de points : ", bg=.BGcolor),
           E.pointMoyenneCex)
    tkgrid(F.PMcex, sticky="w")
    tkgrid(tklabel(F.PMpch, text=" Type de point : ", bg=.BGcolor),
           E.pointMoyennePch)
    tkgrid(F.PMpch, sticky="w")
    tkgrid(F.pointMoyenne,  sticky="w", padx=4, pady=1, columnspan=1, column=1)

    tkgrid(B.valMoyenne,
           tklabel(F.boxplot2,
                   text=" Afficher les valeurs des moyennes sur les boxplot ?",
                   bg=.BGcolor),
           sticky="w", padx=4, pady=2)
    tkgrid(tklabel(F.VMcol, text=" Couleur de fonte des valeurs : ", bg=.BGcolor),
           C.valMoyenneCol, sticky="w")
    tkgrid(F.VMcol, sticky="w")
    tkgrid(tklabel(F.VMdec, text=" Nombre de chiffres décimaux : ", bg=.BGcolor),
           E.NbDecimal, sticky="w")
    tkgrid(F.VMdec, sticky="w")
    tkgrid(F.valMoyenne,  sticky="w", padx=4, pady=1, columnspan=1, column=1)

    ## Éléments généraux :

    tkgrid(F.boxplot1, S.boxplot2, F.boxplot2, sticky="nsew")

    ## tkgrid.configure(S.boxplot, rowspan=2, sticky="ns", pady=0)

    tkgrid(F.boxplotOpt, columnspan=2, sticky="ew", padx=4, pady=6)

    ## On retourne l'environnement (pour accès extérieur aux objets) :
    return(env2)
}

########################################################################################################################
tuneGraphOptions.f <- function(graphType="none")
{
    ## Purpose: Ouvre une fenêtre proposant le choix des options graphiques.
    ##          Les options sont persistantes au cours de la session mais
    ##          peuvent être réinitialisées.
    ## ----------------------------------------------------------------------
    ## Arguments: graphType : type de graphique (pour les options
    ##                        particulières en fonction du type de graphique).
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date:  5 nov. 2012, 17:07

    ## .BGcolor <- "#FFFBCF"
    .BGcolor <- "#F7F5CE"

    if (is.null(getOption("GraphPAMPA")))
    {
        initialiseGraphOptions.f()      # Initialisation des options Graphiques
    }

    env <- environment()
    Done <- tclVar(0)                   # Statut d'exécution.
    P.options <- lapply(options()[names(getOption("P.optionsClass"))],
                        function(x)
                    {
                        tclVar(paste(if (is.logical(x))
                                     {  # Pour avoir "1" au lieu de "TRUE" -> valeurs logiques pour tcl/tk :
                                         as.character(as.numeric(x))
                                     }else{
                                         as.character(x)
                                     }, collapse="*_*"))
                    })  # Sélection des options graphiques de PAMPA.

    P.options.old <- options()[names(getOption("P.optionsClass"))] # Pour pouvoir restorer les options (Cancel)

    WinOpt <- tktoplevel(background=.BGcolor)
    tkwm.title(WinOpt, "Choix des options graphiques")

    ## Ajout optionnel d'un cadre spécifique au type de graphique :
    env2 <- switch(graphType,
                   "boxplot"={
                       addBoxplotOptFrame.f(env=env)
                   },
                   "barplot"={
                       addBarplotOptFrame.f(env=env)
                   },
                   "barplotocc"={
                       addBarplotOccOptFrame.f(env=env)
                   },
                   {
                   })

    ## #### Éléments graphiques:

    ## Frame générale :
    F.generalOpt <- tkwidget(WinOpt, "labelframe",
                             text="Options graphiques générales", padx=4, pady=4,
                             height=30,
                             borderwidth=2, relief="groove",
                             font=tkfont.create(weight="bold", size=10),
                             foreground="darkred",
                             background=.BGcolor)

    ## Objets pour le choix des options :
    ## ... aspect des graphiques :
    F.general1 <- tkframe(F.generalOpt, background=.BGcolor) #, borderwidth=4, relief="groove")

    F.Entry <- tkframe(F.general1, background=.BGcolor, pady=2)
    E.minNbObs <- tkentry(F.Entry, width="3", textvariable=P.options[["P.MinNbObs"]])

    F.Entry2 <- tkframe(F.general1, background=.BGcolor, pady=2)
    E.cex <- tkentry(F.Entry2, width="3", textvariable=P.options[["P.cex"]])

    F.checkBox <- tkframe(F.general1, background=.BGcolor, pady=2)
    B.graphPaper <- tkcheckbutton(F.checkBox, variable=P.options[["P.graphPaper"]])## , bg=.BGcolor)
    B.axesLabels <- tkcheckbutton(F.checkBox, variable=P.options[["P.axesLabels"]])## , bg=.BGcolor)

    F.color <- tkframe(F.general1, background=.BGcolor, pady=2)
    CB.colPalette <- ttkcombobox(F.color, value=PAMPAcolors.f(list=TRUE),
                                 textvariable=P.options[["P.colPalette"]],
                                 state="readonly", width=6, background=.BGcolor)

    F.lang <- tkframe(F.general1, background=.BGcolor, pady=2)
    CB.lang <- ttkcombobox(F.lang, value=c("fr", "en"),
                           textvariable=P.options[["P.lang"]],
                           state="readonly", width=6, background=.BGcolor)

    S.general <- ttkseparator(F.generalOpt, orient="vertical")
    S.general2 <- ttkseparator(F.generalOpt, orient="vertical")

    ## ... sorties fichiers :
    F.general2 <- tkframe(F.generalOpt, background=.BGcolor) #, borderwidth=4, relief="groove")

    F.PDF <- tkframe(F.general2, background=.BGcolor)
    F.PDF1 <- tkframe(F.PDF, background=.BGcolor)
    B.graphPDF <- tkcheckbutton(F.PDF1, variable=P.options[["P.graphPDF"]])## , bg=.BGcolor)

    F.PDF2 <- tkframe(F.PDF, background=.BGcolor)
    B.PDFunFichierPage <- tkcheckbutton(F.PDF2, variable=P.options[["P.PDFunFichierPage"]])## , bg=.BGcolor)
    B.PDFembedFont <- tkcheckbutton(F.PDF2, variable=P.options[["P.pdfEmbedFonts"]])## , bg=.BGcolor)

    F.PNG <- tkframe(F.general2, background=.BGcolor)
    B.graphPNG <- tkcheckbutton(F.PNG, variable=P.options[["P.graphPNG"]])## , bg=.BGcolor)
    F.WMF <- tkframe(F.general2, background=.BGcolor)
    B.graphWMF <- tkcheckbutton(F.WMF, variable=P.options[["P.graphWMF"]])## , bg=.BGcolor)

    F.plusGraph <- tkframe(F.general2, background=.BGcolor)
    B.plusGraph <- tkcheckbutton(F.plusGraph, variable=P.options[["P.plusieursGraphPage"]])## ,
                                 ## bg=.BGcolor)
    CB.ncol <- ttkcombobox(F.plusGraph, value=c("1", "2", "3"),
                           textvariable=P.options[["P.ncolGraph"]],
                           state="readonly", width=1, background=.BGcolor)
    CB.nrow <- ttkcombobox(F.plusGraph, value=c("1", "2", "3"),
                           textvariable=P.options[["P.nrowGraph"]],
                           state="readonly", width=1, background=.BGcolor)

    ## Boutons :
    FrameBT <- tkframe(WinOpt, background=.BGcolor)
    B.OK <- tkbutton(FrameBT, text="  OK  ", # bg=.BGcolor,
                     command=function(){tclvalue(Done) <- 1})
    B.Cancel <- tkbutton(FrameBT, text=" Annuler ", # bg=.BGcolor,
                         command=function(){tclvalue(Done) <- 2})
    B.Reinit <- tkbutton(FrameBT, text=" Réinitialiser ", # bg=.BGcolor,
                         command=function()
                     {
                         initialiseGraphOptions.f()
                         for (i in names(P.options))
                         {
                             eval(tclvalue(P.options[[i]]) <- options()[[i]], envir=env)
                         }

                         invisible(sapply(c("C.NbObsCol", "C.sepGroupesCol",
                                            "C.pointMoyenneCol", "C.valMoyenneCol"),
                                          function(x, env)
                                      {
                                          tryCatch(tkconfigure(get(x, envir=env),
                                                               bg=getOption(sub("C\\.", "P.", x))),
                                                   error=function(e){})
                                      }, env=env2))

                         tcl("update")
                     })

    ## #### Placement des éléments sur la grille :
    ## ... aspect des graphiques :
    tkgrid(tklabel(F.color, text="Palette de couleurs : ", bg=.BGcolor),
           CB.colPalette)
    tkgrid(F.color, sticky="w", padx=4)

    tkgrid(tklabel(F.lang, text="Langue des noms d'axes et variables : ", bg=.BGcolor),
           CB.lang)
    tkgrid(F.lang, sticky="w", padx=4)

    tkgrid(B.graphPaper,
           tklabel(F.checkBox,
                   text=paste(" Graphique pour publication ?\n",
                              "    (plus petit, sans titre)", sep=""),
                   bg=.BGcolor, justify="left"),
           sticky="w", pady=2)
    tkgrid(B.axesLabels,
           tklabel(F.checkBox, text=" Afficher le nom des axes ?", bg=.BGcolor, justify="left"),
           sticky="w", pady=2)
    tkgrid(F.checkBox, sticky="w", padx=4)

    tkgrid(tklabel(F.Entry2, text="Facteur multiplicatif des tailles de fontes : ", bg=.BGcolor),
           E.cex,
           sticky="w")
    tkgrid(F.Entry2, sticky="w", padx=4)

    tkgrid(tklabel(F.Entry, text="Supprimer les graphiques ayant moins de ", bg=.BGcolor),
           E.minNbObs,
           tklabel(F.Entry, text=" observations", bg=.BGcolor),
           sticky="w")
    tkgrid(F.Entry, sticky="w", padx=4)

    ## ... sorties fichiers :
    tkgrid(B.graphPDF,
           L.PDF <- tklabel(F.PDF1, text=" Fichiers PDF ?", bg=.BGcolor, justify="left"),
           sticky="w")
    tkgrid(tklabel(F.PDF2, text="   ", bg=.BGcolor),
           B.PDFunFichierPage,
           tklabel(F.PDF2, text=" Créer un fichier par page (PDF) ?", bg=.BGcolor),
           sticky="w")
    tkgrid(tklabel(F.PDF2, text="   ", bg=.BGcolor),
           B.PDFembedFont,
           tklabel(F.PDF2, text=" Inclure les fontes (PDF) ?", bg=.BGcolor),
           sticky="w")
    tkgrid(F.PDF1, sticky="w", columnspan=2)
    tkgrid(F.PDF2, sticky="w", columnspan=2)
    tkgrid(F.PDF, sticky="w", padx=4, pady=2)

    tkgrid(B.graphPNG,
           tklabel(F.PNG, text=" Fichiers PNG ?", bg=.BGcolor),
           sticky="w")
    tkgrid(F.PNG, sticky="w", padx=4, pady=2)

    tkgrid(B.graphWMF,
           tklabel(F.WMF,
                   text=paste(" Fichiers WMF ?\n",
                              "    (en plus de l'affichage à l'écran ; Windows uniquement)", sep=""),
                   bg=.BGcolor, justify="left"),
           sticky="w")
    tkgrid(F.WMF, sticky="w", padx=4, pady=2)

    tkgrid(B.plusGraph,
           tklabel(F.general2, text=" Plusieurs graphiques par page (", bg=.BGcolor),
           CB.nrow,
           tklabel(F.general2, text=" lignes x ", bg=.BGcolor),
           CB.ncol,
           tklabel(F.general2, text=" colonnes)", bg=.BGcolor), sticky="w")
    tkgrid(F.plusGraph, sticky="ew", padx=4, pady=2)

    ## Options générales:
    tkgrid(tklabel(F.generalOpt, text="Aspect des graphiques :",
                   font=tkfont.create(weight="normal", size=10),
                   foreground="darkred", background=.BGcolor,
                   justify="left"),
           S.general,
           tklabel(F.generalOpt, text="Sorties dans des fichiers :",
                   font=tkfont.create(weight="normal", size=10),
                   foreground="darkred", background=.BGcolor,
                   justify="left"),
           sticky="w", padx=2, pady=6)

    tkgrid(F.general1, S.general2, F.general2, sticky="nsew")

    tkgrid.configure(S.general, rowspan=2, sticky="ns", pady=0)
    tkgrid.configure(S.general2, sticky="ns", pady=0)

    tkgrid(F.generalOpt, columnspan=2, sticky="ew", padx=4, pady=6)

    ## Boutons :
    tkgrid(FrameBT, columnspan=2)
    tkgrid(B.OK,
           tklabel(FrameBT, text="     ", bg=.BGcolor),
           B.Cancel,
           tklabel(FrameBT, text="               ", bg=.BGcolor),
           B.Reinit,
           padx=4, pady=10, sticky="ew")

    ## #### Gestion des évènements :
    tkconfigure(B.PDFunFichierPage , state=ifelse(tclvalue(P.options[["P.graphPDF"]]) == "0", "disabled", "normal"))
    tkconfigure(B.PDFembedFont , state=ifelse(tclvalue(P.options[["P.graphPDF"]]) == "0", "disabled", "normal"))
    tkconfigure(B.graphWMF ,
                            state=ifelse(tclvalue(P.options[["P.graphPDF"]]) == "1" ||
                                            tclvalue(P.options[["P.graphPNG"]]) == "1",
                                         "disabled", "normal"))

    ##  Activer le choix de sortie dans un fichier par page en mode PDF uniquement.
    tkconfigure(B.graphPDF, command=function()
            {
                tkconfigure(B.PDFunFichierPage ,
                            state=ifelse(tclvalue(P.options[["P.graphPDF"]]) == "0", "disabled", "normal"))
                tkconfigure(B.PDFembedFont ,
                            state=ifelse(tclvalue(P.options[["P.graphPDF"]]) == "0", "disabled", "normal"))
                tkconfigure(B.graphWMF ,
                            state=ifelse(tclvalue(P.options[["P.graphPDF"]]) == "1" ||
                                            tclvalue(P.options[["P.graphPNG"]]) == "1",
                                         "disabled", "normal"))

                ## Décocher les graphiques png :
                if (tclvalue(P.options[["P.graphPDF"]]) == "1")
                {
                    tclvalue(P.options[["P.graphPNG"]]) <- "0"
                }else{}
            })

    ## Décocher les graphiques pdf lorsque png activé :
    tkconfigure(B.graphPNG, command=function()
            {

                if (tclvalue(P.options[["P.graphPNG"]]) == "1")
                {
                    tclvalue(P.options[["P.graphPDF"]]) <- "0"
                }else{}

                ## MàJ de l'état du bouton document uniq en pdf :
                tkconfigure(B.PDFunFichierPage ,
                            state=ifelse(tclvalue(P.options[["P.graphPDF"]]) == "0", "disabled", "normal"))
                tkconfigure(B.PDFembedFont ,
                            state=ifelse(tclvalue(P.options[["P.graphPDF"]]) == "0", "disabled", "normal"))
                tkconfigure(B.graphWMF ,
                            state=ifelse(tclvalue(P.options[["P.graphPDF"]]) == "1" ||
                                            tclvalue(P.options[["P.graphPNG"]]) == "1",
                                         "disabled", "normal"))
            })


    tkbind(WinOpt, "<Destroy>", function(){tclvalue(Done) <- 2}) # En cas de destruction de la fenêtre.

    tkfocus(WinOpt)

    tcl("update")

    winSmartPlace.f(WinOpt, xoffset=-5, yoffset=-10)

    tkwait.variable(Done)

    if (tclvalue(Done) == "1")
    {
        ## Sauvegarde des options :
        options(sapply(names(P.options),
                       function (name)
                   {
                       ## Conversion dans la bonne classe (renseignée par l'option P.optionClass) :
                       switch(options("P.optionsClass")[[1]][name],
                              logical=as.logical(as.integer(unlist(strsplit(tclvalue(P.options[[name]]),
                                                                            split="*_*", fixed=TRUE)))),
                              character=as.character(unlist(strsplit(tclvalue(P.options[[name]]),
                                                                     split="*_*", fixed=TRUE))),
                              integer=as.integer(unlist(strsplit(tclvalue(P.options[[name]]),
                                                                 split="*_*", fixed=TRUE))),
                              numeric=as.numeric(unlist(strsplit(tclvalue(P.options[[name]]),
                                                                 split="*_*", fixed=TRUE))),
                              stop("Erreur : Option PAMPA '", name, "' non définie"))
                   }, simplify=FALSE))
     }

    ## Initialisation de la langue des variables de graphiques :
    init.GraphLang.f()

    tkdestroy(WinOpt)                   # Destruction de la fenêtre
}

########################################################################################################################
generalOptions.f <- function()
{
    ## Purpose: choisir des options générales (graphiques et analyses)
    ## ----------------------------------------------------------------------
    ## Arguments: aucun
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 11 sept. 2012, 17:33

    env <- environment()
    Done <- tclVar(0)                   # Statut d'exécution.

    .Background <- "#FFF6BF"            # Couleur pour l'arrière plan des infos.

    P.options <- lapply(options()[names(options("P.optionsClass")[[1]])],
                        function(x)
                    {
                        tclVar(paste(if (is.logical(x))
                                     {  # Pour avoir "1" au lieu de "TRUE" -> valeurs logiques pour tcl/tk :
                                         as.character(as.numeric(x))
                                     }else{
                                         as.character(x)
                                     }, collapse="*_*"))
                    })  # Sélection des options graphiques de PAMPA.

    P.options.old <- options()[names(options("P.optionsClass")[[1]])] # Pour pouvoir restorer les options (Cancel)

    WinOpt <- tktoplevel()
    tkwm.title(WinOpt, "Choix des options d'export (analyses/graphiques)")

    ## Objets pour le choix des options :

    B.saveData <- tkcheckbutton(WinOpt, variable=P.options[["P.saveData"]])
    B.saveStats <- tkcheckbutton(WinOpt, variable=P.options[["P.saveStats"]])

    ## Boutons :
    FrameBT <- tkframe(WinOpt)
    B.OK <- tkbutton(FrameBT, text="  OK  ",
                     command=function(){tclvalue(Done) <- 1})
    B.Cancel <- tkbutton(FrameBT, text=" Annuler ",
                         command=function(){tclvalue(Done) <- 2})
    B.Reinit <- tkbutton(FrameBT, text=" Réinitialiser ",
                         command=function()
                     {
                         initialiseGraphOptions.f()
                         for (i in names(P.options))
                         {
                             eval(tclvalue(P.options[[i]]) <- options()[[i]], envir=env)
                         }
                     })

    ## Placement des éléments sur la grille :

    tkgrid(tklabel(WinOpt))
    tkgrid(tklabel(WinOpt,
                   text="Sauvgardes pour chaque analyse ou graphique :",
                   background=.Background, justify="left"),
           sticky="ew", padx=5, pady=3, columnspan=2)

    tkgrid(tklabel(WinOpt,
                   text="...données du graphique ou de l'analyse\n(fichier *.csv) ? ",
                   justify="right"),
           B.saveData, sticky="es", padx=3, pady=5)

    tkgrid(tklabel(WinOpt,
                   text="...informations sur les données et statistiques\n(fichier *.stat) ? ",
                   justify="right"),
           B.saveStats, sticky="es", padx=3, pady=5)


    tkgrid(tklabel(WinOpt))
    tkgrid(FrameBT, columnspan=2, padx=6, pady=10)
    tkgrid(B.OK, tklabel(FrameBT, text="     "), B.Cancel,
           tklabel(FrameBT, text="               "), B.Reinit,
           tklabel(FrameBT, text="\n"))

    ##
    tkbind(WinOpt, "<Destroy>", function(){tclvalue(Done) <- 2}) # En cas de destruction de la fenêtre.

    tcl("update")
    winSmartPlace.f(WinOpt)

    tkwait.variable(Done)

    if (tclvalue(Done) == "1")
    {
        ## Sauvegarde des options :
        options(sapply(names(P.options),
                       function (name)
                   {
                       ## Conversion dans la bonne classe (renseignée par l'option P.optionClass) :
                       switch(options("P.optionsClass")[[1]][name],
                              logical=as.logical(as.integer(unlist(strsplit(tclvalue(P.options[[name]]),
                                                                            split="*_*", fixed=TRUE)))),
                              character=as.character(unlist(strsplit(tclvalue(P.options[[name]]),
                                                                     split="*_*", fixed=TRUE))),
                              integer=as.integer(unlist(strsplit(tclvalue(P.options[[name]]),
                                                                 split="*_*", fixed=TRUE))),
                              numeric=as.numeric(unlist(strsplit(tclvalue(P.options[[name]]),
                                                                 split="*_*", fixed=TRUE))),
                              stop("Erreur : Option PAMPA '", name, "' non définie"))
                   }, simplify=FALSE))
     }

    tkdestroy(WinOpt)                   # Destruction de la fenêtre
}


### Local Variables:
### ispell-local-dictionary: "english"
### fill-column: 120
### End:
