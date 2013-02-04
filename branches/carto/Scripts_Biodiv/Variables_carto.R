#-*- coding: latin-1 -*-
# Time-stamp: <2013-01-10 16:57:49 yves>

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

### File: Variables_carto.R
### Created: <2012-12-06 16:52:40 yves>
###
### Author: Yves Reecht
###
####################################################################################################
### Description:
###
### Interface de sélection des variables et fonctions pour les représentations cartographiques.
####################################################################################################

########################################################################################################################
bbox.subset.f <- function(refspa, facteur, selection,
                          tclVars=NULL)
{
    ## Purpose: Calculer les limites d'une zone d'après un subset du
    ##          référentiel spatial sur un facteur spatial.
    ## ----------------------------------------------------------------------
    ## Arguments: refspa : le référentiel spatial.
    ##            facteur : le nom du facteur.
    ##            selection : vecteur des modalités sélectionnées.
    ##            tclVars : liste nommée ("N", "E", "S", "W") de variables
    ##                      tcl contenant les limites cardinales
    ##                      (optionelle).
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 12 déc. 2012, 10:14

    if (isPoly <- is.element("SpatialPolygonsDataFrame", class(refspa)))
    {
        if (is.na(selection) || is.na(facteur))
        {
            res <- bbox(refspa)
        }else{
            res <- bbox(refspa[is.element(refspa@data[, facteur], selection) , ])
        }
    }else{
        ## Rien pour l'instant.
    }

    if ( ! is.null(tclVars))
    {
        ## Identifications des limites NESW et
        ## Configuration des variables tcl :
        tryCatch(tclvalue(tclVars[["N"]]) <- round(res["y", "max"], 3),
                 error=function(e){})

        tryCatch(tclvalue(tclVars[["S"]]) <- round(res["y", "min"], 3),
                 error=function(e){})

        tryCatch(tclvalue(tclVars[["E"]]) <- round(res["x", "max"], 3),
                 error=function(e){})

        tryCatch(tclvalue(tclVars[["W"]]) <- round(res["x", "min"], 3),
                 error=function(e){})
    }else{}

    return(res)
}

########################################################################################################################
champsRefspa.f <- function(dataEnv, first=c("ZONE.SURVE", "GROUPE", "AMP"))
{
    ## Purpose: récupérer la liste des champs du référentiel spatial
    ##          (qui sont également disponible dans les unitobs).
    ## ----------------------------------------------------------------------
    ## Arguments: dataEnv : l'environnement des données.
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 10 déc. 2012, 17:27

    refspa <- get("refspa", envir=dataEnv)

    if (isPoly <- is.element("SpatialPolygonsDataFrame", class(refspa)))
    {
        ## Récupération de la table des données seule :
        refspa <- refspa@data
    }else{}

    ## Noms des colonnes d'unitobs :
    unitobsColnames <- colnames(get("unitobs", envir=dataEnv))

    ## Noms des colonnes du référentiel spatial, susceptibles d'être utilisées comme facteurs spatiaux :
    refspaColnames <- sort(colnames(refspa)[## présentes dans unitobs :
                                            is.element(colnames(refspa), unitobsColnames) &
                                            ## ...à l'exclusion de :
                                            ! is.element(colnames(refspa),
                                                         c("OBJECTID", "SITE.SURFACE", "SITE.centrX", "SITE.centrY")) &
                                            ## ...et étant soit des facteurs soit des entiers :
                                            is.element(sapply(refspa, class), c("factor", "integer"))])

    ## Liste ordonnée :
    return(c("",
             first[is.element(first, refspaColnames)],
             "",
             refspaColnames[ ! is.element(refspaColnames, first)]))
}


########################################################################################################################
selectModalitesZoneSpatiales.f <- function(env, dataEnv, refspaName="refspa",
                                           selName="factSpatialSel2", factName="FacteurSpatial2")
{
    ## Purpose: sélection des modalités de facteurs du référentiel spatial
    ##          brute.
    ## ----------------------------------------------------------------------
    ## Arguments:
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 13 déc. 2012, 07:48

    refspa <- get(refspaName, envir=dataEnv)

    if (isPoly <- is.element("SpatialPolygonsDataFrame", class(refspa)))
    {
        refspa <- refspa@data
    }else{
        ## Rien à faire.
    }

    selections <- get(selName, envir=env)
    facteur <- tclvalue(get(factName, envir=env))

    preselect <- NULL                   # sélections persistantes d'une fois sur l'autre
    if ( ! is.na(selections[1]))
    {
        preselect <- selections
    }

    ## Sélection des modalités
    sel <- selectModWindow.f(champ=facteur, data=refspa, selectmode="extended", preselect=preselect)

    if (!is.null(sel) & length(sel) > 0)
    {
        ## Expression à évaluer (stockage des modalités sélectionnées) :
        exprModSel <- paste(selName, " <- c(\"", # Cas du facteur de séparation des graphiques
                            paste(sel, collapse="\", \""),
                            "\")", sep="")

        eval(parse(text=exprModSel), envir=env)
    }
}

########################################################################################################################
selectModalitesSpatiales.f <- function(factor, tableMetrique, env, nextStep, dataEnv,
                                       selName="factSpatialSel", factName="FacteurSpatial")
{
    ## Purpose: Sélection des modalités d'un facteur.
    ## ----------------------------------------------------------------------
    ## Arguments: factor : le nom du facteur sélectionné.
    ##            tableMetrique : nom de la table des métriques.
    ##            nextStep : étape suivante.                     [!!!] on devrait pouvoir s'en passer  [yr: 18/1/2012]
    ##            level : l'ordre du facteur (0 pour celui de séparation des
    ##                    graphiques, 1, 2,... pour les suivants).
    ##            env : environnement de la fonction appelante.
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 10 déc. 2012, 16:47

    facts <- c(tclvalue(get(factName, envir=env)),
               tclvalue(get("FacteurGraph", envir=env)),
               sapply(get("listFacteurs", envir=env), tclvalue))

    selections <- c(list(get(selName, envir=env)), # Liste des modalités déjà sélectionnées
                    list(get("factGraphSel", envir=env)),
                    get("listFactSel", envir=env))

    level <- 0                         # [!!!] à vérifier.

    preselect <- NULL                   # sélections persistantes d'une fois sur l'autre
    if (!is.na(selections[level + 1]))
    {
        preselect <- selections[[level + 1]]
    }

    ## Table réduite :
    metrique <- tclvalue(get("MetriqueChoisie" , envir=env))

    ## Pour les indices de biodiversité recalculés, il faut utiliser "unitSp" et une métrique adaptée.
    if (is.element(nextStep, get("nextStepBiodiv", envir=env)) &&
        tableMetrique == "unit")
    {
        tableMetrique <- "unitSp"
        metrique <- "nombre"
    }else{}

    tmp <- subsetToutesTables.f(metrique=metrique, facteurs=facts, selections=selections,
                                dataEnv=dataEnv, tableMetrique=tableMetrique , exclude=level + 1)

    ## Sélection des modalités
    sel <- selectModWindow.f(champ=factor, data=tmp, selectmode="extended", preselect=preselect)

    if (!is.null(sel) & length(sel) > 0)
    {
        ## Expression à évaluer (stockage des modalités sélectionnées) :
        exprModSel <- paste(selName, " <- c(\"", # Cas du facteur de séparation des graphiques
                            paste(sel, collapse="\", \""),
                            "\")", sep="")

        eval(parse(text=exprModSel), envir=env)
    }
}

########################################################################################################################
selectionZone.f <- function(envir)
{
    ## Purpose: affiche une frame de sélection des zones
    ## ----------------------------------------------------------------------
    ## Arguments: envir : environnement de l'interface de choix de variables
    ##                    carto.
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 12 déc. 2012, 14:53


    evalq(
    {
        if ( ! exists("Fselect"))
        {
            FacteurSpatial2 <- tclVar("")                # Facteur d'agrégation spatiale.
            factSpatialSel2 <- NA                        # Modalités sélectionnées.

            Fselect <- tkframe(FrameInfoSelect,
                               background=.BGcolor)

            L.spatialSelect2 <- tklabel(Fselect,
                                        text=paste("Choix des limites d'après une sélection de zones",
                                                   " sur le référentiel spatial brut (facteur spatial au choix).",
                                                   sep=""),
                                        font=tkfont.create(weight="normal", size=10),
                                        justify="left",
                                        foreground="darkred",
                                        background=.BGcolor,
                                        wraplength=0.95 * as.integer(tkwinfo("width", Fvariables)))

            CB.spatial2 <- ttkcombobox(Fselect, value=factSpat, textvariable=FacteurSpatial2,
                                       state="readonly")

            B.factSpatialSel2 <- tkbutton(Fselect, text=" Sélection... ",
                                          command=function()
                                      {
                                          selectModalitesZoneSpatiales.f(env=env, dataEnv=dataEnv,
                                                                         selName="factSpatialSel2",
                                                                         factName="FacteurSpatial2")
                                          winRaise.f(WinSelection)
                                      })

            B.Valid <- tkbutton(Fselect, text=" Valider ",
                                command=function()
                            {
                                bbox.subset.f(refspa=get("refspa", envir=dataEnv),
                                              facteur=tclvalue(FacteurSpatial2),
                                              selection=factSpatialSel2,
                                              tclVars=tclVarsLim)
                                tcl("update")

                                tkgrid.remove(Fselect)
                            })
            B.cancel2 <- tkbutton(Fselect, text=" Annuler ",
                                  command=function()
                              {
                                  tkgrid.remove(Fselect)
                              })
        }else{}

        if (tclvalue(tkgrid.info(Fselect)) == "")
        {
            tkgrid(L.spatialSelect2, columnspan=2, sticky="w")
            tkgrid(CB.spatial2, B.factSpatialSel2, padx=5, pady=2, sticky="w")
            tkgrid(B.Valid, B.cancel2, padx=10, pady=10)

            tkgrid(Fselect, sticky="ew")
            tcl("update")
        }else{}
    },
        envir=envir)
}


########################################################################################################################
selectionVariables.carto.f <- function(nextStep, dataEnv, baseEnv)
{
    ## Purpose: * Sélection des métrique et facteur(s) ainsi que leur(s)
    ##            modalité(s).
    ##          * Lancement de l'étape suivante (graphique, analyse
    ##            statistique,...).
    ## ----------------------------------------------------------------------
    ## Arguments: nextStep : étape suivante (chaîne de caractères parmi
    ##                       "",...
    ##                       [appelé à s'étoffer]).
    ##            dataEnv : l'environnement des données.
    ## Note : les arguments de cette fonction peuvent changer à l'avenir
    ##        (ajouts)
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date:  6 déc. 2012, 16:53

    .BGcolor <- "#F7F5CE"

    ## Variables :
    env <- environment()                    # Environnement courant
    Done <- tclVar(0)                       # Statut d'exécution

    ## ##################################################
    ## Groupes de traitements :

    ## Étapes avec agrégation par unitobs :
    nextStepUnitobs <- c("spBarBoxplot.unitobs", ## "freq_occurrence",
                         "spSymbols.unitobs")

    ## Étapes sans possibilité d'agrégation par classes de taille :
    nextStepSansCT <- c("")

    ## Étapes avec biodiversité (généralement pour les métriques agrégées par unité d'observation) :
    nextStepBiodiv <- c("spBarBoxplot.unitobs", "spSymbols.unitobs")

    ## Étapes sans facteurs de regroupement :
    nextStepNoGrpFact <- c("spSymbols.unitobs", "spSymbols.esp")

    ## Le même traitement des variables peut être appliqué pour différents "nextStep"
    ## (Permet de lancer une fonction qui ne correspond pas à nextStep ; cas non rencontré pour l'instant, i.e.
    ## casStep[nextStep] == nextStep) :
    casStep <- c("spBarBoxplot.unitobs"="spBarBoxplot.unitobs",
                 "spBarBoxplot.esp"="spBarBoxplot.esp",
                 "spSymbols.unitobs"="spSymbols.unitobs",
                 "spSymbols.esp"="spSymbols.esp")

    ## Facteurs spatiaux :
    factSpat <- champsRefspa.f(dataEnv=dataEnv) # champs du référentiel spatial.
    FacteurSpatial <- tclVar("")                # Facteur d'agrégation spatiale.
    factSpatialSel <- NA                        # Modalités sélectionnées.

    ## Liste des métriques :
    metriques <- champsMetriques.f(nomTable="unitSp", nextStep=nextStep, dataEnv=dataEnv)

    TableMetrique <- tclVar("unitSp")   # Table des métriques.
    MetriqueChoisie <- tclVar("")       # Métrique choisie.

    FacteurGraph <- tclVar("")          # Facteur de séparation des graphiques/sélection d'espèces.
    FacteurGraph.old <- ""              # Stockage de l'encien facteur (pour réinitialiser les modalités
                                        # sélectionnées si besoin).
    FactGraphTbl <- tclVar("refesp")    # Table (ref espèce ou unitobs) à laquel il appartient
    factGraphSel <- NA                  # Modalités sélectionnées

    listFacteurs <- list(tclVar(""))    # Liste des facteurs de regroupement
    listFacteurs.old <- list("")        # Stockage des enciens facteurs (pour réinitialiser les modalités
                                        # sélectionnées si besoin).
    listFactSel <- list(NA)             # Liste des modalités sélectionnées

    LimiteN <- tclVar("")
    LimiteS <- tclVar("")
    LimiteE <- tclVar("")
    LimiteW <- tclVar("")

    TypeGraph <- tclVar("barplot")

    ## ########################
    ## Éléments graphiques :
    WinSelection <- tktoplevel(background="white")          # Fenêtre principale
    tkwm.title(WinSelection,
               paste("Variables pour les cartes ",
                     titreSelVar.f(type="winTitle", nextStep),
                     sep=""))

    ## Onglets :
    NB <- ttknotebook(WinSelection)

    TABvariables <- ttkframe(NB)
    TABzone <- ttkframe(NB)

    tkadd(NB, TABvariables, text="Sélection de métrique")   ### tabid=0
    tkadd(NB, TABzone, text="Emprise spatiale")  ### tabid=1

    Fvariables <- tkframe(TABvariables, background=.BGcolor)
    Fzone <- tkframe(TABzone, background=.BGcolor)

    ## Facteur de regroupement spatial :
    FrameSpatial <- tkwidget(Fvariables, "labelframe",
                             text=titreSelVar.f(type="factSpatial", nextStep),
                             padx=4, pady=4,
                             height=30,
                             borderwidth=2, relief="groove",
                             font=tkfont.create(weight="normal", size=10),
                             foreground="darkred",
                             background=.BGcolor)

    CB.spatial <- ttkcombobox(FrameSpatial, value=factSpat, textvariable=FacteurSpatial,
                               state="readonly")

    B.factSpatialSel <- tkbutton(FrameSpatial, text=" Sélection... ",
                               command=function()
                           {
                               selectModalitesSpatiales.f(factor=tclvalue(FacteurSpatial),
                                                          tableMetrique=tclvalue(TableMetrique),
                                                          env=env, nextStep=nextStep, dataEnv=dataEnv)
                               winRaise.f(WinSelection)
                           })

    ## Métriques :
    FrameMetrique <- tkwidget(Fvariables, "labelframe",
                              text=titreSelVar.f(type="metrique", nextStep),
                              padx=4, pady=4,
                              height=30,
                              borderwidth=2, relief="groove",
                              font=tkfont.create(weight="normal", size=10),
                              foreground="darkred",
                              background=.BGcolor)

    CB.metrique <- ttkcombobox(FrameMetrique, value=metriques, textvariable=MetriqueChoisie,
                               state="readonly")
    RB.unitSpSz <- tkradiobutton(FrameMetrique, variable=TableMetrique, background=.BGcolor,
                                 value="unitSpSz", text=titreSelVar.f(type="tabListespCT", nextStep),
                                 borderwidth=0)
    RB.unitSp <- tkradiobutton(FrameMetrique, variable=TableMetrique, background=.BGcolor,
                               value="unitSp", text=titreSelVar.f(type="tabListesp", nextStep))
    RB.unit <- tkradiobutton(FrameMetrique, variable=TableMetrique, background=.BGcolor,
                             value="unit", text="...de biodiversité ( / unité d'observation)")

    ## Choix du facteur de séparation des graphs + modalités :
    FrameFactGraph <- tkwidget(Fvariables, "labelframe",
                               text=titreSelVar.f(type="factSep", nextStep),
                               padx=4, pady=4,
                               height=30,
                               borderwidth=2, relief="groove",
                               font=tkfont.create(weight="normal", size=10),
                               foreground="darkred",
                               background=.BGcolor)
    FrameRB <- tkframe(FrameFactGraph, background=.BGcolor)
    FrameGB <- tkframe(FrameFactGraph, background=.BGcolor)

    RB.factGraphRefesp <- tkradiobutton(FrameRB, variable=FactGraphTbl, value="refesp", text="du référentiel espèces",
                                        background=.BGcolor)
    RB.factGraphUnitobs <- tkradiobutton(FrameRB, variable=FactGraphTbl, value="unitobs", text="d'unité d'observation",
                                        background=.BGcolor)
    CB.factGraph <- ttkcombobox(FrameGB, value="", textvariable=FacteurGraph, state="readonly")
    B.factGraphSel <- tkbutton(FrameGB, text=" Sélection... ", command=function()
                           {
                               selectModalites.f(factor=tclvalue(FacteurGraph), tableMetrique=tclvalue(TableMetrique),
                                                 env=env, level=0, nextStep=nextStep, dataEnv=dataEnv)
                               winRaise.f(WinSelection)
                           })

    ## Choix des facteurs de regroupement + modalités :
    FrameFact <- tkwidget(Fvariables, "labelframe",
                          text=titreSelVar.f(type="facteurs", nextStep),
                          padx=4, pady=4,
                          height=30,
                          borderwidth=2, relief="groove",
                          font=tkfont.create(weight="normal", size=10),
                          foreground="darkred",
                          background=.BGcolor)
    CB.fact1 <- ttkcombobox(FrameFact,
                            value=champsReferentiels.f(nomTable=tclvalue(TableMetrique), dataEnv=dataEnv,
                                                       nextStep=nextStep),
                            textvariable=listFacteurs[[1]], state="readonly")

    B.factSel1 <- tkbutton(FrameFact, text=" Sélection... ", command=function()
                       {
                           selectModalites.f(tclvalue(listFacteurs[[1]]), tableMetrique=tclvalue(TableMetrique),
                                             env=env, level=1, nextStep=nextStep, dataEnv=dataEnv)
                           winRaise.f(WinSelection)
                       })

    FrameBT <- tkframe(WinSelection, background="white") ##, background=.BGcolor)
    B.OK <- tkbutton(FrameBT, text="  Lancer  ", command=function(){tclvalue(Done) <- 1})
    B.Cancel <- tkbutton(FrameBT, text=" Quitter ", command=function(){tclvalue(Done) <- 2})
    B.optGraph <- tkbutton(FrameBT, text=" Options graphiques... ",
                           command=function(x)
                       {
                           tuneGraphOptions.f(graphType=switch(nextStep,
                                              "boxplot.esp"=, "boxplot.unitobs"={"boxplot"},
                                              "barplot.unitobs"=, "barplot.esp"={"barplot"},
                                              "freq_occurrence.unitobs"=, "freq_occurrence"={"barplotocc"},
                                              {"none"}))
                           winRaise.f(WinSelection)
                       })

    ## Type de graphique :
    FrameTG <- tkframe(FrameBT, background="white")
    CB.TG <- ttkcombobox(FrameTG,
                         value=c("boxplot", "barplot"),
                         textvariable=TypeGraph,
                         width=18,
                         state="readonly")

    ## ####
    ## Emprise spatiale :

    ## Initialisation des limites :
    tclVarsLim <- list(N=LimiteN, E=LimiteE, S=LimiteS, W=LimiteW)

    bbox.subset.f(refspa=get("refspa", envir=dataEnv),
                  facteur=NA,
                  selection=NA,
                  tclVars=tclVarsLim)

    FrameCoords2 <- tkframe(Fzone, background=.BGcolor)
    FrameCoords <- tkwidget(FrameCoords2, "labelframe",
                            text="Limites de la zone à représenter",
                            padx=4, pady=4,
                            ## height=30,
                            borderwidth=2, relief="groove",
                            font=tkfont.create(weight="normal", size=10),
                            foreground="darkred",
                            background=.BGcolor)

    FrameN <- tkframe(FrameCoords, background=.BGcolor)
    E.N <- tkentry(FrameN, width="9", textvariable=LimiteN, justify="right")
    tooltipWidget.f(text=paste("° décimaux Nord (négatif pour l'hémisphère Sud).",
                    "\nAttention : séparateur décimal : \".\" ", sep=""),
                    targetWidget=E.N, yskip=0)

    FrameS <- tkframe(FrameCoords, background=.BGcolor)
    E.S <- tkentry(FrameS, width="9", textvariable=LimiteS, justify="right")
    tooltipWidget.f(text=paste("° décimaux Nord (négatif pour l'hémisphère Sud).",
                    "\nAttention : séparateur décimal : \".\" ", sep=""),
                    targetWidget=E.S, yskip=0)

    FrameE <- tkframe(FrameCoords, background=.BGcolor)
    E.E <- tkentry(FrameE, width="9", textvariable=LimiteE, justify="right")
    tooltipWidget.f(text=paste("° décimaux Est (négatif à l'Ouest du méridien de Greenwich).",
                    "\nAttention : séparateur décimal : \".\" ", sep=""),
                    targetWidget=E.E, yskip=0)

    FrameW <- tkframe(FrameCoords, background=.BGcolor)
    E.W <- tkentry(FrameW, width="9", textvariable=LimiteW, justify="right")
    tooltipWidget.f(text=paste("° décimaux Est (négatif à l'Ouest du méridien de Greenwich).",
                    "\nAttention : séparateur décimal : \".\" ", sep=""),
                    targetWidget=E.W, yskip=0)

    ## Boutons de modifications des limites :

    FrameInfoSelect <- tkframe(Fzone, borderwidth=2, relief="groove",
                            background=.BGcolor)

    B.spatialSelect <- tkbutton(FrameCoords2, text="Sélection de zones...", width=22,
                                command=function()
                            {
                                tkgrid.remove(L.spatialSelect)

                                selectionZone.f(envir=env)

                                tkwm.geometry(WinSelection,"")
                            })
    L.spatialSelect <- tklabel(FrameInfoSelect,
                               text=paste("Choix des limites d'après une sélection de zones",
                                          " sur le référentiel spatial brut (facteur spatial au choix).", sep=""),
                               font=tkfont.create(weight="normal", size=10),
                               justify="left",
                               foreground="darkred",
                               background=.BGcolor)

    B.spatialFact <- tkbutton(FrameCoords2, text="Facteur de regroupement", width=22,
                              command=function()
                          {
                              bbox.subset.f(refspa=get("refspa", envir=dataEnv),
                                            facteur=tclvalue(FacteurSpatial),
                                            selection=if (is.na(factSpatialSel[1]))
                                        {
                                            if (class(unitobs <- get("unitobs", envir=dataEnv)) == "factor")
                                            {
                                                levels(unitobs[ , tclvalue(FacteurSpatial)])
                                            }else{
                                                unique(unitobs[ , tclvalue(FacteurSpatial)])
                                            }
                                        }else{
                                            factSpatialSel
                                        },
                                            tclVars=tclVarsLim)
                              tcl("update")
                          })
    L.spatialFact <- tklabel(FrameInfoSelect,
                             text=paste("Définir les limites d'après la sélection sur le",
                                        " facteur de regroupement spatial (onglet précédent).", sep=""),
                             font=tkfont.create(weight="normal", size=10),
                             justify="left",
                             foreground="darkred",
                             background=.BGcolor)

    B.spatialReinit <- tkbutton(FrameCoords2, text="Réinitialiser", width=22,
                                command=function()
                            {
                                bbox.subset.f(refspa=get("refspa", envir=dataEnv),
                                              facteur=NA,
                                              selection=NA,
                                              tclVars=tclVarsLim)
                                tcl("update")
                            })
    L.spatialReinit <- tklabel(FrameInfoSelect,
                               text=paste("Réinitialiser les limites à l'étendu",
                                          " du référentiel spatial.", sep=""),
                               font=tkfont.create(weight="normal", size=10),
                               justify="left",
                               foreground="darkred",
                               background=.BGcolor)



    ## ############
    ## Évènements :
    tkbind(RB.unitSp, "<Leave>", function(){updateMetrique.f(nomTable=tclvalue(TableMetrique), env=env)})
    tkbind(RB.unitSpSz, "<Leave>", function(){updateMetrique.f(nomTable=tclvalue(TableMetrique), env=env)})
    tkbind(RB.unit, "<Leave>", function(){updateMetrique.f(nomTable=tclvalue(TableMetrique), env=env)})

    tkbind(RB.factGraphUnitobs, "<Leave>", function(){updateFactGraph.f(nomTable=tclvalue(FactGraphTbl), env=env)})
    tkbind(RB.factGraphRefesp, "<Leave>", function(){updateFactGraph.f(nomTable=tclvalue(FactGraphTbl), env=env)})

    tkbind(CB.factGraph, "<FocusIn>", function() {updateFact.f(level=0, env=env)})
    tkbind(CB.fact1, "<FocusIn>", function() {nouvChoixFact.f(level=1, env=env)})

    tkbind(WinSelection, "<Destroy>", function(){tclvalue(Done) <- 2})

    ## Boutons pour l'emprise spatiale :
    tkbind(B.spatialReinit, "<Enter>",
           function()
       {
           tkconfigure(L.spatialReinit,
                       wraplength=0.95 * as.integer(tkwinfo("width", Fvariables)))

           tkgrid(L.spatialReinit, sticky="w")
           tcl("update")
       })
    tkbind(B.spatialReinit, "<Leave>",
           function()
       {
           tkgrid.remove(L.spatialReinit)
       })

    tkbind(B.spatialFact, "<Enter>",
           function()
       {
           tkconfigure(L.spatialFact,
                       wraplength=0.95 * as.integer(tkwinfo("width", Fvariables)))

           tkgrid(L.spatialFact, sticky="w")
           tcl("update")
       })
    tkbind(B.spatialFact, "<Leave>",
           function()
       {
           tkgrid.remove(L.spatialFact)
       })

    tkbind(B.spatialSelect, "<Enter>",
           function()
       {
           tkconfigure(L.spatialSelect,
                       wraplength=0.95 * as.integer(tkwinfo("width", Fvariables)))

           tkgrid(L.spatialSelect, sticky="w")
           tcl("update")
       })
    tkbind(B.spatialSelect, "<Leave>",
           function()
       {
           tkgrid.remove(L.spatialSelect)
       })


    ## #############################
    ## Positionnement des éléments :

    ## Facteur de regroupement spatial :
    tkgrid(tklabel(FrameSpatial, text="    Modalités (toutes par défaut)  ",
                   background=.BGcolor),
           column=1, pady=2)

    tkgrid(CB.spatial, B.factSpatialSel, pady=2)

    tkgrid(FrameSpatial, column=1, columnspan=3, sticky="ew",
           padx=5, pady=5)

    ## Choix de la métrique :
    if (!is.benthos.f() && !is.element(nextStep, nextStepSansCT)) # Table pas pertinente pour benthos
    {
        tkgrid(RB.unitSpSz, sticky="w")
        if (nrow(get("unitSpSz", envir=dataEnv)) == 0) # désactivation si pas de classe de taille dispo.
        {
            tkconfigure(RB.unitSpSz, state="disabled")
        }else{}
    }else{}


    if (is.element(nextStep, c(nextStepBiodiv)))
    {
        tkgrid(RB.unitSp, sticky="w")
        tkgrid(RB.unit, CB.metrique, ## tklabel(FrameMetrique, text=" \n"),
               sticky="w")
    }else{
        tkgrid(RB.unitSp, CB.metrique, ## tklabel(FrameMetrique, text=" \n"),
               sticky="w")
    }

    tkgrid.configure(CB.metrique, padx=10)

    tkgrid(FrameMetrique, column=1, columnspan=3, sticky="ew",
           padx=5, pady=5)

    ## Choix du facteur de séparation des graphiques :
    tkgrid(FrameFactGraph, column=1, columnspan=3, sticky="ew",
           padx=5, pady=5)

    tkgrid(tklabel(FrameGB, text="    Modalités (toutes par défaut)  ",
                                        background=.BGcolor),
           column=1, pady=2)
    tkgrid(CB.factGraph, B.factGraphSel, sticky="s")

    tkgrid(FrameRB, FrameGB, ## tklabel(FrameFactGraph, text="\n"),
           sticky="sw")
    tkgrid(RB.factGraphUnitobs, sticky="w")
    tkgrid(RB.factGraphRefesp, sticky="w")

    tkconfigure(CB.factGraph,
                value=champsRefEspeces.f(site=getOption("P.MPA"), dataEnv=dataEnv,
                                         ordered=TRUE, tableMetrique=tclvalue(TableMetrique),
                                         nextStep=nextStep))
    ## tkconfigure(CB.metrique, value=champsMetriques.f(tclvalue(TableMetrique), nextStep)) # inutile

    if (is.element(nextStep, nextStepUnitobs)) # Pas pertinent pour de l'agrégation /unitobs.
    {
        tkconfigure(RB.factGraphUnitobs, state="disabled")
    }else{}

    ## Choix des facteurs de regroupement :
    if ( ! is.element(nextStep, nextStepNoGrpFact))
    {
        tkgrid(FrameFact, column=1, columnspan=3, sticky="ew",
               padx=5, pady=5)

        tkgrid(tklabel(FrameFact,
                       text="    Modalités (toutes par défaut)  ",
                       background=.BGcolor), column=2,
               sticky="w", pady=2)
        tkgrid(tklabel(FrameFact, text="Facteur 1 ", background=.BGcolor),
               CB.fact1, B.factSel1, sticky="", pady=1)
    }else{
        tkgrid(tklabel(Fvariables, background=.BGcolor))
        pos <- as.character(as.tclObj(tkgrid.info(FrameFactGraph)))

        tkgrid.rowconfigure(Fvariables, pos[which(pos == "-row") + 1], weight=9)
    }

    ## Notebook :
    tkgrid(NB, sticky="ewsn", columnspan=3, pady=2)

    ## tkgrid(Fzone, padx=0, pady=0, sticky="swne")
    tkpack(Fzone, padx=0, pady=0, fill="both", expand=TRUE)
    tkgrid(Fvariables, padx=0, pady=0, sticky="swne")

    ## tcl(NB, "select", 1)
    ## tcl(NB, "select", 0)

    ## Emprise spatiale :
    tcl("update")
    tkconfigure(Fzone,
                width=tkwinfo("width", Fvariables),
                height=tkwinfo("height", Fvariables))

    tkgrid(tklabel(Fzone,
                   text=paste("Veuillez définir les limites de la zone à représenter (par défaut,",
                              " l'ensemble de la zone couverte par le référentiel spatial).\n",
                              "",
                              sep=""),
                   font=tkfont.create(weight="normal", size=10),
                   justify="left",
                   foreground="darkred",
                   background=.BGcolor,
                   wraplength=0.9 * as.integer(tkwinfo("width", Fvariables))),
           padx=5, pady=7, columnspan=2, sticky="w")

    tkgrid(tklabel(FrameN, text="Nord", background=.BGcolor))
    tkgrid(E.N)

    tkgrid(tklabel(FrameE, text="Est", background=.BGcolor))
    tkgrid(E.E)

    tkgrid(tklabel(FrameS, text="Sud", background=.BGcolor))
    tkgrid(E.S)

    tkgrid(tklabel(FrameW, text="Ouest", background=.BGcolor))
    tkgrid(E.W)

    ## tkpack(FrameN, side="top", expand=FALSE)
    ## tkpack(FrameS, side="bottom", expand=FALSE)
    ## tkpack(FrameE, side="right", expand=FALSE)
    ## tkpack(FrameW, side="left", expand=FALSE)
    tkgrid(FrameN, column=1)
    tkgrid(FrameW)
    tkgrid(FrameE, column=2, row=1)
    tkgrid(FrameS, column=1)

    tkgrid(FrameCoords2, sticky="ew", columnspan=2)

    tkgrid.rowconfigure(FrameCoords2, 0, weight=9)
    tkgrid.rowconfigure(FrameCoords2, 1, minsize=1, weight=0)
    tkgrid.rowconfigure(FrameCoords2, 2, minsize=1, weight=0)
    tkgrid.rowconfigure(FrameCoords2, 3, minsize=1, weight=0)

    tkgrid.columnconfigure(FrameCoords2, 0, minsize=1, weight=9)
    tkgrid.columnconfigure(FrameCoords2, 1, minsize=1, weight=7)

    tkgrid(FrameCoords, padx=5, pady=5, sticky="w", rowspan=4)
    tkgrid(B.spatialReinit, column=1, row=1, padx=5, pady=5, sticky="w")
    tkgrid(B.spatialFact, column=1, row=2, padx=5, pady=5, sticky="w")
    tkgrid(B.spatialSelect, column=1, row=3, padx=5, pady=5, sticky="w")

    tkconfigure(FrameCoords2, width=tkwinfo("width", Fzone))

    tkgrid(FrameInfoSelect, padx=5, pady=10, sticky="ew", columnspan=3)

    tkgrid.columnconfigure(Fzone, 0, minsize=1, weight=0)
    tkgrid.columnconfigure(Fzone, 1, minsize=1, weight=0)
    tkgrid.columnconfigure(Fzone, 2, minsize=1, weight=9)

    tkgrid(tklabel(Fzone, background=.BGcolor))

    ## Barre de boutons :
    tkgrid(FrameBT, columnspan=3, pady=10,
           padx=5)## , sticky="ew")

    tkgrid(L.TG <- tklabel(FrameTG, text="Type de graphique :   ", background="white"), sticky="w")
    tkgrid(CB.TG, sticky="w")
    tooltipWidget.f(text=paste("boxplot : graphiques en boîtes à moustache.",
                               "\nbarplot : graphiques en barres."),
                    targetWidget=L.TG)

    tkgrid(FrameTG,  tklabel(FrameBT, text="  ", background="white"),
           B.OK, tklabel(FrameBT, text="  ", background="white"),
           B.Cancel,
           tklabel(FrameBT, text="       ", background="white"),
           B.optGraph,
           sticky="s")

    ## Update des fenêtres :
    tcl("update")

    ## tkfocus(WinSelection)
    winSmartPlace.f(WinSelection)

    ## Tant que l'utilisateur ne ferme pas la fenêtre... :
    repeat
    {
        tkwait.variable(Done)           # attente d'une modification de statut.

        if (tclvalue(Done) == "1")      # statut exécution OK.
        {

            ## Vérifications des variables (si bonnes, le statut reste 1) :
            tclvalue(Done) <- verifVariables.f(metrique=tclvalue(MetriqueChoisie),
                                               factGraph=tclvalue(FacteurGraph), factGraphSel=factGraphSel,
                                               listFact=sapply(listFacteurs, tclvalue), listFactSel=listFactSel,
                                               tableMetrique=tclvalue(TableMetrique),
                                               nextStep=nextStep, dataEnv=dataEnv,
                                               ParentWin=WinSelection)

            if (tclvalue(Done) != "1") {next()} # traitement en fonction du statut : itération
                                        # suivante si les variables ne sont pas bonnes.

            ## Bounding box :
            bbox <- matrix(as.numeric(c(tclvalue(LimiteW), tclvalue(LimiteE),
                                      tclvalue(LimiteS), tclvalue(LimiteN))),
                           byrow=TRUE,
                           nrow=2, ncol=2, dimnames=list(c("x", "y"), c("min", "max")))

            ## ...Sinon, lancement de l'étape suivante :
            switch(casStep[nextStep],
                   spBarBoxplot.unitobs={

                       ## tkmessageBox(message="BoxPlots")
                       subplotCarto.unitobs.f(graphType=tclvalue(TypeGraph),
                                              metrique=tclvalue(MetriqueChoisie),
                                              factSpatial=tclvalue(FacteurSpatial), factSpatialSel=factSpatialSel,
                                              factGraph=tclvalue(FacteurGraph), factGraphSel=factGraphSel,
                                              listFact=sapply(listFacteurs, tclvalue), listFactSel=listFactSel,
                                              tableMetrique=tclvalue(TableMetrique),
                                              bbox=bbox,
                                              dataEnv=dataEnv, baseEnv=baseEnv)
                   },
                   tkmessageBox(message=paste("Aucune action (option '", nextStep, "' pas implémentée).", sep=""),
                                icon="warning"))

            ## Fenêtre de sélection ramenée au premier plan une fois l'étape finie :
            winRaise.f(WinSelection)
        }
        if (tclvalue(Done) == "2") {break()} # statut d'exécution 'abandon' : on sort de la boucle.
    }


    tkdestroy(WinSelection)             # destruction de la fenêtre.
}








### Local Variables:
### ispell-local-dictionary: "english"
### fill-column: 120
### End:
