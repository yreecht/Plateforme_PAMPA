#-*- coding: latin-1 -*-
# Time-stamp: <2018-11-27 13:03:49 yreecht>

## Plateforme PAMPA de calcul d'indicateurs de ressources & biodiversité
##   Copyright (C) 2008-2013 Ifremer - Tous droits réservés.
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

### File: Variables_selection_interface.R
### Created: <2012-01-18 15:55:46 yreecht>
###
### Author: Yves Reecht
###
####################################################################################################
### Description:
###
### Fonctions pour l'interface du système générique de sélection des variables et modalités de
### facteurs.
####################################################################################################


########################################################################################################################
selectModWindow.f <- function(champ, data, selectmode="multiple", sort=TRUE, preselect=NULL, title=NULL, label=NULL)
{
    ## Purpose: Ouvre une fenêtre pour le choix des modalités d'un facteur
    ## ----------------------------------------------------------------------
    ## Arguments: champ : le nom de la colonne du facteur
    ##            data : la table de données
    ##            selectmode : mode de sélection (parmi "single" et
    ##                         "multiple")
    ##            sort : ordonner les modalités ? (booléen)
    ##            preselect : un vecteur de modalités à présélectionner (pour
    ##                        des sélections persistantes).
    ##            title : titre de la fenêtre.
    ##            label : texte d'explication.
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date:  5 août 2010, 09:38

    if(champ == "")                     # condition d'arrêt (pas de champ sélectionné).
    {
        return(NULL)
    }else{
        if (all(is.na(data[ , champ])))  # Cas des champs vides (ajouter un message).
        {
            return(NULL)
        }
    }
    selection <- NULL

    ## ########## Définition des éléments graphiques ##########
    winfac <- tktoplevel()   ## (width = 80)

    if (is.null(title))
    {
        tkwm.title(winfac, paste(mltext("selectModWindow.f.title"), champ, sep=""))
    }else{
        tkwm.title(winfac, title)
    }

    ## Assenceur vertical :
    SCR.y <- tkscrollbar(winfac, repeatinterval=5, command=function(...){tkyview(LB, ...)})

    ## List Box de sélection :
    LB <- tklistbox(winfac, height=20, width=50, selectmode=selectmode,
                    yscrollcommand=function(...)tkset(SCR.y, ...), background="white")

    ## Boutons :
    FrameB <- tkframe(winfac)
    B.OK <- tkbutton(FrameB, text=mltext("OK.button"), command=function()
                {
                    assign("selection", listMod[as.numeric(tkcurselection(LB))+1], parent.env(environment()))
                    ## assign("tmptk", tkcurselection(LB), envir=.GlobalEnv)
                    tkdestroy(winfac)
                })
    B.Cancel <- tkbutton(FrameB, text=mltext("Cancel.button"), command=function()
                     {
                         assign("selection", NULL, parent.env(environment()))
                         tkdestroy(winfac)
                     })

    ## ########## Placement des éléments sur la grille ##########
    ## Explications :
    if (is.null(label))
    {
        tkgrid(tklabel(winfac, text=paste(mltext("selectModWindow.f.CB.1"), champ,
                               mltext("selectModWindow.f.CB.2"),
                               sep="")), columnspan=2)
    }else{
        tkgrid(tklabel(winfac, text=label), columnspan=2)
    }

    ## Avertissement 'plusieurs sélections possibles' :
    if (is.element(selectmode, c("extended", "multiple")))
    {
        tkgrid(tklabel(winfac, text="Plusieurs sélections POSSIBLES.\n"), columnspan=2)
    }else{}

    ## Avertissement mode de sélection étendu :
    if (selectmode == "extended")
    {
        tkgrid(tklabel(winfac,
                       text=paste(mltext("selectModWindow.f.info.1"),
                       mltext("selectModWindow.f.info.2"),
                       mltext("selectModWindow.f.info.3"), sep=""),
                       fg="red"), columnspan=2, rowspan=2)
    }else{}

    tkgrid(LB, SCR.y)
    tkgrid.configure(SCR.y, rowspan=4, sticky="nsw")
    tkgrid(FrameB, columnspan=2, sticky="")
    tkgrid(B.OK, tklabel(FrameB, text="        "), B.Cancel, sticky="", pady=5)

    ## Configuration de la liste :
    if (sort)
    {
        listMod <- unique(as.character(sort(data[ , champ])))
    }else{
        listMod <- unique(as.character(data[ , champ]))
    }

    invisible(sapply(listMod, function(x){tkinsert(LB, "end", x)}))

    ## Sélections persistantes :
    if (!is.null(preselect))
    {
        sapply(which(is.element(listMod, preselect)) - 1,
               function(i){tkselection.set(LB, i)})
    }
    ## tkselection.set(LB, 0)

    tkbind(winfac, "<Control-a>",       # Tout sélectionner
           function()
       {
           sapply(seq(from=0, length.out=length(listMod)),
                  function(i) {tkselection.set(LB, i)})
       })

    ## Affichage/attente :
    tkfocus(LB)

    tcl("update")
    winSmartPlace.f(winfac, xoffset=50, yoffset=-100)

    tkwait.window(winfac)
    return(selection)
}



########################################################################################################################
selectModalites.f <- function(factor, tableMetrique, env, nextStep, dataEnv, level=0)
{

    ## Purpose: Sélection et stockage des modalités d'un facteur
    ## ----------------------------------------------------------------------
    ## Arguments: factor : le nom du facteur sélectionné.
    ##            tableMetrique : nom de la table des métriques.
    ##            nextStep : étape suivante.                     [!!!] on devrait pouvoir s'en passer  [yr: 18/1/2012]
    ##            level : l'ordre du facteur (0 pour celui de séparation des
    ##                    graphiques, 1, 2,... pour les suivants).
    ##            env : environnement de la fonction appelante.
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date:  4 août 2010, 14:21

    facts <- c(tclvalue(get("FacteurGraph", envir=env)),
               sapply(get("listFacteurs", envir=env), tclvalue))

    selections <- c(list(get("factGraphSel", envir=env)), # Liste des modalités déjà sélectionnées
                    get("listFactSel", envir=env))

    preselect <- NULL                   # sélections persistantes d'une fois sur l'autre
    if (!is.na(selections[level + 1]))
    {
        preselect <- selections[[level + 1]]
    }

    ## Table réduite :
    metrique <- tclvalue(get("MetriqueChoisie" , envir=env))

    ## Pour les indices de biodiversité recalculés, il faut utiliser "unitSp" et une métrique adaptée.
    if (is.element(nextStep, c("boxplot.unitobs", "modele_lineaire.unitobs",
                               "MRT.unitobs", "barplot.unitobs",
                               "spBarBoxplot.unitobs", "spSymbols.unitobs")) &&
        tableMetrique == "unit")
    {
        tableMetrique <- "unitSp"
        metrique <- getOption("P.nbName")
    }else{}

    tmp <- subsetToutesTables.f(metrique=metrique, facteurs=facts, selections=selections,
                                dataEnv=dataEnv, tableMetrique=tableMetrique , exclude=level + 1)

    ## Sélection des modalités
    sel <- selectModWindow.f(champ=factor, data=tmp, selectmode="extended", preselect=preselect)

    if (!is.null(sel) & length(sel) > 0)
    {
        ## Expression à évaluer (stockage des modalités sélectionnées) :
        exprModSel <- ifelse(level==0,
                             paste("factGraphSel <- c(\"", # Cas du facteur de séparation des graphiques
                                   paste(sel, collapse="\", \""),
                                   "\")", sep=""),
                             paste("listFactSel[[", level, "]] <- c(\"", # Facteurs de regroupement
                                   paste(sel, collapse="\", \""),
                                   "\")", sep=""))

        eval(parse(text=exprModSel), envir=env)
    }
}



########################################################################################################################
verifVariables.f <- function(metrique, factGraph, factGraphSel, listFact, listFactSel, tableMetrique, nextStep, dataEnv,
                             factSpatial=NULL, factSpatialSel=NULL, ParentWin=NULL)
{
    ## Purpose: Vérification du choix des métrique et facteur(s) et retourne
    ##          un statut (éventuellement accompagné d'un avertissement)
    ## ----------------------------------------------------------------------
    ## Arguments: metrique : la métrique choisie.
    ##            factGraph : le facteur de séparation des graphiques.
    ##            factGraphSel : la sélection de modalités pour ce dernier
    ##            listFact : liste du (des) facteur(s) de regroupement
    ##            listFactSel : liste des modalités sélectionnées pour ce(s)
    ##                          dernier(s)
    ##            tableMetrique : nom de la table des métriques.
    ##            nextStep : nom de l'étape suivante.
    ##            dataEnv : l'environnement des données.
    ##            factSpatial : facteur de zone (carto uniquement).
    ##            factSpatialSel : sélection sur facteur de zone
    ##                             (carto uniquement).
    ##            ParentWin : Fenêtre parente.
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date:  6 août 2010, 15:07

    if (is.null(factSpatial))
    {
        facts <- c(factGraph, unlist(listFact)) # liste des facteurs renseignées.
        selections <- c(list(factGraphSel), listFactSel) # listes des leurs modalités sélectionnées.
    }else{
        facts <- c(factSpatial, factGraph, unlist(listFact)) # liste des facteurs renseignées.
        selections <- c(list(factSpatialSel), list(factGraphSel), listFactSel) # listes des leurs modalités sélectionnées.
    }

    idxFacts <- facts != ""                 #
    facts <- facts[idxFacts]                #
    selections <- selections[idxFacts]      #

    ## Définir [!!!]
    return.val <- 2

    ## Carto sans facteur de zone sélectionné :
    if (is.element(nextStep,
                   c("spBarBoxplot.unitobs",
                     "spBarBoxplot.esp",
                     "spSymbols.unitobs",
                     "spSymbols.esp")) &&
        is.null(factSpatial))
    {
        infoLoading.f(msg=mltext("verifVariables.f.info.2"), icon="error", titleType="check")

        return.val <- 0
    }else{}

    ## Métrique pas sélectionnée :
    if (metrique == "")
    {
        infoLoading.f(msg=mltext("verifVariables.f.info.1"), icon="error", titleType="check")

        return.val <- 0
    }else{}

    ## Pour les indices de biodiversité recalculés, il faut utiliser "unitSp" et une métrique adaptée.
    if (is.element(nextStep,
                   c("boxplot.unitobs", "modele_lineaire.unitobs",
                     "MRT.unitobs", "barplot.unitobs",
                     "spBarBoxplot.unitobs", "spSymbols.unitobs")) &&
        tableMetrique == "unit")
    {
        tableMetrique <- "unitSp"
        metrique <- getOption("P.nbName")
    }else{}


    ## Facteurs dupliqués :
    if (length(facts) > length(unique(facts)))
    {
        n <- length(facts) - length(unique(facts))

        infoLoading.f(msg=paste("Il y a ",
                                ifelse(n == 1, "un", "des"),
                                " facteur",
                                ifelse(n == 1, "", "s"),
                                " dupliqué", ifelse(n == 1, "", "s"), sep=""),
                      icon="error", titleType="check")

        return.val <- 0
    }else{}

    ## Pas de facteur de regroupement sélectionné :
    if (length(listFact[unlist(listFact) != ""]) == 0 &&
        ! is.element(nextStep, c("spSymbols.unitobs", "spSymbols.esp")))
    {
        infoLoading.f(msg=mltext("verifVariables.f.info.3"), icon="error", titleType="check")
        return.val <- 0
    }else{}

    ## Champs vides :
    champsVide <- sapply(subsetToutesTables.f(metrique=metrique, facteurs=facts, selections=selections,
                                              dataEnv=dataEnv, tableMetrique=tableMetrique , exclude=NULL),
                         function(x) {all(is.na(x))})

    if (sum(champsVide) > 0)
    {
        infoLoading.f(msg=paste(mltext("verifVariables.f.info.4"),
                                sapply(which(champsVide),
                                       function(i){varNames[names(champsVide)[i], "nom"]}),
                                mltext("verifVariables.f.info.5"), sep="", collapse="\n"),
                      icon="error", titleType="check")
        return.val <- 0
    }else{}

    ## Métrique par classe de taille mais facteur "classe_taille" pas retenu :
    if (tableMetrique == "unitSpSz" & !is.element("classe_taille", facts))
    {
        infoLoading.f(msg=paste(mltext("verifVariables.f.info.6"),
                                mltext("verifVariables.f.info.7")),
                      icon="warning", titleType="check")

        return.val <- ifelse(return.val, 1, 0)
    }else{}

    ## Métrique par espèce mais facteur 'espece', 'Identifiant' ou 'code_espece' non retenu :
    if (is.element(nextStep,
                   c("boxplot.esp", "modele_lineaire",
                     "freq_occurrence", "MRT.esp",
                     "barplot.esp", "spSymbols.esp", "spBarBoxplot.esp")) &
        !any(is.element(c("espece", "code_espece", "Identifiant"), facts)))
    {
        infoLoading.f(msg=paste(mltext("verifVariables.f.info.9"),
                                mltext("verifVariables.f.info.10")),
                      icon="warning", titleType="check")

        return.val <- ifelse(return.val, 1, 0)
    }else{}


    ## Fréquences d'occurrence... pas plus de deux facteurs de regroupement :
    if (is.element(nextStep,
                   c("freq_occurrence", "freq_occurrence.unitobs",
                     "barplot.esp", "barplot.unitobs",
                     "spBarBoxplot.esp", "spBarBoxplot.unitobs")) &&
        length(listFact[unlist(listFact) != ""]) > 2)
    {
        infoLoading.f(msg=mltext("verifVariables.f.info.8"), icon="error", titleType="check")
        return.val <- 0
    }else{}

    ## Agrégé toutes espèces... vérification de la pertinence des facteurs :
    if (is.element(nextStep, c("boxplot.unitobs", "modele_lineaire.unitobs",
                               "MRT.unitobs", "barplot.unitobs",
                               "spBarBoxplot.unitobs", "spSymbols.unitobs")) &&
        return.val)                     # Inutile si déjà des erreurs (risque de plantage).
    {
        if (is.null(agregations.generic.f(Data=subsetToutesTables.f(metrique=metrique,
                                                                    facteurs=facts,
                                                                    selections=selections,
                                                                    dataEnv=dataEnv,
                                                                    tableMetrique=tableMetrique,
                                                                    add=c("unite_observation", "code_espece")),
                                          metrics=metrique,
                                          factors=if(tableMetrique == "unitSpSz" &&
                                          factGraph != "classe_taille")
                                      {
                                          c("unite_observation", "classe_taille")
                                      }else{
                                          c("unite_observation")
                                      },
                                          listFact=listFact[unlist(listFact) != ""],
                                          unitSpSz=get("unitSpSz", envir=dataEnv),
                                          unitSp=get("unitSp", envir=dataEnv),
                                          info=TRUE,
                                          dataEnv=dataEnv)))
        {
            infoLoading.f(msg=paste(mltext("verifVariables.f.info.11"),
                                    mltext("verifVariables.f.info.12"),
                                    mltext("verifVariables.f.info.13"), sep=""),
                          icon="error")

            return.val <- 0
        }else{}
    }

    ## ####################################################################################################
    ## Spécifique aux modèles linéaires :
    if (is.element(nextStep, c("modele_lineaire", "modele_lineaire.unitobs")))
    {
        data <- subsetToutesTables.f(metrique=metrique, facteurs=facts, selections=selections,
                                     dataEnv=dataEnv, tableMetrique=tableMetrique, add=NULL)

        ## Les facteurs doivent avoir au moins deux niveaux :
        if (any(sapply(data[ , !is.element(colnames(data), c(metrique, factGraph)), drop=FALSE],
                       function(x)length(unique(x))) == 1))
        {
            fact1level <-
                colnames(data)[!is.element(colnames(data),
                                           c(metrique, factGraph))][sapply(data[ , !is.element(colnames(data),
                                                                                               c(metrique, factGraph)), drop=FALSE],
                                                                           function(x)length(unique(x))) == 1]

            infoLoading.f(msg=paste(mltext("verifVariables.f.info.14"), "\n\t- '",
                                    paste(fact1level, collapse="'\n\t- '"),
                                    "'\n", mltext("verifVariables.f.info.15"), sep=""),
                          icon="error")

            return.val <- 0
        }else{}

        ## Bloquer les analyses à plus de 3 facteurs :
        if (length(listFact[unlist(listFact) != ""]) > 3)
        {
            infoLoading.f(msg=paste(mltext("verifVariables.f.info.16"),
                                    mltext("verifVariables.f.info.17"),
                                    mltext("verifVariables.f.info.18"),
                                     sep=""),
                          icon="error")

            return.val <- 0
        }else{
            if (length(listFact[unlist(listFact) != ""]) >= 3) # On garde la possibilité d'augmenter le seuil de refus
                                        # des analyses, ci-dessus, sans modifier celui-ci.
            {
                infoLoading.f(msg=paste(mltext("verifVariables.f.info.19"),
                                        mltext("verifVariables.f.info.20"),
                                        mltext("verifVariables.f.info.21"),
                                        sep=""),
                              icon="warning")

                return.val <- ifelse(return.val, 1, 0)
            }else{}
        }
    }else{}

    if (return.val < 2)                 # erreur ou warning...
    {
        ## Bouton OK + attente de confirmation.
        infoLoading.f(button = TRUE,
                      WinRaise=ParentWin)

        return(return.val)
    }else{                              # ...sinon :
        return(1)                       # tout est OK.
    }
}


########################################################################################################################
updateMetrique.f <- function(nomTable, env)
{
    ## Purpose: Mise à jours des champs de métriques disponibles + mise à
    ##          jour des facteurs pertinents en fonction de la table de
    ##          métriques retenue.
    ## ----------------------------------------------------------------------
    ## Arguments: nomTable : le nom de la table actuellement sélectionnée.
    ##            env : l'environnement de la fonction appelante.
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 11 août 2010, 17:15

    ## Facteurs de regroupements :
    if (exists("listFacteurs", envir=env, inherits=FALSE))
    {
        listFacteurs <- get("listFacteurs", envir=env)
    }else{}

    ## Traitements spécifiques au choix d'une table de métrique.
    switch(nomTable,

           ## Si table unitSp (métriques d'observation) :
           unitSp={
               evalq(tkconfigure(CB.metrique,
                                 value=champsMetriques.f(nomTable="unitSp", nextStep=nextStep, dataEnv=dataEnv)),
                     envir=env)

               evalq(if (!is.element(tclvalue(MetriqueChoisie),
                                     champsMetriques.f(nomTable="unitSp", nextStep=nextStep, dataEnv=dataEnv)))
                 {
                     tclvalue(MetriqueChoisie) <- ""                           # réinitialisation
                     tryCatch(tkconfigure(RB.factGraphRefesp, state="normal"), # réactivation du référentiel
                              error=function(e){return(NULL)})                 # espèces... si possible !
                 }, envir=env)

           },
           ## Si table unitSpSz (métriques d'observation) :
           unitSpSz={
               evalq(tkconfigure(CB.metrique,
                                 value=champsMetriques.f(nomTable="unitSpSz", nextStep=nextStep, dataEnv=dataEnv)),
                     envir=env)

               evalq(if (!is.element(tclvalue(MetriqueChoisie),
                                     champsMetriques.f(nomTable="unitSpSz", nextStep=nextStep, dataEnv=dataEnv)))
                 {
                     tclvalue(MetriqueChoisie) <- ""                           # réinitialisation
                     tryCatch(tkconfigure(RB.factGraphRefesp, state="normal"), # réactivation du référentiel
                              error=function(e){return(NULL)})                 # espèces... si possible !
                 }, envir=env)
           },
           ## Si table TabbleBiodiv (indices de biodiversité) :
           unit={
               evalq(tkconfigure(CB.metrique,
                                 value=champsMetriques.f(nomTable="unit", nextStep=nextStep, dataEnv=dataEnv)),
                     envir=env)

               evalq(if (!is.element(tclvalue(MetriqueChoisie),
                                     champsMetriques.f(nomTable="unit", nextStep=nextStep, dataEnv=dataEnv)))
                 {
                     tclvalue(MetriqueChoisie) <- "" # réinitialisation
                 }, envir=env)
               ## Le seul référentiel pertinent est celui des unités d'observations...
               if (is.element(evalq(nextStep, envir=env),
                              c("boxplot.esp", "modele_lineaire",
                                "barplot.esp")))
               {                        # ... si l'on travaille uniquement sur *toutes les espèces* !
                   evalq(tclvalue(FactGraphTbl) <- "unitobs", envir=env)
                   tryCatch(evalq(tkconfigure(RB.factGraphRefesp, state="disabled"), envir=env), # désactivation du
                                        # référentiel espèces
                            error=function(e){return(NULL)})                 # ... si possible !
               }else{}                  #Rien !

           },

           ## Comportement par défaut (pas de table sélectionnée) :
           evalq(tkconfigure(CB.metrique, value=""), envir=env)
           )

    if (exists("listFacteurs", envir=env, inherits=FALSE))
    {
        ## Traitements communs :
        evalq(updateFactGraph.f(nomTable=tclvalue(FactGraphTbl), env=env), envir=env) # mise à jour de la
                                        # combobox de choix du facteur de séparation des graphiques.

        ## Mise à jour des facteurs de regroupements pertinents :
        eval(parse(text=
                   eval(substitute(paste("tkconfigure(CB.fact", level,
                                         ", value=champsReferentiels.f(nomTable=tclvalue(TableMetrique),",
                                         " dataEnv=dataEnv,",
                                         " nextStep=nextStep))", sep=""),
                                   list(level=1:length(listFacteurs))))), envir=env)

        eval(parse(text=
                   eval(substitute(paste("if (!is.element(tclvalue(listFacteurs[[", level,
                                         "]]), champsReferentiels.f(nomTable=tclvalue(TableMetrique),",
                                         " dataEnv=dataEnv,",
                                         " nextStep=nextStep)))",
                                         "{ tclvalue(listFacteurs[[", level, "]]) <- '' ; ",
                                         "updateFact.f(level=", level,", env=env)}", sep=""),
                                   list(level=1:length(listFacteurs))))), envir=env)
    }else{}
}



########################################################################################################################
updateFactGraph.f <- function(nomTable, env)
{
    ## Purpose: MàJ des champs dans la combobox de choix du facteur de
    ##          séparation des graphiques. Si la table d'origine (référentiel
    ##          d'espèces ou unitobs) a changé, la sélection est
    ##          réinitialisée.
    ## ----------------------------------------------------------------------
    ## Arguments: nomTable : le nom de la table actuellement sélectionnée.
    ##            env : l'environnement de la fonction appelante.
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date:  3 août 2010, 14:01

    switch(nomTable,
           ## Si table "référentiel d'espèce" :
           refesp={
               evalq(tkconfigure(CB.factGraph,
                                 value=champsRefEspeces.f(site=getOption("P.MPA"),
                                                          dataEnv=dataEnv, ordered=TRUE,
                                                          tableMetrique=tclvalue(TableMetrique),
                                                          nextStep=nextStep)),
                     envir=env)
               evalq(if (!is.element(tclvalue(FacteurGraph),
                                     champsRefEspeces.f(site=getOption("P.MPA"),
                                                        dataEnv=dataEnv, tableMetrique=tclvalue(TableMetrique),
                                                        nextStep=nextStep)))
                 {
                     tclvalue(FacteurGraph) <- "" # réinitialisation de la sélection.
                 }, envir=env)
           },
           ## Si table des "unités d'observation" :
           unitobs={
               evalq(tkconfigure(CB.factGraph,
                                 value=champsUnitobs.f(dataEnv=dataEnv, ordered=TRUE, tableMetrique=tclvalue(TableMetrique))),
                     envir=env)

               evalq(if (!is.element(tclvalue(FacteurGraph),
                                     champsUnitobs.f(dataEnv=dataEnv, tableMetrique=tclvalue(TableMetrique))))
                 {
                     tclvalue(FacteurGraph) <- "" # réinitialisation de la sélection.
                 }, envir=env)
           },
           ## Comportement par défaut (pas de table sélectionnée) :
           evalq(tkconfigure(CB.factGraph, value=""), envir=env)
           )
}


########################################################################################################################
updateFact.f <- function(level, env)
{
    ## Purpose: Réinitialisation des modalités sélectionnées si le facteur a
    ##          changé
    ## ----------------------------------------------------------------------
    ## Arguments: level : ordre du facteur
    ##            env : environnement de la fonction appelante
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date:  5 août 2010, 16:16
    if (level == 0)
    {
        ## Cas du facteur de séparation des graphiques :
        if (get("FacteurGraph.old", envir=env) != tclvalue(get("FacteurGraph", envir=env)))
        {
            ## Réinitialisation des modalités sélectionnées :
            evalq(factGraphSel <- NA, envir=env)
            evalq(FacteurGraph.old <- tclvalue(FacteurGraph), envir=env)
        }
    }else{
        ## Autres cas :
        if(length(get("listFacteurs.old", envir=env)) >= level) # pour éviter des erreurs
        {
            if (get("listFacteurs.old", envir=env)[[level]] !=
                tclvalue(get("listFacteurs", envir=env)[[level]]))
            {
                ## Réinitialisation des modalités sélectionnées :
                expr <- paste("listFactSel[[", level, "]] <- NA", sep="")
                eval(parse(text=expr), envir=env)
            }
        }
        ## Mise à jour de "listFacteurs.old" :
        evalq(listFacteurs.old <- as.list(sapply(listFacteurs, tclvalue)), envir=env)
    }
}


########################################################################################################################
nouvChoixFact.f <- function(level, env)
{
    ## Purpose: Ajouter une combo box de choix de facteur de regroupement et
    ##          créer les évènements et boutons associés.
    ## ----------------------------------------------------------------------
    ## Arguments: level : niveau du facteur (integer)
    ##            env : environnement de la fonction d'appel
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date:  3 août 2010, 18:10

    fact <- get("listFacteurs", envir=env) # Récupération de la liste des facteurs sélectionnés.

    if (length(fact) <= level && tclvalue(fact[[level]]) != "") # Si un nouveau facteur a été ajouté :
    {
        ## ########## Expressions ##########
        ## Ajout d'une entrée à la liste des facteurs :
        exprTclvar <- paste("listFacteurs[[", level + 1, "]] <- tclVar('')", sep="")

        ## Ajout d'une entrée à la liste des modalités sélectionnées :
        exprModSel <- paste("listFactSel[[", level + 1, "]] <- NA", sep="")

        ## Création d'une combobox supplémentaire :
        exprCB <- paste("CB.fact", level + 1, " <- ttkcombobox(FrameFact,",
                        " value=champsReferentiels.f(nomTable=tclvalue(TableMetrique),",
                        " dataEnv=dataEnv,",
                        " nextStep=nextStep),",
                        " textvariable=listFacteurs[[", level + 1, "]], state='readonly')", sep="")

        ## Création d'un bouton de sélection des modalités supplémentaire :
        exprSel <- paste("B.factSel", level + 1,
                         " <- tkbutton(FrameFact, text='",
                         mltext("selectionVariables.B.fSel"),
                         "', command=function()",
                         " { selectModalites.f(tclvalue(listFacteurs[[", level + 1, "]]), ",
                         "tableMetrique=tclvalue(TableMetrique), env=env, level=",
                         level + 1, ", nextStep=nextStep, dataEnv=dataEnv) ; winRaise.f(WinSelection) })",
                         sep="")

        ## Affichage de la combobox et du bouton :
        exprGrid <- paste("tkgrid(tklabel(FrameFact, text='",
                          mltext("selectionVariables.Factor"),
                          " ",
                          level + 1, " '",
                          ifelse(exists(".BGcolor", envir=env), ", background=.BGcolor", ""),
                          "), CB.fact", level + 1, ", B.factSel", level + 1,
                          ", sticky='', pady=1)", sep="")

        ## Ajout d'un évènement à la combobox :
        exprBind <- paste("tkbind(CB.fact", level + 1,
                          ", '<FocusIn>', function() {nouvChoixFact.f(level=", level + 1,
                          ", env=env)})", sep="")


        ## ########## Évaluation des expressions ##########
        eval(parse(text=exprTclvar), envir=env)
        eval(parse(text=exprModSel), envir=env)
        eval(parse(text=exprCB), envir=env)
        eval(parse(text=exprSel), envir=env)
        eval(parse(text=exprGrid), envir=env)
        eval(parse(text=exprBind), envir=env)

        tcl("update")
        evalq(winSmartPlace.f(WinSelection), envir=env)
    }

    ## Mise à jour des données du facteur courant :
    updateFact.f(level=level, env=env)
}

########################################################################################################################
titreSelVar.f <- function(type, nextStep)
{
    ## Purpose: Retourne les titre (textes de frames, etc.) adaptés en
    ##          fonction de l'étape suivante
    ## ----------------------------------------------------------------------
    ## Arguments: type : identifiant du titre (chaîne de caractères)
    ##            nextStep : code de l'étape suivante (chaîne de caractères)
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 24 août 2010, 11:28

    texts <- list(
        ## Titre de fenêtre :
        winTitle=c(boxplot.esp=mltext("titreSelVar.f.WT.boxplot.esp"),
                   modele_lineaire=mltext("titreSelVar.f.WT.modele_lineaire"),
                   freq_occurrence=mltext("titreSelVar.f.WT.freq_occurrence"),
                   freq_occurrence.unitobs=mltext("titreSelVar.f.WT.freq_occurrence.unitobs"),
                   boxplot.unitobs=mltext("titreSelVar.f.WT.boxplot.unitobs"),
                   modele_lineaire.unitobs=mltext("titreSelVar.f.WT.modele_lineaire.unitobs"),
                   MRT.unitobs=mltext("titreSelVar.f.WT.MRT.unitobs"),
                   MRT.esp=mltext("titreSelVar.f.WT.MRT.esp"),
                   barplot.unitobs=mltext("titreSelVar.f.WT.barplot.unitobs"),
                   barplot.esp=mltext("titreSelVar.f.WT.barplot.esp"),
                   spBarBoxplot.unitobs=mltext("titreSelVar.f.WT.spBarBoxplot.unitobs"),
                   spBarBoxplot.esp=mltext("titreSelVar.f.WT.spBarBoxplot.esp"),
                   spSymbols.unitobs=mltext("titreSelVar.f.WT.spSymbols.unitobs"),
                   spSymbols.esp=mltext("titreSelVar.f.WT.spSymbols.esp")),
        ## Texte pour le choix métrique :
        metrique=c(boxplot.esp=mltext("titreSelVar.f.M.repesented"),
                   modele_lineaire=mltext("titreSelVar.f.M.explained"),
                   freq_occurrence=mltext("titreSelVar.f.M.freq_occurrence"),
                   freq_occurrence.unitobs=mltext("titreSelVar.f.M.freq_occurrence.unitobs"),
                   boxplot.unitobs=mltext("titreSelVar.f.M.repesented"),
                   modele_lineaire.unitobs=mltext("titreSelVar.f.M.explained"),
                   MRT.unitobs=mltext("titreSelVar.f.M.explained"),
                   MRT.esp=mltext("titreSelVar.f.M.explained"),
                   barplot.unitobs=mltext("titreSelVar.f.M.repesented"),
                   barplot.esp=mltext("titreSelVar.f.M.repesented"),
                   spBarBoxplot.unitobs=mltext("titreSelVar.f.M.repesented"),
                   spBarBoxplot.esp=mltext("titreSelVar.f.M.repesented"),
                   spSymbols.unitobs=mltext("titreSelVar.f.M.repesented"),
                   spSymbols.esp=mltext("titreSelVar.f.M.repesented")),
        ## Texte pour le choix d'un facteur de séparation :
        factSep=c(boxplot.esp=mltext("titreSelVar.f.FS.graphFact"),
                  modele_lineaire=mltext("titreSelVar.f.FS.modele_lineaire"),
                  freq_occurrence=mltext("titreSelVar.f.FS.freq_occurrence"),
                  freq_occurrence.unitobs=mltext("titreSelVar.f.FS.selectSp"),
                  boxplot.unitobs=mltext("titreSelVar.f.FS.selectSp"),
                  modele_lineaire.unitobs=mltext("titreSelVar.f.FS.selectSp"),
                  MRT.unitobs=mltext("titreSelVar.f.FS.selectSp"),
                  MRT.esp=mltext("titreSelVar.f.FS.MRT.esp"),
                  barplot.unitobs=mltext("titreSelVar.f.FS.selectSp"),
                  barplot.esp=mltext("titreSelVar.f.FS.graphFact"),
                  spBarBoxplot.unitobs=mltext("titreSelVar.f.FS.selectSp"),
                  spBarBoxplot.esp=mltext("titreSelVar.f.FS.graphFact"),
                  spSymbols.unitobs=mltext("titreSelVar.f.FS.selectSp"),
                  spSymbols.esp=mltext("titreSelVar.f.FS.graphFact")),
        ## Texte pour le choix du(des) facteur(s) explicatif(s) :
        facteurs=c(boxplot.esp=mltext("titreSelVar.f.F.graph"),
                   modele_lineaire=mltext("titreSelVar.f.F.modele_lineaire"),
                   freq_occurrence=mltext("titreSelVar.f.F.freq_occurrence"),
                   freq_occurrence.unitobs=mltext("titreSelVar.f.F.freq_occurrence"),
                   boxplot.unitobs=mltext("titreSelVar.f.F.graph"),
                   modele_lineaire.unitobs=mltext("titreSelVar.f.F.modele_lineaire"),
                   MRT.unitobs=mltext("titreSelVar.f.F.MRT"),
                   MRT.esp=mltext("titreSelVar.f.F.MRT"),
                   barplot.unitobs=mltext("titreSelVar.f.F.graph"),
                   barplot.esp=mltext("titreSelVar.f.F.graph"),
                   spBarBoxplot.unitobs=mltext("titreSelVar.f.F.graph"),
                   spBarBoxplot.esp=mltext("titreSelVar.f.F.graph"),
                   spSymbols.unitobs=mltext("titreSelVar.f.F.graph"),
                   spSymbols.esp=mltext("titreSelVar.f.F.graph")),
        ## Niveau d'agrégation pour table / espèce :
        tabListesp=c(boxplot.esp=mltext("titreSelVar.f.tLsp.UnitobsSp"),
                     modele_lineaire=mltext("titreSelVar.f.tLsp.UnitobsSp"),
                     freq_occurrence=mltext("titreSelVar.f.tLsp.UnitobsSp"),
                     freq_occurrence.unitobs=mltext("titreSelVar.f.tLsp.Unitobs"),
                     boxplot.unitobs=mltext("titreSelVar.f.tLsp.Unitobs"),
                     modele_lineaire.unitobs=mltext("titreSelVar.f.tLsp.Unitobs"),
                     MRT.unitobs=mltext("titreSelVar.f.tLsp.Unitobs"),
                     MRT.esp=mltext("titreSelVar.f.tLsp.UnitobsSp"),
                     barplot.unitobs=mltext("titreSelVar.f.tLsp.Unitobs"),
                     barplot.esp=mltext("titreSelVar.f.tLsp.UnitobsSp"),
                     spBarBoxplot.unitobs=mltext("titreSelVar.f.tLsp.Unitobs"),
                     spBarBoxplot.esp=mltext("titreSelVar.f.tLsp.UnitobsSp"),
                     spSymbols.unitobs=mltext("titreSelVar.f.tLsp.Unitobs"),
                     spSymbols.esp=mltext("titreSelVar.f.tLsp.UnitobsSp")),
        ## Niveau d'agrégation pour table / classe de taille :
        tabListespCT=c(boxplot.esp=mltext("titreSelVar.f.tLspCT.UnitobsSpCT"),
                       modele_lineaire=mltext("titreSelVar.f.tLspCT.UnitobsSpCT"),
                       freq_occurrence=mltext("titreSelVar.f.tLspCT.UnitobsSpCT"),
                       freq_occurrence.unitobs=mltext("titreSelVar.f.tLspCT.UnitobsCT"),
                       boxplot.unitobs=mltext("titreSelVar.f.tLspCT.UnitobsCT"),
                       modele_lineaire.unitobs=mltext("titreSelVar.f.tLspCT.UnitobsCT"),
                       MRT.unitobs=mltext("titreSelVar.f.tLspCT.UnitobsCT"),
                       MRT.esp=mltext("titreSelVar.f.tLspCT.UnitobsSpCT"),
                       barplot.unitobs=mltext("titreSelVar.f.tLspCT.UnitobsCT"),
                       barplot.esp=mltext("titreSelVar.f.tLspCT.UnitobsSpCT"),
                       spBarBoxplot.unitobs=mltext("titreSelVar.f.tLspCT.UnitobsCT"),
                       spBarBoxplot.esp=mltext("titreSelVar.f.tLspCT.UnitobsSpCT"),
                       spSymbols.unitobs=mltext("titreSelVar.f.tLspCT.UnitobsCT"),
                       spSymbols.esp=mltext("titreSelVar.f.tLspCT.UnitobsSpCT")),
        factSpatial=c(boxplot.esp=mltext("titreSelVar.f.factSpatial"),
                      modele_lineaire="",
                      freq_occurrence="",
                      freq_occurrence.unitobs="",
                      boxplot.unitobs="",
                      modele_lineaire.unitobs="",
                      MRT.unitobs="",
                      MRT.esp="",
                      barplot.unitobs="",
                      barplot.esp="",
                      spBarBoxplot.unitobs=mltext("titreSelVar.f.factSpatial"),
                      spBarBoxplot.esp=mltext("titreSelVar.f.factSpatial"),
                      spSymbols.unitobs=mltext("titreSelVar.f.factSpatial"),
                      spSymbols.esp=mltext("titreSelVar.f.factSpatial"))
        ## =c(boxplot= , modele_lineaire=),
        )

    return(texts[[type]][nextStep])
}


########################################################################################################################

selectionVariables.f <- function(nextStep, dataEnv, baseEnv)
{
    ## Purpose: * Sélection des métrique et facteur(s) ainsi que leur(s)
    ##            modalité(s).
    ##          * Lancement de l'étape suivante (graphique, analyse
    ##            statistique,...).
    ## ----------------------------------------------------------------------
    ## Arguments: nextStep : étape suivante (chaîne de caractères parmi
    ##                       "boxplot.esp", "modele_lineaire",
    ##                       "freq_occurrence", "freq_occurrence.unitobs",
    ##                       "boxplot.unitobs", "modele_lineaire.unitobs",
    ##                       "MRT.unitobs", "MRT.esp",
    ##                       "barplot.unitobs", "barplot.esp",...
    ##                       [appelé à s'étoffer]).
    ##            dataEnv : l'environnement des données.
    ## Note : les arguments de cette fonction peuvent changer à l'avenir
    ##        (ajouts)
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date:  6 août 2010, 14:38

    ## Variables :
    env <- environment()                    # Environnement courant
    Done <- tclVar(0)                       # Statut d'exécution

    ## ##################################################
    ## Groupes de traitements :

    ## Étapes ne nécessitant pas de choix des variables :
    nextStepMetriqueFixe <- c("freq_occurrence", "freq_occurrence.unitobs")

    ## Étapes "graphiques" (besoin d'options graphiques) :
    nextStepGraph <- c("boxplot.esp", "freq_occurrence", "boxplot.unitobs", "freq_occurrence.unitobs",
                       "MRT.unitobs", "MRT.esp", "barplot.unitobs", "barplot.esp")

    ## Étapes sans possibilité d'agrégation par classes de taille :
    nextStepSansCT <- c("")

    ## Étapes avec agrégation par unitobs :
    nextStepUnitobs <- c("boxplot.unitobs", "modele_lineaire.unitobs", ## "freq_occurrence",
                         "freq_occurrence.unitobs",
                         "MRT.unitobs", "barplot.unitobs")

    ## Étapes avec biodiversité (généralement pour les métriques agrégées par unité d'observation) :
    nextStepBiodiv <- c("boxplot.unitobs", "modele_lineaire.unitobs", "MRT.unitobs", "barplot.unitobs")

    ## Le même traitement des variables peut être appliqué pour différents "nextStep"
    ## (Permet de lancer une fonction qui ne correspond pas à nextStep ; cas non rencontré pour l'instant, i.e.
    ## casStep[nextStep] == nextStep) :
    casStep <- c("modele_lineaire"="modele_lineaire",
                 "boxplot.esp"="boxplot.esp",
                 "freq_occurrence"="freq_occurrence",
                 "freq_occurrence.unitobs"="freq_occurrence.unitobs",
                 "boxplot.unitobs"="boxplot.unitobs",
                 "modele_lineaire.unitobs"="modele_lineaire.unitobs",
                 "MRT.unitobs"="MRT.unitobs",
                 "MRT.esp"="MRT.esp",
                 "barplot.unitobs"="barplot.unitobs",
                 "barplot.esp"="barplot.esp")

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

    ## ########################
    ## Éléments graphiques :
    WinSelection <- tktoplevel()          # Fenêtre principale
    tkwm.title(WinSelection,
               paste(mltext("selectionVariables.WT"),
                     titreSelVar.f(type="winTitle", nextStep), sep=""))

    ## Métriques :
    FrameMetrique <- tkframe(WinSelection, borderwidth=2, relief="groove")
    if (is.element(nextStep, nextStepMetriqueFixe))
    {
        switch(nextStep,
               "freq_occurrence"={
                   tclvalue(TableMetrique) <- "TableOccurrences"
                   tclvalue(MetriqueChoisie) <- "freq.occurrence"
               },
               "freq_occurrence.unitobs"={
                   tclvalue(TableMetrique) <- "TableOccurrences"
                   tclvalue(MetriqueChoisie) <- "freq.occurrence"
               })
    }else{                              # Uniquement si choix de métrique possible.
        ## Choix de la métrique :

        CB.metrique <- ttkcombobox(FrameMetrique, value=metriques, textvariable=MetriqueChoisie,
                                   state="readonly")
        RB.unitSpSz <- tkradiobutton(FrameMetrique, variable=TableMetrique,
                                      value="unitSpSz", text=titreSelVar.f(type="tabListespCT", nextStep))
        RB.unitSp <- tkradiobutton(FrameMetrique, variable=TableMetrique,
                                        value="unitSp", text=titreSelVar.f(type="tabListesp", nextStep))
        RB.unit <- tkradiobutton(FrameMetrique, variable=TableMetrique,
                                        value="unit", text="...de biodiversité ( / unité d'observation)")
    }

    ## Choix du facteur de séparation des graphs + modalités :
    FrameFactGraph <- tkframe(WinSelection, borderwidth=2, relief="groove")
    FrameRB <- tkframe(FrameFactGraph)
    FrameGB <- tkframe(FrameFactGraph)

    RB.factGraphRefesp <- tkradiobutton(FrameRB, variable=FactGraphTbl, value="refesp",
                                        text=mltext("selectionVariables.RB.fGraphRefesp"))
    RB.factGraphUnitobs <- tkradiobutton(FrameRB, variable=FactGraphTbl, value="unitobs",
                                         text=mltext("selectionVariables.RB.fGraphUnitobs"))
    CB.factGraph <- ttkcombobox(FrameGB, value="", textvariable=FacteurGraph, state="readonly")
    B.factGraphSel <- tkbutton(FrameGB,
                               text=mltext("selectionVariables.B.fSel"),
                               command=function()
                               {
                                   selectModalites.f(factor=tclvalue(FacteurGraph),
                                                     tableMetrique=tclvalue(TableMetrique),
                                                     env=env, level=0,
                                                     nextStep=nextStep, dataEnv=dataEnv)
                                   winRaise.f(WinSelection)
                               })

    ## Choix des facteurs de regroupement + modalités :
    FrameFact <- tkframe(WinSelection, borderwidth=2, relief="groove")
    CB.fact1 <- ttkcombobox(FrameFact,
                            value=champsReferentiels.f(nomTable=tclvalue(TableMetrique), dataEnv=dataEnv,
                                                       nextStep=nextStep),
                            textvariable=listFacteurs[[1]], state="readonly")

    B.factSel1 <- tkbutton(FrameFact,
                           text=mltext("selectionVariables.B.fSel"),
                           command=function()
                           {
                               selectModalites.f(tclvalue(listFacteurs[[1]]),
                                                 tableMetrique=tclvalue(TableMetrique),
                                                 env=env, level=1,
                                                 nextStep=nextStep, dataEnv=dataEnv)
                               winRaise.f(WinSelection)
                           })

    FrameBT <- tkframe(WinSelection)
    B.OK <- tkbutton(FrameBT, text=mltext("Run.button"),
                     command=function(){tclvalue(Done) <- 1})
    B.Cancel <- tkbutton(FrameBT, text=mltext("Quit.button"),
                         command=function(){tclvalue(Done) <- 2})
    B.optGraph <- tkbutton(FrameBT, text=mltext("GraphOpt.button"),
                           command=function(x)
                       {
                           tuneGraphOptions.f(graphType=switch(nextStep,
                                              "boxplot.esp"=, "boxplot.unitobs"={"boxplot"},
                                              "barplot.unitobs"=, "barplot.esp"={"barplot"},
                                              "freq_occurrence.unitobs"=, "freq_occurrence"={"barplotocc"},
                                              {"none"}))
                           winRaise.f(WinSelection)
                       })

    if (! is.element(nextStep, nextStepGraph))
    {
        tkconfigure(B.optGraph, state="disabled") # Options graphiques uniquement utiles pour des "étapes" graphiques.
    }else{}

    ## ############
    ## Évènements :
    if (! is.element(nextStep, nextStepMetriqueFixe)) # Uniquement si choix de métrique possible.
    {
        tkbind(RB.unitSp, "<Leave>", function(){updateMetrique.f(nomTable=tclvalue(TableMetrique), env=env)})
        tkbind(RB.unitSpSz, "<Leave>", function(){updateMetrique.f(nomTable=tclvalue(TableMetrique), env=env)})
        tkbind(RB.unit, "<Leave>", function(){updateMetrique.f(nomTable=tclvalue(TableMetrique), env=env)})
    }else{}

    tkbind(RB.factGraphUnitobs, "<Leave>", function(){updateFactGraph.f(nomTable=tclvalue(FactGraphTbl), env=env)})
    tkbind(RB.factGraphRefesp, "<Leave>", function(){updateFactGraph.f(nomTable=tclvalue(FactGraphTbl), env=env)})

    tkbind(CB.factGraph, "<FocusIn>", function() {updateFact.f(level=0, env=env)})
    tkbind(CB.fact1, "<FocusIn>", function() {nouvChoixFact.f(level=1, env=env)})

    tkbind(WinSelection, "<Destroy>", function(){tclvalue(Done) <- 2})

    ## #############################
    ## Positionnement des éléments :

    tkgrid(tklabel(WinSelection, text=" "))
    tkgrid(tklabel(FrameMetrique, text=titreSelVar.f(type="metrique", nextStep)), sticky="w")

    if (! is.element(nextStep, nextStepMetriqueFixe)) # Uniquement si choix de métrique possible.
    {
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
            tkgrid(RB.unit, CB.metrique, tklabel(FrameMetrique, text=" \n"), sticky="w")
        }else{
            tkgrid(RB.unitSp, CB.metrique, tklabel(FrameMetrique, text=" \n"), sticky="w")
        }

        ## if (is.element(nextStep, nextStepUnitobs)) # Désactivation temporaire de la biodiversité (en attendant fonctions
        ##                                 # de recalcul).
        ## {
        ##     tkconfigure(RB.unit, state="disabled")
        ## }else{}
    }else{}

    tkgrid(FrameMetrique, column=1, columnspan=3, sticky="w")
    tkgrid(tklabel(WinSelection))

    ## Choix du facteur de séparation des graphiques :
    tkgrid(FrameFactGraph, column=1, columnspan=3, sticky="ew")
    tkgrid(tklabel(FrameFactGraph, text=titreSelVar.f(type="factSep", nextStep)),
           sticky="w", columnspan=2)
    tkgrid(tklabel(FrameGB, text=mltext("selectionVariables.Levels")), column=1)
    tkgrid(CB.factGraph, B.factGraphSel, sticky="n")

    tkgrid(FrameRB, FrameGB, tklabel(FrameFactGraph, text="\n"), sticky="nw")
    tkgrid(RB.factGraphUnitobs, sticky="w")
    tkgrid(RB.factGraphRefesp, sticky="w")
    tkgrid(tklabel(WinSelection), column=4)

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
    tkgrid(FrameFact, column=1, columnspan=3, sticky="w")
    tkgrid(tklabel(FrameFact, text=titreSelVar.f(type="facteurs", nextStep)),
           columnspan=3, sticky="w")

    tkgrid(ttkseparator(FrameFact, orient = "horizontal"), column=0, columnspan=4, sticky="ew")
    tkgrid(tklabel(FrameFact, text=mltext("selectionVariables.Levels")),
           column=2, sticky="w")
    tkgrid(tklabel(FrameFact,
                   text=paste0(mltext("selectionVariables.Factor")," 1 ")),
           CB.fact1, B.factSel1, sticky="n", pady=1)

    tkgrid(tklabel(WinSelection), column=4)

    tkgrid(FrameBT, column=1, columnspan=3)
    tkgrid(B.OK, tklabel(FrameBT, text="      "), B.Cancel,
           tklabel(FrameBT, text="               "), B.optGraph, tklabel(FrameBT, text="\n"))

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

            ## ...Sinon, lancement de l'étape suivante :
            switch(casStep[nextStep],
                   boxplot.esp={

                       ## tkmessageBox(message="BoxPlots")
                       WP2boxplot.f(metrique=tclvalue(MetriqueChoisie),
                                    factGraph=tclvalue(FacteurGraph), factGraphSel=factGraphSel,
                                    listFact=sapply(listFacteurs, tclvalue), listFactSel=listFactSel,
                                    tableMetrique=tclvalue(TableMetrique), dataEnv=dataEnv, baseEnv=baseEnv) # OK
                   },
                   modele_lineaire={

                       ## tkmessageBox(message="Modèles linéaires")
                       modeleLineaireWP2.esp.f(metrique=tclvalue(MetriqueChoisie),
                                               factAna=tclvalue(FacteurGraph), factAnaSel=factGraphSel,
                                               listFact=sapply(listFacteurs, tclvalue), listFactSel=listFactSel,
                                               tableMetrique=tclvalue(TableMetrique),
                                               dataEnv=dataEnv, baseEnv=baseEnv) #
                   },
                   freq_occurrence={
                       barplotOccurrence.f(factGraph=tclvalue(FacteurGraph), factGraphSel=factGraphSel,
                                           listFact=sapply(listFacteurs, tclvalue), listFactSel=listFactSel,
                                           dataEnv=dataEnv, baseEnv=baseEnv)
                   },
                   freq_occurrence.unitobs={
                       barplotOccurrence.unitobs.f(factGraph=tclvalue(FacteurGraph), factGraphSel=factGraphSel,
                                                   listFact=sapply(listFacteurs, tclvalue), listFactSel=listFactSel,
                                                   dataEnv=dataEnv, baseEnv=baseEnv) #
                   },
                   boxplot.unitobs={
                       WP2boxplot.unitobs.f(metrique=tclvalue(MetriqueChoisie),
                                            factGraph=tclvalue(FacteurGraph), factGraphSel=factGraphSel,
                                            listFact=sapply(listFacteurs, tclvalue), listFactSel=listFactSel,
                                            tableMetrique=tclvalue(TableMetrique),
                                            dataEnv=dataEnv, baseEnv=baseEnv) # OK
                   },
                   modele_lineaire.unitobs={
                        modeleLineaireWP2.unitobs.f(metrique=tclvalue(MetriqueChoisie),
                                                    factAna=tclvalue(FacteurGraph), factAnaSel=factGraphSel,
                                                    listFact=sapply(listFacteurs, tclvalue), listFactSel=listFactSel,
                                                    tableMetrique=tclvalue(TableMetrique),
                                                    dataEnv=dataEnv, baseEnv=baseEnv) #
                   },
                   MRT.unitobs={
                       WP2MRT.unitobs.f(metrique=tclvalue(MetriqueChoisie),
                                        factGraph=tclvalue(FacteurGraph), factGraphSel=factGraphSel,
                                        listFact=sapply(listFacteurs, tclvalue), listFactSel=listFactSel,
                                        tableMetrique=tclvalue(TableMetrique), dataEnv=dataEnv, baseEnv=baseEnv) #
                   },
                   MRT.esp={

                       ## tkmessageBox(message="BoxPlots")
                       WP2MRT.esp.f(metrique=tclvalue(MetriqueChoisie),
                                    factGraph=tclvalue(FacteurGraph), factGraphSel=factGraphSel,
                                    listFact=sapply(listFacteurs, tclvalue), listFactSel=listFactSel,
                                    tableMetrique=tclvalue(TableMetrique), dataEnv=dataEnv, baseEnv=baseEnv) #
                   },
                   barplot.esp={

                       ## tkmessageBox(message="BarPlots")
                       WP2barplot.esp.f(metrique=tclvalue(MetriqueChoisie),
                                    factGraph=tclvalue(FacteurGraph), factGraphSel=factGraphSel,
                                    listFact=sapply(listFacteurs, tclvalue), listFactSel=listFactSel,
                                    tableMetrique=tclvalue(TableMetrique), dataEnv=dataEnv, baseEnv=baseEnv) #
                   },
                   barplot.unitobs={
                       WP2barplot.unitobs.f(metrique=tclvalue(MetriqueChoisie),
                                            factGraph=tclvalue(FacteurGraph), factGraphSel=factGraphSel,
                                            listFact=sapply(listFacteurs, tclvalue), listFactSel=listFactSel,
                                            tableMetrique=tclvalue(TableMetrique), dataEnv=dataEnv, baseEnv=baseEnv) #
                   },
                   tkmessageBox(message=paste("Nothing to do (option '", nextStep, "' not implemented).", sep=""),
                                icon="warning"))

            winRaise.f(WinSelection)

            ## Fenêtre de sélection ramenée au premier plan une fois l'étape finie :
            ## winSmartPlace.f(WinSelection)
        }
        if (tclvalue(Done) == "2") {break()} # statut d'exécution 'abandon' : on sort de la boucle.
    }


    tkdestroy(WinSelection)             # destruction de la fenêtre.
}




### Local Variables:
### ispell-local-dictionary: "english"
### fill-column: 120
### End:

