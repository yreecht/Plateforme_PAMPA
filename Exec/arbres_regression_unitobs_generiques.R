#-*- coding: latin-1 -*-

### File: arbres_regression_unitobs_generiques.R
### Time-stamp: <2011-09-01 15:23:14 yreecht>
###
### Author: Yves Reecht
###
####################################################################################################
### Description:
###
### Fonctions de création de boxplots pour les métriques agrégées / unité d'observation.
####################################################################################################

########################################################################################################################
WP2MRT.unitobs.f <- function(metrique, factGraph, factGraphSel, listFact, listFactSel, tableMetrique)
{
    ## Purpose:
    ## ----------------------------------------------------------------------
    ## Arguments: metrique : la métrique choisie.
    ##            factGraph : le facteur de sélection des espèces.
    ##            factGraphSel : la sélection de modalités pour ce dernier
    ##            listFact : liste du (des) facteur(s) de regroupement
    ##            listFactSel : liste des modalités sélectionnées pour ce(s)
    ##                          dernier(s)
    ##            tableMetrique : nom de la table de métriques.
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 10 mai 2011, 14:24

    pampaProfilingStart.f()

    ## Nettoyage des facteurs (l'interface de sélection produit des valeurs vides) :
    listFactSel <- listFactSel[unlist(listFact) != ""]
    listFactSel <- listFactSel[length(listFactSel):1]

    listFact <- listFact[unlist(listFact) != ""]
    listFact <- listFact[length(listFact):1]

    ## Concaténation
    facteurs <- c(factGraph, unlist(listFact)) # Concaténation des facteurs

    selections <- c(list(factGraphSel), listFactSel) # Concaténation des leurs listes de modalités sélectionnées

    ## Données pour la série de MRT :
    if (tableMetrique == "TableBiodiv")
    {
        ## Pour les indices de biodiversité, il faut travailler sur les nombres... :
        tmpData <- subsetToutesTables.f(metrique="nombre", facteurs=facteurs,
                                        selections=selections, tableMetrique="listespunit",
                                        exclude = NULL, add=c("unite_observation", "code_espece"))
    }else{
        ## ...sinon sur la métrique choisie :
        tmpData <- subsetToutesTables.f(metrique=metrique, facteurs=facteurs,
                                        selections=selections, tableMetrique=tableMetrique,
                                        exclude = NULL, add=c("unite_observation", "code_espece"))
    }

    ## Formule du MRT :
    exprMRT <- eval(parse(text=paste(metrique, "~", paste(listFact, collapse=" + "))))

    ## Identification des différents modalités (espèces) du graphique à générer :
    if (factGraph == "")                # Pas de facteur de séparation des graphiques.
    {
        iFactGraphSel <- ""
    }else{
        if (is.na(factGraphSel[1]))            # Toutes les modalités.
        {
            iFactGraphSel <- unique(as.character(sort(tmpData[ , factGraph])))
        }else{                              # Modalités sélectionnées (et présentes parmi les données retenues).
            iFactGraphSel <- factGraphSel[is.element(factGraphSel, tmpData[ , factGraph])]
        }
    }

    ## Agrégation des observations / unité d'observation :
    if (tableMetrique == "unitespta" && factGraph != "classe_taille")
    {
        tmpData <- na.omit(agregationTableParCritere.f(Data=tmpData,
                                                       metrique=metrique,
                                                       facteurs=c("unite_observation", "classe_taille"),
                                                       listFact=listFact))
    }else{
        if (tableMetrique == "TableBiodiv")
        {
            ## Calcul des indices de biodiversité sur sélection d'espèces :
            tmp <- calcBiodiv.f(Data=tmpData,
                                unitobs = "unite_observation", code.especes = "code_espece",
                                nombres = "nombre",
                                indices=metrique)

            ## On rajoute les anciennes colonnes :
            tmpData <- cbind(tmp,
                             tmpData[match(tmp$unite_observation, tmpData$unite_observation),
                                     !is.element(colnames(tmpData), colnames(tmp))])
        }else{
            tmpData <- na.omit(agregationTableParCritere.f(Data=tmpData,
                                                           metrique=metrique,
                                                           facteurs=c("unite_observation"),
                                                           listFact=listFact))
        }
    }

    ## Sauvegarde temporaire des données utilisées pour les graphiques (attention : écrasée à chaque nouvelle série de
    ## graphiques/analyses) :
    DataBackup <<- list(tmpData)

    ## Suppression des 'levels' non utilisés :
    tmpData <- dropLevels.f(tmpData)

    ## Ouverture et configuration du périphérique graphique :
    graphFile <- openDevice.f(noGraph=1,
                              metrique=metrique,
                              factGraph=factGraph,
                              modSel=iFactGraphSel,
                              listFact=listFact,
                              type=ifelse(tableMetrique == "unitespta" && factGraph != "classe_taille",
                                          "CL_unitobs",
                                          "unitobs"),
                              typeGraph="MRT",
                              large=TRUE)

    par(mar=c(1.5, 7, 7, 7), mgp=c(3.5, 1, 0)) # paramètres graphiques.

    ## Titre (d'après les métriques, modalité du facteur de séparation et facteurs de regroupement) :
    mainTitle <- graphTitle.f(metrique=metrique,
                              modGraphSel=iFactGraphSel,
                              factGraph=factGraph,
                              listFact=listFact,
                              type=ifelse(tableMetrique == "unitespta" && factGraph != "classe_taille",
                                          "CL_unitobs",
                                          ifelse(tableMetrique == "unitespta",
                                                 "unitobs(CL)",
                                                 "unitobs")),
                              model="Arbre de régression multivariée")

    tmpMRT <- rpart(exprMRT, data=tmpData)

    plot(tmpMRT, main=mainTitle)
    text.rpart.new(tmpMRT, use.n=TRUE, pretty=0, all=TRUE, xpd=NA)

    ## Écriture des résultats formatés dans un fichier :
    tryCatch(sortiesMRT.f(objMRT=tmpMRT, formule=exprMRT,
                          metrique=metrique,
                          factAna=factGraph, modSel=iFactGraphSel, listFact=listFact,
                          Data=tmpData,
                          type=ifelse(tableMetrique == "unitespta" && factGraph != "classe_taille",
                                      "CL_unitobs",
                                      "unitobs")),
             error=errorLog.f)


    ## On ferme les périphériques PDF :
    if (getOption("P.graphPDF") || isTRUE(getOption("P.graphPNG")))
    {
        dev.off()

        ## Inclusion des fontes dans le pdf si souhaité :
        if (getOption("P.graphPDF") && getOption("P.pdfEmbedFonts"))
        {
            embedFonts(file=graphFile)
        }else{}
    }else{
        if (.Platform$OS.type == "windows" && isTRUE(getOption("P.graphWMF")))
        {
            ## Sauvegarde en wmf si pertinent et souhaité :
            savePlot(graphFile, type="wmf", device=dev.cur())
        }else{}
    }

    pampaProfilingEnd.f()
}








### Local Variables:
### ispell-local-dictionary: "english"
### fill-column: 120
### End:
