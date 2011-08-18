#-*- coding: latin-1 -*-

### File: barplots_occurrence.R
### Time-stamp: <2011-06-23 11:42:43 yreecht>
###
### Author: Yves Reecht
###
####################################################################################################
### Description:
###
### Fonctions pour créer des représentations (barplots) des fréquences d'occurrences relatives (%)
### (utilise certaines fonctions de ./boxplot_generique_calc.R)
####################################################################################################

barplotOccurrence.f <- function(factGraph, factGraphSel, listFact, listFactSel)
{
    ## Purpose: création des barplots d'après les sélections de facteurs et
    ##          modalités.
    ## ----------------------------------------------------------------------
    ## Arguments: factGraph : le facteur de séparation des graphiques.
    ##            factGraphSel : la sélection de modalités pour ce dernier
    ##            listFact : liste du (des) facteur(s) de regroupement
    ##            listFactSel : liste des modalités sélectionnées pour ce(s)
    ##                          dernier(s)
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 14 oct. 2010, 10:51

    metrique <- "freq.occurrence"

    ## Nettoyage des facteurs (l'interface de sélection produit des valeurs vides) :
    listFactSel <- listFactSel[unlist(listFact) != ""]
    listFactSel <- listFactSel[length(listFactSel):1]

    listFact <- listFact[unlist(listFact) != ""]
    listFact <- listFact[length(listFact):1]

    ## Concaténation
    facteurs <- c(factGraph, unlist(listFact)) # Concaténation des facteurs

    selections <- c(list(factGraphSel), listFactSel) # Concaténation des leurs listes de modalités sélectionnées

    ## Données pour la série de boxplots :
    tmpData <- subsetToutesTables.f(metrique="pres_abs", facteurs=facteurs, selections=selections,
                                    tableMetrique="TablePresAbs", exclude = NULL)

    ## Identification des différents graphiques à générer:
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


    ## Sauvegarde temporaire des données utilisées pour les graphiques (attention : écrasée à chaque nouvelle série de
    ## graphiques) :
    DataBackup <<- list()

    ## ###############################################################
    ## Boucle de création des graphiques (par facteur de séparation) :
    for (modGraphSel in iFactGraphSel)
    {
        ## Option graphique :
        cex <- getOption("P.cex")

        ## Préparation des données pour un graphique :
        if (modGraphSel == "")          # ...si pas de facteur de séparation des graphiques
        {
            tmpDataMod <- tmpData
        }else{                          # ...sinon.
            tmpDataMod <- subset(tmpData, tmpData[ , factGraph] == modGraphSel) # Subset des données pour la modalité.
        }

        ## Passage au graphique suivant si le nombre d'observations  < au minimum défini dans les options.
        if (dim(tmpDataMod)[1] < getOption("P.MinNbObs"))
        {
            warning("Nombre d'observations pour ", modGraphSel, " < ", getOption("P.MinNbObs"),
                    " : Graphique non créé !\n")
            next()
        }else{}

        ## Suppression des 'levels' non utilisés :
        tmpDataMod <- dropLevels.f(tmpDataMod)

        ## Sauvegarde temporaire des données :
        DataBackup[[modGraphSel]] <<- tmpDataMod

        ## Ouverture et configuration du périphérique graphique :
        wmfFile <- openDevice.f(noGraph=which(modGraphSel == iFactGraphSel),
                                metrique=metrique,
                                factGraph=factGraph,
                                modSel=if (getOption("P.plusieursGraphPage"))
                            {
                                iFactGraphSel      # toutes les modalités.
                            }else{
                                modGraphSel        # la modalité courante uniquement.
                            },
                                listFact=listFact,
                                type="espece",
                                typeGraph="barplot")

        ## Titre (d'après les métriques, modalité du facteur de séparation et facteurs de regroupement) :
        if (! isTRUE(getOption("P.graphPaper")))
        {
            mainTitle <- graphTitle.f(metrique=metrique,
                                      modGraphSel=modGraphSel,
                                      factGraph=factGraph,
                                      listFact=listFact,
                                      type="espece")
        }else{
            mainTitle <- NULL
        }

        ## Calcul des fréquences :
        heights <- with(tmpDataMod,
                        tapply(pres_abs, lapply(listFact, function(y)eval(parse(text=y))),
                               function(x)
                           {
                               100 * sum(x, na.rm=TRUE) / length(na.omit(x))
                           }))

        ## Paramètres graphiques :
        par(mar=c(4.5,
            ## Marge de gauche dynamique :
            tmp2 <- ifelse((tmp <- 2.1 +
                            ifelse(isTRUE(getOption("P.graphPNG")), # Coef différent pour les PNGs.
                                   75,
                                   100)*
                            max(strDimRotation.f(as.graphicsAnnot(pretty(range(heights, na.rm=TRUE))),
                                                 srt=0,
                                                 unit="figure",
                                                 cex=cex)$width, na.rm=TRUE)) > 11,
                           11,
                           tmp),
            ## Marge supérieure augmentée s'il y a un titre :
            ifelse(isTRUE(getOption("P.graphPaper")) , 3, 8),
            7) + 0.1,
            ## Distance du nom d'axe dépendante de la taille de marge gauche :
            mgp=c(tmp2 - 1.4, 0.9, 0))

        ## Label axe y :
        ylab <- parse(text=paste("'", Capitalize.f(varNames[metrique, "nom"]), "'",
                      ifelse(varNames[metrique, "unite"] != "",
                             paste("~~(", varNames[metrique, "unite"], ")", sep=""),
                             ""),
                      sep=""))

        barPlotTmp <- barplot(heights,
                              beside=TRUE,
                              main=mainTitle,
                              xlab="",
                              ylab=ylab,
                              ylim=c(0, 1.1 * max(heights, na.rm=TRUE)),
                              las=1,
                              col=.ColorPalette(nrow(heights)),
                              cex.lab=cex,
                              cex.axis=cex,
                              legend.text=ifelse(length(listFact) > 1, TRUE, FALSE),
                              args.legend=list("x"="topright", "inset"=-0.08, "xpd"=NA,
                                               "title"=Capitalize.f(varNames[listFact[1], "nom"])))

        mtext(Capitalize.f(varNames[tail(listFact, 1), "nom"]),
              side=1, line=2.3, cex=cex)

        if (getOption("P.NbObs"))
        {
            ## Nombre d'"observations" :
            nbObs <- with(tmpDataMod,
                          tapply(pres_abs,
                                 lapply(listFact, function(y)eval(parse(text=y))),
                                 function(x)
                             {
                                 length(na.omit(x))
                             }))

            ## Nombres sur l'axe supérieur :
            mtext(nbObs, side=3, at=barPlotTmp, las=2, col=getOption("P.NbObsCol"),
                  adj=-0.2)

            legend(x="topleft",
                   legend=expression("Nombre d'observations ("=="nb unités d'observation x nb espèces)"),
                   cex =0.9, col=getOption("P.NbObsCol"), text.col="orange", merge=FALSE)

        }else{}

        ## On ferme les périphériques PNG en mode fichier individuel :
        if (isTRUE(getOption("P.graphPNG")) &&
            (! getOption("P.plusieursGraphPage") || length(iFactGraphSel) <= 1))
        {
            dev.off()
        }else{}

        ## Sauvegarde en wmf si pertinent et souhaité :
        if (.Platform$OS.type == "windows" && isTRUE(getOption("P.graphWMF")) &&
            (! getOption("P.plusieursGraphPage") || length(iFactGraphSel) <= 1))
        {
            savePlot(wmfFile, type="wmf", device=dev.cur())
        }else{}

    }                                   # Fin de boucle graphique

    ## On ferme les périphériques PDF ou PNG restants :
    if (getOption("P.graphPDF") ||
        (isTRUE(getOption("P.graphPNG")) && getOption("P.plusieursGraphPage") && length(iFactGraphSel) > 1))
    {
        dev.off()
    }else{}

    ## Sauvegarde en wmf restants si pertinent et souhaité :
    if (.Platform$OS.type == "windows" && isTRUE(getOption("P.graphWMF")) &&
        getOption("P.plusieursGraphPage") && length(iFactGraphSel) > 1)
    {
        savePlot(wmfFile, type="wmf", device=dev.cur())
    }else{}
}








### Local Variables:
### ispell-local-dictionary: "english"
### fill-column: 120
### End:
