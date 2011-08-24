#-*- coding: latin-1 -*-

### File: barplots_occurrence.R
### Time-stamp: <2011-06-23 11:42:19 yreecht>
###
### Author: Yves Reecht
###
####################################################################################################
### Description:
###
### Fonctions pour créer des représentations (barplots) des fréquences d'occurrences relatives (%)
### (utilise certaines fonctions de ./boxplot_generique_calc.R)
####################################################################################################

barplotOccurrence.unitobs.f <- function(factGraph, factGraphSel, listFact, listFactSel)
{
    ## Purpose: création d'un barplot d'après les sélections de facteurs et
    ##          modalités, avec les présences/absences agrégées par unitobs.
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


    ## Identification des différents modalités (espèces) du graphique à générer :
    if (factGraph == "")                # Pas de facteur.
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
    tmpData <- na.omit(agregationTableParCritere.f(Data=tmpData,
                                                   metrique="pres_abs",
                                                   facteurs=c("unite_observation"),
                                                   listFact=listFact))

    ## Sauvegarde temporaire des données utilisées pour les graphiques (attention : écrasée à chaque nouvelle série de
    ## graphiques) :
    DataBackup <<- list(tmpData)

    ## ###############################################################
    ## Création du graphique si le nombre d'observations  < au minimum défini dans les options :
    if (dim(tmpData)[1] < getOption("P.MinNbObs"))
    {
        warning("Nombre d'observations pour (", paste(iFactGraphSel, collapse=", "), ") < ", getOption("P.MinNbObs"),
                " : Graphique non créé !\n")
    }else{
        ## Option graphique :
        cex <- getOption("P.cex")

        ## Suppression des 'levels' non utilisés :
        tmpData <- dropLevels.f(tmpData)

        ## Ouverture et configuration du périphérique graphique :
        wmfFile <- openDevice.f(noGraph=1,
                                metrique=metrique,
                                factGraph=factGraph,
                                modSel=iFactGraphSel,
                                listFact=listFact,
                                type="unitobs",
                                typeGraph="barplot")

        ## Titre (d'après les métriques, modalité du facteur de séparation et facteurs de regroupement) :
        if (! isTRUE(getOption("P.graphPaper")))
        {
            mainTitle <- graphTitle.f(metrique=metrique,
                                      modGraphSel=iFactGraphSel,
                                      factGraph=factGraph,
                                      listFact=listFact,
                                      type="unitobs")
        }else{
            mainTitle <- NULL
        }

        ## Calcul des fréquences :
        heights <- with(tmpData,
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
            nbObs <- with(tmpData,
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
                   legend=expression("Nombre d'unités d'observation"),
                   cex =0.9, col=getOption("P.NbObsCol"), text.col="orange", merge=FALSE)

        }else{}

    }                                   # Fin de boucle graphique

    ## On ferme les périphériques PDF :
    if (getOption("P.graphPDF") || isTRUE(getOption("P.graphPNG")))
    {
        dev.off()
    }else{
        if (.Platform$OS.type == "windows" && isTRUE(getOption("P.graphWMF")))
        {
            ## Sauvegarde en wmf si pertinent et souhaité :
            savePlot(wmfFile, type="wmf", device=dev.cur())
        }else{}
    }
}








### Local Variables:
### ispell-local-dictionary: "english"
### fill-column: 120
### End:
