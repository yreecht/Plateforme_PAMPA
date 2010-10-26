#-*- coding: latin-1 -*-

### File: boxplots_ttesp_generic.R
### Time-stamp: <2010-10-25 11:47:19 yreecht>
###
### Author: Yves Reecht
###
####################################################################################################
### Description:
###
###
####################################################################################################


########################################################################################################################
graphTitle.unitobs.f <- function(metrique, modGraphSel, factGraph, listFact, model=NULL)
{
    ## Purpose:
    ## ----------------------------------------------------------------------
    ## Arguments:
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 14 oct. 2010, 15:44

    return(paste(ifelse(is.null(model),
                        "valeurs de ",
                        paste(model, " pour ", varNames[metrique, "article"], sep="")),
                 varNames[metrique, "nom"], " agrégé",
                 switch(varNames[metrique, "genre"], # Accord de "agrégé".
                        f="e", fp="es", mp="s", ""),
                 " par unité d'observation",
                 ifelse(modGraphSel[1] == "", # Facteur de séparation uniquement si défini.
                        "\npour toutes les espèces",
                        paste("\npour les espèces correspondant à '", factGraph, "' = (",
                              paste(modGraphSel, collapse=", "), ")", sep="")),
                 "\n selon ",
                 paste(sapply(listFact[length(listFact):1],
                              function(x)paste(varNames[x, c("article", "nom")], collapse="")),
                       collapse=" et "),
                 "\n", sep=""))
}


########################################################################################################################
WP2boxplot.unitobs.f <- function(metrique, factGraph, factGraphSel, listFact, listFactSel, tableMetrique)
{
    ## Purpose: Produire les boxplots en tenant compte des options graphiques
    ## ----------------------------------------------------------------------
    ## Arguments: metrique : la métrique choisie.
    ##            factGraph : le facteur de séparation des graphiques.
    ##            factGraphSel : la sélection de modalités pour ce dernier
    ##            listFact : liste du (des) facteur(s) de regroupement
    ##            listFactSel : liste des modalités sélectionnées pour ce(s)
    ##                          dernier(s)
    ##            tableMetrique : nom de la table de métriques.
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date:  6 août 2010, 16:34

    pampaProfilingStart.f()

    ## Nettoyage des facteurs (l'interface de sélection produit des valeurs vides) :
    listFactSel <- listFactSel[unlist(listFact) != ""]
    listFactSel <- listFactSel[length(listFactSel):1]

    listFact <- listFact[unlist(listFact) != ""]
    listFact <- listFact[length(listFact):1]

    ## Concaténation
    facteurs <- c(factGraph, unlist(listFact)) # Concaténation des facteurs

    selections <- c(list(factGraphSel), listFactSel) # Concaténation des leurs listes de modalités sélectionnées

    ## Données pour la série de boxplots :
    tmpData <- subsetToutesTables.f(metrique=metrique, facteurs=facteurs,
                                    selections=selections, tableMetrique=tableMetrique,
                                    exclude = NULL, add=c("unite_observation", "code_espece"))

    ## Formule du boxplot
    exprBP <- eval(parse(text=paste(metrique, "~", paste(listFact, collapse=" + "))))

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

    ## Agrégation des observations / unité d'observation :
    tmpData <- agregationTableParCritere.f(Data=tmpData,
                                           metrique=metrique,
                                           facteurs=c("unite_observation"),
                                           listFact=listFact)

    ## Sauvegarde temporaire des données utilisées pour les graphiques (attention : écrasée à chaque nouvelle série de
    ## graphiques) :
    DataBackup <<- list(tmpData)

    ## Passage au graphique suivant si le nombre d'observations  < au minimum défini dans les options.
    if (nrow(tmpData) < getOption("P.MinNbObs"))
    {
        warning("Nombre d'observations pour (", paste(iFactGraphSel, collapse=", "), ") < ", getOption("P.MinNbObs"),
                " : Graphique non créé !\n")
    }else{

        ## Suppression des valeurs supérieures à X% du maximum (pour plus de lisibilité) :
        if (getOption("P.maxExclu"))
        {
            tmpData <- tmpData[which(tmpData[, metrique] <=
                                        getOption("P.GraphPartMax") * max(tmpData[, metrique],
                                                                          na.rm=TRUE)), ]
        }else{}

        ## Suppression des 'levels' non utilisés :
        tmpData <- dropLevels.f(tmpData)

        ## Ouverture et configuration du périphérique graphique :
        openDevice.f(noGraph=1,
                     metrique=metrique, factGraph=factGraph, listFact=listFact)

        par(mar=c(9, 5, 8, 1), mgp=c(3.5, 1, 0)) # paramètres graphiques.

        ## Titre (d'après les métriques, modalité du facteur de séparation et facteurs de regroupement) :
        mainTitle <- graphTitle.unitobs.f(metrique=metrique,
                                          modGraphSel=iFactGraphSel, factGraph=factGraph,
                                          listFact=listFact)

        ## Les couleurs pour l'identification des modalités du facteur de second niveau :
        colors <- colBoxplot.f(terms=attr(terms(exprBP), "term.labels"), data=tmpData)

        ## Label axe y :
        ylab <- parse(text=paste("'", Capitalize.f(varNames[metrique, "nom"]), "'",
                      ifelse(varNames[metrique, "unite"] != "",
                             paste("~~(", varNames[metrique, "unite"], ")", sep=""),
                             ""),
                      sep=""))

        ## Boxplot !
        tmpBP <- boxplot(exprBP, data=tmpData,
                         main=mainTitle, ylab=ylab,  ## Capitalize.f(varNames[metrique, "nom"]),
                         varwidth = TRUE, las=2,
                         col=colors)

        ## #################### Informations supplémentaires sur les graphiques ####################

        ## Label si un seul groupe :
        if (length(tmpBP$names) == 1)
        {
            axis(1, at=1, labels=tmpBP$names)
        }else{}

        ## Séparateurs de facteur de premier niveau :
        if (getOption("P.sepGroupes"))
        {
            sepBoxplot.f(terms=attr(terms(exprBP), "term.labels"), data=tmpData)
        }

        ## Séparateurs par défaut :
        abline(v = 0.5+(0:length(tmpBP$names)) , col = "lightgray", lty = "dotted") # Séparations.

        ## Légende des couleurs (facteur de second niveau) :
        if (getOption("P.legendeCouleurs"))
        {
            legendBoxplot.f(terms=attr(terms(exprBP), "term.labels"), data=tmpData)
        }else{}

        ## Moyennes :
        Moyenne <- as.vector(tapply(X=tmpData[, metrique], # moyenne par groupe.
                                    INDEX=as.list(tmpData[ , attr(terms(exprBP),
                                                                     "term.labels"), drop=FALSE]),
                                    FUN=mean, na.rm=TRUE))

        ## ... points :
        if (getOption("P.pointMoyenne"))
        {
            points(Moyenne, pch=19, col=getOption("P.pointMoyenneCol"))
        }else{}

        ## ... valeurs :
        if (getOption("P.valMoyenne"))
        {
            text(1.2 * Moyenne, col = getOption("P.valMoyenneCol"), cex = 0.9, # On pourrait utiliser
                                        # tmpBP$stats[5, ] à la place ?
                 labels=as.character(round(Moyenne, digits=unlist(options("P.NbDecimal")))))
        }else{}

        ## Nombres d'observations :
        if (getOption("P.NbObs"))
        {
            nbObs <- tmpBP$n # Retourné par la fonction 'boxplot'

            ## Nombres sur l'axe supérieur :
            axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)),
                 col.ticks="orange", col.axis = "orange", lty = 2, lwd = 0.5)

            legend("topleft", "Nombre d'enregistrements par boite à moustache",
                   cex =0.9, col=getOption("P.NbObsCol"), text.col="orange", merge=FALSE)
        }else{}

        ## Affichage d'avertissement pour  > X% du max retiré :
        if (getOption("P.maxExclu"))
        {
            legend("top",
                   paste("Enregistrements > ", 100 * getOption("P.GraphPartMax"), "% du maximum retirés", sep=""),
                   cex =0.9, col="red", text.col="red", merge=FALSE)
        }else{}
    }  ## Fin de graphique.

    ## On ferme les périphériques PDF :
    if (getOption("P.graphPDF"))
    {
        dev.off()
    }else{}

    pampaProfilingEnd.f()
}






### Local Variables:
### ispell-local-dictionary: "english"
### fill-column: 120
### End:
