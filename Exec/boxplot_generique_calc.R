#-*- coding: latin-1 -*-

### File: Boxplot_generique_calc.R
### Time-stamp: <2010-10-06 13:52:10 yreecht>
###
### Author: Yves Reecht
###
####################################################################################################
### Description:
###
### Fonctions de traitement des données et des graphiques pour la création de boxplots "à la carte".
####################################################################################################

dropLevels.f <- function(df)
{
    ## Purpose: Supprimer les 'levels' non utilisés des facteurs d'une
    ##          data.frame.
    ## ----------------------------------------------------------------------
    ## Arguments: df : une data.frame
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 10 août 2010, 13:29

    if (class(df) != "data.frame")
    {
        stop("'x' doit être une data.frame")
    }else{
        x <- as.data.frame(sapply(df, function(x)
                              {
                                  return(x[ ,drop=TRUE])
                              }, simplify=FALSE),
                           stringsAsFactors=FALSE)
        return(x)
    }
}

########################################################################################################################
Capitalize.f <- function(x, words=FALSE)
{
    ## Purpose: Mettre en majuscule la première lettre de chaque mot
    ## ----------------------------------------------------------------------
    ## Arguments: x : une chaîne de caractères
    ##            words : tous les mots (TRUE), ou juste le premier.
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date:  9 août 2010, 21:08

    if (words)
    {
        s <- strsplit(x, " ")[[1]]
    }else{
        s <- x
    }

    return(paste(toupper(substring(s, 1,1)), substring(s, 2),
                 sep="", collapse=" "))
}


########################################################################################################################
sepBoxplot.f <- function(terms, data)
{
    ## Purpose: Calculer les positions des séparateurs (facteur de premier
    ##          niveau).
    ## ----------------------------------------------------------------------
    ## Arguments: terms : les termes de l'expression (facteurs ; chaîne de
    ##                    caractères)
    ##            data : le jeu de données (data.frame)
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 10 août 2010, 14:11

    if (length(terms) < 2)
    {
        ## message("Séparateurs inutiles pour moins de deux facteurs")
    }else{
        n <- length(terms)
        ## Positions :
        pos <- seq(from=0.5,
                   by=prod(sapply(data[ , terms[1:(n-1)], drop=FALSE],
                                  function(x){length(unique(na.omit(x)))})),
                   length.out=length(unique(na.omit(data[ , terms[n]]))) + 1)
        ## Lignes verticales :
        abline(v=pos,
               col=rep(getOption("P.sepGroupesCol"), length(pos)),
               lty=rep(1, length(pos)))
    }
}


########################################################################################################################
colBoxplot.f <- function(terms, data)
{
    ## Purpose: Définitions des couleurs pour le facteur de second
    ##          niveau
    ## ----------------------------------------------------------------------
    ## Arguments: terms : les termes de l'expression (facteurs ; chaîne de
    ##                    caractères)
    ##            data : le jeu de données (data.frame)
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 10 août 2010, 15:05

    if (length(terms) < 2)
    {
        ## message("Couleurs inutiles pour moins de deux facteurs")
        return(NULL)
    }else{
        n <- length(terms)

        ## Définition des couleurs :
        col <- rep(heat.colors(n=length(unique(na.omit(data[ , terms[n - 1]])))),
                   each=ifelse(n == 2,
                               1,            # Pas de facteur imbriqué.
                               prod(sapply(data[ , terms[1:(n-2)], drop=FALSE], # nombres de niveaux du (des) facteur(s)
                                           function(x){length(unique(na.omit(x)))})))) # imbriqués.
        return(col)
    }
}

########################################################################################################################
openDevice.f <- function(noGraph, metrique, factGraph, listFact)
{
    ## Purpose: Ouvrir les périphériques graphiques avec les bonnes options
    ## ----------------------------------------------------------------------
    ## Arguments: noGraph : le numéro de graphique (integer)
    ##            metrique : la métrique choisie.
    ##            factGraph : le facteur de séparation des graphiques.
    ##            listFact : liste du (des) facteur(s) de regroupement
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 12 août 2010, 14:54


    if (!getOption("P.graphPDF")) # sorties graphiques à l'écran.
    {
        if (getOption("P.plusieursGraphPage"))     # Plusieurs graphs par page...
        {
            if ((noGraph %% # ...et page remplie.
                 (getOption("P.nrowGraph") * getOption("P.ncolGraph"))) == 1)
            {
                print(paste("Fenêtre", noGraph))
                X11(width=50, height=20, pointsize=10)
                par(mfrow=c(getOption("P.nrowGraph"), getOption("P.ncolGraph")))
            }else{}
        }else{                      # Pas plusieurs graphs par page.
            X11(width=50, height=20, pointsize=10)
        }
    }else{ ## Sorties graphiques en pdf :
        if (noGraph == 1)
        {
            ## Nom de fichier de fichier :
            if (getOption("P.PDFunFichierPage")) # Un fichier par graphique avec numéro.
            {
                pdfFileName <- paste(nameWorkspace, "/FichiersSortie/",
                                     metrique, "_", factGraph, "_", paste(listFact, collapse="-"), "-%03d.pdf", sep="")
                onefile <- FALSE

            }else{                          # Tous les graphiques dans des pages séparées d'un même fichier.
                pdfFileName <- paste(nameWorkspace, "/FichiersSortie/",
                                     metrique, "_", factGraph, "_", paste(listFact, collapse="-"), ".pdf", sep="")
                onefile <- TRUE
            }
            ## Ouverture de fichier :
            pdf(pdfFileName, encoding="ISOLatin1", family="URWHelvetica", onefile=onefile,
                width=20, height=12, pointsize=14)

            ## Si plusieurs graphiques par page :
            if (getOption("P.plusieursGraphPage"))
            {
                par(mfrow=c(getOption("P.nrowGraph"), getOption("P.ncolGraph")))
            }else{}
        }else{}
    }
}


########################################################################################################################
legendBoxplot.f <- function(terms, data)
{
    ## Purpose: Afficher la légende des couleurs (facteur de second
    ##          niveau)
    ## ----------------------------------------------------------------------
    ## Arguments: terms : les termes de l'expression (facteurs ; chaîne de
    ##                    caractères)
    ##            data : le jeu de données (data.frame)
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 10 août 2010, 16:42
    if (length(terms) < 2)
    {
        ## message("Légende des couleurs inutile pour moins de deux facteurs")
    }else{
        n <- length(terms)

        ## Couleurs :
        colors <- unique(colBoxplot.f(terms=terms, data=data))

        ## Noms :
        names <- levels(as.factor(data[ , terms[n - 1]]))

        ## Légende :
        legend("topright", names, col=colors,
               pch = 15, cex =0.9, title=varNames[terms[n - 1], "nom"])
    }
}


########################################################################################################################
WP2boxplot.f <- function(metrique, factGraph, factGraphSel, listFact, listFactSel, tableMetrique)
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

    ## Nettoyage des facteurs (l'interface de sélection produit des valeurs vides) :
    listFactSel <- listFactSel[unlist(listFact) != ""]
    listFactSel <- listFactSel[length(listFactSel):1]

    listFact <- listFact[unlist(listFact) != ""]
    listFact <- listFact[length(listFact):1]

    ## Concaténation
    facteurs <- c(factGraph, unlist(listFact)) # Concaténation des facteurs

    selections <- c(list(factGraphSel), listFactSel) # Concaténation des leurs listes de modalités sélectionnées

    ## Données pour la série de boxplots :
    tmpData <- subsetToutesTables.f(metrique=metrique, facteurs=facteurs, selections=selections,
                                    tableMetrique=tableMetrique, exclude = NULL)

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


    ## Sauvegarde temporaire des données utilisées pour les graphiques (attention : écrasée à chaque nouvelle série de
    ## graphiques) :
    DataBackup <<- list()

    ## ###############################################################
    ## Boucle de création des graphiques (par facteur de séparation) :
    for (modGraphSel in iFactGraphSel)
    {
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

        ## Suppression des valeurs supérieures à X% du maximum (pour plus de lisibilité) :
        if (getOption("P.maxExclu"))
        {
            tmpDataMod <- tmpDataMod[which(tmpDataMod[, metrique] <=
                                           getOption("P.GraphPartMax") * max(tmpDataMod[, metrique],
                                                                             na.rm=TRUE)), ]
        }else{}

        ## Suppression des 'levels' non utilisés :
        tmpDataMod <- dropLevels.f(tmpDataMod)

        ## Sauvegarde temporaire des données :
        DataBackup[[modGraphSel]] <<- tmpDataMod

        ## Ouverture et configuration du périphérique graphique :
        openDevice.f(noGraph=which(modGraphSel == iFactGraphSel),
                     metrique=metrique, factGraph=factGraph, listFact=listFact)

        par(mar=c(9, 5, 8, 1), mgp=c(3.5, 1, 0)) # paramètres graphiques.

        ## Titre (d'après les métriques, modalité du facteur de séparation et facteurs de regroupement) :
        mainTitle <- paste("valeurs de ", varNames[metrique, "nom"],
                           ifelse(iFactGraphSel[1] == "", # Facteur de séparation uniquement si défini.
                                  "",
                                  paste("\npour le champ '", factGraph, "' = ", modGraphSel, sep="")),
                           "\n selon ",
                           paste(sapply(listFact[length(listFact):1],
                                        function(x)paste(varNames[x, c("article", "nom")], collapse="")),
                                 collapse=" et "),
                           "\n\n", sep="")

        ## Les couleurs pour l'identification des modalités du facteur de second niveau :
        colors <- colBoxplot.f(terms=attr(terms(exprBP), "term.labels"), data=tmpDataMod)

        ## Label axe y :
        ylab <- parse(text=paste("'", Capitalize.f(varNames[metrique, "nom"]), "'",
                      ifelse(varNames[metrique, "unite"] != "",
                             paste("~~(", varNames[metrique, "unite"], ")", sep=""),
                             ""),
                      sep=""))

        ## Boxplot !
        tmpBP <- boxplot(exprBP, data=tmpDataMod,
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
            sepBoxplot.f(terms=attr(terms(exprBP), "term.labels"), data=tmpDataMod)
        }

        ## Séparateurs par défaut :
        abline(v = 0.5+(0:length(tmpBP$names)) , col = "lightgray", lty = "dotted") # Séparations.

        ## Légende des couleurs (facteur de second niveau) :
        if (getOption("P.legendeCouleurs"))
        {
            legendBoxplot.f(terms=attr(terms(exprBP), "term.labels"), data=tmpDataMod)
        }else{}

        ## Moyennes :
        Moyenne <- as.vector(tapply(X=tmpDataMod[, metrique], # moyenne par groupe.
                                    INDEX=as.list(tmpDataMod[ , attr(terms(exprBP),
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
    }  ## Fin de boucle graphique.

    ## On ferme les périphériques PDF :
    if (getOption("P.graphPDF"))
    {
        dev.off()
    }else{}

}








### Local Variables:
### ispell-local-dictionary: "english"
### fill-column: 120
### End:
