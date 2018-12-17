#-*- coding: latin-1 -*-
# Time-stamp: <2018-12-09 17:04:03 yreecht>

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

### File: Boxplot_generique_calc.R
### Created: <2012-01-19 17:11:27 yreecht>
###
### Author: Yves Reecht
###
####################################################################################################
### Description:
###
### Fonctions de traitement des données et des graphiques pour la création de boxplots "à la carte".
####################################################################################################

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
        if (length(terms) == 1)
        {
            col <- PAMPAcolors.f(n=length(unique(na.omit(data[ , terms[1]]))))

            return(col)
        }else{
            return(NULL)
        }
    }else{
        n <- length(terms)

        ## Définition des couleurs :
        col <- rep(PAMPAcolors.f(n=length(unique(na.omit(data[ , terms[n - 1]])))),
                   each=ifelse(n == 2,
                               1,            # Pas de facteur imbriqué.
                               prod(sapply(data[ , terms[1:(n-2)], drop=FALSE], # nombres de niveaux du (des) facteur(s)
                                           function(x){length(unique(na.omit(x)))})))) # imbriqués.
        return(col)
    }
}


########################################################################################################################
legendBoxplot.f <- function(terms, data, pch = 15, pt.cex=1.2, cex=0.9)
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
    }else{
        n <- length(terms)

        ## Couleurs :
        colors <- unique(colBoxplot.f(terms=terms, data=data))

        ## Noms :
        names <- levels(as.factor(data[ , terms[n - 1]]))

        ## Légende :
        legend("topright", names, col=colors,
               pch = pch, pt.cex=pt.cex,
               cex = cex, title=varNames.f(terms[n - 1], "nom"))
    }
}

########################################################################################################################
graphTitle.f <- function(metrique, modGraphSel, factGraph, listFact, model=NULL, type="espece",
                         lang = getOption("P.lang"))
{
    ## Purpose:
    ## ----------------------------------------------------------------------
    ## Arguments:
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 14 oct. 2010, 15:44
    return(paste(ifelse(is.null(model),
                        mltext("graphTitle.vals",
                               language = lang),
                        paste(model,
                              mltext("graphTitle.for",
                                     language = lang),
                              varNames[metrique, "article"], sep="")),
                 varNames[metrique, "nom"],
                 ifelse(is.element(type, c("espece", "unitobs", "CL_espece", "unitobs(CL)")),
                        paste(mltext("graphTitle.agg",
                                     language = lang),
                              switch(varNames[metrique, "genre"], # for languages with genre concordence.
                                     f=mltext("graphTitle.f",
                                              language = lang),
                                     fp=mltext("graphTitle.fp",
                                               language = lang),
                                     mp=mltext("graphTitle.mp",
                                               language = lang), ""), sep=""),
                        ""),
                 switch(type,
                        "espece"=mltext("graphTitle.bySpSt",
                                        language = lang),
                        "CL_espece"=mltext("graphTitle.bySCSpSt",
                                           language = lang),
                        "unitobs"=mltext("graphTitle.bySt",
                                         language = lang),
                        "unitobs(CL)"=mltext("graphTitle.byStSC",
                                             language = lang),
                        "CL_unitobs"=mltext("graphTitle.bySCSt",
                                            language = lang),
                        "biodiv"=mltext("graphTitle.biodiv",
                                        language = lang),
                        ""),
                 switch(type,
                        "espece"={
                            ifelse(modGraphSel == "", # Facteur de séparation uniquement si défini.
                                   "",
                                   paste(mltext("graphTitle.sep.SpSt",
                                                language = lang),
                                         " '", factGraph, "' = ", modGraphSel, sep=""))
                        },
                        "CL_espece"={
                            ifelse(modGraphSel == "", # Facteur de séparation uniquement si défini.
                                   "",
                                   paste(mltext("graphTitle.sep.SCSpSt",
                                                language = lang),
                                         " '", factGraph, "' = ", modGraphSel, sep=""))
                        },
                        "unitobs"={
                            ifelse(modGraphSel[1] == "", # Facteur de séparation uniquement si défini.
                                   mltext("graphTitle.sep.St.all",
                                          language = lang),
                                   paste(mltext("graphTitle.sep.St",
                                                language = lang),
                                         " '", factGraph, "' = (",
                                         paste(modGraphSel, collapse=", "), ")", sep=""))
                        },
                        "unitobs(CL)"={
                            ifelse(modGraphSel[1] == "", # Facteur de séparation uniquement si défini.
                                   mltext("graphTitle.sep.StSC.all",
                                          language = lang),
                                   paste(mltext("graphTitle.sep.StSC",
                                                language = lang),
                                         " '", factGraph, "' = (",
                                         paste(modGraphSel, collapse=", "), ")", sep=""))
                        },
                        "CL_unitobs"={
                            ifelse(modGraphSel[1] == "", # Facteur de séparation uniquement si défini.
                                   mltext("graphTitle.sep.SCSt.all",
                                          language = lang),
                                   paste(mltext("graphTitle.sep.SCSt",
                                                language = lang),
                                         " '", factGraph, "' = (",
                                         paste(modGraphSel, collapse=", "), ")", sep=""))
                        },
                        "biodiv"={
                            ifelse(modGraphSel[1] == "", # Facteur de séparation uniquement si défini.
                                   "",
                                   paste(mltext("graphTitle.sep.biodiv",
                                                language = lang),
                                         " '", factGraph, "' = (",
                                         paste(modGraphSel, collapse=", "), ")", sep=""))
                        },
                        ""),
                 mltext("graphTitle.by",
                        language = lang),
                 paste(sapply(listFact[length(listFact):1],
                              function(x)paste(c(## varNames.f(x, "article"),
                                                 "",
                                                 varNames.f(x, "nom")), collapse="")),
                       collapse=mltext("graphTitle.and",
                                       language = lang)),
                 "\n", sep=""))
}


########################################################################################################################
plotValMoyennes.f <- function(moyennes, objBP,
                              adj=c(0.5, -0.4), cex=0.9,...)
{
    ## Purpose: Affichage des moyennes sur les boxplots en évitant le
    ##          recouvrement avec les lignes des boîtes.
    ## ----------------------------------------------------------------------
    ## Arguments: moyennes : les valeurs de moyennes.
    ##            objBP : un objet retourné par la fonction "boxplot".
    ##            adj : justification.
    ##            cex : taille de la police.
    ##            ... : paramètres optionnels supplémentaires passés à text()
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 26 oct. 2010, 15:43

    ## Propriétés des boîtes à moustaches + points hors boîtes + maximum du graphique :
    pointsOut <- as.list(tapply(objBP$out, objBP$group, function(x)x))
    pointsOut[as.character(which(!is.element(seq(length.out=ncol(objBP$stats)),
                                             as.numeric(names(pointsOut)))))] <- NA

    x <- rbind(objBP$stats,
               matrix(sapply(pointsOut,
                             function(x)
                         {
                             c(sort(x, na.last=TRUE),
                               rep(NA, max(sapply(pointsOut, length)) - length(x)))
                         }),
                      ncol=length(pointsOut))[ , order(as.numeric(names(pointsOut))), drop=FALSE],
               if (getOption("P.maxExclu") && getOption("P.GraphPartMax") < 1)
                   {
                       objBP$ylim[2]
                   }else{
                       max(c(objBP$out, objBP$stats), na.rm=TRUE)
                   })

    x[x > objBP$ylim[2] | x < objBP$ylim[1]] <- NA

    x <- apply(x, 2, sort, na.last=TRUE)

    ## Proportions occupées par les différentes parties des boites à moustaches :
    xprop <- apply(x, 2,
                   function(cln)
               {
                   res <- (tail(cln, -1) - head(cln, -1))/max(cln, na.rm=TRUE)
                   if (all(na.omit(res) <= 0))
                   {
                       res[3] <- 1
                   }else{}

                   return(res)
               })

    ## Ordre de priorité décroissante des positions où écrire :
    ord <- c(3, 4, 2, seq(from=5, to=nrow(xprop)), 1)

    ## Première position (dans l'ordre décroissant de priorité) remplissant le critère (> 5.5% de la zone graphique) :
    xi <- sapply(seq(length.out=ncol(xprop)), function(i)which(xprop[ord , i] > 0.055)[1])

    ## Écriture des valeurs de moyennes sur le graphique :
    text(x=seq_along(xi), y=sapply(seq_along(xi), function(i)x[ord[xi][i], i]),
         labels=as.character(round(moyennes, digits=unlist(options("P.NbDecimal")))),
         col=getOption("P.valMoyenneCol"), adj=adj,
         cex=cex,...)

}

########################################################################################################################
plotPetitsEffectifs.f <- function(objBP, nbmin=20)
{
    ## Purpose: Affichage des warnings sur les graphiques
    ## ----------------------------------------------------------------------
    ## Arguments:
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 27 oct. 2010, 10:39

    if (any(objBP$n < nbmin & objBP$n > 0, na.rm=TRUE))
    {
        msg <- c(
                 ## Affichage d'avertissement pour  > X% du max retiré :
                 if (getOption("P.maxExclu") &&  getOption("P.GraphPartMax") < 1)
                 {
                     paste(mltext("plotPetitsEffectifs.rec", language = getOption("P.lang")),
                           " > ", 100 * getOption("P.GraphPartMax"),
                           "% ",
                           mltext("plotPetitsEffectifs.not.disp",
                                  language = getOption("P.lang")), sep="")
                 }else{},
                 paste(mltext("plotPetitsEffectifs.small.n", language = getOption("P.lang")),
                       " (< ", nbmin, ")", sep=""))

        ## "Légende" :
        legend("top",
               msg,
               cex =0.9, text.col="red", merge=FALSE, adj=c(0, 0.2),
               pch=rev(c(24, NA)[seq_along(msg)]),
               col="red3", pt.bg="gold", pt.cex=1.2)


        ## Propriétés des boîtes à moustaches + points hors boîtes + maximum du graphique :
        pointsOut <- as.list(tapply(objBP$out, objBP$group, function(x)x))
        pointsOut[as.character(which(!is.element(seq(length.out=ncol(objBP$stats)),
                                                 as.numeric(names(pointsOut)))))] <- NA

        x <- rbind(min(c(objBP$out, objBP$stats), na.rm=TRUE),
                   objBP$stats,
                   matrix(sapply(pointsOut,
                                 function(x)
                             {
                                 c(sort(x, na.last=TRUE),
                                   rep(NA, max(sapply(pointsOut, length)) - length(x)))
                             }),
                          ncol=length(pointsOut))[ , order(as.numeric(names(pointsOut))), drop=FALSE],
                   if (getOption("P.maxExclu") && getOption("P.GraphPartMax") < 1)
                   {
                       objBP$ylim[2]
                   }else{
                       max(c(objBP$out, objBP$stats), na.rm=TRUE)
                   })

        x[x > objBP$ylim[2] | x < objBP$ylim[1]] <- NA

        x <- apply(x, 2, sort, na.last=TRUE)

        ## Proportions occupées par les différentes parties des boites à moustaches :
        xprop <- apply(x, 2, function(cln)(tail(cln, -1) - head(cln, -1))/max(cln, na.rm=TRUE))

        ord <- c(seq(from=nrow(xprop), to=6), 1, 5, 2, 4, 3) # Ordre de priorité des positions où écrire.

        ## Première position (ordre décroissant de priorité) remplissant le critère (> 8% de la zone graphique) :
        xi <- sapply(seq(length.out=ncol(xprop)), function(i){which(xprop[ord , i] > 0.08)[1]})

        idx <- which(objBP$n < nbmin & objBP$n > 0)

        ampli <- max(c(objBP$out, objBP$stats), na.rm=TRUE) - min(c(objBP$out, objBP$stats), na.rm=TRUE)

        invisible(sapply(seq_along(xi)[idx],
                         function(i)
                     {
                         points(x=i,
                                y=x[ifelse(ord[xi][i] == 1, 1, ord[xi][i] + 1), i] +
                                  ifelse(ord[xi][i] == 1, # Ajustement vertical si en bas.
                                         0.04 * ampli,
                                         ifelse(ord[xi][i] == nrow(xprop), #... si tout en haut.
                                                -0.04 * ampli,
                                                -0.04 * ampli)),
                                pch=24, col = "red3", bg = "gold", cex=1.2)

                     }))

    }else{
        ## Affichage d'avertissement pour  > X% du max retiré :
        if (getOption("P.maxExclu"))
        {
            legend("top",
                   paste(mltext("plotPetitsEffectifs.rec", language = getOption("P.lang")),
                         " > ", 100 * getOption("P.GraphPartMax"),
                         "% ",
                         mltext("plotPetitsEffectifs.not.disp",
                                language = getOption("P.lang")), sep=""),
                   cex =0.9, col="red", text.col="red", merge=FALSE)
        }else{}
    }
}


########################################################################################################################
WP2boxplot.f <- function(metrique, factGraph, factGraphSel, listFact, listFactSel, tableMetrique, dataEnv,
                         baseEnv=.GlobalEnv)
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
    ##            dataEnv : environnement de stockage des données.
    ##            baseEnv : environnement de l'interface principale.
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

    selections <- c(list(factGraphSel), listFactSel) # Concaténation des leurs listes de modalités sélectionnées.

    ## Données pour la série de boxplots :
    tmpData <- subsetToutesTables.f(metrique=metrique, facteurs=facteurs, selections=selections,
                                    dataEnv=dataEnv, tableMetrique=tableMetrique, exclude = NULL)

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
        if (nrow(tmpDataMod) < getOption("P.MinNbObs"))
        {
            warning(mltext("WP2boxplot.W.n.1"),
                    modGraphSel, " < ", getOption("P.MinNbObs"),
                    mltext("WP2boxplot.W.n.2"))

            plotted <- FALSE
            next()
        }else{
            plotted <- TRUE
        }

        ## Suppression des 'levels' non utilisés :
        tmpDataMod <- dropLevels.f(tmpDataMod)

        ## Sauvegarde temporaire des données :
        DataBackup[[modGraphSel]] <<- tmpDataMod

        ## Ouverture et configuration du périphérique graphique :
        graphFileTmp <- openDevice.f(noGraph=which(modGraphSel == iFactGraphSel),
                                     metrique=metrique,
                                     factGraph=factGraph,
                                     modSel=if (getOption("P.plusieursGraphPage"))
                                 {
                                     iFactGraphSel      # toutes les modalités.
                                 }else{
                                     modGraphSel        # la modalité courante uniquement.
                                 },
                                     listFact=listFact,
                                     dataEnv=dataEnv,
                                     type=switch(tableMetrique, # différents types de graphs en fonction de la table de
                                        # données.
                                                 "unitSp"={"espece"},
                                                 "unitSpSz"={"CL_espece"},
                                                 "unit"={"unitobs"},
                                                 "espece"),
                                     typeGraph="boxplot")

        ## graphFile uniquement si nouveau fichier :
        if (!is.null(graphFileTmp)) graphFile <- graphFileTmp

        par(mar=c(9, 5, 8, 1), mgp=c(3.5, 1, 0)) # paramètres graphiques.

        ## Titre (d'après les métriques, modalité du facteur de séparation et facteurs de regroupement) :
        mainTitle <- graphTitle.f(metrique=metrique,
                                  modGraphSel=modGraphSel, factGraph=factGraph,
                                  listFact=listFact,
                                  type=switch(tableMetrique, # différents types de graphs en fonction de la table de
                                        # données.
                                              "unitSp"={"espece"},
                                              "unitSpSz"={"CL_espece"},
                                              "espece"))

        ## Label axe y :
        ylab <- ifelse(getOption("P.axesLabels"),
                       parse(text=paste("\"", Capitalize.f(varNames[metrique, "nom"]), "\"",
                             ifelse(varNames[metrique, "unite"] != "",
                                    paste("~~(", varNames[metrique, "unite"], ")", sep=""),
                                    ""),
                             sep="")),
                       "")

        ## Boxplot !
        tmpBP <- boxplotPAMPA.f(exprBP, data=tmpDataMod,
                                main=mainTitle, ylab=ylab)


        ## #################### Informations supplémentaires sur les graphiques ####################

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
            points(Moyenne,
                   pch=getOption("P.pointMoyennePch"),
                   col=getOption("P.pointMoyenneCol"), lwd=2.5,
                   cex=getOption("P.pointMoyenneCex"))
        }else{}

        ## ... valeurs :
        if (getOption("P.valMoyenne"))
        {
            plotValMoyennes.f(moyennes=Moyenne, objBP=tmpBP)
        }else{}

        if (isTRUE(getOption("P.warnings")))
        {
            ## Avertissement pour les petits effectifs :
            plotPetitsEffectifs.f(objBP=tmpBP, nbmin=5)
        }else{}

        ## Nombres d'observations :
        if (getOption("P.NbObs"))
        {
            nbObs <- tmpBP$n # Retourné par la fonction 'boxplot'

            ## Nombres sur l'axe supérieur :
            axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)),
                 col.ticks=getOption("P.NbObsCol"), col.axis = getOption("P.NbObsCol"),
                 lty = 2, lwd = 0.5,
                 mgp=c(2, 0.5, 0))

            legend("topleft", mltext("WP2boxplot.n.rec", language = getOption("P.lang")),
                   cex =0.9, col=getOption("P.NbObsCol"), text.col=getOption("P.NbObsCol"), merge=FALSE)
        }else{}

        ## ###################################################
        ## Fermeture de graphiques et sauvegarde de fichiers :

        ## On ferme les périphériques PNG en mode fichier individuel :
        if (isTRUE(getOption("P.graphPNG")))
        {
            if ((! getOption("P.plusieursGraphPage") || length(iFactGraphSel) <= 1) && plotted)
            {
                dev.off()

                ## Sauvegarde des données :
                if (getOption("P.saveData"))
                {
                    writeData.f(filename=graphFile, Data=tmpData,
                                cols=NULL)
                }else{}

                ## Sauvegarde des statistiques :
                if (getOption("P.saveStats"))
                {
                    infoStats.f(filename=graphFile, Data=tmpData, agregLevel="species", type="graph",
                                metrique=metrique, factGraph=factGraph, factGraphSel=modGraphSel,
                                listFact=rev(listFact), listFactSel=rev(listFactSel), # On les remets dans un ordre
                                        # intuitif.
                                dataEnv=dataEnv, baseEnv=baseEnv)
                }else{}
            }
        }else{
            ## Sauvegarde en wmf si pertinent et souhaité :
            if (( ! getOption("P.plusieursGraphPage") || length(iFactGraphSel) <= 1) &&
                plotted && ! getOption("P.graphPDF"))
            {
                if (.Platform$OS.type == "windows" && isTRUE(getOption("P.graphWMF")))
                {
                    savePlot(graphFile, type="wmf", device=dev.cur())
                }else{}

                ## Sauvegarde des données :
                if (getOption("P.saveData"))
                {
                    writeData.f(filename=graphFile, Data=tmpData,
                                cols=NULL)
                }else{}

                ## Sauvegarde des statistiques :
                if (getOption("P.saveStats"))
                {
                    infoStats.f(filename=graphFile, Data=tmpData, agregLevel="species", type="graph",
                                metrique=metrique, factGraph=factGraph, factGraphSel=modGraphSel,
                                listFact=rev(listFact), listFactSel=rev(listFactSel), # On les remets dans un ordre
                                        # intuitif.
                                dataEnv=dataEnv, baseEnv=baseEnv)
                }else{}
            }else{}
        }

    }  ## Fin de boucle graphique.

    ## On ferme les périphériques PDF ou PNG restants :
    if (getOption("P.graphPDF") ||
        (isTRUE(getOption("P.graphPNG")) && getOption("P.plusieursGraphPage") && length(iFactGraphSel) > 1)
         && plotted)
    {
        dev.off()

        ## Sauvegarde des données :
        if (getOption("P.saveData"))
        {
            writeData.f(filename=sub("\\%03d", "00X", graphFile),
                        Data=DataBackup, cols=NULL)
        }else{}

        ## Sauvegarde des statistiques :
        if (getOption("P.saveStats"))
        {
            infoStats.f(filename=sub("\\%03d", "00X", graphFile), Data=DataBackup,
                        agregLevel="species", type="graph",
                        metrique=metrique, factGraph=factGraph, factGraphSel=factGraphSel,
                        listFact=rev(listFact), listFactSel=rev(listFactSel), # On les remets dans un ordre intuitif.
                        dataEnv=dataEnv, baseEnv=baseEnv)
        }else{}

        ## Inclusion des fontes dans le pdf si souhaité :
        if (getOption("P.graphPDF") && getOption("P.pdfEmbedFonts"))
        {
            i <- 1

            ## On parcours tous les fichiers qui correspondent au motif :
            while (is.element(basename(tmpFile <- sub("\\%03d", formatC(i, width=3, flag="0"), graphFile)),
                              dir(dirname(graphFile))) &&
                   ## Si pas de remplacement effectif, application pour i==1 uniquement :
                   (i == 1 || grepl(pattern="\\%03d", graphFile)))
            {
                tryCatch(embedFonts(file=tmpFile),
                         error=function(e)
                     {
                         warning(mltext("WP2boxplot.W.pdfFonts"))
                     })

                i <- i + 1
            }
        }else{}
    }else{}

    ## Sauvegarde en wmf + données restants si pertinent et souhaité :
    if ( ! (getOption("P.graphPNG") || getOption("P.graphPDF")) && # Si pas d'autre sortie fichier.
        getOption("P.plusieursGraphPage") && length(iFactGraphSel) > 1
        && plotted)
    {
        if (.Platform$OS.type == "windows" && isTRUE(getOption("P.graphWMF")))
        {
            savePlot(graphFile, type="wmf", device=dev.cur())
        }else{}

        ## Sauvegarde des données :
        if (getOption("P.saveData"))
        {
            writeData.f(filename=sub("\\%03d", "00X", graphFile),
                        Data=DataBackup, cols=NULL)
        }else{}

        ## Sauvegarde des statistiques :
        if (getOption("P.saveStats"))
        {
            infoStats.f(filename=sub("\\%03d", "00X", graphFile), Data=DataBackup,
                        agregLevel="species", type="graph",
                        metrique=metrique, factGraph=factGraph, factGraphSel=factGraphSel,
                        listFact=rev(listFact), listFactSel=rev(listFactSel), # On les remets dans un ordre intuitif.
                        dataEnv=dataEnv, baseEnv=baseEnv)
        }else{}
    }else{}

    pampaProfilingEnd.f()
}








### Local Variables:
### ispell-local-dictionary: "english"
### fill-column: 120
### End:
