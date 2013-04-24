#-*- coding: latin-1 -*-
# Time-stamp: <2013-02-27 19:45:53 yves>

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

### File: Graphiques_carto.R
### Created: <2012-12-13 15:01:01 yves>
###
### Author: Yves Reecht
###
####################################################################################################
### Description:
###
### Fonction de graphiques pour les représentations cartographiques.
####################################################################################################

########################################################################################################################
graphTitle.carto.f <- function(metrique, modGraphSel, factGraph, listFact, factSpatial, model=NULL, type="espece")
{
    ## Purpose: produire un titre de graphique adapté.
    ## ----------------------------------------------------------------------
    ## Arguments: metrique : métrique.
    ##            modGraphSel : modalité(s) de factGraph.
    ##            factGraph : facteur de séparation des graphiques /
    ##                        sélection d'espèces.
    ##            listFact : facteurs d'agrégation.
    ##            factSpatial : facteur de niveau d'agrégation spatiale.
    ##            model :
    ##            type : type de niveaux d'agrégation.
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 21 févr. 2013, 17:44

    return(paste(ifelse(is.null(model),
                        "valeurs de ",
                        paste(model, " pour ", varNames.f(metrique, "article"), sep="")),
                 varNames.f(metrique, "nom"),
                 ifelse(is.element(type, c("espece", "unitobs", "CL_espece", "unitobs(CL)")),
                        paste(" agrégé",
                              switch(varNames.f(metrique, "genre"), # Accord de "agrégé".
                                     f="e", fp="es", mp="s", ""), sep=""),
                        ""),
                 switch(type,
                        "espece"=" par espèce et unité d'observation",
                        "CL_espece"=" par classe de tailles, espèce et unité d'observation",
                        "unitobs"=" par unité d'observation",
                        "unitobs(CL)"=" par unité d'observation",
                        "CL_unitobs"=" par classe de tailles et unité d'observation",
                        "biodiv"=" par unité d'observation",
                        ""),
                 paste("\npar zone '", varNames.f(factSpatial, "nom"), "'", sep=""),
                 switch(type,
                        "espece"={
                            ifelse(modGraphSel == "", # Facteur de séparation uniquement si défini.
                                   "",
                                   paste("\npour le champ '", factGraph, "' = ", modGraphSel, sep=""))
                        },
                        "CL_espece"={
                            ifelse(modGraphSel == "", # Facteur de séparation uniquement si défini.
                                   "",
                                   paste("\npour le champ '", factGraph, "' = ", modGraphSel, sep=""))
                        },
                        "unitobs"={
                            ifelse(modGraphSel[1] == "", # Facteur de séparation uniquement si défini.
                                   "\npour toutes les espèces",
                                   paste("\npour les espèces correspondant à '", factGraph, "' = (",
                                         paste(modGraphSel, collapse=", "), ")", sep=""))
                        },
                        "unitobs(CL)"={
                            ifelse(modGraphSel[1] == "", # Facteur de séparation uniquement si défini.
                                   "\npour toutes les classes de taille",
                                   paste("\npour les classes de tailles correspondant à '", factGraph, "' = (",
                                         paste(modGraphSel, collapse=", "), ")", sep=""))
                        },
                        "CL_unitobs"={
                            ifelse(modGraphSel[1] == "", # Facteur de séparation uniquement si défini.
                                   "\npour toutes les espèces",
                                   paste("\npour les espèces correspondant à '", factGraph, "' = (",
                                         paste(modGraphSel, collapse=", "), ")", sep=""))
                        },
                        "biodiv"={
                            ifelse(modGraphSel[1] == "", # Facteur de séparation uniquement si défini.
                                   "",
                                   paste("\npour les unités d'observation correspondant à '", factGraph, "' = (",
                                         paste(modGraphSel, collapse=", "), ")", sep=""))
                        },
                        ""),
                 ifelse(listFact[1] == "",
                        "",
                        paste("\n selon ",
                              paste(sapply(listFact[length(listFact):1],
                                           function(x)paste(varNames[x, c("article", "nom")], collapse="")),
                                    collapse=" et "),
                              "", sep="")),
                 sep=""))
}


########################################################################################################################
plot.fondCarte.f <- function(refspa, dataEnv, bbox=NULL, lwd=0.2, ...)
{
    ## Purpose: affiche le fond de carte.
    ## ----------------------------------------------------------------------
    ## Arguments: refspa : le référentiel spatial.
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 17 déc. 2012, 17:11

    if (exists("baseMap", envir=dataEnv))
    {
        baseMap <- get("baseMap", envir=dataEnv)
    }else{
        baseMap <- unionSpatialPolygons(SpP=refspa,
                                        IDs=ifelse(is.element(tolower(refspa@data[ , getOption("P.landField")]),
                                                              getOption("P.landMods")),
                                                   "terre", "mer"))

        assign("baseMap", baseMap, envir=dataEnv)
    }

    if (is.null(bbox))
    {
        bbox <- bbox(refspa)
    }else{}

    plot(baseMap, axes=TRUE, col=getOption("P.landCols")[names(baseMap)],
         lwd=lwd, xlim=bbox["x", ], ylim=bbox["y", ], ...)
}

########################################################################################################################
subsetRefspaToData.f <- function(refspa, unitobs, Data)
{
    ## Purpose: réduire le référentiel spatial aux polygones présentant des
    ##          données.
    ## ----------------------------------------------------------------------
    ## Arguments: refspa : le référentiel spatial.
    ##            unitobs : la table d'unités d'observations.
    ##            Data : les données.
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 15 févr. 2013, 17:38

    dataOBJECTID <- unitobs[is.element(unitobs[ , "unite_observation"],
                                       Data[ , "unite_observation"]),
                            "OBJECTID"]

    refspa <- subset(refspa,
                     is.element(refspa@data[ , "OBJECTID"],
                                dataOBJECTID))
}


########################################################################################################################
plot.spatialZones.f <- function(refspa, unitobs, Data, fact, factSel, plot=TRUE)
{
    ## Purpose: afficher les zones sur un graphique.
    ## ----------------------------------------------------------------------
    ## Arguments: refspa : le référentiel spatial.
    ##            unitobs : la table d'unités d'observations.
    ##            Data : les données.
    ##            fact : facteur spatial.
    ##            factSel : (éventuelle) sélections sur le facteur spatial.
    ##            plot : doivent-elles être affichées ?
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 17 déc. 2012, 18:39

    refspa <- subsetRefspaToData.f(refspa=refspa, unitobs=unitobs, Data=Data)

    if ( ! is.na(factSel[1]))
    {
        refspaTmp <- subset(refspa,
                            is.element(refspa@data[ , fact], factSel))
    }else{
        refspaTmp <- subset(refspa,
                            is.element(refspa@data[ , fact],
                                       unitobs[ , fact]) &
                            ! is.na(refspa@data[ , fact]))
    }

    polyZones <- unionSpatialPolygons(SpP=refspaTmp,
                                      IDs=refspaTmp@data[ , fact])

    if (plot)
    {
        plot(polyZones,
             col=PAMPAcolors.f(n=length(polyZones), palette=getOption("P.zonesPalette")),
             add=TRUE)
    }else{}

    return(polyZones)
}

########################################################################################################################
barplotCarto.generic.f <- function(polyZones, Data, factSpatial, metrique, listFact)
{
    ## Purpose: Créer des barplots en subplots.
    ## ----------------------------------------------------------------------
    ## Arguments: polyZones: polygones des zones de regroupmeent spatial.
    ##            Data : données (data.frame).
    ##            factSpatial: facteur de regroupement spatial.
    ##            metrique : la métrique choisie.
    ##            listFact : liste du (des) facteur(s) de regroupement
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date:  4 févr. 2013, 16:15

    ## Barycentres des zones :
    zonesCoord <- coordinates(polyZones)

    ## Données séparées par subplot :
    tmpData <- split(Data, Data[ , factSpatial])

    ## Couleurs de diférentiation des zones :
    colZones <- PAMPAcolors.f(n=length(polyZones), palette=getOption("P.zonesPalette"))
    names(colZones) <- row.names(zonesCoord)

    ## Rectangles pour avoir un fond (transparent) aux subplots.
    for (i in row.names(zonesCoord))
    {
        subplot({plot(0:1, 0:1, xaxt="n", yaxt="n", type="n", xlab="", ylab="", bty="n") ;
                 rect(0, 0, 1, 1, col=rgb(1, 1, 1, 0.5))},
                zonesCoord[i, 1], zonesCoord[i, 2], size=getOption("P.pinSubplot"), type="plt")
    ## }

    ## ## Barplots :
    ## for (i in row.names(zonesCoord))
    ## {
        oldpars <- par(no.readonly=TRUE)

        tryCatch(subplot({barplotPAMPA.f(metrique=metrique, listFact=listFact, Data=tmpData[[i]],
                                         cex=par("cex"),
                                         col.main="black", cex.names=0.8);
                          ## Rectangle aux couleurs de la zone :
                          rect(xleft=mean(par("usr")[1:2]) -
                                      strwidth(i, cex=par("cex.main")) / 2 -
                                      1 * par("cxy")[1],
                               ybottom=par("usr")[4] + 0.4 * par("cxy")[2] * par("cex"),
                               xright=mean(par("usr")[1:2]) +
                                       strwidth(i, cex=par("cex.main")) / 2 +
                                       1 * par("cxy")[1],
                               ytop=par("usr")[4] + 1.1 * par("cxy")[2] * par("cex") +
                                     strheight(i, cex=par("cex.main")),
                               col=colZones[i], xpd=TRUE) ;
                          ## Titre :
                          title(main=i)},
                         zonesCoord[i, 1], zonesCoord[i, 2],
                         size=getOption("P.pinSubplot"),  type="fig",
                         pars=list(bg="white", fg="black", cex=0.75, xpd=NA, ## mgp=c(1.5, 0.5, 0),
                         tcl=-0.2##, mar=c(2.5, 4, 2, 1) + 0.1
                         )),
                 error=function(e){warning(e)})

        ## On rétabli les précédents paramètres pour éviter un mauvais placement des graphiques suivants :
        par(oldpars)
    }
}


########################################################################################################################
boxplotCarto.generic.f <- function(polyZones, Data, factSpatial, exprBP)
{
    ## Purpose: Créer des boxplots en subplots.
    ## ----------------------------------------------------------------------
    ## Arguments: polyZones: polygones des zones de regroupmeent spatial.
    ##            Data : données (data.frame).
    ##            factSpatial: facteur de regroupement spatial.
    ##            exprBP: formule du boxplot
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date:  8 janv. 2013, 16:52

    ## Barycentres des zones :
    zonesCoord <- coordinates(polyZones)

    ## Données séparées par subplot :
    tmpData <- split(Data, Data[ , factSpatial])

    ## Couleurs de diférentiation des zones :
    colZones <- PAMPAcolors.f(n=length(polyZones), palette=getOption("P.zonesPalette"))
    names(colZones) <- row.names(zonesCoord)

    metrique <- as.character(exprBP)[2]

    # browser()
    ## Rectangles pour avoir un fond (transparent) aux subplots.
    for (i in row.names(zonesCoord))
    {
        subplot({plot(0:1, 0:1, xaxt="n", yaxt="n", type="n", xlab="", ylab="", bty="n") ;
                 rect(0, 0, 1, 1, col=rgb(1, 1, 1, 0.5))},
                zonesCoord[i, 1], zonesCoord[i, 2], size=getOption("P.pinSubplot"), type="plt")
    ## }

    ## ## Boxplots :
    ## for (i in row.names(zonesCoord))
    ## {
        oldpars <- par(no.readonly=TRUE)

        tryCatch(subplot({boxplotPAMPA.f(exprBP, data=tmpData[[i]],
                                         main=i, cex=par("cex"),
                                         col.main="black");
                          ## Rectangle aux couleurs de la zone :
                          rect(xleft=mean(par("usr")[1:2]) -
                                      strwidth(i, cex=par("cex.main")) / 2 -
                                      1 * par("cxy")[1],
                               ybottom=par("usr")[4] + 0.4 * par("cxy")[2] * par("cex"),
                               xright=mean(par("usr")[1:2]) +
                                       strwidth(i, cex=par("cex.main")) / 2 +
                                       1 * par("cxy")[1],
                               ytop=par("usr")[4] + 1.1 * par("cxy")[2] * par("cex") +
                                     strheight(i, cex=par("cex.main")),
                               col=colZones[i], xpd=TRUE) ;
                          title(main=i)},
                         zonesCoord[i, 1], zonesCoord[i, 2],
                         size=getOption("P.pinSubplot"),  type="fig",
                         pars=list(bg="white", fg="black", cex=0.75, xpd=NA, ## mgp=c(1.5, 0.5, 0),
                         tcl=-0.2##, mar=c(2.5, 4, 2, 1) + 0.1
                         )),
                 error=function(e){warning(e)})

        ## On rétabli les précédents paramètres pour éviter un mauvais placement des graphiques suivants :
        par(oldpars)
    }
}

########################################################################################################################
subplotCarto.esp.f <- function(graphType,
                               metrique,
                               factSpatial, factSpatialSel,
                               factGraph, factGraphSel,
                               listFact, listFactSel, tableMetrique,
                               bbox=NULL,
                               dataEnv,
                               baseEnv=.GlobalEnv)
{
    ## Purpose: Produire des boxplots répartis sur des cartes.
    ## ----------------------------------------------------------------------
    ## Arguments: graphType : type de graphique (bar|box-plot).
    ##            metrique : la métrique choisie.
    ##            factSpatial : facteur de regroupement spatial.
    ##            factSpatialSel : sélection sur le facteur de regroupement
    ##                             spatial.
    ##            factGraph : le facteur sélection des espèces.
    ##            factGraphSel : la sélection de modalités pour ce dernier
    ##            listFact : liste du (des) facteur(s) de regroupement
    ##            listFactSel : liste des modalités sélectionnées pour ce(s)
    ##                          dernier(s)
    ##            tableMetrique : nom de la table de métriques.
    ##            bbox : emprise spatial (bouding box) ; toute la carte si
    ##                   NULL.
    ##            dataEnv : environnement de stockage des données.
    ##            baseEnv : environnement de l'interface.
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date:  7 févr. 2013, 17:18

    pampaProfilingStart.f()

    ## Nettoyage des facteurs (l'interface de sélection produit des valeurs vides) :
    listFactSel <- listFactSel[unlist(listFact) != ""]
    listFactSel <- listFactSel[length(listFactSel):1]

    listFact <- listFact[unlist(listFact) != ""]
    listFact <- listFact[length(listFact):1]

    ## Concaténation
    facteurs <- c(factSpatial, factGraph, unlist(listFact)) # Concaténation des facteurs

    selections <- c(list(factSpatialSel), list(factGraphSel), listFactSel) # Concaténation des leurs listes de modalités
                                        # sélectionnées

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
            warning("Nombre d'observations pour ", modGraphSel, " < ", getOption("P.MinNbObs"),
                    " : Graphique non créé !\n")

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
                                     modSel=modGraphSel, # la modalité courante uniquement.
                                     listFact=listFact,
                                     dataEnv=dataEnv,
                                     type=switch(tableMetrique, # différents types de graphs en fonction de la table de
                                        # données.
                                                 "unitSp"={"espece"},
                                                 "unitSpSz"={"CL_espece"},
                                                 "unit"={"unitobs"},
                                                 "espece"),
                                     typeGraph=paste("carte_",ifelse(graphType == "boxplot", "boxplot", "barplot"), sep=""))

        ## graphFile uniquement si nouveau fichier :
        if (!is.null(graphFileTmp)) graphFile <- graphFileTmp

        ## Paramètres graphiques :
        par(mar=c(2, 2,
                  ifelse(( ! getOption("P.graphPaper")) && getOption("P.title"),
                         8, 1),
                  1),
            mgp=c(3.5, 1, 0))
##################################################

        ## Titre (d'après les métriques, modalité du facteur de séparation et facteurs de regroupement) :
        mainTitle <- graphTitle.carto.f(metrique=metrique,
                                        modGraphSel=modGraphSel, factGraph=factGraph,
                                        listFact=listFact,
                                        factSpatial=factSpatial,
                                        type=switch(tableMetrique, # différents types de graphs en fonction de la table de
                                        # données.
                                                    "unitSp"={"espece"},
                                                    "unitSpSz"={"CL_espece"},
                                                    "espece"))

        ## Graphiques :
        refspa <- get("refspa", envir=dataEnv)

        ## Affichage du fond de carte :
        plot.fondCarte.f(refspa=refspa, dataEnv=dataEnv, bbox=bbox)

        if (( ! getOption("P.graphPaper")) && getOption("P.title"))
        {
            mtext(mainTitle, line=0.5, cex=1.8)
        }else{}

        ## Affichage des zones de regroupement spatial :
        polyZones <- plot.spatialZones.f(refspa=refspa, unitobs=get("unitobs", envir=dataEnv), Data=tmpDataMod,
                                         fact=factSpatial, factSel=factSpatialSel)

        ## Ajout des subplots :
        switch(graphType,
               "boxplot"=
           {
               tryCatch(boxplotCarto.generic.f(polyZones=polyZones, Data=tmpDataMod,
                                               factSpatial=factSpatial, exprBP=exprBP),
                        error=function(e)
                    {
                        warning("Un graphique n'a pu être créé :\n", e, immediate.=TRUE)
                    })
           },
               "barplot"=
           {
               tryCatch(barplotCarto.generic.f(polyZones=polyZones, Data=tmpDataMod,
                                               factSpatial=factSpatial,
                                               metrique=metrique, listFact=listFact),
                        error=function(e)
                    {
                        warning("Un graphique n'a pu être créé :\n", e, immediate.=TRUE)
                    })
           })

        ## ###################################################
        ## Fermeture de graphiques et sauvegarde de fichiers :

        ## On ferme les périphériques PNG en mode fichier individuel :
        if (isTRUE(getOption("P.graphPNG")))
        {
            if (plotted)
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
            }else{}
        }else{
            ## Sauvegarde en wmf si pertinent et souhaité :
            if (plotted && ! getOption("P.graphPDF"))
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
    if (getOption("P.graphPDF")
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

        ## Inclusion des fontes dans le(s) pdf(s) si souhaité :
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
                         warning("Impossible d'inclure les fontes dans le PDF !")
                     })

                i <- i + 1
            }
        }else{}
    }else{}

    ## ## Sauvegarde en wmf + données restants si pertinent et souhaité :
    ## if ( ! (getOption("P.graphPNG") || getOption("P.graphPDF")) && # Si pas d'autre sortie fichier.
    ##     getOption("P.plusieursGraphPage") && length(iFactGraphSel) > 1
    ##     && plotted)
    ## {
    ##     if (.Platform$OS.type == "windows" && isTRUE(getOption("P.graphWMF")))
    ##     {
    ##         savePlot(graphFile, type="wmf", device=dev.cur())
    ##     }else{}

    ##     ## Sauvegarde des données :
    ##     if (getOption("P.saveData"))
    ##     {
    ##         writeData.f(filename=sub("\\%03d", "00X", graphFile),
    ##                     Data=DataBackup, cols=NULL)
    ##     }else{}

    ##     ## Sauvegarde des statistiques :
    ##     if (getOption("P.saveStats"))
    ##     {
    ##         infoStats.f(filename=sub("\\%03d", "00X", graphFile), Data=DataBackup,
    ##                     agregLevel="species", type="graph",
    ##                     metrique=metrique, factGraph=factGraph, factGraphSel=factGraphSel,
    ##                     listFact=rev(listFact), listFactSel=rev(listFactSel), # On les remets dans un ordre intuitif.
    ##                     dataEnv=dataEnv, baseEnv=baseEnv)
    ##     }else{}
    ## }else{}

    pampaProfilingEnd.f()
}


########################################################################################################################
subplotCarto.unitobs.f <- function(graphType,
                                   metrique,
                                   factSpatial, factSpatialSel,
                                   factGraph, factGraphSel,
                                   listFact, listFactSel, tableMetrique,
                                   bbox=NULL,
                                   dataEnv,
                                   baseEnv=.GlobalEnv)
{
    ## Purpose: Produire des boxplots ou barplots répartis sur des cartes.
    ## ----------------------------------------------------------------------
    ## Arguments: graphType : type de graphique (bar|box-plot).
    ##            metrique : la métrique choisie.
    ##            factSpatial : facteur de regroupement spatial.
    ##            factSpatialSel : sélection sur le facteur de regroupement
    ##                             spatial.
    ##            factGraph : le facteur sélection des espèces.
    ##            factGraphSel : la sélection de modalités pour ce dernier
    ##            listFact : liste du (des) facteur(s) de regroupement
    ##            listFactSel : liste des modalités sélectionnées pour ce(s)
    ##                          dernier(s)
    ##            tableMetrique : nom de la table de métriques.
    ##            bbox : emprise spatial (bouding box) ; toute la carte si
    ##                   NULL.
    ##            dataEnv : environnement de stockage des données.
    ##            baseEnv : environnement de l'interface.
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 17 déc. 2012, 09:33

    pampaProfilingStart.f()

    ## Nettoyage des facteurs (l'interface de sélection produit des valeurs vides) :
    listFactSel <- listFactSel[unlist(listFact) != ""]
    listFactSel <- listFactSel[length(listFactSel):1]

    listFact <- listFact[unlist(listFact) != ""]
    listFact <- listFact[length(listFact):1]

    ## Concaténation
    facteurs <- c(factSpatial, factGraph, unlist(listFact)) # Concaténation des facteurs

    selections <- c(list(factSpatialSel), list(factGraphSel), listFactSel) # Concaténation des leurs listes de modalités
                                        # sélectionnées

    ## Données pour la série de boxplots :
    if (tableMetrique == "unit")
    {
        ## Pour les indices de biodiversité, il faut travailler sur les nombres... :
        tmpData <- subsetToutesTables.f(metrique=getOption("P.nbName"), facteurs=facteurs,
                                        selections=selections, dataEnv=dataEnv, tableMetrique="unitSp",
                                        exclude = NULL, add=c("unite_observation", "code_espece"))
    }else{
        ## ...sinon sur la métrique choisie :
        tmpData <- subsetToutesTables.f(metrique=metrique, facteurs=facteurs,
                                        selections=selections, dataEnv=dataEnv, tableMetrique=tableMetrique,
                                        exclude = NULL, add=c("unite_observation", "code_espece"))
    }

    ## Formule du boxplot
    exprBP <- eval(parse(text=paste(metrique, "~", paste(listFact, collapse=" + "))))

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
    if (tableMetrique == "unitSpSz" && factGraph != "classe_taille")
    {
        tmpData <- na.omit(agregationTableParCritere.f(Data=tmpData,
                                                       metrique=metrique,
                                                       facteurs=c("unite_observation", "classe_taille"),
                                                       dataEnv=dataEnv,
                                                       listFact=c(listFact, factSpatial)))
    }else{
        if (tableMetrique == "unit")
        {
            ## Calcul des indices de biodiversité sur sélection d'espèces :
            tmp <- do.call(rbind,
                           lapply(getOption("P.MPA"),
                                  function(MPA)
                              {
                                  calcBiodiv.f(Data=tmpData,
                                               refesp=get("refesp", envir=dataEnv),
                                               MPA=MPA,
                                               unitobs = "unite_observation", code.especes = "code_espece",
                                               nombres = getOption("P.nbName"),
                                               indices=metrique,
                                               dataEnv=dataEnv)
                              }))


            ## On rajoute les anciennes colonnes :
            tmpData <- cbind(tmp[ , colnames(tmp) != getOption("P.nbName")], # Colonne "nombre" désormais inutile.
                             tmpData[match(tmp$unite_observation, tmpData$unite_observation),
                                     !is.element(colnames(tmpData),
                                                 c(colnames(tmp), getOption("P.nbName"), "code_espece")), drop=FALSE])
        }else{
            tmpData <- na.omit(agregationTableParCritere.f(Data=tmpData,
                                                           metrique=metrique,
                                                           facteurs=c("unite_observation"),
                                                           dataEnv=dataEnv,
                                                           listFact=c(listFact, factSpatial)))
        }
    }

    ## Sauvegarde temporaire des données utilisées pour les graphiques (attention : écrasée à chaque nouvelle série de
    ## graphiques) :
    DataBackup <<- list(tmpData)

    ## Création du graphique si le nombre d'observations  < au minimum défini dans les options :
    if (nrow(tmpData) < getOption("P.MinNbObs"))
    {
        warning("Nombre d'observations pour (", paste(iFactGraphSel, collapse=", "), ") < ", getOption("P.MinNbObs"),
                " : Graphique non créé !\n")
    }else{

        ## Suppression des 'levels' non utilisés :
        tmpData <- dropLevels.f(tmpData)

        ## Ouverture et configuration du périphérique graphique :
        graphFile <- openDevice.f(noGraph=1,
                                  metrique=metrique,
                                  factGraph=factGraph,
                                  modSel=iFactGraphSel,
                                  listFact=listFact,
                                  dataEnv=dataEnv,
                                  type=ifelse(tableMetrique == "unitSpSz" && factGraph != "classe_taille",
                                              "CL_unitobs",
                                              "unitobs"),
                                  typeGraph=paste("carte_",ifelse(graphType == "boxplot", "boxplot", "barplot"), sep=""))

        ## Paramètres graphiques :
        par(mar=c(2, 2,
                  ifelse(( ! getOption("P.graphPaper")) && getOption("P.title"),
                         8, 1),
                  1),
            mgp=c(3.5, 1, 0))

        ## Titre (d'après les métriques, modalité du facteur de séparation et facteurs de regroupement) :[!!!]
        mainTitle <- graphTitle.carto.f(metrique=metrique,
                                        modGraphSel=iFactGraphSel,
                                        factGraph=factGraph,
                                        listFact=listFact,
                                        factSpatial=factSpatial,
                                        type=ifelse(tableMetrique == "unitSpSz" && factGraph != "classe_taille",
                                                    "CL_unitobs",
                                                    ifelse(tableMetrique == "unitSpSz",
                                                           "unitobs(CL)",
                                                           "unitobs")))

        ## ## Label axe y :
        ## ylab <- ifelse(getOption("P.axesLabels"),
        ##                parse(text=paste("'", Capitalize.f(varNames[metrique, "nom"]), "'",
        ##                      ifelse(varNames[metrique, "unite"] != "",
        ##                             paste("~~(", varNames[metrique, "unite"], ")", sep=""),
        ##                             ""),
        ##                      sep="")),
        ##                "")

        ## Graphiques :
        refspa <- get("refspa", envir=dataEnv)

        ## Affichage du fond de carte :
        plot.fondCarte.f(refspa=refspa, dataEnv=dataEnv, bbox=bbox)

        if (( ! getOption("P.graphPaper")) && getOption("P.title"))
        {
            mtext(mainTitle, line=0.5, cex=1.8)
        }else{}

        ## Affichage des zones de regroupement spatial :
        unitobs <- get("unitobs", envir=dataEnv)

        polyZones <- plot.spatialZones.f(refspa=refspa, unitobs=unitobs, Data=tmpData,
                                         fact=factSpatial, factSel=factSpatialSel)

        ## Ajout des subplots :
        switch(graphType,
               "boxplot"=
           {
               tryCatch(boxplotCarto.generic.f(polyZones=polyZones, Data=tmpData,
                                               factSpatial=factSpatial, exprBP=exprBP),
                        error=function(e)
                    {
                        warning("Un graphique n'a pu être créé :\n", e, immediate.=TRUE)
                    })
           },
               "barplot"=
           {
               tryCatch(barplotCarto.generic.f(polyZones=polyZones, Data=tmpData,
                                               factSpatial=factSpatial,
                                               metrique=metrique, listFact=listFact),
                        error=function(e)
                    {
                        warning("Un graphique n'a pu être créé :\n", e, immediate.=TRUE)
                    })
           })

        ## ##################################################
        ## Sauvegarde des données :
        if (getOption("P.saveData"))
        {
            writeData.f(filename=graphFile, Data=tmpData,
                        cols=NULL)
        }else{}

        ## Sauvegarde des infos sur les données et statistiques :
        if (getOption("P.saveStats"))
        {
            infoStats.f(filename=graphFile, Data=tmpData, agregLevel="unitobs", type="graph",
                        metrique=metrique, factGraph=factGraph, factGraphSel=factGraphSel,
                        listFact=rev(listFact), listFactSel=rev(listFactSel), # On les remets dans un ordre intuitif.
                        dataEnv=dataEnv, baseEnv=baseEnv)
        }else{}

        ## On ferme les périphériques PDF :
        if (getOption("P.graphPDF") || isTRUE(getOption("P.graphPNG")))
        {
            dev.off()

            ## Inclusion des fontes dans le pdf si souhaité :
            if (getOption("P.graphPDF") && getOption("P.pdfEmbedFonts"))
            {
                tryCatch(embedFonts(file=graphFile),
                         error=function(e)
                     {
                         warning("Impossible d'inclure les fontes dans le PDF !")
                     })
            }

        }else{
            if (.Platform$OS.type == "windows" && isTRUE(getOption("P.graphWMF")))
            {
                ## Sauvegarde en wmf si pertinent et souhaité :
                savePlot(graphFile, type="wmf", device=dev.cur())
            }else{}
        }
    }
}

########################################################################################################################
symbColCarto.esp.f <- function(graphType,
                               metrique,
                               factSpatial, factSpatialSel,
                               factGraph, factGraphSel,
                               tableMetrique,
                               bbox=NULL,
                               dataEnv,
                               baseEnv=.GlobalEnv)
{
    ## Purpose: Produire des cartes avec soit des symboles de taille variable
    ##          soit une échelle de couleur (variable simples, pas de facteur
    ##          explicatif). Agrégation par espèce par unitobs.
    ## ----------------------------------------------------------------------
    ## Arguments: graphType : type de graphique (symboles|couleurs).
    ##            metrique : la métrique choisie.
    ##            factSpatial : facteur de regroupement spatial.
    ##            factSpatialSel : sélection sur le facteur de regroupement
    ##                             spatial.
    ##            factGraph : le facteur sélection des espèces.
    ##            factGraphSel : la sélection de modalités pour ce dernier.
    ##            tableMetrique : nom de la table de métriques.
    ##            bbox : emprise spatial (bouding box) ; toute la carte si
    ##                   NULL.
    ##            dataEnv : environnement de stockage des données.
    ##            baseEnv : environnement de l'interface.
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 27 févr. 2013, 10:40
}


########################################################################################################################
symbColCarto.unitobs.f <- function(graphType,
                                   metrique,
                                   factSpatial, factSpatialSel,
                                   factGraph, factGraphSel,
                                   tableMetrique,
                                   bbox=NULL,
                                   dataEnv,
                                   baseEnv=.GlobalEnv)
{
    ## Purpose: Produire des cartes avec soit des symboles de taille variable
    ##          soit une échelle de couleur (variable simples, pas de facteur
    ##          explicatif). Agrégation par unitobs.
    ## ----------------------------------------------------------------------
    ## Arguments: graphType : type de graphique (symboles|couleurs).
    ##            metrique : la métrique choisie.
    ##            factSpatial : facteur de regroupement spatial.
    ##            factSpatialSel : sélection sur le facteur de regroupement
    ##                             spatial.
    ##            factGraph : le facteur sélection des espèces.
    ##            factGraphSel : la sélection de modalités pour ce dernier.
    ##            tableMetrique : nom de la table de métriques.
    ##            bbox : emprise spatial (bouding box) ; toute la carte si
    ##                   NULL.
    ##            dataEnv : environnement de stockage des données.
    ##            baseEnv : environnement de l'interface.
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 27 févr. 2013, 10:40

    pampaProfilingStart.f()

    ## ## Nettoyage des facteurs (l'interface de sélection produit des valeurs vides) :
    ## listFactSel <- listFactSel[unlist(listFact) != ""]
    ## listFactSel <- listFactSel[length(listFactSel):1]

    ## listFact <- listFact[unlist(listFact) != ""]
    ## listFact <- listFact[length(listFact):1]

    ## Concaténation
    facteurs <- c(factSpatial, factGraph) # Concaténation des facteurs

    selections <- c(list(factSpatialSel), list(factGraphSel)) # Concaténation des leurs listes de modalités
                                        # sélectionnées

    ## Données pour la série de boxplots :
    if (tableMetrique == "unit")
    {
        ## Pour les indices de biodiversité, il faut travailler sur les nombres... :
        tmpData <- subsetToutesTables.f(metrique=getOption("P.nbName"), facteurs=facteurs,
                                        selections=selections, dataEnv=dataEnv, tableMetrique="unitSp",
                                        exclude = NULL, add=c("unite_observation", "code_espece"))
    }else{
        ## ...sinon sur la métrique choisie :
        tmpData <- subsetToutesTables.f(metrique=metrique, facteurs=facteurs,
                                        selections=selections, dataEnv=dataEnv, tableMetrique=tableMetrique,
                                        exclude = NULL, add=c("unite_observation", "code_espece"))
    }

    ## ## Formule du boxplot
    ## exprBP <- eval(parse(text=paste(metrique, "~", paste(listFact, collapse=" + "))))

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
    if (tableMetrique == "unitSpSz" && factGraph != "classe_taille")
    {
        tmpData <- na.omit(agregationTableParCritere.f(Data=tmpData,
                                                       metrique=metrique,
                                                       facteurs=c("unite_observation", "classe_taille"),
                                                       dataEnv=dataEnv,
                                                       listFact=factSpatial))
    }else{
        if (tableMetrique == "unit")
        {
            ## Calcul des indices de biodiversité sur sélection d'espèces :
            tmp <- do.call(rbind,
                           lapply(getOption("P.MPA"),
                                  function(MPA)
                              {
                                  calcBiodiv.f(Data=tmpData,
                                               refesp=get("refesp", envir=dataEnv),
                                               MPA=MPA,
                                               unitobs = "unite_observation", code.especes = "code_espece",
                                               nombres = getOption("P.nbName"),
                                               indices=metrique,
                                               dataEnv=dataEnv)
                              }))


            ## On rajoute les anciennes colonnes :
            tmpData <- cbind(tmp[ , colnames(tmp) != getOption("P.nbName")], # Colonne "nombre" désormais inutile.
                             tmpData[match(tmp$unite_observation, tmpData$unite_observation),
                                     !is.element(colnames(tmpData),
                                                 c(colnames(tmp), getOption("P.nbName"), "code_espece")), drop=FALSE])
        }else{
            tmpData <- na.omit(agregationTableParCritere.f(Data=tmpData,
                                                           metrique=metrique,
                                                           facteurs=c("unite_observation"),
                                                           dataEnv=dataEnv,
                                                           listFact=factSpatial))
        }
    }

    ## Sauvegarde temporaire des données utilisées pour les graphiques (attention : écrasée à chaque nouvelle série de
    ## graphiques) :
    DataBackup <<- list(tmpData)

    ## Création du graphique si le nombre d'observations  < au minimum défini dans les options :
    if (nrow(tmpData) < getOption("P.MinNbObs"))
    {
        warning("Nombre d'observations pour (", paste(iFactGraphSel, collapse=", "), ") < ", getOption("P.MinNbObs"),
                " : Graphique non créé !\n")
    }else{

        ## Suppression des 'levels' non utilisés :
        tmpData <- dropLevels.f(tmpData)
browser()
        ## Ouverture et configuration du périphérique graphique :
        graphFile <- openDevice.f(noGraph=1,
                                  metrique=metrique,
                                  factGraph=factGraph,
                                  modSel=iFactGraphSel,
                                  listFact=NULL,
                                  dataEnv=dataEnv,
                                  type=ifelse(tableMetrique == "unitSpSz" && factGraph != "classe_taille",
                                              "CL_unitobs",
                                              "unitobs"),
                                  typeGraph=paste("carte_",
                                                  ifelse(graphType == "symboles", "symboles", "couleurs"), sep=""))

        ## Paramètres graphiques :
        par(mar=c(2, 2,
                  ifelse(( ! getOption("P.graphPaper")) && getOption("P.title"),
                         6.5, 1),
                  1),
            mgp=c(3.5, 1, 0))

        ## Titre (d'après les métriques, modalité du facteur de séparation et facteurs de regroupement) :[!!!]
        mainTitle <- graphTitle.carto.f(metrique=metrique,
                                        modGraphSel=iFactGraphSel,
                                        factGraph=factGraph,
                                        listFact=NULL,
                                        factSpatial=factSpatial,
                                        type=ifelse(tableMetrique == "unitSpSz" && factGraph != "classe_taille",
                                                    "CL_unitobs",
                                                    ifelse(tableMetrique == "unitSpSz",
                                                           "unitobs(CL)",
                                                           "unitobs")))

        ## Graphiques :
        refspa <- get("refspa", envir=dataEnv)

        ## Affichage du fond de carte :
        plot.fondCarte.f(refspa=refspa, dataEnv=dataEnv, bbox=bbox)

        if (( ! getOption("P.graphPaper")) && getOption("P.title"))
        {
            mtext(mainTitle, line=0.5, cex=1.8)
        }else{}

        ## Affichage des zones de regroupement spatial :
        unitobs <- get("unitobs", envir=dataEnv)

        polyZones <- plot.spatialZones.f(refspa=refspa, unitobs=unitobs, Data=tmpData,
                                         fact=factSpatial, factSel=factSpatialSel,
                                         plot=FALSE)

        ## Ajout des subplots :
        switch(graphType,
               "symboles"=
           {
               tryCatch(## boxplotCarto.generic.f(polyZones=polyZones, Data=tmpData,
                        ##                        factSpatial=factSpatial, exprBP=exprBP)
                        symbols(x=coordinates(polyZones)[ , 1],
                                y=coordinates(polyZones)[ , 2],
                                circles=tapply(tmpData[ , metrique], tmpData[ , factSpatial], mean),
                                inches=0.5, fg="red",
                                add=TRUE)
                        ,
                        error=function(e)
                    {
                        warning("Un graphique n'a pu être créé correctement :\n", e, immediate.=TRUE)
                    })
           },
               "couleurs"=
           {
               tryCatch(## barplotCarto.generic.f(polyZones=polyZones, Data=tmpData,
                        ##                        factSpatial=factSpatial,
                        ##                        metrique=metrique, listFact=listFact)
                    {
                        rgb.palette <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow",
                                                          "#FF7F00", "red", "#7F0000"), space = "rgb")
                    },
                        error=function(e)
                    {
                        warning("Un graphique n'a pu être créé correctement :\n", e, immediate.=TRUE)
                    })
           })

        ## ##################################################
        ## Sauvegarde des données :
        if (getOption("P.saveData"))
        {
            writeData.f(filename=graphFile, Data=tmpData,
                        cols=NULL)
        }else{}

        ## Sauvegarde des infos sur les données et statistiques :
        if (getOption("P.saveStats"))
        {
            infoStats.f(filename=graphFile, Data=tmpData, agregLevel="unitobs", type="graph",
                        metrique=metrique, factGraph=factGraph, factGraphSel=factGraphSel,
                        listFact=NULL, listFactSel=NULL, # On les remets dans un ordre intuitif.
                        dataEnv=dataEnv, baseEnv=baseEnv)
        }else{}

        ## On ferme les périphériques PDF :
        if (getOption("P.graphPDF") || isTRUE(getOption("P.graphPNG")))
        {
            dev.off()

            ## Inclusion des fontes dans le pdf si souhaité :
            if (getOption("P.graphPDF") && getOption("P.pdfEmbedFonts"))
            {
                tryCatch(embedFonts(file=graphFile),
                         error=function(e)
                     {
                         warning("Impossible d'inclure les fontes dans le PDF !")
                     })
            }

        }else{
            if (.Platform$OS.type == "windows" && isTRUE(getOption("P.graphWMF")))
            {
                ## Sauvegarde en wmf si pertinent et souhaité :
                savePlot(graphFile, type="wmf", device=dev.cur())
            }else{}
        }
    }
}





### Local Variables:
### ispell-local-dictionary: "english"
### fill-column: 120
### End:
