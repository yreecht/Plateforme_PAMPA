#-*- coding: latin-1 -*-
# Time-stamp: <2013-01-10 17:48:14 yves>

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
plot.spatialZones.f <- function(refspa, unitobs, fact, factSel)
{
    ## Purpose: afficher les zones sur un graphique.
    ## ----------------------------------------------------------------------
    ## Arguments:
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 17 déc. 2012, 18:39

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

    plot(polyZones,
         col=PAMPAcolors.f(n=length(polyZones), palette="carto1"),
         add=TRUE)

    return(polyZones)
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


    zonesCoord <- coordinates(polyZones)

    tmpData <- split(Data, Data[ , factSpatial])

    ## Les couleurs pour l'identification des modalités du facteur de second niveau :
    colors <- colBoxplot.f(terms=attr(terms(exprBP), "term.labels"), data=Data)

    colZones <- PAMPAcolors.f(n=length(polyZones), palette="carto1")
    names(colZones) <- row.names(zonesCoord)

    metrique <- as.character(exprBP)[2]

    # browser()
    ## Rectangles pour avoir un fond (transparent) aux subplots.
    for (i in row.names(zonesCoord))
    {
        subplot({plot(0:1, 0:1, xaxt="n", yaxt="n", type="n", xlab="", ylab="", bty="n") ;
                 rect(0, 0, 1, 1, col=rgb(1, 1, 1, 0.7))},
                zonesCoord[i, 1], zonesCoord[i, 2], size=getOption("P.pinSubplot"), type="plt")
    }

    ## Barplots :
    for (i in row.names(zonesCoord))
    {
        oldpars <- par(no.readonly=TRUE)

        tryCatch(subplot({boxplotPAMPA.f(exprBP, data=tmpData[[i]],
                                         main=i, cex=par("cex"),
                                         col.main="black");
                          title(main=i, adj=c(0.505), line=1.03, col.main=colZones[i])},
                         zonesCoord[i, 1], zonesCoord[i, 2],
                         size=getOption("P.pinSubplot"),  type="fig",
                         pars=list(bg="white", fg="black", cex=0.7, xpd=NA## , mgp=c(1.5, 0.5, 0),
                         ## tcl=-0.3, mar=c(2.5, 4, 2, 1) + 0.1
                         )),
                 error=function(e){warning(e)})

        ## On rétabli les précédents paramètres pour éviter un mauvais placement des graphiques suivants :
        par(oldpars)
    }
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
        tmpData <- subsetToutesTables.f(metrique="nombre", facteurs=facteurs,
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
                                               nombres = "nombre",
                                               indices=metrique,
                                               dataEnv=dataEnv)
                              }))


            ## On rajoute les anciennes colonnes :
            tmpData <- cbind(tmp[ , colnames(tmp) != "nombre"], # Colonne "nombre" désormais inutile.
                             tmpData[match(tmp$unite_observation, tmpData$unite_observation),
                                     !is.element(colnames(tmpData),
                                                 c(colnames(tmp), "nombre", "code_espece")), drop=FALSE])
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
                                  typeGraph="carte_boxplot")

        par(mar=c(2, 2, 8, 1), mgp=c(3.5, 1, 0)) # paramètres graphiques.

        ## Titre (d'après les métriques, modalité du facteur de séparation et facteurs de regroupement) :[!!!]
        mainTitle <- graphTitle.f(metrique=metrique,
                                  modGraphSel=iFactGraphSel,
                                  factGraph=factGraph,
                                  listFact=listFact,
                                  type=ifelse(tableMetrique == "unitSpSz" && factGraph != "classe_taille",
                                              "CL_unitobs",
                                              ifelse(tableMetrique == "unitSpSz",
                                                     "unitobs(CL)",
                                                     "unitobs")))

        ## Label axe y :
        ylab <- ifelse(getOption("P.axesLabels"),
                       parse(text=paste("'", Capitalize.f(varNames[metrique, "nom"]), "'",
                             ifelse(varNames[metrique, "unite"] != "",
                                    paste("~~(", varNames[metrique, "unite"], ")", sep=""),
                                    ""),
                             sep="")),
                       "")

        ## Graphiques :
        refspa <- get("refspa", envir=dataEnv)

        ## Affichage du fond de carte :
        plot.fondCarte.f(refspa=refspa, dataEnv=dataEnv, bbox=bbox)

        mtext(mainTitle, line=1, cex=1.8)

        ## Affichage des zones de regroupement spatial :
        polyZones <- plot.spatialZones.f(refspa=refspa, unitobs=get("unitobs", envir=dataEnv),
                                         fact=factSpatial, factSel=factSpatialSel)

        ## Ajout des subplots :
        switch(graphType,
               "boxplot"=
           {
               boxplotCarto.generic.f(polyZones=polyZones, Data=tmpData,
                                      factSpatial=factSpatial, exprBP=exprBP)
           },
               "barplot"=
           {
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







### Local Variables:
### ispell-local-dictionary: "english"
### fill-column: 120
### End:
