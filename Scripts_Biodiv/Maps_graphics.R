#-*- coding: latin-1 -*-
# Time-stamp: <2018-12-09 12:15:18 yreecht>

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

### File: Maps_graphics.R
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
agregationSpatiale.f <- function(Data, metrique, facteur, dataEnv)
{
    ## Purpose:
    ## ----------------------------------------------------------------------
    ## Arguments: Data : Le jeu de données à agréger.
    ##            metrique : la métrique agrégée.
    ##            facteurs : le facteur
    ##            dataEnv : l'environnement des données.
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date:  2 mai 2013, 16:52

    ## Informations (l'étape peut être longue) :
    WinInfo <- agregation.info.f()

    ## traitements selon le type de métrique :
    casMetrique <- c("nombre"="mean",
                     "taille_moyenne"="w.mean.n",
                     "taille_moy"="w.mean",
                     "biomasse"="w.mean",
                     "Biomasse"="w.mean",
                     "poids"="mean",
                     "poids_moyen"="w.mean",
                     "densite"="w.mean",
                     "Densite"="w.mean",
                     "CPUE"="w.mean",
                     "CPUEbiomasse"="w.mean",
                     "pres_abs"="pres",
                     "prop.abondance.CL"="w.mean",
                     "prop.biomasse.CL"="w.mean",
                     ## Benthos :
                     "colonie"="mean",
                     "recouvrement"="w.mean",
                     "taille.moy.colonies"="w.mean",
                     ## SVR (expérimental) :
                     "nombreMax"="max",
                     "nombreSD"="mean",
                     "densiteMax"="max",
                     "densiteSD"="w.mean",
                     "biomasseMax"="max",
                     "reussite.ponte"="w.mean",
                     "pontes"="mean",
                     "traces.lisibles"="mean",
                     "nombre.traces"="mean",
                     ## Biodiversité :
                     "Delta"="mean",
                     "DeltaEtoile"="mean",
                     "DeltaPlus"="mean",
                     "hill"="mean",
                     "LambdaPlus"="mean",
                     "l.simpson"="mean",
                     "pielou"="mean",
                     "richesse_specifique"="mean",
                     "RS.relative.donnees"="mean",
                     "RS.relative.region"="mean",
                     "RS.relative.region.phylum"="mean",
                     "RS.relative.site"="mean",
                     "RS.relative.site.phylum"="mean",
                     "SDeltaPlus"="mean",
                     "simpson"="mean")


    ## Ajout des dimensions d'unitobs si moyennes pondérées :
    if (casMetrique[metrique] == "w.mean")
    {
        unitobs <- get("unitobs", envir=dataEnv)

        Data <- cbind(Data,
                      data.frame(dimobs=do.call("*",
                                                unitobs[match(Data[ , "unite_observation"],
                                                              unitobs[ , "unite_observation"]),
                                                        c("DimObs1", "DimObs2")])))

        ## Les dimobs peuvent être des NAs (notamment dans les cas SVR) :
        if (all(is.na(Data[ , "dimobs"])))
        {
            Data[ , "dimobs"] <- 1
        }else{}

    }else{}

    ## Agrégation de la métrique selon le facteur spatial :
    switch(casMetrique[metrique],
           "mean"={
               res <- tapply(Data[ , metrique],
                             as.list(Data[ , facteur, drop=FALSE]),
                             function(x)
                         {
                             ifelse(all(is.na(x)),
                                    NA,
                                    mean(x, na.rm=TRUE))
                         })
           },
           "w.mean"={
               res <- tapply(1:nrow(Data),
                             as.list(Data[ , facteur, drop=FALSE]),
                             function(ii)
                         {
                             ifelse(all(is.na(Data[ii, metrique])),
                                    NA,
                                    weighted.mean(Data[ii, metrique],
                                                  Data[ii, "dimobs"],
                                                  na.rm=TRUE))
                         })
           },
           stop("Not implemented!")
           )

    ## Nom des dimensions
    names(dimnames(res)) <- c(facteur)

    ## Transformation vers format long :
    reslong <- as.data.frame(as.table(res), responseName=metrique)
    reslong <- reslong[ , c(tail(colnames(reslong), 1), head(colnames(reslong), -1))] # métrique en première.

    ## Fermeture de la fenêtre d'information
    close.info.f(WinInfo)

    return(reslong)
}


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
                        mltext("graphTitle.vals"),
                        paste(model, mltext("graphTitle.for"), varNames.f(metrique, "article"), sep="")),
                 varNames.f(metrique, "nom"),
                 ifelse(is.element(type, c("espece", "unitobs", "CL_espece", "unitobs(CL)")),
                        paste(mltext("graphTitle.agg"),
                              switch(varNames.f(metrique, "genre"), # Accord de "agrégé".
                                     f=mltext("graphTitle.f"),
                                     fp=mltext("graphTitle.fp"),
                                     mp=mltext("graphTitle.mp"), ""), sep=""),
                        ""),
                 switch(type,
                        "espece"=mltext("graphTitle.bySpSt"),
                        "CL_espece"=mltext("graphTitle.bySCSpSt"),
                        "unitobs"=mltext("graphTitle.bySt"),
                        "unitobs(CL)"=mltext("graphTitle.byStSC"),
                        "CL_unitobs"=mltext("graphTitle.bySCSt"),
                        "biodiv"=mltext("graphTitle.biodiv"),
                        "spEspece"=paste(mltext("graphTitle.maps.SpZone"),
                                         " '", varNames.f(factSpatial, "name", quote=FALSE), "'", sep=""),
                        "spCL_espece"=paste(mltext("graphTitle.maps.SCSpZone"),
                                            " '",
                                            varNames.f(factSpatial, "name", quote=FALSE), "'", sep=""),
                        "spUnitobs"=,"spUnitobs(CL)"=paste(" par zone '",
                                                           varNames.f(factSpatial, "name", quote=FALSE), "'", sep=""),
                        "spCL_unitobs"=paste(mltext("graphTitle.maps.SCZone"),
                                             " '",
                                             varNames.f(factSpatial, "name", quote=FALSE), "'", sep=""),
                        ""),
                 switch(type,
                        "spEspece"=,"spCL_espece"=,"spUnitobs"=,"spUnitobs(CL)"=,
                        "spCL_unitobs"=mltext("graphTitle.maps.agg.Zone"),
                        paste(mltext("graphTitle.maps.per.Zone"),
                              " '", varNames.f(factSpatial, "nom", quote=FALSE), "'", sep="")),
                 switch(type,
                        "espece"=,"CL_espece"=,"spEspece"=,"spCL_espece"={
                            ifelse(modGraphSel == "", # Facteur de séparation uniquement si défini.
                                   "",
                                   paste(mltext("graphTitle.maps.mod.field"),
                                         " '", factGraph, "' = ", modGraphSel, sep=""))
                        },
                        "unitobs"=,"CL_unitobs"=,"spUnitobs"=,"spCL_unitobs"={
                            ifelse(modGraphSel[1] == "", # Facteur de séparation uniquement si défini.
                                   mltext("graphTitle.maps.mod.allSp"),
                                   paste(mltext("graphTitle.maps.mod.selSp"),
                                         " '", factGraph, "' = (",
                                         paste(modGraphSel, collapse=", "), ")", sep=""))
                        },
                        "unitobs(CL)"=,"spUnitobs(CL)"={
                            ifelse(modGraphSel[1] == "", # Facteur de séparation uniquement si défini.
                                   mltext("graphTitle.maps.mod.allSC"),
                                   paste(mltext("graphTitle.maps.mod.selSC"),
                                         " '", factGraph, "' = (",
                                         paste(modGraphSel, collapse=", "), ")", sep=""))
                        },
                        "biodiv"={
                            ifelse(modGraphSel[1] == "", # Facteur de séparation uniquement si défini.
                                   "",
                                   paste(mltext("graphTitle.maps.mod.selSt"),
                                         " '", factGraph, "' = (",
                                         paste(modGraphSel, collapse=", "), ")", sep=""))
                        },
                        ""),
                 ifelse(listFact[1] == "",
                        "",
                        paste(mltext("graphTitle.by"),
                              paste(sapply(listFact[length(listFact):1],
                                           function(x)paste(c(## varNames.f(x, "article")
                                                              "",
                                                              varNames.f(x, "nom")), collapse="")),
                                    collapse="graphTitle.and"),
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
        baseMap <- tryCatch(unionSpatialPolygons(SpP=refspa,
                                                 IDs=ifelse(is.element(tolower(refspa@data[ , getOption("P.landField")]),
                                                                       getOption("P.landMods")),
                                                            "terre", "mer")),
                            error=function(e)
                        {
                            return(refspa)
                        })

        assign("baseMap", baseMap, envir=dataEnv)
    }

    if (is.null(bbox))
    {
        bbox <- bbox(refspa)
    }else{}

    tryCatch(plot(baseMap, axes=TRUE, col=getOption("P.landCols")[names(baseMap)],
                  lwd=lwd, xlim=bbox["x", ], ylim=bbox["y", ], ...),
             error=function(e)
         {
             plot(baseMap, axes=TRUE,
                  lwd=lwd, xlim=bbox["x", ], ylim=bbox["y", ], ...)
         })
}

########################################################################################################################
subsetRefspaToData.f <- function(refspa, unitobs, Data, fact="unite_observation")
{
    ## Purpose: réduire le référentiel spatial aux polygones présentant des
    ##          données.
    ## ----------------------------------------------------------------------
    ## Arguments: refspa : le référentiel spatial.
    ##            unitobs : la table d'unités d'observations.
    ##            Data : les données.
    ##            fact : facteur spatial (limité aux unités d'observations si
    ##                   non précisé)
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 15 févr. 2013, 17:38

    if (is.element("unite_observation", fact))
    {
        ## Réduire le référentiel spatial aux OBJECTID avec des données :
        dataOBJECTID <- unitobs[is.element(unitobs[ , "unite_observation"],
                                           Data[ , "unite_observation"]),
                                "OBJECTID"]

        refspa <- subset(refspa,
                         is.element(refspa@data[ , "OBJECTID"],
                                    dataOBJECTID))

    }else{
        if (is.element(fact, colnames(Data)))
        {
            refspa <- subset(refspa,
                             is.element(refspa@data[ , fact],
                                        Data[ , fact]))
        }else{
            commonCol <- colnames(refspa@data)[which(is.element(colnames(refspa@data), colnames(Data)))[1]]

            refspa <- subset(refspa,
                             is.element(refspa@data[ , commonCol],
                                        Data[ , commonCol]))
        }
    }

    return(refspa)
}


########################################################################################################################
plot.spatialZones.f <- function(refspa, unitobs, Data, fact, factSel, plot=TRUE, col=NULL)
{
    ## Purpose: afficher les zones sur un graphique.
    ## ----------------------------------------------------------------------
    ## Arguments: refspa : le référentiel spatial.
    ##            unitobs : la table d'unités d'observations.
    ##            Data : les données.
    ##            fact : facteur spatial.
    ##            factSel : (éventuelle) sélections sur le facteur spatial.
    ##            plot : doivent-elles être affichées ?
    ##            col : couleurs (NULL : palette par défaut).
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 17 déc. 2012, 18:39

    refspa <- subsetRefspaToData.f(refspa=refspa, unitobs=unitobs, Data=Data, fact=fact)

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

    ## Agrégation des zones à l'échelle souhaitée :
    polyZones <- unionSpatialPolygons(SpP=refspaTmp,
                                      IDs=refspaTmp@data[ , fact])

    ## Couleurs automatiques :
    if (is.null(col))
    {
        col <- PAMPAcolors.f(n=length(polyZones), palette=getOption("P.zonesPalette"))
    }else{}

    ## Affichage des zones :
    if (plot)
    {
        plot(polyZones,
             col=col,
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

    env <- environment()

    ## Barycentres des zones... corrigés pour tenir dans les graphiques :
    zonesCoord <- sweep(coordinates(polyZones), 2,
                        list(range(par("usr")[1:2]), range(par("usr")[3:4])),
                        function(x, y)
                    {
                        ymin <- apply(y, 2, sapply, min)
                        ymax <- apply(y, 2, sapply, max)
                        ydiff <- ymax - ymin
                        ymin <- ymin + sweep(ydiff, 2, c(0.08, 0.11), "*")
                        ymax <- ymax - sweep(ydiff, 2, c(0.08, 0.11), "*")
                        ##
                        res <- x
                        res[x < ymin] <- ymin[x < ymin] ## + 0.05 * ydiff[x < ymin]
                        res[x > ymax] <- ymax[x > ymax] ## - 0.05 * ydiff[x > ymax]
                        ##
                        return(res)
                    })

    ## Données séparées par subplot :
    tmpData <- split(Data, Data[ , factSpatial])

    ## Couleurs de diférentiation des zones :
    colZones <- PAMPAcolors.f(n=length(polyZones), palette=getOption("P.zonesPalette"))
    names(colZones) <- row.names(zonesCoord)

    ## Rectangles pour avoir un fond (transparent) aux subplots.
    for (i in row.names(zonesCoord))
    {#browser()
        subplot({plot(0:1, 0:1, xaxt="n", yaxt="n", type="n", xlab="", ylab="", bty="n") ;
                 rect(0, 0, 1, 1, col=rgb(1, 1, 1, 0.5))},
                zonesCoord[i, 1], zonesCoord[i, 2], size=getOption("P.pinSubplot"), type="plt")
    ## }

    ## ## Barplots :
    ## for (i in row.names(zonesCoord))
    ## {
        oldpars <- par(no.readonly=TRUE)

        tryCatch(subplot({BPtmp <- barplotPAMPA.f(metrique=metrique, listFact=listFact, Data=tmpData[[i]],
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
                          title(main=i);
                          ## Sauvegarde temporaire du barplot :
                          assign(x="BPtmp", value=BPtmp, envir=env)},
                         zonesCoord[i, 1], zonesCoord[i, 2],
                         size=getOption("P.pinSubplot"),  type="fig",
                         pars=list(bg="white", fg="black", cex=0.75, xpd=NA, ## mgp=c(1.5, 0.5, 0),
                                   tcl=-0.2##, mar=c(2.5, 4, 2, 1) + 0.1
                         )),
                 error=function(e){warning(e)})

        ## On rétabli les précédents paramètres pour éviter un mauvais placement des graphiques suivants :
        par(oldpars)
    }

    ## Legende du facteur de second niveau :
    if (length(dim(BPtmp$n)) == 2)
    {
        legend(x="topright", legend=row.names(BPtmp$n), fill=PAMPAcolors.f(n=nrow(BPtmp$n)),
               title=Capitalize.f(varNames.f(listFact[1], "nom")), xpd=NA)
    }else{}
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

    env <- environment()

    ## Barycentres des zones... corrigés pour tenir dans les graphiques :
    zonesCoord <- sweep(coordinates(polyZones), 2,
                        list(range(par("usr")[1:2]), range(par("usr")[3:4])),
                        function(x, y)
                    {
                        ymin <- apply(y, 2, sapply, min)
                        ymax <- apply(y, 2, sapply, max)
                        ydiff <- ymax - ymin
                        ymin <- ymin + sweep(ydiff, 2, c(0.08, 0.11), "*")
                        ymax <- ymax - sweep(ydiff, 2, c(0.08, 0.11), "*")
                        ##
                        res <- x
                        res[x < ymin] <- ymin[x < ymin] ## + 0.05 * ydiff[x < ymin]
                        res[x > ymax] <- ymax[x > ymax] ## - 0.05 * ydiff[x > ymax]
                        ##
                        return(res)
                    })

    ## Données séparées par subplot :
    tmpData <- split(Data, Data[ , factSpatial])

    ## Couleurs de diférentiation des zones :
    colZones <- PAMPAcolors.f(n=length(polyZones), palette=getOption("P.zonesPalette"))
    names(colZones) <- row.names(zonesCoord)

    metrique <- as.character(exprBP)[2]

    ## Rectangles pour avoir un fond (transparent) aux subplots.
    for (i in row.names(zonesCoord))
    {
        ## Cadre semi-transparent :
        subplot({plot(0:1, 0:1, xaxt="n", yaxt="n", type="n", xlab="", ylab="", bty="n") ;
                 rect(0, 0, 1, 1, col=rgb(1, 1, 1, 0.5))},
                zonesCoord[i, 1], zonesCoord[i, 2], size=getOption("P.pinSubplot"), type="plt")

        oldpars <- par(no.readonly=TRUE)

        ## subplot :
        tryCatch(subplot({BPtmp <- boxplotPAMPA.f(exprBP, data=tmpData[[i]],
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
                          title(main=i);
                          assign(x="BPtmp", value=BPtmp, envir=env)},
                         zonesCoord[i, 1], zonesCoord[i, 2],
                         size=getOption("P.pinSubplot"),  type="fig",
                         pars=list(bg="white", fg="black", cex=0.75, xpd=NA, ## mgp=c(1.5, 0.5, 0),
                         tcl=-0.2##, mar=c(2.5, 4, 2, 1) + 0.1
                         )),
                 error=function(e){warning(e)})

        ## On rétabli les précédents paramètres pour éviter un mauvais placement des graphiques suivants :
        par(oldpars)
    }

    ## Légende du facteur de second niveau :
    legendBoxplot.f(terms=attr(terms(exprBP), "term.labels"), data=Data, cex=1)
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
                        warning(mltext("subplotCarto.W.1"), e, immediate.=TRUE)
                    })
           },
               "barplot"=
           {
               tryCatch(barplotCarto.generic.f(polyZones=polyZones, Data=tmpDataMod,
                                               factSpatial=factSpatial,
                                               metrique=metrique, listFact=listFact),
                        error=function(e)
                    {
                        warning(mltext("subplotCarto.W.1"), e, immediate.=TRUE)
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
                                listFact=c(factSpatial, rev(listFact)),
                                listFactSel=c(list(factSpatialSel), rev(listFactSel)), # On les remets dans un ordre
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
                                listFact=c(factSpatial, rev(listFact)),
                                listFactSel=c(list(factSpatialSel), rev(listFactSel)), # On les remets dans un ordre
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
                        listFact=c(factSpatial, rev(listFact)),
                        listFactSel=c(list(factSpatialSel), rev(listFactSel)), # On les remets dans un ordre intuitif.
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
                         warning(mltext("WP2boxplot.W.pdfFonts"))
                     })

                i <- i + 1
            }
        }else{}
    }else{}

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
        warning(mltext("WP2boxplot.W.n.1"),
                "(", paste(iFactGraphSel, collapse=", "), ") < ", getOption("P.MinNbObs"),
                mltext("WP2boxplot.W.n.2"))
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
        ##                parse(text=paste("\"", Capitalize.f(varNames[metrique, "nom"]), "\"",
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
                        warning(mltext("subplotCarto.W.1"), e, immediate.=TRUE)
                    })
           },
               "barplot"=
           {
               tryCatch(barplotCarto.generic.f(polyZones=polyZones, Data=tmpData,
                                               factSpatial=factSpatial,
                                               metrique=metrique, listFact=listFact),
                        error=function(e)
                    {
                        warning(mltext("subplotCarto.W.1"), e, immediate.=TRUE)
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
                        listFact=c(factSpatial, rev(listFact)),
                        listFactSel=c(list(factSpatialSel), rev(listFactSel)), # On les remets dans un ordre intuitif.
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
                         warning(mltext("WP2boxplot.W.pdfFonts"))
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
symbolsCarto.generic.f <- function(Data, metrique, polyZones)
{
    ## Purpose: Fonction générique pour ajouter des symbols de taille variable
    ##          sur une carte.
    ## ----------------------------------------------------------------------
    ## Arguments: Data : les données agrégées.
    ##            metrique : la métrique à représenter.
    ##            polyZones : les polygones agrégés selon le facteur spatial.
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date:  7 mai 2013, 17:45

    ## Affichage des symboles :
    symbols(x=coordinates(polyZones)[ , 1],
            y=coordinates(polyZones)[ , 2],
            circles=Data[ , metrique],
            inches=0.5, fg="red",
            add=TRUE)

    ## Titre de légende :
    legendTitle <- parse(text=paste("'", Capitalize.f(varNames.f(metrique, "nom")), "'",
                                    sep=""))

    ## Unité de la légende :
    legendUnit <- parse(text=ifelse(varNames[metrique, "unite"] != "",
                                    paste("(", varNames[metrique, "unite"], ")", sep=""),
                                    ""))

    ## Valeurs pour la légende (échelle de taille) :
    legVals <- tail(pretty(x=c(0, Data[ , metrique]), n=3), -1)
    ## message(legVals)

    ## Calcul du rayon maximal du cercle pour la légende :
    maxInches <- 0.5 * max(legVals) / max(Data[ , metrique], na.rm=TRUE)

    ## Réctangle de la légende :
    rect(xleft=max(par("usr")[1:2]) -
                  max(2.7 * unitConvX.f(x=maxInches, from="inches", to="user") +
                         max(strwidth(as.character(legVals))),
                      max(strwidth(c(legendTitle, legendUnit))) +
                         0.6 * unitConvX.f(x=maxInches, from="inches", to="user")),
         xright=max(par("usr")[1:2]) -
                   0.4 * unitConvX.f(x=maxInches, from="inches", to="user"),
         ytop=max(par("usr")[3:4]) -
                 strheight(" ") * 1.5,
         ybottom=max(par("usr")[3:4]) -
                    strheight(" ") * 1.5 -
                    strheight(legendTitle) * 1.5 -
                    ifelse(length(strheight(legendUnit)), strheight(legendUnit) * 1.0, 0) -
                    (length(legVals) + 1.2) * 1 * unitConvY.f(x=maxInches, from="inches", to="user"),
         col="white")

    ## Symboles de la légende :
    symbols(x= rep(max(par("usr")[1:2]) -
                      1.5 * unitConvX.f(x=maxInches, from="inches", to="user"),
                   times=length(legVals)),
            y= max(par("usr")[3:4]) -
                  strheight(" ") * 1.5 -
                  strheight(legendTitle) * 1.5 -
                  ifelse(length(strheight(legendUnit)), strheight(legendUnit) * 1.0, 0) -
                  seq_along(legVals) * 1 *unitConvY.f(x=maxInches, from="inches", to="user"),
            circles=legVals,
            inches=maxInches, fg="red",
            add=TRUE)

    ## Affichage des valeurs de l'échelle :
    text(x=max(par("usr")[1:2]) -
              2.6 * unitConvX.f(x=maxInches, from="inches", to="user"),
         y= max(par("usr")[3:4]) -
               strheight(" ") * 1.5 -
               strheight(legendTitle) * 1.5 -
               ifelse(length(strheight(legendUnit)), strheight(legendUnit) * 1.0, 0) -
               seq_along(legVals) * 1 *unitConvY.f(x=maxInches, from="inches", to="user"),
         labels=as.character(legVals), adj=c(1, 0.5))

    ## Titre de la légende (métrique + unité) :
    text(x=max(par("usr")[1:2]) -
              max(2.6 * unitConvX.f(x=maxInches, from="inches", to="user") +
                     max(strwidth(as.character(legVals))),
                  max(strwidth(c(legendTitle, legendUnit))) +
                     0.5 * unitConvX.f(x=maxInches, from="inches", to="user")),
         y=c(max(par("usr")[3:4]) -
                strheight(" ") * 1.5 -
                strheight(legendTitle) * 1.5 -
                ifelse(length(strheight(legendUnit)), strheight(legendUnit) * 1.0, 0),
             max(par("usr")[3:4]) -
               strheight(" ") * 1.5 -
               strheight(legendTitle) * 1.5),
         labels=c(legendUnit, legendTitle),
         adj=c(0 , 1))
}

########################################################################################################################
colorsCarto.generic.f <- function(Data, DataNoSp, metrique, polyZones, refspa, unitobs,
                                  factSpatial, factSpatialSel)
{
    ## Purpose: Fonction générique pour ajouter des symbols de taille variable
    ##          sur une carte.
    ## ----------------------------------------------------------------------
    ## Arguments: Data : les données agrégées.
    ##            DataNoSp : données sans agrégation spatiale.
    ##            metrique : la métrique à représenter.
    ##            polyZones : les polygones agrégés selon le facteur spatial.
    ##            refspa : référentiel spatial.
    ##            unitobs : table des unités d'observation.
    ##            factSpatial : facteur spatial.
    ##            factSpatialSel : éventuelle sélection du facteur spatial.
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date:  7 mai 2013, 17:54

    ## rgb.palette <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow",
    ##                                   "#FF7F00", "red", "#7F0000"), space = "rgb")
    rgb.palette <- colorRampPalette(gray.colors(n=2), space="rgb")

    ## discrétisation de la palette de couleur (1001 modalités) :
    N <- 200
    cols <- rgb.palette(n=N)

    ## Calcul de l'indice de couleur correspondant à chaque valeur (utilisation de toute la plage) :
    colsValIdx <- as.integer(((Data[ , metrique] -
                               min(Data[ , metrique], na.rm=TRUE)) /
                              (max(Data[ , metrique], na.rm=TRUE) -
                               min(Data[ , metrique], na.rm=TRUE))) * (N - 1)) + 1

    ## Affichage des zones avec les couleurs adéquates :
    invisible(plot.spatialZones.f(refspa=refspa, unitobs=unitobs, Data=DataNoSp,
                                  fact=factSpatial, factSel=factSpatialSel,
                                  plot=TRUE, col=cols[colsValIdx]))

    ## Nouveau graphique "vide" pour la légende :
    mai <- par("mai")
    par(mar=c(2-1, 2,
              ifelse(( ! getOption("P.graphPaper")) && getOption("P.title"),
                     6.5, 1)-1,
              0.1))

    blank.plot.f(y=c(min(Data[ , metrique], na.rm=TRUE),
                     max(Data[ , metrique], na.rm=TRUE)))

    ## Construction de la légende de couleur :
    sapply(1:N,
           function(i)
       {
           polygon(c(-0.1, -0.1, 1, 1),
                   c((i-1)/N, i/N, i/N, (i-1)/N) *
                      diff(range(Data[ , metrique], na.rm=TRUE)) +
                      min(Data[ , metrique], na.rm=TRUE),
                   col = cols[N-i+1],
                   border=NA )
       })

    axis(side=2)

    ## Titre de légende :
    legendTitle <- parse(text=paste("\"", Capitalize.f(varNames[metrique, "nom"]), "\"",
                                    sep=""))

    ## Unité de la légende :
    legendUnit <- parse(text=ifelse(varNames[metrique, "unite"] != "",
                                    paste("(", varNames[metrique, "unite"], ")", sep=""),
                                    ""))

    text(x=max(par("usr")[1:2]) - max(strwidth(c(legendTitle, legendUnit))),
         y=max(par("usr")[3:4]) + c(0, strheight(legendUnit) * 1.0),
         labels=c(legendUnit, legendTitle), xpd=NA, adj=c(0, 1))
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

    pampaProfilingStart.f()

    ## Nettoyage des facteurs (l'interface de sélection produit des valeurs vides) :
    ## listFactSel <- listFactSel[unlist(listFact) != ""]
    ## listFactSel <- listFactSel[length(listFactSel):1]

    ## listFact <- listFact[unlist(listFact) != ""]
    ## listFact <- listFact[length(listFact):1]

    ## Concaténation
    facteurs <- c(factSpatial, factGraph) # Concaténation des facteurs

    selections <- c(list(factSpatialSel), list(factGraphSel)) # Concaténation des leurs listes de modalités
                                        # sélectionnées

    ## Données pour la série de boxplots :
    tmpData <- subsetToutesTables.f(metrique=metrique, facteurs=facteurs, selections=selections,
                                    dataEnv=dataEnv, tableMetrique=tableMetrique, exclude = NULL)

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
            warning(mltext("WP2boxplot.W.n.1"), modGraphSel, " < ", getOption("P.MinNbObs"),
                    mltext("WP2boxplot.W.n.2"))

            plotted <- FALSE
            next()
        }else{
            plotted <- TRUE
        }

        ## Suppression des 'levels' non utilisés :
        tmpDataMod <- dropLevels.f(tmpDataMod)

        ## Agrégation spatiale (simple) :
        tmpDataMod2 <- agregationSpatiale.f(Data=tmpDataMod, metrique=metrique, facteur=factSpatial, dataEnv=dataEnv)

        ## Sauvegarde temporaire des données :
        DataBackup[[modGraphSel]] <<- tmpDataMod2

        ## Ouverture et configuration du périphérique graphique :
        graphFileTmp <- openDevice.f(noGraph=which(modGraphSel == iFactGraphSel),
                                     metrique=metrique,
                                     factGraph=factGraph,
                                     modSel=modGraphSel, # la modalité courante uniquement.
                                     listFact=factSpatial,
                                     dataEnv=dataEnv,
                                     type=switch(tableMetrique, # différents types de graphs en fonction de la table de
                                        # données.
                                                 "unitSp"={"espece"},
                                                 "unitSpSz"={"CL_espece"},
                                                 "unit"={"unitobs"},
                                                 "espece"),
                                     typeGraph=paste("carte_",
                                                     ifelse(graphType == "symboles", "symboles", "couleurs"), sep=""))

        ## graphFile uniquement si nouveau fichier :
        if (!is.null(graphFileTmp)) graphFile <- graphFileTmp

        ## espace à droite pour légende des niveaux de couleurs :
        if (graphType == "couleurs")
        {
            layout(mat=matrix(c(1, 2), nrow=1), width=c(10, 0.8))
        }else{}

        ## Paramètres graphiques :
        par(mar=c(2, 2,
                  ifelse(( ! getOption("P.graphPaper")) && getOption("P.title"),
                         6.5, 1),
                  1),
            mgp=c(3.5, 1, 0))
        ## ################################################

        ## Titre (d'après les métriques, modalité du facteur de séparation et facteurs de regroupement) :
        mainTitle <- graphTitle.carto.f(metrique=metrique,
                                        modGraphSel=modGraphSel, factGraph=factGraph,
                                        listFact=NULL,
                                        factSpatial=factSpatial,
                                        type=switch(tableMetrique, # différents types de graphs en fonction de la table de
                                        # données.
                                                    "unitSp"={"spEspece"},
                                                    "unitSpSz"={"spCL_espece"},
                                                    "spEspece"))

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

        polyZones <- plot.spatialZones.f(refspa=refspa, unitobs=unitobs, Data=tmpDataMod,
                                         fact=factSpatial, factSel=factSpatialSel,
                                         plot=FALSE)

        ## Ajout des subplots :
        switch(graphType,
               "symboles"=
           {
               tryCatch(## Symboles de taille variable :
                    {
                        symbolsCarto.generic.f(Data=tmpDataMod2, metrique=metrique, polyZones=polyZones)
                    },
                        error=function(e)
                    {
                        warning(mltext("symbolCarto.W.1"), e, immediate.=TRUE)
                    })
           },
               "couleurs"=
           {
               tryCatch(## Échelle de couleurs :
                    {
                        colorsCarto.generic.f(Data=tmpDataMod2, DataNoSp=tmpDataMod, metrique=metrique,
                                              polyZones=polyZones,
                                              refspa=refspa, unitobs=unitobs, factSpatial=factSpatial,
                                              factSpatialSel=factSpatialSel)
                    },
                        error=function(e)
                    {
                        warning(mltext("symbolCarto.W.1"), e, immediate.=TRUE)
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
                    writeData.f(filename=graphFile, Data=tmpDataMod2,
                                cols=NULL)
                }else{}

                ## Sauvegarde des statistiques :
                if (getOption("P.saveStats"))
                {
                    infoStats.f(filename=graphFile, Data=tmpDataMod2,
                                agregLevel=ifelse(tableMetrique == "unitSpSz", "spCL_espece", "spSpecies"),
                                type="graph",
                                metrique=metrique, factGraph=factGraph, factGraphSel=modGraphSel,
                                listFact=factSpatial, listFactSel=factSpatialSel,
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
                    writeData.f(filename=graphFile, Data=tmpDataMod2,
                                cols=NULL)
                }else{}

                ## Sauvegarde des statistiques :
                if (getOption("P.saveStats"))
                {
                    infoStats.f(filename=graphFile, Data=tmpDataMod2,
                                agregLevel=ifelse(tableMetrique == "unitSpSz", "spCL_espece", "spSpecies"),
                                type="graph",
                                metrique=metrique, factGraph=factGraph, factGraphSel=modGraphSel,
                                listFact=factSpatial, listFactSel=factSpatialSel,
                                        # intuitif.
                                dataEnv=dataEnv, baseEnv=baseEnv)
                }else{}
            }else{}
        }

    }  ## Fin de boucle graphique.
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

    ## Données pour la série :
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

    ## Suppression des 'levels' non utilisés :
    tmpData <- dropLevels.f(tmpData)

    ## Agrégation spatiale (simple) :
    tmpData2 <- agregationSpatiale.f(Data=tmpData, metrique=metrique, facteur=factSpatial, dataEnv=dataEnv)

    ## Sauvegarde temporaire des données utilisées pour les graphiques (attention : écrasée à chaque nouvelle série de
    ## graphiques) :
    DataBackup <<- list(tmpData2)

    ## Création du graphique si le nombre d'observations  < au minimum défini dans les options :
    if (nrow(tmpData2) < getOption("P.MinNbObs"))
    {
        warning(mltext("WP2boxplot.W.n.1"), "(", paste(iFactGraphSel, collapse=", "), ") < ", getOption("P.MinNbObs"),
                mltext("WP2boxplot.W.n.2"))
    }else{
        ## browser()
        ## Ouverture et configuration du périphérique graphique :
        graphFile <- openDevice.f(noGraph=1,
                                  metrique=metrique,
                                  factGraph=factGraph,
                                  modSel=iFactGraphSel,
                                  listFact=factSpatial,
                                  dataEnv=dataEnv,
                                  type=ifelse(tableMetrique == "unitSpSz" && factGraph != "classe_taille",
                                              "CL_unitobs",
                                              "unitobs"),
                                  typeGraph=paste("carte_",
                                                  ifelse(graphType == "symboles", "symboles", "couleurs"), sep=""))

        ## espace à droite pour légende des niveaux de couleurs :
        if (graphType == "couleurs")
        {
            layout(mat=matrix(c(1, 2), nrow=1), width=c(10, 0.8))
        }else{}

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
                                                    "spCL_unitobs",
                                                    ifelse(tableMetrique == "unitSpSz",
                                                           "spUnitobs(CL)",
                                                           "spUnitobs")))

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
               tryCatch(## Symboles de taille variable :
                    {
                        symbolsCarto.generic.f(Data=tmpData2, metrique=metrique, polyZones=polyZones)
                    },
                        error=function(e)
                    {
                        warning(mltext("symbolCarto.W.1"), e, immediate.=TRUE)
                    })
           },
               "couleurs"=
           {
               tryCatch(## Échelle de couleurs :
                    {
                        colorsCarto.generic.f(Data=tmpData2, DataNoSp=tmpData, metrique=metrique, polyZones=polyZones,
                                              refspa=refspa, unitobs=unitobs, factSpatial=factSpatial,
                                              factSpatialSel=factSpatialSel)
                    },
                        error=function(e)
                    {
                        warning(mltext("symbolCarto.W.1"), e, immediate.=TRUE)
                    })
           })

        ## ##################################################
        ## Sauvegarde des données :
        if (getOption("P.saveData"))
        {
            writeData.f(filename=graphFile, Data=tmpData2,
                        cols=NULL)
        }else{}

        ## Sauvegarde des infos sur les données et statistiques :
        if (getOption("P.saveStats"))
        {
            infoStats.f(filename=graphFile, Data=tmpData2,
                        agregLevel=ifelse(tableMetrique == "unitSpSz" && factGraph != "classe_taille",
                                          "spCL_unitobs",
                                          ifelse(tableMetrique == "unitSpSz",
                                                 "spUnitobs(CL)",
                                                 "spUnitobs")),
                        type="graph",
                        metrique=metrique, factGraph=factGraph, factGraphSel=factGraphSel,
                        listFact=factSpatial, listFactSel=factSpatialSel,
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
                         warning(mltext("WP2boxplot.W.pdfFonts"))
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
