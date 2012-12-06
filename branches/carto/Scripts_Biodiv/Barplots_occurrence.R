#-*- coding: latin-1 -*-

## Plateforme PAMPA de calcul d'indicateurs de ressources & biodiversité
##   Copyright (C) 2008-2010 Ifremer - Tous droits réservés.
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

### File: barplots_occurrence.R
### Time-stamp: <2012-01-10 18:12:45 yreecht>
###
### Author: Yves Reecht
###
####################################################################################################
### Description:
###
### Fonctions pour créer des représentations (barplots) des fréquences d'occurrences relatives (%)
### (utilise certaines fonctions de ./boxplot_generique_calc.R)
####################################################################################################

barplotOccurrence.f <- function(factGraph, factGraphSel, listFact, listFactSel, dataEnv,
                                baseEnv=.GlobalEnv)
{
    ## Purpose: création des barplots d'après les sélections de facteurs et
    ##          modalités.
    ## ----------------------------------------------------------------------
    ## Arguments: factGraph : le facteur de séparation des graphiques.
    ##            factGraphSel : la sélection de modalités pour ce dernier.
    ##            listFact : liste du (des) facteur(s) de regroupement.
    ##            listFactSel : liste des modalités sélectionnées pour ce(s)
    ##                          dernier(s).
    ##            dataEnv : environnement de stockage des données.
    ##            baseEnv : environnement de l'interface principale.
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
                                    dataEnv=dataEnv, tableMetrique="TablePresAbs", exclude = NULL)

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
                                     type="espece",
                                     typeGraph="barplot")

        ## graphFile uniquement si nouveau fichier :
        if (!is.null(graphFileTmp)) graphFile <- graphFileTmp

        ## Titre (d'après les métriques, modalité du facteur de séparation et facteurs de regroupement) :
        if ((! isTRUE(getOption("P.graphPaper"))) && isTRUE(getOption("P.title")))
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
        ## Marge dynamiques (adaptation à la longueur des labels) :
        optim(par=unlist(par("mai")),   # Le rapport inch/ligne est modifié en changeant les marges => besoin
                                        # de l'optimiser.
              fn=function(x)
          {
              par(mai=c(
                  ## Marge du bas :
                  lineInchConvert.f()$V * cex * unlist(par("lheight")) * 4.5,
                  ## Marge de gauche dynamique :
                  tmp2 <- ifelse((tmp <- lineInchConvert.f()$H * cex * unlist(par("lheight")) * (1.4 +0.4 + 0.9) + # marge
                                        # supplémentaire.
                                  max(strDimRotation.f(as.graphicsAnnot(pretty(range(heights, na.rm=TRUE))),
                                                       srt=0,
                                                       unit="inches",
                                                       cex=cex)$width, na.rm=TRUE)) > 0.7 * unlist(par("pin"))[1],
                                 0.7 * unlist(par("pin"))[1],
                                 tmp),
                  ## Marge supérieure augmentée s'il y a un titre :
                  ifelse(isTRUE(getOption("P.graphPaper")) || (! isTRUE(getOption("P.title"))) ,
                         3 * lineInchConvert.f()$V,
                         8 * lineInchConvert.f()$V),
                  ## Marge de droite :
                  lineInchConvert.f()$H * cex * unlist(par("lheight")) * 7) +
                  lineInchConvert.f()$H * cex * unlist(par("lheight")) * 0.1,
                  ## Distance du nom d'axe dépendante de la taille de marge gauche :
                  mgp=c(tmp2 / lineInchConvert.f()$H - 1.4, 0.9, 0))

              ## Valeur à minimiser :
              return(sum(abs(x - unlist(par("mai")))))
          },
              control=list(abstol=0.01))    # Tolérance.

        ## Label axe y :
        ylab <- ifelse(getOption("P.axesLabels"),
                       parse(text=paste("'", Capitalize.f(varNames[metrique, "nom"]), "'",
                             ifelse(varNames[metrique, "unite"] != "",
                                    paste("~~(", varNames[metrique, "unite"], ")", sep=""),
                                    ""),
                             sep="")),
                       "")

        barPlotTmp <- barplot(heights,
                              beside=TRUE,
                              main=mainTitle,
                              xlab="",
                              ylab=ylab,
                              ylim=c(0, 1.1 * max(heights, na.rm=TRUE)),
                              las=1,
                              col=PAMPAcolors.f(n=nrow(heights)),
                              cex.lab=cex,
                              cex.axis=cex,
                              legend.text=ifelse(length(listFact) > 1, TRUE, FALSE),
                              args.legend=list("x"="topright", "inset"=-0.08, "xpd"=NA,
                                               "title"=Capitalize.f(varNames[listFact[1], "nom"])))

        if (getOption("P.axesLabels"))
        {
            mtext(Capitalize.f(varNames[tail(listFact, 1), "nom"]),
                  side=1, line=2.3, cex=cex)
        }else{}

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
                   cex =0.9, col=getOption("P.NbObsCol"), text.col=getOption("P.NbObsCol"), merge=FALSE)

        }else{}

        ## ###################################################
        ## Fermeture de graphiques et sauvegarde de fichiers :

        ## On ferme les périphériques PNG en mode fichier individuel :
        if (isTRUE(getOption("P.graphPNG")))
        {
            if ((! getOption("P.plusieursGraphPage") || length(iFactGraphSel) <= 1))
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
                                metrique="pres_abs", factGraph=factGraph, factGraphSel=modGraphSel,
                                listFact=rev(listFact), listFactSel=rev(listFactSel), # On les remets dans un ordre
                                        # intuitif.
                                dataEnv=dataEnv, baseEnv=baseEnv)
                }else{}
            }
        }else{
            ## Sauvegarde en wmf si pertinent et souhaité :
            if ((! getOption("P.plusieursGraphPage") || length(iFactGraphSel) <= 1) &&
                !getOption("P.graphPDF"))
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
                                metrique="pres_abs", factGraph=factGraph, factGraphSel=modGraphSel,
                                listFact=rev(listFact), listFactSel=rev(listFactSel), # On les remets dans un ordre
                                        # intuitif.
                                dataEnv=dataEnv, baseEnv=baseEnv)
                }else{}
            }else{}
        }
    }                                   # Fin de boucle graphique


    ## On ferme les périphériques PDF ou PNG restants :
    if (getOption("P.graphPDF") ||
        (isTRUE(getOption("P.graphPNG")) && getOption("P.plusieursGraphPage") && length(iFactGraphSel) > 1))
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
                        metrique="pres_abs", factGraph=factGraph, factGraphSel=factGraphSel,
                        listFact=rev(listFact), listFactSel=rev(listFactSel), # On les remets dans un ordre intuitif.
                        dataEnv=dataEnv, baseEnv=baseEnv)
        }else{}

        ## Inclusion des fontes dans le pdf si souhaité :
        if (getOption("P.graphPDF") && getOption("P.pdfEmbedFonts"))
        {
           i <- 1

            ## On parcours tous les fichiers qui correspondent au motif :
            while (is.element(basename(tmpFile <- sub("\\%03d", formatC(i, width=3, flag="0"), graphFile)),
                              dir(dirname(graphFile))))
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

    ## Sauvegarde en wmf restants si pertinent et souhaité :
    if (!(getOption("P.graphPNG") || getOption("P.graphPDF")) &&
        getOption("P.plusieursGraphPage") && length(iFactGraphSel) > 1)
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
                        metrique="pres_abs", factGraph=factGraph, factGraphSel=factGraphSel,
                        listFact=rev(listFact), listFactSel=rev(listFactSel), # On les remets dans un ordre intuitif.
                        dataEnv=dataEnv, baseEnv=baseEnv)
        }else{}
    }else{}
}








### Local Variables:
### ispell-local-dictionary: "english"
### fill-column: 120
### End:
