#-*- coding: latin-1 -*-

## Plateforme PAMPA de calcul d'indicateurs de ressources & biodiversit�
##   Copyright (C) 2008-2010 Ifremer - Tous droits r�serv�s.
##
##   Ce programme est un logiciel libre ; vous pouvez le redistribuer ou le
##   modifier suivant les termes de la "GNU General Public License" telle que
##   publi�e par la Free Software Foundation : soit la version 2 de cette
##   licence, soit (� votre gr�) toute version ult�rieure.
##
##   Ce programme est distribu� dans l'espoir qu'il vous sera utile, mais SANS
##   AUCUNE GARANTIE : sans m�me la garantie implicite de COMMERCIALISABILIT�
##   ni d'AD�QUATION � UN OBJECTIF PARTICULIER. Consultez la Licence G�n�rale
##   Publique GNU pour plus de d�tails.
##
##   Vous devriez avoir re�u une copie de la Licence G�n�rale Publique GNU avec
##   ce programme ; si ce n'est pas le cas, consultez :
##   <http://www.gnu.org/licenses/>.

### File: barplots_occurrence.R
### Time-stamp: <2012-01-10 18:13:34 yreecht>
###
### Author: Yves Reecht
###
####################################################################################################
### Description:
###
### Fonctions pour cr�er des repr�sentations (barplots) des fr�quences d'occurrences relatives (%)
### (utilise certaines fonctions de ./boxplot_generique_calc.R)
####################################################################################################

barplotOccurrence.unitobs.f <- function(factGraph, factGraphSel, listFact, listFactSel, dataEnv,
                                        baseEnv=.GlobalEnv)
{
    ## Purpose: cr�ation d'un barplot d'apr�s les s�lections de facteurs et
    ##          modalit�s, avec les pr�sences/absences agr�g�es par unitobs.
    ## ----------------------------------------------------------------------
    ## Arguments: factGraph : le facteur de s�paration des graphiques.
    ##            factGraphSel : la s�lection de modalit�s pour ce dernier.
    ##            listFact : liste du (des) facteur(s) de regroupement.
    ##            listFactSel : liste des modalit�s s�lectionn�es pour ce(s)
    ##                          dernier(s).
    ##            dataEnv : environnement de stockage des donn�es.
    ##            baseEnv : environnement de l'interface.
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 14 oct. 2010, 10:51

    metrique <- "freq.occurrence"

    ## Nettoyage des facteurs (l'interface de s�lection produit des valeurs vides) :
    listFactSel <- listFactSel[unlist(listFact) != ""]
    listFactSel <- listFactSel[length(listFactSel):1]

    listFact <- listFact[unlist(listFact) != ""]
    listFact <- listFact[length(listFact):1]

    ## Concat�nation
    facteurs <- c(factGraph, unlist(listFact)) # Concat�nation des facteurs

    selections <- c(list(factGraphSel), listFactSel) # Concat�nation des leurs listes de modalit�s s�lectionn�es

    ## Donn�es pour la s�rie de boxplots :
    tmpData <- subsetToutesTables.f(metrique="pres_abs", facteurs=facteurs, selections=selections,
                                    dataEnv=dataEnv, tableMetrique="TablePresAbs", exclude = NULL)


    ## Identification des diff�rents modalit�s (esp�ces) du graphique � g�n�rer :
    if (factGraph == "")                # Pas de facteur.
    {
        iFactGraphSel <- ""
    }else{
        if (is.na(factGraphSel[1]))            # Toutes les modalit�s.
        {
            iFactGraphSel <- unique(as.character(sort(tmpData[ , factGraph])))
        }else{                              # Modalit�s s�lectionn�es (et pr�sentes parmi les donn�es retenues).
            iFactGraphSel <- factGraphSel[is.element(factGraphSel, tmpData[ , factGraph])]
        }
    }


    ## Agr�gation des observations / unit� d'observation :
    tmpData <- na.omit(agregationTableParCritere.f(Data=tmpData,
                                                   metrique="pres_abs",
                                                   facteurs=c("unite_observation"),
                                                   dataEnv=dataEnv,
                                                   listFact=listFact))

    ## Sauvegarde temporaire des donn�es utilis�es pour les graphiques (attention : �cras�e � chaque nouvelle s�rie de
    ## graphiques) :
    DataBackup <<- list(tmpData)

    ## ###############################################################
    ## Cr�ation du graphique si le nombre d'observations  < au minimum d�fini dans les options :
    if (dim(tmpData)[1] < getOption("P.MinNbObs"))
    {
        warning("Nombre d'observations pour (", paste(iFactGraphSel, collapse=", "), ") < ", getOption("P.MinNbObs"),
                " : Graphique non cr�� !\n")
    }else{
        ## Option graphique :
        cex <- getOption("P.cex")

        ## Suppression des 'levels' non utilis�s :
        tmpData <- dropLevels.f(tmpData)

        ## Ouverture et configuration du p�riph�rique graphique :
        graphFile <- openDevice.f(noGraph=1,
                                  metrique=metrique,
                                  factGraph=factGraph,
                                  modSel=iFactGraphSel,
                                  listFact=listFact,
                                  dataEnv=dataEnv,
                                  type="unitobs",
                                  typeGraph="barplot")

        ## Titre (d'apr�s les m�triques, modalit� du facteur de s�paration et facteurs de regroupement) :
        if ((! isTRUE(getOption("P.graphPaper"))) && isTRUE(getOption("P.title")))
        {
            mainTitle <- graphTitle.f(metrique=metrique,
                                      modGraphSel=iFactGraphSel,
                                      factGraph=factGraph,
                                      listFact=listFact,
                                      type="unitobs")
        }else{
            mainTitle <- NULL
        }

        ## Calcul des fr�quences :
        heights <- with(tmpData,
                        tapply(pres_abs, lapply(listFact, function(y)eval(parse(text=y))),
                               function(x)
                           {
                               100 * sum(x, na.rm=TRUE) / length(na.omit(x))
                           }))


        ## Param�tres graphiques :
        ## Marge dynamiques (adaptation � la longueur des labels) :
        optim(par=unlist(par("mai")),   # Le rapport inch/ligne est modifi� en changeant les marges => besoin
                                        # de l'optimiser.
              fn=function(x)
          {
              par(mai=c(
                  ## Marge du bas :
                  lineInchConvert.f()$V * cex * unlist(par("lheight")) * 4.5,
                  ## Marge de gauche dynamique :
                  tmp2 <- ifelse((tmp <- lineInchConvert.f()$H * cex * unlist(par("lheight")) * (1.4 +0.4 + 0.9) + # marge
                                        # suppl�mentaire.
                                  max(strDimRotation.f(as.graphicsAnnot(pretty(range(heights, na.rm=TRUE))),
                                                       srt=0,
                                                       unit="inches",
                                                       cex=cex)$width, na.rm=TRUE)) > 0.7 * unlist(par("pin"))[1],
                                 0.7 * unlist(par("pin"))[1],
                                 tmp),
                  ## Marge sup�rieure augment�e s'il y a un titre :
                  ifelse(isTRUE(getOption("P.graphPaper"))  || (! isTRUE(getOption("P.title"))),
                         3 * lineInchConvert.f()$V,
                         8 * lineInchConvert.f()$V),
                  ## Marge de droite :
                  lineInchConvert.f()$H * cex * unlist(par("lheight")) * 7) +
                  lineInchConvert.f()$H * cex * unlist(par("lheight")) * 0.1,
                  ## Distance du nom d'axe d�pendante de la taille de marge gauche :
                  mgp=c(tmp2 / lineInchConvert.f()$H - 1.4, 0.9, 0))

              ## Valeur � minimiser :
              return(sum(abs(x - unlist(par("mai")))))
          },
              control=list(abstol=0.01))    # Tol�rance.

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
            nbObs <- with(tmpData,
                          tapply(pres_abs,
                                 lapply(listFact, function(y)eval(parse(text=y))),
                                 function(x)
                             {
                                 length(na.omit(x))
                             }))

            ## Nombres sur l'axe sup�rieur :
            mtext(nbObs, side=3, at=barPlotTmp, las=2, col=getOption("P.NbObsCol"),
                  adj=-0.2)

            legend(x="topleft",
                   legend=expression("Nombre d'unit�s d'observation"),
                   cex =0.9, col=getOption("P.NbObsCol"), text.col=getOption("P.NbObsCol"), merge=FALSE)

        }else{}

        ## ##################################################
        ## Sauvegarde des donn�es :
        if (getOption("P.saveData"))
        {
            writeData.f(filename=graphFile, Data=tmpData,
                        cols=NULL)
        }else{}

        ## Sauvegarde des infos sur les donn�es et statistiques :
        if (getOption("P.saveStats"))
        {
            infoStats.f(filename=graphFile, Data=tmpData, agregLevel="unitobs", type="graph",
                        metrique="pres_abs", factGraph=factGraph, factGraphSel=factGraphSel,
                        listFact=rev(listFact), listFactSel=rev(listFactSel), # On les remets dans un ordre intuitif.
                        dataEnv=dataEnv, baseEnv=baseEnv)
        }else{}

    }                                   # Fin de boucle graphique

    ## On ferme les p�riph�riques PDF :
    if (getOption("P.graphPDF") || isTRUE(getOption("P.graphPNG")))
    {
        dev.off()

        ## Inclusion des fontes dans le pdf si souhait� :
        if (getOption("P.graphPDF") && getOption("P.pdfEmbedFonts"))
        {
            tryCatch(embedFonts(file=graphFile),
                     error=function(e)
                 {
                     warning("Impossible d'inclure les fontes dans le PDF !")
                 })
        }else{}
    }else{
        if (.Platform$OS.type == "windows" && isTRUE(getOption("P.graphWMF")))
        {
            ## Sauvegarde en wmf si pertinent et souhait� :
            savePlot(graphFile, type="wmf", device=dev.cur())
        }else{}
    }
}








### Local Variables:
### ispell-local-dictionary: "english"
### fill-column: 120
### End:
