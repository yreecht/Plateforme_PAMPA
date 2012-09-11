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

### File: fonctions_graphiques.R
### Time-stamp: <2012-01-10 18:24:33 yreecht>
###
### Author: Yves Reecht
###
####################################################################################################
### Description:
###
### Fonctions communes pour les graphiques.
####################################################################################################

########################################################################################################################
makeColorPalette.f <- function()
{
    ## Purpose: Créer la palette de couleur pour les graphiques
    ## ----------------------------------------------------------------------
    ## Arguments: aucun !
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 31 mai 2011, 17:02

    assign(".ColorPalette",
           colorRampPalette(switch(getOption("P.colPalette"),
                                   "heat"=heat.colors(5),
                                   "gray"=c("#787878", "#dddddd"))),
           envir=.GlobalEnv)
}


########################################################################################################################
resFileGraph.f <- function(metrique, factGraph, modSel, listFact, ext, dataEnv,
                           prefix="boxplot", sufixe=NULL, type="espece")
{
    ## Purpose: Définit les noms du fichiers pour les résultats des modèles
    ##          linéaires. L'extension et un prefixe peuvent êtres précisés,
    ##          mais par défaut, c'est le fichier de sorties texte qui est
    ##          créé.
    ## ----------------------------------------------------------------------
    ## Arguments: metrique : nom de la métrique analysée.
    ##            factGraph : nom du facteur de séprataion des analyses/
    ##                        de selection d'espèce(s).
    ##            modSel : modalité(s) de factGraph sélectionnée(s).
    ##            listFact : vecteur des noms de facteurs de l'analyse.
    ##            prefix : préfixe du nom de fichier.
    ##            sufixe : un sufixe pour le nom de fichier.
    ##            ext : extension du fichier.
    ##            type : type de modèle (traitement conditionnel).
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 21 janv. 2011, 10:38

    ## Nom de fichier :
    filename <- paste(get("filePathes", envir=dataEnv)["results"], prefix, "_",
                      ## Métrique analysée :
                      metrique,
                      ifelse(getOption("P.maxExclu") && getOption("P.GraphPartMax") < 1,
                             paste("(", round(100 * getOption("P.GraphPartMax")),"pc-max)", sep=""),
                             ""),
                      "_",
                      ## si facteur de séparation des analyses :
                      "Agr-",
                      switch(type,
                             "espece"="espece+unitobs_",
                             "CL_espece"="CL+espece+unitobs_",
                             "unitobs"="unitobs_",
                             "CL_unitobs"="CL+unitobs_",
                             ""),
                      switch(type,
                             "espece"={
                                 ifelse(factGraph == "",
                                        "",
                                        paste(factGraph, "(", ifelse(modSel[1] != "",
                                                                     paste(modSel, collapse="+"),
                                                                     "toutes"), ")_", sep=""))
                             },
                             "CL_espece"={
                                 ifelse(factGraph == "",
                                        "",
                                        paste(factGraph, "(", ifelse(modSel[1] != "",
                                                                     paste(modSel, collapse="+"),
                                                                     "toutes"), ")_", sep=""))
                             },
                             "unitobs"={
                                 ifelse(factGraph == "",
                                        "(toutes_espèces)_",
                                        paste(factGraph, "(", ifelse(modSel[1] != "",
                                                                     paste(modSel, collapse="+"),
                                                                     "toutes"), ")_", sep=""))
                             },
                             "CL_unitobs"={
                                 ifelse(factGraph == "",
                                        "(toutes_espèces)_",
                                        paste(factGraph, "(", ifelse(modSel[1] != "",
                                                                     paste(modSel, collapse="+"),
                                                                     "toutes"), ")_", sep=""))
                             },
                             ""),
                      ## liste des facteurs de l'analyse
                      paste(listFact, collapse="-"),
                      ## sufixe :
                      ifelse(is.null(sufixe) || sufixe == "",
                             "",
                             paste("_", sufixe, sep="")),
                      ## Extension du fichier :
                      ".", gsub("^\\.([^.]*)", "\\1", ext[1], perl=TRUE), # nettoyage de l'extension si besoin.
                      sep="")

    ## Retourne le nom de fichier :
    return(filename)
}


########################################################################################################################
openDevice.f <- function(noGraph, metrique, factGraph, modSel, listFact, dataEnv,
                         type="espece", typeGraph="boxplot", large=FALSE)
{
    ## Purpose: Ouvrir les périphériques graphiques avec les bonnes options
    ## ----------------------------------------------------------------------
    ## Arguments: noGraph : le numéro de graphique (integer)
    ##            metrique : la métrique choisie.
    ##            factGraph : le facteur de séparation des graphiques.
    ##            modSel :  modalité(s) de factGraph sélectionnée(s).
    ##            listFact : liste du (des) facteur(s) de regroupement.
    ##            type : type de données (traitement conditionnel).
    ##            typeGraph : type de graphique.
    ##            large : pour des traitements particuliers (e.g. MRT)
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 12 août 2010, 14:54

    fileName <- NULL

    if (!getOption("P.graphPDF")) # sorties graphiques à l'écran ou PNG.
    {
        if (isTRUE(getOption("P.graphPNG")))
        {
            if (noGraph == 1 || ! getOption("P.plusieursGraphPage"))
            {
                pngFileName <- resFileGraph.f(metrique=metrique, factGraph=factGraph, modSel=modSel, listFact=listFact,
                                              dataEnv=dataEnv, ext="png", prefix = typeGraph,
                                              sufixe = ifelse(getOption("P.plusieursGraphPage") &&
                                                                (length(modSel) > 1 || modSel[1] == ""),
                                                              "%03d",
                                                              ""),
                                              type = type)

                ## Si plusieurs graphiques par page :
                if (getOption("P.plusieursGraphPage") && length(modSel) > 1 & # Regrouper dans une fonction de test
                    !is.element(type, c("unitobs")))                          # (mutualiser le code). [!!!]
                {
                    png(pngFileName,
                        width=ifelse(large, 120, 90) * 15,
                        height=ifelse(large, 75, 55) * 15,
                        pointsize=14)
                    par(mfrow=c(getOption("P.nrowGraph"), getOption("P.ncolGraph")))
                }else{
                    png(pngFileName,
                        width=ifelse(large, 100,
                                     ifelse(isTRUE(getOption("P.graphPaper")), 50, 75)) * 15,
                        height=ifelse(large, 55,
                                      ifelse(isTRUE(getOption("P.graphPaper")), 30, 40)) * 15, pointsize=14)
                }

                ## Pour retourner le nom de fichier malgré tout :
                fileName <- pngFileName
            }else{}

        }else{   ## Graphiques à l'écran :
            ## Des fonctions différentes pour l'affichage à l'écran, selon la plateforme :
            if (.Platform$OS.type == "windows")
            {
                winFUN <- "windows"
            }else{
                winFUN <- "X11"
            }

            if (getOption("P.plusieursGraphPage") && # Plusieurs graphs par page...
                    length(modSel) > 1 &&            # ...plus d'un facteur sélectionné...
                    !is.element(type, c("unitobs"))) # ...et pas d'agrégation.
            {
                if ((noGraph %% # ...et page remplie.
                     (getOption("P.nrowGraph") * getOption("P.ncolGraph"))) == 1)
                {
                    ## [!!!] Limiter aux cas nécessaires... (cf. plus haut).
                    eval(call(winFUN,
                              width=ifelse(large, 40, 30),  # 80, 60
                              height=ifelse(large, 26, 20), # 45, 35
                              pointsize=ifelse(isTRUE(getOption("P.graphPaper")), 14, 10)))

                    par(mfrow=c(getOption("P.nrowGraph"), getOption("P.ncolGraph")))
                }else{                  # Pas plusieurs graphs par page.
                }
            }else{                      # Pas plusieurs graphs par page.
                eval(call(winFUN,
                          width=ifelse(large, 35,
                                       ifelse(isTRUE(getOption("P.graphPaper")), 10, 25)), # 10, 50
                          height=ifelse(large, 15,
                                        ifelse(isTRUE(getOption("P.graphPaper")), 6, 12)), # 6, 20
                          pointsize=ifelse(isTRUE(getOption("P.graphPaper")), 14, 10)))
            }

            fileName <- resFileGraph.f(metrique=metrique, factGraph=factGraph, modSel=modSel, listFact=listFact,
                                       dataEnv=dataEnv, ext="wmf", prefix = typeGraph,
                                       sufixe = ifelse(getOption("P.plusieursGraphPage") &&
                                                        (length(modSel) > 1 || modSel[1] == ""),
                                                       "%03d",
                                                       ""),
                                       type=type)
        }
    }else{ ## Sorties graphiques en pdf :
        if (noGraph == 1)
        {
            ## Nom de fichier de fichier :
            if (getOption("P.PDFunFichierPage")) # Un fichier par graphique avec numéro.
            {
                pdfFileName <- paste(get("filePathes", envir=dataEnv)["results"],
                                     metrique, "_", factGraph, "_", paste(listFact, collapse="-"), "-%03d.pdf", sep="")
                onefile <- FALSE

            }else{                          # Tous les graphiques dans des pages séparées d'un même fichier.
                pdfFileName <- paste(get("filePathes", envir=dataEnv)["results"],
                                     metrique, "_", factGraph, "_", paste(listFact, collapse="-"), ".pdf", sep="")
                onefile <- TRUE
            }
            ## Ouverture de fichier :
            pdf(pdfFileName, encoding="ISOLatin1", family="URWHelvetica", onefile=onefile,
                width=ifelse(large, 30,
                             ifelse(isTRUE(getOption("P.graphPaper")), 12, 20)),
                height=ifelse(large, 20,
                             ifelse(isTRUE(getOption("P.graphPaper")), 8, 12)),
                pointsize=14)

            ## Si plusieurs graphiques par page :
            if (getOption("P.plusieursGraphPage") &&
                length(modSel) > 1 &&            # Plus d'un facteur sélectionné.
                !is.element(type, c("unitobs"))) # Pas d'agrégation.
            {
                par(mfrow=c(getOption("P.nrowGraph"), getOption("P.ncolGraph")))
            }else{}

            ## Pour retourner le nom de fichier également :
            fileName <- pdfFileName
        }else{}
    }

    par(cex=getOption("P.cex"))
    return(fileName)
}


########################################################################################################################
boxplotPAMPA.f <- function(exprBP, data, main=NULL, cex=getOption("P.cex"),...)
{
    ## Purpose: Boxplot avec un formatage pour pampa
    ## ----------------------------------------------------------------------
    ## Arguments: exprBP : expression décrivant le modèle du boxplot.
    ##            data : les données à utiliser.
    ##            main : titre du graphique.
    ##            cex : taille des caractères.
    ##            ... : arguments optionnels (passés à la fonction boxplot).
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 10 févr. 2011, 17:05

    ## Extraction du nom de la métrique :
    metrique <- deparse(exprBP[[2]])

    ## Les couleurs pour l'identification des modalités du facteur de second niveau :
    colors <- colBoxplot.f(terms=attr(terms(exprBP), "term.labels"), data=data)

    ## ylims :
    ylim <- c(min(data[ , metrique], na.rm=TRUE),
              ifelse(getOption("P.maxExclu") && getOption("P.GraphPartMax") < 1,
                     getOption("P.GraphPartMax") * max(data[ , metrique], na.rm=TRUE),
                     max(data[ , metrique], na.rm=TRUE) +
                     0.1*(max(data[ , metrique], na.rm=TRUE) -
                          min(data[ , metrique], na.rm=TRUE))))

    ## Plot sans affichage pour récupérer l'objet :
    tmpBP <- boxplot(exprBP, data=data,
                     varwidth = TRUE, las=2,
                     col=colors,
                     ylim=ylim,
                     cex.lab=cex,
                     cex.axis=cex,
                     plot=FALSE,
                     ...)

    ## Marge dynamiques (adaptation à la longueur des labels) :
    optim(par=unlist(par("mai")),       # Le rapport inch/ligne est modifié en changeant les marges => besoin
                                        # de l'optimiser.
          fn=function(x)
      {
          par(mai=c(
              ## Marge du bas dynamique :
              ifelse((tmp <- lineInchConvert.f()$V * cex * unlist(par("lheight")) * (0.2 + 0.9) + # marge
                                        # supplémentaire.
                      max(strDimRotation.f(tmpBP$names,
                                           srt=45,
                                           unit="inches",
                                           cex=cex)$height, na.rm=TRUE)) > 0.65 * unlist(par("pin"))[2],
                     0.65 * unlist(par("pin"))[2],
                     tmp),
              ## Marge de gauche dynamique :
              tmp2 <- ifelse((tmp <- lineInchConvert.f()$H * cex * unlist(par("lheight")) * (1.4 +0.4 + 0.9) + # marge
                                        # supplémentaire.
                              max(strDimRotation.f(as.graphicsAnnot(pretty(range(if(getOption("P.maxExclu")
                                                                                    && getOption("P.GraphPartMax") < 1)
                                                                             {
                                                                                 data[data[ ,metrique] <
                                                                                      getOption("P.GraphPartMax") *
                                                                                      max(data[ ,metrique], na.rm=TRUE) ,
                                                                                      metrique]
                                                                             }else{
                                                                                 data[ , metrique]
                                                                             }, na.rm=TRUE))),
                                                   srt=0,
                                                   unit="inches",
                                                   cex=cex)$width, na.rm=TRUE)) > 0.7 * unlist(par("pin"))[1],
                             0.7 * unlist(par("pin"))[1],
                             tmp),
              ## Marge supérieure augmentée s'il y a un titre :
              ifelse(isTRUE(getOption("P.graphPaper")),
                     2 * lineInchConvert.f()$V,
                     8 * lineInchConvert.f()$V),
              ## Marge de droite :
              lineInchConvert.f()$H * cex * unlist(par("lheight")) * 0.5) +
                  lineInchConvert.f()$H * cex * unlist(par("lheight")) * 0.1,
              ## Distance du nom d'axe dépendante de la taille de marge gauche :
              mgp=c(tmp2 / lineInchConvert.f()$H - 1.4, 0.9, 0))

          ## Valeur à minimiser :
          return(sum(abs(x - unlist(par("mai")))))
      },
          control=list(abstol=0.01))    # Tolérance.

    ## Plot avec affichage cette fois :
    tmpBP <- boxplot(exprBP, data=data,
                     varwidth = TRUE, las=2,
                     col=colors,
                     ylim=ylim,
                     xaxt="n",
                     main=if (! isTRUE(getOption("P.graphPaper")))
                 {
                     main
                 }else{NULL},
                     cex.lab=cex,
                     cex.axis=cex,
                     ...)

    ## Ajout de l'axe des abscices :
    axis(side=1, at = seq_along(tmpBP$names), labels = FALSE, tick = TRUE)

    ## Ajout des labels :
    text(x = seq_along(tmpBP$names),
         y = par("usr")[3] -
             ifelse(isTRUE(getOption("P.graphPDF")), # Coef différent pour les PDFs.
                    0.020,
                    0.030) * cex *
             diff(range(par("usr")[3:4])),
         labels = tmpBP$names,
         xpd = TRUE, srt = 45, adj = c(1, 1),
         cex = cex)

    ## Stockage des ylim pour les traitements ultérieurs :
    tmpBP$ylim <- ylim

    return(tmpBP)
}


########################################################################################################################
strDimRotation.f <- function(x, srt=0, unit="user", cex=getOption("P.cex"),...)
{
    ## Purpose: Calcul des dimensions d'une chaîne de caractère à laquelle
    ##          on applique une rotation
    ## ----------------------------------------------------------------------
    ## Arguments: x : vecteur de classe 'character'.
    ##            srt : angle de rotation en degrés.
    ##            unit : unité de sortie.
    ##            ... : arguments supplémentaires passés à str(height|width).
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date:  9 févr. 2011, 16:15

    ## browser()

    ## Dimensions en pouces :
    W.inches <- strwidth(x, unit="inches", cex=cex,...)
    H.inches <- strheight(x, unit="inches", cex=cex,...)

    ## Facteur de conversion avec l'unité souhaitée :
    X.inchesVSunit <- W.inches / strwidth(x, unit=unit, cex=cex,...)
    Y.inchesVSunit <- H.inches / strheight(x, unit=unit, cex=cex,...)

    ## Calcul des largeurs et hauteurs en rotations :
    X.calc <- abs(W.inches * cos(srt * base:::pi / 180)) + abs(H.inches * sin(srt * base:::pi / 180))
    Y.calc <- abs(W.inches * sin(srt * base:::pi / 180)) + abs(H.inches * cos(srt * base:::pi / 180))

    ## Conversion dans l'unité souhaitée :
    return(list(width = X.calc / X.inchesVSunit,
                height = Y.calc / Y.inchesVSunit))
}

########################################################################################################################

lineInchConvert.f <- function()
{
    ## Purpose: Calcul du facteur de conversion inch/ligne.
    ## ----------------------------------------------------------------------
    ## Arguments: Aucun
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 26 août 2011, 13:07

    pars <- par(c("mai", "mar"))

    return(list(H=(pars$mai/pars$mar)[2],
                V=(pars$mai/pars$mar)[1]))
}

########################################################################################################################

unitConvY.f <- function(x=NULL, from="user", to="user")
{
    ## Purpose: Conversion verticale des unités entre différents systèmes de
    ##          coordonnées
    ## ----------------------------------------------------------------------
    ## Arguments: x : valeur à convertir. Si NULL, retourne le rapport
    ##                unité2/unité1 (to/from).
    ##            from : unité de départ.
    ##            to : unité vers laquelle convertir.
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 26 août 2011, 14:21

    Y1 <- strheight("X", unit=from, cex=10)
    Y2 <- strheight("X", unit=to, cex=10)

    if (is.null(x))
    {
        return(Y2/Y1)                   # Rapport uniquement.
    }else{
        return(x * Y2/Y1)               # Valeur convertie.
    }
}

########################################################################################################################

unitConvX.f <- function(x=NULL, from="user", to="user")
{
    ## Purpose: Conversion horizontale des unités entre différents systèmes
    ##          de coordonnées
    ## ----------------------------------------------------------------------
    ## Arguments: x : valeur à convertir. Si NULL, retourne le rapport
    ##                unité2/unité1 (to/from).
    ##            from : unité de départ.
    ##            to : unité vers laquelle convertir.
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 26 août 2011, 14:21

    X1 <- strwidth("XXXXXX", unit=from, cex=10)
    X2 <- strwidth("XXXXXX", unit=to, cex=10)

    if (is.null(x))
    {
        return(X2/X1)                   # Rapport uniquement.
    }else{
        return(x * X2/X1)               # Valeur convertie.
    }
}




### Local Variables:
### ispell-local-dictionary: "english"
### fill-column: 120
### End:
