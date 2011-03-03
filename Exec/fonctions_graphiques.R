#-*- coding: latin-1 -*-

### File: fonctions_graphiques.R
### Time-stamp: <2011-02-21 16:11:17 yreecht>
###
### Author: Yves Reecht
###
####################################################################################################
### Description:
###
### Fonctions communes pour les graphiques.
####################################################################################################

########################################################################################################################
resFileGraph.f <- function(metrique, factGraph, modSel, listFact,
                           ext, prefix="boxplot", sufixe=NULL, type="espece")
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
    filename <- paste(nameWorkspace, "/FichiersSortie/", prefix, "_",
                      ## Métrique analysée :
                      metrique, "_",
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
                                        "(toutes espèces)_",
                                        paste(factGraph, "(", ifelse(modSel[1] != "",
                                                                     paste(modSel, collapse="+"),
                                                                     "toutes"), ")_", sep=""))
                             },
                             "CL_unitobs"={
                                 ifelse(factGraph == "",
                                        "(toutes espèces)_",
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
openDevice.f <- function(noGraph, metrique, factGraph, modSel, listFact, type="espece", typeGraph="boxplot")
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
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 12 août 2010, 14:54


    if (!getOption("P.graphPDF")) # sorties graphiques à l'écran ou PNG.
    {
        if (isTRUE(getOption("P.graphPNG")))
        {
            if (noGraph == 1 || ! getOption("P.plusieursGraphPage"))
            {
                pngFileName <- resFileGraph.f(metrique=metrique, factGraph=factGraph, modSel=modSel, listFact=listFact,
                                              ext="png", prefix = typeGraph,
                                              sufixe = ifelse(getOption("P.plusieursGraphPage") &&
                                                                (length(modSel) > 1 || modSel[1] == ""),
                                                              "%03d",
                                                              ""),
                                              type = type)
                ## paste(nameWorkspace, "/FichiersSortie/",
                ##                  metrique, "_", factGraph, "_", paste(listFact, collapse="-"), "-%03d.png", sep="")



                 ## Si plusieurs graphiques par page :
                if (getOption("P.plusieursGraphPage") && length(modSel) > 1 &
                    !is.element(type, c("unitobs")))
                {
                    png(pngFileName, width=90*15, height=55*15, pointsize=14)
                    par(mfrow=c(getOption("P.nrowGraph"), getOption("P.ncolGraph")))
                }else{
                    png(pngFileName, width=75*15, height=40*15, pointsize=14)
                }
            }else{}

        }else{
            if (getOption("P.plusieursGraphPage"))     # Plusieurs graphs par page...
            {
                if ((noGraph %% # ...et page remplie.
                     (getOption("P.nrowGraph") * getOption("P.ncolGraph"))) == 1)
                {
                    print(paste("Fenêtre", noGraph))
                    X11(width=60, height=35, pointsize=10)
                    par(mfrow=c(getOption("P.nrowGraph"), getOption("P.ncolGraph")))
                }else{}
            }else{                      # Pas plusieurs graphs par page.
                X11(width=50, height=20, pointsize=10)
            }
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
boxplotPAMPA.f <- function(exprBP, data,...)
{
    ## Purpose: Boxplot avec un formatage pour pampa
    ## ----------------------------------------------------------------------
    ## Arguments:
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 10 févr. 2011, 17:05

    ## Extraction du nom de la métrique :
    metrique <- deparse(exprBP[[2]])

    ## Les couleurs pour l'identification des modalités du facteur de second niveau :
    colors <- colBoxplot.f(terms=attr(terms(exprBP), "term.labels"), data=data)

    ## Plot sans affichage pour récupérer l'objet :
    tmpBP <- boxplot(exprBP, data=data,
                     varwidth = TRUE, las=2,
                     col=colors,
                     ylim=c(min(data[ , metrique], na.rm=TRUE),
                            max(data[ , metrique], na.rm=TRUE) +
                            0.1*(max(data[ , metrique], na.rm=TRUE) -
                                 min(data[ , metrique], na.rm=TRUE))),
                     plot=FALSE,
                     ...)

    ## Marge dynamiques :
    par(mar=c(
        ifelse((tmp <- 3 +
                ifelse(isTRUE(getOption("P.graphPDF")), # Coef différent pour les PDFs.
                       42,
                       28)*
                max(strDimRotation.f(tmpBP$names,
                                     srt=45,
                                     unit="figure",
                                     cex=1.0)$height, na.rm=TRUE)) > 11,
               11,
               tmp),
        5, 8, 1))

    ## Plot avec affichage cette fois :
    tmpBP <- boxplot(exprBP, data=data,
                     varwidth = TRUE, las=2,
                     col=colors,
                     ylim=c(min(data[ , metrique], na.rm=TRUE),
                            max(data[ , metrique], na.rm=TRUE) +
                            0.1*(max(data[ , metrique], na.rm=TRUE) -
                                 min(data[ , metrique], na.rm=TRUE))),
                     xaxt="n",
                     ...)

    ## Ajout de l'axe des abscices :
    axis(side=1, at = seq_along(tmpBP$names), labels = FALSE, tick = TRUE)

    ## Ajout des labels :
    text(x = seq_along(tmpBP$names),
         y = par("usr")[3] -
             ifelse(isTRUE(getOption("P.graphPDF")), # Coef différent pour les PDFs.
                    0.020,
                    0.030) *
             diff(range(par("usr")[3:4])),
         labels = tmpBP$names,
         xpd = TRUE, srt = 45, adj = c(1, 1),
         cex = 1.0)

    return(tmpBP)
}






### Local Variables:
### ispell-local-dictionary: "english"
### fill-column: 120
### End:
