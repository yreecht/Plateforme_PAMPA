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

### File: arbres_regression_esp_generiques.R
### Time-stamp: <2012-01-10 18:11:47 yreecht>
###
### Author: Yves Reecht
###
####################################################################################################
### Description:
###
### Fonctions de création de boxplots pour les métriques agrégées / espèce / unité d'observation.
####################################################################################################

########################################################################################################################
print.rpart.fr <- function (x, minlength = 0, spaces = 2, cp, digits = getOption("digits"),
                            ...)
{
    ## Purpose:Francisation de la fonction print.rpart() du package
    ##          "mvpart".
    ## ----------------------------------------------------------------------
    ## Arguments: ceux de mvpart::print.rpart()
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 12 mai 2011, 15:17

    if (!inherits(x, "rpart"))
        stop("Not legitimate rpart object")
    if (!is.null(x$frame$splits))
        x <- rpconvert(x)
    if (!missing(cp))
        x <- prune.rpart(x, cp = cp)
    frame <- x$frame
    ylevel <- attr(x, "ylevels")
    node <- as.numeric(row.names(frame))
    depth <- mvpart:::tree.depth(node)
    indent <- paste(rep(" ", spaces * 32), collapse = "")
    if (length(node) > 1)
    {
        indent <- substring(indent, 1, spaces * seq(depth))
        indent <- paste(c("", indent[depth]), format(node), ")",
                        sep = "")
    }else{
        indent <- paste(format(node), ")", sep = "")
    }

    tfun <- (x$functions)$print
    if (!is.null(tfun)) {
        if (is.null(frame$yval2))
            yval <- tfun(frame$yval, ylevel, digits)
        else yval <- tfun(frame$yval2, ylevel, digits)
    }
    else yval <- format(signif(frame$yval, digits = digits))
    term <- rep(" ", length(depth))
    term[frame$var == "<leaf>"] <- "*"
    ## browser()
    z <- labels(x, digits = digits, minlength = minlength, ...)
    n <- frame$n
    z <- paste(indent, z, n, format(signif(frame$dev, digits = digits)),
               yval, term, sep="\t")
    omit <- x$na.action
    if (length(omit))
        cat("n=", n[1], " (", naprint(omit), ")\n\n", sep = "")
    else cat("n=", n[1], "\n\n")
    if (x$method == "class")
        cat(" noeud), partition, n, perte, yval, (yprob)\n")
    else cat(" noeud), partition, n, déviance, yval\n")
    cat("\t\t\t\t* indique un noeud terminal\n\n")
    cat(z, sep = "\n")
    return(invisible(x))
}

########################################################################################################################
summary.rpart.fr <- function (object, cp = 0, digits = getOption("digits"), file,
                              ...)
{
    ## Purpose: Francisation de la fonction summary.rpart() du package
    ##          "mvpart".
    ## ----------------------------------------------------------------------
    ## Arguments: ceux de summary.rpart()
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 12 mai 2011, 13:55

    if (!inherits(object, "rpart"))
    {
        stop("Not legitimate rpart object")
    }else{}

    if (!is.null(object$frame$splits))
    {
        x <- rpconvert(object)
    }else{
        x <- object
    }

    if (!missing(file))
    {
        sink(file)
        on.exit(sink())
    }else{}

    if (!is.null(x$call))
    {
        cat("Appel :\n")
        dput(x$call)
    }else{}

    omit <- x$na.action
    n <- x$frame$n
    if (length(omit))
    {
        cat("  n=", n[1], " (", naprint(omit), ")\n\n", sep = "")
    }else{
        cat("  n=", n[1], "\n\n")
    }

    print(x$cptable, digits = digits)
    ff <- x$frame
    ylevel <- attr(x, "ylevels")
    id <- as.integer(row.names(ff))
    parent.id <- ifelse(id == 1, 1, floor(id/2))
    parent.cp <- ff$complexity[match(parent.id, id)]
    rows <- (1:length(id))[parent.cp > cp]

    if (length(rows) > 0)
    {
        rows <- rows[order(id[rows])]
    }else{
        rows <- 1
    }

    is.leaf <- (ff$var == "<leaf>")
    index <- cumsum(c(1, ff$ncompete + ff$nsurrogate + 1 * (!is.leaf)))

    if (!all(is.leaf))
    {
        sname <- dimnames(x$splits)[[1]]
        cuts <- vector(mode = "character", length = nrow(x$splits))
        temp <- x$splits[, 2]
        for (i in 1:length(cuts))
        {
            if (temp[i] == -1)
            {
                cuts[i] <- paste("<", format(signif(x$splits[i, 4],
                                                    digits = digits)))
            }else{
                if (temp[i] == 1)
                {
                    cuts[i] <- paste("<", format(signif(x$splits[i, 4],
                                                        digits = digits)))
                }else{
                    cuts[i] <- paste("partitionné en ",
                                     paste(c("G", "-", "D")[x$csplit[x$splits[i, 4], 1:temp[i]]],
                                           collapse = "", sep = ""),
                                     collapse = "")
                }
            }
        }

        if (any(temp < 2))
        {
            cuts[temp < 2] <- format(cuts[temp < 2], justify = "left")
        }else{}

        cuts <- paste(cuts, ifelse(temp >= 2,
                                   ",",
                                   ifelse(temp == 1,
                                          " vers la droite,",
                                          " vers la gauche, ")),
                      sep = "")
    }

    if (is.null(ff$yval2))
    {
        tprint <- x$functions$summary(ff$yval[rows], ff$dev[rows],
                                      ff$wt[rows], ylevel, digits)
    }else{
        tprint <- x$functions$summary(ff$yval2[rows, ], ff$dev[rows],
                                      ff$wt[rows], ylevel, digits)
    }

    for (ii in 1:length(rows))
    {
        i <- rows[ii]
        nn <- ff$n[i]
        twt <- ff$wt[i]
        cat("\nNoeud #", id[i], ": ", nn, " observations",
            sep = "")
        if (ff$complexity[i] < cp || is.leaf[i])
        {
            cat("\n")
        }else{
            cat(",    param de complexité=", format(signif(ff$complexity[i],
                                                           digits)), "\n", sep = "")
        }

        cat(tprint[ii], "\n")
        if (ff$complexity[i] > cp && !is.leaf[i])
        {
            sons <- 2 * id[i] + c(0, 1)
            sons.n <- ff$n[match(sons, id)]
            cat("  fils Gauche=", sons[1], " (", sons.n[1], " obs)",
                " fils Droit=", sons[2], " (", sons.n[2], " obs)",
                sep = "")
            j <- nn - (sons.n[1] + sons.n[2])
            if (j > 1)
            {
                cat(", ", j, " observations restantes\n", sep = "")
            }else{
                if (j == 1)
                {
                    cat(", 1 observation restante\n")
                }else{
                    cat("\n")
                }
            }
            cat("  Partition initiale :\n")
            j <- seq(index[i], length = 1 + ff$ncompete[i])
            if (all(nchar(cuts[j]) < 25))
            {
                temp <- format(cuts[j], justify = "left")
            }else{
                temp <- cuts[j]
            }

            cat(paste("      ", format(sname[j], justify = "left"),
                " ", temp, " improve=", format(signif(x$splits[j,
                  3], digits)), ", (", nn - x$splits[j, 1], " manquant)",
                sep = ""), sep = "\n")

            if (ff$nsurrogate[i] > 0)
            {
                cat("  Partition alternative :\n")
                j <- seq(1 + index[i] + ff$ncompete[i], length = ff$nsurrogate[i])
                agree <- x$splits[j, 3]
                if (all(nchar(cuts[j]) < 25))
                {
                    temp <- format(cuts[j], justify = "left")
                }else{
                    temp <- cuts[j]
                }

                if (ncol(x$splits) == 5)
                {
                  adj <- x$splits[j, 5]
                  cat(paste("      ", format(sname[j], justify = "left"),
                            " ", temp, " agree=", format(round(agree, 3)),
                            ", adj=", format(round(adj, 3)), ", (",
                            x$splits[j, 1], " split)", sep = ""), sep = "\n")
                }else{
                  cat(paste("      ", format(sname[j], justify = "left"),
                            " ", temp, " agree=", format(round(agree, 3)),
                            ", (", x$splits[j, 1], " split)",
                            sep = ""), sep = "\n")
                }
            }
        }
    }

    cat("\n")
    invisible(x)
}

########################################################################################################################
text.rpart.new <- function (x, splits = TRUE, which = 4, label = "yval", FUN = text,
                            all.leaves = FALSE, pretty = NULL, digits = getOption("digits") -
                            2, tadj = 0.65, stats = TRUE, use.n = FALSE, bars = TRUE,
                            legend = FALSE, xadj = 1, yadj = 1, bord = FALSE, big.pts = FALSE,
                            ...)
{
    ## Purpose: Remplace la fonction text.rpart() du package "mvpart"
    ##          (correction de l'alignement vertical)
    ## ----------------------------------------------------------------------
    ## Arguments: ceux de mvpart::text.rpart
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 12 mai 2011, 11:17

    if (!inherits(x, "rpart"))
    {
        stop("Not legitimate rpart")
    }else{}

    if (!is.null(x$frame$splits))
    {
        x <- rpconvert(x)
    }else{}

    frame <- x$frame
    col <- names(frame)
    method <- x$method
    ylevels <- attr(x, "ylevels")

    if (!is.null(ylevels <- attr(x, "ylevels")))
    {
        col <- c(col, ylevels)
    }else{}

    if (is.na(match(label, col)))
    {
        stop("Label must be a column label of the frame component of the tree")
    }else{}

    cxy <- par("cxy")
    if (!is.null(srt <- list(...)$srt) && srt == 90)
    {
        cxy <- rev(cxy)
    }else{}

    xy <- mvpart:::rpartco(x)
    node <- as.numeric(row.names(x$frame))
    is.left <- (node%%2 == 0)
    node.left <- node[is.left]
    parent <- match(node.left/2, node)
    bars <- bars & is.matrix(frame$yval2)
    text.adj <- ifelse(bars, yadj * diff(range(xy$y))/12, 0)

    if (splits)
    {
        left.child <- match(2 * node, node)
        right.child <- match(node * 2 + 1, node)
        rows <- labels(x, pretty = pretty)
        if (which == 1)
        {
            FUN(xy$x, xy$y + tadj * cxy[2], rows[left.child],
                ...)
        }else{
            if (which == 2 | which == 4)
                FUN(xy$x, xy$y + tadj * cxy[2], rows[left.child],
                    pos = 2, ...)
            if (which == 3 | which == 4)
                FUN(xy$x, xy$y + tadj * cxy[2], rows[right.child],
                    pos = 4, ...)
        }
    }else{}

    leaves <- if (all.leaves)
    {
        rep(TRUE, nrow(frame))
    }else{
        frame$var == "<leaf>"
    }
    if (stats)
    {
        if (is.null(frame$yval2))
        {
            stat <- x$functions$text(yval = frame$yval[leaves],
                dev = frame$dev[leaves], wt = frame$wt[leaves],
                ylevel = ylevels, digits = digits, n = frame$n[leaves],
                use.n = use.n)
        }else{
            stat <- x$functions$text(yval = frame$yval2[leaves,
                                     ], dev = frame$dev[leaves], wt = frame$wt[leaves],
                                     ylevel = ylevels, digits = digits, n = frame$n[leaves],
                                     use.n = use.n)
        }

        ## Ajout d'une constante lorsque les effectifs sont également ajoutés (labels sur deux lignes) :
        FUN(xy$x[leaves], xy$y[leaves] - ifelse(use.n, tadj + 0.3, tadj) * cxy[2] - text.adj,
            stat, adj = 0.5, ...)
    }

    if (bars)
    {
        bar.vals <- x$functions$bar(yval2 = frame$yval2)
        sub.barplot(xy$x, xy$y, bar.vals, leaves, xadj = xadj,
                    yadj = yadj, bord = bord, line = TRUE, col = c("lightblue",
                                                           "blue", "darkblue"))
        rx <- range(xy$x)
        ry <- range(xy$y)
        if (!is.null(ylevels))
        {
            bar.labs <- ylevels
        }else{
            bar.labs <- dimnames(x$y)[[2]]
        }
        if (legend & !is.null(bar.labs))
        {
            legend(min(xy$x) - 0.1 * rx, max(xy$y) + 0.05 * ry,
                   bar.labs, col = c("lightblue", "blue", "darkblue"),
                   pch = 15, bty = "n", ...)
        }else{}
    }
    if (big.pts)
    {
        points(xy$x[leaves], xy$y[leaves], pch = 16, cex = 3 *
               par()$cex, col = 2:(sum(leaves) + 1))
    }else{}

    invisible()
}

########################################################################################################################
resFileMRT.f <- function(metrique, factAna, modSel, listFact, dataEnv,
                         prefix=NULL, ext="txt", sufixe=NULL, type="espece")
{
    ## Purpose: Définit les noms du fichiers pour les résultats des arbres
    ##          de régression multivariée.
    ##          L'extension et un prefixe peuvent êtres précisés,
    ##          mais par défaut, c'est le fichier de sorties texte qui est
    ##          créé.
    ## ----------------------------------------------------------------------
    ## Arguments: metrique : nom de la métrique analysée.
    ##            factAna : nom du facteur de séprataion des analyses.
    ##            modSel : modalité de factAna sélectionnée.
    ##            listFact : vecteur des noms de facteurs de l'analyse.
    ##            prefix : préfixe du nom de fichier.
    ##            sufixe : un sufixe pour le nom de fichier.
    ##            ext : extension du fichier.
    ##            type : type de modèle (traitement conditionnel).
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date:  8 sept. 2010, 15:48

    ## si pas de préfix fourni :
    if (is.null(prefix))
    {
        prefix <- "MRT"
    }else{}

    ## Nom de fichier :
    filename <- paste(get("filePathes", envir=dataEnv)["results"], prefix, "_",
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
                                 ifelse(factAna == "",
                                        "",
                                        paste(factAna, "(",
                                              ifelse(modSel[1] != "", modSel, "toutes"),
                                              ")_", sep=""))
                             },
                             "CL_espece"={
                                 ifelse(factAna == "",
                                        "",
                                        paste(factAna, "(", ifelse(modSel[1] != "",
                                                                   paste(modSel, collapse="+"),
                                                                   "toutes"), ")_", sep=""))
                             },
                             "unitobs"={
                                 ifelse(factAna == "",
                                        "(toutes espèces)_",
                                        paste(factAna, "(", ifelse(modSel[1] != "",
                                                                   paste(modSel, collapse="+"),
                                                                   "toutes"), ")_", sep=""))
                             },
                             "CL_unitobs"={
                                 ifelse(factAna == "",
                                        "(toutes espèces)_",
                                        paste(factAna, "(", ifelse(modSel[1] != "",
                                                                   paste(modSel, collapse="+"),
                                                                   "toutes"), ")_", sep=""))
                             },
                             ""),
                      ## liste des facteurs de l'analyse
                      paste(listFact, collapse="-"),
                      ## sufixe :
                      ifelse(is.null(sufixe), "", paste("_", sufixe, sep="")),
                      ## Extension du fichier :
                      ".", gsub("^\\.([^.]*)", "\\1", ext[1], perl=TRUE), # nettoyage de l'extension si besoin.
                      sep="")

    ## Ouverture de la connection (retourne l'objet de type 'connection') si pas un fichier avec extension graphique,
    ## retourne le nom de fichier sinon :
    if (!is.element(gsub("^\\.([^.]*)", "\\1", ext[1], perl=TRUE),
                    c("pdf", "PDF", "png", "PNG", "jpg", "JPG")))
    {
        return(resFile <- file(filename, open="w"))
    }else{
        return(filename)
    }
}

########################################################################################################################
sortiesMRT.f <- function(objMRT, formule, metrique, factAna, modSel, listFact, listFactSel, Data, dataEnv=dataEnv,
                         sufixe=NULL, type="espece", baseEnv=.GlobalEnv)
{
    ## Purpose: Formater les résultats des MRT et les écrire dans un fichier
    ## ----------------------------------------------------------------------
    ## Arguments: objMRT : un objet de classe 'rpart'.
    ##            formule : la formule utilisée (pas lisible dans le call).
    ##            metrique : la métrique choisie.
    ##            factAna : le facteur de séparation des analyses.
    ##            modSel : la modalité courante.
    ##            listFact : liste du (des) facteur(s) de regroupement.
    ##            listFactSel : liste des modalités sélectionnées pour ce(s)
    ##                          dernier(s).
    ##            Data : les données utilisées.
    ##            sufixe : un sufixe pour le nom de fichier.
    ##            type : type d'analyse, pour traitement conditionnel des
    ##                   titres et noms de fichiers.
    ##            baseEnv : environnement de l'interface.
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 25 août 2010, 16:19

    ## longueur des lignes pour les sorties textes :
    oOpt <- options()
    on.exit(options(oOpt))

    options(width=120)

    ## Formule de modèle lisible:
    objMRT$call$formula <- formule
    formule <<- formule
    resMRT <<- objMRT

    ## Chemin et nom de fichier :
    resFile <- resFileMRT.f(metrique=metrique, factAna=factAna,
                            modSel=modSel, listFact=listFact,
                            dataEnv=dataEnv,
                            sufixe=sufixe, type=type)
    on.exit(close(resFile), add=TRUE)

    ## Écriture des résultats :

    cat("Appel :\n", file=resFile, append=FALSE)
    dput(objMRT$call, file=resFile)

    cat("\n\n----------------------------------------------------------------------------------------------------",
        file=resFile, append=TRUE)

    cat("\nRésultat général :\n\n", file=resFile, append=TRUE)

    capture.output(print.rpart.fr(objMRT), file=resFile, append=TRUE)

    cat("\n\n----------------------------------------------------------------------------------------------------",
        file=resFile, append=TRUE)

    cat("\nDétails :\n\n", file=resFile, append=TRUE)

    capture.output(summary.rpart.fr(objMRT), file=resFile, append=TRUE)

    ## ##################################################
    ## Sauvegarde des données :
    filename <- summary(resFile)$description

    ## close(resFile)                      # Maintenant seulement on peut fermer ce fichier.

    if (getOption("P.saveData") &&  ! isTRUE(sufixe == "(red)"))
    {
        writeData.f(filename=filename, Data=Data,
                    cols=NULL)
    }else{}

    ## Sauvegarde des infos sur les données et statistiques :
    if (getOption("P.saveStats") &&  ! isTRUE(sufixe == "(red)"))
    {
        infoStats.f(filename=filename, Data=Data, agregLevel=type, type="stat",
                    metrique=metrique, factGraph=factAna, factGraphSel=modSel,
                    listFact=listFact, listFactSel=listFactSel,
                    dataEnv=dataEnv, baseEnv=baseEnv)
    }else{}
}

########################################################################################################################
WP2MRT.esp.f <- function(metrique, factGraph, factGraphSel, listFact, listFactSel, tableMetrique, dataEnv,
                         baseEnv=.GlobalEnv)
{
    ## Purpose: Produire des arbres de régression multivariée en tenant
    ##          compte des options graphiques + Sorties texte.
    ## ----------------------------------------------------------------------
    ## Arguments: metrique : la métrique choisie.
    ##            factGraph : le facteur de séparation des graphiques.
    ##            factGraphSel : la sélection de modalités pour ce dernier.
    ##            listFact : liste du (des) facteur(s) de regroupement.
    ##            listFactSel : liste des modalités sélectionnées pour ce(s)
    ##                          dernier(s).
    ##            tableMetrique : nom de la table de métriques.
    ##            dataEnv : environnement de stockage des données.
    ##            baseEnv : environnement de l'interface.
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 11 mai 2011, 10:09

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
    exprMRT <- eval(parse(text=paste(metrique, "~", paste(listFact, collapse=" + "))))

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
                                                 "espece"),
                                     typeGraph="MRT")

        ## graphFile uniquement si nouveau fichier :
        if (!is.null(graphFileTmp)) graphFile <- graphFileTmp

        par(mar=c(2, 5, 8, 5), mgp=c(3.5, 1, 0)) # paramètres graphiques.

        ## Titre (d'après les métriques, modalité du facteur de séparation et facteurs de regroupement) :
        mainTitle <- graphTitle.f(metrique=metrique,
                                  modGraphSel=modGraphSel, factGraph=factGraph,
                                  listFact=listFact,
                                  type=switch(tableMetrique, # différents types de graphs en fonction de la table de
                                        # données.
                                              "unitSp"={"espece"},
                                              "unitSpSz"={"CL_espece"},
                                              "unit"={"biodiv"},
                                              "espece"),
                                  model="Arbre de régression multivariée")

        ## Boxplot !
        tmpMRT <- rpart(exprMRT, data=tmpDataMod)

        ## plot(tmpMRT, main=mainTitle)
        ## text(tmpMRT, use.n=TRUE, pretty=1, all=TRUE, xpd=NA, bars=TRUE)

        plot(tmpMRT, main=mainTitle)
        text.rpart.new(tmpMRT, use.n=TRUE, pretty=0, all=TRUE, xpd=NA)

        ## Écriture des résultats formatés dans un fichier :
        tryCatch(sortiesMRT.f(objMRT=tmpMRT, formule=exprMRT,
                              metrique=metrique,
                              factAna=factGraph, modSel=modGraphSel,
                              listFact=listFact, listFactSel=listFactSel,
                              Data=tmpDataMod, dataEnv=dataEnv,
                              type=ifelse(tableMetrique == "unitSpSz",
                                          "CL_espece",
                                          "espece"),
                              baseEnv=baseEnv),
                 error=errorLog.f)

        ## On ferme les périphériques PNG en mode fichier individuel :
        if (isTRUE(getOption("P.graphPNG")))
        {
            if ((! getOption("P.plusieursGraphPage") || length(iFactGraphSel) <= 1))
                dev.off()
        }else{
            ## Sauvegarde en wmf si pertinent et souhaité :
            if (.Platform$OS.type == "windows" && isTRUE(getOption("P.graphWMF")) &&
                (! getOption("P.plusieursGraphPage") || length(iFactGraphSel) <= 1) &&
                !getOption("P.graphPDF"))
            {
                savePlot(graphFile, type="wmf", device=dev.cur())
            }else{}
        }
    }

    ## On ferme les périphériques PDF ou PNG restants :
    if (getOption("P.graphPDF") ||
        (isTRUE(getOption("P.graphPNG")) && getOption("P.plusieursGraphPage") && length(iFactGraphSel) > 1))
    {
            dev.off()

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
                             warning("Impossible d'inclure les fontes dans le PDF !")
                         })

                    i <- i + 1
                }
            }else{}
    }else{}

    ## Sauvegarde en wmf restants si pertinent et souhaité :
    if (.Platform$OS.type == "windows" && isTRUE(getOption("P.graphWMF")) &&
        !(getOption("P.graphPNG") || getOption("P.graphPDF")) &&
        getOption("P.plusieursGraphPage") && length(iFactGraphSel) > 1)
    {
        savePlot(graphFile, type="wmf", device=dev.cur())
    }else{}

    pampaProfilingEnd.f()
}








### Local Variables:
### ispell-local-dictionary: "english"
### fill-column: 120
### End:
