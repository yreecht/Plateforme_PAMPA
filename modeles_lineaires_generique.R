#-*- coding: latin-1 -*-

### File: comparaison_distri_generique.R
### Time-stamp: <2010-09-01 11:37:49 yreecht>
###
### Author: Yves Reecht
###
####################################################################################################
### Description:
###
### Interface de comparaison des distributions d'une métrique :
####################################################################################################

iter.gsub <- function(pattern, replacement, x,...)
{
    if (length(pattern) > 0)
    {
        return(gsub(pattern=pattern[1],
                    replacement=replacement[1],
                    x=iter.gsub(pattern=pattern[-1], replacement=replacement[-1], x=x),
                    ...))
    }else{
        return(x)
    }
}


########################################################################################################################
.my.tkdev <- function (hscale = 1, vscale = 1,...)
{
    ## Purpose: écraser la définition de .my.tkdev du packages tkrplot pour
    ##          permettre de passer des options supplémentaires au
    ##          périphérique graphique.
    ## ----------------------------------------------------------------------
    ## Arguments: ceux de .my.tkdev original + ...
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 23 août 2010, 12:41

    win.metafile(width = 4 * hscale, height = 4 * vscale,...)
}

tkrplot <- function(parent, fun, hscale = 1, vscale = 1,...)
{
    ## Purpose: écraser la définition de tkrplot du packages tkrplot pour
    ##          permettre de passer des options supplémentaires au
    ##          périphérique graphique.
    ## ----------------------------------------------------------------------
    ## Arguments: ceux de tkrplot original + ...
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 23 août 2010, 12:46
    image <- paste("Rplot", .make.tkindex(), sep = "")
    .my.tkdev(hscale, vscale,...)
    try(fun())
    .Tcl(paste("image create Rplot", image))
    lab <- tklabel(parent, image = image)
    tkbind(lab, "<Destroy>", function() .Tcl(paste("image delete",
        image)))
    lab$image <- image
    lab$fun <- fun
    lab$hscale <- hscale
    lab$vscale <- vscale
    lab
}

########################################################################################################################
print.anova.fr <- function(x, digits = max(getOption("digits") - 2, 3), signif.stars = getOption("show.signif.stars"),
                           ...)
{
    ## Purpose: Hack de la méthode print.anova pour franciser les sorties et
    ##          supprimer les infos inutiles.
    ## ----------------------------------------------------------------------
    ## Arguments: ceux de print.anova
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 26 août 2010, 11:36

    attr(x, "row.names")[attr(x, "row.names") == "Residuals"] <- "Résidus"

    ## if (length(attr(x, "heading")) == 2)
    ## {
    ##     attr(x, "heading") <- c("Table d'analyse de la variance :\n",
    ##                             sub("^Response:", "Réponse :", attr(x, "heading")[2]))
    ## }else{
    attr(x, "heading") <- iter.gsub(pattern=c("Analysis of Deviance Table",
                                              "Analysis of Variance Table",
                                              "Model:",
                                              "Negative Binomial",
                                              "Terms added sequentially \\(first to last\\)",
                                              "Response:",
                                              "link:"),
                                    replacement=c("Table d'analyse de la déviance :",
                                                  "Table d'analyse de la variance :",
                                                  "Modèle :",
                                                  "Binomiale négative",
                                                  "Termes ajoutés séquentiellement (premier au dernier)",
                                                  "Réponse :",
                                                  "lien :"),
                                    x=attr(x, "heading"), fixed=TRUE)
    ## }

    if (!is.null(heading <- attr(x, "heading")))
    {
        cat(heading, sep = "\n")
    }else{}

    nc <- dim(x)[2L]
    if (is.null(cn <- colnames(x)))
    {
        stop("'anova' object must have colnames")
    }else{}
    has.P <- grepl("^(P|Pr)\\(", cn[nc])
    zap.i <- 1L:(if (has.P)
             {
                 nc - 1
             }else{
                 nc
             })
    i <- which(substr(cn, 2, 7) == " value")
    i <- c(i, which(!is.na(match(cn, c("F", "Cp", "Chisq")))))
    if (length(i))
    {
        zap.i <- zap.i[!(zap.i %in% i)]
    }else{}
    tst.i <- i
    if (length(i <- grep("Df$", cn)))
    {
        zap.i <- zap.i[!(zap.i %in% i)]
    }else{}
    printCoefmat(x, digits = digits, signif.stars = signif.stars,
                 signif.legend=FALSE,
                 has.Pvalue = has.P, P.values = has.P, cs.ind = NULL,
                 zap.ind = zap.i, tst.ind = tst.i, na.print = "", ...)
    invisible(x)
}


selRowCoefmat <- function(coefsMat, anovaLM, objLM)
{
    ## Purpose: Retourne un vecteur de booléen donnat les indices de ligne de
    ##          la matrice de coefs correspondant à des facteurs ou
    ##          intéractions significatifs (les autres coefs n'ont pas
    ##          d'intéret).
    ## ----------------------------------------------------------------------
    ## Arguments: coefsMat : matrice de coefficients.
    ##            anovaLM : objet correspondant de classe 'anova.lm'.
    ##            objLM : l'objet de classe 'lm' (nécessaire pour traiter
    ##                    les cas de NAs)
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 31 août 2010, 14:31

    if (!is.null(anovaLM))
    {

        ## Facteurs et intéractions dont les coefs doivent être imprimés :
        selectedFactInt <- attr(anovaLM, "row.names")[which(anovaLM[[grep("P[r(]", attr(anovaLM, "names"))]] < 0.05)]

        ## Tous les facteurs :
        facts <- attr(anovaLM, "row.names")[!is.na(anovaLM[[grep("P[r(]", attr(anovaLM, "names"))]])]

        ## indices des intéractions dans "selectedFactInt" :
        has.interactions <- TRUE
        interactions <- grep(":", facts, fixed=TRUE)
        if (length(interactions) == 0)
        {
            interactions <- length(facts) + 1
            has.interactions <- FALSE
        }else{}

        ## Coefficients par facteur (sans compter les intéractions) :
        if (length(facts) == 1)
        {
            factsRows <- list(grep(paste("^", facts, sep=""), row.names(coefsMat), value=TRUE))
            names(factsRows) <- facts

        }else{
            factsRows <- ## sapply(
                sapply(facts[-c(interactions)],
                       function(fact)
                   {
                       grep(":",
                            grep(paste("^", fact, sep=""),
                                 row.names(coefsMat), value=TRUE),
                            value=TRUE, invert=TRUE, fixed=TRUE)
                   }, simplify=FALSE)## , as.vector)
        }




        if (has.interactions)
        {

        }else{}

        ## type de coef (facteur et intéractions) par ligne de la matrice de coef :
        rows <- c("(Intercept)",
                  ## facteurs :
                  unlist(sapply(1:length(factsRows),
                                function(i)
                            {
                                rep(names(factsRows)[i], length(factsRows[[i]]))
                            })))

        ## intéractions :
        if (has.interactions)
        {
            ## nombre de répétitions par type d'intéraction :

            ## liste des nombres de modalités pour chaque facteur d'une intéraction :
            nmod <- sapply(strsplit(facts[interactions], ":"),
                           function(fa) sapply(fa, function(i)length(factsRows[[i]])))

            if (!is.list(nmod))         # corrige un bug lorsqu'uniquement 1 type d'intéraction).
            {
                nmod <- list(as.vector(nmod))
            }else{}

            ## Nombres de répétitions :
            nrep <- sapply(nmod, prod)

            ## Ajout des types d'intéractions :
            rows <- c(rows,
                      unlist(sapply(1:length(nrep), function(i)
                                {
                                    rep(facts[interactions][i], nrep[i])
                                })))
        }else{}

        ## ! unlist(strsplit(selectedFact[c(interactions)], ":")) %in% selectedFact[-c(interactions)]
        ## ! selectedFact[-c(interactions)] %in% unlist(strsplit(selectedFact[c(interactions)], ":"))
        ## Merdique pour les motifs courants (e.g. "an") : plutôt utiliser les nombres de nivaux pour calculer les
        ## positions des coefs à conserver.

        ## Lignes conservées :
        return(is.element(rows, c("(Intercept)", selectedFactInt))[!is.na(objLM$coefficients)])
    }else{
        return(rep(TRUE, nrow(coefsMat)))
    }
}


printCoefmat.red <- function(x, digits = max(3, getOption("digits") - 2),
                             signif.stars = getOption("show.signif.stars"),
                             signif.legend = signif.stars, dig.tst = max(1, min(5, digits - 1)),
                             cs.ind = 1:k, tst.ind = k + 1, zap.ind = integer(0),
                             P.values = NULL,
                             has.Pvalue = nc >= 4 &&
                                          substr(colnames(x)[nc], 1, 3) == "Pr(", eps.Pvalue = .Machine$double.eps,
                             na.print = "NA",
                             anovaLM=NULL,
                             objLM=NULL,
                             ...)
{
    ## Purpose: Modification de printCoefmat pour n'afficher que les z-values
    ##          et p-values, et pour les facteurs significatife uniquement.
    ## ----------------------------------------------------------------------
    ## Arguments: ceux de printCoefmat
    ##            + anovaLM : résultat d'anova globale du modèle (pour les
    ##                        facteurs et intéractions significatifs).
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 31 août 2010, 10:46

    x <- x[selRowCoefmat(x, anovaLM, objLM), , drop=FALSE]



    if (is.null(d <- dim(x)) || length(d) != 2L)
        stop("'x' must be coefficient matrix/data frame")
    nc <- d[2L]
    if (is.null(P.values)) {
        scp <- getOption("show.coef.Pvalues")
        if (!is.logical(scp) || is.na(scp)) {
            warning("option \"show.coef.Pvalues\" is invalid: assuming TRUE")
            scp <- TRUE
        }
        P.values <- has.Pvalue && scp
    }
    else if (P.values && !has.Pvalue)
        stop("'P.values' is TRUE, but 'has.Pvalue' is not")
    if (has.Pvalue && !P.values) {
        d <- dim(xm <- data.matrix(x[, -nc, drop = FALSE]))
        nc <- nc - 1
        has.Pvalue <- FALSE
    }
    else xm <- data.matrix(x)
    k <- nc - has.Pvalue - (if (missing(tst.ind))
        1
    else length(tst.ind))
    if (!missing(cs.ind) && length(cs.ind) > k)
        stop("wrong k / cs.ind")
    Cf <- array("", dim = d, dimnames = dimnames(xm))
    ok <- !(ina <- is.na(xm))
    for (i in zap.ind) xm[, i] <- zapsmall(xm[, i], digits)
    if (length(cs.ind)) {
        acs <- abs(coef.se <- xm[, cs.ind, drop = FALSE])
        if (any(ia <- is.finite(acs))) {
            digmin <- 1 + if (length(acs <- acs[ia & acs != 0]))
                floor(log10(range(acs[acs != 0], finite = TRUE)))
            else 0
            Cf[, cs.ind] <- format(round(coef.se, max(1, digits -
                digmin)), digits = digits)
        }
    }
    if (length(tst.ind))
        Cf[, tst.ind] <- format(round(xm[, tst.ind], digits = dig.tst),
            digits = digits)
    if (any(r.ind <- !((1L:nc) %in% c(cs.ind, tst.ind, if (has.Pvalue) nc))))
        for (i in which(r.ind)) Cf[, i] <- format(xm[, i], digits = digits)
    okP <- if (has.Pvalue)
        ok[, -nc]
    else ok
    x1 <- Cf[okP]
    dec <- getOption("OutDec")
    if (dec != ".")
        x1 <- chartr(dec, ".", x1)
    x0 <- (xm[okP] == 0) != (as.numeric(x1) == 0)
    if (length(not.both.0 <- which(x0 & !is.na(x0)))) {
        Cf[okP][not.both.0] <- format(xm[okP][not.both.0], digits = max(1,
            digits - 1))
    }
    if (any(ina))
        Cf[ina] <- na.print
    if (P.values) {
        if (!is.logical(signif.stars) || is.na(signif.stars)) {
            warning("option \"show.signif.stars\" is invalid: assuming TRUE")
            signif.stars <- TRUE
        }
        if (any(okP <- ok[, nc])) {
            pv <- as.vector(xm[, nc])
            Cf[okP, nc] <- format.pval(pv[okP], digits = dig.tst,
                eps = eps.Pvalue)
            signif.stars <- signif.stars && any(pv[okP] < 0.1)
            if (signif.stars) {
                Signif <- symnum(pv, corr = FALSE, na = FALSE,
                  cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                  symbols = c("***", "**", "*", ".", " "))
                Cf <- cbind(Cf, format(Signif))
            }
        }
        else signif.stars <- FALSE
    }
    else signif.stars <- FALSE

    ## Sélection de colonnes :
    Cf <- Cf[ , ncol(Cf) - c(2:0)]

    print.default(Cf, quote = FALSE, right = TRUE, na.print = na.print,
        ...)
    if (signif.stars && signif.legend)
        cat("---\nSignif. codes: ", attr(Signif, "legend"), "\n")
    invisible(x)
}



########################################################################################################################
plotDist.f <- function(y, family, metrique, env=NULL,...)
{
    ## Purpose: Représenter l'ajustement de distribution et retourner l'objet
    ##          (contient notamment l'AIC du modèle).
    ## ----------------------------------------------------------------------
    ## Arguments: y : les données (numeric ou integer).
    ##            family : la loi de distribution, telle que défini dans
    ##                     'gamlss.family'.
    ##            metrique : nom de la métrique.
    ##            env : l'environnement de la fonction appelante.
    ##            ... : autres arguments à passer comme
    ##                  paramètres graphiques.
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 18 août 2010, 16:46

    ## Seed (je ne sais pas si c'est vraiment nécessaire) :
    set.seed(as.integer(runif(1, min=1, max=512)))

    ## Noms et fonction de densité de la loi pour ajouter les titres ainsi qu'ajuster et représenter la distribution :
    loi <- switch(family,
                  NO=list(name="Normale", densfunName="normal", densfun="dnorm"),
                  LOGNO=list(name="log-Normale", densfunName="log-normal", densfun="dlnorm"),
                  PO=list(name="de Poisson", densfunName="poisson", densfun="dpois"),
                  NBI=list(name="Binomiale négative", densfunName="negative binomial", densfun="dnbinom"))

    ## Traitement des zéros pour la loi Log-Normale :
    if (family == "LOGNO" & sum(y == 0, na.rm=TRUE))
    {
        y <- y + ((min(y, na.rm=TRUE) + 1) / 1000)
    }else{}

    ## abscisses pour la distribution théorique.
    if (is.element(family, c("PO", "NBI")))
    {
        xi <- seq(from=min(y, na.rm=TRUE), to=max(y, na.rm=TRUE))
    }else{
        xi <- seq(from=min(y, na.rm=TRUE), to=max(y, na.rm=TRUE), length.out=5000)
    }


    ## On ajuste la distribution :
    coefLoi <- fitdistr(y, densfun=loi$densfunName)

    ## Calcul des points théoriques à représenter :
    expr <- parse(text=paste(loi$densfun, "(xi, ",       # points à représenter.
                             paste(names(coefLoi$estimate), coefLoi$estimate, sep="=", collapse=", "), # coefs estimés.
                             ")", sep=""))

    yi <- eval(expr)

    ## if (family == "NBI") {xitmp <<- xi ; yitmp <<- yi ; coeftmp <<- coefLoi}

    ## Représentation graphique :
    nbreaks <- 60                       # Nombre de barres

    histTmp <- hist(y, breaks=nbreaks, plot=FALSE) # pour connaitre la fréquence maximale de la distribution observée.

    par(mar=c(3.4, 3.4, 2.5, 0.1), mgp=c(2.0, 0.7, 0), bg="white", cex=0.8, # Paramètres graphiques.
        ...)

    hist(y, breaks=nbreaks, freq=FALSE, # histogramme (distribution observée).
         ylim=c(0, ifelse(max(yi, na.rm=TRUE) > 3 * max(histTmp$density, na.rm=TRUE),
                          3 * max(histTmp$density, na.rm=TRUE),
                          1.05 * max(c(histTmp$density, yi), na.rm=TRUE))),
         xlim=c(min(y, na.rm=TRUE), max(y, na.rm=TRUE)),
         main=paste("Comparaison avec la loi ", loi$name, sep=""),
         ## cex.main=0.9,
         xlab=Capitalize.f(varNames[metrique, "nom"]),
         ylab="Densité de la métrique",
         col="lightgray")

    lines(xi, yi, lwd=2, col="red")     # courbe (distribution théorique).

    ## Calcul d'AIC (entre autres) :
    FA <- as.gamlss.family(family)      # On procède comme dans la fonction histDist.
    fname <- FA$family[1]

    res <- gamlss(y ~ 1, family=fname)

    ## Si un environnement est précisé, la valeur est sauvegardée dans une liste :
    if (!is.null(env))
    {
        eval(substitute(evalq(distList[[family]] <- res, envir=env), list(family=eval(family), res=eval(res))))
    }else{}

    ## Retourne le résultat :
    return(res)
}


########################################################################################################################
choixDistri.f <- function(metrique, data)
{
    ## Purpose: Aider l'utilisateur dans le choix d'une distribution de la
    ##          métrique et lancer les analyses adéquates.
    ## ----------------------------------------------------------------------
    ## Arguments: metrique : le nom de la métrique (variable dépendant)
    ##                       choisie.
    ##            data : le jeu de données contenant la métrique.
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 18 août 2010, 16:19

    ## Systématiquement détruire la fenêtre en quitant :
    on.exit(tkdestroy(WinDistri))
    ## on.exit(print("WinDistri détruite !"), add=TRUE)

    ## ##################################################
    ## Variables :
    env <- environment()                # environnement courant.
    Done <- tclVar(0)                   # État d'exécution.
    LoiChoisie <- tclVar("NO")          # Variable pour le choix de distribution théorique.
    vscale <- 0.75                      # dimension verticale des graphiques.
    hscale <- 1.2                       # dimension horizontale des graphiques.
    pointsize <- 10                     # taille du point pour les graphiques
    distList <- list()                  # liste pour le stockage des AIC et autres.


    ## ##################################################
    ## Éléments graphiques :
    WinDistri <- tktoplevel()           # Fenêtre principale.
    tkwm.title(WinDistri, paste("Choix de distribution théorique de la métrique '", metrique, "'", sep=""))

    ## Frame d'aide :
    FrameHelp <- tkframe(WinDistri)
    T.help <- tktext(FrameHelp, bg="#fae18d", font="arial", width=100,
                     height=4, relief="groove", borderwidth=2)



    ## Frame pour la loi Normale :
    FrameN <- tkframe(WinDistri, borderwidth=2, relief="groove")
    Img.N <- tkrplot(FrameN,            # Création de l'image.
                     fun=function()
                 {
                     plotDist.f(y=data[ , metrique], family="NO", metrique=metrique, env=env)
                 },
                     vscale=vscale, hscale=hscale, pointsize=pointsize)

    RB.N <- tkradiobutton(FrameN, variable=LoiChoisie, value="NO", # bouton de sélection.
                          text=paste("loi Normale (AIC=", round(distList[["NO"]]$aic, 0), "). ", sep=""))


    ## Frame pour la loi log-Normale :
    FrameLogN <- tkframe(WinDistri, borderwidth=2, relief="groove")
    Img.LogN <- tkrplot(FrameLogN, fun=function() # Création de l'image.
                    {
                        plotDist.f(y=data[ , metrique], family="LOGNO", metrique=metrique, env=env)
                    },
                        vscale=vscale, hscale=hscale, pointsize=pointsize)

    RB.LogN <- tkradiobutton(FrameLogN, variable=LoiChoisie, value="LOGNO", # bouton de sélection.
                             text=paste("loi log-Normale (AIC=", round(distList[["LOGNO"]]$aic, 0), "). ", sep=""))

    if (is.integer(data[ , metrique]))
    {
        ## Frame pour la loi de Poisson :
        FramePois <- tkframe(WinDistri, borderwidth=2, relief="groove")
        Img.Pois <- tkrplot(FramePois,  # Création de l'image.
                            fun=function()
                        {
                            plotDist.f(y=data[ , metrique], family="PO", metrique=metrique, env=env)
                        },
                            vscale=vscale, hscale=hscale, pointsize=pointsize)

        RB.Pois <- tkradiobutton(FramePois, variable=LoiChoisie, value="PO", # bouton de sélection.
                                 text=paste("loi de Poisson (AIC=", round(distList[["PO"]]$aic, 0), "). ", sep=""))

        ## Frame pour la loi bionomiale négative :
        FrameNBinom <- tkframe(WinDistri, borderwidth=2, relief="groove")
        Img.NBinom <- tkrplot(FrameNBinom, # Création de l'image.
                              fun=function()
                          {
                              plotDist.f(y=data[ , metrique], family="NBI", metrique=metrique, env=env)
                          },
                              vscale=vscale, hscale=hscale, pointsize=pointsize)

        RB.NBinom <- tkradiobutton(FrameNBinom, variable=LoiChoisie, value="NBI", # bouton de sélection.
                                   text=paste("loi Binomiale négative (AIC=",
                                              round(distList[["NBI"]]$aic, 0), "). ", sep=""))
    }else{}

    ## Boutons :
    FrameB <- tkframe(WinDistri)
    B.OK <- tkbutton(FrameB, text="     OK     ", command=function(){tclvalue(Done) <- "1"})
    B.Cancel <- tkbutton(FrameB, text="   Annuler   ", command=function(){tclvalue(Done) <- "2"})

    ## ##################################################
    ## Placement des éléments sur la grille :

    tkgrid(tklabel(WinDistri, text=" "))
    tkinsert(T.help, "end", paste("INFO :\n", # texte de l'aide.
                                  "Cette fenêtre vous permet de choisir la distribution",
                                  " la plus adaptée pour faire vos analyses.\n",
                                  "La distribution (courbe rouge) s'ajustant le mieux à vos données (histogramme) d'après \n",
                                  "le critère d'information de Akaike (AIC ; doit être le plus petit possible) est pré-sélectionnée.", sep=""))
    tkgrid(T.help)
    tkgrid(FrameHelp, column=1, columnspan=3)

    tkgrid(tklabel(WinDistri, text=" "))
    tkgrid(Img.N, columnspan=2)
    tkgrid(RB.N, row=1, sticky="e")
    tkgrid(tklabel(FrameN, text=" Modèle : ANOVA", fg="red"), row=1, column=1, sticky="w")
    tkgrid(Img.LogN, columnspan=2)
    tkgrid(RB.LogN, sticky="e")
    tkgrid(tklabel(FrameLogN, text=" Modèle : ANOVA, données log-transformées", fg="red"), row=1, column=1, sticky="w")
    tkgrid(tklabel(WinDistri, text=" "), FrameN, tklabel(WinDistri, text=" "), FrameLogN, tklabel(WinDistri, text=" "),
           sticky="ew")
    tkgrid(tklabel(WinDistri, text=" "))

    ## Évènements : sélections en cliquant sur les graphiques :
    tkbind(Img.N, "<Button-1>", function(){tclvalue(LoiChoisie) <- "NO"})
    tkbind(Img.LogN, "<Button-1>", function(){tclvalue(LoiChoisie) <- "LOGNO"})

    ## Pour les données entières seulement :
    if (is.integer(data[ , metrique]))
    {
        tkgrid(Img.Pois, columnspan=2)
        tkgrid(RB.Pois, sticky="e")
        tkgrid(tklabel(FramePois, text=" Modèle : GLM, famille 'Poisson'", fg="red"), row=1, column=1, sticky="w")
        tkgrid(Img.NBinom, columnspan=2)
        tkgrid(RB.NBinom, sticky="e")
        tkgrid(tklabel(FrameNBinom, text=" Modèle : GLM, famille 'Binomiale négative'", fg="red"), row=1, column=1, sticky="w")
        tkgrid(tklabel(WinDistri, text=" "), FramePois, tklabel(WinDistri, text=" "), FrameNBinom,
               tklabel(WinDistri, text=" "), sticky="ew")
        tkgrid(tklabel(WinDistri, text=" "))

        ## Évènements : sélections en cliquant sur les graphiques :
        tkbind(Img.Pois, "<Button-1>", function(){tclvalue(LoiChoisie) <- "PO"})
        tkbind(Img.NBinom, "<Button-1>", function(){tclvalue(LoiChoisie) <- "NBI"})
    }else{}

    ## Boutons :
    tkgrid(FrameB, column=1, columnspan=3)
    tkgrid(B.OK, tklabel(FrameB, text="                         "), B.Cancel)
    tkgrid(tklabel(WinDistri, text=" "))

    ## ##################################################
    ## Autres évènements :
    tkbind(WinDistri, "<Destroy>", function(){tclvalue(Done) <- "2"}) # en cas de destruction de la fenêtre.

    ## tkgrid.configure(T.help, sticky="")
    ## tkgrid.configure(FrameHelp, sticky="")

    tclvalue(LoiChoisie) <- names(distList)[which.min(sapply(distList, function(x){x$aic}))]

    tkwait.variable(Done)               # Attente d'une action de l'utilisateur.

    if (tclvalue(Done) == "1")
    {
        return(tclvalue(LoiChoisie))
    }else{
        return(NULL)
    }

}


########################################################################################################################
sortiesLM.f <- function(lm, formula, metrique, factAna, modSel, listFact, Log=FALSE)
{
    ## Purpose: Formater les résultats de lm et les écrire dans un fichier
    ## ----------------------------------------------------------------------
    ## Arguments: lm : un objet de classe lm
    ##            formula : la formule utilisée (pas lisible dans le call).
    ##            metrique : la métrique choisie.
    ##            factAna : le facteur de séparation des analyses.
    ##            modSel : la modalité courante.
    ##            listFact : liste du (des) facteur(s) de regroupement.
    ##            Log : données log-transformées ou non (booléen).
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 25 août 2010, 16:19

    on.exit(close(resFile))
    on.exit(options(oOpt), add=TRUE)

    oOpt <- options()
    options(width=120)


    ## Formule de modèle lisible:
    lm$call$formula <- formula

    ## lm <<- lm

    if (length(grep("^glm", lm$call)) > 0) # Pour les GLMs.
    {
        anovaLM <- anova(lm, test="Chisq") # Pour les LMs.
    }else{
        anovaLM <- anova(lm)
    }
    sumLM <- summary(lm)

    ## Chemin et nom de fichier :
    filename <- paste(nameWorkspace, "/FichiersSortie/LM", ifelse(Log, "-log", ""), "_",
                      metrique, "_",
                      ifelse(factAna == "",
                             "",
                             paste(factAna, "(", ifelse(modSel != "", modSel, "toutes"), ")_", sep="")),
                      paste(listFact, collapse="-"), ".txt", sep="")

    ## Ouverture de la connection :
    resFile <- file(filename, open="w")

    ## Informations sur le modèle :
    cat("Modèle ajusté :", file=resFile, fill=1)
    cat("\t", deparse(lm$call), "\n\n\n", file=resFile, sep="")

    ## Stats globales :
    if (length(grep("^glm", lm$call)) == 0)
    {
        cat("Statistique de Fisher Globale et R^2 :\n\n", file=resFile)
        cat("\tR^2 multiple : ", format(sumLM$r.squared, digits=3),
            " ;\tR^2 ajusté : ", format(sumLM$adj.r.squared, digits=3), "\n", file=resFile, sep="")
        cat("\tF-statistique : ",
            paste(sapply(sumLM$fstatistic, format, digits=4, nsmall=0),
                  c(" sur ", " et ", " DL,"), sep=""),
            "\tP-valeur : ",
            format.pval(pf(sumLM$fstatistic[1L], sumLM$fstatistic[2L], sumLM$fstatistic[3L], lower.tail = FALSE), digits=4),
            "\n\n\n", file=resFile, sep="")
    }else{
    }


    ## Anova globale du modèle :
    ## cat("Table d'analyse de la variance :", file=resFile, fill=2)
    ## cat("\n\tRéponse : ", row.names(attr(lm$terms, "factors"))[1], "\n", file=resFile)
    capture.output(print.anova.fr(anovaLM), file=resFile)

    ## Significativités des paramètres :
    cat("\n\nSignificativités des paramètres ",
        "\n(seuls ceux correspondant à des facteurs/intéractions significatifs sont représentés) :\n\n",
        file=resFile)

    capture.output(printCoefmat.red(sumLM$coef, anovaLM=anovaLM, objLM=lm), file=resFile)

    ## print(sapply(anova(res), function(x)x), file="", na.print=NULL)
    ## row.names(attr(res$terms, "factors"))[1]
    ## names(attr(res$terms, "dataClasses"))

    flush.console()
    ## close(resFile)

}


########################################################################################################################
modeleLineaireWP2.f <- function(metrique, factAna, factAnaSel, listFact, listFactSel, tableMetrique)
{
    ## Purpose: Gestions des différentes étapes des modèles linéaires.
    ## ----------------------------------------------------------------------
    ## Arguments: metrique : la métrique choisie.
    ##            factAna : le facteur de séparation des graphiques.
    ##            factAnaSel : la sélection de modalités pour ce dernier
    ##            listFact : liste du (des) facteur(s) de regroupement
    ##            listFactSel : liste des modalités sélectionnées pour ce(s)
    ##                          dernier(s)
    ##            tableMetrique : nom de la table de métriques.
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 18 août 2010, 15:59

    ## Nettoyage des facteurs (l'interface de sélection produit des valeurs vides) :
    listFactSel <- listFactSel[unlist(listFact) != ""]
    ## listFactSel <- listFactSel[length(listFactSel):1]

    listFact <- listFact[unlist(listFact) != ""]
    ## listFact <- listFact[length(listFact):1]

    ## Concaténation
    facteurs <- c(factAna, unlist(listFact)) # Concaténation des facteurs

    selections <- c(list(factAnaSel), listFactSel) # Concaténation des leurs listes de modalités sélectionnées

    ## Données pour la série d'analyses :
    tmpData <- subsetToutesTables.f(metrique=metrique, facteurs=facteurs, selections=selections,
                                    tableMetrique=tableMetrique, exclude = NULL)

    ## Identification des différents lots d'analyses à faire:
    if (factAna == "")                # Pas de facteur de séparation des graphiques.
    {
        iFactGraphSel <- ""
    }else{
        if (is.na(factAnaSel[1]))            # Toutes les modalités.
        {
            iFactGraphSel <- unique(as.character(sort(tmpData[ , factAna])))
        }else{                              # Modalités sélectionnées (et présentes parmi les données retenues).
            iFactGraphSel <- factAnaSel[is.element(factAnaSel, tmpData[ , factAna])]
        }
    }

    ## Formules pour différents modèles (avec ou sans transformation log) :
    exprML <- eval(parse(text=paste(metrique, "~", paste(listFact, collapse=" * "))))
    logExprML <- eval(parse(text=paste("log(", metrique, ") ~", paste(listFact, collapse=" * "))))

    ## Boucle sur les modalités de séparation des analyses :
    for (modSel in iFactGraphSel)
    {
        ## Préparation des données pour une analyse :
        if (modSel == "")          # ...si pas de facteur de séparation des graphiques [!!!] changer 'Graph'
        {
            tmpDataMod <- tmpData
        }else{                          # ...sinon.
            tmpDataMod <- subset(tmpData, tmpData[ , factAna] == modSel) # Subset des données pour la modalité.
        }

        ## Aide au choix du type d'analyse :
        loiChoisie <- choixDistri.f(metrique=metrique, data=tmpDataMod[ , metrique, drop=FALSE])

        if (!is.null(loiChoisie))
        {
            message("Loi de distribution choisie = ", loiChoisie)

            Log <- FALSE
            formule <- exprML

            switch(loiChoisie,
                   ## Modèle linéaire :
                   NO={
                       res <- lm(exprML, data=tmpDataMod)
                       ## Mise en forme :
                       ## sortiesLM.f(lm=res, formula=exprML, metrique, factAna, modSel, listFact)
                   },
                   ## Modèle linéaire, données log-transformées :
                   LOGNO={
                       ## Ajout d'une constante à la métrique si contient des zéros :
                       if (sum(tmpDataMod[ , metrique] == 0, na.rm=TRUE))
                       {
                           tmpDataMod[ , metrique] <- tmpDataMod[ , metrique] +
                               ((min(tmpDataMod[ , metrique], na.rm=TRUE) + 1) / 1000)
                       }else{}

                       res <- lm(logExprML, data=tmpDataMod)
                       ## Mise en forme :
                       Log <- TRUE
                       formule <- logExprML
                       ## sortiesLM.f(lm=res, formula=logExprML, metrique, factAna, modSel, listFact, Log=TRUE)
                   },
                   ## GLM, distribution de Poisson :
                   PO={
                       res <- glm(exprML, data=tmpDataMod, family="poisson")
                   },
                   ## GLM, distribution binomiale négative :
                   NBI={
                       res <- glm.nb(exprML, data=tmpDataMod)
                   },)

            res <<- res

            sortiesLM.f(lm=res, formula=formule, metrique, factAna, modSel, listFact, Log=Log)


        }else{
            message("Annulé !")
        }

    }
}







### Local Variables:
### ispell-local-dictionary: "english"
### fill-column: 120
### End:
