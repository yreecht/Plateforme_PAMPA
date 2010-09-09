#-*- coding: latin-1 -*-

### File: comparaison_distri_generique.R
### Time-stamp: <2010-09-09 14:14:26 yreecht>
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
    ##          périphérique graphique + gestion des différents systèmes
    ##          d'exploitation/versions de R.
    ## ----------------------------------------------------------------------
    ## Arguments: ceux de .my.tkdev original + ...
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 23 août 2010, 12:41

    if (Sys.info()["sysname"] == "Windows") # Système Windows
    {
        if(R.version$major == 2 && R.version$minor < 3)
        {
            win.metafile(width=4*hscale, height=4*vscale,...)
        }else{
            win.metafile(width=4*hscale, height=4*vscale, restoreConsole=FALSE,...)
        }
    }else{                              # Systèmes Unix(-like).
        if (exists("X11", env=.GlobalEnv))
        {
            X11("XImage", 480*hscale, 480*vscale,...)
        }else{
            stop("tkrplot only supports Windows and X11")
        }
    }
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

    ## Périphérique graphique :
    .my.tkdev(hscale, vscale,...)

    try(fun())
    .Tcl(paste("image create Rplot", image))

    lab <- tklabel(parent, image = image)
    tkbind(lab, "<Destroy>", function() {.Tcl(paste("image delete", image))})
    lab$image <- image
    lab$fun <- fun
    lab$hscale <- hscale
    lab$vscale <- vscale
    return(lab)
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

    ## Françisation des en-têtes (gsub itératif) :
    attr(x, "heading") <- iter.gsub(pattern=c("Analysis of Deviance Table",
                                              "Analysis of Variance Table",
                                              "Model:",
                                              "Negative Binomial",
                                              "Terms added sequentially \\(first to last\\)",
                                              "Response:",
                                              "link:"),
                                    replacement=c("\n---------------------------------------------------------------------------\nTable d'analyse de la déviance :",
                                                  "\n---------------------------------------------------------------------------\nTable d'analyse de la variance :",
                                                  "Modèle :",
                                                  "Binomiale négative",
                                                  "Termes ajoutés séquentiellement (premier au dernier)",
                                                  "Réponse :",
                                                  "lien :"),
                                    x=attr(x, "heading"), fixed=TRUE)

    ## Définitions issues de la fonction originale :
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
    ## Purpose: Retourne un vecteur de booléen donnant les indices de ligne
    ##          de la matrice de coefs correspondant à des facteurs ou
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
    ##            objLM : objet de classe 'lm' ou 'glm'
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 31 août 2010, 10:46

    ## Sélection des coefficients à montrer (pour effets/interactions significatifs) :
    x <- x[selRowCoefmat(x, anovaLM, objLM), , drop=FALSE]

    ## Définitions issues de la fonction originale :
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
print.summary.glht.red <- function (x, digits = max(3, getOption("digits") - 3), ...)
{
    ## cat("\n\t", "Simultaneous Tests for General Linear Hypotheses\n\n")
    if (!is.null(x$type))
        cat("Comparaisons multiples de moyennes :", x$type, "Contrastes\n\n\n")
    call <- if (isS4(x$model))
        x$model@call
    else x$model$call
    ## if (!is.null(call)) {
    ##     cat("Fit: ")
    ##     print(call)
    ##     cat("\n")
    ## }
    cat("\n")
    pq <- x$test
    mtests <- cbind(pq$coefficients, pq$sigma, pq$tstat, pq$pvalues)
    error <- attr(pq$pvalues, "error")
    pname <- switch(x$alternativ, less = paste("Pr(<", ifelse(x$df ==
        0, "z", "t"), ")", sep = ""), greater = paste("Pr(>",
        ifelse(x$df == 0, "z", "t"), ")", sep = ""), two.sided = paste("Pr(>|",
        ifelse(x$df == 0, "z", "t"), "|)", sep = ""))
    colnames(mtests) <- c("Estimate", "Std. Error", ifelse(x$df ==
        0, "z value", "t value"), pname)
    type <- pq$type
    if (!is.null(error) && error > .Machine$double.eps) {
        sig <- which.min(abs(1/error - (10^(1:10))))
        sig <- 1/(10^sig)
    }
    else {
        sig <- .Machine$double.eps
    }
    cat("Hypothèses linéaires :\n")
    alt <- switch(x$alternative, two.sided = "==", less = ">=",
        greater = "<=")
    rownames(mtests) <- paste(rownames(mtests), alt, x$rhs)
    printCoefmat(mtests, digits = digits, has.Pvalue = TRUE,
        P.values = TRUE, eps.Pvalue = sig)
    switch(type, univariate = cat("(P-valeurs univariées)"),
        `single-step` = cat("(P-valeurs ajustées -- méthode 'single-step')"),
        Shaffer = cat("(P-valeurs ajustées -- méthode de Shaffer)"),
        Westfall = cat("(P-valeurs ajustées -- méthode de Westfall)"),
        cat("(P-valeurs ajustées --", type, "method)"))
    cat("\n\n")
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

    ## browser(condition=(family == "NBI"))  ## [!!!] attention, il arrive que les calculs bloquent ici lors du premier
    ## lancement. (origine inconnue)
    ## On ajuste la distribution :
    try(coefLoi <- fitdistr(y, densfun=loi$densfunName))

    ## Calcul des points théoriques à représenter :
    expr <- parse(text=paste(loi$densfun, "(xi, ",       # points à représenter.
                             paste(names(coefLoi$estimate), coefLoi$estimate, sep="=", collapse=", "), # coefs estimés.
                             ")", sep=""))

    yi <- eval(expr)                    # valeurs pour la loi de distribution théorique ajustée.

    ## Représentation graphique :
    nbreaks <- 60                       # Nombre de barres.

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

    ## Si un environnement est précisé, la valeur est sauvegardée dans une liste 'distList' :
    if (!is.null(env))
    {
        eval(substitute(evalq(distList[[family]] <- res, envir=env), list(family=eval(family), res=eval(res))))
    }else{}

    ## Retourne le résultat :
    return(res)
}



########################################################################################################################
diffSpatiales.f <- function(objLM, factSpatial, factTemp, Data)
{
    ## Purpose: Calcule et retourne la matrice de différences spatiales par
    ##          année (pour une utilisation avec la fonction 'glht').
    ## ----------------------------------------------------------------------
    ## Arguments: objLM : un objet de classe 'lm' ou 'glm'.
    ##            factSpatial : nom du facteur spatial.
    ##            factTemp : nom du facteur temporel.
    ##            Data : données utilisées pour ajuster le modèle.
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date:  7 sept. 2010, 16:15

    ## Coefficients :
    theta <- coef(objLM)

    ## Nom des différences spatiales (statut de protection) :
    sDiff <- apply(combn(levels(Data[ , factSpatial]), 2),
                   2,
                   function(x){paste(rev(x), collapse = " - ")})

    ## Matrice pour construire les différences entre coefficients :
    Dspat <- matrix(0,
                    nrow=nlevels(Data[ , factTemp]) * choose(nlevels(Data[ , factSpatial]), 2),
                    ncol=length(theta))

    ## Noms des colonnes (pas obligatoire mais utile pour vérification) :
    row.names(Dspat) <- paste(levels(Data[ , factTemp]),
                              rep(sDiff, each=nlevels(Data[ , factTemp])), sep=" : ")
    colnames(Dspat) <- names(theta)

    ## Calculs des nombres de colonnes des facteurs et intéraction :
    nlev <- combn(sapply(Data, function(x)
                     {
                         ifelse(is.factor(x),
                                nlevels(x) - 1,
                                1)
                     }),
                  2)

    ## Nombre de colonnes par type de facteur/interaction :
    nCol <- apply(nlev, 2, prod)

    ## Position de la première colonne
    premiereCol <- cumsum(c(1, nCol[- length(nCol)])) + 1

    ## Position des facteurs d'intérêt et leur interaction,
    ## dans l'ordre de l'ensemble des facteurs et interactions :
    facts <- c(factSpatial, factTemp)
    posTemp <- which(attr(res$terms, "term.labels") == factTemp)
    posSpatial <- which(attr(res$terms, "term.labels") == factSpatial)
    posInteraction <- which(is.element(attr(res$terms, "term.labels"),
                                       paste(facts, rev(facts), sep=":")))

    ## Différences entres les effets statuts (sans intéraction temporelles) :
    tmp <- sapply(as.data.frame(combn(1:nlevels(Data[ , factSpatial]), 2)),
                  function(x)
              {
                  m <- matrix(0,
                              ncol=nlevels(Data[ , factSpatial]),
                              nrow=nlevels(Data[ , factTemp]))
                  m[ , x] <- matrix(c(-1, 1),
                                    nrow=nlevels(Data[ , factTemp]),
                                    ncol=2,
                                    byrow=TRUE)
                  return(m)
              }, simplify=FALSE)

    m <- tmp[[1]][NULL, ]
    for(i in 1:length(tmp))
    {
        m <- rbind(m, tmp[[i]])
    }

    Dspat[ , premiereCol[posSpatial] - 1 + 1:nCol[posSpatial]] <- m[ , -1]

    ## Ajout des intéractions :
    tmp2 <- Dspat[ , seq(from=premiereCol[posInteraction], length.out=nCol[posInteraction])]

    l <- 1
    for (i in as.data.frame(combn(0:nCol[posSpatial], 2))) # pour chaque combinaison de statut :
    {
        if(i[1] != 0)
        {
            d1 <- rbind(0, diag(-1, nrow=nCol[posTemp]))
            if (posSpatial > posTemp)   # facteur spatial après le facteur temporel...
            {
                tmp2[seq(from=l, length.out=nlevels(Data[ , factTemp])),
                     seq(from=(i[1] - 1) * nCol[posTemp] + 1, length.out=nCol[posTemp])] <- d1
            }else{                      # ... avant le facteur temporel.
                tmp2[seq(from=l, length.out=nlevels(Data[ , factTemp])),
                     seq(from=i[1], by=nCol[posSpatial] , length.out=nCol[posTemp])] <- d1
            }
        }else{}

        d2 <- rbind(0, diag(1, nrow=nCol[posTemp]))

        if (posSpatial > posTemp)       # facteur spatial après le facteur temporel...
        {
            tmp2[seq(from=l, length.out=nlevels(Data[ , factTemp])),
                 seq(from=(i[2] - 1) * nCol[posTemp] + 1, length.out=nCol[posTemp])] <- d2
        }else{                          # ... avant le facteur temporel.
            tmp2[seq(from=l, length.out=nlevels(Data[ , factTemp])),
                 seq(from=i[2], by=nCol[posSpatial], length.out=nCol[posTemp])] <- d2
        }

        l <- l + nlevels(Data[ , factTemp])
    }

    ## Stockage des différences d'interactions :
    Dspat[ , seq(from=premiereCol[posInteraction], length.out=nCol[posInteraction])] <- tmp2

    return(Dspat)
}


########################################################################################################################
diffTemporelles.f <- function(objLM, factSpatial, factTemp, Data)
{
    ## Purpose: Calcule et retourne la matrice de différences temporelles par
    ##          statut(pour une utilisation avec la fonction 'glht').
    ## ----------------------------------------------------------------------
    ## Arguments: objLM : un objet de classe 'lm' ou 'glm'.
    ##            factSpatial : nom du facteur spatial.
    ##            factTemp : nom du facteur temporel.
    ##            Data : données utilisées pour ajuster le modèle.
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date:  8 sept. 2010, 11:11

    ## Coefficients :
    theta <- coef(objLM)

    tDiff <- paste(c(head(rev(levels(Data[ , factTemp])), 1), head(rev(levels(Data[ , factTemp])),  - 1)),
                   c(tail(rev(levels(Data[ , factTemp])), 1), tail(rev(levels(Data[ , factTemp])),  - 1)),
                   sep=" - ")

    ## Matrice pour construire les différences entre coefficients :
    Dtemp <- matrix(0,
                    ## Il y a autant de différences temporelles que de niveaux pour la variable temporelle (en raison de
                    ## la différence supplémentaire final - initial) :
                    nrow=nlevels(Data[ , factTemp]) * nlevels(Data[ , factSpatial]),
                    ncol=length(theta))

    ## Noms des colonnes (pas obligatoire mais utile pour vérification) :
    row.names(Dtemp) <- paste(rep(levels(Data[ , factSpatial]), each=nlevels(Data[ , factTemp])),
                              tDiff,
                              sep=" : ")


    colnames(Dtemp) <- names(theta)

    ## Calculs des nombres de colonnes des facteurs et intéraction :
    nlev <- combn(sapply(Data, function(x)
                     {
                         ifelse(is.factor(x),
                                nlevels(x) - 1,
                                1)
                     }),
                  2)

    ## Nombre de colonnes par type de facteur/interaction :
    nCol <- apply(nlev, 2, prod)

    ## Position de la première colonne
    premiereCol <- cumsum(c(1, nCol[- length(nCol)])) + 1

    ## Position des facteurs d'intérêt et leur interaction,
    ## dans l'ordre de l'ensemble des facteurs et interactions :
    facts <- c(factSpatial, factTemp)
    posTemp <- which(attr(res$terms, "term.labels") == factTemp)
    posSpatial <- which(attr(res$terms, "term.labels") == factSpatial)
    posInteraction <- which(is.element(attr(res$terms, "term.labels"),
                                       paste(facts, rev(facts), sep=":")))

    ## Différences sur l'effet temporel seul :
    d1 <- rbind(c(-1, rep(0, nCol[posTemp] - 1), 1),
                cbind(0, diag(1, nCol[posTemp])[ , seq(nCol[posTemp], 1)]) +
                cbind(diag(-1, nCol[posTemp])[ , seq(nCol[posTemp], 1)], 0))[ , -1]

    Dtemp[ , seq(from=premiereCol[posTemp],
                 length.out=nCol[posTemp])] <- sapply(as.data.frame(d1), rep, nlevels(Data[ , factSpatial]))

    ## Différences sur les interactions :
    d2 <- Dtemp[ , seq(from=premiereCol[posInteraction],
                        length.out=nCol[posInteraction])]

    l <- nlevels(Data[ , factTemp]) + 1
    for (i in seq(from=0, length.out=nCol[posSpatial]))
    {
        if (posSpatial > posTemp)       # traitement différent selon l'imbrication des facteurs :
        {                               # Cas où le facteur temporel est en premier :
            d2[seq(from=l, length.out=nlevels(Data[ , factTemp])) ,
               seq(from=1, length.out=nCol[posTemp]) + i * nCol[posTemp]] <- d1
        }else{                          #... cas où il est en second :
            d2[seq(from=l, length.out=nlevels(Data[ , factTemp])) ,
               seq(from=1 + i, by=nCol[posSpatial], length.out=nCol[posTemp])] <- d1
        }

        l <- l + nlevels(Data[ , factTemp])
    }

    Dtemp[ , seq(from=premiereCol[posInteraction],
                 length.out=nCol[posInteraction])] <- d2

    return(Dtemp)

}


########################################################################################################################
resFileLM.f <- function(objLM, metrique, factAna, modSel, listFact, Log=FALSE,  prefix=NULL, ext="txt")
{
    ## Purpose: Définit les noms du fichiers pour les résultats des modèles
    ##          linéaires. L'extension et un prefixe peuvent êtres précisés,
    ##          mais par défaut, c'est le fichier de sorties texte qui est
    ##          créé.
    ## ----------------------------------------------------------------------
    ## Arguments: objLM : un objet de classe 'lm' ou 'glm'.
    ##            metrique : nom de la métrique analysée.
    ##            factAna : nom du facteur de séprataion des analyses.
    ##            modSel : modalité de factAna sélectionnée.
    ##            listFact : vecteur des noms de facteurs de l'analyse.
    ##            Log : Est-ce que les données sont log-transformées.
    ##            prefix : préfixe du nom de fichier.
    ##            ext : extension du fichier.
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date:  8 sept. 2010, 15:48

    ## si pas de préfix fourni :
    if (is.null(prefix))
    {
        prefix <- ifelse(length(grep("^lm\\(", deparse(objLM$call), perl=TRUE)) > 0,
                         paste("LM", ifelse(Log, "-log", ""), sep=""),
                         ifelse(length(grep("^glm\\.nb", deparse(objLM$call), perl=TRUE)) > 0,
                                "GLM-NB",
                                ifelse(length(grep("^glm.*poisson", deparse(objLM$call), perl=TRUE)) > 0,
                                       "GLM-P",
                                       "Unknown-model")))
    }else{}

    ## Nom de fichier :
    filename <- paste(nameWorkspace, "/FichiersSortie/", prefix, "_",
                      ## Métrique analysée :
                      metrique, "_",
                      ## si facteur de séparation des analyses :
                      ifelse(factAna == "",
                             "",
                             paste(factAna, "(", ifelse(modSel != "", modSel, "toutes"), ")_", sep="")),
                      ## liste des facteurs de l'analyse
                      paste(listFact, collapse="-"),
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
valPreditesLM.f <- function(objLM, Data, listFact, resFile)
{
    ## Purpose:
    ## ----------------------------------------------------------------------
    ## Arguments: objLM : objet de classe 'lm' ou 'glm'.
    ##            Data : les données utilisées pour ajuster le modèle.
    ##            listFact : un vecteur donnant la liste des noms de
    ##                       facteurs.
    ##            resFile : la connection au fichier résultat
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date:  8 sept. 2010, 16:12


    ## ##################################################
    ## Valeurs prédites :
    OrdreNivFact <- sapply(unique(Data[ , listFact]), as.numeric)

    if (!is.matrix(OrdreNivFact))       # Si un seul facteur, on transforme le vecteur d'ordre des niveaux en matrice.
    {
        OrdreNivFact <- matrix(OrdreNivFact, ncol=1, dimnames=list(NULL, listFact))
    }else{}

    ## Valeurs prédites pour chaque combinaison réalisée des facteurs :
    if (length(grep("^glm", objLM$call)) > 0)
    {
        valPredites <- predict(objLM, newdata=unique(Data[ , listFact, drop=FALSE]), type="response")
    }else{
        valPredites <- predict(objLM, newdata=unique(Data[ , listFact, drop=FALSE]))
    }

    ## Noms des valeurs prédites (combinaisons des différents niveaux de facteurs) :
    nomCoefs <- unique(apply(Data[ , listFact, drop=FALSE], 1, paste, collapse=":"))
    names(valPredites) <- nomCoefs

    ## On remet les modalités en ordre :
    valPredites <- valPredites[eval(parse(text=paste("order(",
                                          paste("OrdreNivFact[ , ", 1:ncol(OrdreNivFact), "]", sep="", collapse=", "),
                                          ")", sep="")))]

    ## Écriture de l'en-tête :
    cat("\n\n\n---------------------------------------------------------------------------",
        "\nValeurs prédites par le modèle :\n\n",
        file=resFile)

    ## Écriture du résultat :
    capture.output(print(valPredites), file=resFile)

}


########################################################################################################################
compMultiplesLM.f <- function(objLM, Data, factSpatial, factTemp, resFile)
{
    ## Purpose: Calculer et écrire les résultats des comparaisons multiples.
    ## ----------------------------------------------------------------------
    ## Arguments: objLM : objet de classe 'lm' ou 'glm'.
    ##            Data : les données utilisées pour ajuster le modèle.
    ##            factSpatial : le nom du facteur utilisé pour les
    ##                          comparaisons spatiales/de statut.
    ##            factTemp : le nom du facteur utilisé pour les comparaisons
    ##                       temporelles.
    ##            resFile : la connection pour les sorties textes.
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date:  8 sept. 2010, 16:38

    ## écriture des en-têtes :
    cat("\n\n\n---------------------------------------------------------------------------",
        "\nComparaisons multiples :",
        file=resFile)

    ## Calcul de la matrice de différences spatiales/de statut :
    Dspat <- diffSpatiales.f(objLM=objLM,
                             factSpatial="statut_protection",
                             factTemp="an",
                             Data=Data)

    ## Calcul de la matrice de différences temporelles :
    Dtemp <- diffTemporelles.f(objLM=objLM,
                               factSpatial="statut_protection",
                               factTemp="an",
                               Data=Data)

    ## Si des coefs n'ont pu être calculés, glht plante... à moins que :
    if (any(is.na(coef(objLM))))
    {
        ## Avertissement :
        cat("\n\n\tAttention : les matrices de différences ont été réduites en raison de ",
            "\n\tcoefficients non calculables (absence de données pour certains ",
            "\n\tniveaux de facteurs/interactions).\n",
            file=resFile)

        ## Réduction des matrices de différences :
        Dspat <- Dspat[ , !is.na(coef(objLM))]
        Dtemp <- Dtemp[ , !is.na(coef(objLM))]

        objLM$coefficients <- objLM$coefficients[!is.na(coef(objLM))]
    }

    ## Résultats des comparaisons spatiales/de statut :
    cat("\n\nComparaisons pour les différences spatiales (statut de protection) par année :\n",
        file=resFile)

    capture.output(print.summary.glht.red(summary(glht(objLM, linfct=Dspat, alternative="two.sided"))),
                   file=resFile)

    ## Résultats des comparaisons temporelles :
    cat("\n\nComparaisons pour les différences temporelles par statut de protection :\n",
        file=resFile)

    capture.output(print.summary.glht.red(summary(glht(objLM, linfct=Dtemp, alternative="two.sided"))),
                   file=resFile)


}


########################################################################################################################
infoStatLM.f <- function(objLM, resFile)
{
    ## Purpose: Écrit les informations sur le modèle insi que les
    ##          statistiques globale dans un fichier résultat
    ## ----------------------------------------------------------------------
    ## Arguments: objLM un objet de classe 'lm' ou 'glm'.
    ##            resFile : une connection pour les sorties.
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date:  8 sept. 2010, 16:57

    ## [!!!] Attention, il arrive que les calculs bloquent ici lors du premier lancement (origine inconnue)
    sumLM <- switch(class(objLM)[1],
                    lm = summary.lm(objLM),
                    glm = summary.glm(objLM),
                    negbin = MASS:::summary.negbin(objLM),
                    summary(objLM))

    ## Informations sur le modèle :
    cat("Modèle ajusté :", file=resFile, fill=1)
    cat("\t", deparse(objLM$call), "\n\n\n", file=resFile, sep="")

    ## Stats globales :
    if (length(grep("^glm", objLM$call)) == 0)
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
}


########################################################################################################################
signifParamLM.f <- function(objLM, resFile)
{
    ## Purpose: Écrire les résultats de l'anova globale du modèle et
    ##          l'estimation de significativités des coefficients du modèle.
    ## ----------------------------------------------------------------------
    ## Arguments: objLM un objet de classe 'lm' ou 'glm'.
    ##            resFile : une connection pour les sorties.
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date:  8 sept. 2010, 17:07

    ## Anovas et résumés :
    if (length(grep("^glm", objLM$call)) > 0) # Pour les GLMs.
    {
        anovaLM <- anova(objLM, test="Chisq") # Pour les LMs.
    }else{
        anovaLM <- anova(objLM)
    }
    sumLM <- summary(objLM)

    ## Anova globale du modèle :
    capture.output(print.anova.fr(anovaLM), file=resFile)

    ## Significativités des paramètres :
    cat("\n\nSignificativités des paramètres ",
        "\n(seuls ceux correspondant à des facteurs/intéractions significatifs sont représentés) :\n\n",
        file=resFile)

    capture.output(printCoefmat.red(sumLM$coef, anovaLM=anovaLM, objLM=objLM), file=resFile)
}


########################################################################################################################
sortiesLM.f <- function(objLM, formule, metrique, factAna, modSel, listFact, Data, Log=FALSE)
{
    ## Purpose: Formater les résultats de lm et les écrire dans un fichier
    ## ----------------------------------------------------------------------
    ## Arguments: objLM : un objet de classe lm
    ##            formule : la formule utilisée (pas lisible dans le call).
    ##            metrique : la métrique choisie.
    ##            factAna : le facteur de séparation des analyses.
    ##            modSel : la modalité courante.
    ##            listFact : liste du (des) facteur(s) de regroupement.
    ##            Data : les données utilisées.
    ##            Log : données log-transformées ou non (booléen).
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 25 août 2010, 16:19

    ## longueur des lignes pour les sorties textes :
    oOpt <- options()
    on.exit(options(oOpt))

    options(width=120)

    ## Formule de modèle lisible:
    objLM$call$formula <- formule
    formule <<- formule

    ## Chemin et nom de fichier :
    resFile <- resFileLM.f(objLM=objLM, metrique=metrique, factAna=factAna, modSel=modSel, listFact=listFact, Log=Log)
    on.exit(close(resFile), add=TRUE)


    ## Informations et statistiques globales sur le modèle :
    infoStatLM.f(objLM=objLM, resFile=resFile)


    ## Anova globale du modèle + significativité des coefficients :
    signifParamLM.f(objLM=objLM, resFile=resFile)


    ## ##################################################
    ## Valeurs prédites par le modèle :
    valPreditesLM.f(objLM=objLM, Data=Data, listFact=listFact, resFile=resFile)

    ## ##################################################
    ## Comparaisons multiples :

    if (all(is.element(c("an", "statut_protection"), listFact)))
    {
        compMultiplesLM.f(objLM=objLM, Data=Data, factSpatial="statut_protection", factTemp="an", resFile=resFile)

        ## Représentation des interactions
        with(Data,
             interaction.plot(an, statut_protection, eval(parse(text=metrique)),
                              ylab=paste(Capitalize.f(varNames[metrique, "nom"]), "moyenne")))
    }else{}

    ## flush.console()
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

    ## Sauvegarde temporaire des données utilisées pour les analyses (attention : écrasée à chaque nouvelle série de
    ## graphiques) :
    DataBackup <<- list()

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

        ## Suppression des 'levels' non utilisés :
        tmpDataMod <- dropLevels.f(tmpDataMod)

        ## Sauvegarde temporaire des données :
        DataBackup[[modSel]] <<- tmpDataMod

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
                       ## sortiesLM.f(lm=res, formule=exprML, metrique, factAna, modSel, listFact)
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
                       ## sortiesLM.f(lm=res, formule=logExprML, metrique, factAna, modSel, listFact, Log=TRUE)
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

            sortiesLM.f(objLM=res, formule=formule, metrique=metrique,
                        factAna=factAna, modSel=modSel, listFact=listFact,
                        Data=tmpDataMod, Log=Log)

        }else{
            message("Annulé !")
        }

    }
}







### Local Variables:
### ispell-local-dictionary: "english"
### fill-column: 120
### End:
