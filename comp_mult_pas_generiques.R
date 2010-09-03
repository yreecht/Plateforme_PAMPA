#-*- coding: latin-1 -*-

### File: comp_mult_pas_generique.R
### Time-stamp: <2010-09-01 17:06:46 yreecht>
###
### Author: Yves Reecht
###
####################################################################################################
### Description:
###
### tests
####################################################################################################
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

compMultSt.tmp.f <- function(objLM, data)
{
    ## Purpose:
    ## ----------------------------------------------------------------------
    ## Arguments:
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date:  1 sept. 2010, 14:46

    nblev1 <- length(levels(data$an))
    nblev2 <- length(levels(data$statut_protection))

    theta <- coef(objLM)

    ## Matrice de tests :
    Dspat <- matrix(0, nrow=nblev1, ncol=length(theta))
    Dspat[ , 1:nblev1] <- 0             # on ne calcule pas de différences entre années
    Dspat[ , nblev1+1] <- 1             # effet RE (effet HR=0 donc pas ds theta
    Dspat[-1, (nblev1+2):length(theta)] <- diag(1, nblev1-1)

    ## Pour un affichage lisible :
    row.names(Dspat) <- paste(levels(data$an), paste(rev(levels(data$statut_protection)), collapse="-"), sep=":")
    colnames(Dspat) <- names(theta)

    TestDspat <- glht(objLM,linfct=Dspat,alternative="two.sided")
    return(summary(TestDspat))
}


compMultAn.tmp.f <- function(objLM, data)
{
    ## Purpose:
    ## ----------------------------------------------------------------------
    ## Arguments:
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date:  1 sept. 2010, 15:35

    nblev1 <- length(levels(data$an))
    nblev2 <- length(levels(data$statut_protection))

    theta <- coef(objLM)

    ## calcul des différences temporelles pour un statut donné
    Ttemp <- matrix(0, nrow=nblev1* 2, ncol=length(theta))

    ans <- levels(data$an)
    diffAns <- paste(ans[c(length(ans), length(ans):2)], ans[c(1, (length(ans) - 1):1)], sep="-")

    row.names(Ttemp) <- paste(rep(levels(data$statut_protection), each=nblev1), diffAns, sep=":")
    colnames(Ttemp) <- names(theta)

    ## 1ère zone :
    Ttemp[1, ] <- c(rep(0, nblev1-1), 1, 0, 0, 0, 0, 0)
    Ttemp[2, ] <- c(rep(0,  nblev1-2), -1, 1, 0, 0, 0, 0, 0)
    Ttemp[3, ] <- c(rep(0,  nblev1-3), -1, 1, 0, 0, 0, 0, 0, 0)
    Ttemp[4, ] <- c(rep(0,  nblev1-4), -1, 1, 0, 0, 0, 0, 0, 0, 0)
    Ttemp[5, ] <- c(rep(0,  nblev1-4), 1, 0, 0, 0, 0, 1, 0, 0, 0)
    ## 2nd zone :
    Ttemp[6, ] <- c(rep(0,  nblev1-1), 1, 1, 0, 0, 0, 1)
    Ttemp[7, ] <- c(rep(0,  nblev1-2), -1, 1, 1, 0, 0, -1, 1)
    Ttemp[8, ] <- c(rep(0,  nblev1-3), -1, 1, 0, 1, 0, -1, 1, 0)
    Ttemp[9, ] <- c(rep(0,  nblev1-4), -1, 1, 0, 0, 1, -1, 1, 0, 0)
    Ttemp[10, ] <- c(rep(0,  nblev1-4), 1, 0, 0, 0, 1, 1, 0, 0, 0)

    TestTtemp <- glht(objLM, linfct=Ttemp, alternative="two.sided")
    return(summary(TestTtemp))
}


### Local Variables:
### ispell-local-dictionary: "english"
### fill-column: 120
### End:
