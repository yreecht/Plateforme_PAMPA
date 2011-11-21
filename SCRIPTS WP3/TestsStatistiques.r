################################################################################
# Nom               : TestsStatistiques.r
# Type              : Programme
# Objet             : Ce programme comporte toutes les fonctions d'analyses statistiques pour l'étude des métriques WP3
#                     Il contient les analyses de variance (pour les variables qualitatives et quantitatives
# Input             : tableaux importés par le topMenu.r
# Output            : lancement de fonctions
# Auteur            : Elodie Gamp
# R version         : 2.8.1
# Date de création  : octobre 2010
# Sources
################################################################################

niveauSpatial=""
####################################################################################################
### Description:
###
### Fonctions de bases de la plateforme (également utilisé pour définir des fonctions de base
### récentes de R lorsqu'elles n'existent pas, par ex si on travaille sur une version ancienne de R)
####################################################################################################

########################################################################################################################
if (!exists("grepl")) {

    grepl <- function(pattern, x,...) {    
        ## Purpose: Émulation des fonctions de 'grepl' (en moins efficace
        ##          probablement) si la fonction n'existe pas.
        ## ----------------------------------------------------------------------
        ## Arguments: pattern : le motif à rechercher.
        ##            x : le vecteur dans lequel chercher le motif.
        ##            ... : arguments supplémentaires pour 'grep'
        ## ----------------------------------------------------------------------
        ## Author: Yves Reecht, Date:  5 oct. 2010, 14:36

        return(sapply(x,
          function(x2)  {
          ## On teste pour chaque élément s'il contient le motif :
            as.logical(length(grep(as.character(pattern), x2,...)))
          }))
    }
} else {}                                 # Sinon rien à faire

####################################################################################################
### Description:
###
### Interface de comparaison des distributions d'une métrique :
####################################################################################################

iter.gsub <- function(pattern, replacement, x,...) {

    if (length(pattern) > 0)  {
    
        return(gsub(pattern=pattern[1],
            replacement=replacement[1],
            x=iter.gsub(pattern=pattern[-1], replacement=replacement[-1], x=x),
            ...))
    } else {
        return(x)
    }
}
########################################################################################################################

.my.tkdev <- function (hscale = 1, vscale = 1,...) {
    ## Purpose: écraser la définition de .my.tkdev du packages tkrplot pour
    ##          permettre de passer des options supplémentaires au
    ##          périphérique graphique + gestion des différents systèmes
    ##          d'exploitation/versions de R.
    ## ----------------------------------------------------------------------
    ## Arguments: ceux de .my.tkdev original + ...
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 23 août 2010, 12:41

    if (Sys.info()["sysname"] == "Windows") { # Système Windows
    
        if(R.version$major == 2 && R.version$minor < 3)  {
            win.metafile(width=4*hscale, height=4*vscale,...)
        } else {
            win.metafile(width=4*hscale, height=4*vscale, restoreConsole=FALSE,...)
        }
    } else {                              # Systèmes Unix(-like).
        if (exists("X11", env=.GlobalEnv)) {        
            X11("XImage", 480*hscale, 480*vscale,...)
        } else {
            stop("tkrplot only supports Windows and X11")
        }
    }
}
########################################################################################################################

tkrplot <- function(parent, fun, hscale = 1, vscale = 1,...) {
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

Capitalize.f <- function(x, words=FALSE) {
    ## Purpose: Mettre en majuscule la première lettre de chaque mot
    ## ----------------------------------------------------------------------
    ## Arguments: x : une chaîne de caractères
    ##            words : tous les mots (TRUE), ou juste le premier.
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date:  9 août 2010, 21:08

    if (words) {    
        s <- strsplit(x, " ")[[1]]
    } else {
        s <- x
    }
    return(paste(toupper(substring(s, 1,1)), substring(s, 2),
                 sep="", collapse=" "))
}
########################################################################################################################

selRowCoefmat <- function(coefsMat, anovaLM, objLM) {
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

    if (!is.null(anovaLM))  {
    
        ## Facteurs et intéractions dont les coefs doivent être imprimés :
        selectedFactInt <- attr(anovaLM, "row.names")[which(anovaLM[[grep("P[r(]", attr(anovaLM, "names"))]] < 0.05)]

        ## Tous les facteurs :
        facts <- attr(anovaLM, "row.names")[!is.na(anovaLM[[grep("P[r(]", attr(anovaLM, "names"))]])]

        ## indices des intéractions dans "selectedFactInt" :
        has.interactions <- TRUE
        interactions <- grep(":", facts, fixed=TRUE)
        if (length(interactions) == 0)  {        
            interactions <- length(facts) + 1
            has.interactions <- FALSE
        } else {}

        ## Coefficients par facteur (sans compter les intéractions) :
        if (length(facts) == 1) {        
            factsRows <- list(grep(paste("^", facts, sep=""), row.names(coefsMat), value=TRUE))
            names(factsRows) <- facts
        } else {
            factsRows <- 
                sapply(facts[-c(interactions)],
                       function(fact) {                   
                          selRows <- grep(paste("^", fact, sep=""),
                                       row.names(coefsMat), value=TRUE)
                          return(selRows[! grepl(":",
                                              selRows,
                                              fixed=TRUE)])
                      }, simplify=FALSE)## , as.vector)
        }

        ## type de coef (facteur et intéractions) par ligne de la matrice de coef :
        rows <- c("(Intercept)",
                  ## facteurs :
                  unlist(sapply(1:length(factsRows),
                        function(i) {
                            rep(names(factsRows)[i], length(factsRows[[i]]))
                        })))

        ## intéractions :
        if (has.interactions) {        
            ## nombre de répétitions par type d'intéraction :

            ## liste des nombres de modalités pour chaque facteur d'une intéraction :
            nmod <- sapply(strsplit(facts[interactions], ":"),
                           function(fa) sapply(fa, function(i)length(factsRows[[i]])))

            if (!is.list(nmod)) {        # corrige un bug lorsqu'uniquement 1 type d'intéraction).
                nmod <- list(as.vector(nmod))
            } else {}

            ## Nombres de répétitions :
            nrep <- sapply(nmod, prod)

            ## Ajout des types d'intéractions :
            rows <- c(rows,
                      unlist(sapply(1:length(nrep), function(i)
                                {
                                    rep(facts[interactions][i], nrep[i])
                                })))
        } else {}

        ## Lignes conservées :
        return(is.element(rows, c("(Intercept)", selectedFactInt))[!is.na(objLM$coefficients)])
    } else {
        return(rep(TRUE, nrow(coefsMat)))
    }
}
########################################################################################################################

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
                             ...)  {
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
################################################################################

plotDist.f <- function(y, family, metrique, env=NULL,...)   {
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
    if (family == "LOGNO" & sum(y == 0, na.rm=TRUE)) {    
        y <- y + ((min(y, na.rm=TRUE) + 1) / 1000)
    } else {}

    ## abscisses pour la distribution théorique.
    if (is.element(family, c("PO", "NBI"))) {
        xi <- seq(from=min(y, na.rm=TRUE), to=max(y, na.rm=TRUE))
    } else {
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
         xlab=Capitalize.f(metrique),
         ylab="Densité de la métrique",
         col="lightgray")

    lines(xi, yi, lwd=2, col="red")     # courbe (distribution théorique).

    ## Calcul d'AIC (entre autres) :
    FA <- as.gamlss.family(family)      # On procède comme dans la fonction histDist.
    fname <- FA$family[1]

    res <- gamlss(y ~ 1, family=fname)

    ## Si un environnement est précisé, la valeur est sauvegardée dans une liste 'distList' :
    if (!is.null(env)) {
        eval(substitute(evalq(distList[[family]] <- res, envir=env), list(family=eval(family), res=eval(res))))
    } else {}

    ## Retourne le résultat :
    return(res)
}
########################################################################################################################

print.anova.fr <- function(x, digits = max(getOption("digits") - 2, 3), signif.stars = getOption("show.signif.stars"),
                           ...) {
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
    if (!is.null(heading <- attr(x, "heading"))) {
            cat(heading, sep = "\n")
    } else {}

    nc <- dim(x)[2L]
    if (is.null(cn <- colnames(x))) {    
        stop("'anova' object must have colnames")
    } else {}
    has.P <- grepl("^(P|Pr)\\(", cn[nc])
    zap.i <- 1L:(if (has.P)
             {
                 nc - 1
             } else {
                 nc
             })
    i <- which(substr(cn, 2, 7) == " value")
    i <- c(i, which(!is.na(match(cn, c("F", "Cp", "Chisq")))))
    if (length(i)) {    
        zap.i <- zap.i[!(zap.i %in% i)]
    } else {}

    tst.i <- i
    if (length(i <- grep("Df$", cn))) {   
        zap.i <- zap.i[!(zap.i %in% i)]
    } else {}

    printCoefmat(x, digits = digits, signif.stars = signif.stars,
                 signif.legend=FALSE,
                 has.Pvalue = has.P, P.values = has.P, cs.ind = NULL,
                 zap.ind = zap.i, tst.ind = tst.i, na.print = "", ...)
    invisible(x)
}

########################################################################################################################


############               LES ANALYSES DE VARIANCE                 ############


###                    Pour les variables quantitatives                      ###
sum_na.f=function(x){               #Cette fonction réalise la somme et renvoit NA s'il n'y a que des NA ou que la longeur du vecteur est 0.
if(sum(!is.na(x))==0){              #rappel TRUE==1 et FALSE==0
  return(NA)
  } else {
  return(sum(x,na.rm=T))
  }
}
########################################################################################################################
subsetToutesTables.f <- function(metrique, facteurs,tableMetrique="") {
    ## Purpose: Former le tableau de données nécessaires pour les analyses de variance, 
    ##          d'après les métriques et facteur(s) séléctionnés
    ## ----------------------------------------------------------------------
    ## Arguments: metrique : la métrique choisie.
    ##            facteurs : les facteurs sélectionnés (tous)
    ##            tableMetrique : le nom de la table des métriques (freqtot ou captures)
    ## ----------------------------------------------------------------------
    ## Author: Elodie Gamp, Date:  1 octobre 2010

#  metrique = "nb_bat"
#  facteurs = c("num_sortie","periodEchant","zone")
#  listFact = c("periodEchant","zone")
#  tableMetrique = "freqtot"         # ne contient que le nom du tableau considéré

  # metrique = "nb"
  # facteurs = c("quest","periodEchant","zone")
  # tableMetrique = "captures"         # ne contient que le nom du tableau considéré
  
  dataMetrique <- eval(parse(text=tableMetrique))      # donc dataMetrique = freqtot ou captures

  ## Subset en fonction de la table de métrique
  switch(tableMetrique,                                    # permet de faire des traitements différents selon le nom du tableau considéré
      ## Cas de la table de fréquentation :
      freqtot = {
#          liste <- parse(text=paste("list(",tableMetrique,"[,'", paste(facteurs, collapse=",freqtot[,'","']",sep=""),")",sep=""))  
          liste <- parse(text=paste("list(",tableMetrique,"[,'", paste(facteurs, collapse=",freqtot[,'","',drop=TRUE]",sep=""),")",sep=""))  
#          liste <- parse(text=paste("list( factor(",tableMetrique,"[,'", paste(facteurs, collapse=",factor(freqtot[,'","'])",sep=""),")",sep=""))  
        tmp <- aggregate(freqtot[,metrique], eval(liste), FUN=sum_na.f)                
          nom <- parse(text=paste("c('",paste(facteurs, collapse="','",sep=""),"','",metrique,"')",sep=""))
          names(tmp)= eval(nom)
      },
      ## Cas de la table de fréquentation 2 :
      freqtot2 = {
          liste <- parse(text=paste("list(",tableMetrique,"[,'", paste(facteurs, collapse=",freqtot2[,'","']",sep=""),")",sep=""))  
        tmp <- aggregate(freqtot2[,metrique], eval(liste), FUN=sum_na.f)                
          nom <- parse(text=paste("c('",paste(facteurs, collapse="','",sep=""),"','",metrique,"')",sep=""))
          names(tmp)= eval(nom)
      },
      ## Cas de la table des captures :
      captures = {
          liste <- parse(text=paste("list(",tableMetrique,"[,'", paste(facteurs, collapse=",captures[,'","']",sep=""),")",sep=""))  
        tmp <- aggregate(captures[,metrique], eval(liste), FUN=sum_na.f)                
          nom <- parse(text=paste("c('",paste(facteurs, collapse="','",sep=""),"','",metrique,"')",sep=""))
          names(tmp)= eval(nom)
      },
      ## Cas de la table des captures avec CPUE :
      captures2 = {
          liste <- parse(text=paste("list(",tableMetrique,"[,'", paste(facteurs, collapse=",captures2[,'","']",sep=""),")",sep=""))  
        tmp <- aggregate(captures2[,metrique], eval(liste), FUN=sum_na.f)                
          nom <- parse(text=paste("c('",paste(facteurs, collapse="','",sep=""),"','",metrique,"')",sep=""))
          names(tmp)= eval(nom)
      },
      ### cas de la table pêche ou tousQuest
      peche = {
        tmp=peche
      },
      pecheQ = {
        tmp=pecheQ
      },
      tousQuest = {
        tmp=tousQuest
      },
      ## Autres cas :
        tkmessageBox(message="Erreur dans la sélection de la table",icon="info")
    )
    ### transformation des valeurs numériques en integer si valeurs entières
      aa<-round(tmp[,metrique])
      a<-tmp[,metrique]
      if (all(na.omit(a==aa))){tmp[,metrique]<-as.integer(tmp[,metrique])}     
      tmp<-tmp[!is.na(tmp[,metrique]),]
    ## transformation des variables explicatives en facteur
    for (i in 1 : length(facteurs)){
      tmp[,facteurs[i]]<-as.factor(tmp[,facteurs[i]])
    }
    
    ## Conversion de la fréquentation ou des captures -> /ha :
    if (is.element(niveauSpatial,facteurs)==TRUE) {
      if (titreSurface != "") {    
        transfoSurface <- surface[match(tmp[,niveauSpatial],names(surface))]
        tmp[, metrique] <- tmp[, metrique] / transfoSurface
      }
    } else {}

    return(tmp)
}
################################################################################

modeleLineaireWP3.f <- function(metrique, facteurs, listFact, tableMetrique,sufixe=NULL) {
    ## Purpose: Gestions des différentes étapes des modèles linéaires.
    ## ----------------------------------------------------------------------
    ## Arguments: metrique : la métrique choisie.
    ##            listFact : liste du (des) facteur(s) de regroupement / facteurs explicatifs à tester
    ##            facteurs : liste des facteurs à tester (listFact) + num_sortie (pour les réplicats des combinaisons de facteurs possibles)
    ##            listFactSel : liste des modalités sélectionnées pour ce(s)
    ##                          dernier(s)
    ##            tableMetrique : nom de la table de métriques.
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 18 août 2010, 15:59

    facteurs <- facteurs
    listFact<-listFact
    listFactOriginal<-listFact
    aGarder=NULL
    tableTest<-eval(parse(text=tableMetrique))
    for (i in 1 : length(listFact)) {
      if(length(levels(tableTest[,listFact[i]])) > 1 ) {aGarder=c(aGarder,i)}
    }
    listFact<-listFact[aGarder]
    if (length(listFact)==0) {
      print(paste("Il est impossible de tester les facteurs",listFactOriginal,"sur la métrique",metrique))
    } else {
      ## Données pour la série d'analyses :
      tmpData <- subsetToutesTables.f(metrique=metrique, facteurs=facteurs, tableMetrique=tableMetrique)
  
      ## Formules pour différents modèles (avec ou sans transformation log) :
      exprML <- eval(parse(text=paste(metrique, "~", paste(listFact, collapse=" * "))))
      logExprML <- eval(parse(text=paste("log(", metrique, ") ~", paste(listFact, collapse=" * "))))
  
      ## Sauvegarde temporaire des données utilisées pour les analyses (attention : écrasée à chaque nouvelle série de
      ## graphiques) :
      DataBackup <<- list()
      tmpDataMod <- tmpData  # sauvegarde
  
  
      ## Aide au choix du type d'analyse :
      loiChoisie <- choixDistri.f(metrique=metrique, data=tmpDataMod[ , metrique, drop=FALSE])
  
          if (!is.null(loiChoisie)) {          
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
                              
                  sortiesLM.f(objLM=res, formule=formule, metrique=metrique,
                          listFact=listFact,Data=tmpDataMod, Log=Log,sufixe=sufixe)
  
              resid.out <- boxplot(residuals(res), plot=FALSE)$out
  
          } else {
              message("Annulé !")
          }
      }
}
################################################################################

########################################################################################################################
choixDistri.f <- function(metrique, data) {
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
    vscale <- 0.6                      # dimension verticale des graphiques.
    hscale <- 1.05                       # dimension horizontale des graphiques.
    pointsize <- 10                     # taille du point pour les graphiques
    distList <- list()                  # liste pour le stockage des AIC et autres.


    ## ##################################################
    ## Éléments graphiques :
    WinDistri <- tktoplevel()           # Fenêtre principale.
    tkwm.title(WinDistri, paste("Choix de distribution théorique de la métrique '", metrique, "'", sep=""))

    tmp <- tktoplevel(WinDistri)
    tkfocus(tmp)
    tkdestroy(tmp)
    ## tkfocus(WinDistri)

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
    } else {}

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
    if (is.integer(data[ , metrique])) {
    
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
    } else {}

    ## Boutons :
    tkgrid(FrameB, column=1, columnspan=3)
    tkgrid(B.OK, tklabel(FrameB, text="                         "), B.Cancel)
    tkgrid(tklabel(WinDistri, text=" "))

    ## ##################################################
    ## Autres évènements :
    tkbind(WinDistri, "<Destroy>", function(){tclvalue(Done) <- "2"}) # en cas de destruction de la fenêtre.

    ## Présélection de la distribution avec le plus petit AIC :
    tclvalue(LoiChoisie) <- names(distList)[which.min(sapply(distList, function(x){x$aic}))]
    ## flush.console()

    tkwait.variable(Done)               # Attente d'une action de l'utilisateur.

    if (tclvalue(Done) == "1") {    
        return(tclvalue(LoiChoisie))
    } else {
        return(NULL)
    }

}

########################################################################################################################
resFileLM.f <- function(objLM, metrique, listFact, Log=FALSE,  prefix=NULL, ext="txt", sufixe=NULL) {
    ## Purpose: Définit les noms du fichiers pour les résultats des modèles
    ##          linéaires. L'extension et un prefixe peuvent êtres précisés,
    ##          mais par défaut, c'est le fichier de sorties texte qui est
    ##          créé.
    ## ----------------------------------------------------------------------
    ## Arguments: objLM : un objet de classe 'lm' ou 'glm'.
    ##            metrique : nom de la métrique analysée.
    ##            listFact : vecteur des noms de facteurs de l'analyse.
    ##            Log : Est-ce que les données sont log-transformées.
    ##            prefix : préfixe du nom de fichier.
    ##            sufixe : un sufixe pour le nom de fichier.
    ##            ext : extension du fichier.
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date:  8 sept. 2010, 15:48

    ## si pas de préfix fourni :
    if (is.null(prefix)){
        prefix <- ifelse(length(grep("^lm\\(", deparse(objLM$call), perl=TRUE)) > 0,
                         paste("LM", ifelse(Log, "-log", ""), sep=""),
                         ifelse(length(grep("^glm\\.nb", deparse(objLM$call), perl=TRUE)) > 0,
                                "GLM-NB",
                                ifelse(length(grep("^glm.*poisson", deparse(objLM$call), perl=TRUE)) > 0,
                                       "GLM-P",
                                       "Unknown-model")))
    } else {}

    ## Nom de fichier :
    filename <- paste("C:/PAMPA/resultats script R/metriquesWP3/stats/", prefix, "_",
                      ## Métrique analysée :
                      metrique, "_",
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
                    c("pdf", "PDF", "png", "PNG", "jpg", "JPG"))) {
        return(resFile <- file(filename, open="w"))
    } else {
        return(filename)
    }
}

########################################################################################################################
valPreditesLM.f <- function(objLM, Data, listFact, resFile){
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

    if (!is.matrix(OrdreNivFact)) {      # Si un seul facteur, on transforme le vecteur d'ordre des niveaux en matrice.
        OrdreNivFact <- matrix(OrdreNivFact, ncol=1, dimnames=list(NULL, listFact))
    } else {}

    ## Valeurs prédites pour chaque combinaison réalisée des facteurs :
    if (length(grep("^glm", objLM$call)) > 0) {    
        valPredites <- predict(objLM, newdata=unique(Data[ , listFact, drop=FALSE]), type="response")
    } else {
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
infoStatLM.f <- function(objLM, resFile) {

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
    if (length(grep("^glm", objLM$call)) == 0) {
    
        cat("Statistique de Fisher Globale et R^2 :\n\n", file=resFile)
        cat("\tR^2 multiple : ", format(sumLM$r.squared, digits=3),
            " ;\tR^2 ajusté : ", format(sumLM$adj.r.squared, digits=3), "\n", file=resFile, sep="")

        cat("\tF-statistique : ",
            paste(sapply(sumLM$fstatistic, format, digits=4, nsmall=0),
                  c(" sur ", " et ", " DL,"), sep=""),
            "\tP-valeur : ",
            format.pval(pf(sumLM$fstatistic[1L], sumLM$fstatistic[2L], sumLM$fstatistic[3L], lower.tail = FALSE), digits=4),
            "\n\n\n", file=resFile, sep="")
    } else { }
}


########################################################################################################################
signifParamLM.f <- function(objLM, resFile)  {
    ## Purpose: Écrire les résultats de l'anova globale du modèle et
    ##          l'estimation de significativités des coefficients du modèle.
    ## ----------------------------------------------------------------------
    ## Arguments: objLM un objet de classe 'lm' ou 'glm'.
    ##            resFile : une connection pour les sorties.
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date:  8 sept. 2010, 17:07

    ## Anovas et résumés :
    if (length(grep("^glm", objLM$call)) > 0) {# Pour les GLMs.    
        anovaLM <- anova(objLM, test="Chisq") # Pour les LMs.
    } else {
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
sortiesLM.f <- function(objLM, formule, metrique, listFact, Data, Log=FALSE, sufixe=NULL) {
    ## Purpose: Formater les résultats de lm et les écrire dans un fichier
    ## ----------------------------------------------------------------------
    ## Arguments: objLM : un objet de classe lm
    ##            formule : la formule utilisée (pas lisible dans le call).
    ##            metrique : la métrique choisie.
    ##            listFact : liste du (des) facteur(s) de regroupement.
    ##            Data : les données utilisées.
    ##            Log : données log-transformées ou non (booléen).
    ##            sufixe : un sufixe pour le nom de fichier.
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
    resFile <- resFileLM.f(objLM=objLM, metrique=metrique, listFact=listFact,
                           Log=Log, sufixe=sufixe)
    on.exit(close(resFile), add=TRUE)


    ## Informations et statistiques globales sur le modèle :
    infoStatLM.f(objLM=objLM, resFile=resFile)


    ## Anova globale du modèle + significativité des coefficients :
    signifParamLM.f(objLM=objLM, resFile=resFile)


    ## ##################################################
    ## Valeurs prédites par le modèle :
    valPreditesLM.f(objLM=objLM, Data=Data, listFact=listFact, resFile=resFile)

    ## ##################################################
    # interaction plot si deux facteurs explicatifs
    if (length(listFact)==2){
         x11(width=50,height=30,pointsize=10)
#          par(mar=c(7, 6, 6, 2), mgp=c(4.5, 0.5, 0))

      cols <- nrow(unique(Data[listFact[2]]))
      if (Log) {                  # Les sens de variations peuvent changer en log (sur les moyennes) =>
                                        # besoin d'un graphique adapté :             
             with(Data, {
                 eval(parse(text=paste("interaction.plot(", listFact[1], ", ", listFact[2],", log(", metrique, "), ylab=\"",
                                       paste("log(", metrique, ") moyen", sep=""), "\",col=1:cols)", sep="")))
              })                         

              } else {
              
             with(Data, {
                 eval(parse(text=paste("interaction.plot(", listFact[1], ", ", listFact[2],", ", metrique, ", ylab=\"",
                                       paste("nombre(", metrique, ") moyen", sep=""), "\",col=1:cols)", sep="")))
              })
              mtext("Interaction plot", side=3, cex=2) 
             }
    }     

    ## ##################################################
   # X11(width=45, height=35)
   # par(mfrow=c(2, 2))
   # hist(objLM$residuals, xlab="valeur des résidus ", ylab= "Fréquence ", main=NULL)
   # mtext("Distribution des résidus", side=3, cex=0.8)

#    plot.lm.fr(objLM, which=2, cex.caption=0.8)
#    plot.lm.fr(objLM, which=c(1, 4), cex.caption=0.8)

    ## flush.console()
}

########################################################################################################################

###                Pour les variables qualitatives binomiales                ###

########################################################################################################################
resFileGLM.f <- function(metrique, listFact, prefix=NULL) {
    ## Purpose: Définit les noms du fichiers pour les résultats des modèles
    ##          linéaires gébéralisés. L'extension et un prefixe peuvent êtres précisés,
    ##          mais par défaut, c'est le fichier de sorties texte qui est
    ##          créé.
    ## ----------------------------------------------------------------------
    ## Arguments: 
    ##            metrique : nom de la métrique analysée.
    ##            listFact : vecteur des noms de facteurs de l'analyse.
    ##            prefix : préfixe du nom de fichier (binom ou multinom)
    ## ----------------------------------------------------------------------
    ## Author: Elodie Gamp, Date:  6 octobre 2010

    ## Nom de fichier :
    filename <- paste("C:/PAMPA/resultats script R/metriquesWP3/stats/", prefix, "_",
                      ## Métrique analysée :
                      metrique, "_",
                      ## liste des facteurs de l'analyse
                      paste(listFact, collapse="-"),
                      ## Extension du fichier :
                      ".txt", sep="")

    ## Ouverture de la connection (retourne l'objet de type 'connection'
        return(resFile <- file(filename, open="w"))
}
###############################################################################

sortiesBINOM.f <- function(metrique, listFact, Data, prefix="binom") {
    ## Purpose: Formater les résultats de glm binomial et les écrire dans un fichier
    ## ----------------------------------------------------------------------
    ## Arguments: 
    ##            metrique : la métrique choisie.
    ##            listFact : liste du (des) facteur(s) de regroupement.
    ##            Data : les données utilisées.
    ##            prefix : préfixe du nom de fichier (binom ou multinom)
    ## ----------------------------------------------------------------------
    ## Author: Elodie Gamp, Date: 6 octobre 2010

    ## longueur des lignes pour les sorties textes :
    oOpt <- options()
    on.exit(options(oOpt))

    options(width=120)

    ## Chemin et nom de fichier :
    resFile <- resFileGLM.f(metrique=metrique, listFact=listFact,
                           prefix=prefix)
    on.exit(close(resFile), add=TRUE)

    modelBIqualiWP3.f (metrique=metrique, listFact=listFact, Data=Data, resFile=resFile)

}
########################################################################################################################
# Gestions des différentes étapes des modèles linéaires généralisés binomiaux pour les métriques WP3
modelBIqualiWP3.f <- function(metrique=metrique, listFact=listFact, Data=Data, resFile=resFile) {
  
  ## transformation des données (suppression NA et variables en facteur)
    Data<-Data[!is.na(Data[,metrique]),]
    listFactOriginal<-listFact
    aGarder=NULL
    for (i in 1 : length(listFact)) {
      Data[,listFact[i]]<-as.factor(Data[,listFact[i]])
      if(length(levels(Data[,listFact[i]])) != 1 || 0) {aGarder=c(aGarder,i)}
    }
    listFact<-listFact[aGarder]
    if (length(listFact)==0) {
      cat(paste("Il est impossible de tester les facteurs",listFactOriginal,"sur la métrique",metrique), file=resFile, fill=1)
    } else {
    
    ## définition de la formule et du modèle binomial
      formule <- eval(parse(text=paste(metrique, "~", paste(listFact, collapse=" * "))))
      glmtest <- glm(formule, data=Data, family="binomial")
      
      ## Formule de modèle lisible:
        glmtest$call$formula <- formule
        formule <<- formule
        sumLM <- summary.glm(glmtest)
      
    ## Enregistrement des informations sur le modèle :
      cat("---------------------------------------------------------------------------\n", file=resFile)
      cat("Modèle ajusté :", file=resFile, fill=1)
      cat("\t", deparse(glmtest$call), "\n\n", file=resFile, sep="")
  
    ## Anova globale du modèle
      anovaglm <- anova(glmtest,test="Chisq") 
      capture.output(print.anova.fr(anovaglm), file=resFile)   
      
    ## Significativités des paramètres :
      cat("\n\n---------------------------------------------------------------------------\n", file=resFile)
      cat("Significativités des paramètres ",
          "\n(seuls ceux correspondant à des facteurs/intéractions significatifs sont représentés) :\n\n",
          file=resFile)
  
      capture.output(printCoefmat.red(sumLM$coef, anovaLM=anovaglm, objLM=glmtest), file=resFile)
  
   # windows()                     # pour avoir les sorties du modèle
   # plot(Data[,metrique][!is.na(Data[,metrique])],fitted(glmtest),main="Proportions estimées par le modèle")        # graph des valeurs estimées par la modèle
  }
}


########################################################################################################################

###              Pour les variables qualitatives multinomiales               ###

########################################################################################################################
resFileMULT.f <- function(metrique, listFact, prefix=NULL){
    ## Purpose: Définit les noms du fichiers pour les résultats des modèles
    ##          multinomiaux. L'extension et un prefixe peuvent êtres précisés,
    ##          mais par défaut, c'est le fichier de sorties texte qui est
    ##          créé.
    ## ----------------------------------------------------------------------
    ## Arguments: 
    ##            metrique : nom de la métrique analysée.
    ##            listFact : vecteur des noms de facteurs de l'analyse.
    ##            prefix : préfixe du nom de fichier (binom ou multinom)
    ## ----------------------------------------------------------------------
    ## Author: Elodie Gamp, Date:  6 octobre 2010

    ## Nom de fichier :
    filename <- paste("C:/PAMPA/resultats script R/metriquesWP3/stats/", prefix, "_",
                      ## Métrique analysée :
                      metrique, "_",
                      ## liste des facteurs de l'analyse
                      paste(listFact, collapse="-"),
                      ## Extension du fichier :
                      ".txt", sep="")

    ## Ouverture de la connection (retourne l'objet de type 'connection'
        return(resFile <- file(filename, open="w"))
}

###############################################################################
sortiesMULTI.f <- function(metrique, listFact, Data, prefix="multinom"){
    ## Purpose: Formater les résultats de lm et les écrire dans un fichier
    ## ----------------------------------------------------------------------
    ## Arguments: 
    ##            metrique : la métrique choisie.
    ##            listFact : liste du (des) facteur(s) de regroupement.
    ##            Data : les données utilisées.
    ##            prefix : préfixe du nom de fichier (binom ou multinom)
    ## ----------------------------------------------------------------------
    ## Author: Elodie Gamp, Date: 6 octobre 2010

    ## longueur des lignes pour les sorties textes :
    oOpt <- options()
    on.exit(options(oOpt))

    options(width=120)

    ## Chemin et nom de fichier :
    resFile <- resFileMULT.f(metrique=metrique, listFact=listFact,
                           prefix=prefix)
    on.exit(close(resFile), add=TRUE)

    modelMULTIqualiWP3.f (metrique=metrique, listFact=listFact, Data=Data, resFile=resFile)

}

################################################################################
# Gestions des différentes étapes des modèles linéaires généralisés multinomiaux pour les métriques WP3
modelMULTIqualiWP3.f <- function(metrique, listFact, Data, resFile=resFile){

    Data<-Data[!is.na(Data[,metrique]),]
    listFactOriginal<-listFact
    aGarder=NULL
    for (i in 1 : length(listFact)) {
      Data[,listFact[i]]<-as.factor(Data[,listFact[i]])
      if(length(levels(Data[,listFact[i]])) != 1 || 0) {aGarder=c(aGarder,i)}
    }
    listFact<-listFact[aGarder]
    if (length(listFact)==0) {
      cat(paste("Il est impossible de tester les facteurs",listFactOriginal,"sur la métrique",metrique), file=resFile, fill=1)
    } else { 
  #metrique<-"effetEcosyst"
  #listFact<-c("periodEchant","activite")
  #Data<-tousQuest

  ### formules utilisées pour construire les différents modèles
      exprML0 <- eval(parse(text=paste(metrique, "~", 1)))    
      exprML1 <- eval(parse(text=paste(metrique, "~", listFact[1])))    
      #exprML2 <- eval(parse(text=paste(metrique, "~", listFact[2])))
    if (length(listFact)>1) {    
      exprML3 <- eval(parse(text=paste(metrique, "~", paste(listFact, collapse=" + "))))    
      exprML4 <- eval(parse(text=paste(metrique, "~", paste(listFact, collapse=" * "))))    
    }
       # mod0<-multnom(Y~1)
      multi0<-multinom(exprML0, data=Data)  # modèle nul pour l'effet du premier facteur 
      ## Formule de modèle lisible:
        multi0$call$formula <- exprML0
        exprML0 <<- exprML0
      sumMulti0 <- summary(multi0)
      
       # mod1<-multnom(Y~X)
      multi1<-multinom(exprML1, data=Data)  # modèle avec facteur 1 
      ## Formule de modèle lisible:
        multi1$call$formula <- exprML1
        exprML1 <<- exprML1
      sumMulti1 <- summary(multi1)
    
    if (length(listFact)>1) {        
       # mod2<-multnom(Y~X+Z)  
      multi2<-multinom(exprML3, data=Data)  # modèle avec les deux facteurs sans intéraction
      ## Formule de modèle lisible:
        multi2$call$formula <- exprML3
        exprML3 <<- exprML3
      sumMulti2 <- summary(multi2)
      
       # mod3<-multnom(Y~X*Z)
      multi3<-multinom(exprML4, data=Data)   # modèle avec les deux facteurs et intéraction
      ## Formule de modèle lisible:
        multi3$call$formula <- exprML4
        exprML4 <<- exprML4
      sumMulti3 <- summary(multi3)
    }
        ## Enregistrement des informations sur les modèles :
      cat("---------------------------------------------------------------------------\n", file=resFile)
      cat("Modèles ajustés :", file=resFile, fill=1)
      cat("\t", deparse(sumMulti0$call), "\n\n", file=resFile, sep="")
      cat("\t", deparse(sumMulti1$call), "\n\n", file=resFile, sep="")
    if (length(listFact)>1) {        
      cat("\t", deparse(sumMulti2$call), "\n\n", file=resFile, sep="")
      cat("\t", deparse(sumMulti3$call), "\n\n", file=resFile, sep="")
    }
  
     #les 4 modèles emboités du plus simple au plus complexe pour voir les effets à chaque pas par anova entre 2 modèles  
      anovamulti1 <- anova(multi1,multi0)        # teste l'effet du premier facteur
      cat("\n\n---------------------------------------------------------------------------\n", file=resFile)
      cat(paste("\neffet du premier facteur :",listFact[1],"\n"), file=resFile)
      capture.output(print.anova.fr(anovamulti1), file=resFile)   
    if (length(listFact)>1) {            
      anovamulti2 <- anova(multi2,multi1)        # teste l'effet du deuxième facteur
      cat("\n---------------------------------------------------------------------------\n", file=resFile)
      cat(paste("\neffet du deuxième facteur :",listFact[2],"\n"), file=resFile)   
      capture.output(print.anova.fr(anovamulti2), file=resFile)  
       
      anovamultiInt <- anova(multi3,multi2)        # teste l'effet de l'interaction entre les deux facteurs
      cat("\n---------------------------------------------------------------------------\n", file=resFile)
      cat(paste("\neffet de l'intéraction des facteurs :",listFact[1],listFact[2],"\n"), file=resFile)   
      capture.output(print.anova.fr(anovamultiInt), file=resFile) 
      cat("\n---------------------------------------------------------------------------\n", file=resFile)     
    }
  }
}

########################################################################################################################





