### FONCTIONS UTILITAIRES
###     - Creation de classes de taille : classeTaille.f
###     - Calcul de biomasse : biomasse.f
###     - Calcul de recouvrement : CalculRecouvrement.f
###     - Affichage des graphiques : AffichageGraphiques.f
################################################################################
################################################################################


################################################################################
## Nom    : CalculRecouvrement.f()
## Objet  : calcul des % de recouvrement par groupe d'unités d'observation
## Calcul  : (somme des obs$nb_ind par espece pour chaque unitobs)/
## Input  : tables "obs", "unitobs", "unit" et "unitesp"
## Output : table "RecouvrementParEspece" pour les graphiques
################################################################################

## [sup] [yr:12/01/2011]:

## CalculRecouvrement.f <- function()
## {
##     print("fonction CalculRecouvrement activée")

##     if (unique(unitobs$type) == "LIT")
##     {
##         obsREC <- tapply(obs$nombre, list(obs$unite_observation, obs$code_espece), sum, na.rm=TRUE)
##         obsTOT <- tapply(obs$nombre, obs$unite_observation, sum, na.rm=TRUE)
##         TabRecouvrementEspUnit <- obsREC/(rep(obsTOT, ncol(obsREC)))
##         ajoutMenuBenthos.f(TabRecouvrementEspUnit)
##         return(TabRecouvrementEspUnit)
##     }else{
##         gestionMSGerreur.f("recouvrementsansLIT")
##     }
## }
################################################################################
## Nom    : classeTaille.f
## Objet  : fonction d'attribution des classes de taille à partir de la taille
##          maximale de l'espèce (valeur Fishbase).
##          Les classes sont petit, moyen, grand.
##          La classe juvénile n'est pas prise en compte ici
## Input  : obs$taille
## Output : obs$classe_taille
################################################################################

classeTaille.f <- function()
{
    print("fonction classeTaille activée")

    ## teste si les tailles sont renseignees dans la table observation
    if (length(unique(obs$taille))>1)
    {
        ## !  suggestion de test : format exact = si grand ou moyen ou petit -> sinon, retourne erreur de format

        ## obs$taille=as.numeric(obs$taille)
        obs$classe_taille[obs$taille < (especes$taillemax[match(obs$code_espece, especes$code_espece)]/3)] <- "P"
        obs$classe_taille[obs$taille >= (especes$taillemax[match(obs$code_espece, especes$code_espece)]/3) &
                          obs$taille < (2*(especes$taillemax[match(obs$code_espece, especes$code_espece)]/3))] <- "M"
        obs$classe_taille[obs$taille >= (2*(especes$taillemax[match(obs$code_espece, especes$code_espece)]/3))] <- "G"
        ct <- 1                         # en une ligne plutôt [yr: 27/07/2010]
        assign("ct", ct, envir=.GlobalEnv)

    }else{
        ct <- 2                         # en une ligne plutôt [yr: 27/07/2010]
        assign("ct", ct, envir=.GlobalEnv)
    }
    assign("obs", obs, envir=.GlobalEnv)
}

########################################################################################################################
AjoutTaillesMoyennes.f <- function(data)
{
    ## Purpose: Calcul des tailles comme les moyennes de classes de taille,
    ##          si seules ces dernières sont renseignées.
    ## ----------------------------------------------------------------------
    ## Arguments: data : les données d'observation.
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 26 août 2010, 16:20

    print("fonction AjoutTaillesMoyennes.f activée")

    ## Calcul des tailles comme tailles moyennes à partir des classes de taille si nécessaire :
    if (all(is.na(data$taille)) &
        length(grep("^([[:digit:]]+)[-_]([[:digit:]]+)$", data$classe_taille)) > 1)
    {
        ## Classes de tailles fermées :
        idxCT <- grep("^([[:digit:]]+)[-_]([[:digit:]]+)$", data$classe_taille)
        data$taille[idxCT] <- unlist(sapply(parse(text=sub("^([[:digit:]]+)[-_]([[:digit:]]+)$",
                                                  "mean(c(\\1, \\2), na.rm=TRUE)",
                                                  data$classe_taille[idxCT])),
                                            eval))
        ## Classes de tailles ouvertes vers le bas (borne inférieure <- 0) :
        idxCT <- grep("^[-_]([[:digit:]]+)$", data$classe_taille)
        data$taille[idxCT] <- unlist(sapply(parse(text=sub("^[-_]([[:digit:]]+)$",
                                                  "mean(c(0, \\1), na.rm=TRUE)",
                                                  data$classe_taille[idxCT])),
                                            eval))
        ## Classes de tailles ouvertes vers le haut (pas de borne supérieur, on fait l'hypothèse que la taille est le
        ## minimum de la gamme) :
        idxCT <- grep("^([[:digit:]]+)[-_]$", data$classe_taille)
        data$taille[idxCT] <- unlist(sapply(parse(text=sub("^([[:digit:]]+)[-_]$",
                                                  "mean(c(\\1), na.rm=TRUE)",
                                                  data$classe_taille[idxCT])),
                                            eval))

        ## Avertissement :
        tkmessageBox(message=paste("Attention, les tailles sont des estimations d'après les classes de taille !\n",
                                   "Les calculs de biomasses et tailles moyennes dépendront directement",
                                   " de ces estimations."),
                     icon="warning")
    }else{}

    return(data)
}

################################################################################
## Nom    : biomasse.f
## Objet  : fonction de calcul de la biomasse
##          dans le cas où la taille est renseignée, suivant le site étudié,
##          les coefficients taille-poids utilisés sont MED, MAY ou NC
## Input  : obs$taille, obs$classe_taille
##          especes$poids.moyens, especes$Coeff.a, especes$Coeff.b
## Output : obs$biomasse
################################################################################


########################################################################################################################
poids.moyen.CT.f <- function(Data)
{
    ## Purpose: poids moyens d'après les classes de taille PMG du référentiel
    ##          espèces.
    ## ----------------------------------------------------------------------
    ## Arguments: Data (type obs).
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 11 oct. 2010, 14:58

    refespTmp <- as.matrix(especes[ , c("poids.moyen.petits", "poids.moyen.moyens", "poids.moyen.gros")])
    row.names(refespTmp) <- as.character(especes$code_espece)

    classID <- c("P"=1, "M"=2, "G"=3)

    res <- sapply(seq(length.out=nrow(Data)),
                  function(i)
              {
                  ifelse(## Si l'espèce est dans le référentiel espèce...
                         is.element(Data$code_espece[i], row.names(refespTmp)),
                         ## ...poids moyen correspondant à l'espèce et la classe de taille :
                         refespTmp[as.character(Data$code_espece[i]),
                                   classID[as.character(Data$classe_taille[i])]],
                         ## Sinon rien :
                         NA)
              })

    return(res)
}



########################################################################################################################
biomasse.f <- function()
{
    print("fonction biomasse activée")

    ## obs$biomasse <- as.numeric(rep(NA, nrow(obs)))  # initialement rempli avec des zéros (pas bon du tout) [!!!]  [yr: 13/08/2010]
    ## cas où la taille est renseignée
    ## if (sum(obs$taille, na.rm=TRUE)!=0)  # [!!!] c'est pas du tout bon ça, si non renseigné, ça se rempli de NA tout
                                        # seul  [yr: 13/08/2010]
    ## {


    siteEtudie <- unique(unitobs$AMP)
    ## calcul des biomasses à partir des relations taille-poids W = n*a*L^b
    if (siteEtudie == "BA" || siteEtudie == "BO" || siteEtudie == "CB" || siteEtudie == "CR" || siteEtudie == "STM")
                                        # [!!!] STM provisoire en attendant mieux.
    {
        iesp <- match(obs$code_espece, especes$code_espece)
        obs$biomasse <- obs$nombre * especes$Coeff.a.Med[match(obs$code_espece, especes$code_espece)] *
            obs$taille^especes$Coeff.b.Med[match(obs$code_espece, especes$code_espece)]
    }else{
        if (siteEtudie == "MAY" || siteEtudie == "RUN")
        {
            iesp <- match(obs$code_espece, especes$code_espece)
            obs$biomasse <- obs$nombre * especes$Coeff.a.MAY[match(obs$code_espece, especes$code_espece)] *
                obs$taille^especes$Coeff.b.MAY[match(obs$code_espece, especes$code_espece)]
        }else{
            ## cas NC et STM
            iesp <- match(obs$code_espece, especes$code_espece)
            obs$biomasse <- obs$nombre * especes$Coeff.a.NC[match(obs$code_espece, especes$code_espece)] *
                obs$taille^especes$Coeff.b.NC[match(obs$code_espece, especes$code_espece)]
        }
    }

    ## Poids d'après les classes de taille lorsque la taille n'est pas renseignée :
    tmpBiom <- poids.moyen.CT.f(Data=obs) * obs$nombre
    if (any(is.na(obs$biomasse[!is.na(tmpBiom)])))
    {
        tmpNbTaille <- sum(!is.na(obs$biomasse))
        tmpNbCT <- sum(!is.na(tmpBiom[is.na(obs$biomasse)]))

        obs$biomasse[is.na(obs$biomasse)] <- tmpBiom[is.na(obs$biomasse)]

        ## Avertissement :
        tkmessageBox(message=paste("Attention, certaines biomasses sont des estimations",
                                   " d'après les poids moyens par classes de taille (G, M, P) par espèce !\n\n",
                                   "Nombres d'enregistrements basés sur :",
                                   "\n\t*  la taille : ", tmpNbTaille,
                                   "\n\t*  les poids par classe de taille : ", tmpNbCT,
                                   "\n(sur ", nrow(obs), ")",
                                   sep=""),
                     icon="warning")

    }else{}                             # Rien
    ## }
    assign("obs", obs, envir=.GlobalEnv)
}

########################################################################################################################
calcTaillesMoyennes.f <- function(data)
{
    ## Purpose: Calcul des tailles comme les moyennes de classes de taille,
    ##          si seules ces dernières sont renseignées.
    ## ----------------------------------------------------------------------
    ## Arguments: data : les données d'observation.
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 26 août 2010, 16:20

    print("fonction calcTaillesMoyennes.f activée")

    res <- data$taille
    classes.taille <- data$classe_taille

    ## Calcul des tailles comme tailles moyennes à partir des classes de taille si nécessaire :
    if (any(is.na(res)[grep("^([[:digit:]]*)[-_]([[:digit:]]*)$", classes.taille)]))
    {

        ## Classes de tailles fermées :
        idxCT <- grep("^([[:digit:]]+)[-_]([[:digit:]]+)$", classes.taille) # classes de tailles qui correspondent aux
                                        # motifs pris en compte...
        idxCT <- idxCT[is.na(res[idxCT])] # ...pour lesquels la taille n'est pas renseignée.
        res[idxCT] <- unlist(sapply(parse(text=sub("^([[:digit:]]+)[-_]([[:digit:]]+)$",
                                                  "mean(c(\\1, \\2), na.rm=TRUE)",
                                                  classes.taille[idxCT])),
                                            eval))

        ## Classes de tailles ouvertes vers le bas (borne inférieure <- 0) :
        idxCT <- grep("^[-_]([[:digit:]]+)$", classes.taille)
        idxCT <- idxCT[is.na(res[idxCT])] # ...pour lesquels la taille n'est pas renseignée.
        res[idxCT] <- unlist(sapply(parse(text=sub("^[-_]([[:digit:]]+)$",
                                                  "mean(c(0, \\1), na.rm=TRUE)",
                                                  classes.taille[idxCT])),
                                            eval))

        ## Classes de tailles ouvertes vers le haut (pas de borne supérieur, on fait l'hypothèse que la taille est le
        ## minimum de la gamme) :
        idxCT <- grep("^([[:digit:]]+)[-_]$", classes.taille)
        idxCT <- idxCT[is.na(res[idxCT])] # ...pour lesquels la taille n'est pas renseignée.
        res[idxCT] <- unlist(sapply(parse(text=sub("^([[:digit:]]+)[-_]$",
                                                  "mean(c(\\1), na.rm=TRUE)",
                                                  classes.taille[idxCT])),
                                            eval))
    }else{}

    return(res)
}

########################################################################################################################
bilanCalcPoids.f <- function(x)
{
    ## Purpose: Affiche un pop-up avec un récapitulatif des calculs de poids
    ##          (si nécessaire).
    ## ----------------------------------------------------------------------
    ## Arguments: x : vecteur avec les nombres de poids ajoutés aux
    ##                observations suivant chaque type de calcul.
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date:  7 déc. 2010, 13:56

    tkmessageBox(message=paste("Les poids sont renseignés pour ",
                 sum(x[-1]), "/", x["total"], " observations et se répartissent comme suit :",
                 "\n\t*  ", x["obs"], " poids observés.",
                 "\n\t*  ", x["taille"], " calculs d'après la taille observée.",
                 ifelse(any(x[-(1:3)] > 0),
                        paste("\n\t*  ", x["taille.moy"],
                              " calculs d'après des tailles estimées sur la base des classes de tailles.",
                              ifelse(!is.na(x["poids.moy"]),
                                     paste("\n\t*  ", x["poids.moy"], " poids moyens par classe de taille (P, M, G), ",
                                           " par espèce.", sep=""),
                                     ""),
                              sep=""),
                        ""),
                 "\n\nLes estimations de biomasses dépendent de ces poids.",
                 sep=""),
                 icon=ifelse(any(x[-(1:3)] > 0), "warning", "info"))
}


########################################################################################################################
calcPoids.f <- function(Data)
{
    ## Purpose: Calculs des poids (manquants) pour un fichier de type
    ##          "observation", avec par ordre de priorité :
    ##           1) conservation des poids observés.
    ##           2) calcul des poids avec les relations taille-poids.
    ##           3) idem avec les tailles d'après les classes de tailles.
    ##           4) poids moyens d'après les classes de tailles (G, M, P).
    ##           5) rien sinon (NAs).
    ##
    ##           Afficher les un bilan si nécessaire.
    ## ----------------------------------------------------------------------
    ## Arguments: Data : jeu de données ; doit contenir les colonnes
    ##                   "code_espece", "unite_observation", "taille",
    ##                   "poids" et éventuellement "classe_taille".
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date:  6 déc. 2010, 11:57

    ## Identification des différents cas :
    casSite <- c("BA"="Med", "BO"="Med", "CB"="Med", "CR"="Med", "STM"="Med", # [!!!] STM provisoire en attendant mieux.
                 "MAY"="MAY", "RUN"="MAY")

    ## Poids observés :
    res <- Data[ , "poids"]

    ## Nombre d'obs totales, nombre de poids obs et nombre de tailles obs (sans poids obs) :
    nbObsType <- c("total"=length(res), "obs"=sum(!is.na(res))) # , "taille"=sum(!is.na(Data[is.na(res) , "taille"])))

    ## indices des tailles renseignées, pour lesquelles il n'y a pas de poids renseigné :
    idxTaille <- !is.na(Data$taille) & is.na(res)

    ## ajout des tailles moyennes d'après la classe de taille :
    Data[ , "taille"] <- calcTaillesMoyennes.f(Data)

    ## indices des tailles ajoutées par cette méthode, pour lesquelles il n'y a pas de poids renseigné :
    idxTailleMoy <- !is.na(Data$taille) & ! idxTaille & is.na(res)

    ## Calcul des poids à partir des relations taille-poids W = n*a*L^b :
    idxP <- is.na(res)                  # indices des poids à calculer.

    switch(casSite[unique(as.character(unitobs$AMP))],
           ## Méditerrannée :
           "Med"={
               res[idxP] <- (Data$nombre * especes$Coeff.a.Med[match(Data$code_espece, especes$code_espece)] *
                             Data$taille ^ especes$Coeff.b.Med[match(Data$code_espece, especes$code_espece)])[idxP]
           },
           ## Certains sites outre-mer :
           "MAY"={
               res[idxP] <- (Data$nombre * especes$Coeff.a.MAY[match(Data$code_espece, especes$code_espece)] *
                             Data$taille ^ especes$Coeff.b.MAY[match(Data$code_espece, especes$code_espece)])[idxP]
           },
           ## Autres (NC,...) :
           res[idxP] <- (Data$nombre * especes$Coeff.a.NC[match(Data$code_espece, especes$code_espece)] *
                         Data$taille ^ especes$Coeff.b.NC[match(Data$code_espece, especes$code_espece)])[idxP]
           )
    ## [!!!] Comptabiliser les tailles incalculables !
    ## Nombre de poids ajoutées à grâce méthode :
    nbObsType[c("taille", "taille.moy")] <- c(sum(!is.na(res[idxTaille])), sum(!is.na(res[idxTailleMoy])))

    if (isTRUE(casSite[unique(as.character(unitobs$AMP))][1] == "Med"))
    {
        ## Poids d'après les classes de taille lorsque la taille n'est pas renseignée :
        tmpNb <- sum(!is.na(res))           # nombre de poids disponibles avant.

        res[is.na(res)] <- (poids.moyen.CT.f(Data=Data) * Data$nombre)[is.na(res)]

        nbObsType["poids.moy"] <- sum(!is.na(res)) - tmpNb # nombre de poids ajoutés.
    }

    ## Récapitulatif :
    bilanCalcPoids.f(x=nbObsType)

    ## Stockage et retour des données :
    Data[ , "poids"] <- res
    return(Data)
}


################################################################################
## Nom    : grp1.f()
## Objet  : calcul des metriques par groupe d'unites d'observation
## Input  : tables "obs", "unitobs", "unit" et "unitesp" + 1 facteur x
## Output : table "grp" pour les graphiques
################################################################################

grp1.f <- function (x)
{
    print("fonction grp1.f activée")
    ## somme des abondances
    obs[, x] <- unitobs[, x][match(obs$unite_observation, unitobs$unite_observation)]
    grpi <- tapply(obs$nombre, obs[, x], sum, na.rm=TRUE)
    grp <- as.data.frame(matrix(NA, dim(grpi)[1], 2))
    colnames(grp) = c(x, "nombre")
    grp$nombre <- as.numeric(grpi)
    grp[, x] <- rownames(grpi)

    ## calcul richesse specifique
    a <- tapply(obs$nombre, list(obs[, x], obs$code_espece), sum, na.rm=TRUE)
    a[is.na(a)] <- 0
    b <- as.data.frame(matrix(NA, dim(a)[1]*dim(a)[2], 3))
    colnames(b) = c(x, "code_espece", "nombre")
    b$nombre <- as.vector(a)
    b[, x] <- rep(dimnames(a)[[1]], dim(a)[2])
    b$code_espece <- rep(dimnames(a)[[2]], each = dim(a)[1], 1)
    b$pres_abs[b$nombre!=0] <- as.integer(1) # pour avoir la richesse spécifique en 'integer'.
    b$pres_abs[b$nombre==0] <- as.integer(0) # pour avoir la richesse spécifique en 'integer'.
    grp$richesse_specifique <- tapply(b$pres_abs, b[, x], sum, na.rm=TRUE)
    rm(a, b, grpi)

    if (unique(unitobs$type) != "LIT")
    {
        ## calcul densites
        unit[, x] <- unitobs[, x][match(unit$unitobs, unitobs$unite_observation)]
        d <- tapply(unit$densite, unit[, x], mean, na.rm=TRUE)
        ## le calcul est fait à partir de $dens donc seules les especes selectionnees pour les calculs de densite sont utilisees.
        grp$densite <- d[match(grp[, x], rownames(d))]
        rm(d)
        grp$nombre <- NULL
        ## suppression de la metrique abondance, on ne travaille que en densites

        ## calcul du pourcentage d'occurrence
        unitesp[, x] <- unitobs[, x][match(unitesp$unite_observation, unitobs$unite_observation)]
        especesPresentes <- subset(unitesp, unitesp$pres_abs==1)$code_espece
        ## calcul du nombre de transects par niveau de facteur
        g <- table(unitesp[, x])/length(unique(unitesp$code_espece))
        for (i in especesPresentes)
        {
            grp[, i] <- NA
        }
        for (i in especesPresentes)
        {
            if (!i%in%unitesp$code_espece)
            {
                grp[, i] <- NA
            }else{
                f <- tapply(unitesp$pres_abs[unitesp[, "code_espece"]==i], unitesp[, x][unitesp[, "code_espece"]==i], na.rm = TRUE, sum)
                grp[, i] <- (f/g)*100
            }
        }
    }else{}

    ## Pourcentage de recouvrement de chaque espece/categorie pour les couvertures biotiques et abiotiques
    if (unique(unitobs$type) == "LIT")
    {
        s <- tapply(grp$nombre, grp[, x], sum, na.rm=TRUE)
        grp$recouvrement <- 100 * grp$nombre / s[match(grp[, x], rownames(s))]
        rm(s)
    }

    assign("grp", grp, envir=.GlobalEnv)
    tkmessageBox(message="La table par groupe d'unite d'observation (1 facteur) calculee: grp", icon="info", type="ok")
    print("La table par groupe d'unite d'observation (1 facteur) calculee: grp")
    grpTranspose <- as.data.frame(t(as.matrix(data.frame(grp))))
    write.csv(grpTranspose, file=paste(nameWorkspace, "/FichiersSortie/Grp.csv", sep=""), row.names = TRUE)
} # fin grp1.f

################################################################################
## Nom    : grp2.f()
## Objet  : calcul des métriques par groupe d'unités d'observation
## Input  : tables "obs", "unitobs", "unit" et "unitesp"
##          + 2 facteurs x et y
## Output : table "grp12" pour les graphiques
################################################################################

grp2f.f <- function (x, y)
{

    print("fonction grp2.f activée")
    ## somme des abondances
    i <- match(obs$unite_observation, unitobs$unite_observation)
    obs[, x] <- unitobs[, x][i]
    obs[, y] <- unitobs[, y][i]
    grp12T <- tapply(obs$nombre, list(obs[, x], obs[, y]), sum, na.rm=TRUE)
    grp12 <- as.data.frame(matrix(NA, dim(grp12T)[1]*dim(grp12T)[2], 3))
    colnames(grp12) = c(x, y, "nombre")
    grp12$nombre <- as.vector(grp12T)
    grp12[, x] <- rep(dimnames(grp12T)[[1]], dim(grp12T)[2])
    grp12[, y] <- rep(dimnames(grp12T)[[2]], each = dim(grp12T)[1], 1)

    ## calcul Richesse Specifique
    a <- tapply(obs$nombre, list(obs[, x], obs[, y], obs$code_espece), sum, na.rm=TRUE)
    a[is.na(a)] <- 0
    b <- as.data.frame(matrix(NA, dim(a)[1]*dim(a)[2]*dim(a)[3], 4))
    colnames(b) = c(x, y, "code_espece", "nombre")
    b$nombre <- as.vector(a, "integer") # 'numeric' changé pour avoir des entiers.
    b[, x] <- rep(dimnames(a)[[1]], times = dim(a)[2] * dim(a)[3])
    b[, y] <- rep(dimnames(a)[[2]], each = dim(a)[1], times = dim(a)[3])
    b$code_espece <- rep(dimnames(a)[[3]], each = dim(a)[1]*dim(a)[2])

    b$pres_abs[b$nombre!=0] <- as.integer(1) # pour avoir la richesse spécifique en 'integer'.1
    b$pres_abs[b$nombre==0] <- as.integer(0) # pour avoir la richesse spécifique en 'integer'.0
    grp12$richesse_specifique <- as.vector(tapply(b$pres_abs, list(b[, x], b[, y]), sum, na.rm=TRUE))
    grp12$richesse_specifique[is.na(grp12$nombre)] <- NA
    ## car pour calcul on à besoin de "a[is.na(a)] <- 0" mais apres il ne faut pas confondre les fois où l'espece n'a pas ete vu, et les fois où les facteurs n'etaient pas croises.
    rm(b)
    rm(a)

    if (unique(unitobs$type) != "LIT")
    {
        ## calcul densites
        i <- match(unit$unitobs, unitobs$unite_observation)
        unit[, x] <- unitobs[, x][i]
        unit[, y] <- unitobs[, y][i]
        d <- tapply(unit$densite, list(unit[, x], unit[, y]), na.rm = TRUE, mean)
        grp12$densite <- as.vector(d, "numeric")
        rm(d)

        ## calcul du pourcentage d'occurrence
        unitesp[, x] <- unitobs[, x][match(unitesp$unite_observation, unitobs$unite_observation)]
        unitesp[, y] <- unitobs[, y][match(unitesp$unite_observation, unitobs$unite_observation)]
        d <- especes$code_espece
        g <- table(unitesp[, x], unitesp[, y])/length(unique(unitesp$code_espece))
        for (i in d)
        {
            grp12[, i] <- NA
        }
        for (i in d )
        {
            if (i%in%unitesp$code_espece == FALSE) # [!!!]
            {
                grp12[, i] <- NA
            }else{
                f <- tapply(unitesp$pres_abs[unitesp[, "code_espece"]==i], list(unitesp[, x][unitesp[, "code_espece"]==i],
                                                    unitesp[, y][unitesp[, "code_espece"]==i]), na.rm = TRUE, sum)
                grp12[, i] <- as.vector((f/g)*100) }
        }
    }
    rm(f)
    rm(g)
    assign("grp12", grp12, envir=.GlobalEnv)
    print("La table par groupe d'unite d'observation (2 facteurs) calculee: grp12")
    grpTranspose <- as.data.frame(t(as.matrix(data.frame(grp12))))
} # fin grp2f.f

################################################################################
## Nom    : grp3f.f()
## Objet  : calcul des métriques par groupe d'unités d'observation
## Input  : tables "obs", "unitobs", "unit" et "unitesp" + 3 facteurs x, y et z
## Output : table "grp3" pour les graphiques
################################################################################

grp3f.f <- function (x, y, z)
{

    print("fonction grp3f.f activée")
    ## somme des abondances
    i <- match(obs$unite_observation, unitobs$unite_observation)
    obs[, x] <- unitobs[, x][i]
    obs[, y] <- unitobs[, y][i]
    obs[, z] <- unitobs[, z][i]
    grp13T <- tapply(obs$nombre, list(obs[, x], obs[, y], obs[, z]), sum, na.rm=TRUE)
    grp13 <- as.data.frame(matrix(NA, dim(grp13T)[1]*dim(grp13T)[2]*dim(grp13T)[3], 4))
    colnames(grp13) = c(x, y, z, "nombre")
    grp13$nombre <- as.vector(grp13T, "integer") # 'numeric' changé pour avoir des entiers.
    grp13[, x] <- rep(dimnames(grp13T)[[1]], times = dim(grp13T)[2] * dim(grp13T)[3])
    grp13[, y] <- rep(dimnames(grp13T)[[2]], each = dim(grp13T)[1], times = dim(grp13T)[3])
    grp13[, z] <- rep(dimnames(grp13T)[[3]], each = dim(grp13T)[1]*dim(grp13T)[2])

    ## calcul Richesse Specifique
    a <- tapply(obs$nombre, list(obs[, x], obs[, y], obs[, z], obs$code_espece), sum, na.rm=TRUE)
    a[is.na(a)] <- 0
    b <- as.data.frame(matrix(NA, dim(a)[1]*dim(a)[2]*dim(a)[3]*dim(a)[4], 5))
    colnames(b) = c(x, y, z, "code_espece", "nombre")
    b$nombre <- as.vector(a, "integer") # 'numeric' changé pour avoir des entiers.
    b[, x] <- rep(dimnames(a)[[1]], times = dim(a)[2] * dim(a)[3] * dim(a)[4])    # <=>   b[, x] <- rownames(a[, , , 1])
    b[, y] <- rep(dimnames(a)[[2]], each = dim(a)[1], times = dim(a)[3] * dim(a)[4])
    b[, z] <- rep(dimnames(a)[[3]], each = dim(a)[1] * dim(a)[2], times = dim(a)[4])
    b$code_espece <- rep(dimnames(a)[[4]], each=dim(a)[1] * dim(a)[2] * dim(a)[3])

    b$pres_abs[b$nombre!=0] <- as.integer(1) # pour avoir la richesse spécifique en 'integer'.1
    b$pres_abs[b$nombre==0] <- as.integer(0) # pour avoir la richesse spécifique en 'integer'.0
    grp13$richesse_specifique <- as.vector(tapply(b$pres_abs, list(b[, x], b[, y], b[, z]), na.rm = TRUE, sum))
    grp13$richesse_specifique[is.na(grp13$nombre)] <- NA # car pour calcul on à besoin de "a[is.na(a)] <- 0" mais apres il ne faut pas confondre les fois où l'espece n'a pas ete vu, et les fois où les facteurs n'etaient pas croises.
    rm(a)
    rm(b)

    if (unique(unitobs$type) != "LIT")
    {
        ## calcul densites
        unit[, x] <- unitobs[, x][match(unit$unitobs, unitobs$unite_observation)]
        unit[, y] <- unitobs[, y][match(unit$unitobs, unitobs$unite_observation)]
        unit[, z] <- unitobs[, z][match(unit$unitobs, unitobs$unite_observation)]
        d <- tapply(unit$densite, list(unit[, x], unit[, y], unit[, z]), na.rm = TRUE, mean)
        grp13$densite <- as.vector(d, "numeric")  # les champs facteurs ont deja ete remplit dans le calcul d'abondances
        rm(d)
        grp13$nombre <- NULL # suppression de la metrique abondance, on ne travaille qu'en densites

        ## calcul du pourcentage d'occurrence
        unitesp[, x] <- unitobs[, x][match(unitesp$unite_observation, unitobs$unite_observation)]
        unitesp[, y] <- unitobs[, y][match(unitesp$unite_observation, unitobs$unite_observation)]
        unitesp[, z] <- unitobs[, z][match(unitesp$unite_observation, unitobs$unite_observation)]
        d <- especes$code_espece
        g <- table(unitesp[, x], unitesp[, y], unitesp[, z])/length(unique(unitesp$code_espece))

        for (i in d)
        {
            grp13[, i] <- NA
        }
        for (i in d )
        {
            if (i%in%unitesp$code_espece == FALSE) # [!!!]
            {
                grp13[, i] <- NA
            }else{
                f <- tapply(unitesp$pres_abs[unitesp[, "code_espece"]==i], list(unitesp[, x][unitesp[, "code_espece"]==i], unitesp[, y][unitesp[, "code_espece"]==i], unitesp[, z][unitesp[, "code_espece"]==i]), na.rm = TRUE, sum)
                grp13[, i] <- as.vector((f/g)*100)
            }
        }
    }

    assign("grp13", grp13, envir=.GlobalEnv)
    tkmessageBox(message="La table par groupe d'unite d'observation (3 facteurs) calculee: grp13", icon="info", type="ok")
    print("La table par groupe d'unite d'observation (3 facteurs) calculee: grp13")
} # fin grp3f.f

################################################################################
## Nom    : grpCT1.f()
## Objet  : calcul des métriques par groupe d'unités d'observation et classes
##          de taille
## Input  : tables "obs", "unitobs", "unit" et "unitesptat" + 1 facteur x
## Output : table "grpCT" pour les graphiques
################################################################################

## [sup] [yr:12/01/2011]:

## grpCT1.f <- function (x)
## {

##     print("fonction grpCT1.f activée")
##     ## somme des abondances
##     obs[, x] <- unitobs[, x][match(obs$unite_observation, unitobs$unite_observation)]
##     grpCTi <- tapply(obs$nombre[obs$classe_taille==unitesptat$classe_taille[1]],
##                      obs[, x][obs$classe_taille==unitesptat$classe_taille[1]], sum, na.rm=TRUE)
##     grpCT <- as.data.frame(matrix(NA, dim(grpCTi)[1], 2))
##     colnames(grpCT) = c(x, "nombre")
##     grpCT$nombre <- as.integer(grpCTi)  # 'as.numeric' remplacé pour avoir des entiers
##     grpCT[, x] <- rownames(grpCTi)

##     ## calcul Richesse Specifique
##     a <- tapply(obs$nombre[obs$classe_taille==unitesptat$classe_taille[1]],
##                 list(obs[, x][obs$classe_taille==unitesptat$classe_taille[1]],
##                      obs$code_espece[obs$classe_taille==unitesptat$classe_taille[1]]), sum, na.rm=TRUE)
##     a[is.na(a)] <- 0
##     b <- as.data.frame(matrix(NA, dim(a)[1]*dim(a)[2], 3))
##     colnames(b) = c(x, "code_espece", "nombre")
##     b$nombre <- as.vector(a)
##     b[, x] <- rep(dimnames(a)[[1]], dim(a)[2])
##     b$code_espece <- rep(dimnames(a)[[2]], each = dim(a)[1], 1)
##     b$pres_abs[b$nombre!=0] <- as.integer(1) # pour avoir la richesse spécifique en 'integer'.1
##     b$pres_abs[b$nombre==0] <- as.integer(0) # pour avoir la richesse spécifique en 'integer'.0
##     grpCT$richesse_specifique <- tapply(b$pres_abs, b[, x], na.rm = TRUE, sum)
##     rm(a, b, grpCTi)

##     if (unique(unitobs$type) != "LIT")
##     {
##         ## calcul densites
##         unitesptat[, x] <- unitobs[, x][match(unitesptat$unitobs, unitobs$unite_observation)]
##         d <- tapply(unitesptat$densite, unitesptat[, x], na.rm = TRUE, mean)  # le calcul est fait à partir de $dens donc seul les espece selectionne pour les calculs de densite sont utilisees.
##         grpCT$densite <- d[match(grpCT[, x], rownames(d))]
##         rm(d)
##         grpCT$nombre <- NULL # suppression de la metrique abondance, on ne travaille que en densites

##         ## calcul du pourcentage d'occurrence
##         unitesp[, x] <- unitobs[, x][match(unitesp$unite_observation, unitobs$unite_observation)]
##         d <- especes$code_espece
##         g <- table(unitesp[, x])/length(unique(unitesp$code_espece))  #calcul du nombre de transects par niveau de facteur

##         for (i in d)
##         {
##             grpCT[, i] <- NA
##         }
##         for (i in d )
##         {
##             if(i%in%unitesp$code_espece == FALSE) # [!!!]
##             {
##                 grpCT[, i] <- NA
##             }else{
##                 f <- tapply(unitesp$pres_abs[unitesp[, "code_espece"]==i], unitesp[, x][unitesp[, "code_espece"]==i], na.rm = TRUE, sum)
##                 grpCT[, i] <- (f/g)*100
##             }
##         }
##     }

##     ## Pourcentage de recouvrement de chaque espece/categorie pour les couvertures biotiques et abiotiques
##     if (unique(unitobs$type) == "LIT")
##     {
##         s <- tapply(grpCT$nombre, grpCT[, x], na.rm = TRUE, sum)
##         grpCT$recouvrement <- 100 * grpCT$nombre / s[match(grpCT[, x], rownames(s))]
##         rm(s)
##     }
##     assign("grpCT", grpCT, envir=.GlobalEnv)
##     tkmessageBox(message="La table par espece et par groupe d'unite d'observation (1 facteur) calculee: grpCT", icon="info", type="ok")
##     print("La table par espece et par groupe d'unite d'observation (1 facteur) calculee: grpCT")
## }

################################################################################
## Nom    : grpCT2.f()
## Objet  : calcul des métriques par groupe d'unités d'observation et classes
##          de taille
## Input  : tables "obs", "unitobs", "unit" et "unitesptat" + 2 facteur x et y
## Output : table "grpCT2" pour les graphiques
################################################################################

## [sup] [yr:12/01/2011]:

## grp2fCT.f <- function (x, y)
## {

##     print("fonction grp2fCT.f activée")
##     ## somme des abondances
##     obs[, x] <- unitobs[, x][match(obs$unite_observation, unitobs$unite_observation)]
##     obs[, y] <- unitobs[, y][match(obs$unite_observation, unitobs$unite_observation)]
##     grpCT2T <- tapply(obs$nombre, list(obs[, x], obs[, y]), sum, na.rm=TRUE)
##     grpCT2 <- as.data.frame(matrix(NA, dim(grpCT2T)[1]*dim(grpCT2T)[2], 3))
##     colnames(grpCT2) = c(x, y, "nombre")
##     grpCT2$nombre <- as.vector(grpCT2T)
##     grpCT2[, x] <- rep(dimnames(grpCT2T)[[1]], dim(grpCT2T)[2])
##     grpCT2[, y] <- rep(dimnames(grpCT2T)[[2]], each = dim(grpCT2T)[1], 1)

##     ## calcul Richesse Specifique
##     a <- tapply(obs$nombre[obs$classe_taille==unitesptat$classe_taille[1]],
##                 list(obs[, x][obs$classe_taille==unitesptat$classe_taille[1]],
##                      obs[, y][obs$classe_taille==unitesptat$classe_taille[1]],
##                      obs$code_espece[obs$classe_taille==unitesptat$classe_taille[1]]), sum, na.rm=TRUE)
##     a[is.na(a)] <- 0
##     b <- as.data.frame(matrix(NA, dim(a)[1]*dim(a)[2]*dim(a)[3], 4))
##     colnames(b) = c(x, y, "code_espece", "nombre")
##     b$nombre <- as.vector(a, "integer") # 'numeric' changé pour avoir des entiers.
##     b[, x] <- rep(dimnames(a)[[1]], times = dim(a)[2] * dim(a)[3])
##     b[, y] <- rep(dimnames(a)[[2]], each = dim(a)[1], times = dim(a)[3])
##     b$code_espece <- rep(dimnames(a)[[3]], each = dim(a)[1]*dim(a)[2])
##     b$pres_abs[b$nombre!=0] <- as.integer(1) # pour avoir la richesse spécifique en 'integer'.1
##     b$pres_abs[b$nombre==0] <- as.integer(0) # pour avoir la richesse spécifique en 'integer'.0
##     grpCT2$richesse_specifique <- as.vector(tapply(b$pres_abs, list(b[, x], b[, y]), na.rm = TRUE, sum))
##     grpCT2$richesse_specifique[is.na(grpCT2$nombre)] <- NA # car pour calcul on a besoin de "a[is.na(a)] <- 0" mais apres il ne faut pas confondre les fois où l'espece n'a pas ete vu, et les fois où les facteurs n'etaient pas croises.
##     rm(b)
##     rm(a)

##     if (unique(unitobs$type) != "LIT")
##     {
##         ## calcul densites
##         unitesptat[, x] <- unitobs[, x][match(unitesptat$unitobs, unitobs$unite_observation)]
##         unitesptat[, y] <- unitobs[, y][match(unitesptat$unitobs, unitobs$unite_observation)]
##         d <- tapply(unitesptat$densite, list(unitesptat[, x], unitesptat[, y]), na.rm = TRUE, mean)
##         grpCT2$densite <- as.vector(d, "numeric")
##         rm(d)

##         ## calcul du pourcentage d'occurrence
##         unitesp[, x] <- unitobs[, x][match(unitesp$unite_observation, unitobs$unite_observation)]
##         unitesp[, y] <- unitobs[, y][match(unitesp$unite_observation, unitobs$unite_observation)]
##         d <- especes$code_espece
##         g <- table(unitesp[, x], unitesp[, y])/length(unique(unitesp$code_espece))

##         for (i in d )
##         {
##             if (i%in%unitesp$code_espece == FALSE) # [!!!]
##             {
##                 grpCT2[, i] <- NA
##             }else{
##                 f <- tapply(unitesp$pres_abs[unitesp[, "code_espece"]==i], list(unitesp[, x][unitesp[, "code_espece"]==i], unitesp[, y][unitesp[, "code_espece"]==i]), na.rm = TRUE, sum)
##                 grpCT2[, i] <- as.vector((f/g)*100)
##             }
##         }
##         rm(f)
##         rm(g)
##         assign("grpCT2", grpCT2, envir=.GlobalEnv)
##         print("La table par espece et par groupe d'unite unite d'observation (2 facteurs) calculee: grpCT2")
##     }
## } # fin grp2fCT

################################################################################
## Nom    : grp3fCT.f()
## Objet  : calcul des métriques par groupe d'unités d'observation et classes
##          de taille
## Input  : tables "obs", "unitobs", "unit" et "unitesptat" + 3 facteurs x, y et z
## Output : table "grpCT3" pour les graphiques
################################################################################

## [sup] [yr:12/01/2011]:

## grp3fCT.f <- function (x, y, z)
## {

##     print("fonction grp3fCT.f activée")
##     ## somme des abondances
##     ## obs$nombre = as.numeric(obs$nombre)
##     obs[, x] <- unitobs[, x][match(obs$unite_observation, unitobs$unite_observation)]
##     obs[, y] <- unitobs[, y][match(obs$unite_observation, unitobs$unite_observation)]
##     obs[, z] <- unitobs[, z][match(obs$unite_observation, unitobs$unite_observation)]
##     grpCT3T <- tapply(obs$nombre, list(obs[, x], obs[, y], obs[, z]), sum, na.rm=TRUE)
##     grpCT3 <- as.data.frame(matrix(NA, dim(grpCT3T)[1]*dim(grpCT3T)[2]*dim(grpCT3T)[3], 4))
##     colnames(grpCT3) = c(x, y, z, "nombre")
##     grpCT3$nombre <- as.vector(grpCT3T, "integer") # 'numeric' changé pour avoir des entiers.
##     grpCT3[, x] <- rep(dimnames(grpCT3T)[[1]], times = dim(grpCT3T)[2] * dim(grpCT3T)[3])
##     grpCT3[, y] <- rep(dimnames(grpCT3T)[[2]], each = dim(grpCT3T)[1], times = dim(grpCT3T)[3])
##     grpCT3[, z] <- rep(dimnames(grpCT3T)[[3]], each = dim(grpCT3T)[1]*dim(grpCT3T)[2])

##     ## calcul Richesse Specifique
##     a <- tapply(obs$nombre[obs$classe_taille==unitesptat$classe_taille[1]],
##                 list(obs[, x][obs$classe_taille==unitesptat$classe_taille[1]],
##                      obs[, y][obs$classe_taille==unitesptat$classe_taille[1]],
##                      obs[, z][obs$classe_taille==unitesptat$classe_taille[1]],
##                      obs$code_espece[obs$classe_taille==unitesptat$classe_taille[1]]), sum, na.rm=TRUE)
##     a[is.na(a)] <- 0
##     b <- as.data.frame(matrix(NA, dim(a)[1]*dim(a)[2]*dim(a)[3]*dim(a)[4], 5))
##     colnames(b) = c(x, y, z, "code_espece", "nombre")
##     b$nombre <- as.vector(a, "integer") # 'numeric' changé pour avoir des entiers.
##     b[, x] <- rep(dimnames(a)[[1]], times = dim(a)[2] * dim(a)[3] * dim(a)[4])    # <=>   b[, x] <- rownames(a[, , , 1])
##     b[, y] <- rep(dimnames(a)[[2]], each = dim(a)[1], times = dim(a)[3] * dim(a)[4])
##     b[, z] <- rep(dimnames(a)[[3]], each = dim(a)[1] * dim(a)[2], times = dim(a)[4])
##     b$code_espece <- rep(dimnames(a)[[4]], each=dim(a)[1] * dim(a)[2] * dim(a)[3])
##     b$pres_abs[b$nombre!=0] <- as.integer(1) # pour avoir la richesse spécifique en 'integer'.1
##     b$pres_abs[b$nombre==0] <- as.integer(0) # pour avoir la richesse spécifique en 'integer'.0
##     grpCT3$richesse_specifique <- as.vector(tapply(b$pres_abs, list(b[, x], b[, y], b[, z]), na.rm = TRUE, sum))
##     grpCT3$richesse_specifique[is.na(grpCT3$densite)] <- NA # car pour calcul on à besoin de "a[is.na(a)] <- 0" mais apres il ne faut pas confondre les fois où l'espece n'a pas ete vu, et les fois où les facteurs n'etaient pas croises.
##     rm(a)
##     rm(b)

##     if (unique(unitobs$type) != "LIT")
##     {
##         ## calcul densites
##         unitesptat[, x] <- unitobs[, x][match(unitesptat$unitobs, unitobs$unite_observation)]
##         unitesptat[, y] <- unitobs[, y][match(unitesptat$unitobs, unitobs$unite_observation)]
##         unitesptat[, z] <- unitobs[, z][match(unitesptat$unitobs, unitobs$unite_observation)]
##         d <- tapply(unitesptat$densite, list(unitesptat[, x], unitesptat[, y], unitesptat[, z]), na.rm = TRUE, mean)
##         grpCT3$densite <- as.vector(d, "numeric")  # les champs facteurs ont deja ete remplit dans le calcul d'abondances
##         rm(d)
##         grpCT3$nombre <- NULL # suppression de la metrique abondance, on ne travaille qu'en densites

##         ## calcul du pourcentage d'occurrence
##         unitesp[, x] <- unitobs[, x][match(unitesp$unite_observation, unitobs$unite_observation)]
##         unitesp[, y] <- unitobs[, y][match(unitesp$unite_observation, unitobs$unite_observation)]
##         unitesp[, z] <- unitobs[, z][match(unitesp$unite_observation, unitobs$unite_observation)]
##         d <- especes$code_espece
##         g <- table(unitesp[, x], unitesp[, y], unitesp[, z])/length(unique(unitesp$code_espece))

##         ## ! manque description des boucles
##         for (i in d)
##         {
##             grpCT3[, i] <- NA
##         }
##         ## ! manque description des boucles
##         for (i in d )
##         {
##             if (i%in%unitesp$code_espece == FALSE) # [!!!]
##             {
##                 grpCT3[, i] <- NA
##             }else{
##                 f <- tapply(unitesp$pres_abs[unitesp[, "code_espece"]==i], list(unitesp[, x][unitesp[, "code_espece"]==i], unitesp[, y][unitesp[, "code_espece"]==i], unitesp[, z][unitesp[, "code_espece"]==i]), na.rm = TRUE, sum)
##                 grpCT3[, i] <- as.vector((f/g)*100)
##             }
##         }
##     }
##     assign("grpCT3", grpCT3, envir=.GlobalEnv)
##     print("La table par espece et par groupe d'unite unite d'observation (3 facteurs) calculee: grpCT3")
## } # fin grp3fCT.f

################################################################################
## Nom     : indicesDiv.f
## Objet   : calculs des indices de diversité taxonomique
## Input   : table "contingence" et référentiel "especes"
## Output  : tables des indices de diversité taxonomique par unité d'observation
## Auteur  : Elodie Gamp et Bastien Preuss
################################################################################

indicesDiv.f <- function ()
{
    print("fonction indicesDiv.f activée")
    ## importation du referentiel especes
    ## tab_sp <- especes

    ## ON PEUT FAIRE BEAUCOUP PLUS SIMPLE ET DIRECT QUE TOUT CE QUI SUIT [yr: 28/10/2010]

    ## ## tableau avec genre et famille
    ## sp.taxon <- data.frame(matrix(NA, ncol(contingence), 5))
    ## IdentifiantsEspeces <- colnames(contingence)
    ## rownames(sp.taxon) <- IdentifiantsEspeces
    ## colnames(sp.taxon) <- c("genre", "famille", "ordre", "classe", "phylum")

    ## ## recherche du genre, de la famille, de l'ordre, de la classe et du phylum
    ## correspondance <- match(IdentifiantsEspeces, especes$code_espece, nomatch=NA, incomparables = FALSE)

    ## genre <- tab_sp$Genre[correspondance]
    ## famille <- tab_sp$Famille[correspondance]
    ## ordre <- tab_sp$Ordre[correspondance]
    ## classe <- tab_sp$Classe[correspondance]
    ## phylum <- tab_sp$Phylum[correspondance]

    ## sp.taxon$genre <- genre
    ## sp.taxon$famille <- famille
    ## sp.taxon$ordre <- ordre
    ## sp.taxon$classe <- classe
    ## sp.taxon$phylum <- phylum

    ## tableau avec genre, famille, etc.
    sp.taxon <- especes[match(colnames(contingence), especes$code_espece, nomatch=NA, incomparables = FALSE),
                        c("Genre", "Famille", "Ordre", "Classe", "Phylum")]

    colnames(sp.taxon) <- c("genre", "famille", "ordre", "classe", "phylum")
    rownames(sp.taxon) <- colnames(contingence)

    ## retrait des lignes ayant un niveau taxonomique manquant dans sp.taxon et dans contingence (en colonnes)

    manque.taxon <- apply(sp.taxon, 1, function(x){any(is.na(x))})
    sp.taxon <- sp.taxon[! manque.taxon, , drop=FALSE]
    contingence <- contingence[, ! manque.taxon, drop=FALSE]

    ## manque_gn <- which(is.na(sp.taxon$genre))
    ## if (length(manque_gn) != 0)
    ## {
    ##     sp.taxon <- sp.taxon[-manque_gn, , drop=FALSE]
    ##     contingence <- contingence[, -manque_gn, drop=FALSE]
    ## }
    ## manque_fa <- which(is.na(sp.taxon$famille))
    ## if (length(manque_fa) != 0)
    ## {
    ##     sp.taxon <- sp.taxon[-manque_fa, , drop=FALSE]
    ##     contingence <- contingence[, -manque_fa, drop=FALSE]
    ## }
    ## manque_or <- which(is.na(sp.taxon$ordre))
    ## if (length(manque_or) != 0)
    ## {
    ##     sp.taxon <- sp.taxon[-manque_or, , drop=FALSE]
    ##     contingence <- contingence[, -manque_or, drop=FALSE]
    ## }
    ## manque_cl <- which(is.na(sp.taxon$classe))
    ## if (length(manque_cl) != 0)
    ## {
    ##     sp.taxon <- sp.taxon[-manque_cl, , drop=FALSE]
    ##     contingence <- contingence[, -manque_cl, drop=FALSE]
    ## }
    ## manque_ph <- which(is.na(sp.taxon$phylum))
    ## if (length(manque_ph) != 0)
    ## {
    ##     sp.taxon <- sp.taxon[-manque_ph, , drop=FALSE]
    ##     contingence <- contingence[, -manque_ph, drop=FALSE]
    ## }

    ## le jeu de donnees doit comporter au moins 2 genres et 2 unité d'observations sinon la fonction taxa2dist ne fonctionne pas
    if (length(unique(sp.taxon$genre))>2)
    {
        ## calcul des distances taxonomiques entre les especes
        taxdis <- taxa2dist(sp.taxon, varstep=TRUE)

        contingence <- round(contingence)
        ## Function finds indices of taxonomic diversity and distinctiness, which are averaged taxonomic distances among species or individuals in the community
        div <- taxondive(contingence, taxdis)

        ## mise de div sous data frame
        ind_div <- data.frame(div[[1]], div[[2]], div[[3]], div[[4]], div[[5]], div[[7]])
        colnames(ind_div) <- c("richesse_specifique", "Delta", "DeltaEtoile", "LambdaPlus", "DeltaPlus", "SDeltaPlus")

        ## Création des objets dans l'environnement global
        assign("div", div, envir=.GlobalEnv)
        assign("taxdis", taxdis, envir=.GlobalEnv)
        assign("ind_div", ind_div, envir=.GlobalEnv)
        ## assign("unit", unit)
    }
    assign("sp.taxon", sp.taxon, envir=.GlobalEnv)
} #fin IndicesDiv




