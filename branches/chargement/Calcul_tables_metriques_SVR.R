#-*- coding: latin-1 -*-

### File: Calcul_tables_metriques_SVR.R
### Time-stamp: <2011-12-23 11:28:19 yreecht>
###
### Author: Yves Reecht
###
####################################################################################################
### Description:
###
### Fonctions spécifiques aux vidéos rotatives pour le calcul des tables de métriques :
####################################################################################################

calc.density.SVR.f <- function(Data, obs, metric="densite",
                               factors=c("unite_observation", "code_espece"))
{
    ## Purpose:
    ## ----------------------------------------------------------------------
    ## Arguments:
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 22 déc. 2011, 11:12

    ## Nom de la colonne de nombre en fonction de la métrique de densité :
    nbMetric <- c(densite="nombre",
                  densiteMax="nombreMax",
                  densiteSD="nombreSD")

    if ( ! is.na(nbMetric[metric]))     # la colonne de nombre doit être définie.
    {
        density <- Data[ , nbMetric[metric]] /
            ## Surface : vecteur recyclé autant de fois qu'il y a de classes de taille
            ##           si Data  en contient.
            (pi * (as.vector(tapply(obs[ , "dmin"],
                                    as.list(obs[ , factors, drop=FALSE]),
                                    max, na.rm=TRUE)))^2)

        ## Vrais zéros :
        density[Data[ , nbMetric[metric]] == 0 & !is.na(Data[ , nbMetric[metric]])] <- 0

        return(density)
    }else{
        stop("Métrique de densité inconnue !")
    }
}

########################################################################################################################
## stat.density.SVR.f <- function(Data, obs, metric,
##                                factors=c("unite_observation", "code_espece"))
## {
##     ## Purpose:
##     ## ----------------------------------------------------------------------
##     ## Arguments:
##     ## ----------------------------------------------------------------------
##     ## Author: Yves Reecht, Date: 22 déc. 2011, 12:10

##     ## Nom de la colonne de nombre en fonction de la métrique de densité :
##     nbMetric <- c(densiteMax="nombreMax",
##                   densiteSD="nombreSD")

##     if ( ! is.na(nbMetric[metric]))     # la colonne de nombre doit être définie.
##     {
##         stat <- Data[ , nbMetric[metric]] / # métrique de nombre
##                 ## / Surface : vecteur recyclé autant de fois qu'il y a de classes de taille
##                 ##           si Data  en contient.
##                 (pi * (as.vector(tapply(obs[ , "dmin"],
##                                         as.list(obs[ , factors, drop=FALSE]),
##                                         max, na.rm=TRUE)))^2)

##         return(stat)
##     }else{
##         stop("Métrique de biomasse inconnue !")
##     }
## }

########################################################################################################################
calc.biomass.SVR.f <- function(Data, obs)
{
    ## Purpose:
    ## ----------------------------------------------------------------------
    ## Arguments:
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 22 déc. 2011, 12:02

    biomass <- Data[ , "poids"] /
        ## Surface : vecteur recyclé autant de fois qu'il y a de classes de taille
        ##           si Data  en contient.
        (pi * (as.vector(tapply(obs[ , "dmin"],
                                as.list(obs[ , c("unite_observation", "code_espece")]),
                                max, na.rm=TRUE)))^2)

    ## Les poids ont été corrigés au préalable et tiennent compte des espèces pour lesquelles
    ## ils ne peuvent être calculés.
    ## Aucune correction n'est donc nécessaire.
}

########################################################################################################################
stat.biomass.SVR.f <- function(Data, obs, metric,
                               factors=c("unite_observation", "code_espece"))
{
    ## Purpose:
    ## ----------------------------------------------------------------------
    ## Arguments:
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 22 déc. 2011, 12:10

    ## Nom de la colonne de nombre en fonction de la métrique de densité :
    nbMetric <- c(biomasseMax="nombreMax",
                  biomasseSD="nombreSD")

    if ( ! is.na(nbMetric[metric]))     # la colonne de nombre doit être définie.
    {
        stat <- Data[ , nbMetric[metric]] * # métrique de nombre
            Data[ , "poids_moyen"] /        # * poids moyens d'un individu.
                ## / Surface : vecteur recyclé autant de fois qu'il y a de classes de taille
                ##           si Data  en contient.
                (pi * (as.vector(tapply(obs[ , "dmin"],
                                        as.list(obs[ , factors, drop=FALSE]),
                                        max, na.rm=TRUE)))^2)

        return(stat)
    }else{
        stop("Métrique de biomasse inconnue !")
    }
}


########################################################################################################################
calc.tables.SVR.f <- function(obs,
                              dataEnv,
                              factors=c("unite_observation", "code_espece", "classe_taille"))
{
    ## Purpose:
    ## ----------------------------------------------------------------------
    ## Arguments:
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 21 déc. 2011, 14:33

    stepInnerProgressBar.f(n=1, msg="Interpolations pour vidéos rotatives (étape longue)")

    switch(getOption("PAMPA.SVR.interp"),
           "extended"={
               statRotations <- statRotation.extended.f(facteurs=factors,
                                                        obs=obs,
                                                        dataEnv=dataEnv)
           },
           "basic"={
               statRotations <- statRotation.basic.f(facteurs=factors,
                                                     obs=obs,
                                                     dataEnv=dataEnv)
           },
           stop(paste("Y'a un truc qui cloche dans les options d'interpolations : ",
                      "\n\tcontactez le support technique !", sep=""))
           )

    ## Correction (locale) des nombres dans les observations :
    corNb <- as.data.frame(as.table(statRotations$nombresTot), responseName="Nb.cor")

    obs$Nb.cor <- NULL                  # Pour le cas où ça aurait déjà été calculé.

    corNb[ , "Nb.obs"] <- as.data.frame(as.table(tapply(obs[ , "nombre"],
                                                        as.list(obs[ , colnames(corNb)[is.element(colnames(corNb),
                                                                                                  colnames(obs))]]),
                                                        function(x){ifelse(all(is.na(x)),
                                                                           NA,
                                                                           sum(x, na.rm=TRUE))})),
                                        responseName="Nb.obs")[ , "Nb.obs"]

    ## Facteur de correction :
    corNb[ , "fact"] <- corNb[ , "Nb.cor"] /corNb[ , "Nb.obs"]

    ## Ajout de la colonne avec le facteur de correction :
    obs <- merge(obs, corNb)

    ## Correction de poids et nombres :
    obs[ , "nombre"] <- obs[ , "nombre"] * obs[ , "fact"]
    obs[ , "poids"] <- obs[ , "poids"] * obs[ , "fact"]

    ## Libération de la mémoire :
    rm(corNb)
    gc()

    ## Moyenne pour les vidéos rotatives (habituellement 3 rotation) :
    nbr <- statRotations[["nombresMean"]]

    switch(is.element("classe_taille", factors),
           "TRUE"=stepInnerProgressBar.f(n=7, msg=paste("Calcul des métriques par unité d'observation, ",
                                                        "espèce et classe de taille...", sep="")),
           "FALSE"=stepInnerProgressBar.f(n=7, msg=paste("Calcul des métriques par unité d'observation et ",
                                                         "espèce...", sep="")))

    ## Création de la data.frame de résultats (avec nombres, unitobs, ):
    res <- calc.numbers.f(nbr)

    ## Statistiques sur les nombres :
    res$nombreMax <- as.vector(statRotations[["nombresMax"]])
    res$nombreSD <- as.vector(statRotations[["nombresSD"]])

    ## Tailles moyennes :
    res[ , "taille_moyenne"] <- calc.meanSize.f(obs=obs, factors=factors)

    ## Poids :
    res[ , "poids"] <- calc.weight.f(obs=obs, Data=res, factors=factors)

    ## Poids moyen par individu :
    res[ , "poids_moyen"] <- calc.meanWeight.f(Data=res)

    ## Densité :
    res[ , "densite"] <- calc.density.SVR.f(Data=res, obs=obs,
                                            metric="densite")

    ## Densité max :
    res[ , "densiteMax"] <- calc.density.SVR.f(Data=res, obs=obs,
                                               metric="densiteMax")

    ## Densité SD :
    res[ , "densiteSD"] <- calc.density.SVR.f(Data=res, obs=obs,
                                              metric="densiteSD")

    ## Biomasse :
    res[ , "biomasse"] <- calc.biomass.SVR.f(Data=res, obs=obs)

    ## Biomasse max :
    res[ , "biomasseMax"] <- stat.biomass.SVR.f(Data=res, obs=obs,
                                                metric="biomasseMax")

    ## Biomasse SD :
    res[ , "biomasseSD"] <- stat.biomass.SVR.f(Data=res, obs=obs,
                                               metric="biomasseSD")

    ## Présence/absence :
    res[ , "pres_abs"] <- calc.presAbs.f(Data=res)

    if (is.element("classe_taille", factors))
    {
        ## Proportions d'abondance par classe de taille :
        res[ , "prop.abondance.CL"] <- unitSpSz.propAb.f(unitSpSz=res)

        ## Proportions de biomasse par classe de taille :
        res[ , "prop.biomasse.CL"] <- unitSpSz.propBiom.f(unitSpSz=res)
    }else{}

    return(res)
}

########################################################################################################################
calc.unitSp.SVR.f <- function(unitSpSz, obs, dataEnv)
{
    ## Purpose:
    ## ----------------------------------------------------------------------
    ## Arguments:
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 23 déc. 2011, 09:43

    ## Agrégation pour les métriques par défaut (idem cas général) :
    unitSp <- calc.unitSp.default.f(unitSpSz)

    ## Calcul à partir des données extrapolées brutes pour les statistiques :
    nbInterp <- get(".NombresSVR", envir=dataEnv)

    nbTmp <- apply(nbInterp,
                   which( ! is.element(names(dimnames(nbInterp)), "classe_taille")),
                   function(x,...)
               {
                   ifelse(all(is.na(x)), NA, sum(x,...))
               }, na.rm=TRUE)

    ## nombres calculés... pour comparaison avec nombres agrégé uniquement :
    nbTest <- as.vector(t(apply(nbTmp,
                                which( ! is.element(names(dimnames(nbTmp)), "rotation")),
                                function(x,...)
                            {
                                ifelse(all(is.na(x)), NA, mean(x,...))
                            }, na.rm=TRUE)))

    if ( ! all.equal(unitSp$nombre, nbTest)) stop("Problème dans le calcul des statistiques SVR !")

    ## nombre max :
    unitSp[ , "nombreMax"] <- as.vector(t(apply(nbTmp,
                                                which( ! is.element(names(dimnames(nbTmp)), "rotation")),
                                                function(x,...)
                                            {
                                                ifelse(all(is.na(x)), NA, max(x,...))
                                            }, na.rm=TRUE)))

    ## nombre SD :
    unitSp[ , "nombreSD"] <- as.vector(t(apply(nbTmp,
                                               which( ! is.element(names(dimnames(nbTmp)), "rotation")),
                                               function(x,...)
                                           {
                                               ifelse(all(is.na(x)), NA, sd(x,...))
                                           }, na.rm=TRUE)))

    ## densité max :
    unitSp[ , "densiteMax"] <- calc.density.SVR.f(Data=unitSp, obs=obs,
                                                  metric="densiteMax")

    ## densité SD :
    unitSp[ , "densiteSD"] <- calc.density.SVR.f(Data=unitSp, obs=obs,
                                                 metric="densiteSD")

    ## Biomasse max :
    unitSp[ , "biomasseMax"] <- stat.biomass.SVR.f(Data=unitSp, obs=obs,
                                                   metric="biomasseMax")

    ## Biomasse SD :
    unitSp[ , "biomasseSD"] <- stat.biomass.SVR.f(Data=unitSp, obs=obs,
                                                  metric="biomasseSD")

    return(unitSp)
}

########################################################################################################################
calc.unit.SVR.f <- function(unitSp, obs, refesp, unitobs, dataEnv,
                            colNombres="nombre")
{
    ## Purpose:
    ## ----------------------------------------------------------------------
    ## Arguments:
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 23 déc. 2011, 10:36

    ## Agrégation des métriques par défaut :
    unit <- calc.unit.default.f(unitSp=unitSp, refesp=refesp, unitobs=unitobs, colNombres="nombre")

    ## Ajout des statistiques sur les rotations
    ## (calcul à partir des données extrapolées brutes pour les statistiques) :
    nbInterp <- get(".NombresSVR", envir=dataEnv)

    nbTmp <- apply(nbInterp,
                   which( ! is.element(names(dimnames(nbInterp)), c("classe_taille", "code_espece"))),
                   function(x,...)
               {
                   ifelse(all(is.na(x)), NA, sum(x,...))
               }, na.rm=TRUE)

    ## nombres calculés... pour comparaison avec nombres agrégé uniquement :
    nbTest <- as.vector(t(apply(nbTmp,
                                which( ! is.element(names(dimnames(nbTmp)), "rotation")),
                                function(x,...)
                            {
                                ifelse(all(is.na(x)), NA, mean(x,...))
                            }, na.rm=TRUE)))

    if ( ! all.equal(unit$nombre, nbTest)) stop("Problème dans le calcul des statistiques SVR !")

    ## nombre max :
    unit[ , "nombreMax"] <- as.vector(t(apply(nbTmp,
                                              which( ! is.element(names(dimnames(nbTmp)), "rotation")),
                                              function(x,...)
                                          {
                                              ifelse(all(is.na(x)), NA, max(x,...))
                                          }, na.rm=TRUE)))

    ## nombre SD :
    unit[ , "nombreSD"] <- as.vector(t(apply(nbTmp,
                                             which( ! is.element(names(dimnames(nbTmp)), "rotation")),
                                             function(x,...)
                                         {
                                             ifelse(all(is.na(x)), NA, sd(x,...))
                                         }, na.rm=TRUE)))

    ## Densite max :
    unit[ , "densiteMax"] <- calc.density.SVR.f(Data=unit, obs=obs,
                                                metric="densiteMax",
                                                factors="unite_observation")

    ## Densite SD :
    unit[ , "densiteSD"] <- calc.density.SVR.f(Data=unit, obs=obs,
                                               metric="densiteSD",
                                               factors="unite_observation")

    ## Biomasse max :
    unit[ , "biomasseMax"] <- stat.biomass.SVR.f(Data=unit, obs=obs,
                                                 metric="biomasseMax",
                                                 factors="unite_observation")

    ## Biomasse SD :
    unit[ , "biomasseSD"] <- stat.biomass.SVR.f(Data=unit, obs=obs,
                                                metric="biomasseSD",
                                                factors="unite_observation")

    return(unit)
}




## ## tmp :
## names(dimnames(.dataEnv$.NombresSVR))
## [1] "unite_observation" "code_espece"       "classe_taille"
## [4] "rotation"

## nombre <- apply(.dataEnv$.NombresSVR, c(1, 2, 4), function(x,...){ifelse(all(is.na(x)), NA, sum(x,...))}, na.rm=TRUE)

## names(dimnames(nombre))
## [1] "unite_observation" "code_espece"       "rotation"

## nombre2 <- apply(nombre, c(1, 2), function(x,...){ifelse(all(is.na(x)), NA, mean(x,...))}, na.rm=TRUE)

## test2$nbcalc <- as.vector(t(nombre2))

## head(subset(test2[ , c(1:3, 10)], nbcalc > 0 & nombre > 0))


## ## unit :
## nombre <- apply(.dataEnv$.NombresSVR, c(1, 4), function(x,...){ifelse(all(is.na(x)), NA, sum(x,...))}, na.rm=TRUE)

## names(dimnames(nombre))

## nombre3 <- apply(nombre, c(1), function(x,...){ifelse(all(is.na(x)), NA, mean(x,...))}, na.rm=TRUE)

## test3$nbcalc <- as.vector(nombre3)

## head(subset(test3[ , c(1:2, ncol(test3))], nbcalc > 0 & nombre > 0))


### Local Variables:
### ispell-local-dictionary: "english"
### fill-column: 120
### End:
