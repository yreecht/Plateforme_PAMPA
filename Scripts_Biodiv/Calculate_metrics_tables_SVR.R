#-*- coding: latin-1 -*-
# Time-stamp: <2018-12-12 17:57:57 yreecht>

## Plateforme PAMPA de calcul d'indicateurs de ressources & biodiversité
##   Copyright (C) 2008-2018 Ifremer - Tous droits réservés.
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

### File: Calculate_metrics_tables_SVR.R
### Created: <2012-01-19 13:37:52 yreecht>
###
### Author: Yves Reecht
###
####################################################################################################
### Description:
###
### Fonctions spécifiques aux vidéos rotatives pour le calcul des tables de métriques :
####################################################################################################


########################################################################################################################
statRotations.f <- function(facteurs, obs, dataEnv=.GlobalEnv)
{
    ## Purpose: Calcul des statistiques des abondances (max, sd) par rotation
    ##          en se basant sur des données déjà interpolées.
    ## ----------------------------------------------------------------------
    ## Arguments: facteurs : vecteur des noms de facteurs d'agrégation
    ##                       (résolution à laquelle on travaille).
    ##            obs : données d'observation.
    ##            dataEnv : environnement des données (pour sauvegarde de
    ##                      résultats intermédiaires).
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 29 oct. 2012, 16:01

    ## Identification des rotations valides :
    if (is.element("observation.unit", facteurs))
    {
        ## Rotations valides (les vides doivent tout de même être renseignés) :
        rotations <- tapply(obs$rotation,
                            as.list(obs[ , c("observation.unit", "rotation"), drop=FALSE]),
                            function(x)length(x) > 0)

        ## Les rotations non renseignés apparaissent en NA et on veut FALSE :
        rotations[is.na(rotations)] <- FALSE
    }else{
        stop(mltext("statRotations.err.1"))
    }

    ## ###########################################################
    ## Nombres par rotation avec le niveau d'agrégation souhaité :
    nombresR <- tapply(obs$number,
                       as.list(obs[ , c(facteurs, "rotation"), drop=FALSE]),
                       function(x,...){ifelse(all(is.na(x)), NA, sum(x,...))},
                       na.rm = TRUE)

    ## Les NAs sont considérés comme des vrais zéros lorsque la rotation est valide :
    nombresR <- sweep(nombresR,
                      match(names(dimnames(rotations)), names(dimnames(nombresR)), nomatch=NULL),
                      rotations,        # Tableau des secteurs valides (booléens).
                      function(x, y)
                  {
                      x[is.na(x) & y] <- 0 # Lorsque NA et secteur valide => 0.
                      return(x)
                  })

    ## ##################################################
    ## Statistiques :

    ## Moyennes :
    nombresMean <- apply(nombresR, which(is.element(names(dimnames(nombresR)), facteurs)),
                         function(x,...){ifelse(all(is.na(x)), NA, mean(x,...))}, na.rm=TRUE)

    ## Maxima :
    nombresMax <- apply(nombresR, which(is.element(names(dimnames(nombresR)), facteurs)),
                        function(x,...){ifelse(all(is.na(x)), NA, max(x,...))}, na.rm=TRUE)

    ## Déviation standard :
    nombresSD <- apply(nombresR, which(is.element(names(dimnames(nombresR)), facteurs)),
                       function(x,...){ifelse(all(is.na(x)), NA, sd(x,...))}, na.rm=TRUE)

    ## Nombre de rotations valides :
    nombresRotations <- apply(rotations, 1, sum, na.rm=TRUE)

    if (is.element("size.class", facteurs))
    {
    ## ## Pour les calculs agrégés par unitobs :
    ## tmpNombresSVR <- apply(nombresR,
    ##                        which(names(dimnames(nombresR)) != "species.code"),
    ##                        sum, na.rm=TRUE)

    ## tmpNombresSVR[!rotations] <- NA

        ## #### Densités brutes (pour agrégations) :
        ## on réduit les facteurs (calcul de rayon par espèce) :
        factors2 <- facteurs[ ! is.element(facteurs, "size.class")]


        ## rayons par espèce / unitobs :
        rayons <- as.table(tapply(obs[obs[ , "number"] > 0 , "min.distance"],
                                  as.list(obs[obs[ , "number"] > 0,
                                              factors2, drop=FALSE]),
                                  max, na.rm=TRUE))

        densitesR <- sweep(nombresR,
                           match(names(dimnames(rayons)), names(dimnames(nombresR))),
                           pi * rayons ^ 2,
                           "/")

        ## Les NAs sont considérés comme des vrais zéros lorsque la rotation est valide :
        densitesR <- sweep(densitesR,
                          match(names(dimnames(rotations)), names(dimnames(densitesR)), nomatch=NULL),
                          rotations,        # Tableau des secteurs valides (booléens).
                          function(x, y)
                      {
                          x[is.na(x) & y] <- 0 # Lorsque NA et secteur valide => 0.
                          return(x)
                      })

        ## Sauvegardes dans l'environnement des données :
        assign(".NombresSVR", nombresR, envir=dataEnv)
        assign(".DensitesSVR", densitesR, envir=dataEnv)
        assign(".Rotations", rotations, envir=dataEnv)
    }else{}

    ## Retour des résultats sous forme de liste :
    return(list(nombresMean=nombresMean, nombresMax=nombresMax, nombresSD=nombresSD,
                nombresRotations=nombresRotations, nombresTot=nombresR))
}


########################################################################################################################
calc.density.SVR.f <- function(Data, obs, metric="density",
                               factors=c("observation.unit", "species.code"))
{
    ## Purpose:
    ## ----------------------------------------------------------------------
    ## Arguments:
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 22 déc. 2011, 11:12

    ## Nom de la colonne de nombre en fonction de la métrique de densité :
    nbMetric <- c(density="number",
                  density.max="number.max",
                  density.sd="number.sd")

    if ( ! all(is.na(nbMetric[metric])))     # la colonne de nombre doit être définie.
    {
        metric <- metric[!is.na(nbMetric[metric])]

        ## Calcul du rayon d'observation :
        Data <- merge(Data,
                      ## Calcul du max du diamètre minimum sur les observation conservées(nombre > 0) :
                      as.data.frame(as.table(tapply(obs[obs[ , "number"] > 0 , "min.distance"],
                                                    as.list(obs[obs[ , "number"] > 0, factors, drop=FALSE]),
                                                    max, na.rm=TRUE)),
                                    responseName="r"))

        ## Calcul des différentes densités :
        res <- lapply(metric,
                      function(x, Data, nbMetrics)
                  {
                      density <- Data[ , nbMetric[x]] /
                          (pi * (Data[ , "r"] ^ 2))

                      ## Vrais zéros :
                      density[Data[ , nbMetric[x]] == 0 & !is.na(Data[ , nbMetric[x]])] <- 0

                      return(density)
                  },
                      Data=Data, nbMetrics=nbMetrics)

        ## Ajout des résultats à Data
        names(res) <- metric

        res <- as.data.frame(res)

        Data <- cbind(Data, res)
        Data$r <- NULL

        ## density <- Data[ , nbMetric[metric]] /
        ##     ## Surface : vecteur recyclé autant de fois qu'il y a de classes de taille
        ##     ##           si Data  en contient.
        ##     (pi * (as.vector(t(tapply(obs[ , "min.distance"],
        ##                               as.list(obs[ , factors, drop=FALSE]),
        ##                               max, na.rm=TRUE))))^2)

        ## Seconde méthode désactivée car certaines fois t() requis, d'autres fois pas.

        return(Data)
    }else{
        stop("Unknown metrics!")
    }
}

########################################################################################################################
calc.biomass.SVR.f <- function(Data, obs, factors=c("observation.unit", "species.code"))
{
    ## Purpose:
    ## ----------------------------------------------------------------------
    ## Arguments:
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 22 déc. 2011, 12:02

    ## Calcul du rayon d'observation :
    Data <- merge(Data,
                  ## Calcul du max du diamètre minimum sur les observation conservées(nombre > 0) :
                  as.data.frame(as.table(tapply(obs[obs[ , "number"] > 0 , "min.distance"],
                                                as.list(obs[obs[ , "number"] > 0, factors, drop=FALSE]),
                                                max, na.rm=TRUE)),
                                responseName="r"))

    biomass <- Data[ , "weight"] /
        (pi * (Data[ , "r"] ^ 2))
    ## Les poids ont été corrigés au préalable et tiennent compte des espèces pour lesquelles
    ## ils ne peuvent être calculés.
    ## Aucune correction n'est donc nécessaire.

    return(biomass)
}

########################################################################################################################
stat.biomass.SVR.f <- function(Data, obs, metric,
                               factors=c("observation.unit", "species.code"))
{
    ## Purpose:
    ## ----------------------------------------------------------------------
    ## Arguments:
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 22 déc. 2011, 12:10

    ## Nom de la colonne de nombre en fonction de la métrique de densité :
    nbMetric <- c(biomass.max="number.max",
                  biomass.sd="number.sd")

    if ( ! all(is.na(nbMetric[metric])))     # la colonne de nombre doit être définie.
    {
        metric <- metric[!is.na(nbMetric[metric])]

        ## Calcul du rayon d'observation :
        Data <- merge(Data,
                      ## Calcul du max du diamètre minimum sur les observation conservées(nombre > 0) :
                      as.data.frame(as.table(tapply(obs[obs[ , "number"] > 0 , "min.distance"],
                                                    as.list(obs[obs[ , "number"] > 0, factors, drop=FALSE]),
                                                    max, na.rm=TRUE)),
                                    responseName="r"))

        ## Calcul des différentes densités :
        res <- lapply(metric,
                      function(x, Data, nbMetrics)
                  {
                      return(Data[ , nbMetric[x]] * # métrique de nombre
                             Data[ , "mean.weight"] /    # * poids moyens d'un individu.
                             (pi * (Data[ , "r"] ^ 2)))
                  },
                      Data=Data, nbMetrics=nbMetrics)

        ## Ajout des résultats à Data
        names(res) <- metric

        res <- as.data.frame(res)

        Data <- cbind(Data, res)
        Data$r <- NULL

        return(Data)
    }else{
        stop("Biomass metrics unknown!")
    }
}


########################################################################################################################
calc.tables.SVR.f <- function(obs,
                              dataEnv,
                              factors=c("observation.unit", "species.code", "size.class"))
{
    ## Purpose: Calcul générique d'une table de métriques pour les vidéos
    ##          rotatives.
    ## ----------------------------------------------------------------------
    ## Arguments: obs : table des observations (data.frame).
    ##            dataEnv : environnement des données.
    ##            factors : les facteurs d'agrégation.
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 21 déc. 2011, 14:33

    stepInnerProgressBar.f(n=1, msg=mltext("calc.tables.SVR.info.1"))

    ## Calcul des statistiques de nombres :
    statRotations <- statRotations.f(facteurs=factors,
                                             obs=obs,
                                             dataEnv=dataEnv)

    ## Moyenne pour les vidéos rotatives (habituellement 3 rotation) :
    nbr <- statRotations[["nombresMean"]]

    switch(is.element("size.class", factors),
           "TRUE"=stepInnerProgressBar.f(n=7, msg=paste(mltext("calc.tables.SVR.info.2"),
                                                        mltext("calc.tables.SVR.info.3"), sep="")),
           "FALSE"=stepInnerProgressBar.f(n=7, msg=paste(mltext("calc.tables.SVR.info.2"),
                                                         mltext("calc.tables.SVR.info.4"), sep="")))

    ## Création de la data.frame de résultats (avec nombres, unitobs, ):
    res <- calc.numbers.f(nbr)

    ## Statistiques sur les nombres :
    res$number.max <- as.vector(statRotations[["nombresMax"]])
    res$number.sd <- as.vector(statRotations[["nombresSD"]])

    ## Tailles moyennes :
    res[ , "mean.length"] <- calc.meanSize.f(obs=obs, factors=factors)

    ## Poids :
    res[ , "weight"] <- calc.weight.f(obs=obs, Data=res, factors=factors)

    ## Poids moyen par individu :
    res[ , "mean.weight"] <- calc.meanWeight.f(Data=res)

    ## Densité (+Max +SD) :
    res <- calc.density.SVR.f(Data=res, obs=obs,
                               metric=c("density", "density.max", "density.sd"))

    ## Biomass :
    res[ , "biomass"] <- calc.biomass.SVR.f(Data=res, obs=obs)

    ## Biomass max+SD :
    res <- stat.biomass.SVR.f(Data=res, obs=obs,
                              metric=c("biomass.max", "biomass.sd"))

    ## Présence/absence :
    res[ , "pres.abs"] <- calc.presAbs.f(Data=res)

    if (is.element("size.class", factors))
    {
        ## Proportions d'abondance par classe de taille :
        res[ , "abundance.prop.SC"] <- unitSpSz.propAb.f(unitSpSz=res,
                                                         factors=factors)

        ## Proportions de biomasse par classe de taille :
        res[ , "biomass.prop.SC"] <- unitSpSz.propBiom.f(unitSpSz=res,
                                                          factors=factors)
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
    unitSp <- calc.unitSp.default.f(unitSpSz, dataEnv=dataEnv)

    ## Calcul à partir des données extrapolées brutes pour les statistiques :
    nbInterp <- get(".NombresSVR", envir=dataEnv)

    nbTmp <- apply(nbInterp,
                   which( ! is.element(names(dimnames(nbInterp)), "size.class")),
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

    if ( ! isTRUE(all.equal(unitSp$number, nbTest))) stop(mltext("calc.unitSp.SVR.err.1"))

    ## nombre max :
    unitSp[ , "number.max"] <- as.vector(t(apply(nbTmp,
                                                which( ! is.element(names(dimnames(nbTmp)), "rotation")),
                                                function(x,...)
                                            {
                                                ifelse(all(is.na(x)), NA, max(x,...))
                                            }, na.rm=TRUE)))

    ## nombre SD :
    unitSp[ , "number.sd"] <- as.vector(t(apply(nbTmp,
                                               which( ! is.element(names(dimnames(nbTmp)), "rotation")),
                                               function(x,...)
                                           {
                                               ifelse(all(is.na(x)), NA, sd(x,...))
                                           }, na.rm=TRUE)))

    ## densité max :
    unitSp <- calc.density.SVR.f(Data=unitSp, obs=obs,
                                 metric=c("density.max", "density.sd"))

    ## Biomass max :
    unitSp <- stat.biomass.SVR.f(Data=unitSp, obs=obs,
                                 metric=c("biomass.max", "biomass.sd"))

    return(unitSp)
}

########################################################################################################################
calc.unit.SVR.f <- function(unitSp, obs, refesp, unitobs, dataEnv,
                            colNombres="number")
{
    ## Purpose:
    ## ----------------------------------------------------------------------
    ## Arguments:
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 23 déc. 2011, 10:36

    ## Agrégation des métriques par défaut :
    unit <- calc.unit.default.f(unitSp=unitSp, refesp=refesp, unitobs=unitobs, colNombres="number", dataEnv=dataEnv)

    ## Ajout des statistiques sur les rotations
    ## (calcul à partir des données extrapolées brutes pour les statistiques) :
    nbInterp <- get(".NombresSVR", envir=dataEnv)

    ## Réduction de la liste d'espèces si besoin (si sélection sur les espèces) :
    if (dim(nbInterp)[names(dimnames(nbInterp)) == "species.code"] > nlevels(unitSp[ , "species.code"]))
    {
        species <- dimnames(nbInterp)[["species.code"]]

        nbInterp <- extract(nbInterp,
                            indices=list(species[is.element(species,
                                                            levels(unitSp[ , "species.code"]))]),
                            dims=which(is.element(names(dimnames(nbInterp)), "species.code")))

    }else{}

    nbTmp <- apply(nbInterp,
                   which( ! is.element(names(dimnames(nbInterp)), c("size.class", "species.code"))),
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

    if ( ! isTRUE(all.equal(unit$number, nbTest))) stop(mltext("calc.unitSp.SVR.err.1"))

    ## nombre max :
    unit[ , "number.max"] <- as.vector(t(apply(nbTmp,
                                              which( ! is.element(names(dimnames(nbTmp)), "rotation")),
                                              function(x,...)
                                          {
                                              ifelse(all(is.na(x)), NA, max(x,...))
                                          }, na.rm=TRUE)))

    ## nombre SD :
    unit[ , "number.sd"] <- as.vector(t(apply(nbTmp,
                                             which( ! is.element(names(dimnames(nbTmp)), "rotation")),
                                             function(x,...)
                                         {
                                             ifelse(all(is.na(x)), NA, sd(x,...))
                                         }, na.rm=TRUE)))

    ## Density max :
    unit <- calc.density.SVR.f(Data=unit, obs=obs,
                               metric=c("density.max", "density.sd"),
                               factors="observation.unit")

    ## Biomass max :
    unit <- stat.biomass.SVR.f(Data=unit, obs=obs,
                               metric=c("biomass.max", "biomass.sd"),
                               factors="observation.unit")

    return(unit)
}




## ## tmp :
## names(dimnames(.dataEnv$.NombresSVR))
## [1] "observation.unit" "species.code"       "size.class"
## [4] "rotation"

## nombre <- apply(.dataEnv$.NombresSVR, c(1, 2, 4), function(x,...){ifelse(all(is.na(x)), NA, sum(x,...))}, na.rm=TRUE)

## names(dimnames(nombre))
## [1] "observation.unit" "species.code"       "rotation"

## nombre2 <- apply(nombre, c(1, 2), function(x,...){ifelse(all(is.na(x)), NA, mean(x,...))}, na.rm=TRUE)

## test2$nbcalc <- as.vector(t(nombre2))

## head(subset(test2[ , c(1:3, 10)], nbcalc > 0 & nombre > 0))


## ## unit :
## nombre <- apply(.dataEnv$.NombresSVR, c(1, 4), function(x,...){ifelse(all(is.na(x)), NA, sum(x,...))}, na.rm=TRUE)

## names(dimnames(nombre))

## nombre3 <- apply(nombre, c(1), function(x,...){ifelse(all(is.na(x)), NA, mean(x,...))}, na.rm=TRUE)

## test3$nbcalc <- as.vector(nombre3)

## head(subset(test3[ , c(1:2, ncol(test3))], nbcalc > 0 & number > 0))


### Local Variables:
### ispell-local-dictionary: "english"
### fill-column: 120
### End:
