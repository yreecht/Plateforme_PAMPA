#-*- coding: latin-1 -*-

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

### File: Calculate_metrics_tables.R
### Time-stamp: <2012-02-24 19:58:32 Yves>
###
### Author: Yves Reecht
###
####################################################################################################
### Description:
###
### Calcul des tables de métriques (unitSpSz, unitSp, unit)
####################################################################################################
########################################################################################################################
calcNumber.default.f <- function(obs,
                                 factors=c("observation.unit", "species.code", "size.class"),
                                 nbName="number")
{
    ## Purpose: Calcul des nombres au niveau d'agrégation souhaité.
    ## ----------------------------------------------------------------------
    ## Arguments: obs : table des observations (data.frame).
    ##            factors : les facteurs d'agrégation.
    ##            nbName : nom de la colonne nombre.
    ##
    ## Output: un array avec autant de dimensions que de facteurs
    ##         d'agrégation.
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 19 déc. 2011, 13:38

    ## Somme des nombres d'individus :
    nbr <- tapply(obs[ , nbName],
                  as.list(obs[ , factors]),
                  sum, na.rm = TRUE)

    ## Absences considérée comme "vrais zéros" :
    nbr[is.na(nbr)] <- 0

    return(nbr)
}

########################################################################################################################
calc.numbers.f <- function(nbr, nbName="number")
{
    ## Purpose: Produit la data.frame qui va servir de table, à partir du
    ##          tableau de nombres produit par calcNumber.default.f().
    ## ----------------------------------------------------------------------
    ## Arguments: nbr : array de nombres avec autant de dimensions que de
    ##                  facteurs d'agrégations.
    ##            nbName : nom de la colonne nombre.
    ##
    ## Output: une data.frame avec (nombre de facteurs d'agrégation + 1)
    ##         colonnes.
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 19 déc. 2011, 13:46

    res <- as.data.frame(as.table(nbr), responseName=nbName)
    ## res$unitobs <- res$observation.unit # Pour compatibilité uniquement !!!

    if (is.element("size.class", colnames(res)))
    {
        res$size.class[res$size.class == ""] <- NA
    }else{}

    ## Si les nombres sont des entiers, leur redonner la bonne classe :
    if (isTRUE(all.equal(res[ , nbName], as.integer(res[ , nbName]))))
    {
        res[ , nbName] <- as.integer(res[ , nbName])
    }else{}

    return(res)
}

########################################################################################################################
calc.meanSize.f <- function(obs,
                            factors=c("observation.unit", "species.code", "size.class"),
                            nbName="number")
{
    ## Purpose: Calcul des tailles moyennes pondérées (par le nombre
    ##          d'individus)
    ## ----------------------------------------------------------------------
    ## Arguments: obs : table des observations (data.frame).
    ##            factors : les facteurs d'agrégation.
    ##            nbName : nom de la colonne nombre.
    ##
    ## Output: vecteur des tailles moyennes.
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 19 déc. 2011, 13:51

    return(as.vector(tapply(seq(length.out=nrow(obs)),
                            as.list(obs[ , factors]),
                            function(ii)
                        {
                            weighted.mean(obs[ii, "length"], obs[ii, nbName])
                        })))
}

########################################################################################################################
calc.weight.f <- function(obs, Data,
                          factors=c("observation.unit", "species.code", "size.class"),
                          nbName="number")
{
    ## Purpose: Calcul des poids totaux.
    ## ----------------------------------------------------------------------
    ## Arguments: obs : table des observations (data.frame).
    ##            Data : la table de métrique (temporaire).
    ##            factors : les facteurs d'agrégation.
    ##            nbName : nom de la colonne nombre.
    ##
    ## Output: vecteur des poids.
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 19 déc. 2011, 14:12

    weight <- as.vector(tapply(obs[ , "weight"],
                               as.list(obs[ , factors]),
                               function(x)
                           {
                               ifelse(all(is.na(x)),
                                      as.numeric(NA),
                                      sum(x, na.rm=TRUE))
                           }))

    ## Cohérence des zéros avec la calculabilité des poids par espèce :
    if (is.element("species.code", factors))
    {
        ## Certains NAs correspondent à des vrai zéros :
        if (!all(is.na(obs[ , "weight"])))
        {
            weight[is.na(weight) & Data[ , nbName] == 0] <- 0
        }

        ## Especes pour lesquelles aucune biomasse n'est calculée.
        noWeightSp <- tapply(weight, Data[ , "species.code"],
                             function(x)all(is.na(x) | x == 0))

        noWeightSp <- names(noWeightSp)[noWeightSp]

        ## Données non disponibles
        ## (évite d'avoir des zéros pour des espèces pour lesquelles le poids ne peut être estimé) :
        weight[is.element(Data[ , "species.code"], noWeightSp)] <- NA
    }else{}

    return(weight)
}

########################################################################################################################
calc.meanWeight.f <- function(Data,
                              nbName="number")
{
    ## Purpose: Calcul des poids moyens (d'individus).
    ## ----------------------------------------------------------------------
    ## Arguments: Data : la table de métrique (temporaire).
    ##            nbName : nom de la colonne nombre.
    ##
    ## Output: vecteur des poids moyens.
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 19 déc. 2011, 14:17

    if ( ! all(is.na(Data[ , "weight"])))
    {
        return(apply(Data[ , c(nbName, "weight")], 1,
                     function(x)
                 {
                     ## Le poids moyen par individu est le poids total / le nombre d'individus :
                     return(as.vector(ifelse(is.na(x[2]) || x[1] == 0,
                                             as.numeric(NA),
                                             x[2]/x[1])))
                 }))
    }else{
        return(NA)
    }
}

########################################################################################################################
calc.density.f <- function(Data, unitobs)
{
    ## Purpose: Calcul généric des densités (également utilisable pour des
    ##          CPUE).
    ## ----------------------------------------------------------------------
    ## Arguments: Data : la table de métrique (temporaire).
    ##            unitobs : table des unités d'observation (data.frame).
    ##
    ## Output: vecteur des densités.
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 19 déc. 2011, 15:47

    ## Traitement spécial si la fraction est un facteur (on doit dabord passer par la classe "character") :
    if (is.factor(unitobs[ , "sampling.rate"]))
    {
        unitobs[ , "sampling.rate"] <- as.numeric(as.character(unitobs[ , "sampling.rate"]))
    }else{}

    ## Calculs :
    return(Data[ , "number"] /
           ## Surface/unité d'effort :
           (unitobs[(idx <- match(Data[ , "observation.unit"],
                                  unitobs[ , "observation.unit"])) ,
                    "obs.dim1"] *
            unitobs[idx, "obs.dim2"] *
            ## ...qui tient compte de la fraction échantillonnée :
            ifelse(is.na(as.numeric(unitobs[idx , "sampling.rate"])),
                   1,
                   as.numeric(unitobs[idx , "sampling.rate"]))))
}

########################################################################################################################
calc.biomass.f <- function(Data, unitobs)
{
    ## Purpose: Calcul généric des biomasses (également utilisable pour des
    ##          CPUE.biomass).
    ## ----------------------------------------------------------------------
    ## Arguments: Data : la table de métrique (temporaire).
    ##            unitobs : table des unités d'observation (data.frame).
    ##
    ## Output: vecteur des biomasses.
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 20 déc. 2011, 11:05

    if (!all(is.na(Data[ , "weight"])))      # (length(unique(obs$biomass))>1)
    {
        ## ##################################################
        ## poids :
        weight <- Data[ , "weight"]

        ## Cohérence des zéros avec la calculabilité des poids par espèce (en principe inutile car fait précédemment sur
        ## le poids mais mis en sécurité si autre utilisation que l'originale) :
        if (is.element("species.code", colnames(Data)))
        {
            ## Especes pour lesquelles aucune biomasse n'est calculée.
            noBiomSp <- tapply(weight, Data[ , "species.code"],
                               function(x)all(is.na(x) | x == 0))

            noBiomSp <- names(noBiomSp)[noBiomSp]

            ## Données non disponibles
            ## (évite d'avoir des zéros pour des espèces pour lesquelles le poids ne peut être estimé) :
            weight[is.element(Data[ , "species.code"], noBiomSp)] <- NA
        }else{}

        ## Poids / surface ou unité d'effort :
        return(as.numeric(weight) /
               ## Surface/unité d'effort :
               (unitobs[(idx <- match(Data[ , "observation.unit"],
                                      unitobs[ , "observation.unit"])) ,
                        "obs.dim1"] *
                unitobs[idx, "obs.dim2"] *
                ## ...qui tient compte de la fraction échantillonnée :
                ifelse(is.na(as.numeric(unitobs[idx , "sampling.rate"])),
                       1,
                       as.numeric(unitobs[idx , "sampling.rate"]))))
    }else{
        ## alerte que les calculs de biomasse sont impossibles
        infoLoading.f(msg=paste(mltext("calc.biomass.info.1"),
                                mltext("calc.biomass.info.2"), sep=""),
                      icon="warning")

        return(NA)
    }
}

########################################################################################################################
calc.presAbs.f <- function(Data,
                           nbName="number")
{
    ## Purpose: Calcul des présences/absences à partir des abondances.
    ## ----------------------------------------------------------------------
    ## Arguments: Data : la table de métrique (temporaire).
    ##            nbName : nom de la colonne nombre.
    ##
    ## Output: vecteur des présences/absences (0 ou 1).
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 20 déc. 2011, 12:04

    ## Presence - absence :
    presAbs <- integer(nrow(Data))
    presAbs[Data[ , nbName] > 0] <- as.integer(1) # pour avoir la richesse spécifique en 'integer'.1
    presAbs[Data[ , nbName] == 0] <- as.integer(0) # pour avoir la richesse spécifique en 'integer'.0

    return(presAbs)
}

########################################################################################################################
unitSpSz.propAb.f <- function(unitSpSz, factors)
{
    ## Purpose: Calcul des proportions d'abondance par classe de taille (au
    ##          sein des croisements d'autres facteurs que "size.class",
    ##          i.e. par unité d'observation par espèce en général).
    ## ----------------------------------------------------------------------
    ## Arguments: Data : la table de métrique (temporaire).
    ##            factors : les facteurs d'agrégation.
    ##
    ## Output: vecteur des proportions d'abondance.
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 20 déc. 2011, 12:11

    ## ##################################################
    ## Proportion d'abondance par classe de taille :

    ## Mise sous forme de tableau multi-dimensionel :
    abondance <- tapply(unitSpSz$density,
                        as.list(unitSpSz[ , factors]),
                        function(x){x}) # -> tableau à 3D.

    ## Autres facteurs que "size.class" :
    factRed <- which(!is.element(factors, "size.class"))

    ## Sommes d'abondances pour chaque unitobs pour chaque espèce :
    sumsCT <- apply(abondance, factRed, sum, na.rm=TRUE)

    ## Calcul des proportions d'abondance -> tableau 3D :
    propAbondance <- sweep(abondance, factRed, sumsCT, FUN="/")
    names(dimnames(propAbondance)) <- factors

    ## Extraction des résultats et remise en ordre :
    tmp <- as.data.frame(as.table(propAbondance),
                         responseName="prop.abondance.SC",
                         stringsAsFactors=FALSE)

    row.names(tmp) <- apply(tmp[ , factors], 1, paste, collapse=":")

    ## Même ordre que unitSpSz :
    tmp <- tmp[apply(unitSpSz[ , factors], 1, paste, collapse=":") , ]

    ## Mise au format colonne + % : ordre [OK]  [yr: 30/10/2012]
    return(100 * tmp$prop.abondance.SC)
}

########################################################################################################################
unitSpSz.propBiom.f <- function(unitSpSz, factors)
{
    ## Purpose: Calcul des proportions de biomasse par classe de taille (au
    ##          sein des croisements d'autres facteurs que "size.class",
    ##          i.e. par unité d'observation par espèce en général).
    ## ----------------------------------------------------------------------
    ## Arguments: Data : la table de métrique (temporaire).
    ##            factors : les facteurs d'agrégation.
    ##
    ## Output: vecteur des proportions de biomasse.
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 20 déc. 2011, 12:17

    if (!is.null(unitSpSz[ , "biomass"]))
    {
        biomasses <- tapply(unitSpSz$biomass,
                            as.list(unitSpSz[ , factors]),
                            function(x){x}) # -> tableau à 3D.

        ## Autres facteurs que "size.class" :
        factRed <- which(!is.element(factors, "size.class"))

        ## Sommes de biomasses pour chaque unitobs pour chaque espèce :
        sumsCT <- apply(biomasses, factRed, sum, na.rm=TRUE)

        ## Calcul des proportions de biomasse -> tableau 3D :
        propBiomass <- sweep(biomasses, factRed, sumsCT, FUN="/")
        names(dimnames(propBiomass)) <- factors

        ## Extraction des résultats et remise en ordre :
        tmp <- as.data.frame(as.table(propBiomass),
                             responseName="prop.biomass.SC",
                             stringsAsFactors=FALSE)

        row.names(tmp) <- apply(tmp[ , factors], 1, paste, collapse=":")

        ## Même ordre que unitSpSz :
        tmp <- tmp[apply(unitSpSz[ , factors], 1, paste, collapse=":") , ]

        ## Mise au format colonne + % : ordre [OK]  [yr: 30/10/2012]
        return(100 * tmp$prop.biomass.SC)
    }else{
        return(NULL)
    }
}

########################################################################################################################
########################################################################################################################
calc.tables.Transect.f <- function(obs, unitobs, dataEnv,
                                   factors=c("observation.unit", "species.code", "size.class"))
{
    ## Purpose: Calcul de tables de métriques pour les transects par défaut.
    ## ----------------------------------------------------------------------
    ## Arguments: obs : table des observations (data.frame).
    ##            unitobs : table des unités d'observation (data.frame).
    ##            dataEnv : environnement des données.
    ##            factors : les facteurs d'agrégation.
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 19 déc. 2011, 12:18

    ## Calcul des nombres par cl / espèces / unitobs :
    nbr <- calcNumber.default.f(obs, factors=factors)

    ## Création de la data.frame de résultats (avec nombres, unitobs, ):
    res <- calc.numbers.f(nbr)

    ## Tailles moyennes :
    res[ , "mean.length"] <- calc.meanSize.f(obs, factors=factors)

    ## Poids :
    res[ , "weight"] <- calc.weight.f(obs=obs, Data=res, factors=factors)

    ## Poids moyen par individu :
    res[ , "mean.weight"] <- calc.meanWeight.f(Data=res)

    ## Densité :
    res[ , "density"] <- calc.density.f(Data=res, unitobs=unitobs)

    ## Biomasse :
    res[ , "biomass"] <- calc.biomass.f(Data=res, unitobs=unitobs)

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
calc.tables.Fishing.f <- function(obs, unitobs, dataEnv,
                                  factors=c("observation.unit", "species.code", "size.class"))
{
    ## Purpose: Calcul des métriques par unité d'observation, par espèce et
    ##          par classe de taille pour la pêche (seules des noms de
    ##          colonnes changent par rapport aux transects par defaut).
    ## ----------------------------------------------------------------------
    ## Arguments: obs : table des observations (data.frame).
    ##            unitobs : table des unités d'observation (data.frame).
    ##            dataEnv : environnement des données.
    ##            factors : les facteurs d'agrégation.
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 21 déc. 2011, 13:20

    res <- calc.tables.Transect.f(obs=obs, unitobs=unitobs, dataEnv=dataEnv, factors=factors)

    ## Renommage des colonnes de densité et biomasse :
    res[ , "CPUE"] <- res$density
    res$density <- NULL
    res[ , "CPUE.biomass"] <- res$biomass # Fonctionne même si biomasse n'existe pas.
    res$biomass <- NULL

    return(res)
}

########################################################################################################################
calc.tables.OBSIND.f <- function(obs, unitobs, dataEnv,
                                 factors=c("observation.unit", "species.code", "size.class"))
{
    ## Purpose: Calcul des métriques par unité d'observation, par espèce et
    ##          par classe de taille pour les observations individuelles
    ##          géoréférencées.
    ## ----------------------------------------------------------------------
    ## Arguments: obs : table des observations (data.frame).
    ##            unitobs : table des unités d'observation (data.frame).
    ##            dataEnv : environnement des données.
    ##            factors : les facteurs d'agrégation.
    ## ----------------------------------------------------------------------
    ## Arguments:
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 25 avril 2013, 18:41

    res <- calc.tables.Transect.f(obs=obs, unitobs=unitobs, dataEnv=dataEnv, factors=factors)

    ## Renommage des colonnes de densité et biomasse :
    res[ , "Density"] <- res$density
    res$density <- NULL
    res[ , "Biomass"] <- res$biomass # Fonctionne même si biomasse n'existe pas.
    res$biomass <- NULL

    return(res)
}



########################################################################################################################
calc.unitSpSz.f <- function(obs, unitobs, refesp, dataEnv)
{
    ## Purpose: Calcul de la table de métriques par  unité d'observation par
    ##          espèce par classe de taille.
    ## ----------------------------------------------------------------------
    ## Arguments: obs : table des observations (data.frame).
    ##            unitobs : table des unités d'observation (data.frame).
    ##            refesp : référentiel espèces.
    ##            dataEnv : environnement des données.
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 16 déc. 2011, 11:51

    runLog.f(msg=c(mltext("logmsg.calc.unitSpSz")))

    pampaProfilingStart.f()

    ## Informations :
    stepInnerProgressBar.f(n=2, msg=mltext("calc.unitSpSz.info.1"))

    ## Définition des types d'observation nécessitant les mêmes méthodes de calcul :
    casObsType <- c("SVR"="SVR",
                    "EMB"="Fishing", "DEB"="Fishing", "PSCI"="Fishing", "PecRec"="Fishing",
                    "LIT"="LIT",
                    "PIT"="PIT",
                    "TRUVC"="Transect", "UVC"="Transect",
                    "TRVID"="Transect", "Q"="Transect",
                    "PFUVC"="Fixe",
                    "TRATO"="TTracks",
                    "OBSIND"="OBSIND")

    ## Calcul des métriques selon cas d'observation :
    if ( ! all(is.na(obs[ , "size.class"])))
    {
        factors <- c("observation.unit", "species.code", "size.class")

        unitSpSz <- switch(casObsType[getOption("P.obsType")],
                           "SVR"={
                               options(P.nbName="number") # Nom du champ de nombres.
                               calc.tables.SVR.f(obs=obs, dataEnv=dataEnv, factors=factors)
                           },
                           "Fishing"={
                               options(P.nbName="number") # Nom du champ de nombres.
                               calc.tables.Fishing.f(obs=obs, unitobs=unitobs, dataEnv=dataEnv, factors=factors)
                           },
                           "LIT"={
                               options(P.nbName="colonies") # Nom du champ de nombres.
                               ## Pas calculé par classe de taille
                           },
                           "PIT"={
                               ## Pas calculé par classe de taille
                           },
                           "Transect"={
                               options(P.nbName="number") # Nom du champ de nombres.
                               calc.tables.Transect.f(obs=obs, unitobs=unitobs, dataEnv=dataEnv, factors=factors)
                           },
                           "Fixe"={
                               ## calc.unitSpSz.Fixe.f()
                           },
                           ## Traces de tortues :
                           "TTracks"={
                               options(P.nbName="tracks.number") # Nom du champ de nombres.
                               calc.tables.TurtleTracks.f(obs=obs, unitobs=unitobs, dataEnv=dataEnv, factors=factors)
                           },
                           ## OBSIND :
                           "OBSIND"={
                               options(P.nbName="number") # Nom du champ de nombres.
                               calc.tables.OBSIND.f(obs=obs, unitobs=unitobs, dataEnv=dataEnv, factors=factors)
                           },
                       {
                           stop(mltext("calc.unitSpSz.err.1"), getOption("P.obsType"),
                                mltext("calc.unitSpSz.err.2"))
                           NULL
                       })
    }else{
        unitSpSz <- data.frame("observation.unit"=NULL, "species.code"=NULL, "number"=NULL,
                               "weight"=NULL, "mean.weight"=NULL, "density"=NULL,
                               "pres.abs"=NULL, "site"=NULL, "biotop"=NULL,
                               "year"=NULL, "protection.status"=NULL)
    }

    pampaProfilingEnd.f()

    return(unitSpSz)
}

########################################################################################################################
calc.unitSp.default.f <- function(unitSpSz, dataEnv=.GlobalEnv)
{
    ## Purpose: Calcul de la table de métriques par unité d'observation par
    ##          espèce, cas général (à l'aide d'agrégations).
    ## ----------------------------------------------------------------------
    ## Arguments: unitSpSz : table de métriques par unité d'observation, par
    ##                       espèce par classe de taille.
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 20 déc. 2011, 15:31

    metrics <- c("number", "mean.length", "weight", "mean.weight", "density", "biomass",
                 "pres.abs",
                 "CPUE", "CPUE.biomass",
                 "colonies", "coverage", "mean.size.colonies")

    usedMetrics <- metrics[is.element(metrics, colnames(unitSpSz))]

    return(agregations.generic.f(Data=unitSpSz,
                                 metrics=usedMetrics,
                                 factors=c("observation.unit", "species.code"),
                                 listFact = NULL,
                                 unitSpSz = unitSpSz,
                                 unitSp = NULL,
                                 dataEnv=dataEnv))
}

########################################################################################################################
calc.unitSp.f <- function(unitSpSz, obs, unitobs, dataEnv)
{
    ## Purpose: Calcul de la table de métriques par unité d'observation par
    ##          espèce (choix de la méthode adéquate en fonction du
    ##          protocole).
    ## ----------------------------------------------------------------------
    ## Arguments: unitSpSz : table de métriques par unité d'observation, par
    ##                       espèce par classe de taille.
    ##            obs : table des observations (data.frame).
    ##            unitobs : table des unités d'observation (data.frame).
    ##            dataEnv : environnement des données.
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 21 déc. 2011, 10:04

    ## Informations :
    stepInnerProgressBar.f(n=1, msg=mltext("calc.unitSp.info.1"))

    pampaProfilingStart.f()

    if (FALSE) ## ! is.null(unitSpSz) && nrow(unitSpSz))
    {
        ## Note : désactivé car plus long que de recalculer la table.
        ##        Conservé car pourrait redevenir intéressant avec de la parallelisation.

        unitSp <- switch(getOption("P.obsType"),
                         SVR=calc.unitSp.SVR.f(unitSpSz=unitSpSz, obs=obs, dataEnv=dataEnv),
                         LIT={
                             warning(paste("Trying to calculate metrics by size class",
                                           " for the benthos...!", sep=""))

                             calc.unitSp.LIT.f(obs=obs, unitobs=unitobs, dataEnv=dataEnv)
                         },
                         calc.unitSp.default.f(unitSpSz=unitSpSz, dataEnv=dataEnv))
    }else{
        factors <- c("observation.unit", "species.code")

        casObsType <- c("SVR"="SVR",
                        "EMB"="Fishing", "DEB"="Fishing", "PSCI"="Fishing", "PecRec"="Fishing",
                        "LIT"="LIT",
                        "PIT"="PIT",
                        "TRUVC"="Transect", "UVC"="Transect",
                        "TRVID"="Transect", "Q"="Transect",
                        "PFUVC"="Fixe",
                        "TRATO"="TTracks",
                        "OBSIND"="OBSIND")

        unitSp <- switch(casObsType[getOption("P.obsType")],
                         "SVR"={
                             options(P.nbName="number") # Nom du champ de nombres.
                             calc.tables.SVR.f(obs=obs, dataEnv=dataEnv, factors=factors)
                         },
                         "Fishing"={
                             options(P.nbName="number") # Nom du champ de nombres.
                             calc.tables.Fishing.f(obs=obs, unitobs=unitobs, dataEnv=dataEnv, factors=factors)
                         },
                         "LIT"={
                             options(P.nbName="colonies") # Nom du champ de nombres.
                             calc.unitSp.LIT.f(obs=obs, unitobs=unitobs, dataEnv=dataEnv)
                         },
                         "PIT"={
                             warning("PIT protocol not implemented yet!")
                             ## calc.unitSpSz.PIT.f()
                         },
                         "Transect"={
                             options(P.nbName="number") # Nom du champ de nombres.
                             calc.tables.Transect.f(obs=obs, unitobs=unitobs, dataEnv=dataEnv, factors=factors)
                         },
                         "Fixe"={
                             warning("\"Fix point\" protocol not implemented yet!")
                             ## calc.unitSpSz.Fixe.f()
                         },
                         ## Traces de tortues :
                         "TTracks"={
                             options(P.nbName="tracks.number") # Nom du champ de nombres.
                             calc.tables.TurtleTracks.f(obs=obs, unitobs=unitobs, dataEnv=dataEnv, factors=factors)
                         },
                           ## OBSIND :
                           "OBSIND"={
                               options(P.nbName="number") # Nom du champ de nombres.
                               calc.tables.OBSIND.f(obs=obs, unitobs=unitobs, dataEnv=dataEnv, factors=factors)
                           },
                     {
                         stop(mltext("calc.unitSp.err.1"), getOption("P.obsType"),
                              mltext("calc.unitSp.err.2"))
                         NULL
                     })
    }

    pampaProfilingEnd.f()

    return(unitSp)
}

########################################################################################################################
calc.unit.default.f <- function(unitSp, refesp, unitobs, colNombres="number", dataEnv=.GlobalEnv,
                                nbName="number")
{
    ## Purpose:
    ## ----------------------------------------------------------------------
    ## Arguments:
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 20 déc. 2011, 17:07

    metrics <- c("number", "mean.length", "weight", "mean.weight", "density", "biomass",
                 "pres.abs",
                 "CPUE", "CPUE.biomass",
                 "colonies", "coverage", "mean.size.colonies",
                 "spawnings", "readable.tracks", "spawning.success",
                 "tracks.number")

    usedMetrics <- metrics[is.element(metrics, colnames(unitSp))]

    if ( ! is.null(unitSp))
    {
        unit <- agregations.generic.f(Data=unitSp,
                                      metrics=usedMetrics,
                                      factors=c("observation.unit"),
                                      listFact = NULL,
                                      unitSpSz = NULL,
                                      unitSp = unitSp,
                                      dataEnv=dataEnv,
                                      nbName=nbName)

        tmp <- do.call(rbind,
                       lapply(unique(as.character(unitobs[ , getOption("P.MPAfield")])),
                              function(MPA)
                          {
                              calcBiodiv.f(Data=subset(unitSp,
                                                       is.element(observation.unit,
                                                                  unitobs[unitobs[ , getOption("P.MPAfield")] == MPA ,
                                                                          "observation.unit"])),
                                           refesp=refesp,
                                           MPA=MPA,
                                           unitobs = "observation.unit",
                                           code.especes = "species.code", nombres = colNombres, indices = "all",
                                           printInfo=TRUE, global=TRUE,
                                           dataEnv=dataEnv)
                          }))

        unit <- merge(unit, tmp[ , colnames(tmp) != colNombres],
                      by.x="observation.unit", by.y="observation.unit")



        return(unit)
    }else{}
}

########################################################################################################################
calc.unit.f <- function(unitSp, obs, refesp, unitobs, dataEnv)
{
    ## Purpose:
    ## ----------------------------------------------------------------------
    ## Arguments:
    ##            obs : table des observations (data.frame).
    ##            unitobs : table des unités d'observation (data.frame).
    ##            refesp : référentiel espèces.
    ##            dataEnv : environnement des données.
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 21 déc. 2011, 10:08

    ## Informations :
    stepInnerProgressBar.f(n=1, msg=mltext("calc.unit.info.1"))

    pampaProfilingStart.f()

    casObsType <- c("SVR"="SVR",
                    "EMB"="Fishing", "DEB"="Fishing", "PSCI"="Fishing", "PecRec"="Fishing",
                    "LIT"="LIT",
                    "PIT"="PIT",
                    "TRUVC"="Transect", "UVC"="Transect",
                    "TRVID"="Transect", "Q"="Transect",
                    "PFUVC"="Fixe",
                    "TRATO"="TTracks")

    ##
    if ( ! is.null(unitSp) && nrow(unitSp))
    {
        unit <- switch(casObsType[getOption("P.obsType")],
                       "SVR"={
                           calc.unit.SVR.f(unitSp=unitSp, obs=obs, refesp=refesp,
                                           unitobs=unitobs, dataEnv=dataEnv, colNombres = "number")
                       },
                       "LIT"={        # Pour les types d'observation qui ont une colonne "colonie" à la place de
                                      # "number" :
                           calc.unit.default.f(unitSp=unitSp, refesp=refesp, unitobs=unitobs, colNombres="colonies",
                                               dataEnv=dataEnv)
                       },

                       "TTracks"={      # Pour les types d'observation qui ont une colonne "tracks.number" à la
                                        # place de "number" :
                           calc.unit.default.f(unitSp=unitSp, refesp=refesp, unitobs=unitobs, colNombres="tracks.number",
                                               dataEnv=dataEnv, nbName="tracks.number")
                       },
                       calc.unit.default.f(unitSp=unitSp, refesp=refesp, unitobs=unitobs, colNombres="number",
                                           dataEnv=dataEnv))
    }else{
        unit <- NULL
    }

    pampaProfilingEnd.f()
    return(unit)
}


########################################################################################################################
########################################################################################################################
calcTables.f <- function(obs, unitobs, refesp, dataEnv)
{
    ## Purpose: Lance le calcul des tables de métriques à divers niveaux
    ##          d'agrégation.
    ## ----------------------------------------------------------------------
    ## Arguments: obs : table des observations (data.frame).
    ##            unitobs : table des unités d'observation (data.frame).
    ##            refesp : référentiel espèces.
    ##            dataEnv : environnement des données.
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 15 déc. 2011, 10:33

    pampaProfilingStart.f()

    runLog.f(msg=c(mltext("logmsg.calcTables")))

    ## Métriques par classe de taille par espèce par unité d'observation :
    unitSpSz <- calc.unitSpSz.f(obs=obs, unitobs=unitobs, refesp=refesp, dataEnv=dataEnv)

    ## Métriques par espèce par unité d'observation :
    unitSp <- calc.unitSp.f(unitSpSz=unitSpSz, obs=obs, unitobs=unitobs, dataEnv=dataEnv)

    ## Métriques par unité d'observation (dont biodiversité) :
    unit <- calc.unit.f(unitSp=unitSp, obs=obs, refesp=refesp, unitobs=unitobs, dataEnv=dataEnv)

    pampaProfilingEnd.f()

    return(list(unitSpSz=unitSpSz, unitSp=unitSp, unit=unit))
}


### Local Variables:
### ispell-local-dictionary: "english"
### fill-column: 120
### End:
