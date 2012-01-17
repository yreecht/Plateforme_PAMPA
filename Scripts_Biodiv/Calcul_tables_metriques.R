#-*- coding: latin-1 -*-

### File: Calcul_tables_metriques.R
### Time-stamp: <2012-01-16 18:47:24 yreecht>
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
                                 factors=c("unite_observation", "code_espece", "classe_taille"))
{
    ## Purpose:
    ## ----------------------------------------------------------------------
    ## Arguments:
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 19 déc. 2011, 13:38

    ## Somme des nombres d'individus :
    nbr <- tapply(obs[ , "nombre"],
                  as.list(obs[ , factors]),
                  sum, na.rm = TRUE)

    ## Absences considérée comme "vrais zéros" :
    nbr[is.na(nbr)] <- 0

    return(nbr)
}

########################################################################################################################
calc.numbers.f <- function(nbr)
{
    ## Purpose:
    ## ----------------------------------------------------------------------
    ## Arguments:
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 19 déc. 2011, 13:46

    res <- as.data.frame(as.table(nbr), responseName="nombre")
    ## res$unitobs <- res$unite_observation # Pour compatibilité uniquement !!!

    if (is.element("classe_taille", colnames(res)))
    {
        res$classe_taille[res$classe_taille == ""] <- NA
    }else{}

    ## Si les nombres sont des entiers, leur redonner la bonne classe :
    if (isTRUE(all.equal(res$nombre, as.integer(res$nombre))))
    {
        res$nombre <- as.integer(res$nombre)
    }else{}

    return(res)
}

########################################################################################################################
calc.meanSize.f <- function(obs,
                            factors=c("unite_observation", "code_espece", "classe_taille"))
{
    ## Purpose:
    ## ----------------------------------------------------------------------
    ## Arguments:
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 19 déc. 2011, 13:51

    return(as.vector(tapply(seq(length.out=nrow(obs)),
                            as.list(obs[ , factors]),
                            function(ii)
                        {
                            weighted.mean(obs[ii, "taille"], obs[ii, "nombre"])
                        })))
}

########################################################################################################################
calc.weight.f <- function(obs, Data,
                              factors=c("unite_observation", "code_espece", "classe_taille"))
{
    ## Purpose:
    ## ----------------------------------------------------------------------
    ## Arguments:
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 19 déc. 2011, 14:12

    weight <- as.vector(tapply(obs[ , "poids"],
                               as.list(obs[ , factors]),
                               function(x)
                           {
                               ifelse(all(is.na(x)),
                                      as.numeric(NA),
                                      sum(x, na.rm=TRUE))
                           }))



    ## Certains NAs correspondent à des vrai zéros :
    if (!all(is.na(obs[ , "poids"])))
    {
        weight[is.na(weight) & Data[ , "nombre"] == 0] <- 0
    }

    ## Especes pour lesquelles aucune biomasse n'est calculée.
    noWeightSp <- tapply(weight, Data[ , "code_espece"],
                       function(x)all(is.na(x) | x == 0))

    noWeightSp <- names(noWeightSp)[noWeightSp]

    ## Données non disponibles
    ## (évite d'avoir des zéros pour des espèces pour lesquelles le poids ne peut être estimé) :
    weight[is.element(Data[ , "code_espece"], noWeightSp)] <- NA

    return(weight)
}

########################################################################################################################
calc.meanWeight.f <- function(Data)
{
    ## Purpose:
    ## ----------------------------------------------------------------------
    ## Arguments:
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 19 déc. 2011, 14:17

    if ( ! all(is.na(Data[ , "poids"])))
    {
        return(apply(Data[ , c("nombre", "poids")], 1,
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
    ## Purpose:
    ## ----------------------------------------------------------------------
    ## Arguments:
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 19 déc. 2011, 15:47

    ## Traitement spécial si la fraction est un facteur (on doit dabord passer par la classe "character") :
    if (is.factor(unitobs[ , "fraction_echantillonnee"]))
    {
        unitobs[ , "fraction_echantillonnee"] <- as.numeric(as.character(unitobs[ , "fraction_echantillonnee"]))
    }else{}

    return(Data[ , "nombre"] /
           ## Surface/unité d'effort :
           (unitobs[(idx <- match(Data[ , "unite_observation"],
                                  unitobs[ , "unite_observation"])) ,
                    "DimObs1"] *
            unitobs[idx, "DimObs2"] *
            ## ...qui tient compte de la fraction échantillonnée :
            ifelse(is.na(as.numeric(unitobs[idx , "fraction_echantillonnee"])),
                   1,
                   as.numeric(unitobs[idx , "fraction_echantillonnee"]))))
}

########################################################################################################################
calc.biomass.f <- function(Data, unitobs)
{
    ## Purpose:
    ## ----------------------------------------------------------------------
    ## Arguments:
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 20 déc. 2011, 11:05

    if (!all(is.na(Data[ , "poids"])))      # (length(unique(obs$biomasse))>1)
    {
        ## ##################################################
        ## poids :

        weight <- Data[ , "poids"]

        ## Especes pour lesquelles aucune biomasse n'est calculée.
        noBiomSp <- tapply(weight, Data[ , "code_espece"],
                           function(x)all(is.na(x) | x == 0))

        noBiomSp <- names(noBiomSp)[noBiomSp]

        ## Données non disponibles
        ## (évite d'avoir des zéros pour des espèces pour lesquelles le poids ne peut être estimé) :
        weight[is.element(Data[ , "code_espece"], noBiomSp)] <- NA

        ## Poids / surface ou unité d'effort :
        return(as.numeric(weight) /
               ## Surface/unité d'effort :
               (unitobs[(idx <- match(Data[ , "unite_observation"],
                                      unitobs[ , "unite_observation"])) ,
                        "DimObs1"] *
                unitobs[idx, "DimObs2"] *
                ## ...qui tient compte de la fraction échantillonnée :
                ifelse(is.na(as.numeric(unitobs[idx , "fraction_echantillonnee"])),
                       1,
                       as.numeric(unitobs[idx , "fraction_echantillonnee"]))))
    }else{
        ## alerte que les calculs de biomasse sont impossibles
        infoLoading.f(msg=paste("Calcul de biomasse impossible : ",
                                "\nles poids ne sont pas renseignés ou ne peuvent être calculés.", sep=""),
                      icon="warning")

        return(NA)
    }
}

########################################################################################################################
calc.presAbs.f <- function(Data)
{
    ## Purpose:
    ## ----------------------------------------------------------------------
    ## Arguments:
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 20 déc. 2011, 12:04

    ## Presence - absence
    presAbs <- integer(nrow(Data))
    presAbs[Data[ , "nombre"] > 0] <- as.integer(1) # pour avoir la richesse spécifique en 'integer'.1
    presAbs[Data[ , "nombre"] == 0] <- as.integer(0) # pour avoir la richesse spécifique en 'integer'.0

    return(presAbs)
}

########################################################################################################################
unitSpSz.propAb.f <- function(unitSpSz)
{
    ## Purpose:
    ## ----------------------------------------------------------------------
    ## Arguments:
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 20 déc. 2011, 12:11

    ## ##################################################
    ## Proportion d'abondance par classe de taille :
    abondance <- with(unitSpSz,
                      tapply(densite, list(unite_observation, code_espece, classe_taille),
                             function(x){x})) # -> tableau à 3D.

    ## Sommes d'abondances pour chaque unitobs pour chaque espèce :
    sumsCT <- apply(abondance, c(1, 2), sum, na.rm=TRUE)

    ## Calcul des proportions d'abondance -> tableau 3D :
    propAbondance <- sweep(abondance, c(1, 2), sumsCT, FUN="/")
    names(dimnames(propAbondance)) <- c("unite_observation", "code_espece", "classe_taille")

    ## Mise au format colonne + % :
    return(100 * as.data.frame(as.table(propAbondance),
                               responseName="prop.abondance.SC",
                               stringsAsFactors=FALSE)$prop.abondance.SC)
}

########################################################################################################################
unitSpSz.propBiom.f <- function(unitSpSz)
{
    ## Purpose:
    ## ----------------------------------------------------------------------
    ## Arguments:
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 20 déc. 2011, 12:17

    if (!is.null(unitSpSz[ , "biomasse"]))
    {
        biomasses <- with(unitSpSz, tapply(biomasse,
                                           list(unite_observation, code_espece, classe_taille),
                                           function(x){x})) # -> tableau à 3D.

        ## Sommes de biomasses pour chaque unitobs pour chaque espèce :
        sumsCT <- apply(biomasses, c(1, 2), sum, na.rm=TRUE)

        ## Calcul des proportions de biomasse -> tableau 3D :
        propBiomass <- sweep(biomasses, c(1, 2), sumsCT, FUN="/")
        names(dimnames(propBiomass)) <- c("unite_observation", "code_espece", "classe_taille")

        ## Mise au format colonne + % :
        return(100 * as.data.frame(as.table(propBiomass),
                                   responseName="prop.biomasse.SC",
                                   stringsAsFactors=FALSE)$prop.biomasse.SC)
    }else{
        return(NULL)
    }
}

########################################################################################################################
########################################################################################################################
calc.tables.Transect.f <- function(obs, unitobs, dataEnv,
                                   factors=c("unite_observation", "code_espece", "classe_taille"))
{
    ## Purpose:
    ## ----------------------------------------------------------------------
    ## Arguments:
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 19 déc. 2011, 12:18

    ## Calcul des nombres par cl / espèces / unitobs :
    nbr <- calcNumber.default.f(obs, factors=factors)

    ## Création de la data.frame de résultats (avec nombres, unitobs, ):
    res <- calc.numbers.f(nbr)

    ## Tailles moyennes :
    res[ , "taille_moyenne"] <- calc.meanSize.f(obs, factors=factors)

    ## Poids :
    res[ , "poids"] <- calc.weight.f(obs=obs, Data=res, factors=factors)

    ## Poids moyen par individu :
    res[ , "poids_moyen"] <- calc.meanWeight.f(Data=res)

    ## Densité :
    res[ , "densite"] <- calc.density.f(Data=res, unitobs=unitobs)

    ## Biomasse :
    res[ , "biomasse"] <- calc.biomass.f(Data=res, unitobs=unitobs)

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
calc.tables.Fishing.f <- function(obs, unitobs, dataEnv,
                                    factors=c("unite_observation", "code_espece", "classe_taille"))
{
    ## Purpose: Calcul des métriques par unité d'observation, par espèce et
    ##          par classe de taille pour la pêche (seules des noms de
    ##          colonnes changent par rapport aux transects par defaut).
    ## ----------------------------------------------------------------------
    ## Arguments:
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 21 déc. 2011, 13:20

    res <- calc.tables.Transect.f(obs=obs, unitobs=unitobs, dataEnv=dataEnv, factors=factors)

    ## Renommage des colonnes de densité et biomasse :
    res[ , "CPUE"] <- res$densite
    res$densite <- NULL
    res[ , "CPUEbiomasse"] <- res$biomasse # Fonctionne même si biomasse n'existe pas.
    res$biomasse <- NULL

    return(res)
}


########################################################################################################################
calc.unitSpSz.f <- function(obs, unitobs, refesp, dataEnv)
{
    ## Purpose:
    ## ----------------------------------------------------------------------
    ## Arguments:
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 16 déc. 2011, 11:51

    runLog.f(msg=c("Calcul des métriques par unité d'observation, espèce et classe de taille :"))

    ## Informations :
    stepInnerProgressBar.f(n=1, msg="Calcul des métriques par unité d'observation, espèce et classe de taille...")

    casObsType <- c("SVR"="SVR",
                    "EMB"="Fishing", "DEB"="Fishing", "PSCI"="Fishing", "PecRec"="Fishing",
                    "LIT"="LIT",
                    "PIT"="PIT",
                    "TRUVC"="Transect", "UVC"="Transect", "TRVID"="Transect",
                    "PFUVC"="Fixe")

    ## Calcul des métriques selon cas d'observation :
    if ( ! all(is.na(obs[ , "classe_taille"])))
    {
        factors <- c("unite_observation", "code_espece", "classe_taille")

        unitSpSz <- switch(casObsType[getOption("P.obsType")],
                           "SVR"={
                               calc.tables.SVR.f(obs=obs, dataEnv=dataEnv, factors=factors)
                           },
                           "Fishing"={
                               calc.tables.Fishing.f(obs=obs, unitobs=unitobs, dataEnv=dataEnv, factors=factors)
                           },
                           "LIT"={
                               ## calc.unitSpSz.LIT.f()
                           },
                           "PIT"={
                               ## calc.unitSpSz.PIT.f()
                           },
                           "Transect"={
                               calc.tables.Transect.f(obs=obs, unitobs=unitobs, dataEnv=dataEnv, factors=factors)
                           },
                           "Fixe"={
                               ## calc.unitSpSz.Fixe.f()
                           },
                       {
                           warning("\n\tType d'observation inconnu pour le calcul des métriques par classe de taille !")
                           NULL
                       })
    }else{
        unitSpSz <- data.frame("unite_observation"=NULL, "code_espece"=NULL, "nombre"=NULL,
                               "poids"=NULL, "poids_moyen"=NULL, "densite"=NULL,
                               "pres_abs"=NULL, "site"=NULL, "biotope"=NULL,
                               "an"=NULL, "statut_protection"=NULL)
    }

    return(unitSpSz)
}

########################################################################################################################
calc.unitSp.default.f <- function(unitSpSz)
{
    ## Purpose:
    ## ----------------------------------------------------------------------
    ## Arguments:
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 20 déc. 2011, 15:31

    metrics <- c("nombre", "taille_moyenne", "poids", "poids_moyen", "densite", "biomasse",
                 "pres_abs",
                 "CPUE", "CPUEbiomasse",
                 "colonie", "recouvrement", "taille.moy.colonies")

    usedMetrics <- metrics[is.element(metrics, colnames(unitSpSz))]

    return(agregations.generic.f(Data=unitSpSz,
                                 metrics=usedMetrics,
                                 factors=c("unite_observation", "code_espece"),
                                 listFact = NULL,
                                 unitSpSz = unitSpSz,
                                 unitSp = NULL))
}

########################################################################################################################
calc.unitSp.f <- function(unitSpSz, obs, unitobs, dataEnv)
{
    ## Purpose:
    ## ----------------------------------------------------------------------
    ## Arguments:
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 21 déc. 2011, 10:04

    ## Informations :
    stepInnerProgressBar.f(n=1, msg="Calcul des métriques par unité d'observation et espèce...")

    if (FALSE) ## ! is.null(unitSpSz) && nrow(unitSpSz))
    {
        ## Note : désactivé car plus long que de recalculer la table.
        ##        Conservé car pourrait redevenir intéressant avec de la parallelisation.

        unitSp <- switch(getOption("P.obsType"),
                         SVR=calc.unitSp.SVR.f(unitSpSz=unitSpSz, obs=obs, dataEnv=dataEnv),
                         LIT={
                             warning(paste("Depuis quand les métriques par classe de taille sont calculées ",
                                           "pour du benthos ?", sep=""))

                             calc.unitSp.LIT.f(obs=obs, unitobs=unitobs, dataEnv=dataEnv)
                         },
                         calc.unitSp.default.f(unitSpSz=unitSpSz))
    }else{
        factors <- c("unite_observation", "code_espece")

        casObsType <- c("SVR"="SVR",
                        "EMB"="Fishing", "DEB"="Fishing", "PSCI"="Fishing", "PecRec"="Fishing",
                        "LIT"="LIT",
                        "PIT"="PIT",
                        "TRUVC"="Transect", "UVC"="Transect", "TRVID"="Transect",
                        "PFUVC"="Fixe")

        unitSp <- switch(casObsType[getOption("P.obsType")],
                         "SVR"={
                             calc.tables.SVR.f(obs=obs, dataEnv=dataEnv, factors=factors)
                         },
                         "Fishing"={
                             calc.tables.Fishing.f(obs=obs, unitobs=unitobs, dataEnv=dataEnv, factors=factors)
                         },
                         "LIT"={
                             calc.unitSp.LIT.f(obs=obs, unitobs=unitobs, dataEnv=dataEnv)
                         },
                         "PIT"={
                             warning("Protocole PIT pas implémenté pour l'instant !")
                             ## calc.unitSpSz.PIT.f()
                         },
                         "Transect"={
                             calc.tables.Transect.f(obs=obs, unitobs=unitobs, dataEnv=dataEnv, factors=factors)
                         },
                         "Fixe"={
                             warning("Protocole \"Point fixe\" pas implémenté pour l'instant !")
                             ## calc.unitSpSz.Fixe.f()
                         },
                     {
                         warning("\n\tType d'observation inconnu pour le calcul des métriques par espèce !")
                         NULL
                     })
    }

    return(unitSp)
}

########################################################################################################################
calc.unit.default.f <- function(unitSp, refesp, unitobs, colNombres="nombre")
{
    ## Purpose:
    ## ----------------------------------------------------------------------
    ## Arguments:
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 20 déc. 2011, 17:07

    metrics <- c("nombre", "taille_moyenne", "poids", "poids_moyen", "densite", "biomasse",
                 "pres_abs",
                 "CPUE", "CPUEbiomasse",
                 "colonie", "recouvrement", "taille.moy.colonies")

    usedMetrics <- metrics[is.element(metrics, colnames(unitSp))]

    if ( ! is.null(unitSp))
    {
        unit <- agregations.generic.f(Data=unitSp,
                                      metrics=usedMetrics,
                                      factors=c("unite_observation"),
                                      listFact = NULL,
                                      unitSpSz = NULL,
                                      unitSp = unitSp)

        tmp <- do.call(rbind,
                       lapply(unique(as.character(unitobs[ , "AMP"])),
                              function(MPA)
                          {
                              calcBiodiv.f(Data=subset(unitSp,
                                                       is.element(unite_observation,
                                                                  unitobs[unitobs[ , "AMP"] == MPA , "unite_observation"])),
                                           refesp=refesp,
                                           MPA=MPA,
                                           unitobs = "unite_observation",
                                           code.especes = "code_espece", nombres = colNombres, indices = "all",
                                           printInfo=TRUE, global=TRUE)
                          }))

        unit <- merge(unit, tmp[ , colnames(tmp) != colNombres],
                      by.x="unite_observation", by.y="unite_observation")



        return(unit)
    }else{}
}

########################################################################################################################
calc.unit.f <- function(unitSp, obs, refesp, unitobs, dataEnv)
{
    ## Purpose:
    ## ----------------------------------------------------------------------
    ## Arguments:
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 21 déc. 2011, 10:08

    ## Informations :
    stepInnerProgressBar.f(n=1, msg="Calcul des métriques par unité d'observation...")

    casObsType <- c("SVR"="SVR",
                    "EMB"="Fishing", "DEB"="Fishing", "PSCI"="Fishing", "PecRec"="Fishing",
                    "LIT"="LIT",
                    "PIT"="PIT",
                    "TRUVC"="Transect", "UVC"="Transect", "TRVID"="Transect",
                    "PFUVC"="Fixe")

    ##
    if ( ! is.null(unitSp) && nrow(unitSp))
    {
        unit <- switch(casObsType[getOption("P.obsType")],
                       "SVR"={
                           calc.unit.SVR.f(unitSp=unitSp, obs=obs, refesp=refesp,
                                           unitobs=unitobs, dataEnv=dataEnv, colNombres = "nombre")
                       },
                       "LIT"={        # Pour les types d'observation qui ont une colonne "colonie" à la place de
                                      # "nombre" :
                           calc.unit.default.f(unitSp=unitSp, refesp=refesp, unitobs=unitobs, colNombres="colonie")
                       },
                       calc.unit.default.f(unitSp=unitSp, refesp=refesp, unitobs=unitobs, colNombres="nombre"))
    }else{
        unit <- NULL
    }

    return(unit)
}


########################################################################################################################
########################################################################################################################
calcTables.f <- function(obs, unitobs, refesp, dataEnv)
{
    ## Purpose:
    ## ----------------------------------------------------------------------
    ## Arguments:
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 15 déc. 2011, 10:33

    pampaProfilingStart.f()

    runLog.f(msg=c("Création des tables de base (calcul de métriques) :"))

    unitSpSz <- calc.unitSpSz.f(obs=obs, unitobs=unitobs, refesp=refesp, dataEnv=dataEnv)

    unitSp <- calc.unitSp.f(unitSpSz=unitSpSz, obs=obs, unitobs=unitobs, dataEnv=dataEnv)

    unit <- calc.unit.f(unitSp=unitSp, obs=obs, refesp=refesp, unitobs=unitobs, dataEnv=dataEnv)

    pampaProfilingEnd.f()

    return(list(unitSpSz=unitSpSz, unitSp=unitSp, unit=unit))
}


### Local Variables:
### ispell-local-dictionary: "english"
### fill-column: 120
### End:
