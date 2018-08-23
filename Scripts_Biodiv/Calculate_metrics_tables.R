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
                                 factors=c("unite_observation", "code_espece", "classe_taille"),
                                 nbName="nombre")
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
calc.numbers.f <- function(nbr, nbName="nombre")
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
    ## res$unitobs <- res$unite_observation # Pour compatibilité uniquement !!!

    if (is.element("classe_taille", colnames(res)))
    {
        res$classe_taille[res$classe_taille == ""] <- NA
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
                            factors=c("unite_observation", "code_espece", "classe_taille"),
                            nbName="nombre")
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
                            weighted.mean(obs[ii, "taille"], obs[ii, nbName])
                        })))
}

########################################################################################################################
calc.weight.f <- function(obs, Data,
                          factors=c("unite_observation", "code_espece", "classe_taille"),
                          nbName="nombre")
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

    weight <- as.vector(tapply(obs[ , "poids"],
                               as.list(obs[ , factors]),
                               function(x)
                           {
                               ifelse(all(is.na(x)),
                                      as.numeric(NA),
                                      sum(x, na.rm=TRUE))
                           }))

    ## Cohérence des zéros avec la calculabilité des poids par espèce :
    if (is.element("code_espece", factors))
    {
        ## Certains NAs correspondent à des vrai zéros :
        if (!all(is.na(obs[ , "poids"])))
        {
            weight[is.na(weight) & Data[ , nbName] == 0] <- 0
        }

        ## Especes pour lesquelles aucune biomasse n'est calculée.
        noWeightSp <- tapply(weight, Data[ , "code_espece"],
                             function(x)all(is.na(x) | x == 0))

        noWeightSp <- names(noWeightSp)[noWeightSp]

        ## Données non disponibles
        ## (évite d'avoir des zéros pour des espèces pour lesquelles le poids ne peut être estimé) :
        weight[is.element(Data[ , "code_espece"], noWeightSp)] <- NA
    }else{}

    return(weight)
}

########################################################################################################################
calc.meanWeight.f <- function(Data,
                              nbName="nombre")
{
    ## Purpose: Calcul des poids moyens (d'individus).
    ## ----------------------------------------------------------------------
    ## Arguments: Data : la table de métrique (temporaire).
    ##            nbName : nom de la colonne nombre.
    ##
    ## Output: vecteur des poids moyens.
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 19 déc. 2011, 14:17

    if ( ! all(is.na(Data[ , "poids"])))
    {
        return(apply(Data[ , c(nbName, "poids")], 1,
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
    if (is.factor(unitobs[ , "fraction_echantillonnee"]))
    {
        unitobs[ , "fraction_echantillonnee"] <- as.numeric(as.character(unitobs[ , "fraction_echantillonnee"]))
    }else{}

    ## Calculs :
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
    ## Purpose: Calcul généric des biomasses (également utilisable pour des
    ##          CPUEbiomasse).
    ## ----------------------------------------------------------------------
    ## Arguments: Data : la table de métrique (temporaire).
    ##            unitobs : table des unités d'observation (data.frame).
    ##
    ## Output: vecteur des biomasses.
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 20 déc. 2011, 11:05

    if (!all(is.na(Data[ , "poids"])))      # (length(unique(obs$biomasse))>1)
    {
        ## ##################################################
        ## poids :
        weight <- Data[ , "poids"]

        ## Cohérence des zéros avec la calculabilité des poids par espèce (en principe inutile car fait précédemment sur
        ## le poids mais mis en sécurité si autre utilisation que l'originale) :
        if (is.element("code_espece", colnames(Data)))
        {
            ## Especes pour lesquelles aucune biomasse n'est calculée.
            noBiomSp <- tapply(weight, Data[ , "code_espece"],
                               function(x)all(is.na(x) | x == 0))

            noBiomSp <- names(noBiomSp)[noBiomSp]

            ## Données non disponibles
            ## (évite d'avoir des zéros pour des espèces pour lesquelles le poids ne peut être estimé) :
            weight[is.element(Data[ , "code_espece"], noBiomSp)] <- NA
        }else{}

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
        infoLoading.f(msg=paste(mltext("calc.biomass.info.1"),
                                mltext("calc.biomass.info.2"), sep=""),
                      icon="warning")

        return(NA)
    }
}

########################################################################################################################
calc.presAbs.f <- function(Data,
                           nbName="nombre")
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
    ##          sein des croisements d'autres facteurs que "classe_taille",
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
    abondance <- tapply(unitSpSz$densite,
                        as.list(unitSpSz[ , factors]),
                        function(x){x}) # -> tableau à 3D.

    ## Autres facteurs que "classe_taille" :
    factRed <- which(!is.element(factors, "classe_taille"))

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
    ##          sein des croisements d'autres facteurs que "classe_taille",
    ##          i.e. par unité d'observation par espèce en général).
    ## ----------------------------------------------------------------------
    ## Arguments: Data : la table de métrique (temporaire).
    ##            factors : les facteurs d'agrégation.
    ##
    ## Output: vecteur des proportions de biomasse.
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 20 déc. 2011, 12:17

    if (!is.null(unitSpSz[ , "biomasse"]))
    {
        biomasses <- tapply(unitSpSz$biomasse,
                            as.list(unitSpSz[ , factors]),
                            function(x){x}) # -> tableau à 3D.

        ## Autres facteurs que "classe_taille" :
        factRed <- which(!is.element(factors, "classe_taille"))

        ## Sommes de biomasses pour chaque unitobs pour chaque espèce :
        sumsCT <- apply(biomasses, factRed, sum, na.rm=TRUE)

        ## Calcul des proportions de biomasse -> tableau 3D :
        propBiomass <- sweep(biomasses, factRed, sumsCT, FUN="/")
        names(dimnames(propBiomass)) <- factors

        ## Extraction des résultats et remise en ordre :
        tmp <- as.data.frame(as.table(propBiomass),
                             responseName="prop.biomasse.SC",
                             stringsAsFactors=FALSE)

        row.names(tmp) <- apply(tmp[ , factors], 1, paste, collapse=":")

        ## Même ordre que unitSpSz :
        tmp <- tmp[apply(unitSpSz[ , factors], 1, paste, collapse=":") , ]

        ## Mise au format colonne + % : ordre [OK]  [yr: 30/10/2012]
        return(100 * tmp$prop.biomasse.SC)
    }else{
        return(NULL)
    }
}

########################################################################################################################
########################################################################################################################
calc.tables.Transect.f <- function(obs, unitobs, dataEnv,
                                   factors=c("unite_observation", "code_espece", "classe_taille"))
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
        res[ , "prop.abondance.CL"] <- unitSpSz.propAb.f(unitSpSz=res,
                                                         factors=factors)

        ## Proportions de biomasse par classe de taille :
        res[ , "prop.biomasse.CL"] <- unitSpSz.propBiom.f(unitSpSz=res,
                                                          factors=factors)
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
    ## Arguments: obs : table des observations (data.frame).
    ##            unitobs : table des unités d'observation (data.frame).
    ##            dataEnv : environnement des données.
    ##            factors : les facteurs d'agrégation.
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
calc.tables.OBSIND.f <- function(obs, unitobs, dataEnv,
                                 factors=c("unite_observation", "code_espece", "classe_taille"))
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
    res[ , "Densite"] <- res$densite
    res$densite <- NULL
    res[ , "Biomasse"] <- res$biomasse # Fonctionne même si biomasse n'existe pas.
    res$biomasse <- NULL

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

    runLog.f(msg=c("Calcul des métriques par unité d'observation, espèce et classe de taille :"))

    pampaProfilingStart.f()

    ## Informations :
    stepInnerProgressBar.f(n=2, msg="Calcul des métriques par unité d'observation, espèce et classe de taille...")

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
    if ( ! all(is.na(obs[ , "classe_taille"])))
    {
        factors <- c("unite_observation", "code_espece", "classe_taille")

        unitSpSz <- switch(casObsType[getOption("P.obsType")],
                           "SVR"={
                               options(P.nbName="nombre") # Nom du champ de nombres.
                               calc.tables.SVR.f(obs=obs, dataEnv=dataEnv, factors=factors)
                           },
                           "Fishing"={
                               options(P.nbName="nombre") # Nom du champ de nombres.
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
                               options(P.nbName="nombre") # Nom du champ de nombres.
                               calc.tables.Transect.f(obs=obs, unitobs=unitobs, dataEnv=dataEnv, factors=factors)
                           },
                           "Fixe"={
                               ## calc.unitSpSz.Fixe.f()
                           },
                           ## Traces de tortues :
                           "TTracks"={
                               options(P.nbName="nombre.traces") # Nom du champ de nombres.
                               calc.tables.TurtleTracks.f(obs=obs, unitobs=unitobs, dataEnv=dataEnv, factors=factors)
                           },
                           ## OBSIND :
                           "OBSIND"={
                               options(P.nbName="nombre") # Nom du champ de nombres.
                               calc.tables.OBSIND.f(obs=obs, unitobs=unitobs, dataEnv=dataEnv, factors=factors)
                           },
                       {
                           stop(mltext("calc.unitSpSz.err.1"), getOption("P.obsType"),
                                mltext("calc.unitSpSz.err.2"))
                           NULL
                       })
    }else{
        unitSpSz <- data.frame("unite_observation"=NULL, "code_espece"=NULL, "nombre"=NULL,
                               "poids"=NULL, "poids_moyen"=NULL, "densite"=NULL,
                               "pres_abs"=NULL, "site"=NULL, "biotope"=NULL,
                               "an"=NULL, "statut_protection"=NULL)
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
    stepInnerProgressBar.f(n=1, msg="Calcul des métriques par unité d'observation et espèce...")

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
        factors <- c("unite_observation", "code_espece")

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
                             options(P.nbName="nombre") # Nom du champ de nombres.
                             calc.tables.SVR.f(obs=obs, dataEnv=dataEnv, factors=factors)
                         },
                         "Fishing"={
                             options(P.nbName="nombre") # Nom du champ de nombres.
                             calc.tables.Fishing.f(obs=obs, unitobs=unitobs, dataEnv=dataEnv, factors=factors)
                         },
                         "LIT"={
                             options(P.nbName="colonie") # Nom du champ de nombres.
                             calc.unitSp.LIT.f(obs=obs, unitobs=unitobs, dataEnv=dataEnv)
                         },
                         "PIT"={
                             warning("PIT protocol not implemented yet!")
                             ## calc.unitSpSz.PIT.f()
                         },
                         "Transect"={
                             options(P.nbName="nombre") # Nom du champ de nombres.
                             calc.tables.Transect.f(obs=obs, unitobs=unitobs, dataEnv=dataEnv, factors=factors)
                         },
                         "Fixe"={
                             warning("\"Fix point\" protocol not implemented yet!")
                             ## calc.unitSpSz.Fixe.f()
                         },
                         ## Traces de tortues :
                         "TTracks"={
                             options(P.nbName="nombre.traces") # Nom du champ de nombres.
                             calc.tables.TurtleTracks.f(obs=obs, unitobs=unitobs, dataEnv=dataEnv, factors=factors)
                         },
                           ## OBSIND :
                           "OBSIND"={
                               options(P.nbName="nombre") # Nom du champ de nombres.
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
calc.unit.default.f <- function(unitSp, refesp, unitobs, colNombres="nombre", dataEnv=.GlobalEnv,
                                nbName="nombre")
{
    ## Purpose:
    ## ----------------------------------------------------------------------
    ## Arguments:
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 20 déc. 2011, 17:07

    metrics <- c("nombre", "taille_moyenne", "poids", "poids_moyen", "densite", "biomasse",
                 "pres_abs",
                 "CPUE", "CPUEbiomasse",
                 "colonie", "recouvrement", "taille.moy.colonies",
                 "pontes", "traces.lisibles", "reussite.ponte",
                 "nombre.traces")

    usedMetrics <- metrics[is.element(metrics, colnames(unitSp))]

    if ( ! is.null(unitSp))
    {
        unit <- agregations.generic.f(Data=unitSp,
                                      metrics=usedMetrics,
                                      factors=c("unite_observation"),
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
                                                       is.element(unite_observation,
                                                                  unitobs[unitobs[ , getOption("P.MPAfield")] == MPA ,
                                                                          "unite_observation"])),
                                           refesp=refesp,
                                           MPA=MPA,
                                           unitobs = "unite_observation",
                                           code.especes = "code_espece", nombres = colNombres, indices = "all",
                                           printInfo=TRUE, global=TRUE,
                                           dataEnv=dataEnv)
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
                                           unitobs=unitobs, dataEnv=dataEnv, colNombres = "nombre")
                       },
                       "LIT"={        # Pour les types d'observation qui ont une colonne "colonie" à la place de
                                      # "nombre" :
                           calc.unit.default.f(unitSp=unitSp, refesp=refesp, unitobs=unitobs, colNombres="colonie",
                                               dataEnv=dataEnv)
                       },

                       "TTracks"={      # Pour les types d'observation qui ont une colonne "nombre.traces" à la
                                        # place de "nombre" :
                           calc.unit.default.f(unitSp=unitSp, refesp=refesp, unitobs=unitobs, colNombres="nombre.traces",
                                               dataEnv=dataEnv, nbName="nombre.traces")
                       },
                       calc.unit.default.f(unitSp=unitSp, refesp=refesp, unitobs=unitobs, colNombres="nombre",
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
