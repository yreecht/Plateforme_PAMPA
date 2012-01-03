#-*- coding: latin-1 -*-

### File: Agregations_generiques.R
### Time-stamp: <2011-12-20 15:30:08 yreecht>
###
### Author: Yves Reecht
###
####################################################################################################
### Description:
###
###
####################################################################################################

agregation.f <- function(metric, Data, factors,casMetrique)
{
    ## Purpose:
    ## ----------------------------------------------------------------------
    ## Arguments:
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 20 déc. 2011, 14:29

    switch(casMetrique[metric],
           "sum"={
               res <- tapply(Data[ , metric],
                             as.list(Data[ , factors, drop=FALSE]),
                             function(x)
                         {
                             ifelse(all(is.na(x)),
                                    NA,
                                    sum(x, na.rm=TRUE))
                         })
           },
           "w.mean"={
               res <- tapply(1:nrow(Data),
                             as.list(Data[ , factors, drop=FALSE]),
                             function(ii)
                         {
                             ifelse(all(is.na(Data[ii, metric])),
                                    NA,
                                    weighted.mean(Data[ii, metric],
                                                  Data[ii, "nombre"],
                                                  na.rm=TRUE))
                         })
           },
           "w.mean.colonies"={
               res <- tapply(1:nrow(Data),
                             as.list(Data[ , factors, drop=FALSE]),
                             function(ii)
                         {
                             ifelse(all(is.na(Data[ii, metric])),
                                    NA,
                                    weighted.mean(Data[ii, metric],
                                                  Data[ii, "colonie"],
                                                  na.rm=TRUE))
                         })
           },
           "w.mean.prop"={
               res <- tapply(1:nrow(Data),
                             as.list(Data[ , factors, drop=FALSE]),
                             function(ii)
                         {
                             ifelse(all(is.na(Data[ii, metric])) || sum(Data[ii, "nombre.tot"], na.rm=TRUE) == 0,
                                    NA,
                                    ifelse(all(na.omit(Data[ii, metric]) == 0), # Pour ne pas avoir NaN.
                                           0,
                                           (sum(Data[ii, "nombre"][ !is.na(Data[ii, metric])], na.rm=TRUE) /
                                            sum(Data[ii, "nombre.tot"], na.rm=TRUE)) *
                                           ## Correction si la classe de taille n'est pas un facteur d'agrégation
                                           ## (sinon valeur divisée par le nombre de classes présentes) :
                                           ifelse(is.element("classe_taille", factors),
                                                  100,
                                                  100 * length(unique(Data$classe_taille)))))
                         })

           },
           "w.mean.prop.bio"={
               res <- tapply(1:nrow(Data),
                             as.list(Data[ , factors, drop=FALSE]),
                             function(ii)
                         {
                             ifelse(all(is.na(Data[ii, metric])) || sum(Data[ii, "biomasse.tot"], na.rm=TRUE) == 0,
                                    NA,
                                    ifelse(all(na.omit(Data[ii, metric]) == 0), # Pour ne pas avoir NaN.
                                           0,
                                           (sum(Data[ii, "biomasse"][ !is.na(Data[ii, metric])], na.rm=TRUE) /
                                            sum(Data[ii, "biomasse.tot"], na.rm=TRUE)) *
                                           ## Correction si la classe de taille n'est pas un facteur d'agrégation
                                           ## (sinon valeur divisée par le nombre de classes présentes) :
                                           ifelse(is.element("classe_taille", factors),
                                                  100,
                                                  100 * length(unique(Data$classe_taille)))))
                         })

           },
           "pres"={
               res <- tapply(Data[ , metric],
                             as.list(Data[ , factors, drop=FALSE]),
                             function(x)
                         {
                             ifelse(all(is.na(x)), # Cas où il n'y a que des NAs.
                                    NA,
                                    ifelse(any(x > 0, na.rm=TRUE), # Sinon...
                                           1, # ...présence si au moins une observation dans le groupe.
                                           0))
                         })
           },
           stop("Pas implémenté !")
           )

    ## Nom des dimensions
    names(dimnames(res)) <- c(factors)

    ## Transformation vers format long :
    reslong <- as.data.frame(as.table(res), responseName=metric)
    reslong <- reslong[ , c(tail(colnames(reslong), 1), head(colnames(reslong), -1))] # métrique en première.

    return(reslong)
}

agregations.generic.f <- function(Data, metrics, factors, listFact=NULL, unitSpSz=NULL, unitSp=NULL)
{
    ## Purpose: Agréger les données selon un ou plusieurs facteurs.
    ## ----------------------------------------------------------------------
    ## Arguments: Data : Le jeu de données à agréger.
    ##            metrics : la métrique agrégée.
    ##            factors : les facteurs
    ##            listFact : noms des facteurs supplémentaires (agrégés et
    ##                       ajoutés à la table de sortie).
    ## Output : une data.frame agrégée.
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 18 oct. 2010, 15:47

    ## Informations (l'étape peut être longue) :
    ## WinInfo <- agregation.info.f()

    ## traitements selon le type de métrique :
    casMetrique <- c("nombre"="sum",
                     "taille_moyenne"="w.mean",
                     "taille_moy"="w.mean",
                     "biomasse"="sum",
                     "poids"="sum",
                     "poids_moyen"="w.mean",
                     "densite"="sum",
                     "CPUE"="sum",
                     "CPUEbiomasse"="sum",
                     "pres_abs"="pres",
                     "prop.abondance.CL"="w.mean.prop", # Pas bon [!!!]
                     "prop.biomasse.CL"="w.mean.prop.bio",  # Pas bon [!!!]
                     ## Benthos :
                     "colonie"="sum",
                     "recouvrement"="sum",
                     "taille.moy.colonies"="w.mean.colonies",
                     ## SVR (expérimental) :
                     "nombreMax"="sum",
                     "nombreSD"="",
                     "densiteMax"="sum",
                     "biomasseMax"="sum")

    ## Ajout du champ nombre pour le calcul des moyennes pondérées s'il est absent :
    if (any(casMetrique[metrics] == "w.mean" | casMetrique[metrics] == "w.mean.prop"))
    {
        if (is.element("classe_taille", colnames(Data)))
        {
            if (is.null(unitSpSz)) stop("unitSpSz doit être défini")

            Data <- merge(Data,
                          unitSpSz[ , c("code_espece", "unite_observation", "classe_taille", "nombre")],
                          by=c("code_espece", "unite_observation", "classe_taille"),
                          suffixes=c("", ".y"))

            ## Ajout de l'abondance totale /espèce/unité d'observation :
            nbTot <- tapply(unitSpSz$nombre,
                            as.list(unitSpSz[ , c("code_espece", "unite_observation")]),
                            sum, na.rm=TRUE)

            Data <- merge(Data,
                          as.data.frame(as.table(nbTot), responseName="nombre.tot"))
        }else{
            if (is.null(unitSp)) stop("unitSp doit être défini")

            Data <- merge(Data,
                          unitSp[ , c("code_espece", "unite_observation", "nombre")], # [!!!] unitSpSz ?
                          by=c("code_espece", "unite_observation"),
                          suffixes=c("", ".y"))
        }
    }else{}

    ## Ajout du champ biomasse pour les proportions de biomasses par classe de taille :
    if (any(casMetrique[metrics] == "w.mean.prop.bio"))
    {
        if (is.null(unitSpSz)) stop("unitSpSz doit être défini")

        Data <- merge(Data,
                      unitSpSz[ , c("code_espece", "unite_observation", "classe_taille", "biomasse")],
                      by=c("code_espece", "unite_observation", "classe_taille"),
                      suffixes=c("", ".y"))

        ## Ajout de la biomasse totale /espèce/unité d'observation :
        biomTot <- tapply(unitSpSz$biomasse,
                          as.list(unitSpSz[ , c("code_espece", "unite_observation")]),
                          function(x)
                      {
                          ifelse(all(is.na(x)),
                                 NA,
                                 sum(x, na.rm=TRUE))
                      })

        Data <- merge(Data,
                      as.data.frame(as.table(biomTot), responseName="biomasse.tot"))
    }

    ## Ajout du champ colonie pour le calcul des moyennes pondérées s'il est absent :
    if (any(casMetrique[metrics] == "w.mean.colonies" & ! is.element("colonie", colnames(Data))))
    {
        Data$colonie <- unitSp[match(apply(Data[ , c("code_espece", "unite_observation")],
                                           1, paste, collapse="*"),
                                     apply(unitSp[ , c("code_espece", "unite_observation")],
                                           1, paste, collapse="*")), "colonie"]
    }else{}


    ## Agrégation de la métrique selon les facteurs
    ## (Reduce applique récursivement le merge() sur les résultats d'agrégation) :
    reslong <- Reduce(function(x,y) {merge(x,y)},
                      sapply(metrics,
                             agregation.f,
                             Data=Data, factors=factors, casMetrique=casMetrique,
                             simplify=FALSE))

    ## Agrégation et ajout des facteurs supplémentaires :
    if (!is.null(listFact))
    {
        reslong <- cbind(reslong,
                         sapply(Data[ , listFact, drop=FALSE],
                                function(fact)
                            {
                                tapply(fact,
                                       as.list(Data[ , factors, drop=FALSE]),
                                       function(x)
                                   {
                                       if (length(x) > 1 && length(unique(x)) > 1) # On doit n'avoir qu'une seule
                                        # modalité...
                                       {
                                           return(NULL)                  # ...sinon on retourne NULL
                                       }else{
                                           unique(as.character(x))
                                       }
                                   })
                            }))
    }else{}

    ## Si certains facteurs ne sont pas de classe facteur, il faut les remettre dans leur classe d'origine :
    if (any(tmp <- sapply(reslong[ , listFact, drop=FALSE], class) != sapply(Data[ , listFact, drop=FALSE], class)))
    {
        for (i in which(tmp))
        {
            switch(sapply(Data[ , listFact, drop=FALSE], class)[i],
                   "integer"={
                       reslong[ , listFact[i]] <- as.integer(as.character(reslong[ , listFact[i]]))
                   },
                   "numeric"={
                       reslong[ , listFact[i]] <- as.numeric(as.character(reslong[ , listFact[i]]))
                   },
                   reslong[ , listFact[i]] <- eval(call(paste("as", sapply(Data[ , listFact, drop=FALSE], class)[i], sep="."),
                                                        reslong[ , listFact[i]]))
                   )
        }
    }else{}

    ## Rétablir l'ordre initial des nivaux de facteurs :
    reslong <- as.data.frame(sapply(colnames(reslong),
                                    function(x)
                                {
                                    if (is.factor(reslong[ , x]))
                                    {
                                        return(factor(reslong[ , x], levels=levels(Data[ , x])))
                                    }else{
                                        return(reslong[ , x])
                                    }
                                }, simplify=FALSE))


    ## Fermeture de la fenêtre d'information
    ## close.info.f(WinInfo)

    ## Vérification des facteurs supplémentaires agrégés. Il ne doit pas y avoir d'élément nul (la fonction précédente
    ## renvoie NULL si plusieurs niveaux de facteurs, i.e. le facteur est un sous ensemble d'un des facteurs
    ## d'agrégation des observations) :
    if (any(sapply(reslong[ , listFact], function(x){any(is.null(unlist(x)))})))
    {
        warning(paste("Un des facteurs annexes est surement un sous-ensemble",
                      " du(des) facteur(s) de regroupement des observations.", sep=""))
        return(NULL)
    }else{
        return(reslong)
    }
}







### Local Variables:
### ispell-local-dictionary: "english"
### fill-column: 120
### End:
