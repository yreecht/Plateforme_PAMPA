#-*- coding: latin-1 -*-

### File: Selection_variables_fonctions.R
### Time-stamp: <2011-05-09 15:45:08 yreecht>
###
### Author: Yves Reecht
###
####################################################################################################
### Description:
###
### Fonctions pour le système générique de sélection des variables, non spécifiques à l'interface.
####################################################################################################


########################################################################################################################
is.benthos.f <- function()
{
    ## Purpose: Raccourci pour tester s'il s'agit d'un jeu de données
    ##          benthos.
    ##          Remarque : liste des types "benthiques" susceptible
    ##          d'évoluer.
    ## ----------------------------------------------------------------------
    ## Arguments: Aucun
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 17 août 2010, 16:19

    benthTypes <- c("LIT")
    return(all(is.element(unitobs$type, benthTypes)))
}

########################################################################################################################
has.no.pres.abs <- function(nextStep, tableMetrique)
{
    ## Purpose: si les présences/absences doivent ne pas être affichées,
    ##          renvoie "pres_abs", NULL sinon
    ## ----------------------------------------------------------------------
    ## Arguments: nextStep : l'identifiant de l'étape suivante
    ##            tableMetrique : la table de métrique.
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 12 oct. 2010, 09:07
    if (is.element(nextStep, c("boxplot.esp", "boxplot.unitobs")) |     # pas proposé si on fait des boxplots.
        length(unique(na.omit(get(tableMetrique, # "            " une seule modalité.
                                  envir=.GlobalEnv)$pres_abs))) < 2)
    {
        return("pres_abs")              # fonctionnement "inversé" !
    }else{
        return(NULL)
    }
}


########################################################################################################################
champsMetriques.f <- function(nomTable, nextStep)
{
    ## Purpose: Retourne la liste des champs pouvant être utilisés comme
    ##          métriques en fonction du nom de table
    ## ----------------------------------------------------------------------
    ## Arguments: nomTable : nom de la table dans laquelle doivent être
    ##                       cherchées les métriques disponibles (chaîne de
    ##                       caractères).
    ##            nextStep : identifiant de l'étape suivante.
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 11 août 2010, 16:14

    if (!is.character(nomTable))
    {
        stop("'nomTable' doit être une chaîne de caractères !")
    }else{
        switch(nomTable,
               ## Table listespunit (métriques d'observation) :
               listespunit={
                   res <- sort(colnames(listespunit)[sapply(listespunit,
                                                            function(x){is.numeric(x) & !all(is.na(x))}) &
                                                     !is.element(colnames(listespunit),
                                                                 c("an",
                                                                   has.no.pres.abs(nextStep, nomTable),
                                                                   ifelse(is.benthos.f(),
                                                                          "nombre",
                                                                          "")))])
               },
               ## Table unitespta (métriques d'observation par classes de taille) :
               unitespta={
                   res <- sort(colnames(unitespta)[sapply(unitespta,
                                                            function(x){is.numeric(x) & !all(is.na(x))}) &
                                                     !is.element(colnames(unitespta),
                                                                 c("an",
                                                                   has.no.pres.abs(nextStep, nomTable),
                                                                   "longitude", "latitude"))])
               },
               ## Table TabbleBiodiv (indices de biodiversité) :
               TableBiodiv={
                   columns <- c("richesse_specifique", "simpson", "l.simpson", "pielou", "hill",
                                "Delta", "DeltaEtoile", "LambdaPlus", "DeltaPlus",
                                "SDeltaPlus",
                                grep("RS.relative", colnames(eval(parse(text=nomTable))), value=TRUE))

                   res <- sort(colnames(TableBiodiv)[sapply(TableBiodiv,
                                                            function(x){is.numeric(x) & !all(is.na(x))}) &
                                                     is.element(colnames(TableBiodiv), columns)])
               },
               ## Autres cas :
               res <- sort(colnames(eval(parse(text=nomTable)))[sapply(eval(parse(text=nomTable)),
                                                                       function(x){is.numeric(x) & !all(is.na(x))}) &
                                                                colnames(listespunit)!="an"])
               )

        return(res[!is.element(res, c(colnames(especes), colnames(unitobs)))])
    }
}


########################################################################################################################
champsUnitobs.f <- function(ordered=FALSE, tableMetrique="")
{
    ## Purpose: Retourne la liste des champs du référentiel d'unités
    ##          d'observations après avoir supprimé les champs vides.
    ## ----------------------------------------------------------------------
    ## Arguments: ordered : faire apparaître les champs principaux en
    ##                      premiers ? (booléen, optionnel)
    ##            tableMetrique : nom de la table de métriques (pour pouvoir
    ##                            ajouter le champs classe de taille, même
    ##                            si ne fait pas partie de cette table).
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date:  3 août 2010, 11:12

    ## Champs principaux :
    cPrincip <- c(
                  ## table "unitobs" :
                  "site", "an", "annee.campagne", "biotope", "statut_protection", "caracteristique_1",
                  "caracteristique_2"
                  )

    ## Champs non-vides de la table 'unitobs' :
    res <- sort(names(unitobs)[sapply(names(unitobs), function(i){!all(is.na(unitobs[ , i]))})])

    ## Champs principaux en premiers :
    if (ordered)
    {
        res <- c(if (tableMetrique == "unitespta")
                 {
                     c("classe_taille", "")
                 }else{
                     ""
                 },
                 sort(cPrincip[is.element(cPrincip, res)]),
                 "", sort(res[!is.element(res, cPrincip)]))
    }else{
        res <- c(if (tableMetrique == "unitespta")
                 {
                     c("classe_taille", "")
                 }else{
                     ""
                 },
                 res)}

    return(res)
}


########################################################################################################################
champsRefEspeces.f <- function(site, ordered=FALSE, tableMetrique="", nextStep=NA)
{
    ## Purpose: Retourne la liste des champs du référentiel espèces après
    ##          avoir supprimé ceux ne correspondant pas au site étudié ainsi
    ##          que les champs vides.
    ## ----------------------------------------------------------------------
    ## Arguments: site : le site étudié (chaîne de caractères).
    ##            ordered : faire apparaître les champs principaux en
    ##                      premiers ? (booléen, optionnel)
    ##            tableMetrique : nom de la table de métriques (pour pouvoir
    ##                            ajouter le champs classe de taille, même
    ##                            si ne fait pas partie de cette table).
    ##            nextStep : étape suivante.
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date:  3 août 2010, 11:16

    ## Champs principaux (externaliser par la suite) :
    if (is.element(tableMetrique, c("listespunit", "TableOccurrences", "unitespta")) &&
        is.element(nextStep, c("boxplot.esp", "modele_lineaire", "freq_occurrence")))
    {
        cPrincip <- c("code_espece", "espece")
    }else{
        cPrincip <- c(
                      ## table "especes" :
                      "code_espece", "Cat_benthique", "Famille", "Genre", "Identifiant",
                      "CategB_general", "CategB_groupe"
                      )
    }

    ## Externaliser la définition des sites par la suite...
    listeSite <- c("RUN" , "MAY" , "BA" , "BO" , "CB" , "CR" , "STM" , "NC")

    ## Noms des sites dont on doit exclure les colonnes :
    sitesExclus <- listeSite[ ! grepl(pattern=paste("^", SiteEtudie, "$", sep=""), x=listeSite)]

    ## champs ne correspondant pas au motif "(Site1|Site2|...)$" :
    champs <- sort(colnames(especes))[! grepl(paste("(", paste(sitesExclus, collapse="|"), ")$", sep=""),
                                              sort(colnames(especes)))]

    ## Champs non-vides de la table 'espèces' :
    res <- sort(champs[sapply(champs,
                              function(i){!all(is.na(especes[is.element(especes$code_espece,
                                                                        obs$code_espece) , i]))})])

    ## Champs principaux en premiers :
    if (ordered)
    {
        res <- c(if (tableMetrique == "unitespta")
                 {
                     c("classe_taille", "")
                 }else{
                     ""
                 },
                 sort(cPrincip[is.element(cPrincip, res)]),
                 "", sort(res[!is.element(res, cPrincip)]))
    }else{
        res <- c(if (tableMetrique == "unitespta")
                 {
                     c("classe_taille", "")
                 }else{
                     ""
                 },
                 res)
    }

    return(res)
}


########################################################################################################################
champsReferentiels.f <- function(nomTable, nextStep=NA)
{
    ## Purpose: Retourne la liste des facteurs pertinents en fonction de la
    ##          table de métriques retenue.
    ## ----------------------------------------------------------------------
    ## Arguments: nomTable : nom de la table de métriques (chaîne de
    ##                       charactère)
    ##            nextStep : étape suivante, pas obligatoire.
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 12 août 2010, 10:18

    if (!is.character(nomTable))
    {
        stop("'nomTable' doit être une chaîne de caractères !")
    }else{
        ## Champs principaux :
        cPrincip <- c(
                      ## table "unitobs" :
                      "site", "an", "annee.campagne", "biotope", "statut_protection", "caracteristique_1",
                      "caracteristique_2",
                      ## table "especes" :
                      "code_espece", "Cat_benthique", "Famille", "Genre", "Identifiant",
                      "CategB_general", "CategB_groupe"
                      )

        ## Champs des unités d'observation :
        cUnitobs <- champsUnitobs.f()

        ## Champs du référentiel espèces :
        if (length(grep("\\.unitobs$", nextStep)) > 0)
        {
            cEspeces <- NA              # les champs du ref espèce n'ont pas de raison d'apparaitre dans les cas de
                                        # tables agrégées par unitobs.
        }else{
            cEspeces <- champsRefEspeces.f(siteEtudie)
        }

        casTables <- c("listespunit"="listespunit",
                       "TablePresAbs"="listespunit",
                       "TableOccurrences"="listespunit",
                       "unitespta"="unitespta",
                       "TableBiodiv"="TableBiodiv")

        switch(casTables[nomTable],
               TableBiodiv={
                   return(c("", sort(cPrincip[is.element(cPrincip, cUnitobs)]),   # champs principaux...
                            "", sort(cUnitobs[!is.element(cUnitobs, cPrincip)]))) # autres.
               },
               listespunit={
                   return(c("", sort(cPrincip[is.element(cPrincip, c(cUnitobs, cEspeces))]), # champs principaux...
                            "", sort(c(cUnitobs[!is.element(cUnitobs, cPrincip)],            # autres.
                                       cEspeces[!is.element(cEspeces, cPrincip)]))))
               },
               unitespta={
                   return(c("", "classe_taille",
                            "", sort(cPrincip[is.element(cPrincip, c(cUnitobs, cEspeces))]), # champs principaux...
                            "", sort(c(cUnitobs[!is.element(cUnitobs, cPrincip)],            # autres.
                                       cEspeces[!is.element(cEspeces, cPrincip)]))))
               },
               {
                   warning("Table de métrique '", nomTable, "' inconnu")
                   return(c("", sort(cPrincip[is.element(cPrincip, c(cUnitobs, cEspeces))]), # champs principaux...
                            "", sort(c(cUnitobs[!is.element(cUnitobs, cPrincip)],            # autres.
                                       cEspeces[!is.element(cEspeces, cPrincip)]))))
               })

    }
}



########################################################################################################################
subsetToutesTables.f <- function(metrique, facteurs, selections, tableMetrique="",
                                 exclude=NULL, add=c("code_espece", "unite_observation"))
{
    ## Purpose: Extraire les données utiles uniquement, d'après les métrique
    ##          et facteur(s) séléctionnés, ainsi que leur(s) sélection(s) de
    ##          modalité(s).
    ## ----------------------------------------------------------------------
    ## Arguments: metrique : la métrique choisie.
    ##            facteurs : les facteurs sélectionnés (tous)
    ##            selections : les sélections de modalités correspondantes
    ##                         (liste).
    ##            tableMetrique : le nom de la table des métriques.
    ##            exclude : niveau de facteur à ne pas prendre en compte pour
    ##                      le subset.
    ##            add : champs (de la table de métrique) à ajouter aux
    ##                  données.
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date:  6 août 2010, 16:46

    ## Si pas de table de métrique disponible ou déjà calculée
    ## ("TableOcurrences" est calculée à partir de la sélection) :
    if (is.element(tableMetrique, c("", "TableOccurrences", "TablePresAbs")))
    {
        tableMetrique <- "listespunit"
    }else{}

    ## Si pas de métrique disponible ou déjà calculée ("freq.occurrence" est calculée à partir de la sélection) :
    if (is.element(metrique, c("", "freq.occurrence")))
    {
        metrique <- "tmp"
        eval(parse(text=paste(tableMetrique, "$tmp <- 0", sep="")))
        eval(parse(text=paste(tableMetrique, "$tmp[", tableMetrique, "$nombre > 0] <- 1", sep="")))
    }else{}

    casTables <- c("listespunit"="listespunit",
                   "TablePresAbs"="listespunit",
                   "unitespta"="unitespta")

    dataMetrique <- eval(parse(text=tableMetrique))

    if (!is.null(add))
    {
        metriques <- c(metrique, add[is.element(add, colnames(dataMetrique))])
    }else{
        metriques <- metrique
    }

    ## Subset en fonction de la table de métrique
    switch(casTables[tableMetrique],
           ## Cas de la table d'observation ou des tables de présence :
           listespunit={
                restmp <- cbind(dataMetrique[!is.na(dataMetrique[ , metrique]) , metriques, drop=FALSE],
                                unitobs[match(dataMetrique$unite_observation[!is.na(dataMetrique[ , metrique])],
                                              unitobs$unite_observation), # ajout des colonnes sélectionnées d'unitobs
                                        facteurs[is.element(facteurs, colnames(unitobs))], drop=FALSE],
                                especes[match(dataMetrique$code_espece[!is.na(dataMetrique[ , metrique])],
                                              especes$code_espece),        # ajout des colonnes sélectionnées d'especes
                                        facteurs[is.element(facteurs, colnames(especes))], drop=FALSE])
            },
           ## Cas de la table d'observations par classes de taille :
           unitespta={
               restmp <- cbind(dataMetrique[!is.na(dataMetrique[ , metrique]) ,
                                            c(metriques, "classe_taille"), drop=FALSE],
                               unitobs[match(dataMetrique$unite_observation[!is.na(dataMetrique[ , metrique])],
                                             unitobs$unite_observation), # ajout des colonnes sélectionnées d'unitobs
                                       facteurs[is.element(facteurs, colnames(unitobs))], drop=FALSE],
                               especes[match(dataMetrique$code_espece[!is.na(dataMetrique[ , metrique])],
                                             especes$code_espece),        # ajout des colonnes sélectionnées d'especes
                                       facteurs[is.element(facteurs, colnames(especes))], drop=FALSE])
           },
           ## Autres cas :
           restmp <- cbind(dataMetrique[!is.na(dataMetrique[ , metrique]) , metriques, drop=FALSE],
                           unitobs[match(dataMetrique$unite_observation[!is.na(dataMetrique[ , metrique])],
                                         unitobs$unite_observation), # ajout des colonnes sélectionnées d'unitobs.
                                   facteurs[is.element(facteurs, colnames(unitobs))], drop=FALSE])
           )

    selCol <- which(!is.na(selections))
    if (!is.null(exclude))
    {
        selCol <- selCol[selCol != exclude]
    }
    for (i in selCol)
    {
        restmp <- subset(restmp, is.element(restmp[ , facteurs[i]], selections[[i]]))
    }

    ## Traitement particulier des classes de taille (mise en facteur avec ordre défini selon le context) :
    if (is.element("classe_taille", colnames(restmp)))
    {
        if (length(grep("^[[:digit:]]*[-_][[:digit:]]*$", unique(as.character(restmp$classe_taille)), perl=TRUE)) ==
            length(unique(as.character(restmp$classe_taille))))
        {
            restmp$classe_taille <-
                factor(as.character(restmp$classe_taille),
                       levels=unique(as.character(restmp$classe_taille))[
                               order(as.numeric(sub("^([[:digit:]]*)[-_][[:digit:]]*$",
                                                    "\\1",
                                                    unique(as.character(restmp$classe_taille)),
                                                    perl=TRUE)),
                                     na.last=FALSE)])
        }else{
            restmp$classe_taille <- factor(restmp$classe_taille)
        }
    }else{}

    ## Conversion des biomasses et densités -> /100m² :
    if (any(is.element(colnames(restmp), c("biomasse", "densite", "biomasseMax", "densiteMax"))) && !is.peche.f())
    {
        restmp[ , is.element(colnames(restmp),
                             c("biomasse", "densite",
                               "biomasseMax", "densiteMax"))] <- 100 *
                                   restmp[, is.element(colnames(restmp),
                                                       c("biomasse", "densite",
                                                         "biomasseMax", "densiteMax"))]
    }else{}

    return(restmp)
}


########################################################################################################################
agregationTableParCritere.f <- function(Data, metrique, facteurs, listFact=NULL)
{
    ## Purpose: Agréger les données selon un ou plusieurs facteurs.
    ## ----------------------------------------------------------------------
    ## Arguments: Data : Le jeu de données à agréger.
    ##            metrique : la métrique agrégée.
    ##            facteurs : les facteurs
    ##            listFact : noms des facteurs supplémentaires (agrégés et
    ##                       ajoutés à la table de sortie).
    ## Output : une data.frame agrégée.
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 18 oct. 2010, 15:47

    ## Informations (l'étape peut être longue) :
    WinInfo <- agregation.info.f()

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
                     "nombreMax"="sum",
                     "nombreSD"="",
                     "densiteMax"="sum",
                     "biomasseMax"="sum")

    ## Ajout du champs nombre pour le calcul des moyennes pondérées s'il est absent :
    if ((casMetrique[metrique] == "w.mean" || casMetrique[metrique] == "w.mean.prop"))
    {
        if (is.element("classe_taille", colnames(Data)))
        {
            Data <- merge(Data,
                          unitespta[ , c("code_espece", "unite_observation", "classe_taille", "nombre")],
                          by=c("code_espece", "unite_observation", "classe_taille"))

            ## Ajout de l'abondance totale /espèce/unité d'observation :
            nbTot <- tapply(unitespta$nombre,
                            as.list(unitespta[ , c("code_espece", "unite_observation")]),
                            sum, na.rm=TRUE)

            Data <- merge(Data,
                          as.data.frame(as.table(nbTot), responseName="nombre.tot"))
        }else{
            Data <- merge(Data,
                          unitespta[ , c("code_espece", "unite_observation", "nombre")],
                          by=c("code_espece", "unite_observation"))
        }
    }else{}

    ## Ajout du champs biomasse pour les proportions de biomasses par classe de taille :
    if (casMetrique[metrique] == "w.mean.prop.bio")
    {
        Data <- merge(Data,
                      unitespta[ , c("code_espece", "unite_observation", "classe_taille", "biomasse")],
                      by=c("code_espece", "unite_observation", "classe_taille"))

        ## Ajout de la biomasse totale /espèce/unité d'observation :
        biomTot <- tapply(unitespta$biomasse,
                          as.list(unitespta[ , c("code_espece", "unite_observation")]),
                          function(x)
                      {
                          ifelse(all(is.na(x)),
                                 NA,
                                 sum(x, na.rm=TRUE))
                      })

        Data <- merge(Data,
                      as.data.frame(as.table(biomTot), responseName="biomasse.tot"))
    }

    ## Ajout du champs colonie pour le calcul des moyennes pondérées s'il est absent :
    if (casMetrique[metrique] == "w.mean.colonies" && ! is.element("colonie", colnames(Data)))
    {
        Data$colonie <- listespunit$colonie[match(apply(Data[ , c("code_espece", "unite_observation")],
                                                        1, paste, collapse="*"),
                                                  apply(listespunit[ , c("code_espece", "unite_observation")],
                                                        1, paste, collapse="*"))]
    }else{}

    ## Agrégation de la métrique selon les facteurs :
    switch(casMetrique[metrique],
           "sum"={
               res <- tapply(Data[ , metrique],
                             as.list(Data[ , facteurs, drop=FALSE]),
                             function(x)
                         {
                             ifelse(all(is.na(x)),
                                    NA,
                                    sum(x, na.rm=TRUE))
                         })
           },
           "w.mean"={
               res <- tapply(1:nrow(Data),
                             as.list(Data[ , facteurs, drop=FALSE]),
                             function(ii)
                         {
                             ifelse(all(is.na(Data[ii, metrique])),
                                    NA,
                                    weighted.mean(Data[ii, metrique],
                                                  Data[ii, "nombre"],
                                                  na.rm=TRUE))
                         })
           },
           "w.mean.colonies"={
               res <- tapply(1:nrow(Data),
                             as.list(Data[ , facteurs, drop=FALSE]),
                             function(ii)
                         {
                             ifelse(all(is.na(Data[ii, metrique])),
                                    NA,
                                    weighted.mean(Data[ii, metrique],
                                                  Data[ii, "colonie"],
                                                  na.rm=TRUE))
                         })
           },
           "w.mean.prop"={
               res <- tapply(1:nrow(Data),
                             as.list(Data[ , facteurs, drop=FALSE]),
                             function(ii)
                         {
                             ifelse(all(is.na(Data[ii, metrique])) || sum(Data[ii, "nombre.tot"], na.rm=TRUE) == 0,
                                    NA,
                                    ifelse(all(na.omit(Data[ii, metrique]) == 0), # Pour ne pas avoir NaN.
                                           0,
                                           (sum(Data[ii, "nombre"][ !is.na(Data[ii, metrique])], na.rm=TRUE) /
                                             sum(Data[ii, "nombre.tot"], na.rm=TRUE)) *
                                           ## Correction si la classe de taille n'est pas un facteur d'agrégation
                                           ## (sinon valeur divisée par le nombre de classes présentes) :
                                           ifelse(is.element("classe_taille", facteurs),
                                                  100,
                                                  100 * length(unique(Data$classe_taille)))))
                         })

           },
           "w.mean.prop.bio"={
               res <- tapply(1:nrow(Data),
                             as.list(Data[ , facteurs, drop=FALSE]),
                             function(ii)
                         {
                             ifelse(all(is.na(Data[ii, metrique])) || sum(Data[ii, "biomasse.tot"], na.rm=TRUE) == 0,
                                    NA,
                                    ifelse(all(na.omit(Data[ii, metrique]) == 0), # Pour ne pas avoir NaN.
                                           0,
                                           (sum(Data[ii, "biomasse"][ !is.na(Data[ii, metrique])], na.rm=TRUE) /
                                             sum(Data[ii, "biomasse.tot"], na.rm=TRUE)) *
                                           ## Correction si la classe de taille n'est pas un facteur d'agrégation
                                           ## (sinon valeur divisée par le nombre de classes présentes) :
                                           ifelse(is.element("classe_taille", facteurs),
                                                  100,
                                                  100 * length(unique(Data$classe_taille)))))
                         })

           },
           "pres"={
               res <- tapply(Data[ , metrique],
                             as.list(Data[ , facteurs, drop=FALSE]),
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
    names(dimnames(res)) <- c(facteurs)

    ## Transformation vers format long :
    reslong <- as.data.frame(as.table(res), responseName=metrique)
    reslong <- reslong[ , c(tail(colnames(reslong), 1), head(colnames(reslong), -1))] # métrique en première.

    ## Agrégartion et ajout des facteurs supplémentaires :
    if (!is.null(listFact))
    {
        reslong <- cbind(reslong,
                         sapply(Data[ , listFact, drop=FALSE],
                                function(fact)
                            {
                                tapply(fact,
                                       as.list(Data[ , facteurs, drop=FALSE]),
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

    ## Fermeture de la fenêtre d'information
    close.info.f(WinInfo)

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


########################################################################################################################
presAbs.f <- function(nombres, logical=FALSE)
{
    ## Purpose: Renvoie les présences/absences d'après les nombres.
    ## ----------------------------------------------------------------------
    ## Arguments: nombres : vecteur de nombre d'individus.
    ##            logical : faut-il renvoyer les résultats sous forme de
    ##                      booléens, ou 0/1 (booléen).
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 29 oct. 2010, 10:20

    if (any(nombres < 0, na.rm=TRUE))
    {
        stop("effectifs inférieurs à 0 !")
    }else{}

    if (logical)
    {
        return(nombres > 0)
    }else{
        nombres[nombres > 0] <- 1
        return(nombres)
    }
}

########################################################################################################################
calcBiodiv.f <- function(Data, unitobs="unite_observation", code.especes="code_espece", nombres="nombre",
                         indices="all")
{
    ## Purpose: calcul des indices de biodiversité
    ## ----------------------------------------------------------------------
    ## Arguments: Data : les données à partir desquelles calculer les
    ##                   indices. Doivent comporter au minimum (colones) :
    ##                     * unités d'observations/sites
    ##                     * espèces présentes
    ##                     * nombre d'individus /espèce/unitobs.
    ##            unitobs : nom de la colone d'unités d'observation.
    ##            especes : nom de la colone d'espèces.
    ##            nombres : nom de la colone de nombres.
    ##            indices : liste des indices à calculer
    ##                      (vecteur de caractères)
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 29 oct. 2010, 08:58

    ## Suppréssion de tout ce qui n'a pas de genre (peut être du non biotique) :
    Data <- Data[especes$Genre[match(Data$code_espece, especes$code_espece)] != "ge.", ]

    ## Suppression des niveaux de facteur inutilisés :
    Data <- dropLevels.f(df=Data)

    ## Si les données ne sont pas encore agrégées /espèce/unitobs on le fait ici :
    if (nrow(Data) > nrow(expand.grid(unique(Data[ , unitobs]), unique(Data[ , code.especes]))))
    {
        Data <- agregationTableParCritere.f(Data=Data, metrique=nombres,
                                            facteurs=c(unitobs, code.especes),
                                            listFact=NULL)
    }else{}

    df.biodiv <- as.data.frame(as.table(tapply(Data[ , nombres],
                                               Data[ , unitobs],
                                               sum, na.rm=TRUE)))

    colnames(df.biodiv) <- c(unitobs, nombres)

    ## ##################################################
    ## Richesse spécifique :
    Data$pres.abs <- presAbs.f(nombres=Data[ , nombres], logical = FALSE)

    df.biodiv$richesse_specifique <- as.vector(tapply(Data$pres.abs,
                                                      Data[ , unitobs], sum, na.rm=TRUE),
                                               "integer")
    ## ... as.vector to avoid the class "array".

    ## richesses specifiques relatives :

    ## Phylum(s) présent(s) dans le jeux de données :
    phylums <- as.character(unique(na.omit(especes$Phylum[match(Data[ , code.especes],
                                                                especes$code_espece)])))

    ## RS relative par rapp. au nombre d'espèces du site :
    if (any(is.element(c("all", "RS.relative.site"), indices)))
    {
        df.biodiv$RS.relative.site <- (df.biodiv$richesse_specifique /
                                       nrow(subset(especes,
                                                   eval(parse(text=paste("Obs", siteEtudie, sep=""))) == "oui"))) * 100
    }

    ## RS relative par rapp. au nombre d'espèces du site et du(des) phylum(s) concerné(s) (jeu de données) :
    if (any(is.element(c("all", "RS.relative.site.phylum"), indices)))
    {
        df.biodiv$RS.relative.site.phylum <- (df.biodiv$richesse_specifique /
                                              nrow(subset(especes,
                                                          eval(parse(text=paste("Obs", siteEtudie, sep=""))) == "oui" &
                                                          is.element(Phylum, phylums)))) * 100
    }

    ## RS relative par rapp. au nombre d'espèces des données :
    if (any(is.element(c("all", "RS.relative.donnees"), indices)))
    {
        df.biodiv$RS.relative.donnees <- (df.biodiv$richesse_specifique /
                                          nrow(subset(especes,
                                                      is.element(code_espece, Data[ , code.especes])))) * 100
    }

    ## ## RS relative par rapp. au nombre d'espèces des données + des phyla présents :
    ## Inutile : "RS.relative.donnees" est par définition limitée au phyla présents !

    ## RS relative par rapp. au nombre d'espèces au niveau régional (OM ou méditerrannée) :
    if (any(is.element(c("all", "RS.relative.region"), indices)))
    {
        df.biodiv$RS.relative.region <- (df.biodiv$richesse_specifique /
                                         nrow(especes)) * 100
    }

    ## RS relative par rapp. au nombre d'espèces au niveau régional (OM ou méditerrannée) et
    ## du(des) phylum(s) concerné(s) (jeu de données) :
    if (any(is.element(c("all", "RS.relative.region.phylum"), indices)))
    {
        df.biodiv$RS.relative.region.phylum <- (df.biodiv$richesse_specifique /
                                                nrow(subset(especes, is.element(Phylum, phylums)))) * 100
    }

    ## ##################################################
    ## Indices de Simpson et Shannon et dérivés :

    matNombres <- tapply(Data[ , nombres], # Matrice de nombres d'individus /espèce/unitobs.
                         list(Data[ , unitobs], Data[ , code.especes]),
                         sum, na.rm=TRUE)

    matNombres[is.na(matNombres)] <- 0  # Vrais zéros

    ## Proportion d'individus de chaque espèce dans l'unitobs :
    propIndiv <- sweep(matNombres, 1,                           #
                       apply(matNombres, 1, sum, na.rm = TRUE), # Nombre d'individus / unitobs ; équiv df.biodiv$nombre.
                       FUN="/")

    ## Indices de Simpson :
    df.biodiv$simpson <- apply(propIndiv^2, 1, sum, na.rm=TRUE)

    if (any(is.element(c("all", "l.simpson"), indices)))
    {
        df.biodiv$l.simpson <- 1 - df.biodiv$simpson
    }

    ## calcul de l'indice de Shannon :
    df.biodiv$shannon <- -1 * apply(propIndiv * log(propIndiv), 1, sum, na.rm=TRUE)

    ## calcul de l'indice de Pielou :
    if (any(is.element(c("all", "pielou"), indices)))
    {
        df.biodiv$pielou <- df.biodiv$shannon / log(df.biodiv$richesse_specifique)
    }

    ## calcul de l'indice de Hill :
    if (any(is.element(c("all", "hill"), indices)))
    {
        df.biodiv$hill <- (1 - df.biodiv$simpson) / exp(df.biodiv$shannon)
                                        # équiv df.biodiv$l.simpson / exp(df.biodiv$shannon)
    }

    ## suppression de l'indice de shannon (non pertinent)
    df.biodiv$shannon <- NULL

    ## ##################################################
    ## Indices de biodiversité taxonomique :
    df.biodivTaxo <- calcBiodivTaxo.f(Data=Data,
                                      unitobs = unitobs, code.especes = code.especes, nombres = nombres,
                                      global = FALSE, printInfo = FALSE,
                                      indices=indices)

    if (!is.null(dim(df.biodivTaxo)))
    {
        df.biodiv <- cbind(df.biodiv,
                           df.biodivTaxo[match(df.biodiv[ ,unitobs], row.names(df.biodivTaxo)), , drop=FALSE])
    }else{}

    for (ind in c("simpson", "shannon", "richesse_specifique"))
    {
        if (! any(is.element(c(ind, "all"), indices)))
        {
            df.biodiv[ , ind] <- NULL
        }else{}
    }

    return(df.biodiv)
}

########################################################################################################################
calcBiodivTaxo.f <- function(Data, unitobs="unite_observation", code.especes="code_espece", nombres="nombre",
                             global=FALSE, printInfo=FALSE,
                             indices="all")
{
    ## Purpose: Calcul des indices de biodiversité basés sur la taxonomie.
    ## ----------------------------------------------------------------------
    ## Arguments: Data : les données à partir desquelles calculer les
    ##                   indices. Doivent comporter au minimum (colones) :
    ##                     * unités d'observations/sites
    ##                     * espèces présentes
    ##                     * nombre d'individus /espèce/unitobs.
    ##            unitobs : nom de la colone d'unités d'observation.
    ##            especes : nom de la colone d'espèces.
    ##            nombres : nom de la colone de nombres.
    ##            global : est-ce que les résultats doivent être exportés
    ##                     globalement (booléen).
    ##            printInfo : affichage des infos ? (booléen).
    ##            indices : liste des indices à calculer
    ##                      (vecteur de caractères), tous par défaut.
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 29 oct. 2010, 14:30

    ## Indices proposés :
    proposed.indices <- c("D"="Delta",
                          "Dstar"="DeltaEtoile",
                          "Lambda"="LambdaPlus",
                          "Dplus"="DeltaPlus",
                          "SDplus"="SDeltaPlus")

    ## On sort de la fonction si elle n'a pas d'intéret :
    if (! any(is.element(c(proposed.indices, "all"), indices)))
    {
        return(NULL)                    # Rien !
    }else{
        ## Suppréssion de tout ce qui n'a pas de genre (peut être du non biotique) :
        Data <- Data[especes$Genre[match(Data$code_espece, especes$code_espece)] != "ge.", ]

        ## Suppression des niveaux de facteur inutilisés :
        Data <- dropLevels.f(df=Data)

        ## Si les données ne sont pas encore agrégées /espèce/unitobs on le fait ici :
        if (nrow(Data) > nrow(expand.grid(unique(Data[ , unitobs]), unique(Data[ , code.especes]))))
        {
            Data <- agregationTableParCritere.f(Data=Data, metrique=nombres,
                                                facteurs=c(unitobs, code.especes),
                                                listFact=NULL)
        }else{}

        ## Table de contingence unitobs-espèces :
        contingence <- tapply(Data[ , nombres],
                              list(Data[ , unitobs], Data[ , code.especes]),
                              sum, na.rm=TRUE)

        contingence[is.na(contingence)] <- 0 # Vrais zéros.

        ## tableau avec genre, famille, etc.
        sp.taxon <- dropLevels.f(especes[match(colnames(contingence),
                                               especes$code_espece, nomatch=NA, incomparables = FALSE),
                                         c("Genre", "Famille", "Ordre", "Classe", "Phylum")])

        ## colnames(sp.taxon) <- c("genre", "famille", "ordre", "classe", "phylum")
        rownames(sp.taxon) <- colnames(contingence)

        ## retrait des lignes ayant un niveau taxonomique manquant dans sp.taxon et dans contingence (en colonnes) :
        manque.taxon <- apply(sp.taxon, 1, function(x){any(is.na(x))})
        sp.taxon <- sp.taxon[! manque.taxon, , drop=FALSE]
        contingence <- contingence[, ! manque.taxon, drop=FALSE]



        ## Calcul des indices (librairie "vegan") :
        if (sum(sapply(sp.taxon, function(x)length(unique(x))) > 1) > 2) # typiquement : une seule famille ou même genre.
        {
            ## Indices retenus :
            if (is.element("all", indices))
            {
                retained.indices <- proposed.indices
            }else{
                retained.indices <- proposed.indices[is.element(proposed.indices, indices)]
            }

            ## calcul des distances taxonomiques entre les especes
            taxdis <- taxa2dist(sp.taxon, varstep=TRUE, check=TRUE)

            ## Function finds indices of taxonomic diversity and distinctiness, which are averaged taxonomic distances among
            ## species or individuals in the community...
            divTaxo <- taxondive(contingence, taxdis)

            ## mise de divTaxo sous forme de data.frame :
            df.biodivTaxo <- as.data.frame(divTaxo[names(retained.indices)])

            colnames(df.biodivTaxo) <- retained.indices # [!!!] "LambdaPlus" ? vraiment ? [???]

            ## affichage des valeurs attendues :
            if (printInfo)
            {
                message(paste("La valeur théorique de Delta est :" , round(divTaxo[["ED"]], 3)))
                message(paste("La valeur théorique de Delta* est :" , round(divTaxo[["EDstar"]], 3)))
                message(paste("La valeur théorique de Delta+ est :" , round(divTaxo[["EDplus"]], 3)))
            }else{}

            ## Résultats :
            if (global)
            {
                ## Création des objets dans l'environnement global
                assign("div", divTaxo, envir=.GlobalEnv)
                assign("taxdis", taxdis, envir=.GlobalEnv)
                assign("ind_div", df.biodivTaxo, envir=.GlobalEnv)
            }else{
                return(df.biodivTaxo)
            }
        }else{                              # nombre de genre < 2.
            switch(sum(sapply(sp.taxon, function(x)length(unique(x))) > 1),
                   "1"={
                       warning("Nombre de Familles < 2 : les indices de diversité taxonomique ne peuvent être calculés.")
                   },
                   "0"={
                       warning("Nombre de genres < 2 : les indices de diversité taxonomique ne peuvent être calculés.")
                   })
        }
    }
}

########################################################################################################################
agregation.info.f <- function()
{
    ## Purpose: fenêtre d'information sur les agrégations en cours.
    ## ----------------------------------------------------------------------
    ## Arguments: aucun
    ## Output : objet de fenêtre (pour pouvoir la détruire).
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date:  3 nov. 2010, 09:34

    WinInfo <- tktoplevel()

    tkwm.title(WinInfo, "Information")

    tkgrid(tklabel(WinInfo, text="\t "),
           tklabel(WinInfo, text="\tAgrégation des données en cours...\n"),
           tklabel(WinInfo, text="\t "),
           sticky="w")
    tkgrid(tklabel(WinInfo, text="\t "),
           tklabel(WinInfo,
                   text=paste("(cette fenêtre se fermera automatiquement)\n", sep="")),
           sticky="w")
    tkfocus(WinInfo)
    winSmartPlace.f(WinInfo)

    return(WinInfo)
}

########################################################################################################################
close.info.f <- function(WinInfo)
{
    ## Purpose: Fermer une fenêtre d'information.
    ## ----------------------------------------------------------------------
    ## Arguments: WinInfo : objet d'identification de fenêtre.
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date:  3 nov. 2010, 09:37

    tkdestroy(WinInfo)
}




### Local Variables:
### ispell-local-dictionary: "english"
### fill-column: 120
### End:
