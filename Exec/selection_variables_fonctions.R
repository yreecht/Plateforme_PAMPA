#-*- coding: latin-1 -*-

### File: Selection_variables_fonctions.R
### Time-stamp: <2010-10-25 15:47:22 yreecht>
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
    if (is.element(nextStep, c("boxplot.esp")) |     # pas proposé si on fait des boxplots.
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
                   return(sort(colnames(listespunit)[sapply(listespunit,
                                                            function(x){is.numeric(x) & !all(is.na(x))}) &
                                                     !is.element(colnames(listespunit),
                                                                 c("an",
                                                                   has.no.pres.abs(nextStep, nomTable),
                                                                   ifelse(is.benthos.f(),
                                                                          "nombre",
                                                                          "")))]))
               },
               ## Table unitespta (métriques d'observation par classes de taille) :
               unitespta={
                   return(sort(colnames(unitespta)[sapply(unitespta,
                                                            function(x){is.numeric(x) & !all(is.na(x))}) &
                                                     !is.element(colnames(unitespta),
                                                                 c("an",
                                                                   has.no.pres.abs(nextStep, nomTable),
                                                                   "longitude", "latitude"))]))
               },
               ## Table TabbleBiodiv (indices de biodiversité) :
               TableBiodiv={
                   columns <- c("richesse_specifique", "simpson", "l.simpson", "pielou", "hill",
                                "Delta", "DeltaEtoile", "LambdaPlus", "DeltaPlus",
                                "SDeltaPlus",
                                grep("RS.relative", colnames(eval(parse(text=nomTable))), value=TRUE))

                   return(sort(colnames(TableBiodiv)[sapply(TableBiodiv,
                                                            function(x){is.numeric(x) & !all(is.na(x))}) &
                                                     is.element(colnames(TableBiodiv), columns)]))
               },
               ## Autres cas :
               return(sort(colnames(eval(parse(text=nomTable)))[sapply(eval(parse(text=nomTable)),
                                                                       function(x){is.numeric(x) & !all(is.na(x))}) &
                                                                colnames(listespunit)!="an"]))
               )
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
                  "site", "an", "biotope", "statut_protection", "caracteristique_1", "caracteristique_2"
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
    }else{}

    return(res)
}


########################################################################################################################
champsRefEspeces.f <- function(site, ordered=FALSE)
{
    ## Purpose: Retourne la liste des champs du référentiel espèces après
    ##          avoir supprimé ceux ne correspondant pas au site étudié ainsi
    ##          que les champs vides.
    ## ----------------------------------------------------------------------
    ## Arguments: site : le site étudié (chaîne de caractères).
    ##            ordered : faire apparaître les champs principaux en
    ##                      premiers ? (booléen, optionnel)
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date:  3 août 2010, 11:16

    ## Champs principaux (externaliser par la suite) :
    cPrincip <- c(
                  ## table "especes" :
                  "code_espece", "Cath_benthique", "Famille", "Genre", "Identifiant", "CategB_general", "CategB_groupe"
                  )

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
        res <- c("", sort(cPrincip[is.element(cPrincip, res)]),
                 "", sort(res[!is.element(res, cPrincip)]))
    }else{}

    return(res)
}


########################################################################################################################
champsReferentiels.f <- function(nomTable)
{
    ## Purpose: Retourne la liste des facteurs pertinents en fonction de la
    ##          table de métriques retenue.
    ## ----------------------------------------------------------------------
    ## Arguments: nomTable : nom de la table de métriques (chaîne de
    ##                       charactère)
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 12 août 2010, 10:18

    if (!is.character(nomTable))
    {
        stop("'nomTable' doit être une chaîne de caractères !")
    }else{
        ## Champs principaux :
        cPrincip <- c(
                      ## table "unitobs" :
                      "site", "an", "biotope", "statut_protection", "caracteristique_1", "caracteristique_2",
                      ## table "especes" :
                      "code_espece", "Cath_benthique", "Famille", "Genre", "Identifiant", "CategB_general", "CategB_groupe"
                      )

        ## Champs des unités d'observation :
        cUnitobs <- champsUnitobs.f()

        ## Champs du référentiel espèces :
        cEspeces <- champsRefEspeces.f(siteEtudie)

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
    if (is.element(tableMetrique, c("", "TableOccurrences")))
    {
        tableMetrique <- "listespunit"
    }else{}

    ## Si pas de métrique disponible ou déjà calculée ("freq.occurrence" est calculée à partir de la sélection) :
    if (is.element(metrique, c("", "freq.occurrence")))
    {
        metrique <- "tmp"
        eval(parse(text=paste(tableMetrique, "$tmp <- 1", sep="")))
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
               restmp <- cbind(dataMetrique[!is.na(dataMetrique[ , metrique]) , c(metriques, "classe_taille"), drop=FALSE],
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
    if (any(is.element(colnames(restmp), c("biomasse", "densite"))) && !is.peche.f())
    {
        restmp[ , is.element(colnames(restmp),
                             c("biomasse", "densite"))] <- 100 * restmp[, is.element(colnames(restmp),
                                                                                     c("biomasse", "densite"))]
    }else{}

    return(restmp)
}


########################################################################################################################
agregationTableParCritere.f <- function(Data, metrique, facteurs, listFact)
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

    ## traitements selon le type de métrique :
    casMetrique <- c("nombre"="sum",
                     "taille_moyenne"="w.mean",
                     "taille_moy"="w.mean",
                     "biomasse"="sum",
                     "poids"="sum",
                     "poids_moyen"="w.mean",
                     "densite"="sum",
                     "CPUE"="sum",
                     "pres_abs"="pres")

    ## Ajout du champs nombre pour le calcul des moyennes pondérées s'il est absent :
    if (casMetrique[metrique] == "w.mean" && ! is.element("nombre", colnames(Data)))
    {
        if (is.element("classe_taille", colnames(Data)))
        {
            Data$nombre <- unitespta$nombre[match(apply(Data[ , c("code_espece", "unite_observation")],
                                                        1, paste, collapse="*"),
                                                  apply(listespunit[ , c("code_espece", "unite_observation")],
                                                        1, paste, collapse="*"))]
        }else{
            Data$nombre <- listespunit$nombre[match(apply(Data[ , c("code_espece", "unite_observation")],
                                                          1, paste, collapse="*"),
                                                    apply(listespunit[ , c("code_espece", "unite_observation")],
                                                          1, paste, collapse="*"))]
        }
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
           )

    ## Nom des dimensions
    names(dimnames(res)) <- c(facteurs)

    ## Transformation vers format long :
    reslong <- as.data.frame(as.table(res), responseName=metrique)
    reslong <- reslong[ , c(tail(colnames(reslong), 1), head(colnames(reslong), -1))] # métrique en première.

    ## Agrégartion et ajout des facteurs supplémentaires :
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

    ## Vérification des facteurs supplémentaires agrégés. Il ne doit pas y avoir d'élément nul (la fonction précédente
    ## renvoie NULL si plusieurs niveaux de facteurs, i.e. le facteur est un sous ensemble d'un des facteurs
    ## d'agrégation des observations) :
    if (any(sapply(reslong[ , listFact], function(x){any(is.null(unlist(x)))})))
    {
        warning("Un des facteurs annexes est surement un sous-ensemble du(des) facteur(s) de regroupement des observations.")
        return(NULL)
    }else{
        return(reslong)
    }
}






### Local Variables:
### ispell-local-dictionary: "english"
### fill-column: 120
### End:
