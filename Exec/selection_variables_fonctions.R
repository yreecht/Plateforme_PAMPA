#-*- coding: latin-1 -*-

### File: Selection_variables_fonctions.R
### Time-stamp: <2010-09-29 09:00:22 yreecht>
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
champsMetriques.f <- function(nomTable)
{
    ## Purpose: Retourne la liste des champs pouvant être utilisés comme
    ##          métriques en fonction du nom de table
    ## ----------------------------------------------------------------------
    ## Arguments: nomTable : nom de la table dans laquelle doivent être
    ##                       cherchées les métriques disponibles (chaîne de
    ##                       caractères).
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
                                                                 c("an", "pres_abs", ifelse(is.benthos.f(),
                                                                                            "nombre",
                                                                                            NULL)))]))
               },
               ## Table unitespta (métriques d'observation par classes de taille) :
               unitespta={
                   return(sort(colnames(unitespta)[sapply(unitespta,
                                                            function(x){is.numeric(x) & !all(is.na(x))}) &
                                                     !is.element(colnames(unitespta),
                                                                 c("an", "pres_abs", "longitude", "latitude"))]))
               },
               ## Table TabbleBiodiv (indices de biodiversité) :
               TableBiodiv={
                   columns <- c("richesse_specifique", "simpson", "l.simpson", "pielou", "hill",
                                "RS_relative", "Delta", "DeltaEtoile", "LambdaPlus", "DeltaPlus",
                                "SDeltaPlus")

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
champsUnitobs.f <- function(ordered=FALSE)
{
    ## Purpose: Retourne la liste des champs du référentiel d'unités
    ##          d'observations après avoir supprimé les champs vides.
    ## ----------------------------------------------------------------------
    ## Arguments: ordered : faire apparaître les champs principaux en
    ##                      premiers ? (booléen, optionnel)
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
        res <- c("", sort(cPrincip[is.element(cPrincip, res)]),
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
    sitesExclus <- grep(pattern=paste("^", SiteEtudie, "$", sep=""), x=listeSite, value=TRUE, invert=TRUE)

    ## champs ne correspondant pas au motif "(Site1|Site2|...)$" :
    champs <- grep(paste("(", paste(sitesExclus, collapse="|"), ")$", sep=""),
                   sort(colnames(especes)),
                   value=TRUE, invert=TRUE)

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

        switch(nomTable,
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
subsetToutesTables.f <- function(metrique, facteurs, selections, tableMetrique="", exclude=NULL)
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
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date:  6 août 2010, 16:46

    if (tableMetrique == "")
    {
        tableMetrique <- "listespunit"
    }else{}

    dataMetrique <- eval(parse(text=tableMetrique))

    ## Subset en fonction de la table de métrique
    switch(tableMetrique,
           ## Cas de la table d'observation :
           listespunit={
                tmp <- cbind(dataMetrique[!is.na(dataMetrique[ , metrique]) , metrique, drop=FALSE],
                             unitobs[match(dataMetrique$unite_observation[!is.na(dataMetrique[ , metrique])],
                                           unitobs$unite_observation), # ajout des colonnes sélectionnées d'unitobs
                                     facteurs[is.element(facteurs, colnames(unitobs))], drop=FALSE],
                             especes[match(dataMetrique$code_espece[!is.na(dataMetrique[ , metrique])],
                                           especes$code_espece),        # ajout des colonnes sélectionnées d'especes
                                     facteurs[is.element(facteurs, colnames(especes))], drop=FALSE])
           },
           ## Cas de la table d'observations par classes de taille :
           unitespta={
               tmp <- cbind(dataMetrique[!is.na(dataMetrique[ , metrique]) , c(metrique, "classe_taille"), drop=FALSE],
                             unitobs[match(dataMetrique$unite_observation[!is.na(dataMetrique[ , metrique])],
                                           unitobs$unite_observation), # ajout des colonnes sélectionnées d'unitobs
                                     facteurs[is.element(facteurs, colnames(unitobs))], drop=FALSE],
                             especes[match(dataMetrique$code_espece[!is.na(dataMetrique[ , metrique])],
                                           especes$code_espece),        # ajout des colonnes sélectionnées d'especes
                                     facteurs[is.element(facteurs, colnames(especes))], drop=FALSE])
           },
           ## Autres cas :
           tmp <- cbind(dataMetrique[!is.na(dataMetrique[ , metrique]) , metrique, drop=FALSE],
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
        tmp <- subset(tmp, is.element(tmp[ , facteurs[i]], selections[[i]]))
    }

    ## Traitement particulier des classes de taille (mise en facteur avec ordre défini selon le context) :
    if (is.element("classe_taille", colnames(tmp)))
    {
        if (length(grep("^[[:digit:]]*[-_][[:digit:]]*$", unique(as.character(tmp$classe_taille)), perl=TRUE)) ==
            length(unique(as.character(tmp$classe_taille))))
        {
            tmp$classe_taille <-
                factor(as.character(tmp$classe_taille),
                       levels=unique(as.character(tmp$classe_taille))[
                               order(as.numeric(sub("^([[:digit:]]*)[-_][[:digit:]]*$",
                                                    "\\1",
                                                    unique(as.character(tmp$classe_taille)),
                                                    perl=TRUE)),
                                     na.last=FALSE)])
        }else{
            tmp$classe_taille <- factor(tmp$classe_taille)
        }
    }else{}

    ## Conversion des biomasses et densités -> /100m² :
    if (any(is.element(colnames(tmp), c("biomasse", "densite"))))
    {
        tmp[ , is.element(colnames(tmp),
                          c("biomasse", "densite"))] <- 100 * tmp[, is.element(colnames(tmp),
                                                                               c("biomasse", "densite"))]
    }else{}

    return(tmp)
}







### Local Variables:
### ispell-local-dictionary: "english"
### fill-column: 120
### End:
