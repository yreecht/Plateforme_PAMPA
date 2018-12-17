#-*- coding: latin-1 -*-
# Time-stamp: <2018-12-17 15:24:02 yreecht>

## Plateforme PAMPA de calcul d'indicateurs de ressources & biodiversité
##   Copyright (C) 2008-2013 Ifremer - Tous droits réservés.
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

### File: Variables_selection_functions.R
### Created: <2012-01-18 17:45:33 yreecht>
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
    return(all(is.element(getOption("P.obsType"), benthTypes)))
}

########################################################################################################################
has.no.pres.abs <- function(nextStep, tableMetrique, dataEnv)
{
    ## Purpose: si les présences/absences doivent ne pas être affichées,
    ##          renvoie "pres.abs", NULL sinon
    ## ----------------------------------------------------------------------
    ## Arguments: nextStep : l'identifiant de l'étape suivante
    ##            tableMetrique : la table de métrique.
    ##            dataEnv : l'environnement des données.
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 12 oct. 2010, 09:07
    if (is.element(nextStep, c("boxplot.esp", "boxplot.unitobs",
                               "barplot.esp", "barplot.unitobs")) |     # pas proposé si on fait des boxplots.
        length(unique(na.omit(get(tableMetrique, # "            " une seule modalité.
                                  envir=dataEnv)$pres.abs))) < 2)
    {
        return("pres.abs")              # fonctionnement "inversé" !
    }else{
        return(NULL)
    }
}


########################################################################################################################
MetricsField.aliases <- function(nomTable, nextStep, dataEnv)
{
    ## Purpose: Retourne la liste des champs pouvant être utilisés comme
    ##          métriques en fonction du nom de table
    ## ----------------------------------------------------------------------
    ## Arguments: nomTable : nom de la table dans laquelle doivent être
    ##                       cherchées les métriques disponibles (chaîne de
    ##                       caractères).
    ##            nextStep : identifiant de l'étape suivante.
    ##            dataEnv : l'environnement des données.
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 11 août 2010, 16:14

    ## On récupère la table de données :
    assign(nomTable, get(nomTable, envir=dataEnv),
           envir=environment())

    unitobs <- get("unitobs", envir=dataEnv)
    refesp <- get("refesp", envir=dataEnv)

    if (!is.character(nomTable))
    {
        stop("'nomTable' must be a character string!")
    }else{
        switch(nomTable,
               ## Table unitSp (métriques d'observation) :
               unitSp={
                   res <- aliases(colnames(unitSp)[sapply(unitSp,
                                                          function(x){is.numeric(x) & !all(is.na(x))}) &
                                                   !is.element(colnames(unitSp),
                                                               c("year",
                                                                 has.no.pres.abs(nextStep,
                                                                                 nomTable,
                                                                                 dataEnv=dataEnv),
                                                                 ifelse(is.benthos.f(),
                                                                        "number",
                                                                        "")))],
                                  reverse = TRUE)

                   res <- res[order(names(res))]
               },
               ## Table unitSpSz (métriques d'observation par classes de taille) :
               unitSpSz={
                   res <- aliases(colnames(unitSpSz)[sapply(unitSpSz,
                                                            function(x){is.numeric(x) & !all(is.na(x))}) &
                                                     !is.element(colnames(unitSpSz),
                                                                 c("year",
                                                                   has.no.pres.abs(nextStep, nomTable, dataEnv=dataEnv),
                                                                   "longitude", "latitude"))],
                                  reverse = TRUE)

                   res <- res[order(names(res))]
               },
               ## Table unit (indices de biodiversité) :
               unit={
                   columns <- c("species.richness", "simpson", "simpson.l", "pielou", "hill",
                                "Delta", "DeltaStar", "LambdaPlus", "DeltaPlus",
                                "SDeltaPlus",
                                grep("relative.SR", colnames(eval(parse(text=nomTable))), value=TRUE))

                   columnsAls <- aliases(columns, reverse = TRUE) # with aliases as names

                   validCols <- colnames(unit)[sapply(unit,
                                                      function(x){is.numeric(x) & !all(is.na(x))}) &
                                               is.element(colnames(unit), columns)]

                   res <- columnsAls[columnsAls %in% validCols]

               },
               ## Autres cas :
               {
                   res <- aliases(colnames(eval(parse(text=nomTable)))[sapply(eval(parse(text=nomTable)),
                                                                              function(x)
                                                                              {
                                                                                  is.numeric(x) & !all(is.na(x))
                                                                              }) &
                                                                       colnames(unitSp)!="year"],
                                  reverse = TRUE)

                   res <- res[order(names(res))]
               }
               )

        return(res[!is.element(res, c(colnames(refesp), colnames(unitobs)))])
    }
}


########################################################################################################################
champsUnitobs.f <- function(dataEnv, ordered=FALSE, tableMetrique="")
{
    ## Purpose: Retourne la liste des champs du référentiel d'unités
    ##          d'observations après avoir supprimé les champs vides.
    ## ----------------------------------------------------------------------
    ## Arguments: ordered : faire apparaître les champs principaux en
    ##                      premiers ? (booléen, optionnel)
    ##            tableMetrique : nom de la table de métriques (pour pouvoir
    ##                            ajouter le champ classe de taille, même
    ##                            si ne fait pas partie de cette table).
    ##            dataEnv : l'environnement des données.
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date:  3 août 2010, 11:12

    ## Récupération des données :
    unitobs <- get("unitobs", envir=dataEnv)

    ## Champs principaux :
    cPrincip <- c(
                  ## table "unitobs" :
                  "site", "year", "annee.campagne", "biotop", "protection.status", "geogr.descriptor1",
                  "geogr.descriptor2"
                  )

    ## Champs non-vides de la table 'unitobs' :
    res <- sort(names(unitobs)[sapply(names(unitobs), function(i){!all(is.na(unitobs[ , i]))})])

    ## Champs principaux en premiers :
    if (ordered)
    {
        res <- c(if (tableMetrique == "unitSpSz")
                 {
                     c("size.class", "")
                 }else{
                     ""
                 },
                 sort(cPrincip[is.element(cPrincip, res)]),
                 "", sort(res[!is.element(res, cPrincip)]))
    }else{
        res <- c(if (tableMetrique == "unitSpSz")
                 {
                     c("size.class", "")
                 }else{
                     ""
                 },
                 res)}

    return(res)
}

UnitobsFields.aliases <- function(dataEnv, ordered=FALSE, tableMetrique="")
{
    ## Purpose: Retourne la liste des champs du référentiel d'unités
    ##          d'observations après avoir supprimé les champs vides.
    ## ----------------------------------------------------------------------
    ## Arguments: ordered : faire apparaître les champs principaux en
    ##                      premiers ? (booléen, optionnel)
    ##            tableMetrique : nom de la table de métriques (pour pouvoir
    ##                            ajouter le champ classe de taille, même
    ##                            si ne fait pas partie de cette table).
    ##            dataEnv : l'environnement des données.
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date:  3 août 2010, 11:12

    ## Récupération des données :
    unitobs <- get("unitobs", envir=dataEnv)

    ## Champs principaux :
    cPrincip <- aliases(c(
                          ## table "unitobs" :
                          "year", "protection.status", "annee.campagne", "site", "biotop",
                          "geogr.descriptor1", "geogr.descriptor2"
                          ), reverse = TRUE)

    ## Champs non-vides de la table 'unitobs' :
    res <- aliases(names(unitobs)[sapply(names(unitobs),
                                         function(i)
                                         {
                                             !all(is.na(unitobs[ , i]))
                                         })],
                   reverse = TRUE)

    res <- res[order(names(res))]       # in alphabetical order anyway.

    ## Champs principaux en premiers :
    if (ordered)
    {
        res <- c(if (tableMetrique == "unitSpSz")
                 {
                     aliases(c("size.class", ""), reverse = TRUE)
                 }else{
                     ""
                 },
                 cPrincip[is.element(cPrincip, res)],
                 "", res[!is.element(res, cPrincip)])
    }else{
        res <- c(if (tableMetrique == "unitSpSz")
                 {
                     aliases(c("size.class", ""), reverse = TRUE)
                 }else{
                     ""
                 },
                 res)}

    return(res)
}


########################################################################################################################
champsRefEspeces.f <- function(site, dataEnv, ordered=FALSE, tableMetrique="", nextStep=NA)
{
    ## Purpose: Retourne la liste des champs du référentiel espèces après
    ##          avoir supprimé ceux ne correspondant pas au site étudié ainsi
    ##          que les champs vides.
    ## ----------------------------------------------------------------------
    ## Arguments: site : le site étudié (chaîne de caractères).
    ##            ordered : faire apparaître les champs principaux en
    ##                      premiers ? (booléen, optionnel)
    ##            tableMetrique : nom de la table de métriques (pour pouvoir
    ##                            ajouter le champ classe de taille, même
    ##                            si ne fait pas partie de cette table).
    ##            nextStep : étape suivante.
    ##            dataEnv : l'environnement des données.
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date:  3 août 2010, 11:16

    ## Récupération des données (référentiel espèce et observations) :
    refesp <- get("refesp", envir=dataEnv)
    obs <- get("obs", envir=dataEnv)

    ## Champs principaux (externaliser par la suite) :
    if (is.element(tableMetrique, c("unitSp", "TableOccurrences", "unitSpSz")) &&
        is.element(nextStep,
                   c("boxplot.esp", "modele_lineaire", "freq_occurrence",
                     "MRT.esp", "barplot.esp")))
    {
        cPrincip <- c("species.code", "species", "scient.name")
    }else{
        cPrincip <- c(
                      ## table "refesp" :
                      "species.code", "benthic.categ", "family", "genus", "scient.name",
                      "CategB_general", "CategB_groupe"
                      )
    }

    ## Externaliser la définition des sites par la suite...
    listeSite <- c("RUN" , "MAY" , "BA" , "BO" , "CB" , "CR" , "STM" , "NC")

    ## Noms des sites dont on doit exclure les colonnes :
    sitesExclus <- listeSite[ ! grepl(pattern=paste("^(",
                                                    paste(site, collapse="|"),
                                                    ")$", sep=""),
                                      x=listeSite)]

    ## champs ne correspondant pas au motif "(Site1|Site2|...)$" :
    champs <- sort(colnames(refesp))[! grepl(paste("(", paste(sitesExclus, collapse="|"), ")$", sep=""),
                                             sort(colnames(refesp)))]

    ## Champs non-vides de la table 'espèces' :
    res <- sort(champs[sapply(champs,
                              function(i){!all(is.na(refesp[is.element(refesp$species.code,
                                                                       obs$species.code) , i]))})])

    ## Champs principaux en premiers :
    if (ordered)
    {
        res <- c(if (tableMetrique == "unitSpSz")
                 {
                     c("size.class", "")
                 }else{
                     ""
                 },
                 sort(cPrincip[is.element(cPrincip, res)]),
                 "", sort(res[!is.element(res, cPrincip)]))
    }else{
        res <- c(if (tableMetrique == "unitSpSz")
                 {
                     c("size.class", "")
                 }else{
                     ""
                 },
                 res)
    }

    return(res)
}

spRefFields.aliases <- function(site, dataEnv, ordered=FALSE, tableMetrique="", nextStep=NA)
{
    ## Purpose: Retourne la liste des champs du référentiel espèces après
    ##          avoir supprimé ceux ne correspondant pas au site étudié ainsi
    ##          que les champs vides.
    ## ----------------------------------------------------------------------
    ## Arguments: site : le site étudié (chaîne de caractères).
    ##            ordered : faire apparaître les champs principaux en
    ##                      premiers ? (booléen, optionnel)
    ##            tableMetrique : nom de la table de métriques (pour pouvoir
    ##                            ajouter le champ classe de taille, même
    ##                            si ne fait pas partie de cette table).
    ##            nextStep : étape suivante.
    ##            dataEnv : l'environnement des données.
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date:  3 août 2010, 11:16

    ## Récupération des données (référentiel espèce et observations) :
    refesp <- get("refesp", envir=dataEnv)
    obs <- get("obs", envir=dataEnv)

    ## Champs principaux (externaliser par la suite) :
    if (is.element(tableMetrique, c("unitSp", "TableOccurrences", "unitSpSz")) &&
        is.element(nextStep,
                   c("boxplot.esp", "modele_lineaire", "freq_occurrence",
                     "MRT.esp", "barplot.esp")))
    {
        cPrincip <- aliases(c("species.code", "species", "scient.name"), reverse = TRUE)
    }else{
        cPrincip <- aliases(c(
                              ## table "refesp" :
                              "species.code", "benthic.categ", "family", "genus", "scient.name",
                              "CategB_general", "CategB_groupe"
                              ), reverse = TRUE)
    }

    ## Externaliser la définition des sites par la suite...
    listeSite <- c("RUN" , "MAY" , "BA" , "BO" , "CB" , "CR" , "STM" , "NC")

    ## Noms des sites dont on doit exclure les colonnes :
    sitesExclus <- listeSite[ ! grepl(pattern=paste("^(",
                                                    paste(site, collapse="|"),
                                                    ")$", sep=""),
                                      x=listeSite)]

    ## champs ne correspondant pas au motif "(Site1|Site2|...)$" :
    champs <- sort(colnames(refesp))[! grepl(paste("(", paste(sitesExclus, collapse="|"), ")$", sep=""),
                                             sort(colnames(refesp)))]

    ## Champs non-vides de la table 'espèces' :
    res <- aliases(champs[sapply(champs,
                                 function(i)
                                 {
                                     !all(is.na(refesp[is.element(refesp$species.code,
                                                                  obs$species.code) , i]))
                                 })],
                   reverse = TRUE)

    res <- res[order(names(res))]

    ## Champs principaux en premiers :
    if (ordered)
    {
        res <- c(if (tableMetrique == "unitSpSz")
                 {
                     aliases(c("size.class", ""), reverse = TRUE)
                 }else{
                     ""
                 },
                 cPrincip[is.element(cPrincip, res)],
                 "", res[!is.element(res, cPrincip)])
    }else{
        res <- c(if (tableMetrique == "unitSpSz")
                 {
                     aliases(c("size.class", ""), reverse = TRUE)
                 }else{
                     ""
                 },
                 res)
    }

    return(res)
}



########################################################################################################################
champsReferentiels.f <- function(nomTable, dataEnv, nextStep=NA)
{
    ## Purpose: Retourne la liste des facteurs pertinents en fonction de la
    ##          table de métriques retenue.
    ## ----------------------------------------------------------------------
    ## Arguments: nomTable : nom de la table de métriques (chaîne de
    ##                       charactère)
    ##            nextStep : étape suivante, pas obligatoire.
    ##            dataEnv : l'environnement des données.
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 12 août 2010, 10:18

    if (!is.character(nomTable))
    {
        stop("'nomTable' must be a character string!")
    }else{
        ## Champs principaux :
        cPrincip <- c(
                      ## table "unitobs" :
                      "year", "protection.status", "site", "annee.campagne", "biotop", "geogr.descriptor1",
                      "geogr.descriptor2",
                      ## table "especes" :
                      "species.code", "benthic.categ", "family", "genus", "scient.name",
                      "CategB_general", "CategB_groupe" # [ml?]
                      )

        ## Champs des unités d'observation :
        cUnitobs <- UnitobsFields.aliases(dataEnv=dataEnv)

        ## Champs du référentiel espèces :
        if (length(grep("\\.unitobs$", nextStep)) > 0) # [!!!] Dangereux  [yr: 18/1/2012]
        {
            cEspeces <- NA              # les champs du ref espèce n'ont pas de raison d'apparaitre dans les cas de
                                        # tables agrégées par unitobs.
        }else{
            cEspeces <- spRefFields.aliases(site=getOption("P.MPA"), dataEnv=dataEnv)
        }

        casTables <- c("unitSp"="unitSp",
                       "TablePresAbs"="unitSp",
                       "TableOccurrences"="unitSp",
                       "unitSpSz"="unitSpSz",
                       "unit"="unit")

        switch(casTables[nomTable],
               unit={
                   return(c("", sort(cPrincip[is.element(cPrincip, cUnitobs)]),   # champs principaux...
                            "", sort(cUnitobs[!is.element(cUnitobs, cPrincip)]))) # autres.
               },
               unitSp={
                   return(c("", sort(cPrincip[is.element(cPrincip, c(cUnitobs, cEspeces))]), # champs principaux...
                            "", sort(c(cUnitobs[!is.element(cUnitobs, cPrincip)],            # autres.
                                       cEspeces[!is.element(cEspeces, cPrincip)]))))
               },
               unitSpSz={
                   return(c("", "size.class",
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

refTablesFields.aliases <- function(nomTable, dataEnv, nextStep=NA)
{
    ## Purpose: Retourne la liste des facteurs pertinents en fonction de la
    ##          table de métriques retenue.
    ## ----------------------------------------------------------------------
    ## Arguments: nomTable : nom de la table de métriques (chaîne de
    ##                       charactère)
    ##            nextStep : étape suivante, pas obligatoire.
    ##            dataEnv : l'environnement des données.
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 12 août 2010, 10:18

    if (!is.character(nomTable))
    {
        stop("'nomTable' must be a character string!")
    }else{
        ## Champs principaux :
        cPrincip <- aliases(c(
                              ## table "unitobs" :
                              "year", "protection.status", "site", "annee.campagne", "biotop",
                              "geogr.descriptor1", "geogr.descriptor2",
                              ## table "especes" :
                              "species.code", "benthic.categ", "family", "genus", "scient.name",
                              "CategB_general", "CategB_groupe" # [ml?]
                              ),
                            reverse = TRUE)

        ## Champs des unités d'observation :
        cUnitobs <- UnitobsFields.aliases(dataEnv=dataEnv)
        cUnitobs <- cUnitobs[as.logical(nchar(cUnitobs))] # remove empty field

        ## Champs du référentiel espèces :
        if (length(grep("\\.unitobs$", nextStep)) > 0) # [!!!] Dangereux  [yr: 18/1/2012]
        {
            cEspeces <- NA              # les champs du ref espèce n'ont pas de raison d'apparaitre dans les cas de
                                        # tables agrégées par unitobs.
        }else{
            cEspeces <- spRefFields.aliases(site=getOption("P.MPA"), dataEnv=dataEnv)
            cEspeces <- cEspeces[as.logical(nchar(cEspeces))] # remove empty field
        }

        casTables <- c("unitSp"="unitSp",
                       "TablePresAbs"="unitSp",
                       "TableOccurrences"="unitSp",
                       "unitSpSz"="unitSpSz",
                       "unit"="unit")

        switch(casTables[nomTable],
               unit={
                   res <- cUnitobs[order(names(cUnitobs))]
               },
               unitSp=,
               unitSpSz={
                   res <- c(cUnitobs, cEspeces)
                   res <- res[order(names(res))]
               },
               {
                   warning("Table de métrique '", nomTable, "' inconnu")


                   res <- c(cUnitobs, cEspeces)
                   res <- res[order(names(res))]
               })

        return(c(if (casTables[nomTable] == "unitSpSz")
                 {
                     aliases(c("", "size.class", ""), reverse = TRUE)
                 }else{
                     ""
                 },
                 cPrincip[is.element(cPrincip, res)],   # champs principaux...
                 "", res[!is.element(res, cPrincip)])) # autres.

    }
}

########################################################################################################################
subsetToutesTables.f <- function(metrique, facteurs, selections,
                                 dataEnv, tableMetrique="",
                                 exclude=NULL, add=c("species.code", "observation.unit"))
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
    ##            add : champ(s) (de la table de métrique) à ajouter aux
    ##                  données.
    ##            dataEnv : l'environnement des données.
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date:  6 août 2010, 16:46

    ## Récupération des référentiels :
    unitobs <- get("unitobs", envir=dataEnv)
    refesp <- get("refesp", envir=dataEnv)

    ## Si pas de table de métrique disponible ou déjà calculée
    ## ("TableOcurrences" est calculée à partir de la sélection) :
    if (is.element(tableMetrique, c("", "TableOccurrences", "TablePresAbs")))
    {
        tableMetrique <- "unitSp"
    }else{}

    casTables <- c("unitSp"="unitSp",
                   "TablePresAbs"="unitSp",
                   "unitSpSz"="unitSpSz")

    ## Récupération de la table de métriques :
    dataMetrique <- get(tableMetrique, envir=dataEnv)

    ## Si pas de métrique disponible ou déjà calculée ("freq.occurrence" est calculée à partir de la sélection) :
    if (is.element(metrique, c("", "occurrence.frequency")))
    {
        metrique <- "tmp"
        dataMetrique$tmp <- 0
        dataMetrique$tmp[dataMetrique[ , getOption("P.nbName")] > 0] <- 1
    }else{}

    if (!is.null(add))
    {
        metriques <- c(metrique, add[is.element(add, colnames(dataMetrique))])
    }else{
        metriques <- metrique
    }

    ## Subset en fonction de la table de métrique
    switch(casTables[tableMetrique],
           ## Cas de la table d'observation ou des tables de présence :
           unitSp={
                restmp <- cbind(dataMetrique[!is.na(dataMetrique[ , metrique]) , metriques, drop=FALSE],
                                unitobs[match(dataMetrique$observation.unit[!is.na(dataMetrique[ , metrique])],
                                              unitobs$observation.unit), # ajout des colonnes sélectionnées d'unitobs
                                        facteurs[is.element(facteurs, colnames(unitobs))], drop=FALSE],
                                refesp[match(dataMetrique$species.code[!is.na(dataMetrique[ , metrique])],
                                             refesp$species.code),        # ajout des colonnes sélectionnées d'especes
                                       facteurs[is.element(facteurs, colnames(refesp))], drop=FALSE])
            },
           ## Cas de la table d'observations par classes de taille :
           unitSpSz={
               restmp <- cbind(dataMetrique[!is.na(dataMetrique[ , metrique]) ,
                                            c(metriques, "size.class"), drop=FALSE],
                               unitobs[match(dataMetrique$observation.unit[!is.na(dataMetrique[ , metrique])],
                                             unitobs$observation.unit), # ajout des colonnes sélectionnées d'unitobs
                                       facteurs[is.element(facteurs, colnames(unitobs))], drop=FALSE],
                               refesp[match(dataMetrique$species.code[!is.na(dataMetrique[ , metrique])],
                                            refesp$species.code),        # ajout des colonnes sélectionnées d'especes
                                      facteurs[is.element(facteurs, colnames(refesp))], drop=FALSE])
           },
           ## Autres cas :
           restmp <- cbind(dataMetrique[!is.na(dataMetrique[ , metrique]) , metriques, drop=FALSE],
                           unitobs[match(dataMetrique$observation.unit[!is.na(dataMetrique[ , metrique])],
                                         unitobs$observation.unit), # ajout des colonnes sélectionnées d'unitobs.
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
    if (is.element("size.class", colnames(restmp)))
    {
        if (length(grep("^[[:digit:]]*[-_][[:digit:]]*$", unique(as.character(restmp$size.class)), perl=TRUE)) ==
            length(unique(as.character(restmp$size.class))))
        {
            restmp$size.class <-
                factor(as.character(restmp$size.class),
                       levels=unique(as.character(restmp$size.class))[
                               order(as.numeric(sub("^([[:digit:]]*)[-_][[:digit:]]*$",
                                                    "\\1",
                                                    unique(as.character(restmp$size.class)),
                                                    perl=TRUE)),
                                     na.last=FALSE)])
        }else{
            restmp$size.class <- factor(restmp$size.class)
        }
    }else{}

    ## Conversion des biomasses et densités -> /100m² :
    if (any(is.element(colnames(restmp), c("biomass", "density",
                                           "biomass.max", "density.max",
                                           "biomass.sd", "density.sd"))) && ! is.peche.f())
    {
        restmp[ , is.element(colnames(restmp),
                             c("biomass", "density",
                               "biomass.max", "density.max",
                               "biomass.sd", "density.sd"))] <- 100 *
                                   restmp[, is.element(colnames(restmp),
                                                       c("biomass", "density",
                                                         "biomass.max", "density.max",
                                                         "biomass.sd", "density.sd"))]
    }else{}

    return(restmp)
}

########################################################################################################################
getReducedSVRdata.f <- function(dataName, data, dataEnv)
{
    ## Purpose: Récupérer des données brutes SVR (nombres, densités) réduites
    ##  aux sélections.
    ## ----------------------------------------------------------------------
    ## Arguments: dataName : nom de tableau à récupérer.
    ##            data : données associées (avec sélections).
    ##            dataEnv : environnement des données.
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 31 oct. 2012, 12:21

    res <- get(dataName, envir=dataEnv)

    ## Limitations au classes de tailles, espèces et unité d'observations sélectionnées :
    if (is.element("species.code", colnames(data)) &&
        is.element("species.code", names(dimnames(res))))
    {
        species <- dimnames(res)[["species.code"]]
        res <- extract(res,
                       indices=list(species[is.element(species, data[ , "species.code"])]),
                       dims=which(is.element(names(dimnames(res)), "species.code")))
    }else{}

    if (is.element("observation.unit", colnames(data)) &&
        is.element("observation.unit", names(dimnames(res))))
    {
        unitObs <- dimnames(res)[["observation.unit"]]
        res <- extract(res,
                       indices=list(unitObs[is.element(unitObs, data[ , "observation.unit"])]),
                       dims=which(is.element(names(dimnames(res)), "observation.unit")))
    }else{}

    if (is.element("size.class", colnames(data)) &&
        is.element("size.class", names(dimnames(res))))
    {
        CL <- dimnames(res)[["size.class"]]
        res <- extract(res,
                       indices=list(CL[is.element(CL, data[ , "size.class"])]),
                       dims=which(is.element(names(dimnames(res)), "size.class")))
    }else{}

    return(res)
}


########################################################################################################################
agregationTableParCritere.f <- function(Data, metrique, facteurs, dataEnv, listFact=NULL,
                                        nbName="number")
{
    ## Purpose: Agréger les données selon un ou plusieurs facteurs.
    ## ----------------------------------------------------------------------
    ## Arguments: Data : Le jeu de données à agréger.
    ##            metrique : la métrique agrégée.
    ##            facteurs : les facteurs
    ##            listFact : noms des facteurs supplémentaires (agrégés et
    ##                       ajoutés à la table de sortie).
    ##            dataEnv : l'environnement des données.
    ##            nbName : nom de la colonne nombre.
    ##
    ## Output: une data.frame agrégée.
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 18 oct. 2010, 15:47

    ## Récupération des données

    ## Informations (l'étape peut être longue) :
    WinInfo <- agregation.info.f()

    ## traitements selon le type de métrique :
    casMetrique <- c("number"="sum",
                     "mean.length"="w.mean",
                     "taille_moy"="w.mean",
                     "biomass"="sum",
                     "Biomass"="sum",
                     "weight"="sum",
                     "mean.weight"="w.mean",
                     "density"="sum",
                     "Density"="sum",
                     "CPUE"="sum",
                     "CPUE.biomass"="sum",
                     "pres.abs"="pres",
                     "abundance.prop.SC"="w.mean.prop", # Pas bon [!!!]
                     "biomass.prop.SC"="w.mean.prop.bio",  # Pas bon [!!!]
                     ## Benthos :
                     "colonies"="sum",
                     "coverage"="sum",
                     "mean.size.colonies"="w.mean.colonies",
                     ## SVR (expérimental) :
                     "number.max"="nbMax",
                     "number.sd"="nbSD",
                     "density.max"="densMax",
                     "density.sd"="densSD",
                     "biomass.max"="sum",
                     "spawning.success"="%.nesting",
                     "spawnings"="sum",
                     "readable.tracks"="sum",
                     "tracks.number"="sum")


    ## Ajout de "readable.tracks" pour le pourcentage de ponte :
    if (any(casMetrique[metrique] == "%.nesting"))
    {
        if (is.element("size.class", colnames(Data)))
        {
            unitSpSz <- get("unitSpSz", envir=dataEnv)

            if (is.null(unitSpSz)) stop("unitSpSz must be defined")

            Data <- merge(Data,
                          unitSpSz[ , c("species.code", "observation.unit", "size.class", "readable.tracks")],
                          by=c("species.code", "observation.unit", "size.class"),
                          suffixes=c("", ".y"))
        }else{
            unitSp <- get("unitSp", envir=dataEnv)

            if (is.null(unitSp)) stop("unitSp must be defined")

            Data <- merge(Data,
                          unitSp[ , c("species.code", "observation.unit", "readable.tracks")],
                          by=c("species.code", "observation.unit"),
                          suffixes=c("", ".y"))
        }
    }else{}

    ## Ajout du champ nombre pour le calcul des moyennes pondérées s'il est absent :
    if ((casMetrique[metrique] == "w.mean" || casMetrique[metrique] == "w.mean.prop"))
    {
        if (is.element("size.class", colnames(Data)))
        {
            unitSpSz <- get("unitSpSz", envir=dataEnv)

            Data <- merge(Data,
                          unitSpSz[ , c("species.code", "observation.unit", "size.class", nbName)],
                          by=c("species.code", "observation.unit", "size.class"))

            ## Ajout de l'abondance totale /espèce/unité d'observation :
            nbTot <- tapply(unitSpSz[ , nbName],
                            as.list(unitSpSz[ , c("species.code", "observation.unit")]),
                            sum, na.rm=TRUE)

            Data <- merge(Data,
                          as.data.frame(as.table(nbTot), responseName="nombre.tot"))
        }else{

            Data <- merge(Data,
                          get("unitSp", envir=dataEnv)[ , c("species.code", "observation.unit", nbName)],
                          by=c("species.code", "observation.unit"))
        }
    }else{}

    ## Ajout du champ biomasse pour les proportions de biomasses par classe de taille :
    if (casMetrique[metrique] == "w.mean.prop.bio")
    {
        unitSpSz <- get("unitSpSz", envir=dataEnv)

        biomass <- colnames(unitSpSz)[is.element(colnames(unitSpSz),
                                                 c("biomass", "CPUE.biomass"))][1]

        Data <- merge(Data,
                      unitSpSz[ , c("species.code", "observation.unit", "size.class",
                                    biomass)],
                      by=c("species.code", "observation.unit", "size.class"))

        ## Ajout de la biomasse totale /espèce/unité d'observation :
        biomTot <- tapply(unitSpSz[ , biomass],
                          as.list(unitSpSz[ , c("species.code", "observation.unit")]),
                          function(x)
                      {
                          ifelse(all(is.na(x)),
                                 NA,
                                 sum(x, na.rm=TRUE))
                      })

        Data <- merge(Data,
                      as.data.frame(as.table(biomTot), responseName="tot.biomass"))
    }

    ## Ajout du champ colonie pour le calcul des moyennes pondérées s'il est absent :
    if (casMetrique[metrique] == "w.mean.colonies" && ! is.element("colonies", colnames(Data)))
    {
        unitSp <- get("unitSp", envir=dataEnv)

        Data$colonies <- unitSp$colonies[match(apply(Data[ , c("species.code", "observation.unit")],
                                                   1, paste, collapse="*"),
                                             apply(unitSp[ , c("species.code", "observation.unit")],
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
                                                  Data[ii, nbName],
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
                                                  Data[ii, "colonies"],
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
                                           (sum(Data[ii, nbName][ !is.na(Data[ii, metrique])], na.rm=TRUE) /
                                             sum(Data[ii, "nombre.tot"], na.rm=TRUE)) *
                                           ## Correction si la classe de taille n'est pas un facteur d'agrégation
                                           ## (sinon valeur divisée par le nombre de classes présentes) :
                                           ifelse(is.element("size.class", facteurs),
                                                  100,
                                                  100 * length(unique(Data$size.class)))))
                         })

           },
           "w.mean.prop.bio"={
               res <- tapply(1:nrow(Data),
                             as.list(Data[ , facteurs, drop=FALSE]),
                             function(ii)
                         {
                             ifelse(all(is.na(Data[ii, metrique])) || sum(Data[ii, "tot.biomass"], na.rm=TRUE) == 0,
                                    NA,
                                    ifelse(all(na.omit(Data[ii, metrique]) == 0), # Pour ne pas avoir NaN.
                                           0,
                                           (sum(Data[ii, biomass][ !is.na(Data[ii, metrique])], na.rm=TRUE) /
                                             sum(Data[ii, "tot.biomass"], na.rm=TRUE)) *
                                           ## Correction si la classe de taille n'est pas un facteur d'agrégation
                                           ## (sinon valeur divisée par le nombre de classes présentes) :
                                           ifelse(is.element("size.class", facteurs),
                                                  100,
                                                  100 * length(unique(Data$size.class)))))
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
           "nbMax"={
               ## Récupération des nombres brutes avec sélections :
               nbTmp <- getReducedSVRdata.f(dataName=".NombresSVR", data=Data, dataEnv=dataEnv)

               ## Somme par croisement de facteur / rotation :
               nbTmp2 <- apply(nbTmp,
                             which(is.element(names(dimnames(nbTmp)), c(facteurs, "rotation"))),
                             function(x)
                         {
                             ifelse(all(is.na(x)), NA, sum(x, na.rm=TRUE))
                         })

               ## Somme par croisement de facteur :
               res <- as.array(apply(nbTmp2,
                                     which(is.element(names(dimnames(nbTmp)), facteurs)),
                                     function(x)
                                 {
                                     ifelse(all(is.na(x)), NA, max(x, na.rm=TRUE))
                                 }))
           },
           "nbSD"={
               ## Récupération des nombres brutes avec sélections :
               nbTmp <- getReducedSVRdata.f(dataName=".NombresSVR", data=Data, dataEnv=dataEnv)

               ## Somme par croisement de facteur / rotation :
               nbTmp2 <- apply(nbTmp,
                             which(is.element(names(dimnames(nbTmp)), c(facteurs, "rotation"))),
                             function(x)
                         {
                             ifelse(all(is.na(x)), NA, sum(x, na.rm=TRUE))
                         })

               ## Somme par croisement de facteur :
               res <- as.array(apply(nbTmp2,
                                     which(is.element(names(dimnames(nbTmp)), facteurs)),
                                     function(x)
                                 {
                                     ifelse(all(is.na(x)), NA, sd(x, na.rm=TRUE))
                                 }))
           },
           "densMax"={
               ## Récupération des nombres brutes avec sélections :
               densTmp <- getReducedSVRdata.f(dataName=".DensitesSVR", data=Data, dataEnv=dataEnv)

               ## Somme par croisement de facteur / rotation :
               densTmp2 <- apply(densTmp,
                                 which(is.element(names(dimnames(densTmp)), c(facteurs, "rotation"))),
                                 function(x)
                             {
                                 ifelse(all(is.na(x)), NA, sum(x, na.rm=TRUE))
                             })

               ## Somme par croisement de facteur :
               res <- as.array(apply(densTmp2,
                                     which(is.element(names(dimnames(densTmp)), facteurs)),
                                     function(x)
                                 {
                                     ifelse(all(is.na(x)), NA, max(x, na.rm=TRUE))
                                 }))
           },
           "densSD"={
               ## Récupération des nombres brutes avec sélections :
               densTmp <- getReducedSVRdata.f(dataName=".DensitesSVR", data=Data, dataEnv=dataEnv)

               ## Somme par croisement de facteur / rotation :
               densTmp2 <- apply(densTmp,
                                 which(is.element(names(dimnames(densTmp)), c(facteurs, "rotation"))),
                                 function(x)
                             {
                                 ifelse(all(is.na(x)), NA, sum(x, na.rm=TRUE))
                             })

               ## Somme par croisement de facteur :
               res <- as.array(apply(densTmp2,
                                     which(is.element(names(dimnames(densTmp)), facteurs)),
                                     function(x)
                                 {
                                     ifelse(all(is.na(x)), NA, sd(x, na.rm=TRUE))
                                 }))
           },
           "%.nesting"={
               res <- tapply(1:nrow(Data),
                             as.list(Data[ , facteurs, drop=FALSE]),
                             function(ii)
                         {
                             ifelse(all(is.na(Data[ii, metrique])),
                                    NA,
                                    weighted.mean(Data[ii, metrique],
                                                  Data[ii, "readable.tracks"],
                                                  na.rm=TRUE))
                         })
           },
           stop("Operation not implemented!")
           )

    ## Nom des dimensions
    names(dimnames(res)) <- c(facteurs)

    ## Transformation vers format long :
    reslong <- as.data.frame(as.table(res), responseName=metrique)
    reslong <- reslong[ , c(tail(colnames(reslong), 1), head(colnames(reslong), -1))] # métrique en première.

    ## Agrégation et ajout des facteurs supplémentaires :
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
    close.info.f(WinInfo)

    ## Vérification des facteurs supplémentaires agrégés. Il ne doit pas y avoir d'élément nul (la fonction précédente
    ## renvoie NULL si plusieurs niveaux de facteurs, i.e. le facteur est un sous ensemble d'un des facteurs
    ## d'agrégation des observations) :
    if (any(sapply(reslong[ , listFact], function(x){any(is.null(unlist(x)))})))
    {
        warning(paste(mltext("agregationTableParCritere.f.W.1"),
                      mltext("agregationTableParCritere.f.W.2"), sep=""))
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
        stop("Negative abundances!")
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
calcBiodiv.f <- function(Data, refesp, MPA, unitobs="observation.unit", code.especes="species.code", nombres="number",
                         indices="all", global=FALSE, printInfo=FALSE, dataEnv=.GlobalEnv)
{
    ## Purpose: calcul des indices de biodiversité
    ## ----------------------------------------------------------------------
    ## Arguments: Data : les données à partir desquelles calculer les
    ##                   indices. Doivent comporter au minimum (colones) :
    ##                     * unités d'observations/sites
    ##                     * espèces présentes
    ##                     * nombre d'individus /espèce/unitobs.
    ##            refesp : le référentiel espèces.
    ##            MPA : l'AMP (chaîne de charactères).
    ##            unitobs : nom de la colone d'unités d'observation.
    ##            code.especes : nom de la colone d'espèces.
    ##            nombres : nom de la colone de nombres.
    ##            indices : liste des indices à calculer
    ##                      (vecteur de caractères)
    ##            global : est-ce que les résultats doivent être exportés
    ##                     globalement (booléen).
    ##            printInfo : affichage des infos (chargement) ? (booléen).
    ##            dataEnv : environnement des données
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 29 oct. 2010, 08:58

    ## Unitobs appartenant a l'AMP courante:
    unitobsData <- get("unitobs", envir=dataEnv)

    Data <- subset(Data,
                   is.element(Data[ , unitobs],
                              unitobsData[unitobsData[ , getOption("P.MPAfield")] == MPA ,
                                          unitobs]))

    DataTmp <- Data

    ## Supression de tout ce qui n'a pas d'espèce précisee (peut être du non biotique ou identification >= genre) :
    if (! nrow(Data <- Data[(spTmp <- refesp$species[match(Data[ , code.especes], refesp$species.code)]) != "sp." &
                            !is.na(spTmp), ]))
    {
        if (printInfo)
        {
            infoLoading.f(msg = paste(mltext("calcBiodiv.f.info.1")## ,
                                      ## "\n   La table de contingence n'a pas été calculée."
                          ), icon = "warning")
        }else{}

        return(Data)
    }else{}

    ## Suppression des niveaux de facteur inutilisés :
    Data <- dropLevels.f(df=Data)

    if (printInfo)
    {
        if (nlevels(DataTmp[ , code.especes]) > nlevels(Data[ , code.especes]))
        {
            nsup <- nlevels(DataTmp[ , code.especes]) - nlevels(Data[ , code.especes])
            infoLoading.f(msg=paste(nsup, " \"species.code\" ",
                                    ifelse(nsup > 1 ,
                                           mltext("calcBiodiv.f.info.2.p"),
                                           mltext("calcBiodiv.f.info.2.s")),
                                    mltext("calcBiodiv.f.info.3"),
                                    ifelse(nsup > 1,
                                           mltext("calcBiodiv.f.info.4.p"),
                                           mltext("calcBiodiv.f.info.4.s")),
                                    mltext("calcBiodiv.f.info.5"),
                                    sep=""))
        }else{}
    }else{}

    ## Si les données ne sont pas encore agrégées /espèce/unitobs on le fait ici :
    if (nrow(Data) > nrow(expand.grid(unique(Data[ , unitobs]), unique(Data[ , code.especes]))))
    {
        Data <- agregations.generic.f(Data=Data, metrics=nombres,
                                      factors=c(unitobs, code.especes),
                                      listFact=NULL, dataEnv=dataEnv)
    }else{}

    df.biodiv <- as.data.frame(as.table(tapply(Data[ , nombres],
                                               Data[ , unitobs],
                                               sum, na.rm=TRUE)))

    colnames(df.biodiv) <- c(unitobs, nombres)

    ## ##################################################
    ## Richesse spécifique :
    Data$pres.abs <- presAbs.f(nombres=Data[ , nombres], logical = FALSE)

    df.biodiv$species.richness <- as.vector(tapply(Data$pres.abs,
                                                   Data[ , unitobs], sum, na.rm=TRUE),
                                            "integer")
    ## ... as.vector to avoid the class "array".

    ## richesses specifiques relatives :

    ## Phylum(s) présent(s) dans le jeux de données :
    phylums <- as.character(unique(na.omit(refesp$phylum[match(Data[ , code.especes],
                                                               refesp$species.code)])))

    ## RS relative par rapp. au nombre d'espèces du site :
    if (any(is.element(c("all", "relative.SR.site"), indices)))
    {
        if (getOption("P.refesp.Coefs") == "new")
        {
            ## Nouveau référentiel espèce ET fichier local chargé :
            if (is.element("observed", colnames(refesp)))
            {
                df.biodiv$relative.SR.site <- (df.biodiv$species.richness /
                                               nrow(subset(refesp,
                                                           is.element(observed, c("oui", "O"))))) * 100
            }else{}
        }else{
            df.biodiv$relative.SR.site <- (df.biodiv$species.richness /
                                           nrow(subset(refesp,
                                                       is.element(eval(parse(text=paste("Obs", MPA, sep=""))),
                                                                  c("oui", "O"))))) * 100
        }
    }

    ## RS relative par rapp. au nombre d'espèces du site et du(des) phylum(s) concerné(s) (jeu de données) :
    if (any(is.element(c("all", "relative.SR.site.phylum"), indices)))
    {
        if (getOption("P.refesp.Coefs") == "new")
        {
            ## Nouveau référentiel espèce ET fichier local chargé :
            if (is.element("observed", colnames(refesp)))
            {
                df.biodiv$relative.SR.site.phylum <- (df.biodiv$species.richness /
                                                      nrow(subset(refesp,
                                                                  is.element(observed, c("oui", "O", "yes", "Y")) &
                                                                  is.element(phylum, phylums)))) * 100 # [ml?]
            }else{}
        }else{
            df.biodiv$relative.SR.site.phylum <- (df.biodiv$species.richness /
                                                  nrow(subset(refesp,
                                                              is.element(eval(parse(text=paste("Obs", MPA, sep=""))),
                                                                         c("oui", "O", "yes", "Y")) &
                                                              is.element(phylum, phylums)))) * 100
        }
    }

    ## RS relative par rapp. au nombre d'espèces des données :
    if (any(is.element(c("all", "relative.SR.data"), indices)))
    {
        df.biodiv$relative.SR.data <- (df.biodiv$species.richness /
                                       nrow(subset(refesp,
                                                   is.element(species.code, Data[ , code.especes])))) * 100
    }

    ## ## RS relative par rapp. au nombre d'espèces des données + des phyla présents :
    ## Inutile : "RS.relative.donnees" est par définition limitée au phyla présents !

    ## RS relative par rapp. au nombre d'espèces au niveau régional (OM ou méditerrannée) :
    if (any(is.element(c("all", "relative.SR.region"), indices)))
    {
        df.biodiv$relative.SR.region <- (df.biodiv$species.richness /
                                         nrow(refesp)) * 100
    }

    ## RS relative par rapp. au nombre d'espèces au niveau régional (OM ou méditerrannée) et
    ## du(des) phylum(s) concerné(s) (jeu de données) :
    if (any(is.element(c("all", "relative.SR.region.phylum"), indices)))
    {
        df.biodiv$relative.SR.region.phylum <- (df.biodiv$species.richness /
                                                nrow(subset(refesp, is.element(phylum, phylums)))) * 100
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

    if (any(is.element(c("all", "simpson.l"), indices)))
    {
        df.biodiv$simpson.l <- 1 - df.biodiv$simpson
    }

    ## calcul de l'indice de Shannon :
    df.biodiv$shannon <- -1 * apply(propIndiv * log(propIndiv), 1, sum, na.rm=TRUE)

    ## calcul de l'indice de Pielou :
    if (any(is.element(c("all", "pielou"), indices)))
    {
        df.biodiv$pielou <- df.biodiv$shannon / log(df.biodiv$species.richness)
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
                                      refesp=refesp,
                                      unitobs = unitobs, code.especes = code.especes, nombres = nombres,
                                      global = global, printInfo = printInfo,
                                      indices=indices,
                                      dataEnv=dataEnv)

    if (!is.null(dim(df.biodivTaxo)))
    {
        df.biodiv <- cbind(df.biodiv,
                           df.biodivTaxo[match(df.biodiv[ ,unitobs], row.names(df.biodivTaxo)), , drop=FALSE])
    }else{}

    for (ind in c("simpson", "shannon", "species.richness"))
    {
        if (! any(is.element(c(ind, "all"), indices)))
        {
            df.biodiv[ , ind] <- NULL
        }else{}
    }

    ## ## On retablit les niveaux de facteurs:
    ## colFact <- colnames(df.biodiv)[is.element(sapply(df.biodiv, class), "factor")]

    ## for (col in colFact)
    ## {
    ##     levels(df.biodiv[ , colFact]) <- levels(DataTmp[ , colFact])
    ## }

    return(df.biodiv)
}

########################################################################################################################
calcBiodivTaxo.f <- function(Data, refesp, unitobs="observation.unit", code.especes="species.code", nombres="number",
                             global=FALSE, printInfo=FALSE,
                             indices="all", dataEnv=.GlobalEnv)
{
    ## Purpose: Calcul des indices de biodiversité basés sur la taxonomie.
    ## ----------------------------------------------------------------------
    ## Arguments: Data : les données à partir desquelles calculer les
    ##                   indices. Doivent comporter au minimum (colones) :
    ##                     * unités d'observations/sites
    ##                     * espèces présentes
    ##                     * nombre d'individus /espèce/unitobs.
    ##            refesp : référentiel espèces.
    ##            unitobs : nom de la colone d'unités d'observation.
    ##            especes : nom de la colone d'espèces.
    ##            nombres : nom de la colone de nombres.
    ##            global : est-ce que les résultats doivent être exportés
    ##                     globalement (booléen).
    ##            printInfo : affichage des infos ? (booléen).
    ##            indices : liste des indices à calculer
    ##                      (vecteur de caractères), tous par défaut.
    ##            dataEnv : environnement des données
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 29 oct. 2010, 14:30

    ## Indices proposés :
    proposed.indices <- c("D"="Delta",
                          "Dstar"="DeltaStar",
                          "Lambda"="LambdaPlus",
                          "Dplus"="DeltaPlus",
                          "SDplus"="SDeltaPlus")

    ## On sort de la fonction si elle n'a pas d'intéret :
    if (! any(is.element(c(proposed.indices, "all"), indices)))
    {
        return(NULL)                    # Rien !
    }else{
        ## Suppression de tout ce qui n'a pas de genre (peut être du non biotique) :
        Data <- Data[refesp$species[match(Data$species.code, refesp$species.code)] != "sp.", ]

        ## Suppression des niveaux de facteur inutilisés :
        Data <- dropLevels.f(df=Data)

        ## Si les données ne sont pas encore agrégées /espèce/unitobs on le fait ici :
        if (nrow(Data) > nrow(expand.grid(unique(Data[ , unitobs]), unique(Data[ , code.especes]))))
        {
            Data <- agregations.generic.f(Data=Data, metrics=nombres,
                                          factors=c(unitobs, code.especes),
                                          listFact=NULL, dataEnv=dataEnv)
        }else{}

        ## Table de contingence unitobs-espèces :
        contingence <- tapply(Data[ , nombres],
                              list(Data[ , unitobs], Data[ , code.especes]),
                              sum, na.rm=TRUE)

        contingence[is.na(contingence)] <- 0 # Vrais zéros.

        ## tableau avec genre, famille, etc.
        sp.taxon <- dropLevels.f(refesp[match(colnames(contingence),
                                              refesp$species.code, nomatch=NA, incomparables = FALSE),
                                        c("species", "genus", "family", "order", "class", "phylum")])

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
            if (!is.null(taxdis <- tryCatch(taxa2dist(sp.taxon, varstep=TRUE, check=TRUE),
                                            error=function(e)
                                        {
                                            if (printInfo)
                                            {
                                                infoLoading.f(msg=mltext("calcBiodivTaxo.f.info.1"),
                                                              icon="warning")
                                            }else{}

                                            ## errorLog.f(error=e, niv = -3)
                                            return(NULL)
                                        })))
            {
                ## Function finds indices of taxonomic diversity and distinctiness, which are averaged taxonomic distances among
                ## species or individuals in the community...
                divTaxo <- taxondive(contingence, taxdis)

                ## mise de divTaxo sous forme de data.frame :
                df.biodivTaxo <- as.data.frame(divTaxo[names(retained.indices)])

                colnames(df.biodivTaxo) <- retained.indices # [!!!] "LambdaPlus" ? vraiment ? [???]

                ## affichage des valeurs attendues :
                if (printInfo)
                {
                    message(paste(mltext("calcBiodivTaxo.f.msg.1") , round(divTaxo[["ED"]], 3)))
                    message(paste(mltext("calcBiodivTaxo.f.msg.2") , round(divTaxo[["EDstar"]], 3)))
                    message(paste(mltext("calcBiodivTaxo.f.msg.3") , round(divTaxo[["EDplus"]], 3)))
                }else{}
            }else{
                divTaxo <- NULL
                df.biodivTaxo <- NULL
            }

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
                       warning(mltext("calcBiodivTaxo.f.Warn.1"))
                   },
                   "0"={
                       warning(mltext("calcBiodivTaxo.f.Warn.2"))
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

    tkwm.title(WinInfo, mltext("agregation.info.f.title"))

    tkgrid(tklabel(WinInfo, text="\t "),
           tklabel(WinInfo, text=mltext("agregation.info.f.info.1")),
           tklabel(WinInfo, text="\t "),
           sticky="w")
    tkgrid(tklabel(WinInfo, text="\t "),
           tklabel(WinInfo,
                   text=paste(mltext("agregation.info.f.info.2"), sep="")),
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
