#-*- coding: latin-1 -*-

### File: modeles_lineaires_unitobs_generiques.R
### Time-stamp: <2010-12-22 17:07:31 yreecht>
###
### Author: Yves Reecht
###
####################################################################################################
### Description:
###
### Modèles linéaires pour les données agrégées par unitobs (toutes espèces, éventuellement
### sélectionnées selon un critère du référentiel).
####################################################################################################


########################################################################################################################
modeleLineaireWP2.unitobs.f <- function(metrique, factAna, factAnaSel, listFact, listFactSel, tableMetrique)
{
    ## Purpose: Gestions des différentes étapes des modèles linéaires.
    ## ----------------------------------------------------------------------
    ## Arguments: metrique : la métrique choisie.
    ##            factAna : le facteur de séparation des graphiques.
    ##            factAnaSel : la sélection de modalités pour ce dernier
    ##            listFact : liste du (des) facteur(s) de regroupement
    ##            listFactSel : liste des modalités sélectionnées pour ce(s)
    ##                          dernier(s)
    ##            tableMetrique : nom de la table de métriques.
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 18 août 2010, 15:59

    ## Nettoyage des facteurs (l'interface de sélection produit des valeurs vides) :
    listFactSel <- listFactSel[unlist(listFact) != ""]
    ## listFactSel <- listFactSel[length(listFactSel):1]

    listFact <- listFact[unlist(listFact) != ""]
    ## listFact <- listFact[length(listFact):1]

    ## Concaténation
    facteurs <- c(factAna, unlist(listFact)) # Concaténation des facteurs

    selections <- c(list(factAnaSel), listFactSel) # Concaténation des leurs listes de modalités sélectionnées

    ## Données pour la série d'analyses :
    tmpData <- subsetToutesTables.f(metrique=metrique, facteurs=facteurs, selections=selections,
                                    tableMetrique=tableMetrique, exclude = NULL,
                                    add=c("unite_observation", "code_espece"))

    ## Identification des différents lots d'analyses à faire:
    if (factAna == "")                # Pas de facteur de séparation des graphiques.
    {
        iFactGraphSel <- ""
    }else{
        if (is.na(factAnaSel[1]))            # Toutes les modalités.
        {
            iFactGraphSel <- unique(as.character(sort(tmpData[ , factAna])))
        }else{                              # Modalités sélectionnées (et présentes parmi les données retenues).
            iFactGraphSel <- factAnaSel[is.element(factAnaSel, tmpData[ , factAna])]
        }
    }

    ## Formules pour différents modèles (avec ou sans transformation log) :
    exprML <- eval(parse(text=paste(metrique, "~", paste(listFact, collapse=" * "))))
    logExprML <- eval(parse(text=paste("log(", metrique, ") ~", paste(listFact, collapse=" * "))))

    ## Agrégation des observations / unité d'observation :
    if (tableMetrique == "unitespta" && factAna != "classe_taille")
    {
        tmpData <- na.omit(agregationTableParCritere.f(Data=tmpData,
                                                       metrique=metrique,
                                                       facteurs=c("unite_observation", "classe_taille"),
                                                       listFact=listFact))
    }else{
        tmpData <- na.omit(agregationTableParCritere.f(Data=tmpData,
                                                       metrique=metrique,
                                                       facteurs=c("unite_observation"),
                                                       listFact=listFact))
    }

    ## Sauvegarde temporaire des données utilisées pour les analyses (attention : écrasée à chaque nouvelle série de
    ## graphiques) :
    DataBackup <<- list(tmpData)


    ## Suppression des 'levels' non utilisés :
    tmpData <- dropLevels.f(tmpData)

    ## Aide au choix du type d'analyse :
    if (metrique == "pres_abs")
    {
        loiChoisie <- "BI"
    }else{
        loiChoisie <- choixDistri.f(metrique=metrique, Data=tmpData[ , metrique, drop=FALSE])
    }

    if (!is.null(loiChoisie))
    {
        message("Loi de distribution choisie = ", loiChoisie)

        if (is.element(loiChoisie, c("LOGNO")))
        {
            Log <- TRUE
            formule <- logExprML
        }else{
            Log <- FALSE
            formule <- exprML
        }

        res <- calcLM.f(loiChoisie=loiChoisie, formule=formule, metrique=metrique, Data=tmpData)

        resLM <<- res

        tryCatch(sortiesLM.f(objLM=res, formule=formule, metrique=metrique,
                             factAna=factAna, modSel=iFactGraphSel, listFact=listFact,
                             Data=tmpData, Log=Log, type="unitobs"),
                 error=errorLog.f)

        resid.out <- boxplot(residuals(res), plot=FALSE)$out

        if (length(resid.out))
        {
            suppr <- supprimeObs.f(residus=resid.out)

            if(!is.null(suppr))
            {
                if (!is.numeric(suppr)) # conversion en numéros de lignes lorsque ce sont des noms :
                {
                    suppr <- which(is.element(row.names(tmpData), suppr))
                }else{}

                tmpData <- tmpData[ - suppr, ]
                res.red <- calcLM.f(loiChoisie=loiChoisie, formule=formule, metrique=metrique, Data=tmpData)

                resLM.red <<- res.red

                tryCatch(sortiesLM.f(objLM=res.red, formule=formule, metrique=metrique,
                                     factAna=factAna, modSel=iFactGraphSel, listFact=listFact,
                                     Data=tmpData, Log=Log, sufixe="(red)", type="unitobs"),
                         error=errorLog.f)
            }else{}

        }else{}


    }else{
        message("Annulé !")
    }
}






### Local Variables:
### ispell-local-dictionary: "english"
### fill-column: 120
### End:
