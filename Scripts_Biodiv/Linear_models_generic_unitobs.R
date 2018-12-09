#-*- coding: latin-1 -*-
# Time-stamp: <2018-12-09 09:42:29 yreecht>

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

### File: Linear_models_generic_unitobs.R
### Created: <2012-01-11 19:27:55 yreecht>
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
modeleLineaireWP2.unitobs.f <- function(metrique, factAna, factAnaSel, listFact, listFactSel, tableMetrique, dataEnv,
                                        baseEnv=.GlobalEnv)
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
    ##            dataEnv : environnement de stockage des données.
    ##            baseEnv : environnement de l'interface.
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
    if (tableMetrique == "unit")
    {
        ## Pour les indices de biodiversité, il faut travailler sur les nombres... :
        tmpData <- subsetToutesTables.f(metrique=getOption("P.nbName"), facteurs=facteurs,
                                        selections=selections, dataEnv=dataEnv, tableMetrique="unitSp",
                                        exclude = NULL, add=c("unite_observation", "code_espece"))
    }else{
        ## ...sinon sur la métrique choisie :
        tmpData <- subsetToutesTables.f(metrique=metrique, facteurs=facteurs, selections=selections,
                                        dataEnv=dataEnv, tableMetrique=tableMetrique, exclude = NULL,
                                        add=c("unite_observation", "code_espece"))
    }

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
    if (tableMetrique == "unitSpSz" && factAna != "classe_taille")
    {
        tmpData <- na.omit(agregationTableParCritere.f(Data=tmpData,
                                                       metrique=metrique,
                                                       facteurs=c("unite_observation", "classe_taille"),
                                                       dataEnv=dataEnv,
                                                       listFact=listFact))
    }else{
        if (tableMetrique == "unit")
        {
            ## Calcul des indices de biodiversité sur sélection d'espèces :
            tmp <- do.call(rbind,
                           lapply(getOption("P.MPA"),
                                  function(MPA)
                              {
                                  calcBiodiv.f(Data=tmpData,
                                               refesp=get("refesp", envir=dataEnv),
                                               MPA=MPA,
                                               unitobs = "unite_observation", code.especes = "code_espece",
                                               nombres = getOption("P.nbName"),
                                               indices=metrique,
                                               dataEnv=dataEnv)
                              }))

            ## On rajoute les anciennes colonnes :
            tmpData <- cbind(tmp[ , colnames(tmp) != getOption("P.nbName")], # Colonne "nombre" désormais inutile.
                             tmpData[match(tmp$unite_observation, tmpData$unite_observation),
                                     !is.element(colnames(tmpData),
                                                 c(colnames(tmp), getOption("P.nbName"), "code_espece")), drop=FALSE])

            ## On garde le strict minimum :
            tmpData <- tmpData[ , is.element(colnames(tmpData), c(metrique, facteurs))]
        }else{
            tmpData <- na.omit(agregationTableParCritere.f(Data=tmpData,
                                                           metrique=metrique,
                                                           facteurs=c("unite_observation"),
                                                           dataEnv=dataEnv,
                                                           listFact=listFact))
        }
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
        message(mltext("modeleLineaireWP2.esp.dist"), " = ", loiChoisie)

        if (is.element(loiChoisie, c("LOGNO")))
        {
            Log <- TRUE
            formule <- logExprML
        }else{
            Log <- FALSE
            formule <- exprML
        }

        res <- calcLM.f(loiChoisie=loiChoisie, formule=formule, metrique=metrique, Data=tmpData)

        ## Écriture des résultats formatés dans un fichier :
        tryCatch(sortiesLM.f(objLM=res, formule=formule, metrique=metrique,
                             factAna=factAna, modSel=iFactGraphSel,
                             listFact=listFact, listFactSel=listFactSel,
                             Data=tmpData, dataEnv=dataEnv, Log=Log,
                             type=ifelse(tableMetrique == "unitSpSz" && factAna != "classe_taille",
                                         "CL_unitobs",
                                         "unitobs"),
                             baseEnv=baseEnv),
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
                                     factAna=factAna, modSel=iFactGraphSel,
                                     listFact=listFact, listFactSel=listFactSel,
                                     Data=tmpData, Log=Log, sufixe="(red)",
                                     type=ifelse(tableMetrique == "unitSpSz" && factAna != "classe_taille",
                                                 "CL_unitobs",
                                                 "unitobs"),
                                     dataEnv=dataEnv, baseEnv=baseEnv),
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
