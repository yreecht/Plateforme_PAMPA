#-*- coding: latin-1 -*-
# Time-stamp: <2018-09-04 10:00:44 yreecht>

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

### File: Turtle_tracks.R
### Created: <2013-01-17 17:35:11 yves>
###
### Author: Yves Reecht
###
####################################################################################################
### Description: Scripts spécifiques pour le chargement et les calculs de métriques relatives
###              aux traces de tortues (protocole Caraïbes ; type d'observation TRATO).
###
####################################################################################################

########################################################################################################################
obsFormatting.TRATO.f <- function(obs)
{
    ## Purpose: Mise en forme et nettoyage du fichier d'observations pour les
    ##          données d'observation de types TRATO
    ## ----------------------------------------------------------------------
    ## Arguments: obs : table de données d'observations.
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 17 janv. 2013, 17:40

    ## Renommage des colonnes "nombre" et "dmin" en "nombre.traces" et "ponte" :
    colnames(obs)[match(c("nombre", "dmin"), colnames(obs))] <- c("nombre.traces", "ponte")

    ponte <- gsub("[[:blank:]]*", "", tolower(obs[ , "ponte"]))

    ponte[ponte == "nl"] <- "NL"
    ponte[ponte == ""] <- NA

    ## Recherche des données incorrectes => NAs :
    if (sum(tmp <- ! (is.na(ponte) | is.element(ponte,
                                                c(paste0(rep(c(tolower(mltext("KW.yes")),
                                                               tolower(mltext("KW.no"))), each = 2),
                                                         c("", "?")),
                                                  "NL"))))) # [ml?]                                               
    {
        ## Changées en NAs :
        ponte[tmp] <- NA

        ## Message au pluriel ?
        pl <- sum(tmp) > 1

        ## Message d'avertissement :
        infoLoading.f(msg=paste(ifelse(pl,
                                       mltext("obsFormatting.TRATO.info.1p"),
                                       mltext("obsFormatting.TRATO.info.1s")),
                                sum(tmp),
                                ifelse(pl,
                                       mltext("obsFormatting.TRATO.info.2p"),
                                       mltext("obsFormatting.TRATO.info.2s")),
                                ifelse(pl,
                                       mltext("obsFormatting.TRATO.info.3p"),
                                       mltext("obsFormatting.TRATO.info.3s")),
                                sep=""),
                      icon="warning")
    }else{}

    ## Sauvegarde dans obs :
    obs[ , "ponte"] <- factor(ponte)

    return(obs)
}

########################################################################################################################
calc.nestingSuccess.f <- function(obs,
                                  Data,
                                  factors=c("unite_observation", "code_espece", "classe_taille"),
                                  nbName="nombre")
{
    ## Purpose: Calcul du pourcentage de réussite de ponte.
    ## ----------------------------------------------------------------------
    ## Arguments: obs : table des observations (data.frame).
    ##            Data : la table de métrique (temporaire).
    ##            factors : les facteurs d'agrégation.
    ##            nbName : nom de la colonne nombre.
    ##
    ## Output: vecteur des réussites de pontes (%).
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 18 janv. 2013, 15:57

    ## Nombre de pontes (sûres + supposées) :
    pontes <- as.vector(tapply(subset(obs, grepl(paste0("^", mltext("KW.yes"), "\\??$"),
                                                 obs$ponte))[ , "nombre"],
                               as.list(subset(obs, grepl(paste0("^", mltext("KW.yes"), "\\??$"),
                                                         obs$ponte))[ , factors]),
                               FUN = function(x)
                               {
                                   ifelse(all(is.na(x)),
                                          as.numeric(NA),
                                   ifelse(all(is.element(na.omit(x), "NL")), # [ml?]
                                          0,
                                          sum(x, na.rm=TRUE)))
                               }))


    ## Correction de NAs à la place de 0 dans pontes lorsque aucune traces observées mais nombre valide (0) :
    pontes[is.na(pontes) & ! is.na(Data[ , nbName])] <- 0

    ## Nombre de traces lisibles :
    traces.lisibles <- as.vector(tapply(subset(obs,
                                               grepl(paste0("^(", mltext("KW.yes"), "|",
                                                            mltext("KW.no"), ")\\??$"), # "^(yes|no)\\??$"
                                                     obs$ponte))[ , "nombre"],
                                        as.list(subset(obs,
                                                       grepl(paste0("^(", mltext("KW.yes"), "|",
                                                                    mltext("KW.no"), ")\\??$"),
                                                             obs$ponte))[ , factors]),
                                        FUN = function(x)
                                        {
                                            ifelse(all(is.na(x)),
                                                   as.numeric(NA),
                                            ifelse(all(is.element(na.omit(x), "NL")),
                                                   0,
                                                   sum(x, na.rm=TRUE)))
                                        }))

    ## Correction de NAs à la place de 0 dans traces lisibles lorsque aucune traces observées mais nombre valide (0) :
    traces.lisibles[is.na(traces.lisibles) & ! is.na(Data[ , nbName])] <- 0

    return(data.frame("pontes"=pontes, "traces.lisibles"=traces.lisibles,
                      "reussite.ponte"=100 * pontes / traces.lisibles))
}


########################################################################################################################
calc.tables.TurtleTracks.f <- function(obs, unitobs, dataEnv, factors)
{
    ## Purpose: Calcul de métriques à partir de traces de tortues.
    ## ----------------------------------------------------------------------
    ## Arguments: obs : table des observations (data.frame).
    ##            unitobs : table des unités d'observation (data.frame).
    ##            dataEnv : environnement des données.
    ##            factors : les facteurs d'agrégation.
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 17 janv. 2013, 19:29

    ## Calcul des nombres par cl / espèces / unitobs :
    nbr <- calcNumber.default.f(obs, factors=factors, nbName="nombre.traces")

    ## Création de la data.frame de résultats (avec nombres, unitobs, ):
    res <- calc.numbers.f(nbr, nbName="nombre.traces")

    ## Calcul du succès de ponte (%) :
    res <- cbind(res, calc.nestingSuccess.f(obs=obs, Data=res, factors=factors, nbName="nombre.traces"))

    ## Tailles moyennes :
    res[ , "taille_moyenne"] <- calc.meanSize.f(obs, factors=factors, nbName="nombre.traces")

    ## Poids :
    res[ , "poids"] <- calc.weight.f(obs=obs, Data=res, factors=factors, nbName="nombre.traces")

    ## Poids moyen par individu :
    res[ , "poids_moyen"] <- calc.meanWeight.f(Data=res, nbName="nombre.traces")

    ## Présence/absence :
    res[ , "pres_abs"] <- calc.presAbs.f(Data=res, nbName="nombre.traces")

    return(res)
}






### Local Variables:
### ispell-local-dictionary: "english"
### fill-column: 120
### End:
