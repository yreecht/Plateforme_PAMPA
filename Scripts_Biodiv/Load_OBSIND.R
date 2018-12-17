#-*- coding: latin-1 -*-
# Time-stamp: <2018-12-17 15:33:42 yreecht>

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

### File: Load_OBSIND.R
### Created: <2013-04-25 12:12:23 yves>
###
### Author: Yves Reecht
###
####################################################################################################
### Description:
###
### Chargement des données de type OBSIND (observation ponctuelle et géolocalisée d'individus).
####################################################################################################


unitobsNew.OBSIND.create.f <- function(unitobs, refspa, dataEnv)
{
    ## Purpose: Création d'une nouvelle table d'unités d'observations pour
    ##          les observations ponctuelles géoréférencées (OBSIND).
    ## ----------------------------------------------------------------------
    ## Arguments: unitobs : table des unités d'observations
    ##                      (réduite à ce stade).
    ##            refspa : référentiel spatial sous forme de shapefile.
    ##            dataEnv : environnmeent des données.
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 25 avril 2013, 16:04

    ## lien entre les unitobs "individuelles" et le référentiel spatial :
    unitobsTmp <- overlayUnitobs.f(unitobs=unitobs, refspa=refspa)

    ## Construction d'un nouvel indice d'unités d'observations (intersection zone - jour
    ## d'observation ; dites "regroupées") :
    unitobsTmp$unitobsNew <- paste("Z", unitobsTmp$OBJECTID, "-J", unitobsTmp$day, "-",
                                   unitobsTmp$month, "-", unitobsTmp$year, sep="")

    ## Sauvegarde d'une table de correspondance entre unités d'observations "individuelles" et
    ## "regroupées" dans l'environnement des données :
    assign(x=".unitobsCorresp",
           value=unitobsTmp[ , c("observation.unit", "unitobsNew")],
           envir=dataEnv)

    ## Suppression des unités d'observation hors polygones :
    unitobsTmp <- unitobsTmp[ ! is.na(unitobsTmp$OBJECTID), ]

    ## Réorganisation des colonnes :
    unitobsTmp <- unitobsTmp[ , c("study.area", "unitobsNew",
                                 "observation.type", "site", "station", "geogr.descriptor1",
                                  "geogr.descriptor2", "sampling.rate", "day", "month",
                                  "year", "hour", "cloud.cover", "wind.direction", "wind.strength",
                                  "sea.condition", "current", "tide", "moon.phase", "latitude",
                                  "longitude", "protection.status", "before.after", "biotop",
                                  "biotop.2", "habitat1", "habitat2", "habitat3", "visibility",
                                  "min.depth", "max.depth", "obs.dim1", "obs.dim2", "nb.divers",
                                  "diver", "OBJECTID")]

    colnames(unitobsTmp)[2] <- "observation.unit"

    ## Agrégations (une ligne par unité d'observation) :
    unitobsNew <- do.call(rbind,
                          lapply(split(unitobsTmp, unitobsTmp[ , "observation.unit"]),
                                 aggreg.unitobsNew.f,
                                 refspa=refspa))

    return(unitobsNew)
}


########################################################################################################################
aggreg.unitobsNew.f <- function(x, refspa)
{
    ## Purpose: Agréger des parties de la nouvelle table des unitobs pour
    ##          n'avoir plus qu'une ligne par unitobs.
    ## ----------------------------------------------------------------------
    ## Arguments: x : sous-table des unitobs avec les données d'une unitobs.
    ##            refspa : le référentiel spatial sous forme de shapefile.
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 25 avril 2013, 17:04

    ## Colonnes ne bénéficiant pas d'un traitement spécial :
    idx <- which(! is.element(colnames(x),
                              c("latitude", "longitude", "visibility", "min.depth", "max.depth",
                                "obs.dim1", "obs.dim2", "diver")))

    ## On conserve la valeur si c'est la même partout dans la colonne :
    x[ , idx] <- sapply(x[ , idx],
                        function(y)
                    {
                        if (length(unique(y)) == 1)
                        {
                            return(y)
                        }else{
                            return(rep(NA, length(y)))
                        }
                    }, simplify=FALSE)

    ## Longitude/latitude = barycentre du polygone :
    x[ , c("latitude", "longitude")] <- refspa@data[x[ , "OBJECTID"],
                                                    c("SITE.centrY", "SITE.centrX")]

    ## Colonnes pour lesquelles on prend le minimum :
    x[ , c("visibility", "min.depth")] <- sapply(x[ , c("visibility", "min.depth")],
                                                function(x)
                                            {
                                                if (all(is.na(x)))
                                                {
                                                    NA
                                                }else{
                                                    min(x, na.rm=TRUE)
                                                }
                                            },
                                                simplify=FALSE)

    ## Colonne pour laquelle on prend le maximum :
    x[ , "max.depth"] <- if (all(is.na(x[ , "max.depth"])))
      {
          NA
      }else{
          max(x[ , "max.depth"], na.rm=TRUE)
      }

    ## Dimobs en km² :
    x[ , "obs.dim1"] <- refspa@data[x[ , "OBJECTID"],
                                   c("SITE.SURFACE")]

    x[ , "obs.dim2"] <- 1

    ## Plusieurs observateurs possibles :
    x[ , "nb.divers"] <- length(unique(x[ , "diver"]))
    x[ , "diver"] <- paste(unique(x[ , "diver"]), collapse=", ")

    return(x[1, !is.element(colnames(x), "OBJECTID"), drop=FALSE])
}





### Local Variables:
### ispell-local-dictionary: "english"
### fill-column: 100
### End:
