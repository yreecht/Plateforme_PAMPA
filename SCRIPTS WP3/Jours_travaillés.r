#-*- coding: latin-1 -*-

### File: dates.R
### Time-stamp: <2011-07-13 11:52:36 yreecht>
###
### Author: Yves Reecht
###
####################################################################################################
### Description:
###
### Calcul des nombres de jours travaillés et chômés par mois en fonction de l'année/année
### de campagne.
####################################################################################################

library(timeDate)

########################################################################################################################
## Holidays :
FRHolidays <- function(year=getRmetricsOptions("currentYear"))
{
    ## Purpose: Retourne un objet avec les dates des jours fériés pour la
    ##          France (et l'OM).
    ## ----------------------------------------------------------------------
    ## Arguments: year : l'année
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 12 juil. 2011, 16:57

    res <- as.timeDate(                 # nécessaire pour l'utilisation avec isBizday().
                       c(
                         ## Jours fériés communs en France :
                         as.Date(sapply(c(listHolidays("FR"), # Liste des jours fériés FR.
                                          "NewYearsDay",     #
                                          "EasterMonday",    # Jours non listés
                                          "Ascension",       # par la fonction
                                          "PentecostMonday", # ci-dessus.
                                          "ChristmasDay"),   #
                                        function(x, year) as.Date(do.call(x, list(year=year))),
                                        year=year),
                                 origin="1970-01-01"),
                         ## Jours spécifiques à l'Outre-mer (à compléter) :
                         if(siteEtudie == "NC"){as.Date(paste(year, "-09-24", sep=""))}else{NULL},
                         if(siteEtudie == "NC" && year == 2008){as.Date(paste(year, "-06-26", sep=""))}else{NULL},
                         if(siteEtudie == "RUN"){as.Date(paste(year, "-12-20", sep=""))}else{NULL}))
}


########################################################################################################################
nbBizDays <- function(years=getRmetricsOptions("currentYear"),
                      inverse=FALSE)
{
    ## Purpose: Compte du nombre de jours travaillés/chômés
    ##          par mois par année
    ## ----------------------------------------------------------------------
    ## Arguments: years : les années.
    ##            inverse : (logical) inversion ? (TRUE => jours chômés).
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 12 juil. 2011, 17:03

    mat <- sapply(X=years,              # ...pour chaque année...
                  FUN=function(year)
              {
                  sapply(1:12,          # ...pour chaque mois...
                         function(month, year)
                     {
                         ## Liste de jours du mois :
                         x <- na.omit(as.Date(paste(year, "-", month, "-", 1:31, sep="")))

                         ## Jours travaillés ? (Biz pour "business" ; logical) :
                         res <- isBizday(as.timeDate(x), holiday=FRHolidays(year=year))

                         ## Résultats chiffrés :
                         if(inverse)
                         {
                             return(sum(!res)) # NON travaillés.
                         }else{
                             return(sum(res)) # Travaillés.
                         }
                     },
                         year=year)     # second argument de la fonction dans second sapply.
              }
                  )

    ## On nomme les colonnes et les lignes pour plus de lisibilité :
    colnames(mat) <- as.character(years)
    row.names(mat) <- as.character(1:12)

    return(mat)
}

########################################################################################################################
## Exemples sur des données WP2 (adapter les noms d'objets, de colonnes, etc.) :

## Nombre de jours travaillés :
nbTrav <- nbBizDays(years=unique(as.numeric(as.character(freqtot$annee))),
                    inverse=FALSE)

## Nombre de jours chômés :
nbChom <- nbBizDays(years=unique(as.numeric(as.character(freqtot$annee))),
                    inverse=TRUE)

## Vérif. :
nbTot <- nbTrav + nbChom

## Jours travaillés et chômés pour le mois de chaque unité d'observation :
travailles <- sapply(1:nrow(freqtot),
                     function(i)
                 {
                     nbTrav[freqtot[i, "mois"], as.character(freqtot[i, "annee"])]
                 })

chomes <- sapply(1:nrow(freqtot),
                 function(i)
                 {
                     nbChom[freqtot[i, "mois"], as.character(freqtot[i, "annee"])]
                 })

## On peut dès lors recalculer les nombres de jours travaillés et chômés par mois, en fonction
## de l'année de campagne/période échantillonnées (si un même mois sur deux années => moyenne) :

## Travaillés :
tapply(travailles, list(freqtot$mois, freqtot$periodEchant), mean, na.rm=TRUE)

## Chômés :
tapply(chomes, list(freqtot$mois, freqtot$periodEchant), mean, na.rm=TRUE)

## Total :
tapply(chomes + travailles, list(freqtot$mois, freqtot$periodEchant), mean, na.rm=TRUE)

## D'éventuels NAs correspondent à des mois non échantillonnés !

### Local Variables:
### ispell-local-dictionary: "english"
### fill-column: 120
### End:
