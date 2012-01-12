#-*- coding: latin-1 -*-

### File: Calcul_tables_metriques_LIT.R
### Time-stamp: <2012-01-09 13:32:21 yreecht>
###
### Author: Yves Reecht
###
####################################################################################################
### Description:
###
### Fonctions spécifiques aux observations de type Line Intercept Transect (LIT ; benthos)
### pour le calcul des tables de métriques :
####################################################################################################

calc.unitSp.LIT.f <- function(obs, unitobs, dataEnv)
{
    ## Purpose:
    ## ----------------------------------------------------------------------
    ## Arguments:
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 23 déc. 2011, 18:22

    ## Calcul des nombres par cl / espèces / unitobs :
    nbr <- calcNumber.default.f(obs,
                                factors=c("unite_observation", "code_espece"))

    ## Création de la data.frame de résultats (avec nombres, unitobs, ):
    res <- calc.numbers.f(nbr)


    ## Taille de transect :
    transectSz <- tapply(res[ , "nombre"], res[ , "unite_observation"], sum, na.rm=TRUE)

    ## Pourcentage de recouvrement de chaque espèce/categorie pour les couvertures biotiques et abiotiques :
    res[ , "recouvrement"] <- as.vector(100 * res[ , "nombre"] /
                                        transectSz[match(res[ , "unite_observation"],
                                                         rownames(transectSz))])
    rm(transectSz)

    ## Nombre de colonies (longueurs de transition > 0) :
    obs$count <- ifelse(obs[ , "nombre"] > 0, 1, 0) # [???] isTRUE ?  [yr: 3/1/2012]

    res[ , "colonie"] <- as.vector(tapply(obs$count,
                                          as.list(obs[ , c("unite_observation", "code_espece")]),
                                          sum, na.rm=TRUE))

    res[ , "colonie"][is.na(res[ , "colonie"])] <- 0 # [???]

    res[ , "taille.moy.colonies"] <- apply(res[ , c("nombre", "colonie")], 1,
                                           function(x)
                                       {
                                           ifelse(x[2] == 0, NA, x[1] / x[2])
                                       })

    ## Présence/absence :
    res[ , "pres_abs"] <- calc.presAbs.f(Data=res)

    return(res)
}








### Local Variables:
### ispell-local-dictionary: "english"
### fill-column: 120
### End:
