#-*- coding: latin-1 -*-

## Plateforme PAMPA de calcul d'indicateurs de ressources & biodiversité
##   Copyright (C) 2008-2010 Ifremer - Tous droits réservés.
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

### File: nombres_SVR.R
### Time-stamp: <2011-12-22 16:59:46 yreecht>
###
### Author: Yves Reecht
###
####################################################################################################
### Description:
###
### Calculs deqs nombres pour les données vidéos rotatives.
####################################################################################################

statRotation.basic.f <- function(facteurs, obs=obs, dataEnv=.GlobalEnv)
{
    ## Purpose: Calcul des statistiques des abondances (max, sd) par rotation
    ##          en se basant sur des interpolations basiques.
    ## ----------------------------------------------------------------------
    ## Arguments: facteurs : vecteur des noms de facteurs d'agrégation
    ##                       (résolution à laquelle on travaille).
    ##            obs : données d'observation.
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date:  9 déc. 2010, 10:01

    ## Identification des secteurs et rotations valides :
    if (is.element("unite_observation", facteurs))
    {
        ## Rotations valides (les vides doivent tout de même être renseignés) :
        rotations <- tapply(obs$rotation,
                            as.list(obs[ , c("unite_observation", "rotation"), drop=FALSE]),
                            function(x)length(x) > 0)

        ## Les rotations non renseignés apparaissent en NA et on veut FALSE :
        rotations[is.na(rotations)] <- FALSE
    }else{
        stop("\n\tL'unité d'observation doit faire partie des facteurs d'agrégation !!\n")
    }

    ## ###########################################################
    ## Nombres par rotation avec le niveau d'agrégation souhaité :
    ## Ajouter les interpolations [!!!][SVR]
    nombresR <- tapply(obs$nombre,
                       as.list(obs[ , c(facteurs, "rotation"), drop=FALSE]),
                       function(x,...){ifelse(all(is.na(x)), NA, sum(x,...))},
                       na.rm = TRUE)

    ## Nombres de secteurs valides / rotation (au même format que nombresR) :
    nombresSecVal <- tapply(obs$sec.valides,
                            as.list(obs[ , c(facteurs, "rotation"), drop=FALSE]),
                            mean, na.rm = TRUE)

    ## Correction des nombres (règle de trois) :
    nombresR <- nombresR * max(nombresSecVal, na.rm=TRUE) / nombresSecVal

    ## Les NAs sont considérés comme des vrais zéros lorsque la rotation est valide :
    nombresR <- sweep(nombresR,
                      match(names(dimnames(rotations)), names(dimnames(nombresR)), nomatch=NULL),
                      rotations,        # Tableau des secteurs valides (booléens).
                      function(x, y)
                  {
                      x[is.na(x) & y] <- 0 # Lorsque NA et secteur valide => 0.
                      return(x)
                  })

    ## ##################################################
    ## Statistiques :

    ## Moyennes :
    nombresMean <- apply(nombresR, which(is.element(names(dimnames(nombresR)), facteurs)),
                         function(x,...){ifelse(all(is.na(x)), NA, mean(x,...))}, na.rm=TRUE)

    ## Maxima :
    nombresMax <- apply(nombresR, which(is.element(names(dimnames(nombresR)), facteurs)),
                        function(x,...){ifelse(all(is.na(x)), NA, max(x,...))}, na.rm=TRUE)

    ## Déviation standard :
    nombresSD <- apply(nombresR, which(is.element(names(dimnames(nombresR)), facteurs)),
                       function(x,...){ifelse(all(is.na(x)), NA, sd(x,...))}, na.rm=TRUE)

    ## Nombre de rotations valides :
    nombresRotations <- apply(rotations, 1, sum, na.rm=TRUE)

    ## if (!is.element("classe_taille", facteurs))
    ## {
    ## ## Pour les calculs agrégés par unitobs :
    ## tmpNombresSVR <- apply(nombresR,
    ##                        which(names(dimnames(nombresR)) != "code_espece"),
    ##                        sum, na.rm=TRUE)

    ## tmpNombresSVR[!rotations] <- NA

    assign(".NombresSVR", nombresR, envir=dataEnv)
    assign(".Rotations", rotations, envir=dataEnv)
    ## }else{}

    ## Retour des résultats sous forme de liste :
    return(list(nombresMean=nombresMean, nombresMax=nombresMax, nombresSD=nombresSD,
                nombresRotations=nombresRotations, nombresTot=nombresR))
}

########################################################################################################################
interpSecteurs.f <- function(sectUnitobs)
{
    ## Purpose: Interpolation des valeurs pour les secteurs non valides
    ##          (rotations valides seulement ; interpolations étendues)
    ##          en trois étapes :
    ##            1) interpolation sur le même secteur d'après les autres
    ##               rotations.
    ##            2) interpolation sur les secteurs adjacents de la même
    ##               rotation.
    ##            3) moyenne sur la roatation pour les secteurs qui ne
    ##               peuvent être interpolés avec les deux premières étapes.
    ## ----------------------------------------------------------------------
    ## Arguments: sectUnitobs : matrice des secteurs de l'unité
    ##                          d'observation, avec les secteurs en colonnes
    ##                          et les rotations en lignes.
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date:  9 nov. 2010, 14:52


    ## On ne travaille que sur les rotations valides (i.e. ayant au moins un secteur valide) :
    idx <- apply(sectUnitobs, 1, function(x)!all(is.na(x)))

    tmp <- sectUnitobs[idx, , drop=FALSE]

    ## Étape 1 : interpolation sur le même secteur dans les autres rotations :
    tmp <- matrix(apply(tmp, 2, function(x)
                    {
                        x[which(is.na(x))] <- mean(x, na.rm=TRUE)
                        return(x)
                    }), ncol=ncol(tmp))

    ## Étape 2 : interpolation sur les secteurs adjacents de la même rotation :
    if (any(is.na(tmp)))
    {
        tmp <- t(apply(tmp,
                       1,
                       function(x)
                   {
                       xi <- which(is.na(x))
                       x[xi] <- sapply(xi,
                                       function(i, x){mean(x[i + c(0, 2)], na.rm=TRUE)},
                                       x=c(tail(x, 1), x, head(x, 1)))
                       return(x)
                   }))

        if (any(is.na(tmp)))
        {
            ## Étape 3 : moyenne de la rotation sinon :
            tmp <- t(apply(tmp, 1,
                           function(x)
                       {
                           x[which(is.na(x))] <- mean(x, na.rm=TRUE)
                           return(x)
                       }))
        }else{}
    }else{}



    ## Récupération dans le tableau d'origine :
    sectUnitobs[idx, ] <- tmp
    ## sectUnitobs <- as.array(sectUnitobs)

    if (any(dim(sectUnitobs) != c(3, 6))) print(sectUnitobs)


    return(sectUnitobs)
}

########################################################################################################################
statRotation.extended.f <- function(facteurs, obs=obs, dataEnv=.GlobalEnv)
{
    ## Purpose: Calcul des statistiques des abondances (max, sd) par
    ##          rotation, en utilisant des interpolations étendues.
    ## ----------------------------------------------------------------------
    ## Arguments: facteurs : vecteur des noms de facteurs d'agrégation
    ##                       (résolution à laquelle on travaille).
    ##            obs : données d'observation.
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date:  9 nov. 2010, 09:25

    ## Identification des secteurs et rotations valides :
    if (is.element("unite_observation", facteurs))
    {
        ## Secteurs valides (les vides doivent tout de même être renseignés) :
        secteurs <- tapply(obs$secteur,
                           as.list(obs[ , c("unite_observation", "rotation", "secteur"), drop=FALSE]),
                           function(x)length(x) > 0)

        ## Les secteurs non renseignés apparaissent en NA et on veut FALSE :
        secteurs[is.na(secteurs)] <- FALSE

        ## Rotations valides :
        rotations <- apply(secteurs, c(1, 2), any) # Au moins un secteur renseigné.

    }else{
        stop("\n\tL'unité d'observation doit faire partie des facteurs d'agrégation !!\n")
    }

    ## ###########################################################
    ## Nombres par rotation avec le niveau d'agrégation souhaité :
    ## Ajouter les interpolations [!!!][SVR]
    nombresRS <- tapply(obs$nombre,
                        as.list(obs[ , c(facteurs, "rotation", "secteur"), drop=FALSE]),
                        sum, na.rm = TRUE)

    tmp <- apply(nombresRS,
                 which(is.element(names(dimnames(nombresRS)), facteurs)),
                 interpSecteurs.f)

    ## On récupère les dimensions d'origine (résultats de la fonction dans 1 vecteur ; attention, plus le même ordre) :
    dim(tmp) <- c(tail(dim(nombresRS), 2), head(dim(nombresRS), -2))

    ## On renomme les dimensions avec leur nom d'origine en tenant compte du nouvel ordre :
    dimnames(tmp) <- dimnames(nombresRS)[c(tail(seq_along(dimnames(nombresRS)), 2),
                                           head(seq_along(dimnames(nombresRS)), -2))]

    ## On restocke le tout dans nombresRS (attentions, toujours travailler sur les noms de dimensions)
    nombresRS <- tmp


    ## Les NAs sont considérés comme des vrais zéros lorsque la rotation est valide :
    nombresRS <- sweep(nombresRS,
                       match(names(dimnames(secteurs)), names(dimnames(nombresRS)), nomatch=NULL),
                       secteurs,        # Tableau des secteurs valides (booléens).
                       function(x, y)
                   {
                       x[is.na(x) & y] <- 0 # Lorsque NA et secteur valide => 0.
                       return(x)
                   })


    nombresR <- apply(nombresRS,
                      which(is.element(names(dimnames(nombresRS)), c(facteurs, "rotation"))),
                      function(x){ifelse(all(is.na(x)), NA, sum(x, na.rm=TRUE))})



    ## ##################################################
    ## Statistiques :

    ## Moyennes :
    nombresMean <- apply(nombresR, which(is.element(names(dimnames(nombresR)), facteurs)),
                         function(x,...){ifelse(all(is.na(x)), NA, mean(x,...))}, na.rm=TRUE)

    ## Maxima :
    nombresMax <- apply(nombresR, which(is.element(names(dimnames(nombresR)), facteurs)),
                        function(x,...){ifelse(all(is.na(x)), NA, max(x,...))}, na.rm=TRUE)

    ## Déviation standard :
    nombresSD <- apply(nombresR, which(is.element(names(dimnames(nombresR)), facteurs)),
                       function(x,...){ifelse(all(is.na(x)), NA, sd(x,...))}, na.rm=TRUE)

    ## Nombre de rotations valides :
    nombresRotations <- apply(rotations, 1, sum, na.rm=TRUE)

    ## if (!is.element("classe_taille", facteurs))
    ## {
        ## ## Pour les calculs agrégés par unitobs :
        ## tmpNombresSVR <- apply(nombresR,
        ##                        which(names(dimnames(nombresR)) != "code_espece"),
        ##                        sum, na.rm=TRUE)

        ## tmpNombresSVR[!rotations] <- NA

    assign(".NombresSVR", nombresR, envir=dataEnv)
    assign(".Rotations", rotations, envir=dataEnv)
    ## }else{}

    ## Retour des résultats sous forme de liste :
    return(list(nombresMean=nombresMean, nombresMax=nombresMax, nombresSD=nombresSD,
                nombresRotations=nombresRotations, nombresTot=nombresRS))
}







### Local Variables:
### ispell-local-dictionary: "english"
### fill-column: 120
### End:
