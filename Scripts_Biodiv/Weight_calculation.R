#-*- coding: latin-1 -*-
# Time-stamp: <2018-12-12 20:54:49 yreecht>

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

### File: Weight_calculation.R
### Created: <2012-01-18 17:06:02 yreecht>
###
### Author: Yves Reecht
###
####################################################################################################
### Description:
###
###
####################################################################################################

sizeClasses.f <- function(Data, refesp,
                          vars=c(sp="species.code", sz="length", szcl="size.class", szmax="Lmax"))
{
    ## Purpose:
    ## ----------------------------------------------------------------------
    ## Arguments: Data : table de données de type Obs.
    ##            refesp : référentiel espèces.
    ##            vars : nom des colonnes de différents types dans Data.
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 15 déc. 2011, 11:33

    runLog.f(msg=c(mltext("logmsg.sizeclass")))

    if (any(idx <- (( ! is.na(Data[ , vars["sz"]])) &
                    is.na(Data[ , vars["szcl"]]))))
    {
        ## On ne travaille que sur les lignes où la classe de taille peut être définie :
        df <- Data[idx, ]
        levels(df[ , vars["szcl"]]) <- c(levels(df[ , vars["szcl"]]), "P", "M", "G")

        ## Premier tiers de la gamme de taille (d'après la taille maximale) -> "P" :
        idxP <- ((idxTmp <- df[ , vars["sz"]] < (refesp[match(df[ , vars["sp"]] ,
                                             refesp[ , vars["sp"]]),
                                       vars["szmax"]]/3)) &
                ! is.na(idxTmp))

        df[idxP, vars["szcl"]] <- "P"

        ## Second tiers de la gamme de taille (d'après la taille maximale) -> "M" :
        idxM <- ((idxTmp <- df[ , vars["sz"]] >= (refesp[match(df[ , vars["sp"]] ,
                                                               refesp[ , vars["sp"]]),
                                                         vars["szmax"]]/3) & # et
                  df[ , vars["sz"]] < (2*(refesp[match(df[ , vars["sp"]] ,
                                                       refesp[ , vars["sp"]]),
                                                 vars["szmax"]]/3))) &
                 ! is.na(idxTmp))

        df[idxM, vars["szcl"]] <- "M"

        ## Troisième tiers de la gamme de taille (d'après la taille maximale) -> "G" :
        idxG <- ((idxTmp <- df[ , vars["sz"]] >= (2*(refesp[match(df[ , vars["sp"]] ,
                                                                  refesp[ , vars["sp"]]),
                                                            vars["szmax"]]/3))) &
                 ! is.na(idxTmp))

        df[idxG, vars["szcl"]] <- "G"

        ## On rajoute les classes de tailles calculées à l'ensemble du jeu de données :
        if ( ! all(is.na(df[ , vars["szcl"]])))
        {
            levels(Data[ , vars["szcl"]]) <- levels(df[ , vars["szcl"]])
            Data[idx, vars["szcl"]] <- df[ , vars["szcl"]]
        }else{}

        ## ct <- 1                         # en une ligne plutôt [yr: 27/07/2010]
        ## assign("ct", ct, envir=.GlobalEnv)

    }else{
        ## ct <- 2                         # en une ligne plutôt [yr: 27/07/2010]
        ## assign("ct", ct, envir=.GlobalEnv)
    }

    return(Data)
}

########################################################################################################################
addMeanSize.f <- function(Data,
                          vars=c(sz="length", szcl="size.class"))
{
    ## Purpose: Calcul des tailles comme les moyennes de classes de taille,
    ##          si seules ces dernières sont renseignées.
    ## ----------------------------------------------------------------------
    ## Arguments: Data : les données d'observation.
    ##            vars : nom des colonnes de différents types dans Data.
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 26 août 2010, 16:20

    runLog.f(msg=c(mltext("logmsg.meansize.szcl")))

    res <- Data[ , vars["sz"]]
    classes.taille <- Data[ , vars["szcl"]]

    ## Calcul des tailles comme tailles moyennes à partir des classes de taille si nécessaire :
    if (any(is.na(res)[grep("^([[:digit:]]*)[-_]([[:digit:]]*)$", classes.taille)]))
    {

        ## Classes de tailles fermées :
        idxCT <- grep("^([[:digit:]]+)[-_]([[:digit:]]+)$", classes.taille) # classes de tailles qui correspondent aux
                                        # motifs pris en compte...
        idxCT <- idxCT[is.na(res[idxCT])] # ...pour lesquels la taille n'est pas renseignée.
        res[idxCT] <- unlist(sapply(parse(text=sub("^([[:digit:]]+)[-_]([[:digit:]]+)$",
                                                  "mean(c(\\1, \\2), na.rm=TRUE)",
                                                  classes.taille[idxCT])),
                                            eval))

        ## Classes de tailles ouvertes vers le bas (borne inférieure <- 0) :
        idxCT <- grep("^[-_]([[:digit:]]+)$", classes.taille)
        idxCT <- idxCT[is.na(res[idxCT])] # ...pour lesquels la taille n'est pas renseignée.
        res[idxCT] <- unlist(sapply(parse(text=sub("^[-_]([[:digit:]]+)$",
                                                  "mean(c(0, \\1), na.rm=TRUE)",
                                                  classes.taille[idxCT])),
                                            eval))

        ## Classes de tailles ouvertes vers le haut (pas de borne supérieur, on fait l'hypothèse que la taille est le
        ## minimum de la gamme) :
        idxCT <- grep("^([[:digit:]]+)[-_]$", classes.taille)
        idxCT <- idxCT[is.na(res[idxCT])] # ...pour lesquels la taille n'est pas renseignée.
        res[idxCT] <- unlist(sapply(parse(text=sub("^([[:digit:]]+)[-_]$",
                                                  "mean(c(\\1), na.rm=TRUE)",
                                                  classes.taille[idxCT])),
                                            eval))
    }else{}

    return(res)
}

########################################################################################################################
meanWeight.SzCl.f <- function(Data, refesp,
                             vars=c(sp="species.code", szcl="size.class"))
{
    ## Purpose: poids moyens d'après les classes de taille PMG du référentiel
    ##          espèces.
    ## ----------------------------------------------------------------------
    ## Arguments: Data (type obs).
    ##            refesp : le référentiel espèces.
    ##            nom des colonnes de différents types dans Data.
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 11 oct. 2010, 14:58

    runLog.f(msg=c(mltext("logmsg.meanweight.szcl")))

    refespTmp <- as.matrix(refesp[ , c("mean.weight.small", "mean.weight.medium", "mean.weight.large")])
    row.names(refespTmp) <- as.character(refesp[ , vars["sp"]] )

    classID <- c("P"=1, "M"=2, "G"=3)

    res <- sapply(seq(length.out=nrow(Data)),
                  function(i)
              {
                  ifelse(## Si l'espèce est dans le référentiel espèce...
                         is.element(Data[i , vars["sp"]], row.names(refespTmp)),
                         ## ...poids moyen correspondant à l'espèce et la classe de taille :
                         refespTmp[as.character(Data[i , vars["sp"]]),
                                   classID[as.character(Data[i , vars["szcl"]])]],
                         ## Sinon rien :
                         NA)
              })

    return(res)
}

########################################################################################################################
summarize.calcWeight.f <- function(x, MPA)
{
    ## Purpose: Affiche un pop-up avec un récapitulatif des calculs de poids
    ##          (si nécessaire).
    ## ----------------------------------------------------------------------
    ## Arguments: x : vecteur avec les nombres de poids ajoutés aux
    ##                observations suivant chaque type de calcul.
    ##            MPA : l'AMP ; permet d'avoir un avertissement identifié par
    ##                  AMP pour les jeux de données multi-sites.
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date:  7 déc. 2010, 13:56

    runLog.f(msg=c(mltext("logmsg.weight.summary")))

    infoLoading.f(msg=paste(MPA, mltext("summarize.calcWeight.info.1"),
                  sum(x[-1]), "/", x["total"], mltext("summarize.calcWeight.info.2"),
                  ifelse(sum(x[-1]) > 0,
                         paste(         # Info complémentaires si des poids estimés...
                               mltext("summarize.calcWeight.info.3"),
                               "\n\t*  ", x["obs"], mltext("summarize.calcWeight.info.4"),
                               "\n\t*  ", x["length"], mltext("summarize.calcWeight.info.5"),
                               ifelse(any(x[-(1:3)] > 0),
                                      paste("\n\t*  ", x["taille.moy"],
                                            mltext("summarize.calcWeight.info.6"),
                                            mltext("summarize.calcWeight.info.7"),
                                            ifelse(!is.na(x["poids.moy"]),
                                                   paste("\n\t*  ", x["poids.moy"],
                                                         mltext("summarize.calcWeight.info.8"),
                                                         sep=""),
                                                   ""),
                                            sep=""),
                                      ""),
                               mltext("summarize.calcWeight.info.9"),
                               sep=""),
                         "."),          # ...sinon rien.
                  sep=""),
                  icon=ifelse(any(x[-(1:3)] > 0), "warning", "info"))
}

########################################################################################################################
calcWeightMPA.f <- function(Data, refesp, MPA,
                            vars=c(sp="species.code",
                                   sz="length", wg="weight", szcl="size.class",
                                   szmax="Lmax", nb="number"))
{
    ## Purpose: Calculs des poids (manquants) pour un fichier de type
    ##          "observation" pour **une seule AMP**, avec par ordre
    ##          de priorité :
    ##           1) conservation des poids observés.
    ##           2) calcul des poids avec les relations taille-poids.
    ##           3) idem avec les tailles d'après les classes de tailles.
    ##           4) poids moyens d'après les classes de tailles (G, M, P).
    ##           5) rien sinon (NAs).
    ##
    ##           Afficher les un bilan si nécessaire.
    ## ----------------------------------------------------------------------
    ## Arguments: Data : jeu de données ; doit contenir les colonnes
    ##                   décrites par vars (à l'exception de szcl qui
    ##                   est optionnelle).
    ##            refesp : référentiel espèces.
    ##            MPA : l'AMP où ont été collectées les données. En ne
    ##                  passant cet argument, on peut travailler sur une
    ##                  partie d'un jeu de données multi-sites.
    ##            vars : nom des colonnes de différents types dans Data.
    ##
    ## Output: Data avec les poids calculés dans la colonne "weight".
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date:  6 déc. 2010, 11:57

    runLog.f(msg=c(mltext("logmsg.weightMPA.calc")))

    ## Identification des différents cas :
    casSite <- c("BA"="Med", "BO"="Med", "CB"="Med", "CR"="Med", "STM"="Med", # [!!!] STM provisoire en attendant mieux.
                 "MAY"="MAY", "RUN"="MAY") ## [!!!] add possibility of an option for generic use.

    ## L'AMP est mise au format désiré (on ne peut traiter qu'un élément à la fois) :
    MPA <- as.character(MPA[1])

    ## Poids observés :
    res <- Data[ , "weight"]

    ## Nombre d'obs totales, nombre de poids obs et nombre de tailles obs (sans poids obs) :
    nbObsType <- c("total"=length(res), "obs"=sum(!is.na(res))) # , "length"=sum(!is.na(Data[is.na(res) , "length"])))

    ## indices des tailles renseignées, pour lesquelles il n'y a pas de poids renseigné :
    idxTaille <- !is.na(Data[ , vars["sz"]]) & is.na(res)

    ## ajout des tailles moyennes d'après la classe de taille :
    Data[ , vars["sz"]] <- addMeanSize.f(Data=Data, vars = vars)

    ## indices des tailles ajoutées par cette méthode, pour lesquelles il n'y a pas de poids renseigné :
    idxTailleMoy <- !is.na(Data[ , vars["sz"]]) & ! idxTaille & is.na(res)

    ## Calcul des poids à partir des relations taille-poids W = n*a*L^b :
    idxP <- is.na(res)                  # indices des poids à calculer.

    if (getOption("P.refesp.Coefs") == "new")
    {
        res[idxP] <- (Data[ , vars["nb"]] * refesp$a.coeff[match(Data[ , vars["sp"]],
                                                                 refesp[ , vars["sp"]])] *
                      Data[ , vars["sz"]] ^ refesp$b.coeff[match(Data[ , vars["sp"]],
                                                                 refesp[ , vars["sp"]])])[idxP]
    }else{
        switch(casSite[MPA],
               ## Méditerrannée :
               "Med"={
                   res[idxP] <- (Data[ , vars["nb"]] * refesp$Coeff.a.Med[match(Data[ , vars["sp"]],
                                                                                refesp[ , vars["sp"]])] *
                                 Data[ , vars["sz"]] ^ refesp$Coeff.b.Med[match(Data[ , vars["sp"]],
                                                                                refesp[ , vars["sp"]])])[idxP]
               },
               ## Certains sites outre-mer :
               "MAY"={
                   res[idxP] <- (Data[ , vars["nb"]] * refesp$Coeff.a.MAY[match(Data[ , vars["sp"]],
                                                                                refesp[ , vars["sp"]])] *
                                 Data[ , vars["sz"]] ^ refesp$Coeff.b.MAY[match(Data[ , vars["sp"]],
                                                                                refesp[ , vars["sp"]])])[idxP]
               },
               ## Autres (NC,...) :
               res[idxP] <- (Data[ , vars["nb"]] * refesp$Coeff.a.NC[match(Data[ , vars["sp"]],
                                                                           refesp[ , vars["sp"]])] *
                             Data[ , vars["sz"]] ^ refesp$Coeff.b.NC[match(Data[ , vars["sp"]],
                                                                           refesp[ , vars["sp"]])])[idxP]
               )
    }

    ## [!!!] Comptabiliser les tailles incalculables !
    ## Nombre de poids ajoutées grâce à la méthode :
    nbObsType[c("length", "taille.moy")] <- c(sum(!is.na(res[idxTaille])), sum(!is.na(res[idxTailleMoy])))

    ## Ajout des classes de taille (P, M, G) si nécessaire lorsque la taille est connue
    ## (permet le calcul / poids moyen de classe si les coefs a et b sont inconnus) :
    Data <- sizeClasses.f(Data=Data, refesp=refesp, vars = vars)

    if ((getOption("P.refesp.Coefs") == "new" && # Nouveau référentiel avec fichier local chargé.
         all(is.element(c("mean.weight.small", "mean.weight.medium", "mean.weight.large"), colnames(refesp)))) ||
        isTRUE(MPA == "BO"))
    {
        ## Poids d'après les classes de taille lorsque la taille n'est pas renseignée :
        tmpNb <- sum(!is.na(res))           # nombre de poids disponibles avant.

        res[is.na(res)] <- (meanWeight.SzCl.f(Data=Data, refesp=refesp, vars=vars) *
                            Data[ , vars["nb"]])[is.na(res)]

        nbObsType["poids.moy"] <- sum(!is.na(res)) - tmpNb # nombre de poids ajoutés.
    }else{}

    ## Récapitulatif :
    summarize.calcWeight.f(x=nbObsType, MPA=MPA)

    ## Stockage et retour des données :
    Data[ , "weight"] <- res
    return(Data)
}

########################################################################################################################
calcWeight.f <- function(Data)
{
    ## Purpose: Lance le calcul des poids pour chaque AMP et assemble les
    ##          résultat en une seule data.frame de type "observations".
    ## ----------------------------------------------------------------------
    ## Arguments: Data : une liste contenant 3 data.frames nommées :
    ##                   * obs : table des observations.
    ##                   * unitobs : référentiel d'unités d'observations.
    ##                   * refesp : le référentiel espèces.
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 16 déc. 2011, 11:34

    stepInnerProgressBar.f(n=1, msg=mltext("calcWeight.info.1"))

    Data$obs <- do.call(rbind,          # Réassemblage de la...
                        lapply(unique(Data$unitobs[ , getOption("P.MPAfield")]), # ...liste des observations par cas
                                        # d'étude avec les poids estimés.
                               function(i, obs, unitobs, refesp)
                           {
                               stepInnerProgressBar.f(n=0,
                                                      msg=paste(mltext("calcWeight.info.2"), i))

                               ## Sélection des observations du cas d'étude i :
                               obs <- obs[is.element(obs$observation.unit,
                                                     unitobs[unitobs[ , getOption("P.MPAfield")] == i,
                                                             "observation.unit"]), ]

                               ## Estimation des poids :
                               return(calcWeightMPA.f(Data=obs, refesp=refesp, MPA=i))
                           },
                               obs=Data$obs, unitobs=Data$unitobs, refesp=Data$refesp))

    return(Data)
}




### Local Variables:
### ispell-local-dictionary: "english"
### fill-column: 120
### End:
