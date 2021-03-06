#-*- coding: latin-1 -*-
# Time-stamp: <2018-12-12 20:54:49 yreecht>

## Plateforme PAMPA de calcul d'indicateurs de ressources & biodiversit�
##   Copyright (C) 2008-2018 Ifremer - Tous droits r�serv�s.
##
##   Ce programme est un logiciel libre ; vous pouvez le redistribuer ou le
##   modifier suivant les termes de la "GNU General Public License" telle que
##   publi�e par la Free Software Foundation : soit la version 2 de cette
##   licence, soit (� votre gr�) toute version ult�rieure.
##
##   Ce programme est distribu� dans l'espoir qu'il vous sera utile, mais SANS
##   AUCUNE GARANTIE : sans m�me la garantie implicite de COMMERCIALISABILIT�
##   ni d'AD�QUATION � UN OBJECTIF PARTICULIER. Consultez la Licence G�n�rale
##   Publique GNU pour plus de d�tails.
##
##   Vous devriez avoir re�u une copie de la Licence G�n�rale Publique GNU avec
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
    ## Arguments: Data : table de donn�es de type Obs.
    ##            refesp : r�f�rentiel esp�ces.
    ##            vars : nom des colonnes de diff�rents types dans Data.
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 15 d�c. 2011, 11:33

    runLog.f(msg=c(mltext("logmsg.sizeclass")))

    if (any(idx <- (( ! is.na(Data[ , vars["sz"]])) &
                    is.na(Data[ , vars["szcl"]]))))
    {
        ## On ne travaille que sur les lignes o� la classe de taille peut �tre d�finie :
        df <- Data[idx, ]
        levels(df[ , vars["szcl"]]) <- c(levels(df[ , vars["szcl"]]), "P", "M", "G")

        ## Premier tiers de la gamme de taille (d'apr�s la taille maximale) -> "P" :
        idxP <- ((idxTmp <- df[ , vars["sz"]] < (refesp[match(df[ , vars["sp"]] ,
                                             refesp[ , vars["sp"]]),
                                       vars["szmax"]]/3)) &
                ! is.na(idxTmp))

        df[idxP, vars["szcl"]] <- "P"

        ## Second tiers de la gamme de taille (d'apr�s la taille maximale) -> "M" :
        idxM <- ((idxTmp <- df[ , vars["sz"]] >= (refesp[match(df[ , vars["sp"]] ,
                                                               refesp[ , vars["sp"]]),
                                                         vars["szmax"]]/3) & # et
                  df[ , vars["sz"]] < (2*(refesp[match(df[ , vars["sp"]] ,
                                                       refesp[ , vars["sp"]]),
                                                 vars["szmax"]]/3))) &
                 ! is.na(idxTmp))

        df[idxM, vars["szcl"]] <- "M"

        ## Troisi�me tiers de la gamme de taille (d'apr�s la taille maximale) -> "G" :
        idxG <- ((idxTmp <- df[ , vars["sz"]] >= (2*(refesp[match(df[ , vars["sp"]] ,
                                                                  refesp[ , vars["sp"]]),
                                                            vars["szmax"]]/3))) &
                 ! is.na(idxTmp))

        df[idxG, vars["szcl"]] <- "G"

        ## On rajoute les classes de tailles calcul�es � l'ensemble du jeu de donn�es :
        if ( ! all(is.na(df[ , vars["szcl"]])))
        {
            levels(Data[ , vars["szcl"]]) <- levels(df[ , vars["szcl"]])
            Data[idx, vars["szcl"]] <- df[ , vars["szcl"]]
        }else{}

        ## ct <- 1                         # en une ligne plut�t [yr: 27/07/2010]
        ## assign("ct", ct, envir=.GlobalEnv)

    }else{
        ## ct <- 2                         # en une ligne plut�t [yr: 27/07/2010]
        ## assign("ct", ct, envir=.GlobalEnv)
    }

    return(Data)
}

########################################################################################################################
addMeanSize.f <- function(Data,
                          vars=c(sz="length", szcl="size.class"))
{
    ## Purpose: Calcul des tailles comme les moyennes de classes de taille,
    ##          si seules ces derni�res sont renseign�es.
    ## ----------------------------------------------------------------------
    ## Arguments: Data : les donn�es d'observation.
    ##            vars : nom des colonnes de diff�rents types dans Data.
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 26 ao�t 2010, 16:20

    runLog.f(msg=c(mltext("logmsg.meansize.szcl")))

    res <- Data[ , vars["sz"]]
    classes.taille <- Data[ , vars["szcl"]]

    ## Calcul des tailles comme tailles moyennes � partir des classes de taille si n�cessaire :
    if (any(is.na(res)[grep("^([[:digit:]]*)[-_]([[:digit:]]*)$", classes.taille)]))
    {

        ## Classes de tailles ferm�es :
        idxCT <- grep("^([[:digit:]]+)[-_]([[:digit:]]+)$", classes.taille) # classes de tailles qui correspondent aux
                                        # motifs pris en compte...
        idxCT <- idxCT[is.na(res[idxCT])] # ...pour lesquels la taille n'est pas renseign�e.
        res[idxCT] <- unlist(sapply(parse(text=sub("^([[:digit:]]+)[-_]([[:digit:]]+)$",
                                                  "mean(c(\\1, \\2), na.rm=TRUE)",
                                                  classes.taille[idxCT])),
                                            eval))

        ## Classes de tailles ouvertes vers le bas (borne inf�rieure <- 0) :
        idxCT <- grep("^[-_]([[:digit:]]+)$", classes.taille)
        idxCT <- idxCT[is.na(res[idxCT])] # ...pour lesquels la taille n'est pas renseign�e.
        res[idxCT] <- unlist(sapply(parse(text=sub("^[-_]([[:digit:]]+)$",
                                                  "mean(c(0, \\1), na.rm=TRUE)",
                                                  classes.taille[idxCT])),
                                            eval))

        ## Classes de tailles ouvertes vers le haut (pas de borne sup�rieur, on fait l'hypoth�se que la taille est le
        ## minimum de la gamme) :
        idxCT <- grep("^([[:digit:]]+)[-_]$", classes.taille)
        idxCT <- idxCT[is.na(res[idxCT])] # ...pour lesquels la taille n'est pas renseign�e.
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
    ## Purpose: poids moyens d'apr�s les classes de taille PMG du r�f�rentiel
    ##          esp�ces.
    ## ----------------------------------------------------------------------
    ## Arguments: Data (type obs).
    ##            refesp : le r�f�rentiel esp�ces.
    ##            nom des colonnes de diff�rents types dans Data.
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 11 oct. 2010, 14:58

    runLog.f(msg=c(mltext("logmsg.meanweight.szcl")))

    refespTmp <- as.matrix(refesp[ , c("mean.weight.small", "mean.weight.medium", "mean.weight.large")])
    row.names(refespTmp) <- as.character(refesp[ , vars["sp"]] )

    classID <- c("P"=1, "M"=2, "G"=3)

    res <- sapply(seq(length.out=nrow(Data)),
                  function(i)
              {
                  ifelse(## Si l'esp�ce est dans le r�f�rentiel esp�ce...
                         is.element(Data[i , vars["sp"]], row.names(refespTmp)),
                         ## ...poids moyen correspondant � l'esp�ce et la classe de taille :
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
    ## Purpose: Affiche un pop-up avec un r�capitulatif des calculs de poids
    ##          (si n�cessaire).
    ## ----------------------------------------------------------------------
    ## Arguments: x : vecteur avec les nombres de poids ajout�s aux
    ##                observations suivant chaque type de calcul.
    ##            MPA : l'AMP ; permet d'avoir un avertissement identifi� par
    ##                  AMP pour les jeux de donn�es multi-sites.
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date:  7 d�c. 2010, 13:56

    runLog.f(msg=c(mltext("logmsg.weight.summary")))

    infoLoading.f(msg=paste(MPA, mltext("summarize.calcWeight.info.1"),
                  sum(x[-1]), "/", x["total"], mltext("summarize.calcWeight.info.2"),
                  ifelse(sum(x[-1]) > 0,
                         paste(         # Info compl�mentaires si des poids estim�s...
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
    ##          de priorit� :
    ##           1) conservation des poids observ�s.
    ##           2) calcul des poids avec les relations taille-poids.
    ##           3) idem avec les tailles d'apr�s les classes de tailles.
    ##           4) poids moyens d'apr�s les classes de tailles (G, M, P).
    ##           5) rien sinon (NAs).
    ##
    ##           Afficher les un bilan si n�cessaire.
    ## ----------------------------------------------------------------------
    ## Arguments: Data : jeu de donn�es ; doit contenir les colonnes
    ##                   d�crites par vars (� l'exception de szcl qui
    ##                   est optionnelle).
    ##            refesp : r�f�rentiel esp�ces.
    ##            MPA : l'AMP o� ont �t� collect�es les donn�es. En ne
    ##                  passant cet argument, on peut travailler sur une
    ##                  partie d'un jeu de donn�es multi-sites.
    ##            vars : nom des colonnes de diff�rents types dans Data.
    ##
    ## Output: Data avec les poids calcul�s dans la colonne "weight".
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date:  6 d�c. 2010, 11:57

    runLog.f(msg=c(mltext("logmsg.weightMPA.calc")))

    ## Identification des diff�rents cas :
    casSite <- c("BA"="Med", "BO"="Med", "CB"="Med", "CR"="Med", "STM"="Med", # [!!!] STM provisoire en attendant mieux.
                 "MAY"="MAY", "RUN"="MAY") ## [!!!] add possibility of an option for generic use.

    ## L'AMP est mise au format d�sir� (on ne peut traiter qu'un �l�ment � la fois) :
    MPA <- as.character(MPA[1])

    ## Poids observ�s :
    res <- Data[ , "weight"]

    ## Nombre d'obs totales, nombre de poids obs et nombre de tailles obs (sans poids obs) :
    nbObsType <- c("total"=length(res), "obs"=sum(!is.na(res))) # , "length"=sum(!is.na(Data[is.na(res) , "length"])))

    ## indices des tailles renseign�es, pour lesquelles il n'y a pas de poids renseign� :
    idxTaille <- !is.na(Data[ , vars["sz"]]) & is.na(res)

    ## ajout des tailles moyennes d'apr�s la classe de taille :
    Data[ , vars["sz"]] <- addMeanSize.f(Data=Data, vars = vars)

    ## indices des tailles ajout�es par cette m�thode, pour lesquelles il n'y a pas de poids renseign� :
    idxTailleMoy <- !is.na(Data[ , vars["sz"]]) & ! idxTaille & is.na(res)

    ## Calcul des poids � partir des relations taille-poids W�=�n*a*L^b :
    idxP <- is.na(res)                  # indices des poids � calculer.

    if (getOption("P.refesp.Coefs") == "new")
    {
        res[idxP] <- (Data[ , vars["nb"]] * refesp$a.coeff[match(Data[ , vars["sp"]],
                                                                 refesp[ , vars["sp"]])] *
                      Data[ , vars["sz"]] ^ refesp$b.coeff[match(Data[ , vars["sp"]],
                                                                 refesp[ , vars["sp"]])])[idxP]
    }else{
        switch(casSite[MPA],
               ## M�diterrann�e :
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
    ## Nombre de poids ajout�es gr�ce � la m�thode :
    nbObsType[c("length", "taille.moy")] <- c(sum(!is.na(res[idxTaille])), sum(!is.na(res[idxTailleMoy])))

    ## Ajout des classes de taille (P, M, G) si n�cessaire lorsque la taille est connue
    ## (permet le calcul / poids moyen de classe si les coefs a et b sont inconnus) :
    Data <- sizeClasses.f(Data=Data, refesp=refesp, vars = vars)

    if ((getOption("P.refesp.Coefs") == "new" && # Nouveau r�f�rentiel avec fichier local charg�.
         all(is.element(c("mean.weight.small", "mean.weight.medium", "mean.weight.large"), colnames(refesp)))) ||
        isTRUE(MPA == "BO"))
    {
        ## Poids d'apr�s les classes de taille lorsque la taille n'est pas renseign�e :
        tmpNb <- sum(!is.na(res))           # nombre de poids disponibles avant.

        res[is.na(res)] <- (meanWeight.SzCl.f(Data=Data, refesp=refesp, vars=vars) *
                            Data[ , vars["nb"]])[is.na(res)]

        nbObsType["poids.moy"] <- sum(!is.na(res)) - tmpNb # nombre de poids ajout�s.
    }else{}

    ## R�capitulatif :
    summarize.calcWeight.f(x=nbObsType, MPA=MPA)

    ## Stockage et retour des donn�es :
    Data[ , "weight"] <- res
    return(Data)
}

########################################################################################################################
calcWeight.f <- function(Data)
{
    ## Purpose: Lance le calcul des poids pour chaque AMP et assemble les
    ##          r�sultat en une seule data.frame de type "observations".
    ## ----------------------------------------------------------------------
    ## Arguments: Data : une liste contenant 3 data.frames nomm�es :
    ##                   * obs : table des observations.
    ##                   * unitobs : r�f�rentiel d'unit�s d'observations.
    ##                   * refesp : le r�f�rentiel esp�ces.
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 16 d�c. 2011, 11:34

    stepInnerProgressBar.f(n=1, msg=mltext("calcWeight.info.1"))

    Data$obs <- do.call(rbind,          # R�assemblage de la...
                        lapply(unique(Data$unitobs[ , getOption("P.MPAfield")]), # ...liste des observations par cas
                                        # d'�tude avec les poids estim�s.
                               function(i, obs, unitobs, refesp)
                           {
                               stepInnerProgressBar.f(n=0,
                                                      msg=paste(mltext("calcWeight.info.2"), i))

                               ## S�lection des observations du cas d'�tude i :
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
