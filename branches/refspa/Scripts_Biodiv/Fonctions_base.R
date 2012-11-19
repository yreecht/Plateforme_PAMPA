#-*- coding: latin-1 -*-

## Plateforme PAMPA de calcul d'indicateurs de ressources & biodiversité
##   Copyright (C) 2008-2012 Ifremer - Tous droits réservés.
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

### File: fonctions_base.R
### Time-stamp: <2012-01-18 17:45:51 yreecht>
###
### Author: Yves Reecht
###
####################################################################################################
### Description:
###
### Fonctions de bases de la plateforme (également utilisé pour définir des fonctions de base
### récentes de R lorsqu'elles n'existent pas, par ex si on travaille sur une version ancienne de R)
####################################################################################################

########################################################################################################################
dropLevels.f <- function(df, which=NULL)
{
    ## Purpose: Supprimer les 'levels' non utilisés des facteurs d'une
    ##          data.frame.
    ## ----------------------------------------------------------------------
    ## Arguments: df : une data.frame
    ##            which : indice des colonnes à inclure (toutes par défaut).
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 10 août 2010, 13:29

    if (class(df) != "data.frame")
    {
        stop("'df' doit être une data.frame")
    }else{
        if (is.null(which))
        {
            x <- as.data.frame(sapply(df, function(x)
                                  {
                                      return(x[ ,drop=TRUE])
                                  }, simplify=FALSE),
                               stringsAsFactors=FALSE)
        }else{                          # Cas où seulement certaines colonnes sont traitées.
            x <- df

            x[ , which] <- as.data.frame(sapply(df[ , which, drop=FALSE],
                                                function(x)
                                            {
                                                return(x[ , drop=TRUE])
                                            }, simplify=FALSE),
                                         stringsAsFactors=FALSE)
        }

        return(x)
    }
}


########################################################################################################################
Capitalize.f <- function(x, words=FALSE)
{
    ## Purpose: Mettre en majuscule la première lettre de chaque mot
    ## ----------------------------------------------------------------------
    ## Arguments: x : une chaîne de caractères
    ##            words : tous les mots (TRUE), ou juste le premier.
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date:  9 août 2010, 21:08

    if (words)
    {
        s <- strsplit(x, " ")[[1]]
    }else{
        s <- x
    }

    return(paste(toupper(substring(s, 1,1)), substring(s, 2),
                 sep="", collapse=" "))
}

########################################################################################################################
if (!exists("grepl"))
{
    grepl <- function(pattern, x,...)
    {
        ## Purpose: Émulation des fonctions de 'grepl' (en moins efficace
        ##          probablement) si la fonction n'existe pas.
        ## ----------------------------------------------------------------------
        ## Arguments: pattern : le motif à rechercher.
        ##            x : le vecteur dans lequel chercher le motif.
        ##            ... : arguments supplémentaires pour 'grep'
        ## ----------------------------------------------------------------------
        ## Author: Yves Reecht, Date:  5 oct. 2010, 14:36

        return(sapply(x,
                      function(x2)
                  {
                      ## On teste pour chaque élément s'il contient le motif :
                      as.logical(length(grep(as.character(pattern), x2,...)))
                  }))
    }
}else{}                                 # Sinon rien à faire

########################################################################################################################
is.peche.f <- function()
{
    ## Purpose: Définir s'il s'ajit d'un jeu de données "pêche".
    ## ----------------------------------------------------------------------
    ## Arguments: aucun
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 19 oct. 2010, 15:45

    if (length(getOption("P.obsType")) > 1)
    {
        stop("Plusieurs types d'observations")
    }else{
        return(is.element(as.character(getOption("P.obsType")),
                          c("EMB", "DEB", "PSCI", "PecRec")))
    }
}


########################################################################################################################
pampaProfilingStart.f <- function(interval=0.1)
{
    ## Purpose: Initialiser le profilage d'une portion de code. Le nom de
    ##          fichier est construit d'après le nom de la fonction
    ##          appelante.
    ## ----------------------------------------------------------------------
    ## Arguments: interval : intervalle d'échantillonnage.
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 20 oct. 2010, 15:58

    if (!is.null(getOption("pampaProfiling")) && getOption("pampaProfiling"))
    {
        filename <- paste("logs/Rprof-", deparse(sys.call(-1)[[1]]), ".out", sep="")

        Rprof(filename=filename, interval=interval)
    }else{}
}



########################################################################################################################
pampaProfilingEnd.f <- function()
{
    ## Purpose: Stopper le profilage d'une portion de code.
    ## ----------------------------------------------------------------------
    ## Arguments: aucun.
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 20 oct. 2010, 15:58

    if (!is.null(getOption("pampaProfiling")) && getOption("pampaProfiling"))
    {
        Rprof(filename=NULL)
    }else{}
}

########################################################################################################################
runLog.f <- function(msg, niv=-1)
{
    ## Purpose: Écrire les appels de fonctions dans un fichier log, si et
    ##          seulement si l'option est activée.
    ## ----------------------------------------------------------------------
    ## Arguments: msg : message.
    ##            niv : niveau de l'appel pour retrouver la fonction
    ##                  appelante () ; si NULL, seul le message est
    ##                  écrit.
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date:  8 févr. 2011, 14:14

    on.exit(if (exists("logFile") &&
                tryCatch(isOpen(logFile),
                         error=function(e)return(FALSE))) close(logFile))

    ## Test d'existance et éventuelle création du dossier de logs :
    if (!isTRUE(file.info("./logs")$isdir))
      {
          dir.create("./logs")
      }

    ## Test d'existance et éventuelle création du fichier de log du jour :
    logFileName <- paste("Runs_", format(Sys.Date(), "%d-%m-%Y"), ".log", sep="")

    if (!file.exists(paste("./logs/", logFileName, sep="")) ||
        isTRUE(file.info(paste("./logs/", logFileName, sep=""))$isdir))
      {
          file.create(paste("./logs/", logFileName, sep=""))
      }

    logFile <- file(description=paste("./logs/", logFileName, sep=""),
                    open="a", encoding="latin1")

    ## on.exit(close(logFile))

    callingFct <- ifelse(is.null(niv),
                         "",
                         deparse(sys.call(niv)))

    cat(paste("\n", format(Sys.time(), "[%H:%M:%S] : "),
              paste(msg, collapse="\n\t"), "\n",
              paste(callingFct, collapse="\n\t"), "\n", sep=""),
        file=logFile)

    close(logFile)
}


########################################################################################################################
errorLog.f <- function(error, niv=-3)
{
    ## Purpose: Écrire les erreurs dans un fichier log + avertissement de
    ##          l'utilisateur
    ## ----------------------------------------------------------------------
    ## Arguments: error : erreur (récupérée par la fonction tryCatch).
    ##            niv : niveau de l'appel pour retrouver la fonction
    ##                  appelante (-3 par défaut pour tryCatch).
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 22 déc. 2010, 11:54

    on.exit(if (exists("logFile") &&
                tryCatch(isOpen(logFile),
                         error=function(e)return(FALSE))) close(logFile))

    ## Test d'existance et éventuelle création du dossier de logs :
    if (!isTRUE(file.info("./logs")$isdir))
      {
          dir.create("./logs")
      }

    ## Test d'existance et éventuelle création du fichier de log du jour :
    logFileName <- paste("Errors_", format(Sys.Date(), "%d-%m-%Y"), ".log", sep="")

    if (!file.exists(paste("./logs/", logFileName, sep="")) ||
        isTRUE(file.info(paste("./logs/", logFileName, sep=""))$isdir))
      {
          file.create(paste("./logs/", logFileName, sep=""))
      }

    logFile <- file(description=paste("./logs/", logFileName, sep=""),
                    open="a", encoding="latin1")


    callingFct <- sys.call(niv)

    cat(paste("\n", format(Sys.time(), "[%H:%M:%S]"), "\n",
              paste(deparse(callingFct), collapse="\n\t"), " :\n", sep=""),
        file=logFile)
    capture.output(print(error), file=logFile)
    cat("\n", file=logFile)

    close(logFile)

    message("\n\tIl y a eu une erreur.", "\n\tVoir le fichier log : ", logFileName, "\n")
}


########################################################################################################################
backupEnv.f <- function(envSource, envSink)
{
    ## Purpose: Sauvegarde d'un environnement dans un autre
    ## ----------------------------------------------------------------------
    ## Arguments: envSource : l'environnement à sauvegarder (marche également
    ##                        avec une liste).
    ##            envSink : l'environnement de sauvegarde (peut être
    ##                      manquant => nouvel environnement)
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 13 déc. 2011, 15:39

    ## Test de l'environnement "puit" :
    if (missing(envSink))
    {
        envSink <- new.env()            # Création s'il est manquant.
    }else{
        if (! is.environment(envSink)) stop("envSink n'est pas un environnement")
    }

    ## Copie des élements :
    invisible(sapply(envSource,
                     function(x, Nx, env)
                 {
                     i <- sys.call()[[2]][[3]]
                     assign(Nx[i], x, envir=env)
                 },
                     Nx=names(as.list(envSource)),
                     env=envSink))

    return(invisible(envSink))
}

########################################################################################################################
listInEnv.f <- function(list, env)
{
    ## Purpose: Copie les éléments d'une liste (nommée) dans un environnement
    ##          (avec comme nom d'élément son nom dans la liste).
    ## ----------------------------------------------------------------------
    ## Arguments: list : la liste à copier.
    ##            env : l'environnement dans lequel enregistrer les
    ##                  éléments.
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date:  4 janv. 2012, 15:38

    if (is.null(names(list)))
    {
        listNames <- paste("obj", seq(length.out=length(list)), sep="")

        warning("Liste non nommée : les éléments ont été nommés \"obj1\", \"obj2\", etc.")
    }else{
        listNames <- names(list)
    }

    invisible(sapply(list,
                     function(x, xN, env)
                 {
                     ## Numéro d'itération :
                     i <- sys.call()[[2]][[3]]

                     ## Assignement :
                     assign(xN[i], x, envir=env)
                 },
                     xN=listNames, env=env))
}

########################################################################################################################
writeData.f <- function(filename, Data, cols=NULL)
{
    ## Purpose: Écrire les données des graphiques ou analyses dans des
    ##          fichiers csv (format français).
    ## ----------------------------------------------------------------------
    ## Arguments: filename : nom du fichier à créer.
    ##            Data : jeu de données à sauvegarder.
    ##            cols : colonnes à sauvegarder (toutes si NULL).
    ##            filePathes : chemin des dossiers.
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date:  6 sept. 2012, 11:37

    ## Ajout de l'extension si besoin :
    if ( ! grepl("\\.csv$", filename[1], ignore.case=TRUE))
    {
        filename <- paste(filename, ".csv", sep="")
    }else{}

    ## Si l'argument est une liste de data.frame, elles sont agrégées :
    if (class(Data) == "list")
    {
        if (all(sapply(Data, class) == "data.frame"))
        {
            Data <- do.call(rbind, Data)
        }else{
            warning("Sauvegarde de données : erreur de programmation !")
        }

    }else{}

    ## Colonnes retenues :
    if (is.null(cols)) cols <- colnames(Data)

    cols <- cols[is.element(cols, colnames(Data))]


    ## Écriture du fichier
    tryCatch(write.csv2(Data[ , cols],
                        file=filename,
                        row.names = FALSE),
             error=function(e)
         {
             message("Impossible d'écrire dans :", filename)
             errorLog.f(error=e, niv=-4)
         })
}

########################################################################################################################
printGeneralDataInfo.f <- function(dataEnv, baseEnv, File)
{
    ## Purpose: Écrire dans un fichier les informations générales sur le jeu
    ##          de données (inclue les sélections au niveau de la
    ##          plateforme).
    ## ----------------------------------------------------------------------
    ## Arguments: dataEnv : environnement des données.
    ##            baseEnv : environnement de l'interface principale.
    ##            File : connection du fichier où écrire les informations.
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 11 sept. 2012, 10:36

    ## Informations sur les fichiers de données :
    cat(paste("################\nJeu de données :\n",
              "\n  * Identification de l'AMP : ", paste(getOption("P.MPA"), collapse=", "),
              "\n  * Répertoire de données : ", dataEnv$fileNames["ws"], "/Data/",
              "\n  * Données d'observation : ", dataEnv$fileNames["obs"],
              "\n  * Données unités d'observation :) : ", dataEnv$fileNames["unitobs"],
              "\n  * Référentiel espèces : ", dataEnv$fileNames["refesp"],
              "\n  * Référentiel spatial : ", dataEnv$fileNames["refspa"], "\n",
              sep=""),
        file=File)

    ## Sélections au niveau de la plateforme :
    cat(ifelse((tmp <- evalq(tclvalue(tkcget(MonCritere, "-text")), envir=.baseEnv)) == "Tout",
               "\nPas de sélection générale sur les données.\n",
               paste("\nSélection(s) générale(s):\n\n", tmp, "\n", sep="")),
        file=File)
}

########################################################################################################################
printSelectionInfo.f <- function(metrique, factGraph, factGraphSel, listFact, listFactSel,
                                 File,
                                 agregLevel=c("species", "unitobs"), type=c("graph", "stat"))
{
    ## Purpose: Écrire dans un fichier les informations sur la sélection de
    ##          données (obtenue par l'interface standard de sélection).
    ## ----------------------------------------------------------------------
    ## Arguments: metrique : la métrique choisie.
    ##            factGraph : le facteur sélection des espèces.
    ##            factGraphSel : la sélection de modalités pour ce dernier
    ##            listFact : liste du (des) facteur(s) de regroupement
    ##            listFactSel : liste des modalités sélectionnées pour ce(s)
    ##                          dernier(s)
    ##            File : connection du fichier où écrire les informations.
    ##            agregLevel : niveau d'agrégation de la fonction appelante.
    ##            type : type de fonction appelante (grapique ou analyse).
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 11 sept. 2012, 10:41

    cat("\n##################################################\n",
        "Métrique et facteurs (et éventuelles sélections) :\n",
        sep="", file=File)

    ## Informations sur la métrique :
    cat("\n Métrique :", varNames[metrique, "nom"],
        paste("(", varNames[metrique, "unite"],")", sep=""),
        "\n", file=File)

    ## Facteurs de séparation de graphiques/analyses ou sélection d'observations :
    switch(agregLevel,
           "species"=,"CL_espece"=,"espece"={ # Adapté également pour les LMs.
               cat("\nFacteur de séparation des ",
                   switch(type,
                          "graph"="graphiques",
                          "stat"="analyses"),
                   " : ",
                   ifelse(factGraph == "", "aucun !",
                          ifelse(is.na(factGraphSel[1]),
                                 paste(varNames[factGraph, "nom"], "(attention, aucune sélection !!!)"),
                                 paste(varNames[factGraph, "nom"], " (",
                                       paste(factGraphSel, collapse=", "), ")", sep=""))), "\n",
                   sep="", file=File)
           },
           "unitobs"=,"CL_unitobs"={
               cat("\nFacteur de sélection des observations à agréger : ",
                   ifelse(factGraph == "", "aucun (toutes les espèces/classes de taille) !",
                          ifelse(is.na(factGraphSel[1]),
                                 paste(varNames[factGraph, "nom"], "(aucune sélection)"),
                                 paste(varNames[factGraph, "nom"], " (",
                                       paste(factGraphSel, collapse=", "), ")", sep=""))), "\n",
                   sep="", file=File)
           })

    ## Facteurs de regroupements :
    cat(switch(type,
               "graph"="\nFacteur(s) de regroupement : ",
               "stat"="\nFacteur(s) de l'analyse : "), "\n",
        file=File)

    invisible(sapply(1:length(listFact),
                     function(i)
                 {
                     cat("\n  * ",
                         ifelse(is.na(listFactSel[[i]][1]),
                                       paste(varNames[listFact[i], "nom"], "(aucune sélection)"),
                                       paste(varNames[listFact[i], "nom"], " (",
                                             paste(listFactSel[[i]], collapse=", "), ")", sep="")), "\n",
                         sep="", file=File)
                 }))
}

########################################################################################################################
summary.fr <- function(object,...)
{
    ## Purpose: Franciser les sorties d'un summary (numeric uniquement).
    ## ----------------------------------------------------------------------
    ## Arguments: object : objet à résumer.
    ##            ... : argument supplémentaires passés à summary().
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 13 sept. 2012, 15:47

    if ( ! is.numeric(object)) stop("Erreur de programmation")

    ## Calcul du résumé :
    res <- summary(object=object, ...)

    ## Changement des noms d'éléments :
    names(res) <- c("Min.", "1er.Quart.", "Médiane", "Moyenne", "3e.Quart.", "Max")

    return(res)
}


########################################################################################################################
printStats.f <- function(Data, metrique, listFact, File, headline=NULL)
{
    ## Purpose: Écrire les tableaux de statistiques générales et par
    ##          croisement de facteur dans un fichier.
    ## ----------------------------------------------------------------------
    ## Arguments: Data : les données du graphique/de l'analyse.
    ##            metrique : nom de la métrique.
    ##            listFact : liste des facteurs de regroupement/de l'analyse.
    ##            File : la connection du fichier où écrire.
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 11 sept. 2012, 10:09

    ## Ligne d'en-tête (si besoin : traitement par espèces uniquement) :
    if ( ! is.null(headline))
    {
        cat("\n", rep("#", nchar(headline) + 3), "\n",
            "## ", headline, "\n",
            sep="", file=File)
    }else{}

    cat("\n########################\nStatistiques générales :\n\n", file=File)

    capture.output(print(summary.fr(Data[ , metrique])), file=File, append=TRUE)

    cat("\n#########################################",
        "\nStatistiques par croisement de facteurs :\n\n", file=File, sep="")

    ## Calcul du summary pour chaque croisement (existant) de facteur :
    res <- with(Data,
                tapply(eval(parse(text=metrique)),
                       INDEX=do.call(paste,
                                     c(lapply(listFact,
                                              function(y)eval(parse(text=y))),
                                       sep=".")),
                       FUN=summary.fr))

    ## Assemblage du résultat dans un tableau
    capture.output(print(do.call(rbind, res)),
                   file=File, append=TRUE)

    ## Ligne vide (pour l'esthétique) :
    cat("\n", file=File)
}


########################################################################################################################
infoStats.f <- function(filename, Data, agregLevel=c("species", "unitobs"), type=c("graph", "stat"),
                        metrique, factGraph, factGraphSel, listFact, listFactSel,
                        dataEnv, baseEnv=.GlobalEnv)
{
    ## Purpose: Écrire les infos et statistique sur les données associées à
    ##          un graphique ou analyse.
    ## ----------------------------------------------------------------------
    ## Arguments: filename : chemin du fichier de résultats.
    ##            Data : données du graphique/de l'analyse.
    ##            agregLevel : niveau d'agrégation de la fonction appelante.
    ##            type : type de fonction appelante (grapique ou analyse).
    ##            metrique : la métrique choisie.
    ##            factGraph : le facteur sélection des espèces.
    ##            factGraphSel : la sélection de modalités pour ce dernier
    ##            listFact : liste du (des) facteur(s) de regroupement
    ##            listFactSel : liste des modalités sélectionnées pour ce(s)
    ##                          dernier(s)
    ##            dataEnv : environnement de stockage des données.
    ##            baseEnv : environnement de l'interface.
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 10 sept. 2012, 15:26

    ## Ajout de l'extension si besoin :
    if ( ! grepl("\\.stats$", filename[1], ignore.case=TRUE))
    {
        filename <- paste(filename, ".stats", sep="")
    }else{}

    ## Ouverture du fichier :
    File <- file(description=filename,
                 open="w", encoding="latin1")

    ## Si erreur, on referme le fichier à la sortie de fonction :
    on.exit(if (exists("filename") &&
                tryCatch(isOpen(File),
                         error=function(e)return(FALSE))) close(File))

    ## Informations générales sur les données :
    printGeneralDataInfo.f(dataEnv=dataEnv, baseEnv=baseEnv, File=File)

    ## Informations sur les métriques et facteurs du graphique :
    printSelectionInfo.f(metrique=metrique, factGraph=factGraph, factGraphSel=factGraphSel,
                         listFact=listFact, listFactSel=listFactSel, File=File,
                         agregLevel=agregLevel, type=type)

    ## Statistiques :
    if (class(Data) == "list")
    {
        cat("\n###################################################",
            "\nStatistiques par niveaux de facteur de séparation :\n",
            sep="", file=File)

        invisible(sapply(1:length(Data),
                         function(i)
                     {
                         printStats.f(Data=Data[[i]], metrique=metrique, listFact=listFact, File=File,
                                      headline=factGraphSel[i])
                     }))
    }else{
        printStats.f(Data=Data, metrique=metrique, listFact=listFact, File=File,
                     headline=NULL)
    }

    ## Fermeture du fichier :
    close(File)

}






### Local Variables:
### ispell-local-dictionary: "english"
### fill-column: 120
### End:
