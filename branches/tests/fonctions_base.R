#-*- coding: latin-1 -*-

### File: fonctions_base.R
### Time-stamp: <2011-04-12 17:00:31 yreecht>
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
                                                return(x[ ,drop=TRUE])
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

    if (length(unique(unitobs$type)) > 1)
    {
        stop("Plusieurs types d'observations")
    }else{
        return(is.element(as.character(unique(unitobs$type)),
                          c("EMB", "DEB", "PSCI", "PecRec")))
    }
}


########################################################################################################################
pampaProfilingStart.f <- function()
{
    ## Purpose:
    ## ----------------------------------------------------------------------
    ## Arguments:
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 20 oct. 2010, 15:58

    if (!is.null(getOption("pampaProfiling")) && getOption("pampaProfiling"))
    {
        filename <- paste("Rprof-", deparse(sys.call(-1)[[1]]), ".out", sep="")

        Rprof(filename=filename, interval=0.1)
    }else{}
}



########################################################################################################################
pampaProfilingEnd.f <- function()
{
    ## Purpose:
    ## ----------------------------------------------------------------------
    ## Arguments:
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
    ##                  écrit
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




### Local Variables:
### ispell-local-dictionary: "english"
### fill-column: 120
### End:
