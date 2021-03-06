#-*- coding: latin-1 -*-

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

### File: fonctions_base.R
### Time-stamp: <2012-01-18 17:45:51 yreecht>
###
### Author: Yves Reecht
###
####################################################################################################
### Description:
###
### Fonctions de bases de la plateforme (�galement utilis� pour d�finir des fonctions de base
### r�centes de R lorsqu'elles n'existent pas, par ex si on travaille sur une version ancienne de R)
####################################################################################################

########################################################################################################################
varNames.f <- function(fields, info="name", quote=TRUE)
{
    ## Purpose: revoyer les informations (en particulier nom) sur le nom
    ##          "d'usage" d'un ou plusieurs champ(s).
    ## ----------------------------------------------------------------------
    ## Arguments: fields : champ(s) recherch�(s).
    ##            info : type d'info ("name", "article", "gender", "unit")
    ##            quote : faut-il mettre des guillemets pour les noms de
    ##                    champs tels-quels (pas de nom d'usage d�fini).
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 21 f�vr. 2013, 18:21

    info <- info[1]

    if (is.element(info, c("nom", "name")))
    {
        ## S'il n'est pas d�finit, le nom d'usage est remplac� par le nom de champ plut�t que par NA :
        res <- ifelse(is.na(tmp <- varNames[fields, "nom"]),
                      paste(ifelse(quote, "\"", ""),
                            fields,
                            ifelse(quote, "\"", ""), sep=""),
                      tmp)
    }else{
        ## Possibilit� de nommer les infos en fran�ais et anglais:
        res <- ifelse(is.na(varNames[fields, info]),
                      "",
                      varNames[fields,
                               switch(info,
                                      "article"="article",
                                      "genre"=,
                                      "gender"="genre",
                                      "unite"=,
                                      "unit"="unite",
                                      "nom")])
    }

    return(res)
}


########################################################################################################################
dropLevels.f <- function(df, which=NULL)
{
    ## Purpose: Supprimer les 'levels' non utilis�s des facteurs d'une
    ##          data.frame.
    ## ----------------------------------------------------------------------
    ## Arguments: df : une data.frame
    ##            which : indice des colonnes � inclure (toutes par d�faut).
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 10 ao�t 2010, 13:29

    if (class(df) != "data.frame")
    {
        stop("'df' must be a data.frame")
    }else{
        if (is.null(which))
        {
            x <- as.data.frame(sapply(df, function(x)
                                  {
                                      return(x[ ,drop=TRUE])
                                  }, simplify=FALSE),
                               stringsAsFactors=FALSE)
        }else{                          # Cas o� seulement certaines colonnes sont trait�es.
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
    ## Purpose: Mettre en majuscule la premi�re lettre de chaque mot
    ## ----------------------------------------------------------------------
    ## Arguments: x : une cha�ne de caract�res
    ##            words : tous les mots (TRUE), ou juste le premier.
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date:  9 ao�t 2010, 21:08

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
        ## Purpose: �mulation des fonctions de 'grepl' (en moins efficace
        ##          probablement) si la fonction n'existe pas.
        ## ----------------------------------------------------------------------
        ## Arguments: pattern : le motif � rechercher.
        ##            x : le vecteur dans lequel chercher le motif.
        ##            ... : arguments suppl�mentaires pour 'grep'
        ## ----------------------------------------------------------------------
        ## Author: Yves Reecht, Date:  5 oct. 2010, 14:36

        return(sapply(x,
                      function(x2)
                  {
                      ## On teste pour chaque �l�ment s'il contient le motif :
                      as.logical(length(grep(as.character(pattern), x2,...)))
                  }))
    }
}else{}                                 # Sinon rien � faire

########################################################################################################################
is.peche.f <- function()
{
    ## Purpose: D�finir s'il s'agit d'un jeu de donn�es "p�che".
    ## ----------------------------------------------------------------------
    ## Arguments: aucun
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 19 oct. 2010, 15:45

    if (length(getOption("P.obsType")) > 1)
    {
        stop("Several observation types")
    }else{
        return(is.element(as.character(getOption("P.obsType")),
                          c("EMB", "DEB", "PSCI", "PecRec")))
    }
}


########################################################################################################################
pampaProfilingStart.f <- function(interval=0.1)
{
    ## Purpose: Initialiser le profilage d'une portion de code. Le nom de
    ##          fichier est construit d'apr�s le nom de la fonction
    ##          appelante.
    ## ----------------------------------------------------------------------
    ## Arguments: interval : intervalle d'�chantillonnage.
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
    ## Purpose: �crire les appels de fonctions dans un fichier log, si et
    ##          seulement si l'option est activ�e.
    ## ----------------------------------------------------------------------
    ## Arguments: msg : message.
    ##            niv : niveau de l'appel pour retrouver la fonction
    ##                  appelante () ; si NULL, seul le message est
    ##                  �crit.
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date:  8 f�vr. 2011, 14:14

    on.exit(if (exists("logFile") &&
                tryCatch(isOpen(logFile),
                         error = function(e)return(FALSE))) close(logFile))

    logDir <- file.path(PAMPAhome, "logs")

    ## Test d'existance et �ventuelle cr�ation du dossier de logs:
    if (! dir.exists(logDir))
    {
        dir.create(logDir)
    }

    ## Test d'existance et �ventuelle cr�ation du fichier de log du jour :
    logFileName <- paste("Runs_", format(Sys.Date(), "%d-%m-%Y"), ".log", sep="")

    if (!file.exists(file.path(logDir, logFileName)) ||
        isTRUE(file.info(file.path(logDir, logFileName))$isdir))
    {
        file.create(file.path(logDir, logFileName))
    }

    logFile <- file(description = file.path(logDir, logFileName),
                    open = "a", encoding = "latin1")

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
    ## Purpose: �crire les erreurs dans un fichier log + avertissement de
    ##          l'utilisateur
    ## ----------------------------------------------------------------------
    ## Arguments: error : erreur (r�cup�r�e par la fonction tryCatch).
    ##            niv : niveau de l'appel pour retrouver la fonction
    ##                  appelante (-3 par d�faut pour tryCatch).
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 22 d�c. 2010, 11:54

    on.exit(if (exists("logFile") &&
                tryCatch(isOpen(logFile),
                         error=function(e)return(FALSE))) close(logFile))

    logDir <- file.path(PAMPAhome, "logs")

    ## Test d'existance et �ventuelle cr�ation du dossier de logs :
    if (! dir.exists(logDir))
      {
          dir.create(logDir)
      }

    ## Test d'existance et �ventuelle cr�ation du fichier de log du jour :
    logFileName <- paste("Errors_", format(Sys.Date(), "%d-%m-%Y"), ".log", sep="")

    if (!file.exists(file.path(logDir, logFileName)) ||
        isTRUE(file.info(file.path(logDir, logFileName))$isdir))
      {
          file.create(file.path(logDir, logFileName))
      }

    logFile <- file(description=file.path(logDir, logFileName),
                    open="a", encoding="latin1")


    callingFct <- sys.call(niv)

    cat(paste("\n", format(Sys.time(), "[%H:%M:%S]"), "\n",
              paste(deparse(callingFct), collapse="\n\t"), " :\n", sep=""),
        file=logFile)
    capture.output(print(error), file=logFile)
    cat("\n", file=logFile)

    close(logFile)

    message(mltext("errorLog.f.msg1"), mltext("errorLog.f.msg2"), logFileName, "\n")
}


########################################################################################################################
backupEnv.f <- function(envSource, envSink)
{
    ## Purpose: Sauvegarde d'un environnement dans un autre
    ## ----------------------------------------------------------------------
    ## Arguments: envSource : l'environnement � sauvegarder (marche �galement
    ##                        avec une liste).
    ##            envSink : l'environnement de sauvegarde (peut �tre
    ##                      manquant => nouvel environnement)
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 13 d�c. 2011, 15:39

    ## Test de l'environnement "puit" :
    if (missing(envSink))
    {
        envSink <- new.env()            # Cr�ation s'il est manquant.
    }else{
        if (! is.environment(envSink)) stop("envSink isn't an environnement")
    }

    ## Copie des �lements :
    invisible(sapply(envSource,
                     function(x, Nx, env)
                 {
                     i <- sys.call()[[2]][[3]]

                     if (is.symbol(x = i)) # for compatibility R > 3.1
                     {
                         i <- eval(i, sys.frame(-1))
                     }
                     assign(Nx[i], x, envir=env)
                 },
                     Nx=names(as.list(envSource)),
                     env=envSink))

    return(invisible(envSink))
}

########################################################################################################################
listInEnv.f <- function(list, env)
{
    ## Purpose: Copie les �l�ments d'une liste (nomm�e) dans un environnement
    ##          (avec comme nom d'�l�ment son nom dans la liste).
    ## ----------------------------------------------------------------------
    ## Arguments: list : la liste � copier.
    ##            env : l'environnement dans lequel enregistrer les
    ##                  �l�ments.
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date:  4 janv. 2012, 15:38

    if (is.null(names(list)))
    {
        listNames <- paste("obj", seq(length.out=length(list)), sep="")

        warning("Unnamed list: the elements have been named \"obj1\", \"obj2\", etc.")
    }else{
        listNames <- names(list)
    }

    invisible(sapply(list,
                     function(x, xN, env)
                 {
                     ## Num�ro d'it�ration :
                     i <- sys.call()[[2]][[3]]

                     if (is.symbol(x = i)) # for compatibility R > 3.1
                     {
                         i <- eval(i, sys.frame(-1))
                     }
                     ## Assignement :
                     assign(x = xN[i], value = x, envir=env)
                 },
                     xN=listNames, env=env))
}

########################################################################################################################
writeData.f <- function(filename, Data, cols=NULL)
{
    ## Purpose: �crire les donn�es des graphiques ou analyses dans des
    ##          fichiers csv (format fran�ais).
    ## ----------------------------------------------------------------------
    ## Arguments: filename : nom du fichier � cr�er.
    ##            Data : jeu de donn�es � sauvegarder.
    ##            cols : colonnes � sauvegarder (toutes si NULL).
    ##            filePathes : chemin des dossiers.
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date:  6 sept. 2012, 11:37

    ## Ajout de l'extension si besoin :
    if ( ! grepl("\\.csv$", filename[1], ignore.case=TRUE))
    {
        filename <- paste(filename, ".csv", sep="")
    }else{}

    ## Si l'argument est une liste de data.frame, elles sont agr�g�es :
    if (class(Data) == "list")
    {
        if (all(sapply(Data, class) == "data.frame"))
        {
            Data <- do.call(rbind, Data)
        }else{
            warning("Data saving: programming error!")
        }

    }else{}

    ## Colonnes retenues :
    if (is.null(cols)) cols <- colnames(Data)

    cols <- cols[is.element(cols, colnames(Data))]


    ## �criture du fichier
    tryCatch(write.csv(Data[ , cols],
                       file=filename,
                       row.names = FALSE),
             error=function(e)
         {
             message(mltext("writeData.f.msg"), filename)
             errorLog.f(error=e, niv=-4)
         })
}

########################################################################################################################
printGeneralDataInfo.f <- function(dataEnv, baseEnv, File)
{
    ## Purpose: �crire dans un fichier les informations g�n�rales sur le jeu
    ##          de donn�es (inclue les s�lections au niveau de la
    ##          plateforme).
    ## ----------------------------------------------------------------------
    ## Arguments: dataEnv : environnement des donn�es.
    ##            baseEnv : environnement de l'interface principale.
    ##            File : connection du fichier o� �crire les informations.
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 11 sept. 2012, 10:36

    ## Informations sur les fichiers de donn�es :
    cat(paste(mltext("printGeneralDataInfo.f.1"),
              mltext("printGeneralDataInfo.f.2"), paste(getOption("P.MPA"), collapse=", "),
              mltext("printGeneralDataInfo.f.3"), dataEnv$fileNames["ws"], "/Data/",
              mltext("printGeneralDataInfo.f.4"), dataEnv$fileNames["obs"],
              mltext("printGeneralDataInfo.f.5"), dataEnv$fileNames["unitobs"],
              mltext("printGeneralDataInfo.f.6"), dataEnv$fileNames["refesp"],
              mltext("printGeneralDataInfo.f.7"), dataEnv$fileNames["refspa"], "\n",
              sep=""),
        file=File)

    ## S�lections au niveau de la plateforme :
    cat(ifelse((tmp <- evalq(tclvalue(tkcget(MonCritere, "-text")), envir=.baseEnv)) == "Tout",
               mltext("printGeneralDataInfo.f.8"),
               paste(mltext("printGeneralDataInfo.f.9"), tmp, "\n", sep="")),
        file=File)
}

########################################################################################################################
printSelectionInfo.f <- function(metrique, factGraph, factGraphSel, listFact, listFactSel,
                                 File,
                                 agregLevel=c("species", "unitobs"), type=c("graph", "stat"))
{
    ## Purpose: �crire dans un fichier les informations sur la s�lection de
    ##          donn�es (obtenue par l'interface standard de s�lection).
    ## ----------------------------------------------------------------------
    ## Arguments: metrique : la m�trique choisie.
    ##            factGraph : le facteur s�lection des esp�ces.
    ##            factGraphSel : la s�lection de modalit�s pour ce dernier
    ##            listFact : liste du (des) facteur(s) de regroupement
    ##            listFactSel : liste des modalit�s s�lectionn�es pour ce(s)
    ##                          dernier(s)
    ##            File : connection du fichier o� �crire les informations.
    ##            agregLevel : niveau d'agr�gation de la fonction appelante.
    ##            type : type de fonction appelante (grapique ou analyse).
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 11 sept. 2012, 10:41

    cat("\n##################################################\n",
        mltext("printSelectionInfo.f.1"),
        sep="", file=File)

    ## Informations sur la m�trique :
    cat(mltext("printSelectionInfo.f.2"), varNames[metrique, "nom"],
        paste("(", varNames[metrique, "unite"],"),", sep=""),
        "\n", file=File)

    ## Niveau d'agr�gation :
    cat(mltext("printSelectionInfo.f.3"),
        switch(agregLevel,
               "CL_espece"=,"CL_unitobs"=,"spCL_unitobs"=,"spCL_espece"={
                   mltext("printSelectionInfo.f.4")
               }),
        switch(agregLevel,
               "CL_espece"=,"spCL_espece"=,"species"=,"spSpecies"=,"spEspece"={
                   mltext("printSelectionInfo.f.5")
               }),
        switch(agregLevel,
               "spUnitobs"=,"spCL_unitobs"=,"spCL_espece"=,"spUnitobs(CL)"=,"spSpecies"=,"spEspece"={
                   paste(listFact, mltext("printSelectionInfo.f.6"), sep="")
              }),
        mltext("printSelectionInfo.f.7"),
        switch(agregLevel,
               "spUnitobs"=,"spCL_unitobs"=,"spCL_espece"=,"spUnitobs(CL)"=,"spSpecies"=,"spEspece"={
                   ")"
              }),
        ".\n",
        sep="", file=File)

    ## Facteurs de s�paration de graphiques/analyses ou s�lection d'observations :
    switch(agregLevel,
           "species"=,"CL_espece"=,"espece"={ # Adapt� �galement pour les LMs.
               cat(mltext("printSelectionInfo.f.8"),
                   switch(type,
                          "graph"=mltext("printSelectionInfo.f.9"),
                          "stat"=mltext("printSelectionInfo.f.10")),
                   " : ",
                   ifelse(factGraph == "", "printSelectionInfo.f.11",
                          ifelse(is.na(factGraphSel[1]),
                                 paste(varNames.f(factGraph, "nom"), mltext("printSelectionInfo.f.12")),
                                 paste(varNames.f(factGraph, "nom"), " (",
                                       paste(factGraphSel, collapse=", "), ")", sep=""))), "\n",
                   sep="", file=File)
           },
           "unitobs"=,"CL_unitobs"=,"unitobs(CL)"=,"spUnitobs"={
               cat(mltext("printSelectionInfo.f.13"),
                   ifelse(factGraph == "", mltext("printSelectionInfo.f.14"),
                          ifelse(is.na(factGraphSel[1]),
                                 paste(varNames.f(factGraph, "nom"), mltext("printSelectionInfo.f.15")),
                                 paste(varNames.f(factGraph, "nom"), " (",
                                       paste(factGraphSel, collapse=", "), ")", sep=""))), "\n",
                   sep="", file=File)
           })

    ## Facteurs de regroupements :
    if (is.element(agregLevel, c("spCL_unitobs", "spCL_espece", "spSpecies", "spEspece",
                                 "spUnitobs", "spUnitobs(CL)"))) {type <- "spatialGraph"}

    cat(switch(type,
               "graph"=mltext("printSelectionInfo.f.16"),
               "stat"=mltext("printSelectionInfo.f.17"),
               "spatialGraph"=mltext("printSelectionInfo.f.18")), "\n",
        file=File)

    invisible(sapply(1:length(listFact),
                     function(i)
                 {
                     cat("\n  * ",
                         ifelse(is.na(listFactSel[[i]][1]),
                                       paste(varNames.f(listFact[i], "nom"), mltext("printSelectionInfo.f.15")),
                                       paste(varNames.f(listFact[i], "nom"), " (",
                                             paste(listFactSel[[i]], collapse=", "), ")", sep="")), "\n",
                         sep="", file=File)
                 }))
}

########################################################################################################################
summary.fr <- function(object, digits = max(3, getOption("digits") - 3),...)
{
    ## Purpose: Franciser les sorties d'un summary (numeric uniquement).
    ## ----------------------------------------------------------------------
    ## Arguments: object : objet � r�sumer.
    ##            ... : argument suppl�mentaires pass�s � summary().
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 13 sept. 2012, 15:47

    if ( ! is.numeric(object)) stop("Programming error")

    ## Calcul du r�sum� :
    res <- c(summary(object=object, digits, ...), "sd"=signif(sd(x=object), digits=digits), "N"=length(object))

    ## Changement des noms d'�l�ments :
    names(res) <- c(mltext("summary.min"), mltext("summary.1stQ"),
                    mltext("summary.med"), mltext("summary.mean"),
                    mltext("summary.3rdQ"), mltext("summary.max"),
                    mltext("summary.sd"), mltext("summary.N"))

    return(res)
}


########################################################################################################################
printStats.f <- function(Data, metrique, listFact, File, headline=NULL)
{
    ## Purpose: �crire les tableaux de statistiques g�n�rales et par
    ##          croisement de facteur dans un fichier.
    ## ----------------------------------------------------------------------
    ## Arguments: Data : les donn�es du graphique/de l'analyse.
    ##            metrique : nom de la m�trique.
    ##            listFact : liste des facteurs de regroupement/de l'analyse.
    ##            File : la connection du fichier o� �crire.
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 11 sept. 2012, 10:09

    ## Ligne d'en-t�te (si besoin : traitement par esp�ces uniquement) :
    if ( ! is.null(headline))
    {
        cat("\n", rep("#", nchar(headline) + 3), "\n",
            "## ", headline, "\n",
            sep="", file=File)
    }else{}

    cat(mltext("printStats.f.1"), file=File)

    capture.output(print(summary.fr(Data[ , metrique])), file=File, append=TRUE)

    if ( ! is.null(listFact))
    {
        cat("\n#########################################",
            mltext("printStats.f.2"), file=File, sep="")

        ## Calcul du summary pour chaque croisement (existant) de facteur :
        res <- with(Data,
                    tapply(eval(parse(text=metrique)),
                           INDEX=do.call(paste,
                                         c(lapply(listFact,
                                                  function(y)eval(parse(text=y))),
                                           sep=".")),
                           FUN=summary.fr))

        ## Assemblage du r�sultat dans un tableau
        capture.output(print(do.call(rbind, res)),
                       file=File, append=TRUE)
    }else{}

    ## Ligne vide (pour l'esth�tique) :
    cat("\n", file=File)
}


########################################################################################################################
infoStats.f <- function(filename, Data, agregLevel=c("species", "unitobs"), type=c("graph", "stat"),
                        metrique, factGraph, factGraphSel, listFact, listFactSel,
                        dataEnv, baseEnv=.GlobalEnv)
{
    ## Purpose: �crire les infos et statistique sur les donn�es associ�es �
    ##          un graphique ou analyse.
    ## ----------------------------------------------------------------------
    ## Arguments: filename : chemin du fichier de r�sultats.
    ##            Data : donn�es du graphique/de l'analyse.
    ##            agregLevel : niveau d'agr�gation de la fonction appelante.
    ##            type : type de fonction appelante (grapique ou analyse).
    ##            metrique : la m�trique choisie.
    ##            factGraph : le facteur s�lection des esp�ces.
    ##            factGraphSel : la s�lection de modalit�s pour ce dernier
    ##            listFact : liste du (des) facteur(s) de regroupement
    ##            listFactSel : liste des modalit�s s�lectionn�es pour ce(s)
    ##                          dernier(s)
    ##            dataEnv : environnement de stockage des donn�es.
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

    ## Si erreur, on referme le fichier � la sortie de fonction :
    on.exit(if (exists("filename") &&
                tryCatch(isOpen(File),
                         error=function(e)return(FALSE))) close(File))

    ## Informations g�n�rales sur les donn�es :
    printGeneralDataInfo.f(dataEnv=dataEnv, baseEnv=baseEnv, File=File)

    ## Informations sur les m�triques et facteurs du graphique :
    printSelectionInfo.f(metrique=metrique, factGraph=factGraph, factGraphSel=factGraphSel,
                         listFact=listFact, listFactSel=listFactSel, File=File,
                         agregLevel=agregLevel, type=type)

    ## Statistiques :
    if (class(Data) == "list")
    {
        cat("\n###################################################",
            mltext("infoStats.f.1"),
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

########################################################################################################################
shell.exec  <- function(x, wait = FALSE, ...)
{
    ## Purpose: Replacement for shell.exe (doesn't exist on Linux or MAC)
    ## ----------------------------------------------------------------------
    ## Arguments: x: document path to be opened
    ##            wait: whether to hold the R console
    ##            ...: optional arguments to system()
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date:  4 Jun 2020, 21:16

    if (exists("shell.exec",where = "package:base"))
        return(base::shell.exec(x))

    if (.Platform$OS.type == "unix")
    {
        comm  <- paste0("xdg-open \"", x, "\"")
    }else{
        comm <- paste0("open ", x)
    }
    return(system(comm, wait = wait, ...))
}





### Local Variables:
### ispell-local-dictionary: "english"
### fill-column: 120
### End:
