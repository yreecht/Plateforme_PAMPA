#-*- coding: latin-1 -*-
# Time-stamp: <2018-11-28 14:57:41 yreecht>

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

### File: Load_files_manually.R
### Created: <2012-02-24 20:23:01 Yves>
###
### Author: Yves Reecht
###
####################################################################################################
### Description:
###
### Scripts pour le chargement manuel des fichiers
####################################################################################################


########################################################################################################################
loadManual.f <- function(baseEnv, dataEnv)
{
    ## Purpose: Chargement des données avec choix manuel des fichiers et
    ##          dossiers.
    ## ----------------------------------------------------------------------
    ## Arguments: baseEnv : environnement de l'interface principale.
    ##            dataEnv : environnement des données.
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 14 déc. 2011, 18:20

    ## Sauvegarde et suppression des noms de fichiers, options, etc. :
    dataEnvTmp <- backupEnv.f(dataEnv) # On sauvegarde temporairement l'environnement de données.
    obsTypeTmp <- getOption("P.obsType") # ...et le type d'observations.

    ## Choix des fichiers et dossiers :
    fileNames <- chooseFiles.f(dataEnv = dataEnv)

    if ( ! is.null(fileNames))
    {
        ## ...après chooseFiles.f car utilise une éventuelle valeur préalable de l'espace de travail.
        suppressWarnings(rm(list=ls(envir=dataEnv)[!is.element(ls(envir=dataEnv), "fileNames")],
                            envir=dataEnv)) # [!!!] revoir  [yr: 13/12/2011]


        ## Vérification de la configuration :
        filePathes <- testConfig.f(requiredVar=getOption("P.requiredVar"),
                                   fileNames = fileNames,
                                   dataEnv = dataEnv)

        ## chargement (conditionnel) des données :
        if (! is.null(filePathes))
        {
            Data <- loadData.f(filePathes=filePathes, dataEnv=dataEnv, baseEnv = baseEnv)

            updateSummaryTable.f(get("tclarray", envir=baseEnv),
                                 filePathes, Data,
                                 get("table1", envir=baseEnv))
        }else{
            stop(mltext("error.config.issue"))
        }

        ## Calculs des poids (faits par AMP) :
        if ( ! is.benthos.f())
        {
            Data <- calcWeight.f(Data=Data)
        }else{}

        ## Assignement des données dans l'environnement adéquat :
        listInEnv.f(list=Data, env=dataEnv)

        ## assign("Data", Data, envir=.GlobalEnv) # [tmp]  [yr: 20/12/2011]

        ## Calcul des tables de métriques :
        metrics <- calcTables.f(obs=Data$obs, unitobs=Data$unitobs, refesp=Data$refesp, dataEnv=dataEnv)

        stepInnerProgressBar.f(n=2, msg=mltext("loadDefault.info.1"))

        ## Assignement des tables de métriques dans l'environnement adéquat :
        listInEnv.f(list=metrics, env=dataEnv)

        ## assign("metrics", metrics, envir=.GlobalEnv) # [tmp]  [yr: 20/12/2011]

        assign("backup", c(metrics,
                           list(obs=Data$obs),
                           tryCatch(list(".NombresSVR"=get(".NombresSVR", envir=dataEnv),
                                         ".DensitesSVR"=get(".DensitesSVR", envir=dataEnv)),
                                    error=function(e){NULL})),
               envir=dataEnv)

        ## Export des tables de métriques :
        stepInnerProgressBar.f(n=1, msg=mltext("loadDefault.info.2"))

        exportMetrics.f(unitSpSz=metrics$unitSpSz, unitSp=metrics$unitSp, unit=metrics$unit,
                        obs=Data$obs, unitobs=Data$unitobs, refesp=Data$refesp,
                        filePathes=filePathes, baseEnv=baseEnv)

        ## Ajout des fichiers créés au log de chargement :
        add.logFrame.f(msgID="fichiers", env = baseEnv,
                       results=filePathes["results"],
                       has.SzCl=( ! is.null(metrics$unitSpSz) &&
                                 prod(dim(metrics$unitSpSz))))

        ## Fin des informations de chargement (demande de confirmation utilisateur) :
        stepInnerProgressBar.f(n=2, msg=mltext("loadDefault.info.3"),
                               font=tkfont.create(weight="bold", size=9), foreground="darkred")

        updateInterface.load.f(baseEnv=baseEnv, tabObs=Data$obs)

        gestionMSGaide.f(namemsg="SelectionOuTraitement", env=baseEnv)

        infoLoading.f(button=TRUE, WinRaise=get("W.main", envir=baseEnv))

        ## [!!!] ajouter réinitialisation des menus si échec  [yr: 14/12/2011]
        ## return(Data)
    }else{}                             # Sinon rien !
}

########################################################################################################################
chooseWS.f <- function(dir=getwd(), env=NULL)
{
    ## Purpose: Choix d'un répertoir de travail.
    ## ----------------------------------------------------------------------
    ## Arguments: dir : répertoir initial.
    ##            env : environnement de l'interface de choix de fichier
    ##                  (optionnel) pour la modification du résumé.
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 14 déc. 2011, 18:45

    dir <- tclvalue(tkchooseDirectory(initialdir=ifelse(!is.na(dir),
                                                        dir, "")))

    if (!nchar(dir))
    {
        return(NULL)## Rien !
    }else{
        if (as.logical(length(grep("[dD]ata$", dir))) && !file.exists(paste(dir, "/Data", sep="")))
        {
            dir <- sub("/[dD]ata$", "", (oldDir <- dir))
            tkmessageBox(message=paste("\"", oldDir, mltext("chooseWS.info.1"), dir, "\" !",
                         mltext("chooseWS.info.2"), sep=""),
                         icon="warning")
        }else{}

        if (!is.null(env))
        {
            eval(substitute(evalq(tkconfigure(SummaryWS,
                                              text=dir,
                                              foreground="darkred"),
                                  envir=env),
                            list(dir=dir)))
        }else{}

        return(dir)
    }
}

########################################################################################################################
chooseUnitobs.f <- function(dir=getwd(), env=NULL)
{
    ## Purpose: Choix d'un fichier d'unités d'observation.
    ## ----------------------------------------------------------------------
    ## Arguments: dir : répertoir initial.
    ##            env : environnement de l'interface de choix de fichier
    ##                  (optionnel) pour la modification du résumé.
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 14 déc. 2011, 18:45

    runLog.f(msg=c(mltext("logmsg.manual.unitobs")))

    nameUnitobs <- tclvalue(tkgetOpenFile(initialdir=paste(dir, "/Data/", sep=""),
                                          filetypes = "{{Text files} {.txt .csv}} {{All files} *}"))

    ## On enlève le nom de chemin pour ne conserver que le nom du fichier:
    nameUnitobs <- basename(nameUnitobs)

    if (!nchar(nameUnitobs))
    {
        return(NULL) ## Rien !
    }else{
        if (!is.null(env))
        {
            eval(substitute(evalq(tkconfigure(SummaryUnitobs,
                                              text=nameUnitobs,
                                              foreground="darkred"),
                                  envir=env),
                            list(nameUnitobs=nameUnitobs)))
        }else{}

        return(nameUnitobs)
    }
}

########################################################################################################################
chooseObservations.f <- function(dir=getwd(), env=NULL)
{
    ## Purpose: Choix d'un fichier d'observations
    ## ----------------------------------------------------------------------
    ## Arguments: dir : répertoir initial.
    ##            env : environnement de l'interface de choix de fichier
    ##                  (optionnel) pour la modification du résumé.
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 14 déc. 2011, 18:45

    runLog.f(msg=c(mltext("logmsg.manual.obs")))

    namefileObs <- tclvalue(tkgetOpenFile(initialdir=paste(dir, "/Data/", sep=""),
                                          filetypes = "{{Text files} {.txt .csv}} {{All files} *}"))

    ## On enlève le nom de chemin pour ne conserver que le nom du fichier:
    namefileObs <- basename(namefileObs)

    if (!nchar(namefileObs))
    {
        return(NULL)
    }else{
        ## ici du coup, on peut y mettre un choix ou reconnaitre le référentiel automatiquement
        if (!is.null(env))
        {
            eval(substitute(evalq(tkconfigure(SummaryObs,
                                              text=namefileObs,
                                              foreground="darkred"),
                                  envir=env),
                            list(namefileObs=namefileObs)))
        }else{}

        return(namefileObs)
    }
}

########################################################################################################################
chooseRefesp.f <- function(dir=getwd(), env=NULL)
{
    ## Purpose: Choix d'un fichier de référentiel espèces
    ## ----------------------------------------------------------------------
    ## Arguments: dir : répertoir initial.
    ##            env : environnement de l'interface de choix de fichier
    ##                  (optionnel) pour la modification du résumé.
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 14 déc. 2011, 18:50

    runLog.f(msg=c(mltext("logmsg.manual.refesp")))

    namefileRef <- tclvalue(tkgetOpenFile(initialdir=paste(dir, "/Data/", sep=""),
                                          filetypes = "{{Text files} {.txt .csv}} {{All files} *}"))

    ## On enlève le nom de chemin pour ne conserver que le nom du fichier:
    namefileRef <- basename(namefileRef)

    if (!nchar(namefileRef))
    {
        return(NULL)
    }else{
        if (!is.null(env))
        {
            eval(substitute(evalq(tkconfigure(SummaryRefEsp,
                                              text=namefileRef,
                                              foreground="darkred"),
                                  envir=env),
                            list(namefileRef=namefileRef)))
        }else{}

        return(namefileRef)
    }
}

chooseRefesp.local.f <- function(dir=getwd(), env=NULL)
{
    ## Purpose: Choix d'un fichier de référentiel espèces
    ## ----------------------------------------------------------------------
    ## Arguments: dir : répertoir initial.
    ##            env : environnement de l'interface de choix de fichier
    ##                  (optionnel) pour la modification du résumé.
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 15 janv. 2013, 17:14

    runLog.f(msg=c(mltext("logmsg.manual.refesp.local")))

    namefileRef <- tclvalue(tkgetOpenFile(initialdir=paste(dir, "/Data/", sep=""),
                                          filetypes = "{{Text files} {.txt .csv}} {{All files} *}"))

    ## On enlève le nom de chemin pour ne conserver que le nom du fichier:
    namefileRef <- basename(namefileRef)

    if (!nchar(namefileRef))
    {
        return(NULL)
    }else{
        if (!is.null(env))
        {
            eval(substitute(evalq(tkconfigure(SummaryRefEspLoc,
                                              text=namefileRef,
                                              foreground="darkred"),
                                  envir=env),
                            list(namefileRef=namefileRef)))
        }else{}

        return(namefileRef)
    }
}


########################################################################################################################
chooseRefspa.f <- function(dir=getwd(), env=NULL)
{
    ## Purpose: Choix d'un fichier de référentiel spatial
    ## ----------------------------------------------------------------------
    ## Arguments: dir : répertoir initial.
    ##            env : environnement de l'interface de choix de fichier
    ##                  (optionnel) pour la modification du résumé.
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 14 déc. 2011, 18:55

    runLog.f(msg=c(mltext("logmsg.manual.refspa")))

    namefileRef <- tclvalue(tkgetOpenFile(initialdir=paste(dir, "/Data/", sep=""),
                                          filetypes = "{{Text & Shape files} {.txt .csv .shp}} {{All files} *}"))

    ## On enlève le nom de chemin pour ne conserver que le nom du fichier:
    namefileRef <- basename(namefileRef)

    if (!nchar(namefileRef))
    {
        return(NULL)
    }else{
        if (!is.null(env))
        {
            eval(substitute(evalq(tkconfigure(SummaryRefSpa,
                                              text=namefileRef,
                                              foreground="darkred"),
                                  envir=env),
                            list(namefileRef=namefileRef)))
        }else{}

        return(namefileRef)
    }
}


########################################################################################################################
chooseFiles.f <- function(dataEnv)
{
    ## Purpose: Choix manuel des fichiers et dossiers.
    ## ----------------------------------------------------------------------
    ## Arguments: dataEnv : environnement des données
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 14 déc. 2011, 18:31

    runLog.f(msg=c(mltext("logmsg.manual.load.interface")))


    ## ########################################################
    tt <- tktoplevel(height=50, width=300)
    tkwm.title(tt, mltext("chooseFiles.title"))

    ## Variables :
    Done <- tclVar(0)
    env <- environment()

    if (! missing(dataEnv) && is.environment(dataEnv) && exists("fileNames", envir=dataEnv))
    {
        fileNames <- get("fileNames", envir=dataEnv)
    }else{
        fileNames <- c(unitobs=NA,
                       obs=NA,
                       refesp=NA,
                       locrefesp=NA,
                       refspa=NA,
                       ws=getwd())
    }

    workSpaceTmp <- fileNames["ws"]
    unitobsTmp <- ifelse(is.na(fileNames["unitobs"]), character(), fileNames["unitobs"])
    obsTmp <- ifelse(is.na(fileNames["obs"]), character(), fileNames["obs"])
    refespTmp <- ifelse(is.na(fileNames["refesp"]), character(), fileNames["refesp"])
    locrefespTmp <- ifelse(is.na(fileNames["locrefesp"]), character(), fileNames["locrefesp"])
    refspaTmp <- ifelse(is.na(fileNames["refspa"]), character(), fileNames["refspa"])

    ## ########################################################

    ## Information importante :
    L.Info <-  tklabel(tt,
                       text=paste(mltext("chooseFiles.info.1"),
                                  mltext("chooseFiles.info.2"), sep=""),
                       bg="#FFFBCF", foreground="darkred",
                       font=tkfont.create(family="arial", ## weight="bold",
                                          size=9),#,
                       width=71, height=4, # taille.
                       relief="groove", borderwidth=2,
                       justify="left")

    button.widget0 <- tkbutton(tt, text=mltext("chooseFiles.info.WD"), ## width=45,
                               command=function()
                           {
                               if ( ! is.null(workSpaceTmp <- chooseWS.f(dir=workSpaceTmp, env=env)))
                               {
                                   assign("workSpaceTmp",
                                          workSpaceTmp,
                                          envir=env)
                               }
                               tcl("update")
                           },
                               justify="left")

    button.widget1 <- tkbutton(tt, text=mltext("chooseFiles.BT.1"),
                               command=function()
                           {
                               if ( ! is.null(unitobsTmp <- chooseUnitobs.f(dir=workSpaceTmp, env=env)))
                               {
                                   assign("unitobsTmp",
                                          unitobsTmp,
                                          envir=env)
                               }
                           },
                               justify="left")

    button.widget2 <- tkbutton(tt, text=mltext("chooseFiles.BT.2"),
                               command=function()
                           {
                               if ( ! is.null(obsTmp <- chooseObservations.f(dir=workSpaceTmp, env=env)))
                               {
                                   assign("obsTmp",
                                          obsTmp,
                                          envir=env)
                               }
                           },
                               justify="left")

    button.widget3 <- tkbutton(tt, text=mltext("chooseFiles.BT.3"),
                               command=function()
                           {
                               if ( ! is.null(refespTmp <- chooseRefesp.f(dir=workSpaceTmp, env=env)))
                               {
                                   assign("refespTmp",
                                          refespTmp,
                                          envir=env)
                               }
                           },
                               justify="left")

    button.widget32 <- tkbutton(tt, text=mltext("chooseFiles.BT.32"),
                                command=function()
                            {
                                if ( ! is.null(locrefespTmp <- chooseRefesp.local.f(dir=workSpaceTmp, env=env)))
                                {
                                    assign("locrefespTmp",
                                           locrefespTmp,
                                           envir=env)
                                }
                            },
                                justify="left")

    B.clear32 <- tkbutton(tt, text=mltext("chooseFiles.BT.clear"),
                          command=function()
                      {
                          assign("locrefespTmp",
                                 NA,
                                 envir=env)

                          tkconfigure(SummaryRefEspLoc, text=mltext("chooseFiles.info.none"))
                      })

    button.widget4 <- tkbutton(tt, text=mltext("chooseFiles.BT.4"),
                               command=function()
                           {
                               if ( ! is.null(refspaTmp <- chooseRefspa.f(dir=workSpaceTmp, env=env)))
                               {
                                   assign("refspaTmp",
                                          refspaTmp,
                                          envir=env)
                               }
                           },
                               justify="left")

    B.clear4 <- tkbutton(tt, text=mltext("chooseFiles.BT.clear"),
                         command=function()
                     {
                         assign("locrefespTmp",
                                NA,
                                envir=env)

                         tkconfigure(SummaryRefSpa, text=mltext("chooseFiles.info.none"))
                     })

    FrameBT <- tkframe(tt)

    OK.but <- tkbutton(FrameBT, text=mltext("chooseFiles.BT.OK"),
                       command=function(){tclvalue(Done) <- "1"})

    B.Cancel <- tkbutton(FrameBT, text=mltext("chooseFiles.BT.Cancel"),
                         command=function(){tclvalue(Done) <- "2"})

    tkgrid(L.Info,
           columnspan=3,
           pady=3, padx=5, sticky="ew")

    tkgrid(button.widget0,
           SummaryWS <- tklabel(tt, text=paste(mltext("chooseFiles.info.default"),
                                               ifelse(!is.na(workSpaceTmp),
                                                      workSpaceTmp,
                                                      mltext("chooseFiles.info.none")))),
           pady=3, padx=5, sticky="w")

    tkgrid(button.widget1,
           SummaryUnitobs <- tklabel(tt, text=paste(mltext("chooseFiles.info.default"),
                                                    ifelse(!is.na(unitobsTmp),
                                                           unitobsTmp,
                                                           mltext("chooseFiles.info.none")))),
           pady=3, padx=5, sticky="w")

    tkgrid(button.widget2,
           SummaryObs <- tklabel(tt, text=paste(mltext("chooseFiles.info.default"),
                                                ifelse(!is.na(obsTmp),
                                                       obsTmp,
                                                       mltext("chooseFiles.info.none")))),
           pady=3, padx=5, sticky="w")

    tkgrid(button.widget3,
           SummaryRefEsp <- tklabel(tt, text=paste(mltext("chooseFiles.info.default"),
                                                   ifelse(!is.na(refespTmp),
                                                          refespTmp,
                                                          mltext("chooseFiles.info.none")))),
           pady=3, padx=5, sticky="w")

    tkgrid(button.widget32,
           SummaryRefEspLoc <- tklabel(tt, text=paste(mltext("chooseFiles.info.default"),
                                                      ifelse(!is.na(locrefespTmp),
                                                             locrefespTmp,
                                                             mltext("chooseFiles.info.none")))),
           B.clear32,
           pady=3, padx=5, sticky="w")

    tkgrid(button.widget4,
           SummaryRefSpa <- tklabel(tt, text=paste(mltext("chooseFiles.info.default"),
                                                   ifelse(!is.na(refspaTmp),
                                                          refspaTmp,
                                                          mltext("chooseFiles.info.none")))),
           B.clear4,
           pady=3, padx=5, sticky="w")

    tkgrid(OK.but, tklabel(FrameBT, text="            "), B.Cancel, pady=5, padx=5)

    tkgrid(FrameBT, pady=5, padx=5, columnspan=2)

    ## Informations sur l'espace de travail en gras au passage sur le bouton de choix de celui-ci :
    tkbind(button.widget0,
           "<Enter>", function(){tkconfigure(L.Info,
                                             font=tkfont.create(family="arial", weight="bold", size=9))})

    tkbind(SummaryWS,
           "<Enter>", function(){tkconfigure(L.Info,
                                             font=tkfont.create(family="arial", weight="bold", size=9))})

    tkbind(button.widget0,
           "<Leave>", function(){tkconfigure(L.Info,
                                             font=tkfont.create(family="arial", size=9))})

    tkbind(SummaryWS,
           "<Leave>", function(){tkconfigure(L.Info,
                                             font=tkfont.create(family="arial", size=9))})

    tkbind(tt, "<Destroy>", function(){tclvalue(Done) <- "2"})

    tkgrid.configure(button.widget0, button.widget1, button.widget2, button.widget3, button.widget32, button.widget4,
                     sticky="ew")


    tkfocus(tt)
    tcl("update")
    winSmartPlace.f(tt)

    tkwait.variable(Done)

    if (tclvalue(Done) == "1")
    {
        tkdestroy(tt)

        fileNames <- c(unitobs=unname(unitobsTmp),
                       obs=unname(obsTmp),
                       refesp=unname(refespTmp),
                       locrefesp=unname(locrefespTmp),
                       refspa=unname(refspaTmp),
                       ws=unname(workSpaceTmp))

        ## Sauvegarde dans l'environnement des données :
        if (! missing(dataEnv) && is.environment(dataEnv))
        {
            assign("fileNames", fileNames, envir=dataEnv)
        }else{}

        ## Retourne les noms de fichiers :
        return(fileNames)
    }else{
        tkdestroy(tt)

        return(NULL)
    }
}



### Local Variables:
### ispell-local-dictionary: "english"
### fill-column: 120
### End:

