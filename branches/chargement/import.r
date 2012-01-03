########################################################################################################################
chercheEspaceTravail.f <- function(dir="", env=NULL)
{
    dir <- tclvalue(tkchooseDirectory(initialdir=ifelse(!is.na(dir),
                                                        dir, "")))

    if (!nchar(dir))
    {
        return(NULL)## Rien !
    }else{
        if (as.logical(length(grep("[dD]ata$", dir))) && !file.exists(paste(dir, "/Data", sep="")))
        {
            dir <- sub("/[dD]ata$", "", (oldDir <- dir))
            tkmessageBox(message=paste("\"", oldDir, "\" changé en \"", dir, "\" !",
                         "\n\nLe dossier de donnée est un sous-répertoire de l'espace de travail.", sep=""),
                         icon="warning")
        }else{}

        if (!is.null(env))
        {
            eval(substitute(evalq(tkconfigure(ResumerEspaceTravail,
                                              text=dir,
                                              foreground="darkred"),
                                  envir=env),
                            list(dir=dir)))
        }else{}

        return(dir)
    }
}

########################################################################################################################
openUnitobs.f <- function(dir, env=NULL)
{
    runLog.f(msg=c("Choix manuel du fichiers d'unités d'observations :"))

    nameUnitobs <- tclvalue(tkgetOpenFile(initialdir=paste(dir, "/Data/", sep="")))

    ## On enlève le nom de chemin pour ne conserver que le nom du fichier:
    nameUnitobs <- sub(paste(dir, "/Data/", sep=""), '', nameUnitobs)

    if (!nchar(nameUnitobs))
    {
        return(NULL) ## Rien !
    }else{
        if (!is.null(env))
        {
            eval(substitute(evalq(tkconfigure(ResumerSituationFichierUnitesObs,
                                              text=nameUnitobs,
                                              foreground="darkred"),
                                  envir=env),
                            list(nameUnitobs=nameUnitobs)))
        }else{}

        return(nameUnitobs)

        ## assign("fileNameUnitobs", nameUnitobs, envir=.GlobalEnv)
    }
}

########################################################################################################################
openObservations.f <- function(dir, env=NULL)
{
    runLog.f(msg=c("Choix manuel du fichiers d'observations :"))

    namefileObs <- tclvalue(tkgetOpenFile(initialdir=paste(dir, "/Data/", sep="")))

    ## On enlève le nom de chemin pour ne conserver que le nom du fichier:
    namefileObs <- sub(paste(dir, "/Data/", sep=""), '', namefileObs)

    if (!nchar(namefileObs))
    {
        return(NULL)
    }else{
        ## ici du coup, on peut y mettre un choix ou reconnaitre le référenciel automatiquement
        if (!is.null(env))
        {
            eval(substitute(evalq(tkconfigure(ResumerSituationFichierObs,
                                              text=namefileObs,
                                              foreground="darkred"),
                                  envir=env),
                            list(namefileObs=namefileObs)))
        }else{}

        return(namefileObs)
    }
}

########################################################################################################################
openListespeces.f <- function(dir, env=NULL)
{
    runLog.f(msg=c("Choix manuel du fichiers du référentiel espèces :"))

    namefileRef <- tclvalue(tkgetOpenFile(initialdir=paste(dir, "/Data/", sep="")))

    ## On enlève le nom de chemin pour ne conserver que le nom du fichier:
    namefileRef <- sub(paste(dir, "/Data/", sep=""), '', namefileRef)

    if (!nchar(namefileRef))
    {
        return(NULL)
    }else{
        if (!is.null(env))
        {
            eval(substitute(evalq(tkconfigure(ResumerSituationReferencielEspece,
                                              text=namefileRef,
                                              foreground="darkred"),
                                  envir=env),
                            list(namefileRef=namefileRef)))
        }else{}

        return(namefileRef)
    }
}

openfile.f <- function()
{
    runLog.f(msg=c("Interface de choix manuel des fichiers de données."))


    ## ########################################################
    tt <- tktoplevel(height=50, width=300)
    tkwm.title(tt, "Choix des fichiers de données à importer")

    ## Variables :
    Done <- tclVar(0)
    env <- environment()

    workSpaceTmp <- get("nameWorkspace", envir=.GlobalEnv)
    unitobsTmp <- ifelse(exists("fileNameUnitobs", envir=.GlobalEnv), get("fileNameUnitobs", envir=.GlobalEnv), character())
    obsTmp <- ifelse(exists("fileNameObs", envir=.GlobalEnv), get("fileNameObs", envir=.GlobalEnv), character())
    refespTmp <- ifelse(exists("fileNameRefesp", envir=.GlobalEnv), get("fileNameRefesp", envir=.GlobalEnv), character())

    ## ########################################################

    ## Information importante :
    L.Info <-  tklabel(tt,
                       text=paste("L'espace de travail est le répertoire qui contient le dossier \"Data\".",
                                  "\nNe pas selectionner ce dernier !", sep=""),
                       bg="#FFFBCF", foreground="darkred",
                       font=tkfont.create(family="arial", ## weight="bold",
                                          size=9),#,
                       width=71, height=3, # taille.
                       relief="groove", borderwidth=2,
                       justify="left")

    button.widget0 <- tkbutton(tt, text="Espace de travail", ## width=45,
                               command=function()
                           {
                               assign("workSpaceTmp",
                                      chercheEspaceTravail.f(dir=workSpaceTmp, env=env),
                                      envir=env)
                           },
                               justify="left")

    button.widget1 <- tkbutton(tt, text="Table de données d'unités d'observation",
                               command=function()
                           {
                               assign("unitobsTmp",
                                      openUnitobs.f(dir=workSpaceTmp, env=env),
                                      envir=env)
                           },
                               justify="left")

    button.widget2 <- tkbutton(tt, text="Table de données d'observations",
                               command=function()
                           {
                               assign("obsTmp",
                                      openObservations.f(dir=workSpaceTmp, env=env),
                                      envir=env)
                           },
                               justify="left")

    button.widget3 <- tkbutton(tt, text="Référentiel espèces",
                               command=function()
                           {
                               assign("refespTmp",
                                      openListespeces.f(dir=workSpaceTmp, env=env),
                                      envir=env)
                           },
                               justify="left")

    FrameBT <- tkframe(tt)

    OK.but <- tkbutton(FrameBT, text=" Valider ",
                       command=function(){tclvalue(Done) <- "1"})

    B.Cancel <- tkbutton(FrameBT, text="  Annuler  ",
                         command=function(){tclvalue(Done) <- "2"})

    tkgrid(L.Info,
           columnspan=2,
           pady=3, padx=5, sticky="ew")

    tkgrid(button.widget0,
           ResumerEspaceTravail <- tklabel(tt, text=paste("non sélectionné - par défaut :",
                                                          ifelse(!is.na(workSpaceTmp),
                                                                 workSpaceTmp, "RIEN !!!"))),
           pady=3, padx=5, sticky="w")

    tkgrid(button.widget1,
           ResumerSituationFichierUnitesObs <- tklabel(tt, text=paste("non sélectionné - par défaut :",
                                                                      ifelse(!is.na(unitobsTmp),
                                                                             unitobsTmp, "RIEN !!!"))),
           pady=3, padx=5, sticky="w")

    tkgrid(button.widget2,
           ResumerSituationFichierObs <- tklabel(tt, text=paste("non sélectionné - par défaut :",
                                                                ifelse(!is.na(obsTmp),
                                                                       obsTmp, "RIEN !!!"))),
           pady=3, padx=5, sticky="w")

    tkgrid(button.widget3,
           ResumerSituationReferencielEspece <- tklabel(tt, text=paste("non sélectionné - par défaut :",
                                                                       ifelse(!is.na(refespTmp),
                                                                              refespTmp, "RIEN !!!"))),
           pady=3, padx=5, sticky="w")

    tkgrid(OK.but, tklabel(FrameBT, text="            "), B.Cancel, pady=5, padx=5)

    tkgrid(FrameBT, pady=5, padx=5, columnspan=2)

    ## Informations sur l'espace de travail en gras au passage sur le bouton de choix de celui-ci :
    tkbind(button.widget0,
           "<Enter>", function(){tkconfigure(L.Info,
                                             font=tkfont.create(family="arial", weight="bold", size=9))})

    tkbind(ResumerEspaceTravail,
           "<Enter>", function(){tkconfigure(L.Info,
                                             font=tkfont.create(family="arial", weight="bold", size=9))})

    tkbind(button.widget0,
           "<Leave>", function(){tkconfigure(L.Info,
                                             font=tkfont.create(family="arial", size=9))})

    tkbind(ResumerEspaceTravail,
           "<Leave>", function(){tkconfigure(L.Info,
                                             font=tkfont.create(family="arial", size=9))})

    tkbind(tt, "<Destroy>", function(){tclvalue(Done) <- "2"})

    tkgrid.configure(button.widget0, button.widget1, button.widget2, button.widget3, sticky="ew")


    tkfocus(tt)
    tcl("update")
    winSmartPlace.f(tt)

    tkwait.variable(Done)

    if (tclvalue(Done) == "1")
    {
        tkdestroy(tt)
        ## Changement des variables globales
        pathMaker.f(nameWorkspace=workSpaceTmp,
                    fileNameUnitobs=unitobsTmp,
                    fileNameObs=obsTmp,
                    fileNameRefesp=refespTmp)

        opendefault.f()

    }else{
        tkdestroy(tt)
    }


}
