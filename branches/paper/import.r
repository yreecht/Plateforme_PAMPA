openfile.f <- function()
{
    runLog.f(msg=c("Choix manuel des fichiers de données :"))

    ## Choix de l'espace de travail
    chercheEspaceTravail.f <- function() # [imb]
    {
        nameWorkspace <- tclvalue(tkchooseDirectory(initialdir=nameWorkspace))

        if (!nchar(nameWorkspace))
        {
            ## Rien !
        }else{
            assign("nameWorkspace", nameWorkspace, envir=.GlobalEnv)
            ## setwd(nameWorkspace)
            tkconfigure(ResumerEspaceTravail,
                        text=nameWorkspace,
                        foreground="darkred")
            tkinsert(helpframe, "end", "\n Choisissez maintenant votre fichier d'unités d'observations")
        }
    }

    ## ####  Choix des fichiers de donnees source en .txt
    openUnitobs.f <- function() # [imb]
    {
        runLog.f(msg=c("Choix manuel du fichiers d'unités d'observations :"))

        nameUnitobs <- tclvalue(tkgetOpenFile(initialdir=paste(nameWorkspace, "/Data/", sep="")))

        ## On enlève le nom de chemin pour ne conserver que le nom du fichier:
        nameUnitobs <- sub(paste(nameWorkspace, "/Data/", sep=""), '', nameUnitobs)

        if (!nchar(nameUnitobs))
        {
            ## Rien !
        }else{
            tkconfigure(ResumerSituationFichierUnitesObs,
                        text=nameUnitobs,
                        foreground="darkred")

            tkinsert(helpframe, "end", "\n Choisissez maintenant votre fichier d'observations")

            assign("fileName1", nameUnitobs, envir=.GlobalEnv)
        }
    }

    openObservations.f <- function() # [imb]
    {
        runLog.f(msg=c("Choix manuel du fichiers d'observations :"))

        namefileObs <- tclvalue(tkgetOpenFile(initialdir=paste(nameWorkspace, "/Data/", sep="")))

        ## On enlève le nom de chemin pour ne conserver que le nom du fichier:
        namefileObs <- sub(paste(nameWorkspace, "/Data/", sep=""), '', namefileObs)

        if (!nchar(namefileObs))
        {
            ## Rien !
        }else{
            assign("fileName2", namefileObs, envir=.GlobalEnv)

            ## ici du coup, on peut y mettre un choix ou reconnaitre le référenciel automatiquement
            tkconfigure(ResumerSituationFichierObs,
                        text=namefileObs,
                        foreground="darkred")

            tkinsert(helpframe, "end", "\n Sélectionnez votre référenciel espèce")
        }
    }

    openListespeces.f <- function() # [imb]
    {
        runLog.f(msg=c("Choix manuel du fichiers du référentiel espèces :"))

        namefileRef <- tclvalue(tkgetOpenFile(initialdir=paste(nameWorkspace, "/Data/", sep="")))

        ## On enlève le nom de chemin pour ne conserver que le nom du fichier:
        namefileRef <- sub(paste(nameWorkspace, "/Data/", sep=""), '', namefileRef)

        if (!nchar(namefileRef))
        {
            ## Rien !
        }else{
            tkconfigure(ResumerSituationReferencielEspece,
                        text=namefileRef, foreground="darkred")

            assign("fileName3", namefileRef, envir=.GlobalEnv)
        }
    }

    tt <- tktoplevel(height=50, width=300)
    tkwm.title(tt, "Choix des fichiers de données à importer")

    OK <- tclVar(0)
    button.widget0 <- tkbutton(tt, text="Espace de travail", ## width=45,
                               command=chercheEspaceTravail.f,
                               justify="left")

    button.widget1 <- tkbutton(tt, text="Table de données d'unités d'observation",
                               command=openUnitobs.f,
                               justify="left")

    button.widget2 <- tkbutton(tt, text="Table de données d'observations",
                               command=openObservations.f,
                               justify="left")

    button.widget3 <- tkbutton(tt, text="Référentiel espèces",
                               command=openListespeces.f,
                               justify="left")

    OnOK <- function()  # [imb]
    {
        tkdestroy(tt)
    }

    OK.but <-tkbutton(tt, text="Valider", command=OnOK)

    tkgrid(button.widget0,
           ResumerEspaceTravail <- tklabel(tt, text=paste("non sélectionné - par défaut :",
                                               nameWorkspace)),
           pady=3, padx=5, sticky="w")

    tkgrid(button.widget1,
           ResumerSituationFichierUnitesObs <- tklabel(tt, text=paste("non sélectionné - par défaut :", fileName1)),
           pady=3, padx=5, sticky="w")

    tkgrid(button.widget2,
           ResumerSituationFichierObs <- tklabel(tt, text=paste("non sélectionné - par défaut :", fileName2)),
           pady=3, padx=5, sticky="w")

    tkgrid(button.widget3,
           ResumerSituationReferencielEspece <- tklabel(tt, text=paste("non sélectionné - par défaut :", fileName3)),
           pady=3, padx=5, sticky="w")

    tkgrid(OK.but, pady=5, padx=5)

    tkgrid.configure(button.widget0, button.widget1, button.widget2, button.widget3, sticky="ew")
    tkgrid.configure(OK.but, sticky="we", columnspan=2)

    tkfocus(tt)
    winSmartPlace.f(tt)
    tkwait.window(tt)

    ## Changement des variables globales
    pathMaker.f()

    opendefault.f()
}
