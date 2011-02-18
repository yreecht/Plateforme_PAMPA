openfile.f <- function()
{
    runLog.f(msg=c("Choix manuel des fichiers de données :"))

    ## Choix de l'espace de travail
    chercheEspaceTravail.f <- function() # [imb]
    {
        nameWorkspace <- tclvalue(tkchooseDirectory(initialdir=nameWorkspace))

        if (!nchar(nameWorkspace))
        {
            ## tkmessageBox(message="Aucun espace de travail n'a ete selectionné!")
        }else{
            assign("nameWorkspace", nameWorkspace, envir=.GlobalEnv)
            ## setwd(nameWorkspace)
            tkconfigure(ResumerEspaceTravail, text=paste("Espace de travail : ", nameWorkspace))
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
            ## tkmessageBox(message="Aucun fichier n'a ete selectionne!")
        }else{
            message(nameUnitobs)

            tkconfigure(ResumerSituationFichierUnitesObs, text=paste("Fichier d'unités d'observations : ", nameUnitobs))
            tkinsert(helpframe, "end", "\n Choisissez maintenant votre fichier d'observations")
            ## nameUnitobs
            ## assign("fileNameUnitObs", paste(nameWorkspace, "/Data/", nameUnitobs, sep=""), envir=.GlobalEnv)
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
            ## tkmessageBox(message="Aucun fichier n'a ete selectionne!")
        }else{
            message(namefileObs)
            ## assign("fileNameObs", paste(nameWorkspace, "/Data/", namefileObs, sep=""), envir=.GlobalEnv)
            assign("fileName2", namefileObs, envir=.GlobalEnv)
            ## ici du coup, on peut y mettre un choix ou reconnaitre le référenciel automatiquement
            tkconfigure(ResumerSituationFichierObs, text=paste("Fichier d'observations : ", namefileObs))
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
            ## tkmessageBox(message="Aucun fichier n'a ete selectionne!")
        }else{
            message(namefileRef)
            tkconfigure(ResumerSituationReferencielEspece, text=paste("Fichier référenciel espèce : ", namefileRef))
            assign("fileName3", namefileRef, envir=.GlobalEnv)
        }
    }

    tt <- tktoplevel(height=50, width=300)
    tkwm.title(tt, "Import des fichiers de donnees")
    OK <- tclVar(0)
    button.widget0 <- tkbutton(tt, text="Espace de travail", width=45,
                               command=chercheEspaceTravail.f)
    button.widget1 <- tkbutton(tt, text="Table de donnees unites d'observation", command=openUnitobs.f)
    button.widget2 <- tkbutton(tt, text="Table de donnees d'observations", command=openObservations.f)
    button.widget3 <- tkbutton(tt, text="Referentiel especes", command=openListespeces.f)
    OnOK <- function()  # [imb]
    {
        tkdestroy(tt)
    }
    OK.but <-tkbutton(tt, text="Valider", command=OnOK)
    tkgrid(button.widget0,
           ResumerEspaceTravail <- tklabel(tt, text=paste("Espace de travail : ", "non sélectionné - par défaut :",
                                               nameWorkspace)))

    tkgrid(button.widget1,
           ResumerSituationFichierUnitesObs <- tklabel(tt, text=paste("Fichier d'unités d'observations : ",
                                                                      "non sélectionné - par défaut :", fileName1)))

    tkgrid(button.widget2,
           ResumerSituationFichierObs <- tklabel(tt, text=paste("Fichier d'observations : ",
                                                                "non sélectionné - par défaut :", fileName2)))

    tkgrid(button.widget3,
           ResumerSituationReferencielEspece <- tklabel(tt, text=paste("Référentiel espèce : ",
                                                                       "non sélectionné - par défaut :", fileName3)))

    tkgrid(OK.but)
    tkgrid.configure(button.widget0, button.widget1, button.widget2, button.widget3, sticky="w")
    tkgrid.configure(OK.but, sticky="we")

    tkfocus(tt)
    winSmartPlace.f(tt)
    tkwait.window(tt)
    ## Changement des variables globales
    pathMaker.f()

    opendefault.f()
}
