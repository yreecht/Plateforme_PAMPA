
ModifierMenuApresImport.f <- function()
{
    runLog.f(msg=c("Modification des menus suite au chargement des données :"))

    ## Réactivation des menus qui nécessitent le chargement préalable :
    tkentryconfigure(topMenu, 1, state="normal")
    tkentryconfigure(topMenu, 2, state="normal")
    tkentryconfigure(topMenu, 3, state="normal")
    tkentryconfigure(topMenu, 4, state="normal")
    tkentryconfigure(topMenu, 5, state="normal")

    ## Réactivation des entrées du menu "Données" qui nécessitent le chargement préalable :
    tkentryconfigure(import, 3, state="normal")
    tkentryconfigure(import, 6, state="normal")
    ## tkentryconfigure(import, 7, state="normal")

    NbEsp <- length(unique(obs$code_espece))
    tkconfigure(ResumerSituationEspecesSelectionnees, text=paste("-> Nombre d'espèces concernées : ", NbEsp))
    Nbunitobs <- length(unique(obs$unite_observation))
    tkconfigure(ResumerSituationUnitobsSelectionnees,
                text=paste("-> Nombre  d'unités d'observations concernées : ", Nbunitobs))

    tkconfigure(button.DataRestore, state="disabled")

    ## Suppression de la colonne "sélections" si besoin :
    if (nchar(tclvalue(tclarray[[0, 4]])) > 3)
    {
        tkdelete(table1, "cols", "end", 1)
    }

    winRaise.f(tm)
}

MiseajourTableau.f <- function(tclarray)
{
    ##  ############# Mise à jour des valeurs dans le tableau #############
    tclarray[[1, 1]] <- sub(paste(nameWorkspace, "/Data/", sep=""), '', fileName1)
    tclarray[[1, 2]] <- dim(unitobs)[1]
    tclarray[[1, 3]] <- dim(unitobs)[2]
    tclarray[[2, 1]] <- sub(paste(nameWorkspace, "/Data/", sep=""), '', fileName2)
    tclarray[[2, 2]] <- dim(obs)[1]
    tclarray[[2, 3]] <- dim(obs)[2]
    tclarray[[3, 1]] <- sub(paste(nameWorkspace, "/Data/", sep=""), '', fileName3)
    tclarray[[3, 2]] <- dim(especes)[1]
    tclarray[[3, 3]] <- dim(especes)[2]

    tmp <- sapply(1:3,
                  function(i)
              {
                  nchar(tclvalue(tclarray[[i, 1]]))
              })

    tcl(.Tk.ID(table1), "width", "1", max(tmp)+2)
}

ModifierInterfaceApresSelection.f <- function(Critere, Valeur)
{
    runLog.f(msg=c("Modification de l'interface suite à une sélection d'enregistrement :"))

    if (tryCatch(nchar(tclvalue(tclarray[[0, 4]])),
                 error=function(e){0}) < 3 && Jeuxdonnescoupe)
    {
        tkinsert(table1, "cols", "end", 1)
        tclarray[[0, 4]] <- "Sélection"
    }

    tclarray[[2, 4]] <- dim(obs)[1]
    tkconfigure(MonCritere, text=Critere)
    tkconfigure(MesEnregistrements, text=Valeur)
    NbEsp <- length(unique(obs$code_espece))
    tkconfigure(ResumerSituationEspecesSelectionnees, text = paste("-> Nombre d'espèces concernées : ",
                                                      NbEsp))
    Nbunitobs <- length(unique(obs$unite_observation))
    tkconfigure(ResumerSituationUnitobsSelectionnees,
                text=paste("-> Nombre d'unités d'observations dans le fichier d'observations : ", Nbunitobs))
    if (Jeuxdonnescoupe==1)
    {
        tkconfigure(button.DataRestore, state="normal")
    }
    ## tkconfigure(frameLower, text=paste(length(obs$code_espece[obs$code_espece==fa]), " enregistrements
    ## concernés", sep=""))

    eval(winRaise.f(tm), envir=.GlobalEnv)
}

ModifierInterfaceApresRestore.f <- function(Critere="Aucun", Valeur="NA")
{
    runLog.f(msg=c("Modification de l'interface après restauration des données originales :"))

    tkdelete(table1, "cols", "end", 1)

    tclarray[[0, 4]] <- ""
    tclarray[[2, 4]] <- dim(obs)[1]

    tkconfigure(MonCritere, text=Critere)
    tkconfigure(MesEnregistrements, text=Valeur)
    NbEsp <- length(unique(obs$code_espece))
    tkconfigure(ResumerSituationEspecesSelectionnees, text=paste("-> Nombre d'espèces concernées : ", NbEsp))
    Nbunitobs <- length(unique(obs$unite_observation))
    tkconfigure(ResumerSituationUnitobsSelectionnees,
                text=paste("-> Nombre d'unités d'observations dans le fichier d'observations : ", Nbunitobs))
    tkconfigure(button.DataRestore, state="disabled")

    winRaise.f(tm)
}

