
ModifierMenuApresImport.f <- function()
{
    runLog.f(msg=c("Modification des menus suite au chargement des donn�es :"))

    ## R�activation des menus qui n�cessitent le chargement pr�alable :
    tkentryconfigure(topMenu, 1, state="normal")
    tkentryconfigure(topMenu, 2, state="normal")
    tkentryconfigure(topMenu, 3, state="normal")
    tkentryconfigure(topMenu, 4, state="normal")
    tkentryconfigure(topMenu, 5, state="normal")

    ## R�activation des entr�es du menu "Donn�es" qui n�cessitent le chargement pr�alable :
    tkentryconfigure(import, 3, state="normal")
    tkentryconfigure(import, 6, state="normal")
    ## tkentryconfigure(import, 7, state="normal")

    tkconfigure(button.DataRestore, state="disabled")

    ## Suppression de la colonne "s�lections" si besoin :
    if (nchar(tclvalue(tclarray[[0, 4]])) > 3)
    {
        tkdelete(table1, "cols", "end", 1)
    }

    tkconfigure(MonCritere, text="Tout")

    winRaise.f(tm)
}

MiseajourTableau.f <- function(tclarray)
{
    ##  ############# Mise � jour des valeurs dans le tableau #############
    tclarray[["1,1"]] <- sub(paste(nameWorkspace, "/Data/", sep=""), '', fileName1)
    tclarray[["1,2"]] <- dim(unitobs)[1]
    tclarray[["1,3"]] <- dim(unitobs)[2]
    tclarray[["2,1"]] <- sub(paste(nameWorkspace, "/Data/", sep=""), '', fileName2)
    tclarray[["2,2"]] <- dim(obs)[1]
    tclarray[["2,3"]] <- dim(obs)[2]
    tclarray[["3,1"]] <- sub(paste(nameWorkspace, "/Data/", sep=""), '', fileName3)
    tclarray[["3,2"]] <- dim(especes)[1]
    tclarray[["3,3"]] <- dim(especes)[2]

    tmp <- sapply(1:3,
                  function(i)
              {
                  nchar(tclvalue(tclarray[[i, 1]]))
              })

    tcl(.Tk.ID(table1), "width", "1", max(tmp)+2)
}

ModifierInterfaceApresSelection.f <- function(Critere, Valeur)
{
    runLog.f(msg=c("Modification de l'interface suite � une s�lection d'enregistrement :"))

    ## Ajout d'une colonne en fin de table avec les informations de s�lection :
    if (tryCatch(nchar(tclvalue(tclarray[[0, 4]])),
                 error=function(e){0}) < 3 && Jeuxdonnescoupe)
    {
        tkinsert(table1, "cols", "end", 1)
        tclarray[[0, 4]] <- "S�lection"
    }

    tclarray[[1, 4]] <- nlevels(obs$unite_observation) # Nombre d'unitobs conserv�es (! peut diff�rer du nombre dans le
                                        # fichier d'observation).
    tclarray[[2, 4]] <- nrow(obs)       # Nombre d'observations
    tclarray[[3, 4]] <- nlevels(obs$code_espece) # Nombre d'esp�ces conserv�es (! peut diff�rer du nombre dans le
                                        # fichier d'observation).

    ## Information sur les crit�res :
    if((tmp <- tclvalue(tkcget(MonCritere, "-text"))) == "Tout") # ou bien : tcl(.Tk.ID(MonCritere), "cget", "-text")
    {                                   # Si pas de s�lection pr�c�dente, on affiche seulement le nouveau crit�re...
        tkconfigure(MonCritere, text=Critere)
    }else{                              # ...sinon on l'ajoute aux existants.
        tkconfigure(MonCritere, text=paste(tmp, Critere, sep="\n\n"))
    }

    ## Nombre d'esp�ces r�ellement restantes dans les observations :
    tkconfigure(ResumerSituationEspecesSelectionnees,
                text = paste("-> Nombre d'esp�ces",
                             ifelse(Jeuxdonnescoupe, " restantes", ""),
                             " dans le fichier d'observation",
                             ifelse(Jeuxdonnescoupe,
                                    " (peut diff�rer du nombre retenu !) : ", " : "),
                             length(unique(obs$code_espece)), sep=""))

    ## Nombre d'esp�ces r�ellement restantes dans les observations :
    tkconfigure(ResumerSituationUnitobsSelectionnees,
                text = paste("-> Nombre d'unit�s d'observations",
                             ifelse(Jeuxdonnescoupe, " restantes", ""),
                             " dans le fichier d'observations",
                             ifelse(Jeuxdonnescoupe,
                                    " (peut diff�rer du nombre retenu !) : ", " : "),
                             length(unique(obs$unite_observation)), sep=""))

    if (Jeuxdonnescoupe==1)
    {
        tkconfigure(button.DataRestore, state="normal")
    }

    eval(winRaise.f(tm), envir=.GlobalEnv)
}

ModifierInterfaceApresRestore.f <- function(Critere="Aucun", Valeur="NA")
{
    runLog.f(msg=c("Modification de l'interface apr�s restauration des donn�es originales :"))

    tkdelete(table1, "cols", "end", 1)

    tclarray[[0, 4]] <- ""
    tclarray[[2, 4]] <- dim(obs)[1]

    tkconfigure(MonCritere, text=Critere)

    ## Nombre d'esp�ces r�ellement restantes dans les observations :
    tkconfigure(ResumerSituationEspecesSelectionnees,
                text = paste("-> Nombre d'esp�ces",
                             " dans le fichier d'observation : ",
                             length(unique(obs$code_espece)), sep=""))

    ## Nombre d'esp�ces r�ellement restantes dans les observations :
    tkconfigure(ResumerSituationUnitobsSelectionnees,
                text = paste("-> Nombre d'unit�s d'observations",
                             " dans le fichier d'observations : ",
                             length(unique(obs$unite_observation)), sep=""))

    ## D�sactivation du bouton de restauration des donn�es originales :
    tkconfigure(button.DataRestore, state="disabled")

    winRaise.f(tm)
}

