
ModifierMenuApresImport.f <- function()
{
    runLog.f(msg=c("Modification des menus suite au chargement des données :"))

    ## Réactivation des menus qui nécessitent le chargement préalable :
    tkconfigure(MB.selection, state="normal")
    tkconfigure(MB.traitement, state="normal")
    tkconfigure(MB.analyse, state="normal")

    ## Réactivation des entrées du menu "Données" qui nécessitent le chargement préalable :
    tkentryconfigure(import, 3, state="normal")
    tkentryconfigure(import, 4, state="normal")
    tkentryconfigure(import, 5, state="normal")
    tkentryconfigure(import, 6, state="normal")

    ## Désactivation du bouton et du menu de restauration des données originales :
    tkconfigure(B.DataRestore, state="disabled")
    tkentryconfigure(selection, 5, state="disabled")

    if (siteEtudie == "NC")
    {
        if (! grepl("^Carte", tclvalue(tkentrycget(traitement, "8", "-label"))))
        {
            tkadd(traitement, "command", label="Carte de métrique /unité d'observation (démonstration)...",
                  background="#FFFBCF",
                  command=function ()
              {
                  selectionVariablesCarte.f()
                  winRaise.f(tm)
              })
        }else{
            tkentryconfigure(traitement, 8, state="normal")
        }
    }else{
        if (grepl("^Carte", tclvalue(tkentrycget(traitement, "8", "-label"))))
        {
            tkdelete(traitement, "8", "8")
        }else{}
    }

    ## Suppression de la colonne "sélections" si besoin :
    if (tryCatch(nchar(tclvalue(tclarray[[0, 4]])),
                 error=function(e){0}) > 3)
    {
        tkdelete(table1, "cols", "end", 1)
    }

    tkconfigure(MonCritere, text="Tout")

    ## Titre du cadre de sélection en non-gras :
    tkconfigure(L.criteres, font=tkfont.create(size=8))

    winRaise.f(tm)
}

########################################################################################################################
ColAutoWidth.f <- function(TK.table)
{
    ## Purpose: Largeur automatique des colonnes du tableau
    ## ----------------------------------------------------------------------
    ## Arguments: TK.table : un widget tableau tcl.
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 18 avr. 2011, 16:40

    ## Dimensions du tableau (à partir de 0 => +1) :

    dim.array <- as.numeric(unlist(strsplit(tclvalue(tcl(.Tk.ID(TK.table),
                                                         "index", "bottomright")),
                                            ",")))

    ## Redimensionnement des colonnes :
    invisible(sapply(0:dim.array[2],
                     function(j)
                 {
                     tmp <- sapply(0:dim.array[1],
                                   function(i, j)
                               {
                                   max(sapply(strsplit(tclvalue(tcl(TK.table,
                                                                    "get",
                                                                    paste(i, ",", j, sep=""))),
                                                       split="\n|\r", perl=TRUE),
                                              nchar))
                               }, j)

                     tcl(.Tk.ID(TK.table), "width", j, max(c(tmp, 3)) + 3)
                 }))
}

########################################################################################################################
RowAutoEight.f <- function(TK.table)
{
    ## Purpose: Hauteur automatique des lignes du tableau
    ## ----------------------------------------------------------------------
    ## Arguments: TK.table : un widget tableau tcl.
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 18 avr. 2011, 16:40

    ## Dimensions du tableau (à partir de 0 => +1) :

    dim.array <- as.numeric(unlist(strsplit(tclvalue(tcl(.Tk.ID(TK.table),
                                                         "index", "bottomright")),
                                            ",")))

    ## Redimensionnement des colonnes :
    invisible(sapply(0:dim.array[1],
                     function(i)
                 {
                     tmp <- sapply(0:dim.array[2],
                                   function(j, i)
                               {
                                   sum(unlist(strsplit(tclvalue(tcl(TK.table,
                                                                    "get",
                                                                    paste(i, ",", j, sep=""))),
                                                       split="\n|\r", perl=TRUE)) != "")
                               }, i=i)

                     tcl(.Tk.ID(TK.table), "height", i, max(c(tmp, 1)))
                 }))
}

########################################################################################################################
MiseajourTableau.f <- function(tclarray)
{
    ##  ############# Mise à jour des valeurs dans le tableau #############
    tclarray[["1,1"]] <- sub(paste(nameWorkspace, "/Data/", sep=""), '', fileName1)
    tclarray[["1,2"]] <- ncol(unitobs)
    tclarray[["1,3"]] <- nrow(unitobs)
    tclarray[["2,1"]] <- sub(paste(nameWorkspace, "/Data/", sep=""), '', fileName2)
    tclarray[["2,2"]] <- ncol(obs)
    tclarray[["2,3"]] <- nrow(obs)
    tclarray[["3,1"]] <- sub(paste(nameWorkspace, "/Data/", sep=""), '', fileName3)
    tclarray[["3,2"]] <- ncol(especes)
    tclarray[["3,3"]] <- nrow(especes)

    ColAutoWidth.f(table1)
}

ModifierInterfaceApresSelection.f <- function(Critere, Valeur)
{
    runLog.f(msg=c("Modification de l'interface suite à une sélection d'enregistrement :"))

    ## Ajout d'une colonne en fin de table avec les informations de sélection :
    if (tryCatch(nchar(tclvalue(tclarray[[0, 4]])),
                 error=function(e){0}) < 3 && Jeuxdonnescoupe)
    {
        tkinsert(table1, "cols", "end", 1)
        tclarray[[0, 4]] <- "Sélection"
    }

    tclarray[[1, 4]] <- nlevels(obs$unite_observation) # Nombre d'unitobs conservées (! peut différer du nombre dans le
                                        # fichier d'observation).
    tclarray[[2, 4]] <- nrow(obs)       # Nombre d'observations
    tclarray[[3, 4]] <- nlevels(obs$code_espece) # Nombre d'espèces conservées (! peut différer du nombre dans le
                                        # fichier d'observation).

    ## Information sur les critères :
    if((tmp <- tclvalue(tkcget(MonCritere, "-text"))) == "Tout") # ou bien : tcl(.Tk.ID(MonCritere), "cget", "-text")
    {                                   # Si pas de sélection précédente, on affiche seulement le nouveau critère...
        tkconfigure(MonCritere, text=Critere)
    }else{                              # ...sinon on l'ajoute aux existants.
        tkconfigure(MonCritere, text=paste(tmp, Critere, sep="\n\n"))
    }

    ## Titre du cadre de sélection en gras :
    if (Critere != "Tout")
    {
        tkconfigure(L.criteres, font=tkfont.create(weight="bold", size=8))
    }else{
        tkconfigure(L.criteres, font=tkfont.create(size=8))
    }

    ## Nombre d'espèces réellement restantes dans les observations :
    tkconfigure(ResumerSituationEspecesSelectionnees,
                text = paste("-> Nombre de codes espèce",
                             ifelse(Jeuxdonnescoupe, " restants", ""),
                             " dans le fichier d'observation",
                             ifelse(Jeuxdonnescoupe,
                                    " (peut différer du nombre retenu !) : ", " : "),
                             length(unique(obs$code_espece)), sep=""),
                state="normal")

    ## Nombre d'espèces réellement restantes dans les observations :
    tkconfigure(ResumerSituationUnitobsSelectionnees,
                text = paste("-> Nombre d'unités d'observations",
                             ifelse(Jeuxdonnescoupe, " restantes", ""),
                             " dans le fichier d'observations",
                             ifelse(Jeuxdonnescoupe,
                                    " (peut différer du nombre retenu !) : ", " : "),
                             length(unique(obs$unite_observation)), sep=""),
                state="normal")

    if (Jeuxdonnescoupe==1) # Ré-activation du bouton et du menu de restauration des données originales.
    {
        tkconfigure(B.DataRestore, state="normal")
        tkentryconfigure(selection, 5, state="normal")
    }

    eval(winRaise.f(tm), envir=.GlobalEnv)
}

ModifierInterfaceApresRestore.f <- function(Critere="Aucun", Valeur="NA")
{
    runLog.f(msg=c("Modification de l'interface après restauration des données originales :"))

    tkdelete(table1, "cols", "end", 1)

    tclarray[[0, 4]] <- ""
    tclarray[[2, 4]] <- dim(obs)[1]

    tkconfigure(MonCritere, text=Critere)

    ## Titre du cadre de sélection en non-gras :
    tkconfigure(L.criteres, font=tkfont.create(size=8))

    ## Nombre d'espèces réellement restantes dans les observations :
    tkconfigure(ResumerSituationEspecesSelectionnees,
                text = paste("-> Nombre d'espèces",
                             " dans le fichier d'observation : ",
                             length(unique(obs$code_espece)), sep=""),
                state="normal")

    ## Nombre d'espèces réellement restantes dans les observations :
    tkconfigure(ResumerSituationUnitobsSelectionnees,
                text = paste("-> Nombre d'unités d'observations",
                             " dans le fichier d'observations : ",
                             length(unique(obs$unite_observation)), sep=""),
                state="normal")

    ## Désactivation du bouton et du menu de restauration des données originales :
    tkconfigure(B.DataRestore, state="disabled")
    tkentryconfigure(selection, 5, state="disabled")

    winRaise.f(tm)
}

