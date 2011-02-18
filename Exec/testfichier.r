
## essais de rendre générique après avec unitobs et obs
testfileref.f <- function ()
{
    runLog.f(msg=c("Informations sur le référentiel espèces :"))

    tclRequire("Tktable")
    ## Déclaration des objets fenetre, tableau
    wintest <- tktoplevel(width = 100)
    tclarrayRefEsp <- tclArray()

    ## Fonctions activées par les boutons de la fenêtre
    FermerWinTest <- function ()
    {
        tkdestroy(wintest)
    }

    EnregistrerWinTest <- function ()
    {
        FichierCSV <- paste(NomDossierTravail, "Infos_", fileName3, ".csv", sep="")
        write.csv(dataframeRefEsp, file=FichierCSV, row.names = FALSE)
        gestionMSGinfo.f("InfoRefSpeEnregistre", FichierCSV)
        tkmessageBox(message=paste("Votre fichier d'information sur le référentiel espèce",
                                   " a été enregistré au format CSV dans le dossier de travail", sep=""))
    }

    ## Déclaration des objets bouton
    Fermer.but <- tkbutton(wintest, text="Fermer", command=FermerWinTest)
    Enregistrer.but <- tkbutton(wintest, text="Enregistrer en CSV", command=EnregistrerWinTest)

    ## Sélection des valeurs de la table espèces correspondant au jeux de données
    matable <- "obs"
    objtable <- eval(parse(text=matable))
    ChampPresence <- paste("Obs", SiteEtudie, sep="")
    especesPresentes <- subset(especes, especes[, ChampPresence]=="oui")

    ## construction de l'objet dataframe
    dataframeRefEsp <- as.data.frame(names(especes))
    colnames(dataframeRefEsp)[1]="Nom_Champ"
    dataframeRefEsp[, 2]=""
    dataframeRefEsp[, 3]=""
    colnames(dataframeRefEsp)[2]="Nb_Valeurs"
    colnames(dataframeRefEsp)[3]="%_renseignement"

    ## construction de l'objet tableau
    tclarrayRefEsp[[0, 0]] <- "nb"
    tclarrayRefEsp[[0, 1]] <- "Nom"
    tclarrayRefEsp[[0, 2]] <- "nb valeurs"
    tclarrayRefEsp[[0, 3]] <- "Tx renseignement"

    for (nbChamp in (1:dim(especesPresentes)[2]))
    {
        ## Remplissage du tableau
        tclarrayRefEsp[[nbChamp, 0]] <- nbChamp
        tclarrayRefEsp[[nbChamp, 1]] <- names(especes[nbChamp])
        tclarrayRefEsp[[nbChamp, 2]] <- length(unique(especes[, nbChamp][match(obs$code_espece, especes$code_espece)],
                                                      na.rm=TRUE))

        tclarrayRefEsp[[nbChamp, 3]] <-
            paste(round(length(unique(especesPresentes$code_espece[!is.na(especesPresentes[, nbChamp])])) /
                        length(unique(especesPresentes$code_espece))*100, digits=2), "%")

        ## Remplissage du dataframe pour l'enregistrement
        dataframeRefEsp[nbChamp, 2] <- length(unique(especes[, nbChamp][match(obs$code_espece, especes$code_espece)],
                                                     na.rm=TRUE))
        dataframeRefEsp[nbChamp, 3] <-
            round(length(unique(especesPresentes$code_espece[!is.na(especesPresentes[, nbChamp])])) /
                  length(unique(especesPresentes$code_espece))*100, digits=2)
    }

    ## message(head(dataframeRefEsp))
    ## message(dim(tclarrayRefEsp[2]))
    ## message(head(dataframeRefEsp))

    ## construction de la fenêtre
    tkwm.title(wintest, paste("Informations sur ", fileName3))
    frameOverwintest <- tkframe(wintest)
    imgAsLabelwintest <- tklabel(wintest, image=imageAMP, bg="white")
    tkgrid(imgAsLabelwintest, frameOverwintest, sticky="w")
    tkgrid(tklabel(frameOverwintest, text=paste("Taux de renseignement des champs de ", fileName3,
                                     "\npour le jeu de données\n", fileName2), relief="groove", borderwidth=2,
                   bg="yellow"))

    ## tkgrid.configure(frameOverwintest, columnspan=1, column=1)
    tkgrid(tklabel(wintest, text=paste("Nombre de champs de ", fileName3, " : ", dim(especesPresentes)[2])),
           Enregistrer.but)

    tkgrid(tklabel(wintest,
                   text=paste("Nombre d'espèces référencées pour ", SiteEtudie, " : ", dim(especesPresentes)[1])))

    tkgrid(tklabel(wintest,
                   text=paste("Nombre d'espèces du jeux de données ", fileName2, " : ",
                   length(unique(obs$code_espece)))), Fermer.but)

    tkgrid(tklabel(wintest,
                   text=paste("\nInformations sur les ", length(unique(obs$code_espece)),
                   "espèces \nDU JEU DE DONNEES \n\nVous pouvez copier-coller ce tableau dans Excel")))

    tableTestRefEsp <- tkwidget(wintest, "table", variable=tclarrayRefEsp, rows=dim(especesPresentes)[2]+1, cols=4,
                                colwidth=27, titlerows=1, titlecols=1, selectmode="extended", background="white",
                                xscrollcommand=function(...) {tkset(xscr, ...)}, yscrollcommand=function(...)
                                tkset(yscrtb, ...))

    xscr <-tkscrollbar(wintest, orient="horizontal", command=function(...)tkxview(tableTestRefEsp, ...))
    yscrtb <- tkscrollbar(wintest, command=function(...)tkyview(tableTestRefEsp, ...))
    tkgrid(tableTestRefEsp, yscrtb, columnspan=3)
    tkgrid.configure(yscrtb, sticky="nse")
    tkgrid(xscr, sticky="new")

    tkconfigure(tableTestRefEsp, variable=tclarrayRefEsp, background="white", selectmode="extended",
                rowseparator="\"\n\"", colseparator="\"\t\"")

    tkgrid.configure(tableTestRefEsp, columnspan=2, sticky="w")
    ## barplot(dataframeRefEsp)
    tkfocus(wintest)
}
