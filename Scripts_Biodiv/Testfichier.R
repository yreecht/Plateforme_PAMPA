
## essais de rendre générique après avec unitobs et obs
testfileref.f <- function (dataEnv, baseEnv)
{
    ## Récupération des données :
    filePathes <- get("filePathes", envir=dataEnv)
    fileNames <- get("fileNames", envir=dataEnv)
    refesp <- get("refesp", envir=dataEnv)
    obs <- get("obs", envir=dataEnv)

    runLog.f(msg=c("Informations sur le référentiel espèces :"))

    tclRequire("Tktable")
    ## Déclaration des objets fenetre, tableau
    W.test <- tktoplevel(width = 100)
    tclarrayRefEsp <- tclArray()

    ## Fonctions activées par les boutons de la fenêtre
    FermerWinTest <- function ()
    {
        tkdestroy(W.test)
        winRaise.f(tm)
    }

    EnregistrerWinTest <- function ()
    {
        FichierCSV <- paste(filePathes["results"], "Infos_", fileNames["refesp"], ".csv", sep="")
        write.csv2(dataframeRefEsp, file=FichierCSV, row.names = FALSE)

        add.logFrame.f(msgID="InfoRefSpeEnregistre", env = baseEnv, file=FichierCSV)
    }

    ## Déclaration des objets bouton
    Fermer.but <- tkbutton(W.test, text="Fermer", command=FermerWinTest)
    Enregistrer.but <- tkbutton(W.test, text="Enregistrer en CSV", command=EnregistrerWinTest)

    ## Sélection des valeurs de la table espèces correspondant au jeux de données
    sites <- getOption("P.MPA")
    espSite <- paste("Obs", sites, sep="")

    especes.select <- dropLevels.f(subset(refesp,
                                          is.element(code_espece, levels(obs$code_espece))))

    ## Externaliser la définition des sites par la suite...
    listeSite <- c("RUN" , "MAY" , "BA" , "BO" , "CB" , "CR" , "STM" , "NC")

    ## Noms des sites dont on doit exclure les colonnes :
    sitesExclus <- listeSite[ ! grepl(pattern=paste("^(",
                                                    paste(sites, collapse="|"),
                                                    ")$", sep=""), x=listeSite)]

    ## champs ne correspondant pas au motif "(Site1|Site2|...)$" :
    champsSite <- colnames(especes.select)[! grepl(paste("(", paste(sitesExclus, collapse="|"), ")$", sep=""),
                                                   colnames(especes.select))]

    especes.select <- especes.select[ , champsSite]

    ## construction de l'objet dataframe
    dataframeRefEsp <- as.data.frame(names(refesp))
    colnames(dataframeRefEsp)[1]="Nom_Champ"
    dataframeRefEsp[, 2]=""
    dataframeRefEsp[, 3]=""
    colnames(dataframeRefEsp)[2]="Nb_Valeurs"
    colnames(dataframeRefEsp)[3]="%_renseignement"

    ## construction de l'objet tableau
    tclarrayRefEsp[[0, 0]] <- "Champ #"
    tclarrayRefEsp[[0, 1]] <- "Nom"
    tclarrayRefEsp[[0, 2]] <- "Nb de valeurs"
    tclarrayRefEsp[[0, 3]] <- "% renseigné"

    for (nbChamp in (1:dim(especes.select)[2]))
    {
        ## Remplissage du tableau
        tclarrayRefEsp[[nbChamp, 0]] <- nbChamp
        tclarrayRefEsp[[nbChamp, 1]] <- names(especes.select)[nbChamp]
        tclarrayRefEsp[[nbChamp, 2]] <- sum(!is.na(especes.select[, nbChamp]))

        tclarrayRefEsp[[nbChamp, 3]] <-
            paste(round(sum(!is.na(especes.select[ , nbChamp])) /
                        nrow(especes.select) * 100, digits=2), "%")

        ## Remplissage du dataframe pour l'enregistrement
        dataframeRefEsp[nbChamp, 2] <- sum(!is.na(especes.select[, nbChamp]))
        dataframeRefEsp[nbChamp, 3] <-
            paste(round(sum(!is.na(especes.select[ , nbChamp])) /
                        nrow(especes.select) * 100, digits=2), "%")
    }

    ## construction de la fenêtre
    tkwm.title(W.test, paste("Informations sur ", fileNames["refesp"]))
    frameOverwintest <- tkframe(W.test)
    imgAsLabelwintest <- tklabel(frameOverwintest, image=imageAMP, bg="white")


    tkgrid(frameOverwintest, sticky="ew", columnspan=2)

    tkgrid(imgAsLabelwintest,
           tklabel(frameOverwintest,
                   text=paste("Taux de renseignement des champs de ", fileNames["refesp"],
                              "\npour le jeu de données (tient compte des sélections)\n", fileNames["obs"]),
                   relief="groove", borderwidth=2,
                   bg="yellow", justify="left"),
           padx=5, sticky="e")

    tkgrid.configure(imgAsLabelwintest, sticky="w")

    ## tkgrid.configure(frameOverwintest, columnspan=1, column=1)
    tkgrid(tklabel(W.test, text=paste("Nombre de champs de ", fileNames["refesp"], " : ", dim(especes.select)[2])),
           Enregistrer.but)

    tkgrid(tklabel(W.test,
                   text=paste("Nombre d'espèces référencées pour ",
                              paste(sites, collapse=", "), " : ",
                   nrow(subset(refesp,
                               apply(refesp[, espSite, drop=FALSE], 1,
                                     function(x) any(x == "oui")))))))

    tkgrid(tklabel(W.test,
                   text=paste("Nombre d'espèces du jeux de données ", fileNames["obs"], " : ",
                   length(unique(obs$code_espece)))), Fermer.but)

    tkgrid(tklabel(W.test,
                   text=paste(## "\nInformations sur les ", length(unique(obs$code_espece)),
                              ## "espèces \nDU JEU DE DONNEES ",
                              "\n\nVous pouvez copier-coller ce tableau dans Excel", sep="")))

    tableTestRefEsp <- tkwidget(W.test, "table",
                                variable=tclarrayRefEsp, rows=ncol(especes.select) + 1, cols=4,
                                colwidth=27, titlerows=1, titlecols=1, selectmode="extended", background="white",
                                xscrollcommand=function(...) {tkset(xscr, ...)},
                                yscrollcommand=function(...) {tkset(yscrtb, ...)})

    xscr <-tkscrollbar(W.test, orient="horizontal", command=function(...)tkxview(tableTestRefEsp, ...))
    yscrtb <- tkscrollbar(W.test, command=function(...)tkyview(tableTestRefEsp, ...))
    tkgrid(tableTestRefEsp, yscrtb, columnspan=3)
    tkgrid.configure(yscrtb, sticky="nsw")
    tkgrid(xscr, sticky="new", columnspan=3)

    tkconfigure(tableTestRefEsp, variable=tclarrayRefEsp, background="white", selectmode="extended",
                rowseparator="\"\n\"", colseparator="\"\t\"")

    tkgrid.configure(tableTestRefEsp, columnspan=2, sticky="w")
    ## barplot(dataframeRefEsp)
    tkfocus(W.test)
    winSmartPlace.f(W.test)
}
