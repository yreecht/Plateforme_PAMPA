########################################################################################################################
Voirentableau <- function(Montclarray, title="", height=-1, width=-1, nrow=-1, ncol=-1, infos=title)
{
    tclRequire("Tktable")

    tb <- tktoplevel()
    tkwm.title(tb, title)

    tkbind(tb, "<Destroy>",
           function()
       {
           winRaise.f(tm)
       })

    ## Fonctions activées par les boutons de la fenêtre
    FermerWintb <- function()
    {
        tkdestroy(tb)
    }

    EnregistrerWintb <- function()
    {
        FichierCSV <- paste(NomDossierTravail, "Tableau_", title, ".csv", sep="")
        write.csv2(dataframetb, file=FichierCSV, row.names = FALSE)

        add.logFrame.f(msgID="InfoRefSpeEnregistre", env = .GlobalEnv, file=FichierCSV)
    }

    ## Déclaration des objets bouton
    Fermer.but <- tkbutton(tb, text="Fermer", command=FermerWintb)
    Enregistrer.but <- tkbutton(tb, text="Enregistrer en CSV", command=EnregistrerWintb)

    ## ICI CONTINUER LA MISE EN FORME
    dataframetb <- data.frame(1:nrow)

    for (nbChamps in (1:nrow))          # Vectoriser ??? [yr: 27/07/2010]
    {
        for (nbCol in (1:ncol))
        {
            dataframetb[nbChamps, nbCol] <- "a remplir" # tclvalue(Montclarray[[nbCol, nbChamps]])
            ## A FINIR!!!
        }
    }

    ## Éléments graphiques :
    frameOverwintb <- tkframe(tb)
    imgAsLabelwintb <- tklabel(frameOverwintb, image=imageAMP, bg="white")

    xscr <-tkscrollbar(tb, orient="horizontal", command=function(...)tkxview(tabletb,...))
    yscrtb <- tkscrollbar(tb, repeatinterval=3, command=function(...)tkyview(tabletb,...))

    tabletb <- tkwidget(tb, "table", rows=nrow, cols=ncol, titlerows=1, titlecols=0,
                        height=height+1, width=width+1, colwidth=23,
                        xscrollcommand=function(...) tkset(xscr,...),
                        yscrollcommand=function(...) tkset(yscrtb,...))

    ## ## Ne fonctionne pas [!!!] :
    ## tkbind(tabletb, "<MouseWheel>", function(...) tkyview(tabletb,...))
    ## tkbind(yscrtb, "<MouseWheel>", function(...) tkyview(tabletb,...))

    ## Placement des éléments :
    tkgrid(frameOverwintb, sticky="ew", columnspan=4)
    tkgrid(imgAsLabelwintb,
           tklabel(frameOverwintb, text=""),
           LB.titre <- tklabel(frameOverwintb, text=infos, relief="groove",
                               borderwidth=2, bg="yellow", justify="left"),
           padx=5, pady=5, sticky="nw")

    tkgrid.configure(LB.titre, sticky="new")

    tkgrid(tklabel(tb, text=paste("\n***", "\nVous pouvez copier-coller ce tableau dans Excel")))

    tkgrid(Enregistrer.but, Fermer.but, pady=5, padx=5)

    tkgrid(tabletb, yscrtb, columnspan=3, sticky="nsew")
    tkgrid.configure(yscrtb, sticky="nse")
    tkgrid(xscr, sticky="new", columnspan=2)

    tkconfigure(tabletb, variable=Montclarray, background="white", selectmode="extended", rowseparator="\"\n\"",
                colseparator="\"\t\"", titlerows=1,
                maxwidth=550)

    tkgrid.configure(tabletb, columnspan=2, sticky="w")

    tcl("update")
    ColAutoWidth.f(tabletb)
    RowAutoEight.f(tabletb)

    tcl("update")
    winSmartPlace.f(tb, xoffset=-30)

    return (tabletb)
}

########################################################################################################################
VoirPlanEchantillonnage.f <- function()
{
    runLog.f(msg=c("Affichage du plan d'échantillonnage :"))

    myRarrayPE <- read.csv2(paste(NomDossierTravail,
                                  "PlanEchantillonnage_basique",
                                  ifelse(Jeuxdonnescoupe, "_selection", ""),
                                  ".csv", sep=""),
                            row.names=1)

    tclarrayPE <- tclArray()
    ## tclarrayPE[[0, ]] <- c("Année", "Type", "Fréquence")

    tclarrayPE[[0, 0]] <- paste("\tStatut de protection ",
                                "\n  Année\t\t\t", sep="")

    ## Remplissage du tableau tcl :
    for (i in (1:nrow(myRarrayPE)))
    {
        tclarrayPE[[i, 0]] <- row.names(myRarrayPE)[i]

        for (j in (1:ncol(myRarrayPE)))
        {
            if (i == 1)
            {
                tclarrayPE[[0, j]] <- colnames(myRarrayPE)[j]
            }else{}

            tclarrayPE[[i, j]] <- as.character(myRarrayPE[i, j])
        }
    }

    pe <- tktoplevel()
    tkwm.title(pe, "Plan d'échantillonnage")
    tablePlanEch <- tkwidget(pe, "table", variable=tclarrayPE, rows=dim(myRarrayPE)[1]+1, cols=3,
                             titlerows=1, titlecol=1,
                             selectmode="extended", colwidth=30, background="white")


    tkpack(tablePlanEch)
    tcl("update")

    RowAutoEight.f(tablePlanEch)
    ColAutoWidth.f(tablePlanEch)

    tcl("update")
    winSmartPlace.f(pe)
}

########################################################################################################################
VoirInformationsDonneesEspeces.f <- function()
{

    Nbesp <- length(unique(unitesp$code_espece))
    message(paste(Nbesp, "espèces considérées dans ce jeux de données"))

    tclarrayID <- tclArray()
    tclarrayID[[0, 0]] <- "Espèce"               # Indexation étrange... [yr: 27/07/2010]
    tclarrayID[[0, 1]] <- "Nb indiv min/unitobs" # -> non, habituelle pour tclarrays !
    tclarrayID[[0, 2]] <- "Nb indiv max/unitobs" #
    tclarrayID[[0, 3]] <- "Fréquence d'occurrence"     #

    mini <- tapply(unitesp$nombre, unitesp$code_espece, min, na.rm=TRUE)
    maxi <- tapply(unitesp$nombre, unitesp$code_espece, max, na.rm=TRUE)

    nbunitobs <- nrow(unique(unitobs))
    pacha <- unitesp[, c("unite_observation", "code_espece", "nombre", "pres_abs"), drop=FALSE]

    for (i in (1:Nbesp))
    {
        tclarrayID[[i, 0]] <- unique(as.character(unitesp$code_espece))[i]
        tclarrayID[[i, 1]] <- mini[i]
        tclarrayID[[i, 2]] <- maxi[i]
        tclarrayID[[i, 3]] <-
            paste(round(length(pacha$unite_observation[pacha$pres_abs==1 &
                                                       pacha$code_espece==unique(unitesp$code_espece)[i]]) /
                        length(pacha$unite_observation[pacha$code_espece==unique(unitesp$code_espece)[i]]) * 100,
                        digits=2), "%")
    }

    ## Pour informations sur le nombre d'espèces :
    tmp <- unique(cbind(pacha[ , "code_espece"],
                        especes[match(pacha$code_espece, especes$code_espece),
                                c("Phylum", "espece")]))

    tableInfodonnees <- Voirentableau(tclarrayID,
                                      title="Informations par espèce",
                                      infos=paste("Informations par espèce :",
                                                  " \n\n\t* nombre de catégories observées = ", nrow(tmp),
                                                  " \n\t* nombre de catégories taxinomiques (biotiques) observées = ",
                                                  sum(!is.na(tmp$Phylum)),
                                                  " \n\t* nombre d'espèces observées = ",
                                                  sum(!is.na(tmp$espece) & tmp$espece != "sp."),
                                                  " \n\nCes informations tiennent compte des sélections en cours.", sep=""),
                                      height=Nbesp, width=4, nrow=Nbesp + 1, ncol=4)
}#fin VoirInformationsDonneesEspeces

########################################################################################################################
VoirInformationsDonneesUnitobs.f <- function()
{

    Nbunitobs <- nlevels(obs$unite_observation) ## length(unique(unitobs$unite_observation))
    message(paste(Nbunitobs, "unités d'observations considérées dans ce jeux de données"))
    tclarrayID <- tclArray()

    tclarrayID[[0, 0]] <- "Unité d'observation"    #
    tclarrayID[[0, 1]] <- "Site"                   #
    tclarrayID[[0, 2]] <- "Biotope"                #
    tclarrayID[[0, 3]] <- "Nb espèce"              #
    tclarrayID[[0, 4]] <- "Nb indiv max/espèce"    #

    pacha <- unitesp[ , c("unite_observation", "code_espece", "nombre", "pres_abs"), drop=FALSE]

    mini <- tapply(unitesp$nombre, unitesp$unite_observation, min, na.rm=TRUE) # [!!!]

    maxi <- tapply(unitesp$nombre, unitesp$unite_observation, max, na.rm=TRUE) # [!!!]

    for (i in (1:Nbunitobs))
    {
        tclarrayID[[i, 0]] <- levels(obs$unite_observation)[i]
        tclarrayID[[i, 1]] <- as.character(unitobs$site[unitobs$unite_observation ==
                                                        levels(obs$unite_observation)[i]])
        tclarrayID[[i, 2]] <- as.character(unitobs$biotope[unitobs$unite_observation ==
                                                        levels(obs$unite_observation)[i]])    #
        tclarrayID[[i, 3]] <-                                                                        #
            length(pacha$code_espece[pacha$pres_abs==1 &
                                     pacha$unite_observation == levels(obs$unite_observation)[i]])
        tclarrayID[[i, 4]] <- maxi[i]
    }

    tableInfodonnees <- Voirentableau(tclarrayID,
                                      title="Informations par unitobs",
                                      infos=paste("Informations par unité d'observation :",
                                                  "\n\n! tiennent compte des sélections en cours.",
                                                  sep=""),
                                      height=Nbunitobs, width=5, nrow=Nbunitobs + 1, ncol=5)
}


