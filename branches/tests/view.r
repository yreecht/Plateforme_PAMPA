########################################################################################################################
Voirentableau <- function(Montclarray, title="", height=-1, width=-1, nrow=-1, ncol=-1)
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
        write.csv(dataframetb, file=FichierCSV, row.names = FALSE)

        gestionMSGinfo.f("InfoRefSpeEnregistre", FichierCSV)

        tkmessageBox(message=paste("Votre fichier d'information sur le référentiel",
                                   "\n\tespèce a été enregistré au format",
                                   " CSV dans le dossier de travail", sep=""))
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
    tkgrid(frameOverwintb, sticky="ew", columnspan=3)
    tkgrid(imgAsLabelwintb,
           tklabel(frameOverwintb, text="\t\t"),
           LB.titre <- tklabel(frameOverwintb, text=title, relief="groove", borderwidth=2, bg="yellow"),
           padx=5, pady=5, sticky="w")

    tkgrid.configure(LB.titre, sticky="ew")

    tkgrid(tklabel(tb, text=paste("\n***", "\nVous pouvez copier-coller ce tableau dans Excel")))

    tkgrid(Enregistrer.but, Fermer.but, pady=5, padx=5)

    tkgrid(tabletb, yscrtb, columnspan=3)
    tkgrid.configure(yscrtb, sticky="nsw")
    tkgrid(xscr, sticky="new", columnspan=2)

    tkconfigure(tabletb, variable=Montclarray, background="white", selectmode="extended", rowseparator="\"\n\"",
                colseparator="\"\t\"", titlerows=1,
                maxwidth=550)

    tkgrid.configure(tabletb, columnspan=2, sticky="w")

    winSmartPlace.f(tb, xoffset=-30)

    return (tabletb)
}

########################################################################################################################
VoirPlanEchantillonnage.f <- function()
{
    runLog.f(msg=c("Affichage du plan d'échantillonnage :"))

    myRarrayPE <- read.csv(paste(nameWorkspace, "./FichiersSortie/PlanEchantillonnage.csv", sep=""),
                           sep=",", dec=".", header=TRUE)
    tkinsert(txt.w, "end", paste("\n fichier Plan d'échantillonnage lu :\n ", myRarrayPE))

    tclarrayPE <- tclArray()
    ## tclarrayPE[[0, ]] <- c("Année", "Type", "Fréquence")
    tclarrayPE[[0, 0]] <- "Année"
    tclarrayPE[[0, 1]] <- "Type"
    tclarrayPE[[0, 2]] <- "Fréquence"

    for (i in (1:dim(myRarrayPE)[1]))
    {
        for (j in (0:2))
        {
            tclarrayPE[[i, j]] <- myRarrayPE[i, j+1]
        }
    }

    pe <- tktoplevel()
    tkwm.title(pe, "Plan d'échantillonnage")
    tablePlanEch <- tkwidget(pe, "table", variable=tclarrayPE, rows=dim(myRarrayPE)[1]+1, cols=3, titlerows=1,
                             selectmode="extended", colwidth=30, background="white")
    tkpack(tablePlanEch)
}

########################################################################################################################
VoirInformationsDonneesEspeces.f <- function()
{

    Nbesp <- length(unique(unitesp$code_espece))
    message(paste(Nbesp, "espèces considérées dans ce jeux de données"))

    tclarrayID <- tclArray()
    tclarrayID[[0, 0]] <- "Espece"               # Indexation étrange... [yr: 27/07/2010]
    tclarrayID[[0, 1]] <- "Nb indiv min/unitobs" #
    tclarrayID[[0, 2]] <- "Nb indiv max/unitobs" #
    tclarrayID[[0, 3]] <- "Frequence espece"     #

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

    tableInfodonnees <- Voirentableau(tclarrayID, title="Informations par espèce",
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

    tableInfodonnees <- Voirentableau(tclarrayID, title="Informations par unitobs",
                                      height=Nbunitobs, width=5, nrow=Nbunitobs + 1, ncol=5)
}


