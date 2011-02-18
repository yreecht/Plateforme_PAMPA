Voirentableau <- function(Montclarray, title="", height=-1, width=-1, nrow=-1, ncol=-1)
{
    tb <- tktoplevel()
    tclRequire("Tktable")
    tkwm.title(tb, title)

    ## Fonctions activées par les boutons de la fenêtre
    FermerWintb <- function()
    {
        tkdestroy(tb)
    }

    EnregistrerWintb <- function()
    {
        FichierCSV <- paste(NomDossierTravail, "Tableau_", title, ".csv", sep="")
        write.csv(dataframetb, file=FichierCSV, row.names = FALSE)
        ## gestionMSGinfo.f("InfoRefSpeEnregistre", FichierCSV)
        tkmessageBox(message="Votre fichier d'information sur le référentiel
                espèce a été enregistré au format CSV dans le dossier de travail")
    }

    ## Déclaration des objets bouton
    Fermer.but <- tkbutton(tb, text="Fermer", command=FermerWintb)
    Enregistrer.but <- tkbutton(tb, text="Enregistrer en CSV", command=EnregistrerWintb)
    message("tableau")
    message(ncol)
    message(nrow)
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
    message("data frame réalisée")
    dim(dataframetb)
    frameOverwintb <- tkframe(tb)
    imgAsLabelwintb <- tklabel(tb, image=imageAMP, bg="white")
    tkgrid(imgAsLabelwintb, frameOverwintb, sticky="w")
    tkgrid(tklabel(frameOverwintb, text=title, relief="groove", borderwidth=2, bg="yellow"))
    tkgrid(tklabel(tb, text=paste("\n***", "\nVous pouvez copier-coller ce tableau dans Excel")))
    tkgrid(Enregistrer.but, Fermer.but)
    tabletb <- tkwidget(tb, "table", rows=nrow, cols=ncol, titlerows=1, titlecols=0,
                        height=height+1, width=width+1, colwidth=23,
                        xscrollcommand=function(...) tkset(xscr,...),
                        yscrollcommand=function(...) tkset(yscrtb,...))
    xscr <-tkscrollbar(tb, orient="horizontal", command=function(...)tkxview(tabletb,...))
    yscrtb <- tkscrollbar(tb, command=function(...)tkyview(tabletb,...))

    tkgrid(tabletb, yscrtb, columnspan=3)
    tkgrid.configure(yscrtb, sticky="nsw")
    tkgrid(xscr, sticky="new")
    tkconfigure(tabletb, variable=Montclarray, background="white", selectmode="extended", rowseparator="\"\n\"",
                colseparator="\"\t\"", titlerows=1)
    tkgrid.configure(tabletb, columnspan=2, sticky="w")
    return (tabletb)
}

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
    nbunitobs <- dim(unique(unitobs))[1]
    pacha <- unitesp[, c("unite_observation", "code_espece", "nombre", "pres_abs")]

    for (i in (1:Nbesp))
    {
        tclarrayID[[i, 0]] <- unique(unitesp$code_espece)[i]
        tclarrayID[[i, 1]] <- mini[i]
        tclarrayID[[i, 2]] <- maxi[i]
        tclarrayID[[i, 3]] <-
            paste(round(length(pacha$unite_observation[pacha$pres_abs==1 &
                                                       pacha$code_espece==unique(unitesp$code_espece)[i]]) /
                        length(pacha$unite_observation[pacha$code_espece==unique(unitesp$code_espece)[i]]) * 100,
                        digits=2), "%")
    }
    tableInfodonnees <- Voirentableau(tclarrayID, title="Informations par espèce",
                                      height=Nbesp, width=4, nrow=Nbesp, ncol=4)
}#fin VoirInformationsDonneesEspeces

VoirInformationsDonneesUnitobs.f <- function()
{

    Nbunitobs <- length(unique(unitobs$unite_observation))
    message(paste(Nbunitobs, "unités d'observations considérées dans ce jeux de données"))
    tclarrayID <- tclArray()

    tclarrayID[[0, 0]] <- "Unité d'observation"    # Indexation étrange... [yr: 27/07/2010]
    tclarrayID[[0, 1]] <- "Site"                   #
    tclarrayID[[0, 2]] <- "Biotope"                #
    tclarrayID[[0, 3]] <- "Nb espèce/unitobs"      #
    tclarrayID[[0, 4]] <- "Nb indiv max/unitobs"   #

    nbunitobs <- dim(unique(unitobs))[1]
    pacha <- unitesp[, c("unite_observation", "code_espece", "nombre", "pres_abs")]
    ## mini=tapply(unitesp$nombre, unitesp$code_espece, min, na.rm=TRUE)
    message(head(pacha))
    maxi <- tapply(unitobs$nombre, unitobs$unite_observation, max, na.rm=TRUE)
    for (i in (1:Nbunitobs))
    {
        tclarrayID[[i, 0]] <- unique(unitobs$unite_observation)[i]                     # Indexation étrange... [yr: 27/07/2010]
        tclarrayID[[i, 1]] <- unitobs$site[unique(unitobs$unite_observation)[i]]       #
        tclarrayID[[i, 2]] <- unitobs$biotope[unique(unitobs$unite_observation)[i]]    #
        tclarrayID[[i, 3]] <-                                                          #
            length(pacha$code_espece[pacha$pres_abs==1 &
                                     pacha$unite_observation==unique(unitobs$unite_observation)[i]])
        tclarrayID[[i, 4]] <- "autre"
    }
    message("Tableau récapitulatif de unitobs réalisé")

    tableInfodonnees <- Voirentableau(tclarrayID, title="Informations par unitobs",
                                      height=Nbunitobs, width=5, nrow=Nbunitobs, ncol=5)
}


