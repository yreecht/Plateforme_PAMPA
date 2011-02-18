
########################################################################################################################
selectionObs.SVR.f <- function()
{
    ## Purpose: Définir le seuil de Dmin (en m) au-dessus duquel les
    ##          observations ne sont pas prises en compte.
    ## ----------------------------------------------------------------------
    ## Arguments: aucun
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date:  8 nov. 2010, 10:29

    onOK.selectionObs.SVR.f <- function()
    {
        ## Purpose: Action lorsque le bouton de choix de seuil pour les SVR est
        ##          cliqué.
        ## ----------------------------------------------------------------------
        ## Arguments: aucun
        ## ----------------------------------------------------------------------
        ## Author: Yves Reecht, Date:  8 nov. 2010, 12:01

        if (tclvalue(Suppr) == "1")
        {
            if (!is.na(as.numeric(tclvalue(Level))))
            {
                tclvalue(Done) <- "1"
            }else{
                tclvalue(Done) <- "2"
            }
        }else{
            tclvalue(Level) <- Inf
            tclvalue(Done) <- "1"
        }
    }

    dminDefault <- 5                    # 5m par défaut.

    Done <- tclVar("0")
    Suppr <- tclVar("1")                # Seuil utilisé par défaut.
    Level <- tclVar(dminDefault)        # tclVar pour le seuil (initialisée au défaut).

    WinSVR <- tktoplevel()
    tkwm.title(WinSVR, "Sélection des observations")

    FrameInfo <- tkframe(WinSVR, borderwidth=2, relief="groove")

    CB.supprObs <- tkcheckbutton(WinSVR, variable=Suppr)
    E.supprLevel <- tkentry(WinSVR, width=3, textvariable=Level)

    FrameBT <- tkframe(WinSVR)
    BT.OK <- tkbutton(FrameBT, text="   OK   ",
                      command=onOK.selectionObs.SVR.f)

    tkbind(WinSVR, "<Destroy>", function(){tclvalue(Done) <- "3"}) # En cas de destruction de fenêtre.
    tkbind(E.supprLevel, "<Return>", onOK.selectionObs.SVR.f)

    ## Placement des éléments graphiques :
    tkgrid(tklabel(WinSVR, text=""))

    tkgrid(FrameInfo, column=2, columnspan=2, sticky="we")
    tkgrid(tklabel(FrameInfo,
                   text=paste("Information\n\n Types d'interpolations : ",
                   ifelse(getOption("PAMPA.SVR.interp") == "extended",
                          "étendues !",
                          "simples !"), "\n", sep=""), justify="left"), sticky="w")

    tkgrid(tklabel(WinSVR, text=""))

    tkgrid(tklabel(WinSVR, text="\t"),
           CB.supprObs,
           tklabel(WinSVR, text="  Ne conserver que les observations pour lesquelles Dmin =< "),
           E.supprLevel,
           tklabel(WinSVR, text="m ?\t "))

    tkgrid(tklabel(FrameBT, text="\n\t"),
           BT.OK,
           tklabel(FrameBT, text="\t\n"))
    tkgrid(FrameBT, column=0, columnspan=5)

    tkfocus(E.supprLevel)

    winSmartPlace.f(WinSVR, xoffset=-200, yoffset=-50)

    repeat
    {
        tkwait.variable(Done)           # attente d'une modification de statut.

        switch(tclvalue(Done),
               "1"={                    # Statut OK :
                   tkdestroy(WinSVR)
                   return(as.numeric(tclvalue(Level)))
               },
               "2"={                    # Le seuil n'est pas numérique :
                   tkmessageBox(message="Vous devez choisir un seuil numérique (séparateur '.')",
                                icon="error")
                   tclvalue(Done) <- "0"
                   tkfocus(E.supprLevel)
                   winRaise.f(WinSVR)
                   next()
               },
               "3"={                    # Destruction de la fenêtre :
                   return(NULL)
               })
    }

    tkdestroy(WinSVR)
}


################################################################################
## Nom    : lectureFichierEspeces.f()
## Objet  : lecture du référentiel espèces
## Input  : fichier espèces
## Output : table espèces
## Modif 02/12/09 lecture d'un fichier CSV (DP)
################################################################################

lectureFichierEspeces.f <- function ()
{
    ## rm(especes)
    runLog.f(msg=c("Chargement du référentiel espèces :"))

    ## Importation des caracteristiques des especes
    especes <- read.table(fileNameRefEsp, sep="\t", dec=".", quote="", header=TRUE, encoding="latin1")
    names(especes) <- c("code_espece", "GrSIH", "CodeSIH", "IssCaap", "TaxoCode", "CodeFAO", "CodeFB", "Phylum",
                        "Cath_benthique", "Classe", "Ordre", "Famille", "Genre", "espece", "Identifiant", "ObsNC",
                        "ObsRUN", "ObsMAY", "ObsSTM", "ObsCB", "ObsBA", "ObsBO", "ObsCR", "taillemax", "L50",
                        "cryptique", "mobilite", "territorial", "nocturne", "comportement.grp", "agreg.saison",
                        "position.col.eau", "strategie.demo", "Type.ponte", "Habitat.Prefere", "Changement.sexe",
                        "regim.alim", "interet.chasseNC", "interet.chasseRUN", "interet.chasseMAY", "interet.chasseSTM",
                        "interet.chasseCB", "interet.chasseBA", "interet.chasseBO", "interet.chasseCR",
                        "interet.ligneNC", "interet.ligneRUN", "interet.ligneMAY", "interet.ligneSTM",
                        "interet.ligneCB", "interet.ligneBA", "interet.ligneBO", "interet.ligneCR", "interet.filetNC",
                        "interet.filetRUN", "interet.filetMAY", "interet.filetSTM", "interet.filetCB",
                        "interet.filetBA", "interet.filetBO", "interet.filetCR", "interet.casierNC",
                        "interet.casierRUN", "interet.casierMAY", "interet.casierSTM", "interet.casierCB",
                        "interet.casierBA", "interet.casierBO", "interet.casierCR", "interet.piedNC", "interet.piedRUN",
                        "interet.piedMAY", "interet.piedSTM", "interet.piedCB", "interet.piedBA", "interet.piedBO",
                        "interet.piedCR", "interetComMAY", "Coeff.a.Med", "Coeff.b.Med", "Coeff.a.NC", "Coeff.a.MAY",
                        "Coeff.b.NC", "Coeff.b.MAY", "poids.moyen.petits", "poids.moyen.moyens", "poids.moyen.gros",
                        "taille_max_petits", "taille_max_moyens", "niveau.a.et.b.MED", "niveau.a.et.b.NC",
                        "niveau.a.et.b.MAY", "emblematiqueNC", "emblematiqueRUN", "emblematiqueMAY", "emblematiqueSTM",
                        "emblematiqueCB", "emblematiqueBA", "emblematiqueBO", "emblematiqueCR", "stat.IUCN",
                        "autre.statutNC", "autre.statutRUN", "autre.statutMAY", "autre.statutSTM", "autre.statutCB",
                        "autre.statutBA", "autre.statutBO", "autre.statutCR", "etat.pop.localNC", "etat.pop.localRUN",
                        "etat.pop.localMAY", "etat.pop.localSTM", "etat.pop.localCB", "etat.pop.localBA",
                        "etat.pop.localBO", "etat.pop.localCR", "endemiqueNC", "endemiqueRUN", "endemiqueMAY",
                        "endemiqueSTM", "endemiqueCB", "endemiqueBA", "endemiqueBO", "endemiqueCR")

    ## Verification du nombre de colonnes:
    if (dim(especes)[2] != 125)
    {
        rm(especes)
        gestionMSGerreur.f("nbChampEsp")
    }
    if (nrow(especes)!=0)
    {
        especes[especes=="-999"] <- NA
    }

    ## Ajout de cathégories benthiques supplémentaires lues dans un fichier de correspondance :
    correspCatBenthique <- read.csv(paste(basePath, "/Exec/corresp-cat-benth.csv", sep=""), row.names=1)

    especes <- cbind(especes, correspCatBenthique[as.character(especes$Cath_benthique), , drop=FALSE])

    ## Pour vérif :
    ## na.omit(especes[as.integer(runif(50,min=1, max=3553)), c("Cath_benthique", "CategB_general", "CategB_groupe")])

    ## Suppression de la ligne en NA
    especes <- subset(especes, !is.na(especes$code_espece))
    assign("especes", especes, envir=.GlobalEnv)
}

################################################################################
## Nom    : opendefault.f()
## Objet  : choix de l'espace de travail par defaut C:/PAMPA
##          si pas fait, crée le dossier,
##          teste la présence de Fichiersortie/ si pas fait, crée le dossier,
##          création de la table des observations,
##          création de la table des unités d'observations,
##          création du référentiel espèces
##          création d'une table de contingence
##          création d'un récapitulatif du plan d'échantillonnage
## A FINIR
## Input  : fichier espèces
##          fichier observations
##          fichier unité d'observations
## Output : table espèces
##          table observations
##          table unité d'observations
##          fichier et table de contingence
##          fichier et table plan d'échantillonnage
################################################################################

## Creation de l'environnement par defaut
environnementdefault.f <- function (nameWorkspace)
{
    runLog.f(msg=c("Vérification de l'environnement de travail :"))

    if (!missing(nameWorkspace))
    {
        if (file.access(nameWorkspace, mode = 0)== 0)
        {
            tkinsert(txt.w, "end", paste(nameWorkspace, " existe\n", sep=""))
            if (file.access(paste(nameWorkspace, "/FichiersSortie", sep=""), mode = 0)==-1)
            {
                dir.create(paste(nameWorkspace, "/FichiersSortie", sep=""),
                           showWarnings = TRUE, recursive = FALSE, mode = "0777")
                tkinsert(txt.w, "end", paste("\n", nameWorkspace, "/FichiersSortie a été créé", sep=""))
            }
        }else{
            dir.create(nameWorkspace, showWarnings = TRUE, recursive = FALSE, mode = "0777")
            dir.create(paste(nameWorkspace, "/FichiersSortie", sep=""),
                       showWarnings = TRUE, recursive = FALSE, mode = "0777")
            tkinsert(txt.w, "end",
                     paste("\n", nameWorkspace, " et", nameWorkspace, "/FichiersSortie ont été créés", sep=""))
        }
    }else{
        gestionMSGerreur.f("noWorkspace")
    }
    ## message(nameWorkspace)
    return(nameWorkspace)
}

## Choix par defauts de C:/PAMPA
opendefault.f <- function ()
{

    pampaProfilingStart.f()

    runLog.f(msg=c("--------------------------------------------------------------------------------",
                   "Nouveau chargement des données :"))

    pathMaker.f()                       # MàJ des variables "fileNameUnitObs", "fileNameObs", "fileNameRefEsp". Pour les
                                        # cas où les variables fileName1-3 auraient changé.

    message(paste("chargement de ", fileNameUnitObs, fileNameObs, fileNameRefEsp))

    tkconfigure(ResumerEspaceTravail, text=paste("Espace de travail : ", nameWorkspace))

    infoGeneral.f(msg="Chargement des données")

    environnementdefault.f(nameWorkspace)
    ## après, return fonction dans variables environnement
    tkinsert(txt.w, "end", paste("\n", "Patientez, chargement des données en cours ...\n", sep=""))
    ## ################################################################################
    message(fileNameUnitObs)
    unitobs <- read.table(fileNameUnitObs, sep="\t", dec=".", header=TRUE, encoding="latin1")
    names(unitobs) <- c("AMP", "unite_observation", "type", "site", "station", "caracteristique_1", "caracteristique_2",
         "fraction_echantillonnee", "jour", "mois", "an", "heure", "nebulosite", "direction_vent", "force_vent",
         "etat_mer", "courant", "maree", "phase_lunaire", "latitude", "longitude", "statut_protection", "avant_apres",
         "biotope", "biotope_2", "habitat1", "habitat2", "habitat3", "visibilite", "prof_min", "prof_max", "DimObs1",
         "DimObs2", "nb_plong", "plongeur")

    levels(unitobs$caracteristique_1) <- c(levels(unitobs$caracteristique_1), "NA") # bon ça corrige l'erreur ci-dessous
                                        # mais est-ce bien nécessaire ? [yr: 23/08/2010]
    unitobs$caracteristique_1[is.na(unitobs$caracteristique_1)] <- "NA" # [!!!] Erreur !

    ## ## Réorganisation des niveaux du facteur "statut_protection" pour avoir les bonnes couleurs dans les graphiques :
    ## if (all(is.element(levels(unitobs$statut_protection), c("RE", "PP", "HR"))))
    ## {
    ##     levels(unitobs$statut_protection) <- sort(levels(unitobs$statut_protection), decreasing=TRUE)
    ## }else{}

    if (unique(unitobs$type)[1]=="PecRec")
    {
        x.lt <- as.POSIXlt(as.character(unitobs$heure), format="%Hh%M")
        unitobs$heureEnq <- x.lt$hour + x.lt$min/60 + x.lt$sec/3600
        x.lt <- as.POSIXlt(as.character(unitobs$DimObs1), format="%Hh%M")
        unitobs$heureDeb <- x.lt$hour + x.lt$min/60 + x.lt$sec/3600


        unitobs$DimObs1 <- sapply(seq(length.out=nrow(unitobs)),
                                  function(i)
                              {
                                  switch(as.character(unitobs$heureEnq[i] < unitobs$heureDeb[i]),
                                         "TRUE"=(24 - unitobs$heureDeb[i]) + unitobs$heureEnq[i],
                                         "FALSE"=unitobs$heureEnq[i] - unitobs$heureDeb[i],
                                         "NA"=NA)
                              })

        unitobs$DimObs1[unitobs$DimObs1 == 0] <- NA
    }

    if (nrow(unitobs)!=0)
    {
        unitobs[unitobs=="-999"] <- NA
    }

    ## Années : integer -> factor (nécessaire pour les analyses stats):
    unitobs$an <- factor(unitobs$an)

    assign("unitobs", unitobs, envir=.GlobalEnv)
    assign("siteEtudie", unique(unitobs$AMP), envir=.GlobalEnv)

    obs <- read.table(fileNameObs, sep="\t", dec=".", header=TRUE, encoding="latin1")

    if (unique(unitobs$type) != "SVR")
    {
        colnames(obs) <- c("unite_observation", "secteur", "code_espece", "sexe", "taille", "classe_taille", "poids",
                           "nombre", "dmin", "dmax")
    }else{
        ## On renomme les colonnes + identification du type d'interpolation :
        switch(as.character(ncol(obs)),
               "11"={
                   colnames(obs) <- c("unite_observation", "rotation", "secteur", "code_espece", "sexe", "taille",
                                      "classe_taille", "poids", "nombre", "dmin", "dmax")

                   options(PAMPA.SVR.interp="extended")
               },
               "14"={
                   colnames(obs) <- c("unite_observation", "rotation", "sec.valides", "sec.ciel", "sec.sol",
                                      "sec.obstrue", "code_espece", "sexe", "taille",
                                      "classe_taille", "poids", "nombre", "dmin", "dmax")

                   options(PAMPA.SVR.interp="basic")
               },
               stop("Le fichier d'observations ne contient pas le bon nombre de colonnes"))

        obs$rotation <- as.numeric(obs$rotation)

        dminMax <- NULL
        while (is.null(dminMax))
        {
            dminMax <- selectionObs.SVR.f()
        }

        obs <- subset(obs, dmin <= dminMax)
    }


    ## remplacement des -999 en NA
    if (as.logical(nrow(obs)))                      # !=0
    {
        obs[obs == "-999"] <- NA
    }

    ## nombre : numeric -> factor (nécessaire pour une bonne prise en compte dans les analyses stat) :
    obs$nombre <- as.integer(obs$nombre)

    ## if (is.na(unique(obs$unite_observation[-unique(unitobs$unite_observation)]))==FALSE) # [!!!]
                                        # cause erreur + complètement tordu
    if (!all(is.element(obs$unite_observation, unitobs$unite_observation))) # Ça devrait être mieux
                                        # comme ça !
    {
        ## cas ou obs contient des unites d'obs absentes dans unitobs
        warning("La table observations contient des unités d'observation absentes dans la table unitobs.")

        ## Ajout du message pour le chargement :
        infoLoading.f(msg=paste("Attention, la Table obs contient ",
                                sum(!is.element(obs$unite_observation, unitobs$unite_observation)),
                                " (sur ",
                                nrow(obs),
                                ") enregistrements avec des unités d'observation absentes dans la table unitobs ",
                                sep=""),
                     icon="warning")
    }

    ## Référentiel espèces :
    lectureFichierEspeces.f()

    ## ############# Récapitulatif du plan d'échantillonnage ############# # [!!!] : revoir tout ça !
    if (NA %in% unique(unitobs$site) == FALSE) # [!!!]
    {
        PlanEchantillonnage <- with(unitobs, table(an, caracteristique_1, biotope, statut_protection, exclude = NA))
    }else{
        if (NA %in% unique(unitobs$biotope) == FALSE) # [!!!]
        {
            PlanEchantillonnage <- with(unitobs, table(an, habitat3, statut_protection, exclude = NA))
        }else{
            PlanEchantillonnage <- with(unitobs, table(an, site, statut_protection, exclude = NA))
        }
    }

    PlanEchantillonnage <- with(unitobs, table(an, type, exclude = NA)) # [!!!] Et ça sert à quoi ce qu'il y a juste
                                        # avant ?
    ## A l'ajout de avant_apres, table de sortie vide ! trop de parametres ?
    ## PlanEchantillonnage = with(unitobs, table(an, type, site, biotope, statut_protection, avant_apres, exclude = NA))
    recap <- as.data.frame(PlanEchantillonnage)
    write.csv(recap, file=paste(NomDossierTravail, "PlanEchantillonnage.csv", sep=""), row.names=FALSE)
    message("Récapitulatif du plan d'échantillonnage créé : PlanEchantillonnage.csv")
    ## rm(PlanEchantillonnage)

    ## ################
    assign("obs", obs, envir=.GlobalEnv)

    tkconfigure(ResumerSituationFichierUnitesObs,
                text=paste("Fichier d'unités d'observations : ", fileNameUnitObs, " Nb Enr : ",
                           dim(unitobs)[1], " Nb Champs : ", dim(unitobs)[2]))
    tkconfigure(ResumerSituationFichierObs,
                text=paste("Fichier d'observations : ", fileNameObs, " Nb Enr : ",
                           dim(obs)[1], " Nb Champs : ", dim(obs)[2]))
    tkconfigure(ResumerSituationReferencielEspece,
                text=paste("Fichier référenciel espèce : ", fileNameRefEsp, " Nb Enr : ",
                           dim(especes)[1], " Nb Champs : ", dim(especes)[2]))
    tkconfigure(ResumerAMPetType,
                text=paste("AMP considérée", unique(unitobs$AMP), " type d'observation : ", unique(unitobs$type)))

    ## ################# Creation de la table de contingence ##################

    if (unique(unitobs$type) != "SVR")
    {
        obsSansCathBenth <- obs
        obsSansCathBenth$Genre <- especes$Genre[match(obsSansCathBenth$code_espece, especes$code_espece)]
        if(length(obsSansCathBenth$Genre[obsSansCathBenth$Genre=="ge."])>0)
        {
            infoLoading.f(msg=paste("Pour les calculs des indices de diversité, ",
                                     length(obsSansCathBenth$Genre[obsSansCathBenth$Genre=="ge."]),
                                     " observations de la table de contingence pour lesquels le genre ",
                                     " \n\tn'est pas renseigné ('ge.') dans le référentiel espèce ont été supprimées",
                                     sep=""),
                          icon="info")

            obsSansCathBenth <- subset(obsSansCathBenth, obsSansCathBenth$Genre!="ge.")

        }
        contingence <- tapply(obsSansCathBenth$nombre,
                              list(obsSansCathBenth$unite_observation, obsSansCathBenth$code_espece), na.rm=TRUE, sum)
    }else{
        contingenceSVRt <- tapply(obs$nombre,
                                  list(obs$unite_observation, obs$rotation, obs$code_espece), na.rm=TRUE, mean)
        contingenceSVRt[is.na(contingenceSVRt)] <- 0
        contingenceSVR <- as.data.frame(matrix(NA, dim(contingenceSVRt)[1] * dim(contingenceSVRt)[2] *
                                               dim(contingenceSVRt)[3], 4))
        colnames(contingenceSVR) = c("unitobs", "rotation", "code_espece", "abondance")
        contingenceSVR$abondance <- as.vector(contingenceSVRt)
        contingenceSVR$unitobs <- rep(dimnames(contingenceSVRt)[[1]],
                                      times = dim(contingenceSVRt)[2] * dim(contingenceSVRt)[3])
        contingenceSVR$rotation <- rep(dimnames(contingenceSVRt)[[2]],
                                       each = dim(contingenceSVRt)[1], times = dim(contingenceSVRt)[3])
        contingenceSVR$code_espece <- rep(dimnames(contingenceSVRt)[[3]],
                                          each = dim(contingenceSVRt)[1]*dim(contingenceSVRt)[2])
        contingence <- tapply(contingenceSVR$abondance,
                              list(contingenceSVR$unitobs, contingenceSVR$code_espece), na.rm=TRUE, sum)
    }

    contingence[is.na(contingence)] <- 0
    ## Suppression des especes qui ne sont jamais vues
    ## Sinon problemes pour les calculs d'indices de diversite.
    a <- which(apply(contingence, 2, sum, na.rm=TRUE) == 0)
    if (length(a) != 0)
    {
        contingence <- contingence[, -a, drop=FALSE]
    }
    rm(a)

    ## idem
    b <- which(apply(contingence, 1, sum, na.rm=TRUE) == 0)
    if (length(b) != 0)
    {
        contingence <- contingence[-b, , drop=FALSE]
    }
    rm(b)
    assign("contingence", contingence, envir=.GlobalEnv)

    ## Attention, si la table de contingence avait ete cree anterieurement lors
    ## d'une utilisation des routines par exemple, et est toujours presente
    ## dans le dossier de travail, elle sera detectee comme existante.
    if (exists("contingence", envir=.GlobalEnv, frame, mode="any", inherits=TRUE))
    {
        write.csv(contingence, file=paste(NomDossierTravail, "ContingenceUnitObsEspeces.csv", sep=""))
        message("Table de contingence unités d'observations/espèces créée : ContingenceUnitObsEspeces.csv")
    }

    if (!exists("contingence", envir=.GlobalEnv, frame, mode="any", inherits=TRUE))
    {
        infoLoading.f(msg="La table de contingence n'a pas été calculée",
                      icon="warning")

        warning("La table de contingence n'a pas été calculée")
    }

    ## Conversion des dates (tester si meilleur endroit pour faire ça) [yr: 17/08/2010] :


    ## Verification du type d'observation
    paste("Type d'observation =", unique(unitobs$type), sep=" ") # [inc] broken !
    if (length(unique(unitobs$type)) > 1)
    {
        tkmessageBox(message="Choisissez le ou les types d'observations que vous souhaitez analyser",
                     icon="warning", type="ok")

        message("Choix du type de jeux de données activé")

        ChoixFacteurSelect.f(unitobs$type, "type", "multiple", 1, "selectType")

        message("Choix du type de jeux de données activé, sélection sur :")
        message(selectType)

        obs$type <- unitobs$type[match(obs$unite_observation, unitobs$unite_observation)]
        obs <- subset(obs$type, obs$type == selectType)
        unitobs <- subset(unitobs$type, unitobs$type == selectType)
        assign("obs", obs, envir=.GlobalEnv)
        assign("unitobs", unitobs, envir=.GlobalEnv)
    }

    ## Creation des tables de base :
    creationTablesBase.f()

    ## ! ici, donner des noms avec une base variable, pour rendre les fichiers indépendants et plus facilement
    ## ! reconnaissables

    gestionMSGinfo.f("BasetxtCreate")
    gestionMSGaide.f("SelectionOuTraitement")
    MiseajourTableau.f(tclarray)
    ModifierMenuApresImport.f()
    creationTablesCalcul.f()
    ModifierInterfaceApresSelection.f("Aucun", "NA")

    tkgrid.configure(scr, sticky="ns")

    ## creation du vecteur de couleurs pour les futurs graphiques
    cl <<- colors()
    ## Fin lignes temporaires
    ## ################################################################################

    pampaProfilingEnd.f()
} # fin de opendefault.f
################################################################################


