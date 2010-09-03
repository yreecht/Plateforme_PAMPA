openfile.f = function()
{
    print("fonction openfile.f activée")

    ## Choix de l'espace de travail
    chercheEspaceTravail.f = function() # [imb]
    {
        nameWorkspace = setwd(tclvalue(tkchooseDirectory()))
        if (!nchar(nameWorkspace))
        {
            tkmessageBox(message="Aucun espace de travail n'a ete selectionné!")
        }
        tkconfigure(ResumerEspaceTravail, text=paste("Espace de travail : ",nameWorkspace))
        tkinsert(helpframe,"end","\n Choisissez maintenant votre fichier d'unités d'observations")
    }

####  Choix des fichiers de donnees source en .txt
    openUnitobs.f = function() # [imb]
    {

        print("fonction openUnitobs activée")
        nameUnitobs<-tclvalue(tkgetOpenFile())
        nameUnitobs<-sub(paste(nameWorkspace,"/Data/",sep=""),'',nameUnitobs)   #ici, on enlève le nom de chemin pour ne conserver que le nom du fichier
        if (!nchar(nameUnitobs))
        {
            tkmessageBox(message="Aucun fichier n'a ete selectionne!")
        }
        print(nameUnitobs)

        tkconfigure(ResumerSituationFichierUnitesObs, text=paste("Fichier d'unités d'observations : ",nameUnitobs))
        tkinsert(helpframe,"end","\n Choisissez maintenant votre fichier d'observations")
        ## nameUnitobs
        assign("fileNameUnitObs",paste(nameWorkspace,"/Data/",nameUnitobs,sep=""),envir=.GlobalEnv)
        assign("fileName1",paste(nameWorkspace,"/Data/",nameUnitobs,sep=""),envir=.GlobalEnv)
    }

    openObservations.f = function() # [imb]
    {

        print("fonction openObservations activée")
        namefileObs<-tclvalue(tkgetOpenFile())
        namefileObs<-sub(paste(nameWorkspace,"/Data/",sep=""),'',namefileObs)   #ici, on enlève le nom de chemin pour ne conserver que le nom du fichier
        if (!nchar(namefileObs))
            tkmessageBox(message="Aucun fichier n'a ete selectionne!")
        print(namefileObs)
        assign("fileNameObs",namefileObs,envir=.GlobalEnv)
        assign("fileName2",paste(nameWorkspace,"/Data/",namefileObs,sep=""),envir=.GlobalEnv)
        ## ici du coup, on peut y mettre un choix ou reconnaitre le référenciel automatiquement
        tkconfigure(ResumerSituationFichierObs, text=paste("Fichier d'observations : ",namefileObs))
        tkinsert(helpframe,"end","\n Sélectionnez votre référenciel espèce")
    }

    openListespeces.f = function() # [imb]
    {

        print("fonction openListespeces activée")
        namefileRef<-tclvalue(tkgetOpenFile())
        namefileRef<-sub(paste(nameWorkspace,"/Data/",sep=""),'',namefileRef)   #ici, on enlève le nom de chemin pour ne conserver que le nom du fichier
        if (!nchar(namefileRef))
        {
            tkmessageBox(message="Aucun fichier n'a ete selectionne!")
        }
        print(namefileRef)
        tkconfigure(ResumerSituationReferencielEspece, text=paste("Fichier référenciel espèce : ",namefileRef))
        assign("fileName3",paste(nameWorkspace,"/Data/",namefileRef,sep=""),envir=.GlobalEnv)
    }

    tt <- tktoplevel(height=50,width=300)
    tkwm.title(tt,"Import des fichiers de donnees")
    OK <- tclVar(0)
    button.widget0 <- tkbutton(tt,text="Espace de travail",width=45,command=chercheEspaceTravail.f)
    button.widget1 <- tkbutton(tt,text="Table de donnees unites d'observation",command=openUnitobs.f)
    button.widget2 <- tkbutton(tt,text="Table de donnees d'observations",command=openObservations.f)
    button.widget3 <- tkbutton(tt,text="Referentiel especes",command=openListespeces.f)
    OnOK <- function()  # [imb]
    {
        tkdestroy(tt)
    }
    OK.but <-tkbutton(tt,text="Valider",command=OnOK)
    tkgrid(button.widget0,ResumerEspaceTravail<-tklabel(tt,text=paste("Espace de travail : ","non sélectionné - par défaut :",nameWorkspace)))
    tkgrid(button.widget1,ResumerSituationFichierUnitesObs<-tklabel(tt,text=paste("Fichier d'unités d'observations : ","non sélectionné - par défaut :",fileName1)))
    tkgrid(button.widget2,ResumerSituationFichierObs<-tklabel(tt,text=paste("Fichier d'observations : ","non sélectionné - par défaut :",fileName2)))
    tkgrid(button.widget3,ResumerSituationReferencielEspece<-tklabel(tt,text=paste("Référentiel espèce : ","non sélectionné - par défaut :",fileName3)))
    tkgrid(OK.but)
    tkgrid.configure(button.widget0,button.widget1,button.widget2,button.widget3,sticky="w")
    tkgrid.configure(OK.but,sticky="we")

    tkfocus(tt)
    tkwait.window(tt)
    ## Changement des variables globales
    assign("fileNameUnitObs",fileName1,envir=.GlobalEnv)
    assign("fileNameObs",fileName2,envir=.GlobalEnv)
    assign("fileNameRefEsp",fileName3,envir=.GlobalEnv)

    opendefault.f()
}



################################################################################
## IMPORTATION DES DONNEES
##    Données observations : table "obs"
##    Données unités d'observation : table "unitobs"
##    Données caractéristiques des espèces : table "especes"
##
## Après importation, un récapitulatif du plan d'echantillonnage est généré
## Un tableau de contingence "contingence.txt" est également créé
################################################################################

importTremail.f = function()
{
    print("fonction importTremail activée")
    typePeche = "Tremail"
    assign("typePeche",typePeche,envir=.GlobalEnv)
    import.donnees.f()
}

importChasseFusil.f = function() {
    print("fonction importChasseFusil activée")
    typePeche = "Chasse fusil"
    assign("typePeche",typePeche,envir=.GlobalEnv)
    import.donnees.f()
}

importLigneSansCanne.f = function()
{
    print("fonction importLigneSansCanne activée")
    typePeche = "Ligne sans canne"
    assign("typePeche",typePeche,envir=.GlobalEnv)
    import.donnees.f()
}

importLigneAvecCanne.f = function()
{
    print("fonction importLigneAvecCanne activée")
    typePeche = "Ligne avec canne"
    assign("typePeche",typePeche,envir=.GlobalEnv)
    import.donnees.f()
}


################################################################################
## Nom    : import.donnees.f()
## Objet  : choix d'un espace de travail
##          création de la table des observations,
##          création de la table des unités d'observations,
##          création du référentiel espèces
##          création d'une table de contingence
##          création d'un récapitulatif du plan d'échantillonnage
## Input  : fichier espèces
##          fichier observations
##          fichier unité d'observations
## Output : table espèces
##          table observations
##          table unité d'observations
##          fichier et table de contingence
##          fichier et table plan d'échantillonnage
################################################################################

import.donnees.f = function()
{
    print("fonction import.donnees activée")
    ## Choix de l'espace de travail
    openWorkspace.f = function()  # [imb]
    {
        nameWorkspace = setwd(tclvalue(tkchooseDirectory()))
        if (!nchar(nameWorkspace))
        {
            tkmessageBox(message="Aucun workspace n'a ete selectionne!")
        }
        tkconfigure(ResumerEspaceTravail, text=paste("Espace de travail : ",nameWorkspace))
        tkinsert(helpframe,"end","\n Choisissez maintenant votre fichier d'unités d'observations")
    }

    tt <- tktoplevel(height=50,width=300)
    tkwm.title(tt,"Import des fichiers de donnees")
    OK <- tclVar(0)
    button.widget0 <- tkbutton(tt,text="Espace de travail",command=openWorkspace.f)
    list.widget <- tklistbox(tt,height=8,width=30,selectmode="single",background="white")
    sitesPAMPA <- c("Banyuls","Bonifacio","Cap Roux","Côte Bleue","Mayotte","Nouvelle-Caledonie","Reunion","Saint-Martin")
    aliasSites <- c("BA","BO","CR","CB","MA","NC","RUN","STM")

    for (i in (1:length(sitesPAMPA)))
    {
        tkinsert(list.widget,"end",sitesPAMPA[i])
    }
    button.widget1 <- tkbutton(tt,text="Table de donnees unites d'observation",command=openUnitobs.f)
    button.widget2 <- tkbutton(tt,text="Table de donnees d'observations",command=openObservations.f)
    button.widget3 <- tkbutton(tt,text="Referentiel especes",command=openListespeces.f)

    OnOK <- function()  # [imb]
    {
        siteEtudie <- aliasSites[as.numeric(tkcurselection(list.widget))+1]
        assign("siteEtudie",siteEtudie,envir=.GlobalEnv)
        tkdestroy(tt)
    }
    OK.but <-tkbutton(tt,text="Valider",command=OnOK)
    tkpack(button.widget0,list.widget,button.widget1,button.widget2,button.widget3,OK.but)
    tkfocus(tt)
    tkwait.window(tt)

    ## constitution des tables
    assign("fileNameUnitObs",fileName1,envir=.GlobalEnv)
    assign("fileNameObs",fileName2,envir=.GlobalEnv)
    assign("fileNameRefEsp",fileName3,envir=.GlobalEnv)

    ## ############# Import des donnees unites d'observation ##############

    unitobs <- read.table(fileNameUnitObs, sep="\t", dec=".", header=TRUE, colClasses = "character", encoding="latin1")
    names(unitobs) <- c("AMP","unite_observation","type","site","station","caracteristique_1","caracteristique_2",
                        "fraction_echantillonnee","jour","mois","an",
                        "heure","nebulosite","direction_vent","force_vent","etat_mer","courant","maree","phase_lunaire",
                        "latitude","longitude","statut_protection","avant_apres","biotope","biotope_2",
                        "habitat1","habitat2","habitat3","visibilite","prof_min","prof_max","DimObs1",
                        "DimObs2","nb_plong","plongeur")

    unitobs$caracteristique_1[is.na(unitobs$caracteristique_1)]="na"
    if(unique(unitobs$caracteristique_1)=="PecRec")
    {
        unitobs$DimObs1bis<-unitobs$DimObs1
        x.lt <- as.POSIXlt(as.character(unitobs$heure), format="%Hh%M")
        unitobs$heureEnq <- x.lt$hour + x.lt$min/60 + x.lt$sec/3600
        x.lt <- as.POSIXlt(as.character(unitobs$DimObs1), format="%Hh%M")
        unitobs$heureDeb<-x.lt$hour + x.lt$min/60 + x.lt$sec/3600
        for (i in 1:nrow(unitobs))
        {
            if (unitobs$heureEnq[i] < unitobs$heureDeb[i])
            {
                unitobs$DimObs1bis[i]<-(24-unitobs$heureDeb[i])+unitobs$heureEnq[i]
            }else{
                unitobs$DimObs1bis[i]<-unitobs$heureEnq[i]-unitobs$heureDeb[i]
            }
        }
        unitobs$DimObs1<-unitobs$DimObs1bis
        unitobs<-unitobs[,c("AMP","unite_observation","type","site","station","caracteristique_1","caracteristique_2",
                            "fraction_echantillonnee","jour","mois","an",
                            "heure","nebulosite","direction_vent","force_vent","etat_mer","courant","maree","phase_lunaire",
                            "latitude","longitude","statut_protection","avant_apres","biotope","biotope_2",
                            "habitat1","habitat2","habitat3","visibilite","prof_min","prof_max","DimObs1",
                            "DimObs2","nb_plong","plongeur")]
    }


    unitobs$visibilite = as.numeric(unitobs$visibilite)
    unitobs$prof_min = as.numeric(unitobs$prof_min)
    unitobs$prof_max = as.numeric(unitobs$prof_max)
    unitobs$DimObs1=as.numeric(unitobs$DimObs1)
    unitobs$DimObs2=as.numeric(unitobs$DimObs2)
    assign("unitobs",unitobs,envir=.GlobalEnv)

    ## Verification du nombre de colonnes:
    if (dim(unitobs)[2] != 35)
    {
        rm(unitobs)
        gestionMSGerreur.f(nbChampUnitobs)
    }

    ## ############# Import des données observation ##############

    obs <- read.table(fileNameObs, sep="\t", dec=".", header=TRUE, colClasses = "character", encoding="latin1")
    if (unique(unitobs$type) != "SVR")
    {
        names(obs)=c("unite_observation","secteur","code_espece","sexe","taille","classe_taille","poids","nombre","dmin","dmax")
    }else{
        names(obs)=c("unite_observation","rotation","secteur","code_espece","sexe","taille","classe_taille","poids","nombre","dmin","dmax")
        obs$rotation=as.numeric(obs$rotation)
    }

    obs$secteur=as.numeric(obs$secteur)
    obs$taille=as.numeric(obs$taille)
    obs$poids=as.numeric(obs$poids)
    obs$nombre=as.numeric(obs$nombre)
    obs$dmin=as.numeric(obs$dmin)
    obs$dmax=as.numeric(obs$dmax)

    ## Cas de la video : selection des observations dont dmin <=5
    if (unique(unitobs$type) == "SVR")
    {
        obs = subset(obs,dmin<=5)
    }
    ## Cas du distance sampling / selection des observations dont dmin <=5 et on conserve
    ## la proportion de D2-D1 si on a une observation avec D1 <5m et D2 >5m (sinon sous-estimation des metriques comptage)
    if (unique(unitobs$type) == "UVCDS")
    {
        obs = subset(obs,dmin<=5)
        ## modification des abondances et poids bruts dans le cas D1<5m et D2>5m
        for (i in 1:dim(obs)[1])
        {
            if (obs$dmax[i]>5 && obs$dmin[i]<5)
            {
                obs$nombre[i] = obs$nombre[i] * ((5-obs$dmin[i]) /(obs$dmax[i]-obs$dmin[i]))
                obs$poids[i] = obs$poids[i] * ((5-obs$dmin[i]) /(obs$dmax[i]-obs$dmin[i]))
            }
        }
    }

    ## Vérification du nombre de colonnes:
    if (unique(unitobs$type) != "SVR")
    {
        if (dim(obs)[2] != 10)
        {
            rm(obs)
            tkmessageBox(message="ATTENTION, votre fichier 'Observations' ne comporte pas le bon nombre de champs! Corrigez-le et recommencez l'importation.",icon="warning",type="ok")
        }
    }else{
        if (dim(obs)[2] != 11)
        {
            rm(obs)
            tkmessageBox(message="ATTENTION, votre fichier 'Observations' ne comporte pas le bon nombre de champs! Corrigez-le et recommencez l'importation.",icon="warning",type="ok")
        }
    }

    ## remplacement des valeurs -999 par NA
    ## ne concerne que les champs numeriques
    obs$secteur[obs$secteur=="-999"]<-NA
    obs$taille[obs$taille=="-999"]<-NA
    obs$poids[obs$poids=="-999"]<-NA
    obs$nombre[obs$nombre=="-999"]<-NA
    obs$dmin[obs$dmin=="-999"]<-NA
    obs$dmax[obs$dmax=="-999"]<-NA

    ## suppression des observations dont le nombre d'individus est à zero
    if (dim(subset(obs, nombre == 0))[1] > 0)
    {
        obs <- subset(obs, nombre != 0)
        ## tkmessageBox(message="Des observations dont le nombre d'individus est à 0 ont ete supprimees.",icon="warning",type="ok")
    }

    assign("obs",obs,envir=.GlobalEnv)

    ## ############# Importation des caracteristiques des especes #############
    lectureFichierEspeces.f()

    ## rm(fileNameUnitObs,envir=.GlobalEnv)
    ## rm(fileNameObs,envir=.GlobalEnv)
    ## rm(fileNameRefEsp,envir=.GlobalEnv)

    ## ############# Mise à jour des valeurs dans le tableau #############
    tclarray[[1,1]] <- fileName1
    tclarray[[1,2]] <- dim(unitobs)[1]
    tclarray[[1,3]] <- dim(unitobs)[2]
    tclarray[[2,1]] <- fileName2
    tclarray[[2,2]] <- dim(obs)[1]
    tclarray[[2,3]] <- dim(obs)[2]
    tclarray[[3,1]] <- fileName3
    tclarray[[3,2]] <- dim(especes)[1]
    tclarray[[3,3]] <- dim(especes)[2]
    ## ############# Récapitulatif du plan d'échantillonnage #############
    if (NA %in% unique(unitobs$site) == FALSE)
    {
        PlanEchantillonnage = with(unitobs,table(an,caracteristique_1,biotope,statut_protection, exclude = NA))
    }else{
        if (NA %in% unique(unitobs$biotope) == FALSE)
        {
            PlanEchantillonnage = with(unitobs,table(an,habitat3,statut_protection, exclude = NA))
        }else{
            PlanEchantillonnage = with(unitobs,table(an,site,statut_protection, exclude = NA))
        }
    }

    PlanEchantillonnage = with(unitobs,table(an,type, exclude = NA))
    ## A l'ajout de avant_apres, table de sortie vide ! trop de parametres ?
    ## PlanEchantillonnage = with(unitobs,table(an,type,site,biotope,statut_protection,avant_apres, exclude = NA))
    recap = as.data.frame(PlanEchantillonnage)
    write.csv(recap,file=paste(nameWorkspace,"/FichiersSortie/PlanEchantillonnage.csv",sep=""),row.names=FALSE)
    print(recap)
    print("Recapitulatif du plan d'echantillonnage cree : PlanEchantillonnage.csv")
    rm(PlanEchantillonnage)


    ## ################# Creation de la table de contingence ##################

    if (unique(unitobs$type) != "SVR")
    {
        contingence <- tapply(obs$nombre, list(obs$unite_observation, obs$code_espece), sum, na.rm=TRUE)
    } else {
        contingenceSVRt <- tapply(obs$nombre,list(obs$unite_observation,obs$rotation,obs$code_espece), mean, na.rm=TRUE)
        contingenceSVRt[is.na(contingenceSVRt)] <- 0
        contingenceSVR <- as.data.frame(matrix(NA, dim(contingenceSVRt)[1] *
                                                   dim(contingenceSVRt)[2] * dim(contingenceSVRt)[3], 4))
        colnames(contingenceSVR) <- c("unitobs","rotation","code_espece","abondance")
        contingenceSVR$abondance <- as.vector(contingenceSVRt)
        contingenceSVR$unitobs <- rep(dimnames(contingenceSVRt)[[1]],times = dim(contingenceSVRt)[2] * dim(contingenceSVRt)[3])
        contingenceSVR$rotation <- rep(dimnames(contingenceSVRt)[[2]],each = dim(contingenceSVRt)[1], times = dim(contingenceSVRt)[3])
        contingenceSVR$code_espece <- rep(dimnames(contingenceSVRt)[[3]],each = dim(contingenceSVRt)[1]*dim(contingenceSVRt)[2])
        contingence <- tapply(contingenceSVR$abondance, list(contingenceSVR$unitobs,contingenceSVR$code_espece), sum, na.rm=TRUE)
    }

    contingence[is.na(contingence)] <- 0
    ## Suppression des especes qui ne sont jamais vues
    ## Sinon problemes pour les calculs d'indices de diversite.
    a <- which(apply(contingence, 2, sum, na.rm=TRUE) == 0) # On peut faire plus direct [yr: 17/08/2010]
    if (length(a) != 0)
    {
        contingence <- contingence[ , -a, drop=FALSE] # drop=FALSE pour conserver une matrice [yr: 17/08/2010]
    }
    rm(a)
    ## idem
    b <- which(apply(contingence, 1, sum, na.rm=TRUE) == 0) # On peut faire plus direct [yr: 17/08/2010]
    if (length(b) != 0)
    {
        contingence <- contingence[-b, , drop=FALSE] # drop=FALSE pour conserver une matrice [yr: 17/08/2010]
    }
    rm(b)
    assign("contingence",contingence,envir=.GlobalEnv)

    ## Attention, si la table de contingence avait ete cree anterieurement lors
    ## d'une utilisation des routines par exemple, et est toujours presente
    ## dans le dossier de travail, elle sera detectee comme existante.
    if (exists("contingence", envir=.GlobalEnv, frame, mode="any", inherits=TRUE))
    {
        print("Table de contingence unite d'observations/especes creee : ContingenceUnitObsEspeces.csv")
        write.csv(contingence,file=paste(nameWorkspace,"/FichiersSortie/ContingenceUnitObsEspeces.csv",sep=""))
    }
    if (!exists("contingence", envir=.GlobalEnv, frame, mode="any", inherits=TRUE))
    {
        ReturnVal <- tkmessageBox(title="Table de contingence", message="La table de contingence n'a pas ete calculee",
                                  icon="warning",type="ok")
        print("La table de contingence n'a pas ete calculee")
    }

    ## Verification du type d'observation
    paste("Type d'observation =",unique(unitobs$type),sep=" ")

    if (length(unique(unitobs$type)) > 1)
    {
        tkmessageBox(message="ATTENTION, differents types d'observations ne peuvent être analyses ensemble!",icon="warning",type="ok")
    } else {
        ## Creation des tables de base
        creationTablesBase.f()
        biomasse.f()
        tkmessageBox(message="\nLes fichiers .csv:
     - Contingence
     - PlanEchantillonnage
     - Metriques par unité d'observation (UnitobsMetriques.csv)
     - Metriques par unité d'observation pour les espèces présentes (ListeEspecesUnitobs.csv)
     - Metriques par unité d'observation / espèce (UnitobsEspeceMetriques.csv)
     - Metriques par unité d'observation / espèce / classe de taille (UnitobsEspeceClassetailleMetriques.csv )
     ont été crées",icon="warning",type="ok") # spell checking !!! [yr: 27/07/2010] [OK]
    }
    gestionMSGinfo.f("BasetxtCreate")
    gestionMSGaide.f("SelectionOuTraitement")
    ModifierMenuApresImport.f()
    ## creation du vecteur de couleurs pour les futurs graphiques
    cl <<- colors()
} #fin import.donnees.f()
