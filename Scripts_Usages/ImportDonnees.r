################################################################################
# Nom              : import donnees.r
# Type              : Programme
# Objet             : Programme d'importation des données à partir des fichiers 
#                     texte enregistrés dans le dossier C:/PAMPA/Donnees_Usages.
# Input             : TXT et SHP
# Output            : Data
# Auteur            : Elodie Gamp
# R version         : 2.11.1
# Date de création  : Avril 2011
# Sources
################################################################################

###################  SCRIPT D'IMPORTATION DES DONNEES  #########################
#
# Ce script contient l'importation des données de fréquentation, des données des
# enquêtes de pêche récréative, de plaisance, de plongée et de sentier sous-marin.
# Afin de pouvoir les traiter, sont également importées le référentiel espèces, 
# le référentiel spatial ainsi que le fond de carte
# On y constitue 9 tables contenant ces informations et une carte :
# "freqtot" qui contient les données de fréquentation
# "peche" qui contient les données des questionnaires de pêche
# "captures" qui contient les données de captures du jour
# "capturesAn" qui contient les données de captures annuelles
# "plaisance" qui contient les données des questionnaires de plaisance
# "plongee" qui contient les données des questionnaires de plongée
# "excursion" qui contient les données des questionnaires de sentier sous-marin
# "refEspeces" qui contient le référentiel espèces
# "refSpatial" qui contient le référentiel spatial du site
# "refEngin" qui contient le référentiel engin 
# "carte" qui contient le fond de carte du site étudié. (supprimé actuellement 
#    car package maptools non fonctionnel sur les nouvelles versions de R)
#
#####################################################################################

# fonction pour compter les lignes sans NA
lengthnna.f = function(x){length(x[!is.na(x)])}


### choix du site d'étude
  tt <- tktoplevel(height=50, width=300)
  winSmartPlace.f(tt)
  tkwm.title(tt, "Import des fichiers de donnees")
  OK <- tclVar(0)
  #button.widget0 <- tkbutton(tt, text="Espace de travail", command=openWorkspace.f)
  list.widget <- tklistbox(tt, height=8, width=30, selectmode="single", background="white")
  sitesPAMPA <- c("Banyuls", "Bonifacio", "Cap Roux", "Côte Bleue", "Mayotte", "Nouvelle-Caledonie", "Reunion", "Saint-Martin")
  aliasSites <- c("BA", "BO", "CR", "CB", "MA", "NC", "RUN", "STM")
  for (i in (1:length(sitesPAMPA))) {
      tkinsert(list.widget, "end", sitesPAMPA[i])
  }
  OnOK <- function() {
       siteEtudie <- aliasSites[as.numeric(tkcurselection(list.widget))+1]
       assign("siteEtudie", siteEtudie, envir=.GlobalEnv)
       tkdestroy(tt)
  }
  OK.but <-tkbutton(tt, text="Valider", command=OnOK)
  tkpack(list.widget, OK.but)
  tkfocus(tt)
  tkwait.window(tt)


# intialisation de la fenêtre d'information au cours du chargement
  infoGeneral.f(msg=paste("      Chargement des données du site", siteEtudie, "      "), 
                    font=tkfont.create(weight="bold", size=9), foreground="darkred")

# initialisation de la barre de progression                    
    initInnerTkProgressBar.f(initial=0, max=15, width=450)
    stepInnerProgressBar.f(n=0, msg="Début du chargement")  # progression du chargement
    
# message d'information et de rappel pour l'importation des données dans R
  infoLoading.f(msg=, "Les tables sont importées automatiquement!            
                    Vérifiez que : 
                        - les noms des fichiers sont bien ceux précisés dans le guide d'utilisation des scripts Usages
                        - tous les fichiers sont dans le dossier 'C:/PAMPA/Donnees_Usages'.", 
                icon="info")


## importation du calendrier général (obligatoire pour les calculs des moyennes stratifiées)
stepInnerProgressBar.f(n=1, msg="Chargement du calendrier général")  # progression du chargement
calendrierGeneral <- read.table(file="C:/PAMPA/Donnees_Usages/CalendrierGeneral.txt", sep="\t", dec=".",header=T)
    if (dim(calendrierGeneral)[2] != 10){
       infoLoading.f(msg=paste("ATTENTION, votre fichier 'Calendrier général' comporte ", dim(calendrierGeneral)[2], " champs au lieu de 10. 
                          Fermez la fenêtre de chargement , corrigez votre fichier et recommencez l'importation.", sep=""), 
                    icon="warning")
      rm(calendrierGeneral)
      infoLoading.f(button=TRUE)
    }
colnames(calendrierGeneral) <- c("AnneeCalendaire" , "periodEchant" , "semestre" , "trimestre" , 
                                  "mois" , "moisAn" , "nbJS" , "nbJW" , "pdsJS" , "pdsJW")
                                     
### importation du référentiel espèces
  stepInnerProgressBar.f(n=1, msg="Chargement du référentiel espèces")  # progression du chargement
  
  refEspeces=read.table(file="C:/PAMPA/Donnees_Usages/refEspeces.txt", sep="\t", dec=".", header=T)
  # 125 colonnes
  #Vérification du nombre de colonnes:
    if (dim(refEspeces)[2] != 125){
       infoLoading.f(msg=paste("ATTENTION, votre fichier 'Référentiel espèces' comporte ", dim(refEspeces)[2], " champs au lieu de 125. 
                          Fermez la fenêtre de chargement , corrigez votre fichier et recommencez l'importation.", sep=""), 
                    icon="warning")
      rm(refEspeces)
      infoLoading.f(button=TRUE)
    }
  # renomination des colonnes du référentiel espèces
    colnames(refEspeces)=c("codeSp", "GroupeSIH", "CodeSIH", "IssCaap", "TaxoCode", "codeFAO", "codeFB", "phylum", 
                            "categBent", "classe", "ordre", "famille", "genre", "espece", "identifiant", "ObsNC", 
                            "ObsRUN", "ObsMAY", "ObsSTM", "ObsCB", "ObsBA", "ObsBO", "ObsCR", "tailleMax", "L50", 
                            "cryptique", "mobilite", "territorial", "nocturne", "comportementGrp", "agregSaison", 
                            "positionColEau", "strategieDemo", "TypePonte", "HabitatPref", "ChangementSexe", "regimAlim",
                            "interetChasseNC", "interetChasseRUN", "interetChasseMAY", "interetChasseSTM", "interetChasseCB", 
                            "interetChasseBA", "interetChasseBO", "interetChasseCR", "interetLigneNC", "interetLigneRUN", 
                            "interetLigneMAY", "interetLigneSTM", "interetLigneCB", "interetLigneBA", "interetLigneBO", 
                            "interetLigneCR", "interetFiletNC", "interetFiletRUN", "interetFiletMAY", "interetFiletSTM", 
                            "interetFiletCB", "interetFiletBA", "interetFiletBO", "interetFiletCR", "interetCasierNC", 
                            "interetCasierRUN", "interetCasierMAY", "interetCasierSTM", "interetCasierCB", "interetCasierBA", 
                            "interetCasierBO", "interetCasierCR", "interetPiedNC", "interetPiedRUN", "interetPiedMAY", 
                            "interetPiedSTM", "interetPiedCB", "interetPiedBA", "interetPiedBO", "interetPiedCR", "interetComMAY",
                            "CoeffAMed", "CoeffBMed", "CoeffANC", "CoeffAMAY", "CoeffBNC", "CoeffBMAY", "pdsMoyPetits", 
                            "pdsMoyMoyens", "pdsMoyGros", "tailleMaxPetits", "tailleMaxMoyens", "niveauABMed", "niveauABNC", 
                            "niveauABMAY", "emblematiqueNC", "emblematiqueRUN", "emblematiqueMAY", "emblematiqueSTM", 
                            "emblematiqueCB", "emblematiqueBA", "emblematiqueBO", "emblematiqueCR", "statIUCN", "autreStatutNC", 
                            "autreStatutRUN", "autreStatutMAY", "autreStatutSTM", "autreStatutCB", "autreStatutBA", "autreStatutBO", 
                            "autreStatutCR", "etatPopLocalNC", "etatPopLocalRUN", "etatPopLocalMAY", "etatPopLocalSTM", "etatPopLocalCB", 
                            "etatPopLocalBA", "etatPopLocalBO", "etatPopLocalCR", "endemiqueNC", "endemiqueRUN", "endemiqueMAY", 
                            "endemiqueSTM", "endemiqueCB", "endemiqueBA", "endemiqueBO", "endemiqueCR")  
  # remplacement des -999 en NA
  refEspeces[refEspeces=="-999"]<-NA

  
### importation du référentiel spatial de l'AMP
  stepInnerProgressBar.f(n=1, msg="Chargement du référentiel spatial")  # progression du chargement
  
  refSpatial = read.table(file="C:/PAMPA/Donnees_Usages/refSpatial.txt", header=T, sep="\t", dec=".")
  # 15 colonnes
  #Vérification du nombre de colonnes:
    if (dim(refSpatial)[2] != 15){
        infoLoading.f(msg=paste("ATTENTION, votre fichier 'Référentiel spatial' comporte ", dim(refSpatial)[2], " champs au lieu de 15. 
                          Fermez la fenêtre de chargement , corrigez votre fichier et recommencez l'importation.", sep=""), 
                      icon="warning")
      rm(refSpatial)
      infoLoading.f(button=TRUE)
    }
  # renomination des colonnes du référentiel spatial
  colnames(refSpatial)<-c("codeZone", "zone", "AMP", "site", "station", "groupe", "longitude", "latitude", "surface", "lineaireCotier", "statutProtec", 
                          "zonagePeche", "codeSIH", "zonagePAMPA", "nbCM")
  # remplacement des -999 en NA
  if (nrow(refSpatial)!=0) {
    refSpatial[refSpatial==-999]<-NA
    # couleur pour distinguer les différents statuts de protection dans les représentations graphiques
    refSpatial$color <- "yellow"
    refSpatial$color [which(refSpatial$zonagePAMPA=="Z1")] <- "red"
    refSpatial$color [which(refSpatial$zonagePAMPA=="Z2")] <- "orange"
  }

  ### importation du référentiel engin
  stepInnerProgressBar.f(n=1, msg="Chargement du référentiel engin")  # progression du chargement
  
  refEngin = read.table(file="C:/PAMPA/Donnees_Usages/refEngin.txt", header=T, sep="\t", dec=".")
  # 15 colonnes
  #Vérification du nombre de colonnes:
    if (dim(refEngin)[2] != 9){
        infoLoading.f(msg=paste("ATTENTION, votre fichier 'Référentiel Engin' comporte ", dim(refEngin)[2], " champs au lieu de 9. 
                                Fermez la fenêtre de chargement , corrigez votre fichier et recommencez l'importation.", sep=""), 
                      icon="warning")
      rm(refEngin)
      infoLoading.f(button=TRUE)
    }
  # renomination des colonnes du référentiel spatial
  colnames(refEngin)<-c("codeEngin", "actPeche", "engin", "technique", "type", "libelSIH", "codeSIH", "IdSIH", "codeFAO")
  # remplacement des -999 en NA
    refEngin[refEngin==-999]<-NA


###### import des données de fréquentation  
  stepInnerProgressBar.f(n=1, msg="Chargement des données de fréquentation")  # progression du chargement
  
  freqtot=read.table(file="C:/PAMPA/Donnees_Usages/FREQUENTATION_export.txt", sep="\t", dec=".", header=T)
  # 35 colonnes
  #Vérification du nombre de colonnes:
  if (dim(freqtot)[2] != 35){
       infoLoading.f(msg=paste("ATTENTION, votre fichier 'Fréquentation' comporte ", dim(freqtot)[2], " champs au lieu de 35. 
                        Fermez la fenêtre de chargement , corrigez votre fichier et recommencez l'importation.", sep=""), 
                    icon="warning")
    rm(freqtot)
    infoLoading.f(button=TRUE)
  }
  # renomination des colonnes du tableau de fréquentation
  colnames(freqtot)<-c("AMP", "numSortie", "nbMoyen", "periodEchant", "annee", "mois", "jour", "saison", "typeJ", "heure", "meteo", 
                        "nebulosite", "directionVent", "forceVent", "etatMer", "lune",  "zone", "zonagePAMPA", "groupe", "typeBat", 
                        "tailleBat", "immat", "nbPers", "nbLigne", "mouillage", "natureFond", "latitude", "longitude", "act1", 
                        "categAct1", "act2", "categAct2", "questInfo", "sens1", "sens2")
  # remplacement des -999 en NA
  if (nrow(freqtot)!=0) {
    freqtot[freqtot==-999]<-NA
    freqtot$latitude[freqtot$latitude==0]<-NA
    freqtot$longitude[freqtot$longitude==0]<-NA

    # rajout d'un champ pour compiler les périodes d'échantillonnage
    if (length(unique(freqtot$periodEchant)) == 1 ) {
        freqtot$periodEchantCouplee <- freqtot$periodEchant
    } else {
        freqtot$periodEchantCouplee <- paste(sort(unique(freqtot$periodEchant))[1], "_", 
                                               sort(unique(freqtot$periodEchant))[length(sort(unique(freqtot$periodEchant)))], 
                                               sep="")
    }
         
    # rajout d'une colonne pour le calcul du nombre de bateaux (nombre au lieu de caractère)
    freqtot$nbBat<-1
    freqtot$nbBat[is.na(freqtot$immat)==TRUE]<-0
   
    # R ne reconnait pas les tailles <5m et >10m, remplacement par inf et sup
    levels(freqtot$tailleBat) <- c(levels(freqtot$tailleBat),"inf5m", "sup10m")
    freqtot$tailleBat[which(is.element(freqtot$tailleBat, c("<5m")))] <- "inf5m"
    freqtot$tailleBat[which(is.element(freqtot$tailleBat, c(">10m")))] <- "sup10m"
          
    # rajout d'une colonne pour la simplification des types de jour
    freqtot$typeJsimp <- "JS"
    freqtot$typeJsimp[which(is.element(freqtot$typeJ, c("PONT", "JW", "JW+VAC", "JF")))] <- "JW"
   
    # rajout d'une colonne trimestre
    freqtot$trimestre <- "mar_avril_mai"
    freqtot$trimestre[which(is.element(freqtot$mois, c(6, 7, 8)))] <- "juin_juil_aout"
    freqtot$trimestre[which(is.element(freqtot$mois, c(9, 10, 11)))] <- "sept_oct_nov"
    freqtot$trimestre[which(is.element(freqtot$mois, c(12, 1, 2)))] <- "dec_janv_fev"

  # rajout d'une colonne semestre
    freqtot$semestre <- "mar_aout"
    freqtot$semestre[which(is.element(freqtot$mois, c(9, 10, 11, 12, 1, 2)))] <- "sep_fev"

    
    ## lorsque le nb de personnes à bord est inconnu, il est estimé à la moyenne du nb de personnes à bord pour cette activité
    freqBat <- subset(freqtot, freqtot$immat!="NA")           
    nbPersMoyen <- tapply(freqBat$nbPers, freqBat$act1, mean, na.rm=T)
    freqtot$nbPersMoy <- nbPersMoyen[match(freqtot$act1, names(nbPersMoyen))]
    freqtot$nbPers [is.na(freqtot$nbPers)] <- freqtot$nbPersMoy[is.na(freqtot$nbPers)]
     
    
    #colonne nb-bat et nb-pers pour les activités doubles
    freqtot$nbPersAct<-freqtot$nbPers
    freqtot$nbPersAct[!is.na(freqtot$act1) & !is.na(freqtot$act2)] <- freqtot$nbPersAct[!is.na(freqtot$act1) & !is.na(freqtot$act2)]/2 # à vérifier sur un fichier avec doubles activités
    freqtot$nbBatAct<-freqtot$nbBat
    freqtot$nbBatAct[!is.na(freqtot$act1) & !is.na(freqtot$act2)] <- freqtot$nbBatAct[!is.na(freqtot$act1) & !is.na(freqtot$act2)]/2 # à vérifier sur un fichier avec doubles activités    

    # cas spécifique de Banyuls
    if (siteEtudie=="BA") {
      for (i in 1 : nrow(freqtot)) {
          freqtot$sens1[i] = unlist(strsplit(as.character(freqtot$numSortie[i]), "\\_"))[3]
          freqtot$sens2[i] = paste(unlist(strsplit(as.character(freqtot$numSortie[i]), "\\_"))[1], "_", 
                                       unlist(strsplit(as.character(freqtot$numSortie[i]), "\\_"))[2], sep="")
      }
      freqtotBA <- freqtot[1,]
      freqtotBA <- rbind (freqtotBA, freqtot[!is.element (freqtot$sens1, c("am", "pm")) ,])
      
      freqtotam <- subset(freqtot, freqtot$sens1=="am") 
      freqtotam1 <- freqtotam[is.element(freqtotam$act1, c("ACTTou", "CHAL", "CR", "ED", "EXCU", "JS", "PE", "PL", "PLCom", "PM", "POC", "PRO", "PS", NA) ),]
      
      freqtotpm <- subset(freqtot, freqtot$sens1=="pm") 
      freqtotpm1 <- freqtotam[is.element(freqtotam$act1, c("AUC", "BA", "CA", "CH", "KY", NA, "PB", "KS", "PI", "PR", "PV", "RE", "SF", "SN", "SSM") ),]
      
      freqtot <- rbind (freqtotBA, freqtotam1, freqtotpm1)
      levels(freqtot$numSortie) <- c(levels(freqtot$numSortie),levels(freqtot$sens2))
      freqtot$numSortie <- freqtot$sens2
      freqtot <- freqtot[-1,]
    }
    




    # cas spécifique de La Réunion
    if (siteEtudie=="RUN") {                                                        
      for (i in 1 : nrow(freqtot)){
        if (freqtot$act1[i]=="PE" | freqtot$act1[i]=="PL" | freqtot$act1[i]=="ACTTou" | freqtot$act1[i]=="PS" | freqtot$act1[i]=="JS"){
          freqtot$nbPersAct[i]=NA
          freqtot$nbBatAct[i]=freqtot$nbPers[i]
          freqtot$nbBat[i]=freqtot$nbPers[i]
          freqtot$nbPers[i]=NA
        }
        if (freqtot$act1[i]=="BA" | freqtot$act1[i]=="PR" | freqtot$act1[i]=="CH" | freqtot$act1[i]=="PP" | freqtot$act1[i]=="SF" | freqtot$act1[i]=="PV" | freqtot$act1[i]=="KS" | freqtot$act1[i]=="SN" ){
          freqtot$nbPersAct[i]=freqtot$nbPers[i]
          freqtot$nbBatAct[i]=0
          freqtot$nbBat[i]=0
        }
        if (freqtot$act1[i]=="AUC") {
          freqtot$nbPersAct[i]=0
          freqtot$nbBatAct[i]=0
        }
      }
    }
    
    # transformation des mois en format date
    moisDates<-paste("01", freqtot$mois, freqtot$annee, sep="-")         
    freqtot$moisAn<-format(as.Date(moisDates, "%d-%m-%Y"), "%Y/%m")
#    freqtot$moisAn<-paste(freqtot$annee, "_", freqtot$mois, sep="")
  
    # tableau du nombre de bateaux enregistrés sur l'ensemble des sorties
    freqNonNulleB <- subset(freqtot, freqtot$immat!="NA")
    freqTot <- dim(freqNonNulleB)[1]
    freqTotPeriodB <- as.data.frame(table(freqNonNulleB$periodEchant))
    freqTotPeriodP <- tapply(freqtot$nbPers, freqtot$periodEchant, sum, na.rm=T)
    freqTotPeriod <- cbind (freqTotPeriodB, freqTotPeriodP)
    colnames(freqTotPeriod) <- c("période", "nb de bateaux", "nb de personnes")
    write.csv(freqTotPeriod, file="C:/PAMPA/Resultats_Usages/recensement fréquentation total.csv", row.names = F)
  }

  
###### import des données des questionnaires de pêche récréative
  stepInnerProgressBar.f(n=1, msg="Chargement des enquêtes de pêche récréative")  # progression du chargement
  
  peche=read.table(file="C:/PAMPA/Donnees_Usages/ENQUETE PECHE_export.txt", sep="\t", dec=".", header=T)
  # 115 colonnes
  #Vérification du nombre de colonnes:
    if (dim(peche)[2] != 116){
       infoLoading.f(msg=paste("ATTENTION, votre fichier 'Pêche' comporte ", dim(peche)[2], " champs au lieu de 116. 
        Fermez la fenêtre de chargement , corrigez votre fichier et recommencez l'importation.", sep=""), icon="warning")
      rm(peche)
      infoLoading.f(button=TRUE)
    }
  # renomination des colonnes du tableau de questionnaires de pêche
  colnames(peche)<-c("AMP", "quest", "numSortie", "periodEchant", "annee", "mois", "jour", "saison", "typeJ", "heure", "zone", 
                      "nomZone", "zonagePAMPA", "groupe", "meteo", "etatMer", "lune",  "directionVent", "forceVent", "nebulosite", 
                      "latitude", "longitude", "tailleBat", "categBat", "typeBat", "mouillage", "refus", "dejaEnq", "questComp", 
                      "actPeche1", "actPeche2", "lieuDepart",  "nomDepart", "nomDepartTot", "nbPecheur", "zone1", "zone2", 
                      "engin1", "nbEngin1", "engin2", "nbEngin2", "engin3", "nbEngin3", "debutPec", "finPec", "dureeSortie", "dureePec", 
                      "capture", "pratique", "anciennete", "planifAn", "planifSemaine", "planifJour", "planifEnv1", "planifEnv2", 
                      "actHab1", "enginHab1a", "enginHab1b", "nbSortie1", "actHab2", "enginHab2a", "enginHab2b", "nbSortie2", 
                      "nbSortieTot", "partAMP", "dureePecMoy", "choixSite1", "choixSite2", "choixSite3", "raisonPeche1", "raisonPeche2", 
                      "catCaptAn", "evolNb", "evolTaille", "evolDiv", "budgetAn", "existenceAMP", "influAMP", "defAMP", "connaisRegAMP", 
                      "connaisRegPec",  "suffiInfo", "adaptReg", "respectReg", "assoProces", "effetEcosyst", "effetEcono", "effetAct", 
                      "relationPP", "relationPA", "relationCH", "relationPL",  "relationPS", "relationJS", "relationKS", "avisAMP", 
                      "connaisCM", "utilCM", "sexe", "anNais", "CSP", "pays", "codResid", "residence", "commune", "resident", "nbNuit", 
                      "nbPersVoy", "depTot", "premVisi", "anNb", "nbVisitAn", "revenir", "influAct", "recontact", "resultat")

  
  pecheQ <- peche # pour les sites n'ayant pas d'enquêtes de pêche, c'est cette table qui est prise dans tousQuest
  if (nrow(peche)!=0) {
  ## rajout d'une colonne pour spécifier l'activité
    peche$toutConfondu <- rep("peche", nrow(peche))
  
  # rajout d'un champ pour compiler les périodes d'échantillonnage
    if (length(unique(peche$periodEchant)) == 1 ) {
        peche$periodEchantCouplee <- peche$periodEchant
    } else {
        peche$periodEchantCouplee <- paste(sort(unique(peche$periodEchant))[1], "_", 
                                               sort(unique(peche$periodEchant))[length(sort(unique(peche$periodEchant)))], 
                                               sep="")
    } 
      
    peche[peche==-999]<-NA   
    peche$latitude[peche$latitude==0]<-NA
    peche$longitude[peche$longitude==0]<-NA 
    levels(peche$resident)=c(levels(peche$resident), "non-resident", "resident")
    peche$resident[peche$resident=="0"]<-"non-resident"
    peche$resident[peche$resident=="1"]<-"resident"
    peche$resident[peche$resident=="non"]<-"non-resident"
    peche$resident[peche$resident=="oui"]<-"resident"  
    peche$heure[peche$heure=="00h00"]<-NA         # spécificité access
    peche$debutPec[peche$debutPec=="00h00"]<-NA   # spécificité access
    pecheE<-subset(peche, peche$refus=="non")          # pecheQ sert pour les métriques d'opinion, pratique et socio-éco 
    pecheQ<-subset(pecheE, pecheE$dejaEnq=="non")     #  donc retrait des questionnaires qui n'ont pas ces informations

    if (siteEtudie == "BA"){                         # spécificité Banyuls, les enquêtes terrain, n'ont pas les questions d'opinion
      pecheQu<-rbind(subset(pecheQ, pecheQ$questComp == "distribue"), subset(pecheQ, pecheQ$questComp == "distribue_terrain"))
      pecheQ<-pecheQu
    }
#    pecheNC <- subset(pecheQ, pecheQ$questComp=="oui")      # revoir cas de la NC (pls découpages, rajout de questions au cours du temps)
  }


###### import des données des captures du jour
  stepInnerProgressBar.f(n=1, msg="Chargement des données de captures")  # progression du chargement
  
  captures=read.table(file="C:/PAMPA/Donnees_Usages/CAPTURES_export.txt", sep="\t", dec=".", header=T)
  #34 colonnes
  #Vérification du nombre de colonnes:
  if (dim(captures)[2] != 34){
       infoLoading.f(msg=paste("ATTENTION, votre fichier 'Captures' comporte ", dim(captures)[2], " champs au lieu de 34. 
        Fermez la fenêtre de chargement , corrigez votre fichier et recommencez l'importation.", sep=""), icon="warning")
    rm(captures)
    infoLoading.f(button=TRUE)
  }
  # renomination des colonnes du tableau des captures du jour
  colnames(captures)<-c("quest", "numSortie", "periodEchant", "annee", "mois", "saison", "typeJ", "meteo", "nebulosite", 
                        "directionVent", "forceVent", "etatMer", "lune", "zone", "zonagePAMPA", "latitude", "longitude", "eval", 
                        "codeSp", "identifiant", "phylum", "famille", "genre", "espece", "nb", "tailleEstim", "tailleMesu", 
                        "pdsEstim", "pdsMesu", "codeEngin", "engin", "type", "enginType", "act_peche")
  # remplacement des -999 en NA
  if (nrow(captures)!=0) {
    captures$latitude[captures$latitude==0]<-NA
    captures$longitude[captures$longitude==0]<-NA
    captures[captures==-999]<-NA
  
  # rajout d'un champ AMP
    captures$AMP<-rep(siteEtudie, nrow(captures))
  
    
  # rajout d'un champ pour compiler les périodes d'échantillonnage
    if (length(unique(captures$periodEchant)) == 1 ) {
        captures$periodEchantCouplee <- captures$periodEchant
    } else {
        captures$periodEchantCouplee <- paste(sort(unique(captures$periodEchant))[1], "_", 
                                               sort(unique(captures$periodEchant))[length(sort(unique(captures$periodEchant)))], 
                                               sep="")
    } 
     
  # rajout d'une colonne pour la simplification des types de jour
    captures$typeJsimp<-captures$typeJ
    captures$typeJsimp[which(captures$typeJ=="VAC")]="JS"
    captures$typeJsimp[which(captures$typeJ=="JW+VAC")]="JW"
    captures$typeJsimp[which(captures$typeJ=="PONT")]="JW"
    captures$typeJsimp[which(captures$typeJ=="JF")]="JW"
   
   # rajout d'une colonne trimestre
    captures$trimestre <- "mar_avril_mai"
    captures$trimestre[which(is.element(captures$mois, c(6, 7, 8)))]<-"juin_juil_aout"
    captures$trimestre[which(is.element(captures$mois, c(9, 10, 11)))]<-"sept_oct_nov"
    captures$trimestre[which(is.element(captures$mois, c(12, 1, 2)))]<-"dec_janv_fev"

  # rajout d'une colonne semestre
    captures$semestre <- "mar_aout"
    captures$semestre[which(is.element(captures$mois, c(9, 10, 11, 12, 1, 2)))]<-"sep_fev"
  
  # calcul du poids à partir des relations taille/poids
  captures$CoeffB[is.element(captures$AMP, c("BA" , "BO" , "CB" , "CR"))] <- refEspeces$CoeffBMed[match(captures$codeSp[is.element(captures$AMP, c("BA" , "BO" , "CB" , "CR"))], 
                                                                                                        refEspeces$codeSp)]
  captures$CoeffA[is.element(captures$AMP, c("BA" , "BO" , "CB" , "CR"))] <- refEspeces$CoeffAMed[match(captures$codeSp[is.element(captures$AMP, c("BA" , "BO" , "CB" , "CR"))], 
                                                                                                        refEspeces$codeSp)]
  
  captures$CoeffB[is.element(captures$AMP, c("MAY", "RUN"))] <- refEspeces$CoeffBMAY[match(captures$codeSp[is.element(captures$AMP, c("MAY", "RUN"))], 
                                                                                          refEspeces$codeSp)]
  captures$CoeffA[is.element(captures$AMP, c("MAY", "RUN"))] <- refEspeces$CoeffAMAY[match(captures$codeSp[is.element(captures$AMP, c("MAY", "RUN"))], 
                                                                                          refEspeces$codeSp)]
 
  captures$CoeffB[is.element(captures$AMP, c("NC"))] <- refEspeces$CoeffBNC[match(captures$codeSp[is.element(captures$AMP, c("NC"))], 
                                                                                  refEspeces$codeSp)]
  captures$CoeffA[is.element(captures$AMP, c("NC"))] <- refEspeces$CoeffANC[match(captures$codeSp[is.element(captures$AMP, c("NC"))], 
                                                                                  refEspeces$codeSp)]
 
  captures$pdsMesu2 <- captures$pdsMesu
  captures$pdsMesu2 [is.na(captures$pdsMesu2)] <- captures$pdsEstim [is.na(captures$pdsMesu2)] 
  captures$pdsMesu2 [ is.na(captures$pdsMesu2)] <- (captures$tailleMesu [is.na(captures$pdsMesu2)] ^ captures$CoeffB [is.na(captures$pdsMesu2)]) * captures$CoeffA [is.na(captures$pdsMesu2)]      
  
    
  # rajout de la colonne groupe
  captures$groupe<-refSpatial$groupe[match(captures$zone, refSpatial$codeZone)]

# tableau du nombre total de captures
  capturesTot <- as.data.frame(tapply (captures$nb, captures$periodEchant, sum, na.rm=T))
  capturesPds <- as.data.frame(tapply (captures$pdsMesu, captures$periodEchant, sum, na.rm=T))
  capturesTot <- cbind(capturesTot, capturesPds)
  names(capturesTot) <- c("nb total de captures", "pds total mesuré de captures")
  write.csv(capturesTot, file="C:/PAMPA/Resultats_Usages/capture totale.csv", row.names = T)
}      


###### import des données des captures annuelles
  stepInnerProgressBar.f(n=1, msg="Chargement des données de captures annuelles")  # progression du chargement
  
  capturesAn=read.table(file="C:/PAMPA/Donnees_Usages/CAPTURES ANNUELLES_export.txt", sep="\t", dec=".", header=T)
  # 29colonnes
  #Vérification du nombre de colonnes:
  if (dim(capturesAn)[2] != 29){
       infoLoading.f(msg=paste("ATTENTION, votre fichier 'Captures annuelles' comporte ", dim(capturesAn)[2], " champs au lieu de 29. 
        Fermez la fenêtre de chargement , corrigez votre fichier et recommencez l'importation.", sep=""), icon="warning")
    rm(capturesAn)
    infoLoading.f(button=TRUE)
  }
  # renomination des colonnes du tableau des captures annuelles
  colnames(capturesAn)<-c("AMP", "quest", "numSortie", "periodEchant", "spAn1", "phylum1", "famille1", "genre1", "espece1", "spAn2", 
                          "phylum2", "famille2", "genre2", "espece2", "spAn3", "phylum3", "famille3", "genre3", "espece3", "spAn4", 
                          "phylum4", "famille4", "genre4", "espece4", "spAn5", "phylum5", "famille5", "genre5", "espece5")

  if (nrow(capturesAn)!=0) {
    # rajout d'un champ pour compiler les périodes d'échantillonnage
    if (length(unique(capturesAn$periodEchant)) == 1 ) {
        capturesAn$periodEchantCouplee <- capturesAn$periodEchant
    } else {
        capturesAn$periodEchantCouplee <- paste(sort(unique(capturesAn$periodEchant))[1], "_", 
                                               sort(unique(capturesAn$periodEchant))[length(sort(unique(capturesAn$periodEchant)))], 
                                               sep="")
    } 
  }
     

###### import des données des questionnaires de plaisance
  stepInnerProgressBar.f(n=1, msg="Chargement des enquêtes de plaisance")  # progression du chargement
  
  plaisance=read.table(file="C:/PAMPA/Donnees_Usages/ENQUETE PLAISANCE_export.txt", sep="\t", dec=".", header=T)
  # 77colonnes
  #Vérification du nombre de colonnes:
    if (dim(plaisance)[2] != 77){
       infoLoading.f(msg=paste("ATTENTION, votre fichier 'Plaisance' comporte ", dim(plaisance)[2], " champs au lieu de 77. 
        Fermez la fenêtre de chargement , corrigez votre fichier et recommencez l'importation.", sep=""), icon="warning")
    rm(plaisance)
    infoLoading.f(button=TRUE)
    }
  # renomination des colonnes
  colnames(plaisance)<-c("quest", "numSortie", "periodEchant", "annee", "mois", "jour", "saison", "typeJ", "directionVent", "forceVent", 
                          "heure", "zone", "typeBat", "tailleBat", "nbPers", "mouillage", "natureFond", "lieuDepart", "nomDepart", 
                          "nomDepartTot", "dureeSortie", "lieuNuit", "act1", "categAct1", "act2", "categAct2", "choixSite1", "choixSite2", 
                          "existenceAMP", "influAMP", "defAMP", "connaisRegAMP", "adaptReg", "respectReg", "visiteAMP", "premVisi", 
                          "partAMP", "nbVisitAn", "interetAMP1", "interetAMP2", "amenagements", "utilBBQ", "alimBBQ", "panneaux", 
                          "connaisCM", "utilCM", "suffiCM", "suffiInfo", "effetEcosyst", "effetEcono", "effetAct", "assoProces", 
                          "effetNourrissage", "relationPP", "relationPA", "relationCH", "relationPL", "relationPS", "relationJS", 
                          "relationKS", "avisAMP", "recontact", "resultat", "sexe", "anNais", "CSP", "pays", "codResid", "residence", 
                          "commune", "resident", "transport", "nbNuit", "depTot", "logement", "revenir", "anNb")
  
  # remplacer les -999 par NA
  if (nrow(plaisance)!=0) {
    plaisance[plaisance==-999]<-NA
    levels(plaisance$resident)=c(levels(plaisance$resident), "non-resident", "resident")
    plaisance$resident[plaisance$resident=="0"]<-"non-resident"
    plaisance$resident[plaisance$resident=="1"]<-"resident"
    plaisance$resident[plaisance$resident=="non"]<-"non-resident"
    plaisance$resident[plaisance$resident=="oui"]<-"resident"
    
    ## rajout d'une colonne pour spécifier l'activité
    plaisance$toutConfondu <- rep("plaisance", nrow(plaisance))

    # rajout d'un champ pour compiler les périodes d'échantillonnage
    if (length(unique(plaisance$periodEchant)) == 1 ) {
        plaisance$periodEchantCouplee <- plaisance$periodEchant
    } else {
        plaisance$periodEchantCouplee <- paste(sort(unique(plaisance$periodEchant))[1], "_", 
                                               sort(unique(plaisance$periodEchant))[length(sort(unique(plaisance$periodEchant)))], 
                                               sep="")
    } 

  }


###### import des données des questionnaires plongée
  stepInnerProgressBar.f(n=1, msg="Chargement des enquêtes de plongée")  # progression du chargement
  
  plongee=read.table(file="C:/PAMPA/Donnees_Usages/ENQUETE PLONGEE_export.txt", sep="\t", dec=".", header=T)
  # 70 colonnes
  #Vérification du nombre de colonnes:
  if (dim(plongee)[2] != 70){
       infoLoading.f(msg=paste("ATTENTION, votre fichier 'Plongée' comporte ", dim(plongee)[2], " champs au lieu de 70. 
        Fermez la fenêtre de chargement , corrigez votre fichier et recommencez l'importation.", sep=""), icon="warning")
    rm(plongee)
    infoLoading.f(button=TRUE)
  }
  # renomination des colonnes du tableau de plongée
  colnames(plongee)<-c("quest", "periodEchant", "AMP", "enqueteur", "pratique", "nbAn", "cadre", "anciennete", "budgetAn", 
                        "choixSite1", "choixSite2", "choixSite3", "plongeePerturb", "raison1", "raison2", "existenceAMP", 
                        "premPlongee", "anNb", "nbVisitAn", "site1", "site2", "site3", "satisf", "chgmt", "chgmt1", "chgmt2", 
                        "influAMP", "revenir", "nuisance1", "nuisance2", "preInfo1", "preInfo2", "informateur", "actions", 
                        "suffiInfo", "adaptReg", "respectReg", "effetEcosyst", "effetEcosystB", "effetAct", "effetEcono", 
                        "assoProces", "relationPP", "relationPA", "relationCH", "relationPL", "relationPS", "relationJS", 
                        "relationKS", "relationAU", "raisonConflits", "sexe", "anNais", "pays", "region", "commune", "resident", 
                        "menage", "revenu", "chasse", "CSP", "influAct", "nbPlSite", "nbPLSitePasse", "nbNuit", "logement", 
                        "transport", "nbPersVoy", "depTot", "suggestions")                                                                 
  
  # remplacement des -999 en NA
  if (nrow(plongee)!=0) {
    plongee[plongee==-999]<-NA 
    levels(plongee$resident)=c(levels(plongee$resident), "non-resident", "resident")
    plongee$resident<-as.character(plongee$resident)
    plongee$resident[plongee$resident=="0"]<-"non-resident"
    plongee$resident[plongee$resident=="1"]<-"resident"
    plongee$resident[plongee$resident=="non"]<-"non-resident"
    plongee$resident[plongee$resident=="oui"]<-"resident"
  
  ## rajout d'une colonne pour spécifier l'activité
    plongee$toutConfondu <- rep("plongee", nrow(plongee))

    # rajout d'un champ pour compiler les périodes d'échantillonnage
    if (length(unique(plongee$periodEchant)) == 1 ) {
        plongee$periodEchantCouplee <- plongee$periodEchant
    } else {
        plongee$periodEchantCouplee <- paste(sort(unique(plongee$periodEchant))[1], "_", 
                                               sort(unique(plongee$periodEchant))[length(sort(unique(plongee$periodEchant)))], 
                                               sep="")

    } 
  }


###### import des données des questionnaires d'excursion
  stepInnerProgressBar.f(n=1, msg="Chargement des enquêtes d'excursion-PMT")  # progression du chargement
  
  excursion=read.table(file="C:/PAMPA/Donnees_Usages/ENQUETES EXCURSION-PMT_export.txt", sep="\t", dec=".", header=T)
  # 70 colonnes
  #Vérification du nombre de colonnes:
  if (dim(excursion)[2] != 53){
       infoLoading.f(msg=paste("ATTENTION, votre fichier 'Excursion' comporte ", dim(excursion)[2], " champs au lieu de 53. 
        Fermez la fenêtre de chargement , corrigez votre fichier et recommencez l'importation.", sep=""), icon="warning")
    rm(excursion)
    infoLoading.f(button=TRUE)
  }
  # renomination des colonnes du tableau de plongée
  colnames(excursion)<-c("quest", "periodEchant", "AMP", "enqueteur", "premVisi", "anciennete", "nbVisitAn", "revenir", 
                          "act1", "act2", "choixSite1", "choixSite2", "influSSM", "existenceSSM", "parcoursGuide", 
                          "degreSatisf", "satisfaction1", "satisfaction2", "satisfaction3", "satisfaction4", "satisfaction5", 
                          "toucher", "PMTperturb", "amelioration", "connaisCM", "utilCM", "suffiCM", "existenceAMP", 
                          "connaisRegAMP", "defAMP", "visiAutreAMP", "effetEcosyst", "effetNourrissage", "avisAMP", "influAMP", 
                          "amenagements", "panneaux", "suffiInfo", "sexe", "anNais", "resident", "pays", "region", "commune", "menage", 
                          "CSP", "revenu", "influAct", "logement", "transport", "nbNuit", "nbPersVoy", "depTot")

  # remplacement des -999 en NA
  if (nrow(excursion)!=0) {
    excursion[excursion==-999]<-NA
    levels(excursion$resident)=c(levels(excursion$resident), "non-resident", "resident")
    excursion$resident<-as.character(excursion$resident)
    excursion$resident[excursion$resident=="non"]<-"non-resident"
    excursion$resident[excursion$resident=="oui"]<-"resident"

  ## rajout d'une colonne pour spécifier l'activité
    excursion$toutConfondu <- rep("excursion", nrow(excursion))

    # rajout d'un champ pour compiler les périodes d'échantillonnage
    if (length(unique(excursion$periodEchant)) == 1 ) {
        excursion$periodEchantCouplee <- excursion$periodEchant
    } else {
        excursion$periodEchantCouplee <- paste(sort(unique(excursion$periodEchant))[1], "_", 
                                               sort(unique(excursion$periodEchant))[length(sort(unique(excursion$periodEchant)))], 
                                               sep="")
    } 
  }


### import du fond de carte
#  carte=read.shape("C:/PAMPA/Donnees_Usages/carte/cartefile")     # à revoir avec les nouvelles versions de R
stepInnerProgressBar.f(n=1, msg="Données chargées")  # progression du chargement
infoLoading.f(msg="Les fichiers de données ont bien été chargés !
    REMARQUE : lorsque le nombre de personnes était inconnu, il a été estimé par
    le nombre moyen de personnes pratiquant cette activité à bord d'un bateau.", icon="info")


# Appel du script avec les fonctions et calculs préliminaires obligatoires
source("C:/PAMPA/Scripts_Usages/FonctionsGenerales.r")

################################################################################



