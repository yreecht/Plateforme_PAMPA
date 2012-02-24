################################################################################
# Nom               : Fonctions et calculs obligatoires
# Type              : Programme
# Objet             : Ce programme contient Fonctions génériques nécessaires pour le calcul des métriques.
#                     Il réalise également les calculs génériques nécessaires pour le calcul des métriques.
# Input             : TXT
# Output            : Data
# Auteur            : Elodie GAMP
# R version         : 2.11.1
# Date de création  : Avril 2011
# Sources
################################################################################

stepInnerProgressBar.f(n=1, msg="Chargement des fonctions obligatoires")  # progression du chargement

#### fonctions nécessaires pour le script  

########################################################################################################################
dropLevels.f <- function(df, which=NULL)
{
    ## Purpose: Supprimer les 'levels' non utilisés des facteurs d'un
    ##          data.frame.
    ## ----------------------------------------------------------------------
    ## Arguments: df : une data.frame
    ##            which : indice des colonnes à inclure (toutes par défaut).
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 10 août 2010, 13:29

    if (class(df) != "data.frame")
    {
        stop("'df' doit être une data.frame")
    }else{
        if (is.null(which))
        {
            x <- as.data.frame(sapply(df, function(x)
                                  {
                                      return(x[ ,drop=TRUE])
                                  }, simplify=FALSE),
                               stringsAsFactors=FALSE)
        }else{                          # Cas où seulement certaines colonnes sont traitées.
            x <- df

            x[ , which] <- as.data.frame(sapply(df[ , which, drop=FALSE],
                                                function(x)
                                            {
                                                return(x[ ,drop=TRUE])
                                            }, simplify=FALSE),
                                         stringsAsFactors=FALSE)
        }

        return(x)
    }
}

################################################################################

# fonction pour remplacer les valeurs par des données binaires
lengthnb.f = function(x){x=1}

# fonction pour compter les lignes sans NA
lengthnna.f = function(x){length(x[!is.na(x)])}


## message d'info de fin de programmation des fonctions
infoLoading.f(msg="Les fonctions ont bien été programmées.", icon="info")

################################################################################

stepInnerProgressBar.f(n=1, msg="Calculs et analyses préalables en cours")  # progression du chargement
#### pré analyses/calculs obligatoires pour le calcul des métriques

## mouillage des bateaux sur chacun des sites
  nbbatMouillage <- tapply(freqtot$immat, 
                           list(freqtot$numSortie, freqtot$zone, freqtot$mouillage), 
                           lengthnna.f)                                          # ligne=numSortie, col=zone, 3D=mouillage
  nbbatMouillage[which(is.na(nbbatMouillage) == "TRUE")] <- 0
  nbtotZone<-tapply(freqtot$immat, list(freqtot$numSortie, freqtot$zone), lengthnna.f)

## liste catégories d'activité
  listeCateg <- unique(levels(freqtot$categAct1), levels(freqtot$categAct2))
## liste des activités
  listeAct <- unique(levels(freqtot$act1), levels(freqtot$act2))
## liste statuts
  listeStatut <- levels(freqtot$statut)
## liste des types de jour
  listeTypeJ <- levels(freqtot$typeJ)
## liste des engins
  listeEngin <- unique(refEngin$codeEngin)
## liste des regroupement d'engins
  listeEnginGnx <- unique(refEngin$engin)

## couleur fond de carte
#  if (siteEtudie=="NC") {
#    colcarte=ifelse(carte$att.data["L1_ATTRIB"]=="oceanic", "white", "lightyellow")
#  } else {
#    colcarte="lightyellow"
#    }

## définition des intérêts halieutiques selon le site d'étude
  Interet = refEspeces[, c("codeSp", paste("interetChasse", siteEtudie, sep=""), paste("interetLigne", siteEtudie, sep=""),
                           paste("interetFilet", siteEtudie, sep=""), paste("interetCasier", siteEtudie, sep=""),
                           paste("interetPied", siteEtudie, sep=""))]
colnames(Interet)<-c("spCode", "interetCHA", "interetLG", "interetFIL", "interetCAS", "interetPP")


## préalables au calcul des CPUE
prealableCPUE.f = function () {
if (nrow(peche)!=0) {
  peche <- peche[which(is.na(peche$heure) == "FALSE"), ]   # enlève les lignes dont la durée de pêche est inconnue (début de pêche-heure enquête)
  peche <- peche[which(is.na(peche$debutPec) == "FALSE"), ]
  peche$nbEngin1[is.na(peche$nbEngin1)] <- 0
  peche$nbEngin2[is.na(peche$nbEngin2)] <- 0
  peche$nbEngin3[is.na(peche$nbEngin3)] <- 0

  nbEngin1 <- peche[, c("quest", "engin1", "nbEngin1")]
  colnames(nbEngin1) <- c("quest", "engin", "nbEngin")
  nbEngin2 <- peche[, c("quest", "engin2", "nbEngin2")]
  colnames(nbEngin2) <- c("quest", "engin", "nbEngin")
  nbEngin3 <- peche[, c("quest", "engin3", "nbEngin3")]
  colnames(nbEngin3) <-c ("quest", "engin", "nbEngin")
  nbEngin <- rbind(nbEngin1, nbEngin2, nbEngin3)

  nbEngin <- nbEngin[which(is.na(nbEngin$engin)=="FALSE"), ]
  nbEngin$code <- paste(nbEngin$quest, nbEngin$engin, sep="")
  captures$code <- paste(captures$quest, captures$codeEngin, sep="")
  captures$nbEngin <- nbEngin$nbEngin[match(captures$code, nbEngin$code)]

## Temps écoulé depuis le début de la pêche (à l'heure de l'enquête
  x.lt <- as.POSIXlt(as.character(peche$heure), format="%Hh%M")
  peche$heureEnq <- x.lt$hour + x.lt$min/60 + x.lt$sec/3600
  x.lt <- as.POSIXlt(as.character(peche$debutPec), format="%Hh%M")
  peche$heureDeb<-x.lt$hour + x.lt$min/60 + x.lt$sec/3600
  peche$dureePecEnq <- peche$heureEnq-peche$heureDeb
  peche$dureePecEnq[peche$dureePecEnq<0] <- (24-peche$heureDeb[peche$dureePecEnq<0])+peche$heureEnq[peche$dureePecEnq<0]

  captures$dureePecEnq <- peche$dureePecEnq[match(captures$quest, peche$quest)]
  captures <- captures[which(is.na(captures$nbEngin)=="FALSE"), ]
  captures <- captures[which(captures$nbEngin!=0), ]
#  captures<-captures[which(is.na(captures$nb)=="FALSE"), ]
  captures <- captures[which(captures$dureePecEnq!=0), ]
  captures$CPUEUnit <- captures$nb/(captures$nbEngin*captures$dureePecEnq)
  captures$CPUEBiomasse <- captures$pdsMesu2/(captures$nbEngin*captures$dureePecEnq)
  }
#  tkmessageBox(title="CPUE", message="Les captures pour lesquelles la durée de pêche, ou le nombre d'engins ou le nombre d'individus
#  n'est pas rensignée ne sont pas prises en compte dans le calcul des CPUE", icon="warning", type="ok")
  captures$actEngin <-paste(captures$actPeche, captures$engin)
  assign("captures2", captures, envir=.GlobalEnv)
}
prealableCPUE.f()
CAPTURES2<-captures2

################################################################################

### construction d'une table pour les questions communes des questionnaires
stepInnerProgressBar.f(n=1, msg="Construction de la table 'tousQuest'")  # progression du chargement

anneeActuelle <- as.numeric(format(Sys.Date(),"%Y"))
## recup pêche
  pecheTrans2 <- pecheQ
  pecheTrans2$activite <- rep("peche", nrow(pecheTrans2))
  pecheTrans2$activiteSpe <- pecheTrans2$actPeche1
  pecheTrans2$suffiCM <- rep(factor(NA), nrow(pecheTrans2))
  pecheTrans2$nbSortie1[is.na(pecheTrans2$nbSortie1)] <- 0
  pecheTrans2$nbSortie2[is.na(pecheTrans2$nbSortie2)] <- 0
  pecheTrans2$nbSortie <- pecheTrans2$nbSortie1 + pecheTrans2$nbSortie2
  if (siteEtudie == "CB" || siteEtudie == "RUN") {
    pecheTrans2$nbSortie <- pecheTrans2$nbSortieTot
  }
  pecheTrans <- pecheTrans2[, c("AMP", "quest", "numSortie", "activite", "activiteSpe", "periodEchant", "dureeSortie", "pratique",
                                "anciennete", "nbSortie", "partAMP", "choixSite1", "choixSite2", "choixSite3", "budgetAn",
                                "existenceAMP", "influAMP", "defAMP", "connaisRegAMP", "suffiInfo", "adaptReg", "respectReg",
                                "assoProces", "effetEcosyst", "effetEcono", "effetAct", "relationPP", "relationPA", "relationCH",
                                "relationPL", "relationPS", "relationJS", "relationKS", "avisAMP", "connaisCM", "utilCM", "suffiCM",
                                "sexe", "anNais", "CSP", "pays", "commune", "resident", "nbNuit", "nbPersVoy", "depTot", "premVisi",
                                "anNb", "nbVisitAn", "revenir", "influAct")]

## recup plaisance
plaisance2 <- plaisance
  plaisance2$AMP <- rep(siteEtudie, nrow(plaisance2))
  plaisance2$activite <- rep("plaisance", nrow(plaisance2))
  plaisance2$activiteSpe <- rep("plaisance", nrow(plaisance2))
  plaisance2$pratique <- rep(NA, nrow(plaisance2))
  plaisance2$anciennete <- rep(NA, nrow(plaisance2))
  plaisance2$nbSortie <- rep(NA, nrow(plaisance2))
  plaisance2$choixSite3 <- rep(NA, nrow(plaisance2))
  plaisance2$budgetAn <- rep(NA, nrow(plaisance2))
  plaisance2$nbPersVoy <- rep(NA, nrow(plaisance2))
  plaisance2$influAct <- rep(NA, nrow(plaisance2))
  plaisanceTrans <- plaisance2[, c("AMP", "quest", "numSortie", "activite", "activiteSpe", "periodEchant", "dureeSortie", "pratique",
                                   "anciennete", "nbSortie", "partAMP", "choixSite1", "choixSite2" , "choixSite3", "budgetAn",
                                   "existenceAMP", "influAMP", "defAMP", "connaisRegAMP", "suffiInfo", "adaptReg", "respectReg",
                                   "assoProces", "effetEcosyst", "effetEcono", "effetAct", "relationPP", "relationPA", "relationCH",
                                   "relationPL", "relationPS", "relationJS", "relationKS", "avisAMP", "connaisCM", "utilCM", "suffiCM",
                                   "sexe", "anNais", "CSP", "pays", "commune", "resident", "nbNuit", "nbPersVoy", "depTot", "premVisi",
                                   "anNb", "nbVisitAn", "revenir", "influAct")]

## recup plongée
plongee2 <- plongee
  plongee2$numSortie <- rep(NA, nrow(plongee2))
  plongee2$nbSortie <- plongee2$nbAn
  plongee2$activite <- rep("plongee", nrow(plongee2))
  plongee2$activiteSpe <- rep("plongee", nrow(plongee2))
  plongee2$ancien <- anneeActuelle - plongee2$anciennete
    plongee2$ancien2 <- plongee2$ancien
    if (nrow(plongee)!=0){
      plongee2$ancien2[plongee2$ancien > 20] = ">20ans"
      plongee2$ancien2[11<=plongee2$ancien & plongee2$ancien<=20] = "11a20ans"
      plongee2$ancien2[6<=plongee2$ancien & plongee2$ancien<=10] = "6a10ans"
      plongee2$ancien2[1<plongee2$ancien & plongee2$ancien<=5] = "1a5ans"
      plongee2$ancien2[plongee2$ancien<=1] = "<1an"
    }
  plongee2$anciennete <- plongee2$ancien2
  plongee2$dureeSortie <- rep(NA, nrow(plongee2))
    plongee2$partAMP <- (plongee2$nbVisitAn/plongee2$nbAn) * 100
    plongee2$partAMP2[plongee2$partAMP >= 76] = "76a100%"
    plongee2$partAMP2[51<=plongee2$partAMP & plongee2$partAMP<=75] = "51a75%"
    plongee2$partAMP2[26<=plongee2$partAMP & plongee2$partAMP<=50] = "26a50%"
    plongee2$partAMP2[1<=plongee2$partAMP & plongee2$partAMP<=25] = "1a25%"
    plongee2$partAMP2[plongee2$partAMP==0] = "0%"
  plongee2$partAMP <- plongee2$partAMP2
  plongee2$defAMP <- rep(NA, nrow(plongee2))
  plongee2$connaisRegAMP <- rep(NA, nrow(plongee2))
  plongee2$avisAMP <- rep(NA, nrow(plongee2))
  plongee2$connaisCM <- rep(NA, nrow(plongee2))
  plongee2$utilCM <- rep(NA, nrow(plongee2))
  plongee2$suffiCM <- rep(NA, nrow(plongee2))
  if (is.na(unique(plongee2$effetEcosyst)[1]) == TRUE){
    plongee2$effetEcosyst <- plongee2$effetEcosystB
  }
  plongee2$premVisi <- rep(NA, nrow(plongee2))
  plongeeTrans <- plongee2[, c("AMP", "quest", "numSortie", "activite", "activiteSpe", "periodEchant", "dureeSortie", "pratique",
                               "anciennete", "nbSortie", "partAMP", "choixSite1", "choixSite2", "choixSite3", "budgetAn",
                               "existenceAMP", "influAMP", "defAMP", "connaisRegAMP", "suffiInfo", "adaptReg", "respectReg",
                               "assoProces", "effetEcosyst", "effetEcono", "effetAct", "relationPP", "relationPA",
                               "relationCH", "relationPL", "relationPS", "relationJS", "relationKS", "avisAMP", "connaisCM",
                               "utilCM", "suffiCM", "sexe", "anNais", "CSP", "pays", "commune", "resident", "nbNuit", "nbPersVoy",
                               "depTot", "premVisi", "anNb", "nbVisitAn", "revenir", "influAct")]

## recup excursion
excursion2 <- excursion
  excursion2$numSortie <- rep(NA, nrow(excursion2))
  excursion2$activite <- rep("excursion", nrow(excursion2))
  excursion2$activiteSpe <- rep("EXCU", nrow(excursion2))
  excursion2$activiteSpe[excursion2$parcoursGuide=="oui"] <- "SSM"
  excursion2$dureeSortie <- rep(NA, nrow(excursion2))
  excursion2$pratique <- rep(NA, nrow(excursion2))
  excursion2$ancien <- anneeActuelle-excursion2$anciennete
    excursion2$ancien2 <- excursion2$ancien
    if (nrow(excursion)!=0){
      excursion2$ancien2[excursion2$ancien > 20] = ">20ans"
      excursion2$ancien2[11<=excursion2$ancien & excursion2$ancien<=20] = "11a20ans"
      excursion2$ancien2[6<=excursion2$ancien & excursion2$ancien<=10] = "6a10ans"
      excursion2$ancien2[1<excursion2$ancien & excursion2$ancien<=5] = "1a5ans"
      excursion2$ancien2[excursion2$ancien<=1] = "<1an"
    }
  excursion2$anciennete <- excursion2$ancien2
  excursion2$nbSortie <- rep(NA, nrow(excursion2))
  excursion2$partAMP <- rep(NA, nrow(excursion2))
  excursion2$choixSite3 <- rep(NA, nrow(excursion2))
  excursion2$budgetAn <- rep(NA, nrow(excursion2))
  excursion2$adaptReg <- rep(NA, nrow(excursion2))
  excursion2$respectReg <- rep(NA, nrow(excursion2))
  excursion2$assoProces <- rep(NA, nrow(excursion2))
  excursion2$effetEcono <- rep(NA, nrow(excursion2))
  excursion2$effetAct <- rep(NA, nrow(excursion2))
  excursion2$relationPP <- rep(NA, nrow(excursion2))
  excursion2$relationPA <- rep(NA, nrow(excursion2))
  excursion2$relationCH <- rep(NA, nrow(excursion2))
  excursion2$relationPL <- rep(NA, nrow(excursion2))
  excursion2$relationPS <- rep(NA, nrow(excursion2))
  excursion2$relationJS <- rep(NA, nrow(excursion2))
  excursion2$relationKS <- rep(NA, nrow(excursion2))
  excursion2$anNb <- rep(NA, nrow(excursion2))
  excursionTrans <- excursion2[, c("AMP", "quest", "numSortie", "activite", "activiteSpe", "periodEchant", "dureeSortie", "pratique",
                                   "anciennete", "nbSortie", "partAMP", "choixSite1", "choixSite2", "choixSite3", "budgetAn",
                                   "existenceAMP", "influAMP", "defAMP", "connaisRegAMP", "suffiInfo", "adaptReg", "respectReg",
                                   "assoProces", "effetEcosyst", "effetEcono", "effetAct", "relationPP", "relationPA", "relationCH",
                                   "relationPL", "relationPS", "relationJS", "relationKS", "avisAMP", "connaisCM", "utilCM", "suffiCM",
                                   "sexe", "anNais", "CSP", "pays", "commune", "resident", "nbNuit", "nbPersVoy", "depTot", "premVisi",
                                   "anNb", "nbVisitAn", "revenir", "influAct")]
## compil tous questionnaires
tousQuest <- rbind(pecheTrans, plaisanceTrans, plongeeTrans, excursionTrans)
## rajout d'une colonne pour tous usages
tousQuest$toutConfondu <- rep("tousUsages", nrow(tousQuest))

# rajout d'un champ pour compiler les périodes d'échantillonnage
    if (length(unique(tousQuest$periodEchant)) == 1 ) {
        tousQuest$periodEchantCouplee <- tousQuest$periodEchant
    } else {
        tousQuest$periodEchantCouplee <- paste(sort(unique(tousQuest$periodEchant))[1], "_",
                                               sort(unique(tousQuest$periodEchant))[length(sort(unique(tousQuest$periodEchant)))],
                                               sep="")
    }

### calcul des limites des barplot
limActPeche <- c(0, length(unique(peche$act_peche1)))
limResPeche <- c(0, length(unique(peche$resident)))
limActTot <- c(0, length(unique(tousQuest$activite)))
limActDet <- c(0, length(unique(tousQuest$activitePec)))
limResTot <- c(0, length(unique(tousQuest$resident)))

### sauvegarde de tous les fichiers importés
FREQTOT <- freqtot      # fichiers initiaux
PECHE <- peche
PECHEQ <- pecheQ
CAPTURES <- captures
CAPTURES2 <- captures2
CAPTURESAN <- capturesAn
PLAISANCE <- plaisance
PLONGEE <- plongee
EXCURSION <- excursion
TOUSQUEST <- tousQuest
freqtotRef <- freqtot
freqtotModif <- freqtot  # fichiers après un choix de l'utilisateur (ex : un usage, une année, etc.)
pecheModif <- peche
pecheQModif <- pecheQ
capturesModif <- captures
captures2Modif <- captures2
capturesAnModif <- capturesAn
plaisanceModif <- plaisance
plongeeModif <- plongee
excursionModif <- excursion
tousQuestModif <- tousQuest
freqtotRefModif <- freqtot

#mise à jour de l'interface
MiseajourTableau.f(tclarray)
MiseajourTableauInfo.f(tclarray2)
tkconfigure(ResumerAMP, text = paste("\n Vous avez importé les jeux de données de", siteEtudie, " \n \n"))
gestionMSGaide.f("traitements")

## Fin des informations de chargement (demande de confirmation utilisateur) :
    stepInnerProgressBar.f(n = 0, msg = "Fin de chargement !",
                           font = tkfont.create(weight = "bold", size = 9), foreground = "darkred")
    infoLoading.f(msg = "Vous pouvez maintenant procéder au calcul des metriques", icon = "info")
    infoLoading.f(button = TRUE)


################################################################################
