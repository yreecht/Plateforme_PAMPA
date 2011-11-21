  calendrierGeneral <- read.table(file="C:/PAMPA/DONNEES WP3/CalendrierGeneral.txt", sep="\t", dec=".",header=T)
# nombre="nb_bat"
# nombre="nb_pers"


######### FONCTIONS DE CALCUL 
# pour calcul de la moyenne par niveau spatial
  MoySpatial.f <- function(MoyAnnee) {
    tapply(MoyAnnee,MoyZone$regroupement,sum,na.rm=T)
  }
# pour calcul de la moyenne par niveau temporel
  MoyTemporel.f <- function(MoyAnnee) {
    tapply(MoyAnnee,MoyTemp$regroupement,mean,na.rm=T)
  }
# pour calcul de la variance par niveau spatial
#  VarSpatial.f <- function(VarAnnee) {
#    tapply(VarAnnee,VarZone$regroupement,sum,na.rm=T)
#  }
# pour calcul de la variance par niveau temporel  
#  VarTemporel.f <- function(VarAnnee) {
#    tapply(VarAnnee,VarTemp$regroupement,mean,na.rm=T)
#  }
  
### choix de l'année à extrapoler
  ChoixAnneeExtrapol.f = function () {
    choixAnneeExtrapol<-tktoplevel()
    tkwm.title(choixAnneeExtrapol,"Année à extrapoler")
    anneeChoisie <- tclVar(2008)
    entry.annee <-tkentry(choixAnneeExtrapol,width="20",textvariable=anneeChoisie)
    tkgrid(tklabel(choixAnneeExtrapol,text="Entrer l'année que vous souhaitez extrapoler"))
    tkgrid(entry.annee)
    OnOK <- function()
    {
    	anneeVal <- tclvalue(anneeChoisie)
    	tkdestroy(choixAnneeExtrapol)
    	tkmessageBox(message=paste("Les données seront extrapolées pour ",anneeVal))
      assign("anneeChoisie",as.numeric(anneeVal),envir=.GlobalEnv)
    }
    OK.but <-tkbutton(choixAnneeExtrapol,text="   OK   ",command=OnOK)
    tkbind(entry.annee, "anneeChoisie",OnOK)
    tkgrid(OK.but)
    tkfocus(choixAnneeExtrapol)
    tkwait.window(choixAnneeExtrapol)
  }
  ChoixAnneeExtrapol.f()
FreqAnneeChoisie <- subset(freqtot, freqtot$periodEchant==anneeChoisie)


# formation du tableau 3D d'informations selon les types de jours Annee*mois*typeJ
nbAnnee <- length(unique(calendrierGeneral$Annee))
nbMois <-  length(unique(calendrierGeneral$Mois))
InfoJour <- array (0,c(nbAnnee, nbMois, 4),
            list(unique(calendrierGeneral$Annee), unique(calendrierGeneral$Mois), c("nbJS","nbJW","PdsJS","PdsJW")))
            
  InfoJour [,,"nbJS"] <- tapply(calendrierGeneral$nbJS,list(calendrierGeneral$Annee,calendrierGeneral$Mois),sum,na.rm=T)
  InfoJour [,,"nbJW"] <- tapply(calendrierGeneral$nbJW,list(calendrierGeneral$Annee,calendrierGeneral$Mois),sum,na.rm=T)
  InfoJour [,,"PdsJS"] <- round(InfoJour [,,"nbJS"]/(InfoJour [,,"nbJS"]+InfoJour [,,"nbJW"]),digits=2)
  InfoJour [,,"PdsJW"] <- round(InfoJour [,,"nbJW"]/(InfoJour [,,"nbJS"]+InfoJour [,,"nbJW"]),digits=2)


#### Préparation pré-Bootstrap
Nboot = 500  # taille du bootstrap (nb de réplications)

## tableaux de stockage des réusltats bootstrap
FreqBatMoisAMP <- matrix(NA,ncol=length(unique(FreqAnneeChoisie$mois)),nrow=Nboot)               #  Nombre de bateaux par mois
colnames(FreqBatMoisAMP) <- unique(FreqAnneeChoisie$mois) 
#VarBatMoisAMP <- FreqBatMoisAMP               #  variance nombre de bateaux par mois

FreqBatTrimAMP <- matrix(NA,ncol=length(unique(FreqAnneeChoisie$trimestre)),nrow=Nboot)               #  Nombre de bateaux par trimestre
colnames(FreqBatTrimAMP) <- unique(FreqAnneeChoisie$trimestre) 
#VarBatTrimAMP <- FreqBatTrimAMP               #  variance nombre de bateaux par trimestre

FreqBatZonageAMP <- matrix(NA,ncol=length(unique(FreqAnneeChoisie$statut)),nrow=Nboot)               #  Nombre de bateaux par zonage
colnames(FreqBatZonageAMP) <- unique(FreqAnneeChoisie$statut) 
#VarBatZonageAMP <- FreqBatZonageAMP               #  variance nombre de bateaux par zonage

FreqBatZoneAMP <- matrix(NA,ncol=length(unique(FreqAnneeChoisie$zone)),nrow=Nboot)               #  Nombre de bateaux par zonage
colnames(FreqBatZoneAMP) <- unique(FreqAnneeChoisie$zone) 
#VarBatZoneAMP <- FreqBatZoneAMP               #  variance nombre de bateaux par zonage

temporelDispo <- unique( FreqAnneeChoisie [ , "mois" ] )
nbTemporel <- length(temporelDispo)

Sys.time()
################ BOUCLE BOOTSTRAP 
for (z in 1 : Nboot)  {               ## boucle en [z] pour les 1 000  itérations
    FreqBoot = NULL       # Re_echant
    for (i in 1: nbTemporel) {          ## boucle en [i] pour sélectionner chaque trimestre
        
        freqTrimestre <- FreqAnneeChoisie[which(FreqAnneeChoisie[,"mois"]==temporelDispo[i]),]
    # fréquentation pour le trimestre i (y compris les zéros pour les zonesXdate où aucun bateau)
        TypeJ <- unique( freqTrimestre$typeJsimp)  # types de jours vus durant ce trimestre    
    
        for (j in 1 : length(TypeJ)) {   ## boucle en [j] pour sélectionner chaque type de jour vus ce trimestre i
            freqTJ <- freqTrimestre[which(freqTrimestre$typeJsimp == TypeJ[j]),] 
    # ensemble des bateaux vus durant le trimestre i et le type de jour j (y compris les zéros pour les zonesXdate où aucun bateau)
    # (tableau à 36 colonnes)
            Sortie <- unique(freqTJ$num_sortie)   # liste des sorties du tri i et type de jour j
            TirageSortie <- sample(Sortie,replace=T,size=length(Sortie))
    # échantillonne avec remise une proportion prop_ech des jours de sorties du trimestre i et du type_J j
  
  # reconstitution du tableau de fréquentation ré-échantillonné
            FreqEchant <- FreqAnneeChoisie[1,]
            FreqEchant$numBootstrap <- NA
            for (S in 1 : length(TirageSortie)) {
              FreqSortie <- subset(FreqAnneeChoisie,FreqAnneeChoisie$num_sortie==TirageSortie[S])
              FreqSortie$numBootstrap <- paste(FreqSortie$num_sortie,S,sep="_")         # permet de distinguer les sorties tirées plusieurs fois
              FreqEchant <- rbind(FreqEchant,FreqSortie)
  
            }
            FreqEchant <- FreqEchant[-1,]
  # tableau à 36 colonnes des bateaux rééchantillonés pour le trimestre i et le type de jour j
              if (is.null(FreqBoot)) { FreqBoot <- FreqEchant         # nécessaire pour le premier passage sinon rbind marche pas
              } else { FreqBoot <- rbind(FreqBoot,FreqEchant) }          # colle les lignes de bateaux rééchantillonnés pour (i,j)
        
        } ## fin de la boucle en type de jour (j)
    } # fin de la boucle du trimestre i
  
  
  FreqJour <- tapply(FreqBoot[,nombre],list(FreqBoot$mois,factor(FreqBoot$zone),factor(FreqBoot$typeJsimp),factor(FreqBoot$numBootstrap),factor(FreqBoot$periodEchant)),sum,na.rm=T)
  # tableau 3D : fréquentation moyenne par mois, zone, type de jour et periodEchant
  MoyFreqJour <- apply(FreqJour,c(1,2,3,5),mean,na.rm=T)
#  VarFreqJour <- apply(FreqJour,c(1,2,3,5),var,na.rm=T)
  
  # Calcul de la moyenne stratifiée
  MoyenneStratifiee <- array (0,c(dim(MoyFreqJour)[1],dim(MoyFreqJour)[2], 2, dim(MoyFreqJour)[4]),
              list(dimnames(MoyFreqJour)[[1]],dimnames(MoyFreqJour)[[2]], c("MoyFreqJSstrat","MoyFreqJWstrat"), dimnames(MoyFreqJour)[[4]]))
  
  MoyenneStratifiee[,,"MoyFreqJSstrat",] <- MoyFreqJour[,,"JS",] * rep(InfoJour [match(dimnames(MoyFreqJour)[[4]],dimnames(InfoJour)[[1]]),,"PdsJS"],dim(MoyFreqJour)[[2]])
  MoyenneStratifiee[,,"MoyFreqJWstrat",] <- MoyFreqJour[,,"JW",] * rep(InfoJour [match(dimnames(MoyFreqJour)[[4]],dimnames(InfoJour)[[1]]),,"PdsJW"],dim(MoyFreqJour)[[2]])
  MoyFreqStrat <- apply(MoyenneStratifiee,c(1,2,4),sum,na.rm=T)
  
  # Calcul de la variance stratifiée
#  VarianceStratifiee <- array (0,c(dim(VarFreqJour)[1],dim(VarFreqJour)[2], 2, dim(VarFreqJour)[4]),
#              list(dimnames(VarFreqJour)[[1]],dimnames(VarFreqJour)[[2]], c("VarFreqJSstrat","VarFreqJWstrat"), dimnames(VarFreqJour)[[4]]))
  
#  VarianceStratifiee[,,"VarFreqJSstrat",] <- VarFreqJour[,,"JS",] * (rep(InfoJour [match(dimnames(VarFreqJour)[[4]],dimnames(InfoJour)[[1]]),,"PdsJS"],dim(VarFreqJour)[[2]]))^2
#  VarianceStratifiee[,,"VarFreqJWstrat",] <- VarFreqJour[,,"JW",] * (rep(InfoJour [match(dimnames(VarFreqJour)[[4]],dimnames(InfoJour)[[1]]),,"PdsJW"],dim(VarFreqJour)[[2]]))^2
#  VarFreqStrat <- apply(VarianceStratifiee,c(1,2,4),sum,na.rm=T)
  
  
  # Calcul de la moyenne par niveaux spatiaux
## zonage PAMPA
  niveauSpaChoisi="statutPAMPA"
  MoyZone <- as.data.frame(apply(MoyFreqStrat,c(2,3),mean,na.rm=T))
  MoyZone$regroupement<-refSpatial[,niveauSpaChoisi][match(rownames(MoyZone),as.character(refSpatial$codeZone))]
  
    if (ncol(MoyZone)-1 ==1) {
      MoySpatial <- tapply(MoyZone[,1],MoyZone$regroupement,sum,na.rm=T)
    } else {
      MoySpatial <- apply(MoyZone[,1:(ncol(MoyZone)-1)],2,MoySpatial.f)
    }
  FreqBatZonageAMP [z,] <- MoySpatial[match(colnames(FreqBatZonageAMP),rownames(MoySpatial))]

  # zones
  niveauSpaChoisi="codeZone"
  MoyZone <- as.data.frame(apply(MoyFreqStrat,c(2,3),mean,na.rm=T))
  MoyZone$regroupement<-refSpatial[,niveauSpaChoisi][match(rownames(MoyZone),as.character(refSpatial$codeZone))]
  
    if (ncol(MoyZone)-1 ==1) {
      MoySpatial <- tapply(MoyZone[,1],MoyZone$regroupement,sum,na.rm=T)
    } else {
      MoySpatial <- apply(MoyZone[,1:(ncol(MoyZone)-1)],2,MoySpatial.f)
    }
  FreqBatZoneAMP [z,] <- MoySpatial[match(colnames(FreqBatZoneAMP),rownames(MoySpatial))]

  
  # Calcul de la moyenne par niveaux temporels
  ### trimestre
  niveauTempChoisi="Trimestre"
  MoyTemp <- as.data.frame(apply(MoyFreqStrat,c(1,3),sum,na.rm=T))
  MoyTemp$regroupement<-calendrierGeneral[,niveauTempChoisi][match(rownames(MoyTemp),as.character(calendrierGeneral$Mois))]
    
    if (ncol(MoyTemp)-1 ==1) {
      MoyTemporel <- tapply(MoyTemp[,1],MoyTemp$regroupement,mean,na.rm=T)
    } else {
      MoyTemporel <- apply(MoyTemp[,1:(ncol(MoyTemp)-1)],2,MoyTemporel.f)
    }
  FreqBatTrimAMP [z,] <- MoyTemporel[match(colnames(FreqBatTrimAMP),rownames(MoyTemporel))]
    
  ### mois
  niveauTempChoisi="Mois"
  MoyTemp <- as.data.frame(apply(MoyFreqStrat,c(1,3),sum,na.rm=T))
  MoyTemp$regroupement<-calendrierGeneral[,niveauTempChoisi][match(rownames(MoyTemp),as.character(calendrierGeneral$Mois))]
    
    if (ncol(MoyTemp)-1 ==1) {
      MoyTemporel <- tapply(MoyTemp[,1],MoyTemp$regroupement,mean,na.rm=T)
    } else {
      MoyTemporel <- apply(MoyTemp[,1:(ncol(MoyTemp)-1)],2,MoyTemporel.f)
    }
  FreqBatMoisAMP [z,] <- MoyTemporel[match(colnames(FreqBatMoisAMP),rownames(MoyTemporel))]
  
  
  # Calcul de la variance par niveaux spatiaux
  # zonage
#  niveauSpaChoisi="statutPAMPA"
#  VarZone <- as.data.frame(apply(VarFreqStrat,c(2,3),mean,na.rm=T))
#  VarZone$regroupement<-refSpatial[,niveauSpaChoisi][match(rownames(VarZone),as.character(refSpatial$codeZone))]
    
#    if (ncol(VarZone)-1 ==1) {
#      VarSpatial <- tapply(VarZone[,1],VarZone$regroupement,sum,na.rm=T)
#    } else {
#      VarSpatial <- apply(VarZone[,1:(ncol(VarZone)-1)],2,VarSpatial.f)
#    }
#  VarBatZonageAMP [z,] <- VarSpatial[match(colnames(VarBatZonageAMP),rownames(VarSpatial))]

  # zones
#  niveauSpaChoisi="codeZone"
#  VarZone <- as.data.frame(apply(VarFreqStrat,c(2,3),mean,na.rm=T))
#  VarZone$regroupement<-refSpatial[,niveauSpaChoisi][match(rownames(VarZone),as.character(refSpatial$codeZone))]
    
#    if (ncol(VarZone)-1 ==1) {
#      VarSpatial <- tapply(VarZone[,1],VarZone$regroupement,sum,na.rm=T)
#    } else {
#      VarSpatial <- apply(VarZone[,1:(ncol(VarZone)-1)],2,VarSpatial.f)
#    }
#  VarBatZoneAMP [z,] <- VarSpatial[match(colnames(VarBatZoneAMP),rownames(VarSpatial))]

  # Calcul de la variance par niveaux temporels
  ### trimestre
#  niveauTempChoisi="Trimestre"
#  VarTemp <- as.data.frame(apply(VarFreqStrat,c(1,3),sum,na.rm=T))
#  VarTemp$regroupement<-calendrierGeneral[,niveauTempChoisi][match(rownames(VarTemp),as.character(calendrierGeneral$Mois))]
    
#    if (ncol(VarTemp)-1 ==1) {
#      VarTemporel <- tapply(VarTemp[,1],VarTemp$regroupement,mean,na.rm=T)
#    } else {
#      VarTemporel <- apply(VarTemp[,1:(ncol(VarTemp)-1)],2,VarTemporel.f)
#    }
#  VarBatTrimAMP [z,] <- VarTemporel[match(colnames(VarBatTrimAMP),rownames(VarTemporel))]
    
  ### mois
#  niveauTempChoisi="Mois"
#  VarTemp <- as.data.frame(apply(VarFreqStrat,c(1,3),sum,na.rm=T))
#  VarTemp$regroupement<-calendrierGeneral[,niveauTempChoisi][match(rownames(VarTemp),as.character(calendrierGeneral$Mois))]
    
#    if (ncol(VarTemp)-1 ==1) {
#      VarTemporel <- tapply(VarTemp[,1],VarTemp$regroupement,mean,na.rm=T)
#    } else {
#      VarTemporel <- apply(VarTemp[,1:(ncol(VarTemp)-1)],2,VarTemporel.f)
#    }
#  VarBatMoisAMP [z,] <- VarTemporel[match(colnames(VarBatMoisAMP),rownames(VarTemporel))]
  
}  ### fin de la boucle en Z
Sys.time()

# calcul des moyennes, variances et IC bootstrapés
FreqBootBatMoisAMP <- apply(FreqBatMoisAMP,2,mean,na.rm=T)
#VarBootBatMoisAMP <- apply(VarBatMoisAMP,2,mean,na.rm=T)
VarBootBatMoisAMP2 <- apply(FreqBatMoisAMP,2,var,na.rm=T)                              
InfBootBatMoisAMP <- apply(FreqBatMoisAMP,2,quantile,probs=0.025,na.rm=T)
SupBootBatMoisAMP <- apply(FreqBatMoisAMP,2,quantile,probs=0.975,na.rm=T)

FreqBootBatTrimAMP <- apply(FreqBatTrimAMP,2,mean,na.rm=T)
#VarBootBatTrimAMP <- apply(VarBatTrimAMP,2,mean,na.rm=T)
VarBootBatTrimAMP2 <- apply(FreqBatTrimAMP,2,var,na.rm=T)
InfBootBatTrimAMP <- apply(FreqBatTrimAMP,2,quantile,probs=0.025,na.rm=T)
SupBootBatTrimAMP <- apply(FreqBatTrimAMP,2,quantile,probs=0.975,na.rm=T)

FreqBootBatZonageAMP <- apply(FreqBatZonageAMP,2,mean,na.rm=T)               
#VarBootBatZonageAMP <- apply(VarBatZonageAMP,2,mean,na.rm=T)
VarBootBatZonageAMP2 <- apply(FreqBatZonageAMP,2,var,na.rm=T)
InfBootBatZonageAMP <- apply(FreqBatZonageAMP,2,quantile,probs=0.025,na.rm=T)
SupBootBatZonageAMP <- apply(FreqBatZonageAMP,2,quantile,probs=0.975,na.rm=T)

FreqBootBatZoneAMP <- apply(FreqBatZoneAMP,2,mean,na.rm=T)               
#VarBootBatZoneAMP <- apply(VarBatZoneAMP,2,mean,na.rm=T)
VarBootBatZoneAMP2 <- apply(FreqBatZoneAMP,2,var,na.rm=T)
InfBootBatZoneAMP <- apply(FreqBatZoneAMP,2,quantile,probs=0.025,na.rm=T)
SupBootBatZoneAMP <- apply(FreqBatZoneAMP,2,quantile,probs=0.975,na.rm=T)
