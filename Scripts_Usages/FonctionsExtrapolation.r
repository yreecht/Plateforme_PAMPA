################################################################################
# Nom               : FonctionsExtrapolation.r
# Type              : Programme
# Objet             : Ce programme comporte toutes les fonctions pour l'extrapolation 
#                     de la fréquentation
#                     Ces fonctions seront appelées dans l'interface relative à 
#                     l'extrapolation des données de fréquentation
# Input             : calendrier et fichier txt
# Output            : lancement de fonctions
# Auteur            : Elodie Gamp
# R version         : 2.11.1
# Date de création  : janvier 2012
# Sources
################################################################################


# FONCTION 1 : tableau de référence utilisé dans toutes les fonctions suivantes (en gardant les mêmes niveaux spatial et temporel)
### Fonction de calcul des nombres de jours par mois, 
TabEchant.f <- function(FreqAnneeChoisie, niveauSpatial="zone") {

  spatialDispo <- sort(as.character(unique(FreqAnneeChoisie[,niveauSpatial])))    # ttes zones dispo dans FreqAnneeChoisie 
  temporelDispo <- sort(unique(calendrier$mois))                            # tous les mois avec sortie dans FreqAnneeSortie
  nbSpatial <- length(spatialDispo)
  nbTemporel <- length(temporelDispo)

  # préliminaires pour certaines calculs de la table d'échantillonnage
  CaracSortie <- unique(FreqAnneeChoisie[,c("numSortie","jour","saison","typeJ","typeJsimp","mois",niveauSpatial)])   # caractéristique sortie
  assign("CaracSortie",CaracSortie,envir=.GlobalEnv)   
  
##########  tkmessagebox info si niveau spatial ou niveau temporel manquant (à mettre dans fichier de sortie)
############## régler la question de la validité de l'extrapolation en cas de strates vides.

  InfoProtocoleEchant <- array (0,c(nbTemporel, nbSpatial, 6),
                  list(temporelDispo, spatialDispo, c("JS","JW","TJ","NbJSAnnee","NbJWAnnee","NbTJAnnee")))

  effectifSortie <- table(CaracSortie$typeJsimp,
                          factor(CaracSortie$mois),
                          factor(CaracSortie[,niveauSpatial]))
  
  InfoProtocoleEchant [,,"JS"] <- effectifSortie ["JS", match(dimnames(InfoProtocoleEchant)[[1]], 
                                                              dimnames(effectifSortie)[[2]]),] # nb de JS échantillonnés
  InfoProtocoleEchant [,,"JW"] <- effectifSortie ["JW", match(dimnames(InfoProtocoleEchant)[[1]], 
                                                              dimnames(effectifSortie)[[2]]),] # idem pour les JW
  InfoProtocoleEchant[is.na(InfoProtocoleEchant)] <- 0                          ## en cas d'absence d'un mois dans les observations
  
  InfoProtocoleEchant [,,"TJ"] <- InfoProtocoleEchant [,,"JS"] + InfoProtocoleEchant [,,"JW"]
  
  InfoProtocoleEchant[,,"NbJSAnnee"] <- rep(tapply(calendrier$JS,
                                                   calendrier$mois,
                                                   sum,na.rm=T),
                                            nbSpatial)       # nb JS dispo dans le calendrier
  InfoProtocoleEchant[,,"NbJWAnnee"] <- rep(tapply(calendrier$JW,
                                                   calendrier$mois,
                                                   sum,na.rm=T),
                                            nbSpatial)       # idem en JW
  InfoProtocoleEchant[,,"NbTJAnnee"] <- InfoProtocoleEchant[,,"NbJSAnnee"] + InfoProtocoleEchant[,,"NbJWAnnee"]
  return(InfoProtocoleEchant)
}


#########################################################################################################################################################################################
# FONCTION 2 : Fonction du calcul de la moyenne et de la variance par mois, zone, et type de jour 
CalculMoyenneVariance.f <- function(FreqAnneeChoisie, variable, niveauSpatial="zone") {

    # tableau en 4D : somme de la fréquentation par mois, zone, type de jour et sortie
    FreqJour <- tapply(FreqAnneeChoisie[,variable],list(FreqAnneeChoisie$mois,
                                                      factor(FreqAnneeChoisie[,niveauSpatial]),
                                                      factor(FreqAnneeChoisie$typeJsimp),
                                                      factor(FreqAnneeChoisie$numSortie)),
                        sum,na.rm=T)
    # tableau 3D : fréquentation moyenne par mois, zone, et type de jour 
    MoyFreqJour <- apply(FreqJour,c(1,2,3),mean,na.rm=T)
    # tableau 3D : variance de la fréquentation par mois, zone, et type de jour
    VarFreqJour <- apply(FreqJour,c(1,2,3),var,na.rm=T)
    
    FreqMoyVar <- list(MoyFreqJour,VarFreqJour)
    return(FreqMoyVar)   # la fonction donne une liste comportant 2 tableaux 3D  : un contenant la moyenne de la fréquentation, l'autre la variance de la fréquentation
                                # par mois, zone, et type de jour
}


#########################################################################################################################################################################################
# FONCTION 3 : calcul de l'estimation de la fréquentation par mois et zone
FreqTempSpatial.f <- function(InfoProtocoleEchant = InfoProtocoleEchant, FreqMoyVar = FreqMoyVar)  {
                                        # rappel : InfoProtocoleEchant = tableau en 3D , 1=mois, 2=zone et 3 = type de jour
   spatialDispo <- dimnames(InfoProtocoleEchant)[[2]]
   nbSpatial <- length(spatialDispo)
   temporelDispo <- dimnames(InfoProtocoleEchant)[[1]]
   nbTemporel <- length(temporelDispo)

   FreqMoy <- FreqMoyVar[[1]]     # tableau en 3D : 1=mois, 2=zone et 3 = type de jour
   FreqVar <- FreqMoyVar[[2]]     # tableau en 3D : 1=mois, 2=zone et 3 = type de jour

### etimation fréquentation spatiale/mois*typeJour
  matcheJS <- match(dimnames(as.matrix(InfoProtocoleEchant[,,"NbJSAnnee"]))[[1]], dimnames(as.matrix(FreqMoy[,,"JS"]))[[1]]) 
  matcheJW <- match(dimnames(as.matrix(InfoProtocoleEchant[,,"NbJWAnnee"]))[[1]], dimnames(as.matrix(FreqMoy[,,"JW"]))[[1]]) 
    
  FreqSpatialMoisJour <- array (0,c(nbTemporel, nbSpatial, 2),
                  list(temporelDispo, spatialDispo, c("FreqJS","FreqJW")))

  matchTemp1 <- match(dimnames(FreqSpatialMoisJour)[[1]], dimnames(as.matrix(FreqMoy[,,"JW"]))[[1]]) 
  matchTemp2 <- match(dimnames(FreqSpatialMoisJour)[[1]], dimnames(InfoProtocoleEchant)[[1]]) 

   FreqSpatialMoisJour[,,"FreqJS"] <- FreqMoy[matchTemp1,,"JS"] * InfoProtocoleEchant[matchTemp2,,"NbJSAnnee"] [matcheJS] 
   FreqSpatialMoisJour[,,"FreqJW"] <- FreqMoy[matchTemp1,,"JW"] * InfoProtocoleEchant[matchTemp2,,"NbJWAnnee"] [matcheJW]  

  return(FreqSpatialMoisJour)  # tableau 3D :  fréquentation extrapolée par niveau spatial, mois et typeJ
}


#########################################################################################################################################################################################
### FONCTION 4 (types jours confondus) : Calcul de l'estimateur par niveau temporel 
###                 + graphiques et enregistrements du résultats
Estimateur.f <- function (FreqSpatialMoisJour, InfoProtocoleEchant, 
                          niveauTemporel, niveauSpatial, periode, facteurSep = NULL, 
                          modalites = NULL, titre = "", nombre, graph = T, statBoot = NULL) {

  # somme de l'estimation sur les deux types de jours
  FreqSpatialMois <- apply(FreqSpatialMoisJour, c(1,2), sum, na.rm=T)

  # somme de l'estimation sur le niveau temporel choisi
    EstimSpatialTemp1 <- as.data.frame(FreqSpatialMois)
    EstimSpatialTemp1$niveau <- calendrier[,niveauTemporel][match(rownames(EstimSpatialTemp1),
                                                                           calendrier$mois)]
    EstimSpatialTemp <- aggregate(EstimSpatialTemp1[,1:(ncol(EstimSpatialTemp1)-1)],
                                  list(EstimSpatialTemp1$niveau),
                                  sum, na.rm=T)
    rownames(EstimSpatialTemp) <- EstimSpatialTemp[,1]
    EstimSpatialTemp2 <- as.data.frame(t(EstimSpatialTemp[,-1]))
  
  # somme de l'estimation sur le niveau spatial choisi 
    EstimSpatialTemp2$niveauSpa <- refSpatial[,niveauSpatial][match(rownames(EstimSpatialTemp2),
                                                                    refSpatial$codeZone)]
    EstimSpatialTemp <- aggregate(EstimSpatialTemp2[,1:(ncol(EstimSpatialTemp2)-1)],
                                  list(EstimSpatialTemp2$niveauSpa),
                                  sum, na.rm=T)
    rownames(EstimSpatialTemp) <- EstimSpatialTemp[,1]
    EstimSpatialTemp <- as.matrix(EstimSpatialTemp[,-1])

    if(length(unique(EstimSpatialTemp1$niveau)) == 1) { rownames(EstimSpatialTemp) <- sort(unique(EstimSpatialTemp2$niveauSpa)) }
 #   if(ncol(EstimSpatialTemp) == 1) { colnames(EstimSpatialTemp) <- unique(EstimSpatialTemp2$niveauSpa) }    

    if (graph==T) {   # hors bootstrap
     
        write.csv(EstimSpatialTemp , file = paste("C:/PAMPA/Resultats_Usages/extrapolation/EstimExtrapol",periode, nombre,niveauTemporel, "et", niveauSpatial,titre,".csv",sep=""))
  
        ## Barplot de la fréquentation estimée
        x11(width=50,height=30,pointsize=6)
        layout(matrix(c(1,2),ncol=1), height=c(10,2))
        par(oma=c(0, 2, 4, 0),mar=c(2, 6, 4, 2))
        plotEstimat <- barplot(EstimSpatialTemp, beside = T, cex.lab = 2.5, cex.axis = 2.5 ,
                                                 cex.names = 2.5, cex.main = 2.6, xlab = niveauTemporel,
                                                 ylab = paste("Nombre de",nombre,"estimé (+- IC 95%)"),
                                                 ylim = c(0,max(EstimSpatialTemp, na.rm=T)), 
                                                 main = paste("En",periode,": Estimation du nombre de",nombre,"extrapolé \n par",niveauTemporel, "et", niveauSpatial, titre))
        
        if (length(statBoot) != 0) {       # uniquement si les IC sont calculés par bootstrap
            layout(matrix(c(1,2),ncol=1), height=c(10,2))
            par(oma=c(0, 2, 4, 0),mar=c(2, 6, 4, 2))
            
            if(length(dim(statBoot))== 4) {
                statBoot <- apply(statBoot,c(1,2,4), sum, na.rm=T)   # enlève la dimension supplémentaire    
            }
            plotEstimat <- barplot(EstimSpatialTemp, beside = T, cex.lab = 2.5, cex.axis = 2.5 ,
                                                     cex.names = 2.5, cex.main = 2.6, xlab = niveauTemporel,
                                                     ylab = paste("Nombre de",nombre,"estimé (+- IC 95%)"),
                                                     ylim = c(0,max(statBoot[,,"ICSupEstime"], na.rm=T)),      # limite dépendant de la valeur max d'IC
                                                     main = paste("En",periode,": Estimation du nombre de",nombre,"extrapolé \n par",niveauTemporel, "et", niveauSpatial, titre))
    
              if(length(unique(EstimSpatialTemp1$niveau)) == 1) {
                  matchICc <- 1
              } else {
                  matchICc <- match(colnames(EstimSpatialTemp), colnames(statBoot))
              }
              matchICr <- match(rownames(EstimSpatialTemp), rownames(statBoot)) 

              if (sum(dim(EstimSpatialTemp))== 2) {           # si un seul niveau spatial et un seul niveau temporel
                  arrows(plotEstimat, statBoot[,,"ICInfEstime"], 
                         plotEstimat, statBoot[,,"ICSupEstime"], code = 3, col = "red", angle = 90, length = .1)
    
              } else {
                  arrows(plotEstimat, statBoot[matchICr,matchICc,"ICInfEstime"], 
                         plotEstimat, statBoot[matchICr,matchICc,"ICSupEstime"], code = 3, col = "red", angle = 90, length = .1)
              }
          
        } else {}
                
        plot(1:2, 1:2, type = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "", bty = "n")
        legend("center",
                title = "Légende des couleurs",
                legend = rownames(EstimSpatialTemp), col = grey.colors(nrow(EstimSpatialTemp)), pch=15, ncol=4, xpd=NA, cex = 2.5)

        savePlot (filename = paste("C:/PAMPA/Resultats_Usages/extrapolation/EstimExtrapol", periode, nombre, niveauTemporel, niveauSpatial, titre, sep=""), type =c("bmp"))
        return(EstimSpatialTemp)
      
    } else {          # uniquement pour bootstrap
        return(EstimSpatialTemp)
        print(EstimSpatialTemp)
    }
}


#########################################################################################################################################################################################
### FONCTION 4 (types jours distincts) : Calcul de l'estimateur par niveau temporel 
###                 + graphiques et enregistrements du résultats
EstimateurJ.f <- function (FreqAnneeChoisie, FreqSpatialMoisJour, InfoProtocoleEchant = InfoProtocoleEchant, 
                           niveauTemporel, niveauSpatial, periode, facteurSep = NULL, 
                           modalites = NULL, titre = "",nombre = "bateaux",graph = T, statBoot = NULL) {

 # somme de l'estimation sur le niveau temporel choisi
  EstimSpatialTempJ1 <- unlist(apply(FreqSpatialMoisJour, 3, list), recursive = FALSE)
 # browser()
#  EstimSpatialTempJ1 <- FreqSpatialMoisJour
  Estim <- lapply(1:2,FUN = function(i,...) {
                   Estimateur.f(FreqSpatialMoisJour = EstimSpatialTempJ1[[i]],
                                InfoProtocoleEchant = InfoProtocoleEchant, 
                                niveauTemporel = niveauTemporel, 
                                niveauSpatial = niveauSpatial,
                                periode = periode, 
                                titre = paste(titre, "typeJ=", names(EstimSpatialTempJ1)[[i]]),
                                nombre = nombre,
                                graph = graph,
                                statBoot = statBoot[,,i,, drop=F])  # ne prend que les infos de statBoot correspondant au typeJ i
                   }
        ) 
#  EstimSpatialTempJ <-  array(unlist(Estim), dim = c(length(unique(FreqAnneeChoisie[,niveauSpatial])), 
#                                                     length(unique(FreqAnneeChoisie[,niveauTemporel])), 
#                                                     2), 
#                                             dimnames = c(list(as.character(unique(FreqAnneeChoisie[,niveauSpatial]))),
#                                                          list(unique(FreqAnneeChoisie[,niveauTemporel])), 
#                                                          list(names(EstimSpatialTempJ1))))
#  print(EstimSpatialTempJ) 
  return(Estim)
        
}

#########################################################################################################################################################################################

### FONCTION 2 ADAPTEE ACTIVITE : Fonction du calcul de la moyenne et de la variance par niveau temporel, niveau spatial, et type de jour ADAPTEE AUX ACTIVITES
CalculMoyenneVarianceAct.f<-function(FreqAnneeChoisie, variable, niveauSpatial = "zone", facteurSep, modalites)  {

  spatialDispo <-sort(as.character(unique(FreqAnneeChoisie[,niveauSpatial])))    # ttes zones dispo dans FreqAnneeChoisie (à sortir des fct générique mais interne au fct interface)
  temporelDispo <- sort(unique(FreqAnneeChoisie$mois))    # tous les mois avec sortie dans FreqAnneeSortie
  nbSpatial <- length(spatialDispo)
  nbTemporel <- length(temporelDispo)
  
  FreqAnneeChoisie <- dropLevels.f(FreqAnneeChoisie, which=niveauSpatial)
    ## transformation pour récupérer les 0 pour le facteur de séparation choisi  
     Freq1 <- with(FreqAnneeChoisie,
                tapply(FreqAnneeChoisie[ , variable],
                        as.list(FreqAnneeChoisie[, c("mois", niveauSpatial,                     # éléments nommés pour avoir les noms de dimensions dans le tableau
                                                     "typeJsimp", "numSortie",
                                        facteurSep)]),                            # facteurSep défini le champ du facteur de séparation
                        sum, na.rm=TRUE))

   ## Indice des ensembles caractéristiques/sorties  existantes (hors sélection)
   idx <- with(FreqAnneeChoisie,
              tapply(FreqAnneeChoisie[ , variable],
                        as.list(FreqAnneeChoisie[, c("mois", niveauSpatial,                     # éléments nommés pour avoir les noms de dimensions dans le tableau
                                                     "typeJsimp", "numSortie")]),                                    # Les mêmes moins le champ de sélection
                     function(x)ifelse(length(x), TRUE, FALSE)))

   ## Renvoie quand même NA si inexistante => on remplace par FALSE
   idx[is.na(idx)] <- FALSE

   # Tableau de fréquentation par facteurSep (5D), corrigé pour
   ## tenir compte des activités non observées sur sorties effectives :
    FreqCor1 <- sweep(Freq1,
                 which(is.element(names(dimnames(Freq1)),  # Dimensions de Freq1 communes avec idx
                                  names(dimnames(idx)))), # (caractéristiques de sorties existantes).
                 idx,
                 function(x, y)
             {
                 x[is.na(x) & y] <- 0     # on ne remplace que les NAs des sorties existantes.
                 return(x)
             })

    # lors d'une sélection, FreqCor remplace le tableau 4D FreqJour
    # Attention, 1D de plus dans FreqCor. Les fonctions suivantes ne seront appliquées que pour la modalité choisie
    Agarde <- dimnames(FreqCor1)[[5]] [is.element(dimnames(FreqCor1)[[5]] , modalites)]
    FreqCor <- FreqCor1[,,,,Agarde, drop=F]                                      # réduit le tableau à la modalité choisie
    
    # tableau 3D : fréquentation moyenne par mois, niveau spatial et type de jour 
    MoyFreqJour <- apply(FreqCor,c(1,2,3),mean,na.rm=T)

    # tableau 3D : variance de la fréquentation par mois, niveau spatial et type de jour
    VarFreqJour <- apply(FreqCor,c(1,2,3),var,na.rm=T)
    
    FreqMoyVar <- list(MoyFreqJour,VarFreqJour)
    return(FreqMoyVar)   # la fonction donne une liste comportant 2 tableaux 3D  : un contenant la moyenne de la fréquentation, l'autre la variance de la fréquentation
}


####################################################################################################################################################################