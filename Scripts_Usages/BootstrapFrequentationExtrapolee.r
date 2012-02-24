################################################################################
# Nom               : BootstrapFréquentationExtrapolée.r
# Type              : Programme
# Objet             : Fonctions Bootstrap appliquées aux métriques extrapolées de fréquentation 
#                     Ces fonctions seront appelées dans l'interface relative à 
#                     l'extrapolation des données de fréquentation.
# Input             : data importées en txt
# Output            : tableaux et graphs
# Auteur            : Elodie Gamp
# R version         : 2.11.1
# Date de création  : janvier 2012
# Sources
################################################################################


##################################################################################################
BootstrapExtrapolationTsJ.f <- function (niveauSpatial, niveauTemporel , 
                                      variable, anneeChoisie) 
{
    ## Purpose: Lancement de la fonction bootstrap pour les données d'extrapolation
    ##          en considérant que le type de jour est confondu, sans facteur de séparation
    ## ----------------------------------------------------------------------
    ## Arguments: niveauSpatial : niveau spatial choisi pour le calcul de l'extrapolation
    ##            niveauTemporel : niveau temporel chosisi pour le calcul de l'extrapolation
    ##            variable : variable à extrapoler
    ##            anneeChoisie : année (période d'échantillonnage) à extrapoler
    ## ----------------------------------------------------------------------
    ## Author: Elodie Gamp, Date: janvier 2012
         
 #   Sys.time()  ### affiche la date et l'heure
      
        ## définition de "nombre" selon la valeur de "variable"
        if (is.element(variable,
                       c("nbBat", "nbBatAct"))) {
            nombre <- "bateaux"
        } else {}
        
        if (is.element(variable,
                       c("nbPers", "nbPersAct"))) {
            nombre <- "personnes"
        } else {}
        
        if (is.element(variable,
                       c("nbLigne"))) {
            nombre <- "lignes"
        } else {}
        
        ## rajout de la colonne correspondant au niveau spatial choisi
        freqtot[, niveauSpatial] <- refSpatial[, niveauSpatial][match(freqtot$zone , refSpatial$codeZone)]
    
        ## rajout de la colonne correspondant au niveau temporel choisi
        freqtot[, niveauTemporel] <- calendrierGeneral[ , niveauTemporel][match(freqtot$moisAn , calendrierGeneral$moisAn)]
    
        ## subset du tableau de fréquentation sur la période choisie
        FreqAnneeChoisie <- subset(freqtot, freqtot$periodEchant == anneeChoisie)
    
    # rajout d'une colonne pour le numéro sortie*tirage (si tirage en doublon)
      #FreqAnneeChoisie$numBootstrap <- FreqAnneeChoisie$numSortie
      SAUVFreqAnneeChoisie <- FreqAnneeChoisie

     # niveauTemporel <- "mois"
      Nboot = 300  # taille du bootstrap (nb de réplications)
      temporelDispo <- unique( FreqAnneeChoisie [ , niveauTemporel ] )
      nbTemporel <- lengthnna.f(temporelDispo)
      spatialDispo <- unique( FreqAnneeChoisie [ , niveauSpatial ] )
      nbSpatial <- lengthnna.f(spatialDispo)
    
    # construction de la matrice de stockage des résultats des z itérations 
    
    FreqEstimee <- array(NA, dim = c(nbSpatial, 
                                     nbTemporel, 
                                     Nboot), 
                             dimnames = c(list(as.character(sort(spatialDispo))),
                                          list(as.character(temporelDispo)), 
                                          list(seq(1 : Nboot))))
    ### BOUCLE BOOTSTRAP ###
        
    for (z in 1 : Nboot)  {                                                     ## boucle en [z] pour les 1 000  itérations
      FreqBoot = NULL                                                           # tableau de stockage de l'échantillonnage
      FreqAnneeChoisie <- SAUVFreqAnneeChoisie                                  # évite de prendre un tableau modifié pour effectuer une nouvelle itération
      

      TirageSortie <- unlist(tapply(FreqAnneeChoisie$numSortie,
                         paste(FreqAnneeChoisie[, niveauTemporel], FreqAnneeChoisie$typeJsimp),
                         FUN=function(x){sample(unique(x), replace=T, size=length(unique(x)))}))

      tirage <- lapply(TirageSortie, FUN = function(x) {subset(FreqAnneeChoisie, 
                                                               FreqAnneeChoisie$numSortie==x)})
      names(tirage) <- seq(1,length(tirage))
      
      num <- unlist(lapply(names(tirage),
                           FUN=function(x){tirage[[x]]$num <- rep(x,nrow(tirage[[x]])) ;
                           tirage[[x]]$numBoot<-paste(tirage[[x]]$numSortie,tirage[[x]]$num)}))
      freqEchant <- do.call("rbind",tirage)
      freqEchant$numBoot <- num

      #### le tableau de fréquentation ré_échantillonné pour 1 itération est terminé
      #### les métriques sont recalculées sur ce nouveau tableau  freqEchant
    
    # certaines sorties ont été tirées plusieurs fois, FreqBoot$numBoot permet de notifier à R 
    # qu'il faut les considérer comme 2sorties différentes (= deux tirages)
    FreqBoot <- freqEchant  
    FreqBoot$numSortie <- FreqBoot$numBoot   

# Sys.time()  
    ### Calculs des extrapolations pour l'itération z
        estimBoot <- LancementExtrapolation.f (tab = FreqBoot, 
                                               niveauSpatial = niveauSpatial, 
                                               niveauTemporel = niveauTemporel, 
                                               variable = variable,
                                               periode = anneeChoisie, 
                                               rgpmtJour = "tousJours", 
                                               facteurSep = NULL, 
                                               modalites = NULL, 
                                               titre = "", 
                                               graph = F,
                                               statBoot = NULL)
                                               
       if (ncol(estimBoot)==1) {
          FreqEstimee [,,z] <- estimBoot [match(rownames(FreqEstimee) ,rownames(estimBoot)),]
       } else {
          FreqEstimee [,,z] <- estimBoot [, match(colnames(FreqEstimee) ,colnames(estimBoot))]
       } 
    } # fin du bootstrap  (boucle sur z itérations)
    
    
     ## ESTIMATIONS PAR BOOTSTRAP
    
     # nombre de bateaux toutes activités confondues
      MoyBoot <- apply(FreqEstimee, c(1,2), mean,na.rm=T)
      VarBoot <- apply(FreqEstimee, c(1,2), var,na.rm=T)
      InfBoot <- apply(FreqEstimee, c(1,2), quantile, probs=0.025, na.rm=T)
      SupBoot <- apply(FreqEstimee, c(1,2), quantile, probs=0.975, na.rm=T)

    # tableau de stockage des estimations bootstrap  
    StatBoot <- array(NA, dim = c(nbSpatial, 
                                  nbTemporel, 
                                  4), 
                             dimnames = c(list(rownames(MoyBoot)),
                                          list(colnames(MoyBoot)), 
                                          list(c("moyenneEstimee", "varianceEstimee", "ICInfEstime", "ICSupEstime"))))
    StatBoot [,,"moyenneEstimee"] <- MoyBoot
    StatBoot [,,"varianceEstimee"] <- VarBoot
    StatBoot [,,"ICInfEstime"] <- InfBoot
    StatBoot [,,"ICSupEstime"] <- SupBoot
    
    sink(paste("C:/PAMPA/Resultats_Usages/bootstrap/estimation de ", variable, 
                                    " par ", niveauSpatial, " et ", 
                                    niveauTemporel, ".txt", sep=""))
    print(StatBoot)
    sink()
    #Sys.time()  ### affiche la date et l'heure
    return (StatBoot)

}


################################################################################
BootstrapExtrapolationTyJ.f <- function (niveauSpatial, niveauTemporel , 
                                      variable, anneeChoisie) 
{
    ## Purpose: Lancement de la fonction bootstrap pour les données d'extrapolation
    ##          en considérant que le type de jour est distinct, sans facteur de séparation
    ## ----------------------------------------------------------------------
    ## Arguments: niveauSpatial : niveau spatial choisi pour le calcul de l'extrapolation
    ##            niveauTemporel : niveau temporel chosisi pour le calcul de l'extrapolation
    ##            variable : variable à extrapoler
    ##            anneeChoisie : année (période d'échantillonnage) à extrapoler
    ## ----------------------------------------------------------------------
    ## Author: Elodie Gamp, Date: janvier 2012
         
  #  Sys.time()  ### affiche la date et l'heure
      
        ## définition de "nombre" selon la valeur de "variable"
        if (is.element(variable,
                       c("nbBat", "nbBatAct"))) {
            nombre <- "bateaux"
        } else {}
        
        if (is.element(variable,
                       c("nbPers", "nbPersAct"))) {
            nombre <- "personnes"
        } else {}
        
        if (is.element(variable,
                       c("nbLigne"))) {
            nombre <- "lignes"
        } else {}
        
        ## rajout de la colonne correspondant au niveau spatial choisi
        freqtot[, niveauSpatial] <- refSpatial[, niveauSpatial][match(freqtot$zone , refSpatial$codeZone)]
    
        ## rajout de la colonne correspondant au niveau temporel choisi
        freqtot[, niveauTemporel] <- calendrierGeneral[ , niveauTemporel][match(freqtot$moisAn , calendrierGeneral$moisAn)]
    
        ## subset du tableau de fréquentation sur la période choisie
        FreqAnneeChoisie <- subset(freqtot, freqtot$periodEchant == anneeChoisie)
    
    # rajout d'une colonne pour le numéro sortie*tirage (si tirage en doublon)
      #FreqAnneeChoisie$numBootstrap <- FreqAnneeChoisie$numSortie
      SAUVFreqAnneeChoisie <- FreqAnneeChoisie
#Sys.time()      
     # niveauTemporel <- "mois"
      Nboot = 300  # taille du bootstrap (nb de réplications)
      temporelDispo <- unique( FreqAnneeChoisie [ , niveauTemporel ] )
      nbTemporel <- lengthnna.f(temporelDispo)
      spatialDispo <- unique( FreqAnneeChoisie [ , niveauSpatial ] )
      nbSpatial <- lengthnna.f(spatialDispo)
    
    # construction de la matrice de stockage des résultats des z itérations 
    
    FreqEstimee <- array(NA, dim = c(nbSpatial, 
                                     nbTemporel,
                                     2, 
                                     Nboot), 
                             dimnames = c(list(as.character(sort(spatialDispo))),
                                          list(as.character(temporelDispo)),
                                          list(c("FreqJS", "FreqJW")), 
                                          list(seq(1 : Nboot))))
    ### BOUCLE BOOTSTRAP ###
        
    for (z in 1 : Nboot)  {                                                     ## boucle en [z] pour les 1 000  itérations
      FreqBoot = NULL                                                           # tableau de stockage de l'échantillonnage
      FreqAnneeChoisie <- SAUVFreqAnneeChoisie                                  # évite de prendre un tableau modifié pour effectuer une nouvelle itération
      

      TirageSortie <- unlist(tapply(FreqAnneeChoisie$numSortie,
                         paste(FreqAnneeChoisie[, niveauTemporel], FreqAnneeChoisie$typeJsimp),
                         FUN=function(x){sample(unique(x), replace=T, size=length(unique(x)))}))

      tirage <- lapply(TirageSortie, FUN = function(x) {subset(FreqAnneeChoisie, 
                                                               FreqAnneeChoisie$numSortie==x)})
      names(tirage) <- seq(1,length(tirage))
      
      num <- unlist(lapply(names(tirage),
                           FUN=function(x){tirage[[x]]$num <- rep(x,nrow(tirage[[x]])) ;
                           tirage[[x]]$numBoot<-paste(tirage[[x]]$numSortie,tirage[[x]]$num)}))
      freqEchant <- do.call("rbind",tirage)
      freqEchant$numBoot <- num

      #### le tableau de fréquentation ré_échantillonné pour 1 itération est terminé
      #### les métriques sont recalculées sur ce nouveau tableau  freqEchant
    
    # certaines sorties ont été tirées plusieurs fois, FreqBoot$numBoot permet de notifier à R 
    # qu'il faut les considérer comme 2sorties différentes (= deux tirages)
    FreqBoot <- freqEchant  
    FreqBoot$numSortie <- FreqBoot$numBoot   

# Sys.time()  
    ### Calculs des extrapolations pour l'itération z
        estimBoot <- LancementExtrapolation.f (tab = FreqBoot, 
                                               niveauSpatial = niveauSpatial, 
                                               niveauTemporel = niveauTemporel, 
                                               variable = variable,
                                               periode = anneeChoisie, 
                                               rgpmtJour = "typeJours", 
                                               facteurSep = NULL, 
                                               modalites = NULL, 
                                               titre = "", 
                                               graph = F,
                                               statBoot = NULL)
                                               
#       FreqEstimee [,,,z] <- estimBoot [, match(colnames(FreqEstimee) , colnames(estimBoot)), match(dimnames(FreqEstimee)[[3]] , dimnames(estimBoot)[[3]])]
       FreqEstimee [,,"FreqJS",z] <- estimBoot[[1]] 
       FreqEstimee [,,"FreqJW",z] <- estimBoot[[2]] 
               
    } # fin du bootstrap  (boucle sur z itérations)
    
    
     ## ESTIMATIONS PAR BOOTSTRAP
    
     # nombre de bateaux toutes activités confondues
      MoyBoot <- apply(FreqEstimee, c(1,2,3), mean,na.rm=T)
      VarBoot <- apply(FreqEstimee, c(1,2,3), var,na.rm=T)
      InfBoot <- apply(FreqEstimee, c(1,2,3), quantile, probs=0.025, na.rm=T)
      SupBoot <- apply(FreqEstimee, c(1,2,3), quantile, probs=0.975, na.rm=T)

    # tableau de stockage des estimations bootstrap  
      StatBoot <- array(NA, dim = c(nbSpatial, 
                                    nbTemporel,
                                    2, 
                                    4), 
                               dimnames = c(list(rownames(MoyBoot)),
                                            list(colnames(MoyBoot)),
                                            list(dimnames(MoyBoot)[[3]]), 
                                            list(c("moyenneEstimee", "varianceEstimee", "ICInfEstime", "ICSupEstime"))))
      StatBoot [,,,"moyenneEstimee"] <- MoyBoot
      StatBoot [,,,"varianceEstimee"] <- VarBoot
      StatBoot [,,,"ICInfEstime"] <- InfBoot
      StatBoot [,,,"ICSupEstime"] <- SupBoot
      
      sink(paste("C:/PAMPA/Resultats_Usages/bootstrap/estimation par type de jour de ",  
                                    variable, " par ", niveauSpatial, " et ", 
                                    niveauTemporel, ".txt", sep=""))
      print(StatBoot)
      sink()
      return (StatBoot)
 #   Sys.time()  ### affiche la date et l'heure
}


################################################################################
BootstrapExtrapolationTsJFacteur.f <- function (niveauSpatial, niveauTemporel , 
                                                variable, anneeChoisie, facteurSep, modalite) 
{
    ## Purpose: Lancement de la fonction bootstrap pour les données d'extrapolation
    ##          en considérant que le type de jour est confondu, avec facteur de séparation
    ## ----------------------------------------------------------------------
    ## Arguments: niveauSpatial : niveau spatial choisi pour le calcul de l'extrapolation
    ##            niveauTemporel : niveau temporel chosisi pour le calcul de l'extrapolation
    ##            variable : variable à extrapoler
    ##            anneeChoisie : année (période d'échantillonnage) à extrapoler
    ##            facteurSep : facteur de séparation choisi pour l'extrapolation
    ##            modalite : modalité choisie pour l'extrapolation
    ## ----------------------------------------------------------------------
    ## Author: Elodie Gamp, Date: janvier 2012
         
  #  Sys.time()  ### affiche la date et l'heure
      
        ## définition de "nombre" selon la valeur de "variable"
        if (is.element(variable,
                       c("nbBat", "nbBatAct"))) {
            nombre <- "bateaux"
        } else {}
        
        if (is.element(variable,
                       c("nbPers", "nbPersAct"))) {
            nombre <- "personnes"
        } else {}
        
        if (is.element(variable,
                       c("nbLigne"))) {
            nombre <- "lignes"
        } else {}
        
        ## rajout de la colonne correspondant au niveau spatial choisi
        freqtot[, niveauSpatial] <- refSpatial[, niveauSpatial][match(freqtot$zone , refSpatial$codeZone)]
    
        ## rajout de la colonne correspondant au niveau temporel choisi
        freqtot[, niveauTemporel] <- calendrierGeneral[ , niveauTemporel][match(freqtot$moisAn , calendrierGeneral$moisAn)]
    
        ## subset du tableau de fréquentation sur la période choisie
        FreqAnneeChoisie <- subset(freqtot, freqtot$periodEchant == anneeChoisie)
    
    # rajout d'une colonne pour le numéro sortie*tirage (si tirage en doublon)
      #FreqAnneeChoisie$numBootstrap <- FreqAnneeChoisie$numSortie
      SAUVFreqAnneeChoisie <- FreqAnneeChoisie
#Sys.time()      
     # niveauTemporel <- "mois"
      Nboot = 300  # taille du bootstrap (nb de réplications)
      temporelDispo <- unique( FreqAnneeChoisie [ , niveauTemporel ] )
      nbTemporel <- lengthnna.f(temporelDispo)
      spatialDispo <- unique( FreqAnneeChoisie [ , niveauSpatial ] )
      nbSpatial <- lengthnna.f(spatialDispo)
    
    # construction de la matrice de stockage des résultats des z itérations 
    
    FreqEstimee <- array(NA, dim = c(nbSpatial, 
                                     nbTemporel, 
                                     Nboot), 
                             dimnames = c(list(as.character(sort(spatialDispo))),
                                          list(as.character(temporelDispo)), 
                                          list(seq(1 : Nboot))))
    ### BOUCLE BOOTSTRAP ###
        
    for (z in 1 : Nboot)  {                                                     ## boucle en [z] pour les 1 000  itérations
      FreqBoot = NULL                                                           # tableau de stockage de l'échantillonnage
      FreqAnneeChoisie <- SAUVFreqAnneeChoisie                                  # évite de prendre un tableau modifié pour effectuer une nouvelle itération
      

      TirageSortie <- unlist(tapply(FreqAnneeChoisie$numSortie,
                         paste(FreqAnneeChoisie[, niveauTemporel], FreqAnneeChoisie$typeJsimp),
                         FUN=function(x){sample(unique(x), replace=T, size=length(unique(x)))}))

      tirage <- lapply(TirageSortie, FUN = function(x) {subset(FreqAnneeChoisie, 
                                                               FreqAnneeChoisie$numSortie==x)})
      names(tirage) <- seq(1,length(tirage))
      
      num <- unlist(lapply(names(tirage),
                           FUN=function(x){tirage[[x]]$num <- rep(x,nrow(tirage[[x]])) ;
                           tirage[[x]]$numBoot<-paste(tirage[[x]]$numSortie,tirage[[x]]$num)}))
      freqEchant <- do.call("rbind",tirage)
      freqEchant$numBoot <- num

      #### le tableau de fréquentation ré_échantillonné pour 1 itération est terminé
      #### les métriques sont recalculées sur ce nouveau tableau  freqEchant
    
    # certaines sorties ont été tirées plusieurs fois, FreqBoot$numBoot permet de notifier à R 
    # qu'il faut les considérer comme 2sorties différentes (= deux tirages)
    FreqBoot <- freqEchant  
    FreqBoot$numSortie <- FreqBoot$numBoot   

# Sys.time()  
    ### Calculs des extrapolations pour l'itération z
        estimBoot <- LancementExtrapolationFacteursSep.f (tab = FreqBoot, 
                                                          niveauSpatial = niveauSpatial, 
                                                          niveauTemporel = niveauTemporel, 
                                                          variable = variable,
                                                          periode = anneeChoisie, 
                                                          rgpmtJour = "tousJours", 
                                                          facteurSep = facteurSep, 
                                                          modalites = modalite, 
                                                          titre = paste("pour le facteur",facteurSep, "=",modalite), 
                                                          graph = F,
                                                          statBoot = NULL)
                                               
      if (ncol(estimBoot)==1) {     # si un seul niveau temporel
          FreqEstimee [,,z] <- estimBoot [match(rownames(FreqEstimee) ,rownames(estimBoot)),]
       } else {
          FreqEstimee [,,z] <- estimBoot [, match(colnames(FreqEstimee) ,colnames(estimBoot))]
       } 
 
#       FreqEstimee [,,z] <- estimBoot         
    } # fin du bootstrap  (boucle sur z itérations)
    
    
     ## ESTIMATIONS PAR BOOTSTRAP
    
     # nombre de bateaux toutes activités confondues
      MoyBoot <- apply(FreqEstimee, c(1,2), mean,na.rm=T)
      VarBoot <- apply(FreqEstimee, c(1,2), var,na.rm=T)
      InfBoot <- apply(FreqEstimee, c(1,2), quantile, probs=0.025, na.rm=T)
      SupBoot <- apply(FreqEstimee, c(1,2), quantile, probs=0.975, na.rm=T)

    # tableau de stockage des estimations bootstrap  
      StatBoot <- array(NA, dim = c(nbSpatial, 
                                    nbTemporel, 
                                    4), 
                               dimnames = c(list(rownames(MoyBoot)),
                                            list(colnames(MoyBoot)), 
                                            list(c("moyenneEstimee", "varianceEstimee", "ICInfEstime", "ICSupEstime"))))
      StatBoot [,,"moyenneEstimee"] <- MoyBoot
      StatBoot [,,"varianceEstimee"] <- VarBoot
      StatBoot [,,"ICInfEstime"] <- InfBoot
      StatBoot [,,"ICSupEstime"] <- SupBoot
      
      sink(paste("C:/PAMPA/Resultats_Usages/bootstrap/estimation de ", variable, 
                                    " pour ", facteurSep, " = ", modalite, " par ", niveauSpatial, " et ", 
                                    niveauTemporel, ".txt", sep=""))
      print(StatBoot)
      sink()
      return (StatBoot)    
 #   Sys.time()  ### affiche la date et l'heure
}


################################################################################
BootstrapExtrapolationTyJFacteur.f <- function (niveauSpatial, niveauTemporel , 
                                                variable, anneeChoisie, facteurSep, modalite) 
{
    ## Purpose: Lancement de la fonction bootstrap pour les données d'extrapolation
    ##          en considérant que le type de jour est distinct, avec facteur de séparation
    ## ----------------------------------------------------------------------
    ## Arguments: niveauSpatial : niveau spatial choisi pour le calcul de l'extrapolation
    ##            niveauTemporel : niveau temporel chosisi pour le calcul de l'extrapolation
    ##            variable : variable à extrapoler
    ##            anneeChoisie : année (période d'échantillonnage) à extrapoler
    ##            facteurSep : facteur de séparation choisi pour l'extrapolation
    ##            modalite : modalité choisie pour l'extrapolation
    ## ----------------------------------------------------------------------
    ## Author: Elodie Gamp, Date: janvier 2012
         
  #  Sys.time()  ### affiche la date et l'heure
      
        ## définition de "nombre" selon la valeur de "variable"
        if (is.element(variable,
                       c("nbBat", "nbBatAct"))) {
            nombre <- "bateaux"
        } else {}
        
        if (is.element(variable,
                       c("nbPers", "nbPersAct"))) {
            nombre <- "personnes"
        } else {}
        
        if (is.element(variable,
                       c("nbLigne"))) {
            nombre <- "lignes"
        } else {}
        
        ## subset du tableau de fréquentation sur la période choisie
        FreqAnneeChoisie <- subset(freqtot, freqtot$periodEchant == anneeChoisie)
 
        ## rajout de la colonne correspondant au niveau spatial choisi
        FreqAnneeChoisie[, niveauSpatial] <- refSpatial[, niveauSpatial][match(FreqAnneeChoisie$zone , refSpatial$codeZone)]
    
        ## rajout de la colonne correspondant au niveau temporel choisi
        FreqAnneeChoisie[, niveauTemporel] <- calendrierGeneral[ , niveauTemporel][match(FreqAnneeChoisie$moisAn , calendrierGeneral$moisAn)]
    
    # rajout d'une colonne pour le numéro sortie*tirage (si tirage en doublon)
      #FreqAnneeChoisie$numBootstrap <- FreqAnneeChoisie$numSortie
      SAUVFreqAnneeChoisie <- FreqAnneeChoisie
#Sys.time()      
     # niveauTemporel <- "mois"
      Nboot = 300  # taille du bootstrap (nb de réplications)
      temporelDispo <- unique( FreqAnneeChoisie [ , niveauTemporel ] )
      nbTemporel <- lengthnna.f(temporelDispo)
      spatialDispo <- unique( FreqAnneeChoisie [ , niveauSpatial ] )
      nbSpatial <- lengthnna.f(spatialDispo)
    
    # construction de la matrice de stockage des résultats des z itérations 
    
    # construction de la matrice de stockage des résultats des z itérations 
    
    FreqEstimee <- array(NA, dim = c(nbSpatial, 
                                     nbTemporel,
                                     2, 
                                     Nboot), 
                             dimnames = c(list(as.character(sort(spatialDispo))),
                                          list(as.character(sort(temporelDispo))),
                                          list(c("FreqJS", "FreqJW")), 
                                          list(seq(1 : Nboot))))
    ### BOUCLE BOOTSTRAP ###
        
    for (z in 1 : Nboot)  {                                                     ## boucle en [z] pour les 1 000  itérations
      FreqBoot = NULL                                                           # tableau de stockage de l'échantillonnage
      FreqAnneeChoisie <- SAUVFreqAnneeChoisie                                  # évite de prendre un tableau modifié pour effectuer une nouvelle itération
      

      TirageSortie <- unlist(tapply(FreqAnneeChoisie$numSortie,
                         paste(FreqAnneeChoisie[, niveauTemporel], FreqAnneeChoisie$typeJsimp),
                         FUN=function(x){sample(unique(x), replace=T, size=length(unique(x)))}))

      tirage <- lapply(TirageSortie, FUN = function(x) {subset(FreqAnneeChoisie, 
                                                               FreqAnneeChoisie$numSortie==x)})
      names(tirage) <- seq(1,length(tirage))
      
      num <- unlist(lapply(names(tirage),
                           FUN=function(x){tirage[[x]]$num <- rep(x,nrow(tirage[[x]])) ;
                           tirage[[x]]$numBoot<-paste(tirage[[x]]$numSortie,tirage[[x]]$num)}))
      freqEchant <- do.call("rbind",tirage)
      freqEchant$numBoot <- num

      #### le tableau de fréquentation ré_échantillonné pour 1 itération est terminé
      #### les métriques sont recalculées sur ce nouveau tableau  freqEchant
    
    # certaines sorties ont été tirées plusieurs fois, FreqBoot$numBoot permet de notifier à R 
    # qu'il faut les considérer comme 2sorties différentes (= deux tirages)
    FreqBoot <- freqEchant  
    FreqBoot$numSortie <- FreqBoot$numBoot   

# Sys.time()  
    ### Calculs des extrapolations pour l'itération z
        estimBoot <- LancementExtrapolationFacteursSep.f (tab = FreqBoot, 
                                                          niveauSpatial = niveauSpatial, 
                                                          niveauTemporel = niveauTemporel, 
                                                          variable = variable,
                                                          periode = anneeChoisie, 
                                                          rgpmtJour = "typeJours", 
                                                          facteurSep = facteurSep, 
                                                          modalites = modalite, 
                                                          titre = paste("pour le facteur",facteurSep, "=",modalite), 
                                                          graph = F,
                                                          statBoot = NULL)
                                               
      # FreqEstimee [,,,z] <- estimBoot [, match(colnames(FreqEstimee) , names(estimBoot)[[2]]), match(dimnames(FreqEstimee)[[3]] , names(estimBoot)[[3]])]
       FreqEstimee [,,"FreqJS",z] <- estimBoot[[1]] 
       FreqEstimee [,,"FreqJW",z] <- estimBoot[[2]] 
       
    } # fin du bootstrap  (boucle sur z itérations)
    
    
     ## ESTIMATIONS PAR BOOTSTRAP
    
     # nombre de bateaux toutes activités confondues
      MoyBoot <- apply(FreqEstimee, c(1,2,3), mean,na.rm=T)
      VarBoot <- apply(FreqEstimee, c(1,2,3), var,na.rm=T)
      InfBoot <- apply(FreqEstimee, c(1,2,3), quantile, probs=0.025, na.rm=T)
      SupBoot <- apply(FreqEstimee, c(1,2,3), quantile, probs=0.975, na.rm=T)

    # tableau de stockage des estimations bootstrap  
      StatBoot <- array(NA, dim = c(nbSpatial, 
                                    nbTemporel,
                                    2, 
                                    4), 
                               dimnames = c(list(rownames(MoyBoot)),
                                            list(colnames(MoyBoot)),
                                            list(dimnames(MoyBoot)[[3]]), 
                                            list(c("moyenneEstimee", "varianceEstimee", "ICInfEstime", "ICSupEstime"))))
      StatBoot [,,,"moyenneEstimee"] <- MoyBoot
      StatBoot [,,,"varianceEstimee"] <- VarBoot
      StatBoot [,,,"ICInfEstime"] <- InfBoot
      StatBoot [,,,"ICSupEstime"] <- SupBoot
      
      sink(paste("C:/PAMPA/Resultats_Usages/bootstrap/estimation par type de jour de ",  
                                    variable, " pour ", facteurSep, " = ", modalite, 
                                    " par ", niveauSpatial, " et ", 
                                    niveauTemporel, ".txt", sep=""))
      print(StatBoot)
      sink()    
      return (StatBoot)
 #   Sys.time()  ### affiche la date et l'heure
}


################################################################################