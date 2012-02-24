################################################################################
# Nom               : FonctionsGraphFrequentation.r
# Type              : Programme
# Objet             : Fonctions nécessaires pour les calculs et le lancement des
#                     graphiques relatifs aux données de fréquentation. 
#                     Ces fonctions seront appelées par l'interface de fréquentation.
# Input             : aucun
# Output            : lancement de fonctions
# Auteur            : Elodie Gamp & Yves Reecht
# R version         : 2.11.1
# Date de création  : Mai 2011
# Sources
################################################################################

########################################################################################################################
CreationTabintermediaireFreq.f <- function(tab, niveauSpatial, niveauTemporel, variable)
{
    ## Purpose: Retourne un tableau en 4D contenant la fréquentation moyenne
    ##          en nombre de "variables" par mois, niveauSpatial, type de jour
    ##          et periodEchant
    ## ----------------------------------------------------------------------
    ## Arguments: tab : tableau de données à considérer
    ##            niveauSpatial : niveau spatial choisi pour le calcul
    ##            niveauTemporel : niveau temporel choisi pour le calcul
    ##            variable : la variable choisie pour le calcul
    ## ----------------------------------------------------------------------
    ## Author: Elodie Gamp, Date: 15 sept. 2011

    # tableau de base de fréquentation : niveauSpatial*mois*type de jour*sortie*periodEchant       observation par niveauSpatial par sortie
    FreqJour <- tapply(tab[,variable],list(tab$mois,
                                          factor(tab[,niveauSpatial]) , factor(tab$typeJsimp),
                                          factor(tab$numSortie) , factor(tab$periodEchant)),
                                          sum , na.rm=T)

    # tableau 4D : fréquentation moyenne par mois, niveauSpatial, type de jour et periodEchant
    MoyFreqJour <- apply(FreqJour,c(1,2,3,5),mean,na.rm=T)

    return(MoyFreqJour)
}

########################################################################################################################
PourCreationInfoJour.f <- function(calendrierGeneral)
{
    ## Purpose: Crée un tableau 3D d'informations selon les types de jours
    ##          1 = Annee*mois*JS
    ##          2 = Annee*mois*JW
    ##          3 = Annee*mois*niveauTemporelChoisi*JS
    ##          4 = Annee*mois*niveauTemporelChoisi*JW
    ## ----------------------------------------------------------------------
    ## Arguments: calendrierGeneral : le tableau comptabilisant les jours par niveau temporel
    ## ----------------------------------------------------------------------
    ## Author: Elodie Gamp, Date: 15 sept. 2011

    ## Types de graphiques possibles pour les données de fréquentation

  # formation du tableau 3D d'informations selon les types de jours Annee*mois*typeJ
  nbAnnee <- length(unique(calendrierGeneral$periodEchant))
  nbMois <-  length(unique(calendrierGeneral$mois))
  InfoJour <- array (0,c(nbAnnee, nbMois, 4),
                        list(unique(calendrierGeneral$periodEchant),
                             unique(calendrierGeneral$mois),
                             c("nbJS","nbJW","nbJStemp","nbJWtemp")))

    InfoJour [,,"nbJS"] <- tapply(calendrierGeneral$nbJS ,
                                  list(calendrierGeneral$periodEchant, calendrierGeneral$mois),
                                  sum, na.rm=T)     # nb de jours semaine par mois et période échant
    InfoJour [,,"nbJW"] <- tapply(calendrierGeneral$nbJW,
                                  list(calendrierGeneral$periodEchant, calendrierGeneral$mois),
                                  sum, na.rm=T)

  ##  rajout du nombre de jour par type de jour selon le niveau temporel choisi
    InfoJour [,,"nbJStemp"] <- tapply(calendrierGeneral$nbJStemp,
                                      list(calendrierGeneral$periodEchant , calendrierGeneral$mois),
                                      sum , na.rm=T)                                                # nb de JS par mois/niveauTemporel et periodEchant

    InfoJour [,,"nbJWtemp"] <- tapply(calendrierGeneral$nbJWtemp,
                                      list(calendrierGeneral$periodEchant , calendrierGeneral$mois),
                                      sum , na.rm=T)                                                # les mois d'une même année et même trimestre ont la même valeur

    return(InfoJour)

}

########################################################################################################################
CreationInfoJour.f <- function(niveauTemporel)
{
    ## Purpose: Retourne un tableau en 3D contenant la répartition du nombre de jours
    ##          selon le type de jour, le mois et l'année ou
    ##          selon le type de jour, le niveauTemporel et l'année
    ## ----------------------------------------------------------------------
    ## Arguments: niveauTemporel : niveau temporel choisi pour le calcul
    ## ----------------------------------------------------------------------
    ## Author: Elodie Gamp, Date: 15 sept. 2011

    ## rajout d'une colonne correspondant à periodEchant_niveauTemporel
    calendrierGeneral$Temp <- paste(calendrierGeneral$periodEchant , calendrierGeneral[,niveauTemporel])

    ## rajout des colonnes du nombre de jours par type de jours par niveauTemporel choisi dans calendrierGeneral
      InfoJourTempJS <- tapply(calendrierGeneral$nbJS , calendrierGeneral$Temp , sum , na.rm=T)   # nb de JS par niveau temporel choisi
      InfoJourTempJW <- tapply(calendrierGeneral$nbJW , calendrierGeneral$Temp , sum , na.rm=T)

    ### complément du calendrierGeneral
    ##### REMARQUE : (les mois d'une même année et même iveau temporel choisi ont la même valeur)
    calendrierGeneral$nbJStemp <- InfoJourTempJS[match(calendrierGeneral$Temp ,
                                                        names(InfoJourTempJS))]   # nb de JS par niveau temporel choisi
    calendrierGeneral$nbJWtemp <- InfoJourTempJW[match(calendrierGeneral$Temp ,
                                                        names(InfoJourTempJW))]   # nb de JW par niveau temporel choisi
    # formation du tableau 3D d'informations selon les types de jours Annee*mois*typeJ et Annee*mois*typeJ*niveauTemporel
    InfoJour <- PourCreationInfoJour.f(calendrierGeneral)

    return(InfoJour)
}

########################################################################################################################
MoyenneStratifieeTypeJ.f <- function(tab, MoyFreqJour , InfoJour , niveauTemporel)
{
    ## Purpose: Retourne un tableau en 3D contenant la moyenne stratifiée par
    ##          type de jour, niveauSpatial et niveauTemporel
    ##          Correspond à OPTION 1 : souhait de garder la distinction des types de jours
    ## ----------------------------------------------------------------------
    ## Arguments: tab : tableau de données à considérer
    ##            MoyFreqJour : tableau des moyennes par mois, niveauSpatial, type J et periodEchant
    ##            InfoJour : tableau de répartition des jours par type, mois, niveauTemp
    ##            niveauTemporel : niveau temporel choisi pour le calcul
    ## ----------------------------------------------------------------------
    ## Author: Elodie Gamp, Date: 15 sept. 2011

    # Tableau dans lequel seront stockée les moyennes
    MoyenneStratifiee <- array (0 , c(dim(MoyFreqJour)[1] , dim(MoyFreqJour)[2] ,
                                        2 , dim(MoyFreqJour)[4]) ,
                                list(dimnames(MoyFreqJour)[[1]] , dimnames(MoyFreqJour)[[2]],
                                    c("MoyFreqJSstrat", "MoyFreqJWstrat"), dimnames(MoyFreqJour)[[4]]))

    # calcul intermédiaire des dénominateurs
    denomJS <- InfoJour[, , "nbJS"] / InfoJour[, , "nbJStemp"]   # division du nbJSmois par la somme des nbJSniveauTemporel
    denomJW <- InfoJour[, , "nbJW"] / InfoJour[, , "nbJWtemp"]   # division du nbJWmois par la somme des nbJWniveauTemporel

    # supression dans le fichier denom des mois non échantillonnés
    denomJS <- denomJS[,is.element (colnames(denomJS),rownames(MoyFreqJour))]
    denomJW <- denomJW[,is.element (colnames(denomJW),rownames(MoyFreqJour))]
         
    ## moyennes stratifiées pour JS
    MoyenneStratifiee[, , "MoyFreqJSstrat",] <- MoyFreqJour[, , "JS",] * rep(denomJS [match(dimnames(MoyFreqJour)[[4]] ,
                                                                                            dimnames(denomJS)[[1]]),] ,
                                                                          dim(MoyFreqJour)[[2]])
    ## moyennes stratifiées pour JW
    MoyenneStratifiee[, , "MoyFreqJWstrat",] <- MoyFreqJour[, , "JW",] * rep(denomJW [match(dimnames(MoyFreqJour)[[4]] ,
                                                                                            dimnames(denomJW)[[1]]),] ,
                                                                          dim(MoyFreqJour)[[2]])
    ### Somme par niveauTemporel choisi
       if (length(unique(tab[, niveauTemporel]))!=1) {

        ## Indice des niveauTemporel ordonnés selon les mois (! être sûr que tous les mois sont présents dans le calendrier!!!) :
        calendrierGeneral2 <-calendrierGeneral[is.element(calendrierGeneral$mois,rownames(MoyFreqJour)),]     # enlève les mois non échantillonnés
        
        Indx <- unique(calendrierGeneral2[order(calendrierGeneral2$mois) ,
                                       c(niveauTemporel, "mois")])[, niveauTemporel]

        ## Apply sur les dimensions inchangées
          MoyStrat <- apply(X = MoyenneStratifiee,
                            MARGIN = c(2:4),
                            FUN = tapply, INDEX = Indx, sum, na.rm=TRUE)      # avec tapply + ses arguments 2 à 4.
      } else {
          MoyStrat <- apply(MoyenneStratifiee, c(2:4), sum, na.rm=TRUE)      # si un seul niveau pour niveauTemporel choisi, calcul un peu différent 
      }

      return(MoyStrat)

}

########################################################################################################################
MoyenneStratifieeTousJ.f <- function(tab, MoyFreqJour , InfoJour , niveauTemporel)
{
    ## Purpose: Retourne un tableau en 3D contenant la moyenne stratifiée par
    ##          niveauSpatial et niveauTemporel (types de jour confondus)
    ##          Correspond à OPTION 2 : souhait de ne pas garder la distinction type jours
    ## ----------------------------------------------------------------------
    ## Arguments: tab : tableau de données à considérer
    ##            MoyFreqJour : tableau des moyennes par mois, niveauSpatial, type J et periodEchant
    ##            InfoJour : tableau de répartition des jours par type, mois, niveauTemp
    ##            niveauTemporel : niveau temporel choisi pour le calcul
    ## ----------------------------------------------------------------------
    ## Author: Elodie Gamp, Date: 15 sept. 2011

    # Tableau dans lequel seront stockée les moyennes
    MoyenneStratifiee <- array (0 , c(dim(MoyFreqJour)[1] , dim(MoyFreqJour)[2] ,
                                        2 , dim(MoyFreqJour)[4]) ,
                                list(dimnames(MoyFreqJour)[[1]] , dimnames(MoyFreqJour)[[2]],
                                 c("MoyFreqJSstrat","MoyFreqJWstrat"), dimnames(MoyFreqJour)[[4]]))

    # calcul intermédiaire des dénominateurs selon le niveau temporel choisi
    denomJS <- InfoJour[, , "nbJS"] / (InfoJour[, , "nbJStemp"] + InfoJour[, , "nbJWtemp"])   # division du nbJSmois par la somme des nbJS+nbJWniveauTemporel
    denomJW <- InfoJour[, , "nbJW"] / (InfoJour[, , "nbJStemp"] + InfoJour[, , "nbJWtemp"])   # division du nbJWmois par la somme des nbJS+nbJWniveauTemporel

    # supression dans le fichier denom des mois non échantillonnés
    denomJS <- denomJS[,is.element (colnames(denomJS),rownames(MoyFreqJour))]
    denomJW <- denomJW[,is.element (colnames(denomJW),rownames(MoyFreqJour))]

    ## moyennes stratifiées pour JS
    MoyenneStratifiee[, , "MoyFreqJSstrat",] <- MoyFreqJour[, , "JS",] * rep(denomJS [match(dimnames(MoyFreqJour)[[4]] ,
                                                                                            dimnames(denomJS)[[1]]),] ,
                                                                         dim(MoyFreqJour)[[2]])
    ## moyennes stratifiées pour JW
    MoyenneStratifiee[, , "MoyFreqJWstrat",] <- MoyFreqJour[, , "JW",] * rep(denomJW [match(dimnames(MoyFreqJour)[[4]] ,
                                                                                            dimnames(denomJW)[[1]]),] ,
                                                                         dim(MoyFreqJour)[[2]])
    ### Somme par niveauTemporel choisi
      if (length(unique(tab[,niveauTemporel]))!=1) {

        ## Indice des niveauTemporel ordonnés selon les mois (! être sûr que tous les mois sont présents dans le calendrier!!!) :
        calendrierGeneral2 <-calendrierGeneral[is.element(calendrierGeneral$mois,rownames(MoyFreqJour)),]     # enlève les mois non échantillonnés
        
        Indx <- unique(calendrierGeneral2[order(calendrierGeneral2$mois) ,
                                       c(niveauTemporel, "mois")])[, niveauTemporel]

        ## Apply sur les dimensions inchangées
          MoyStrat1 <- apply(X = MoyenneStratifiee,
                              MARGIN = c(2:4),
                              FUN = tapply, INDEX = Indx, sum, na.rm = TRUE)    # avec tapply + ses arguments 2 à 4.
          MoyStrat <- apply(MoyStrat1, c(1,2,4), sum,na.rm=T)                   # somme des valeurs*poids des deux types de jours
      } else {

          ## Indice des niveauTemporel ordonnés selon les mois (! être sûr que tous les mois sont présents dans le calendrier!!!)
          Indx <- rep(unique(tab[, niveauTemporel]), dim(MoyenneStratifiee)[1])

          ## Apply sur les dimensions inchangées...
          MoyStrat1 <- apply(X = MoyenneStratifiee,
                            MARGIN = c(2:4),
                            FUN = tapply, INDEX=Indx, sum, na.rm=TRUE)          # avec tapply + ses arguments 2 à 4.

          MoyStrat <- apply(MoyStrat1, c(1,3), sum, na.rm=T)                    # somme des valeurs*poids des deux types de jours
       }

       return(MoyStrat)

}

########################################################################################################################
BoxplotFreqTypeJ.f <- function(tab, variable , niveauSpatial , niveauTemporel, MoyStrat, periode)
{
    ## Purpose: Lance la fonction graphique boxplot par
    ##          type de jour, niveauSpatial et niveauTemporel
    ##          Correspond à OPTION 1 : souhait de garder la distonction des types de jours
    ##          Formation du tableau des stats descriptives
    ## ----------------------------------------------------------------------
    ## Arguments: tab : tableau de données à considérer
    ##            variable : variable choisie pour le calcul de la fréquentation
    ##            niveauSpatial : niveau spatial choisi pour le calcul
    ##            niveauTemporel : niveau temporel choisi pour le calcul
    ##            MoyStrat : moyenne stratifiée par type de jours, niveauSpatial et niveauTemporel
    ##            periode : choix du regroupement des periodes d'échantillonnage
    ## ----------------------------------------------------------------------
    ## Author: Elodie Gamp, Date: 15 sept. 2011


    ## tableau temporaire de fréquentation sur les paramètres choisis
    freqZone = tapply(tab[, variable] , list(factor(tab$numSortie) , factor(tab[,niveauSpatial]) ,
                                             factor(tab[,niveauTemporel]) , factor(tab$typeJsimp)) ,
                      sum, na.rm = T)

    # formation de la fenêtre graphique
      x11(width = 50 , height = 30 , pointsize = 6)
      nbCol <- dim(freqZone)[3]
      par(mfrow = c(2 , nbCol) , oma = c(0 , 0 , 5 , 0))    # agrandissement de la marge externe supérieure (pour titre général)

    # boxplot appliqué sur chaque niveauTemporel
      if (length(unique(tab[ , niveauTemporel])) != 1) {
          sapply(seq(1 : dim(freqZone)[4]) ,                                    # premier sapply sur le type de jour
                FUN = function(j) {sapply(seq(1:dim(freqZone)[3]),              # deuxième sapply sur le niveau temporel
                                          FUN = function(x , j){if (sum(freqZone[, , x , j], na.rm=T)!=0) {
                                                                        boxplot(freqZone[, , x , j] ,
                                                                        main = paste(dimnames(freqZone)[[4]][j] , dimnames(freqZone)[[3]][x]) ,
                                                                        cex.main = 3 , cex.axis=2.5 , cex.names=2.5);
                                                                text(1 : dim(freqZone)[2] , max(freqZone[, , x , j] , na.rm=T) ,
                                                                     labels = round(MoyStrat[x , , j , ] , digits = 1)  ,col = "red" , cex=2.5)}} , j)})
      } else {
          sapply(seq(1:dim(freqZone)[4]),                                       # premier sapply sur le type de jour
                 FUN =function(j) {sapply(seq(1:dim(freqZone)[3]),              # deuxième sapply sur le niveau temporel
                                          FUN=function(x , j){if (sum(freqZone[, , x , j], na.rm=T)!=0) {
                                                                      boxplot(freqZone[, , x , j] ,
                                                                      main = paste(dimnames(freqZone)[[4]][j] , dimnames(freqZone)[[3]][x]) ,
                                                                      cex.main = 3 , cex.axis = 2.5 , cex.names = 2.5);
                                                              text(1 : dim(freqZone)[2] , max(freqZone[, , x , j] , na.rm=T) ,
                                                                   labels = round(MoyStrat[ , j ,],digits = 1) , col = "red" , cex = 2.5)}} , j)})
      }
      mtext(paste(unique(tab$periodEchant), "fréquentation en" , variable , "par type de jours, par" , niveauTemporel ,"et par" , niveauSpatial) ,
            line=2 , outer=TRUE , cex = 3)
      savePlot(filename = paste("C:/PAMPA/Resultats_Usages/frequentation/boxplotfréquentation en" ,
                                variable , "par type de jours, par" , niveauTemporel ,"et par" , 
                                niveauSpatial, unique(tab$periodEchant)), type = c("png"))

    # calcul des autres statistiques descriptives (min, max, mediane)
        # variance et IC sont calculés par bootstrap
    maxiFreq <- apply(freqZone , c(2:4) , max , na.rm=T)
    miniFreq <- apply(freqZone , c(2:4) , min , na.rm=T)
    medianeFreq <- apply(freqZone , c(2:4), median , na.rm=T)
    nbSortie <- apply(freqZone , c(2:4) ,lengthnna.f)
    TabStatDescrip <- list("nbSorties" = nbSortie , "moyenneStratifieeFreq" = MoyStrat ,
                           "miniFreq" = miniFreq , "maxiFreq" = maxiFreq ,
                           "medianeFreq" = medianeFreq)
    sink(paste("C:/PAMPA/Resultats_Usages/frequentation/frequentation" , variable , "par" , niveauSpatial , "et" , niveauTemporel , unique(tab$periodEchant), ".txt"))
    print(TabStatDescrip)
    sink()
}

########################################################################################################################
BoxplotFreqTousJ.f <- function(tab, variable , niveauSpatial , niveauTemporel, MoyStrat, periode)
{
    ## Purpose: Lance la fonction graphique boxplot par
    ##          niveauSpatial et niveauTemporel (types de jours confondus)
    ##          Correspond à OPTION 2 : souhait de ne pas garder la distinction type jours
    ##          Formation du tableau des stats descriptives
    ## ----------------------------------------------------------------------
    ## Arguments: tab : tableau de données à considérer
    ##            variable : variable choisie pour le calcul de la fréquentation
    ##            niveauSpatial : niveau spatial choisi pour le calcul
    ##            niveauTemporel : niveau temporel choisi pour le calcul
    ##            MoyStrat : moyenne stratifiée par type de jours, niveauSpatial et niveauTemporel
    ##            periode : choix du regroupement des periodes d'échantillonnage
    ## ----------------------------------------------------------------------
    ## Author: Elodie Gamp, Date: 15 sept. 2011

    freqZone = tapply(tab[ , variable] , list(factor(tab$numSortie) , factor(tab[,niveauSpatial]) ,
                                                factor(tab[,niveauTemporel])) ,
                      sum , na.rm=T)

    # formation de la fenêtre graphique
      x11(width = 50 , height = 30 , pointsize = 6)
      nbCol <- ceiling(dim(freqZone)[3] / 2)
      par(mfrow = c(2 , nbCol) , oma = c(0 , 0 , 5 , 0))    # agrandissement de la marge externe supérieure (pour titre général)

    # boxplot appliqué sur chaque niveauTemporel
      if (length(unique(tab[ , niveauTemporel])) != 1) {
          sapply(seq(1 : dim(freqZone)[3]) ,
                 FUN = function(x){if (sum(freqZone[, , x], na.rm=T)!=0) {
                                            boxplot(freqZone[, , x] ,
                                            main = dimnames(freqZone)[[3]][x] ,
                                            cex.main = 3 , cex.axis = 2.5 , cex.names = 2.5) ;
                                   text(1 : dim(freqZone)[2] , max(freqZone[, , x] , na.rm=T) ,
                                       labels = round(MoyStrat[x , ,] , digits = 1) , col = "red" , cex = 2.5)}})
      } else {
          sapply(seq(1 : dim(freqZone)[3]),
                    FUN = function(x){if (sum(freqZone[, , x], na.rm=T)!=0) {
                                            boxplot(freqZone[, , x] ,
                                              main = dimnames(freqZone)[[3]][x] ,
                                              cex.main = 3 , cex.axis = 2.5 , cex.names = 2.5) ;
                                      text(1 : dim(freqZone)[2] , max(freqZone[, , x] , na.rm=T) ,
                                           labels = round(MoyStrat[, x] , digits = 1) , col = "red" , cex = 2.5)}})
      }
      mtext(paste(unique(tab$periodEchant), "fréquentation en" , variable , "par" , niveauTemporel ,"et par" , niveauSpatial) ,
            line = 2 , outer = TRUE , cex = 3)
      savePlot(filename = paste("C:/PAMPA/Resultats_Usages/frequentation/boxplotfréquentation en" ,
                                variable , "par" , niveauTemporel ,"et par" , niveauSpatial, unique(tab$periodEchant)), type =c("png"))

    # calcul des autres statistiques descriptives (min, max, mediane)
        # variance et IC sont calculés par bootstrap
    maxiFreq <- apply(freqZone , c(2,3) , max , na.rm=T)
    miniFreq <- apply(freqZone , c(2,3) , min , na.rm=T)
    medianeFreq <- apply(freqZone , c(2,3) , median , na.rm=T)
    nbSortie <- apply(freqZone , c(2,3) , lengthnna.f)
    TabStatDescrip <- list("nbSorties" = nbSortie , "moyenneStratifieeFreq" = MoyStrat ,
                           "miniFreq" = miniFreq , "maxiFreq" = maxiFreq ,
                           "medianeFreq" = medianeFreq)
    sink(paste("C:/PAMPA/Resultats_Usages/frequentation/frequentation" , variable , "par" , niveauSpatial , "et" , niveauTemporel , unique(tab$periodEchant), ".txt"))
    print(TabStatDescrip)
    sink()

}


########################################################################################################################
CasParticulierFacteurSep.f <- function(tab, niveauSpatial, niveauTemporel, variable, facteurSep, modalites)
{
    ## Purpose: Adapte le tableau de fréquentation au choix d'un facteur de séparation
    ##          Permet ainsi de considérer les 0 lorsque la fréquentation est nulle pour
    ##          la modalité du facteur alors qu'elle ne l'est pas pour toutes modalités confondues
    ##          Puis retourne un tableau en 5D (similaire FreqJour + 1D contenant
    ##          la fréquentation moyenne en nombre de "variables" par mois, niveauSpatial,
    ##          type de jour et periodEchant sur le facteur de sélection choisi et les
    ##          valeurs des modalités choisies
    ## ----------------------------------------------------------------------
    ## Arguments: tab : tableau de fréquentation considéré (freqtotExt)
    ##            niveauSpatial : niveau spatial choisi pour le calcul
    ##            niveauTemporel : niveau temporel choisi pour le calcul
    ##            variable : la variable choisie pour le calcul
    ##            facteurSep : facteur de séparation choisi
    ##            modalites : modalites du facteur de séparation choisi
    ## ----------------------------------------------------------------------
    ## Author: Elodie Gamp, Date: 15 sept. 2011

    ## rajout de la colonne correspondant au niveau spatial choisi
  #  tab[,niveauSpatial] <- refSpatial[,niveauSpatial][match(tab$zone , refSpatial$codeZone)]

    ## rajout de la colonne correspondant au niveau temporel choisi
  #  tab[,niveauTemporel] <- calendrierGeneral[,niveauTemporel][match(tab$moisAn , calendrierGeneral$moisAn)]

  #  tab <- dropLevels.f(tab, which=niveauSpatial)

  #### manip de transfo de fréquentation (FreqJour) pour prise en compte des 0 lors d'une sélection

   ## Tableau de fréquentation par valeur du facteur (non corrigé)
   Freq1 <- with(tab,
                tapply(tab[ , variable],
                        as.list(tab[, c("mois", niveauSpatial,                  # éléments nommés pour avoir les noms de dimensions dans le tableau
                                        "typeJsimp", "numSortie",
                                        "periodEchant", facteurSep)]),          # facteurSep défini le champ du facteur de séparation
                        sum, na.rm=TRUE))

   ## Indice des ensembles caractéristiques/sorties  existantes (hors sélection)
   idx <- with(tab,
              tapply(tab[ , variable],
                      as.list(tab[, c("mois", niveauSpatial,                    # éléments nommés pour avoir les noms de dimensions dans le tableau
                                      "typeJsimp", "numSortie",
                                      "periodEchant")]),                        # Les mêmes moins le champ de sélection
                     function(x)ifelse(length(x), TRUE, FALSE)))

   ## Renvoie quand même NA si inexistante => on remplace par FALSE
   idx[is.na(idx)] <- FALSE

   # Tableau de fréquentation par facteurSep, corrigé pour
   ## tenir compte des activités non observées sur sorties effectives :
    FreqCor1 <- sweep(Freq1,
                 which(is.element(names(dimnames(Freq1)),           # Dimensions de Freq1 communes avec idx
                                  names(dimnames(idx)))),           # (caractéristiques de sorties existantes).
                     idx,
                     function(x, y)
                     {
                         x[is.na(x) & y] <- 0                       # on ne remplace que les NAs des sorties existantes.
                         return(x)
                     })

    # lors d'une sélection, FreqCor remplace le tableau 5D FreqJour
    # Attention, 1D de plus dans FreqCor. Les fonctions suivantes ne seront appliquées que pour les modalités choisies
    Agarde <- dimnames(FreqCor1)[[6]] [is.element(dimnames(FreqCor1)[[6]] , modalites)]
    FreqCor <- FreqCor1[, , , , , Agarde, drop=F]                                        # réduit le tableau aux modalités choisies

    # tableau 5D : fréquentation moyenne par mois, niveauSpatial, type de jour, periodEchant et facteurSep
    MoyFreqJourFact <- apply(FreqCor, c(1,2,3,5,6), mean, na.rm=T)

    return(MoyFreqJourFact)

}


########################################################################################################################
BoxplotFreqTypeJFacteur.f <- function(tab, variable , niveauSpatial , niveauTemporel, MoyStrat, periode, facteurSep, modalites)
{
    ## Purpose: POUR LES FACTEURS DE SEPARATION : Lance la fonction graphique boxplot par
    ##          type de jour, niveauSpatial et niveauTemporel
    ##          Correspond à OPTION 1 : souhait de garder la distonction des types de jours
    ##          Formation du tableau des stats descriptives
    ## ----------------------------------------------------------------------
    ## Arguments: tab : tableau de fréquentation à considérer (freqtotExt)
    ##            variable : variable choisie pour le calcul de la fréquentation
    ##            niveauSpatial : niveau spatial choisi pour le calcul
    ##            niveauTemporel : niveau temporel choisi pour le calcul
    ##            MoyStrat : moyenne stratifiée par type de jours, niveauSpatial et niveauTemporel
    ##            periode : choix du regroupement des periodes d'échantillonnage
    ##            facteurSep : facteur de séparation choisi
    ##            modalites : valeurs du facteur de séparation considérées
    ## ----------------------------------------------------------------------
    ## Author: Elodie Gamp, Date: 15 sept. 2011

    ## ## rajout de la colonne correspondant au niveau spatial choisi
    ## tab[,niveauSpatial] <- refSpatial[,niveauSpatial][match(tab$zone , refSpatial$codeZone)]

    ## ## rajout de la colonne correspondant au niveau temporel choisi
    ## tab[,niveauTemporel] <- calendrierGeneral[,niveauTemporel][match(tab$moisAn , calendrierGeneral$moisAn)]

  #### manip de transfo de fréquentation (FreqJour) pour prise en compte des 0 lors d'une sélection

   ## Tableau de fréquentation par valeur du facteur (non corrigé)
   Freq1 <- with(tab,
                tapply(tab[ , variable],
                        as.list(tab[, c("numSortie", niveauSpatial,                     # éléments nommés pour avoir les noms de dimensions dans le tableau
                                        niveauTemporel, "typeJsimp", facteurSep )]),    # facteurSep défini le champ du facteur de séparation
                        sum, na.rm=TRUE))

   ## Indice des ensembles caractéristiques/sorties  existantes (hors sélection)
   idx <- with(tab,
              tapply(tab[ , variable],
                        as.list(tab[, c("numSortie", niveauSpatial,                     # éléments nommés pour avoir les noms de dimensions dans le tableau
                                        niveauTemporel, "typeJsimp" )]),                # Les mêmes moins le champ de sélection    , facteurSep
                     function(x)ifelse(length(x), TRUE, FALSE)))

   ## Renvoie quand même NA si inexistante => on remplace par FALSE
   idx[is.na(idx)] <- FALSE

  # Tableau de fréquentation par facteurSep, corrigé pour
   ## tenir compte des activités non observées sur sorties effectives :
    FreqCor1 <- sweep(Freq1,
                 which(is.element(names(dimnames(Freq1)),               # Dimensions de Freq1 communes avec idx
                                  names(dimnames(idx)))),               # (caractéristiques de sorties existantes).
                 idx,
                 function(x, y)
             {
                 x[is.na(x) & y] <- 0                                   # on ne remplace que les NAs des sorties existantes.
                 return(x)
             })

    # lors d'une sélection, FreqCor remplace le tableau 5D FreqJour
    # Attention, 1D de plus dans FreqCor. Les fonctions suivantes ne seront appliquées que pour les modalités choisies
    Agarde <- dimnames(FreqCor1)[[5]] [is.element(dimnames(FreqCor1)[[5]] , modalites)]
    freqZone <- FreqCor1[, , , , Agarde, drop=F]                                        # réduit le tableau aux modalités choisies

    # formation de la fenêtre graphique
      x11(width = 50 , height = 30 , pointsize = 6)
      nbCol <- dim(freqZone)[3]
      par(mfrow = c(2 , nbCol) , oma = c(0 , 0 , 5 , 0))    # agrandissement de la marge externe supérieure (pour titre général)

    # boxplot appliqué sur chaque niveauTemporel
      if (length(unique(tab[ , niveauTemporel])) != 1) {
          sapply(seq(1 : dim(freqZone)[4]) ,                                   # premier sapply sur le type de jour
                FUN = function(j) {sapply(seq(1:dim(freqZone)[3]),           # deuxième sapply sur le niveau temporel
                                          FUN = function(x , j){if (sum(freqZone[, , x , j ,], na.rm=T)!=0) {
                                                                        boxplot(freqZone[, , x , j ,] ,
                                                                        main = paste(dimnames(freqZone)[[4]][j] , dimnames(freqZone)[[3]][x]) ,
                                                                        cex.main = 3 , cex.axis=2.5 , cex.names=2.5);
                                                                text(1 : dim(freqZone)[2] , max(freqZone[, , x , j ,] , na.rm=T) ,
                                                                     labels = round(MoyStrat[x , , j , ] , digits = 1)  ,col = "red" , cex=2.5)}} , j)})
      } else {
          sapply(seq(1:dim(freqZone)[4]),                                   # premier sapply sur le type de jour
                 FUN =function(j) {sapply(seq(1:dim(freqZone)[3]),           # deuxième sapply sur le niveau temporel
                                          FUN=function(x , j){if (sum(freqZone[, , x , j ,], na.rm=T)!=0) {
                                                                      boxplot(freqZone[, , x , j ,] ,
                                                                      main = paste(dimnames(freqZone)[[4]][j] , dimnames(freqZone)[[3]][x]) ,
                                                                      cex.main = 3 , cex.axis = 2.5 , cex.names = 2.5);
                                                              text(1 : dim(freqZone)[2] , max(freqZone[, , x , j ,] , na.rm=T) ,
                                                                   labels = round(MoyStrat[ , j ,],digits = 1) , col = "red" , cex = 2.5)}} , j)})
      }
      if (sum(freqZone, na.rm=T)!=0) {
        mtext(paste(unique(tab$periodEchant), "fréquentation en" , variable , "par type de jours, par" , 
                    niveauTemporel ,"et par" , niveauSpatial, facteurSep, "=", modalites) ,
              line=2 , outer=TRUE , cex = 3)
        savePlot(filename = paste("C:/PAMPA/Resultats_Usages/frequentation/boxplotfréquentation en" ,
                                  variable , "par type de jours, par" , niveauTemporel ,"et par" , 
                                  niveauSpatial, unique(tab$periodEchant), facteurSep, "=", modalites), type =c("png"))
    }
    # calcul des autres statistiques descriptives (min, max, mediane)
        # variance et IC sont calculés par bootstrap
    maxiFreq <- apply(freqZone , c(2:4) , max , na.rm=T)
    miniFreq <- apply(freqZone , c(2:4) , min , na.rm=T)
    medianeFreq <- apply(freqZone , c(2:4), median , na.rm=T)
    nbSortie <- apply(freqZone , c(2:4) ,lengthnna.f)
    TabStatDescrip <- list("nbSorties" = nbSortie , "moyenneStratifieeFreq" = MoyStrat ,
                           "miniFreq" = miniFreq , "maxiFreq" = maxiFreq ,
                           "medianeFreq" = medianeFreq)
    sink(paste("C:/PAMPA/Resultats_Usages/frequentation/frequentation" , variable , "par" , 
              niveauSpatial , "et" , niveauTemporel , unique(tab$periodEchant), facteurSep, "=", modalites, ".txt"))
    print(TabStatDescrip)
    sink()
}

########################################################################################################################
BoxplotFreqTousJFacteur.f <- function(tab, variable , niveauSpatial , niveauTemporel, MoyStrat, periode, facteurSep, modalites)
{
    ## Purpose: Lance la fonction graphique boxplot par
    ##          niveauSpatial et niveauTemporel (types de jours confondus)
    ##          Correspond à OPTION 2 : souhait de ne pas garder la distinction type jours
    ##          Formation du tableau des stats descriptives
    ## ----------------------------------------------------------------------
    ## Arguments: tab : tableau de fréquentation à considérer (freqtotExt)
    ##            variable : variable choisie pour le calcul de la fréquentation
    ##            niveauSpatial : niveau spatial choisi pour le calcul
    ##            niveauTemporel : niveau temporel choisi pour le calcul
    ##            MoyStrat : moyenne stratifiée par type de jours, niveauSpatial et niveauTemporel
    ##            periode : choix du regroupement des periodes d'échantillonnage
    ##            facteurSep : facteur de séparation choisi
    ##            modalites : valeurs du facteur de séparation considérées
    ## ----------------------------------------------------------------------
    ## Author: Elodie Gamp, Date: 15 sept. 2011

    ## rajout de la colonne correspondant au niveau spatial choisi
    tab[,niveauSpatial] <- refSpatial[,niveauSpatial][match(tab$zone , refSpatial$codeZone)]

    ## rajout de la colonne correspondant au niveau temporel choisi
    tab[,niveauTemporel] <- calendrierGeneral[,niveauTemporel][match(tab$moisAn , calendrierGeneral$moisAn)]

  #### manip de transfo de fréquentation (FreqJour) pour prise en compte des 0 lors d'une sélection

   ## Tableau de fréquentation par valeur du facteur (non corrigé)
   Freq1 <- with(tab,
                tapply(tab[ , variable],
                        as.list(tab[, c("numSortie", niveauSpatial,             # éléments nommés pour avoir les noms de dimensions dans le tableau
                                        niveauTemporel, facteurSep )]),         # facteurSep défini le champ du facteur de séparation
                        sum, na.rm=TRUE))

   ## Indice des ensembles caractéristiques/sorties  existantes (hors sélection)
   idx <- with(tab,
              tapply(tab[ , variable],
                        as.list(tab[, c("numSortie", niveauSpatial,             # éléments nommés pour avoir les noms de dimensions dans le tableau
                                        niveauTemporel )]),                     # Les mêmes moins le champ de sélection  , facteurSep
                     function(x)ifelse(length(x), TRUE, FALSE)))

   ## Renvoie quand même NA si inexistante => on remplace par FALSE
   idx[is.na(idx)] <- FALSE

  # Tableau de fréquentation par facteurSep, corrigé pour
   ## tenir compte des activités non observées sur sorties effectives :
    FreqCor1 <- sweep(Freq1,
                 which(is.element(names(dimnames(Freq1)),                       # Dimensions de Freq1 communes avec idx
                                  names(dimnames(idx)))),                       # (caractéristiques de sorties existantes).
                 idx,
                 function(x, y)
             {
                 x[is.na(x) & y] <- 0                                           # on ne remplace que les NAs des sorties existantes.
                 return(x)
             })

    # lors d'une sélection, FreqCor remplace le tableau 5D FreqJour
    # Attention, 1D de plus dans FreqCor. Les fonctions suivantes ne seront appliquées que pour les modalités choisies
    Agarde <- dimnames(FreqCor1)[[4]] [is.element(dimnames(FreqCor1)[[4]] , modalites)]
    freqZone <- FreqCor1[, , , Agarde, drop=F]                                        # réduit le tableau aux modalités choisies

    # formation de la fenêtre graphique
      x11(width = 50 , height = 30 , pointsize = 6)
      nbCol <- ceiling(dim(freqZone)[3] / 2)
      par(mfrow = c(2 , nbCol) , oma=c(0 , 0 , 5 , 0))    # agrandissement de la marge externe supérieure (pour titre général)

    # boxplot appliqué sur chaque niveauTemporel
      if (length(unique(tab[ , niveauTemporel])) != 1) {
          sapply(seq(1 : dim(freqZone)[3]) ,
                 FUN = function(x){if (sum(freqZone[, , x ,], na.rm=T)!=0) {
                                            boxplot(freqZone[, , x ,] ,
                                            main = dimnames(freqZone)[[3]][x] ,
                                            cex.main = 3 , cex.axis = 2.5 , cex.names = 2.5) ;
                                  text(1 : dim(freqZone)[2] , max(freqZone[, , x ,] , na.rm=T) ,
                                       labels = round(MoyStrat[x , ,] , digits = 1) , col = "red" , cex = 2.5)}})
      } else {
          sapply(seq(1 : dim(freqZone)[3]),
                    FUN = function(x){if (sum(freqZone[, , x ,], na.rm=T)!=0) {
                                              boxplot(freqZone[, , x ,] ,
                                              main = dimnames(freqZone)[[3]][x] ,
                                              cex.main = 3 , cex.axis = 2.5 , cex.names = 2.5) ;
                                      text(1 : dim(freqZone)[2] , max(freqZone[, , x ,] , na.rm=T) ,
                                           labels = round(MoyStrat[, x] , digits = 1) , col = "red" , cex = 2.5)}})
      }
      if (sum(freqZone, na.rm=T)!=0) {
        mtext(paste(unique(tab$periodEchant), "fréquentation en" , variable , "par" , niveauTemporel ,
                    "et par" , niveauSpatial, facteurSep, "=", modalites) ,
              line = 2 , outer = TRUE , cex = 3)
        savePlot(filename = paste("C:/PAMPA/Resultats_Usages/frequentation/boxplotfréquentation en" ,
                                  variable , "par" , niveauTemporel ,"et par" , niveauSpatial, 
                                  unique(tab$periodEchant), facteurSep, "=", modalites), type =c("png"))
     }
    # calcul des autres statistiques descriptives (min, max, mediane)
        # variance et IC sont calculés par bootstrap
    maxiFreq <- apply(freqZone , c(2,3) , max , na.rm=T)
    miniFreq <- apply(freqZone , c(2,3) , min , na.rm=T)
    medianeFreq <- apply(freqZone , c(2,3) , median , na.rm=T)
    nbSortie <- apply(freqZone , c(2,3) , lengthnna.f)
    TabStatDescrip <- list("nbSorties" = nbSortie , "moyenneStratifieeFreq" = MoyStrat ,
                           "miniFreq" = miniFreq , "maxiFreq" = maxiFreq ,
                           "medianeFreq" = medianeFreq)
    sink(paste("C:/PAMPA/Resultats_Usages/frequentation/frequentation" , variable , "par" , 
               niveauSpatial , "et" , niveauTemporel , unique(tab$periodEchant), facteurSep, "=", modalites, ".txt"))
    print(TabStatDescrip)
    sink()

}


########################################################################################################################
TransfoDoubleActivite.f <- function(tab, facteurSep)
{
    ## Purpose: Transforme le tableau initial tab afin de comptabiliser les
    ##          doubles activités/catégories d'activité et n'avoir plus qu'une
    ##          seule colonne avec les deux activités
    ## ----------------------------------------------------------------------
    ## Arguments: tab : tableau de données à considérer
    ##            facteurSep : facteur de séparation choisi
    ## ----------------------------------------------------------------------
    ## Author: Elodie Gamp, Date: 15 sept. 2011


     ## Sélection des lignes avec deux activités/catégories et duplication en deux tableaux
    if (is.element(facteurSep, c("act1")))
    {
        tmp <- tab[!is.na(tab$act1) & !is.na(tab$act2), ]
    } else {
        tmp <- tab[!is.na(tab$categAct1) & !is.na(tab$categAct2), ]
    }

    tmpAct1 <- (tmpAct2 <- tmp)


    ## Suppression respective des deux colonnes
    if (nrow(tmpAct1) != 0)
    {
        tmpAct1$act2 <- NA                      # (on ne conserve que l'info de act1)
    } else {}
    if (nrow(tmpAct2) != 0)
    {
        tmpAct2$act1 <- NA                      # (on ne conserve que l'info de act2)
    } else {}


    ## Obtention du tableau avec les lignes dupliquées
    if (is.element(facteurSep, c("act1")))        # activités
    {
        tmp <- tab[!is.na(tab$act1) & !is.na(tab$act2), ]

        freqtotExt <- rbind(tab[is.na(tab$act1) |                   # lignes avec juste une activité
                                is.na(tab$act2), ],
                            rbind(tmpAct1,                          # + deux tableaux pour (un avec act1 et l'autre avec act2)
                                  tmpAct2))                         # dupliquer les lignes avec deux activités

        rm(list=c("tmpAct1", "tmpAct2"))                            # pas la peine de les conserver.

        ## Création d'une unique colonne d'activité à partir de act1 et act2
        freqtotExt$act <- as.factor(apply(freqtotExt[ , c("act1", "act2")], 1,
                                    function(x)
                                    {
                                        ifelse(all(is.na(x)), NA, x[!is.na(x)])
                                    }))

    } else {            # catégories d'activités

        tmp <- tab[!is.na(tab$categAct1) & !is.na(tab$categAct2), ]

        freqtotExt <- rbind(tab[is.na(tab$categAct1) |              # lignes avec juste une activité.
                                is.na(tab$categAct2), ],
                            rbind(tmpAct1,                          # + deux tableaux pour
                                  tmpAct2))                         # dupliquer les lignes avec deux activités.

        rm(list=c("tmpAct1", "tmpAct2"))    # pas la peine de les conserver.

        ## Création d'une unique colonne d'activité à partir de act1 et act2
        freqtotExt$act <- as.factor(apply(freqtotExt[ , c("categAct1", "categAct2")], 1,
                                    function(x)
                                    {
                                        ifelse(all(is.na(x)), NA, x[!is.na(x)])
                                    }))

    }

    return(freqtotExt)

}


########################################################################################################################
