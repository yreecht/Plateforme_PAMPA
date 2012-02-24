################################################################################
# Nom               : CalculIndicateurComposite.r
# Type              : Programme
# Objet             : Ensemble des fonctions permettant de calculer les 
#                    indicateurs élémentaires et composites à partir des données
#                    d'enquêtes pêche
# Input             : data importées en txt
# Output            : tableaux et graphs
# Auteur            : Elodie Gamp (sur la base de Pierre-Olivier Goffard)
# R version         : 2.11.1
# Date de création  : décembre 2011
# Sources
################################################################################

#####################         Indicateurs Composites pour la perception de l'AMP par ses usagers         #######################

# Nettoyage de la table en supprimmant toutes les valeurs manquante#
Nettoyage.f <- function(tab1){
  tab <- subset(tab1,tab1$existenceAMP!="NA" | tab1$suffiInfo!="NA" | 
                tab1$respectReg!="NA" | tab1$adaptReg!="NA" | 
                tab1$effetEcosyst!="NA" |tab1$effetEcono!="NA" |tab1$effetAct!="NA" )
  return(tab)
}
 
# Calcul des indicateurs élémentaires par variable
# Indicateurs élémentaires permettant le calcul de l'indicateur de Perception/Connaissance 
# par la méthode INSEE pour calculer l'indice de confiance des ménages

# Indicateur de connaissance de l'AMP
IndicExistenceAMP.f  <- function(tab2){
  AvisPos <- subset(tab2, tab2$existenceAMP == "oui")
  AvisNeg <- subset(tab2, tab2$existenceAMP == "non")
  IndicIntermed <- (nrow(AvisPos) - nrow(AvisNeg)) / 
                    (nrow(AvisPos) + nrow(AvisNeg))
  IndicElem <- (IndicIntermed/2) + 0.5
  return(round(IndicElem, digits = 2))
}

# Indicateur de la suffisance de l'information
IndicSuffiInfo.f <- function(tab2){
  AvisPos <- subset(tab2, tab2$suffiInfo == "oui")
  AvisNeg <- subset(tab2, tab2$suffiInfo == "non")
  IndicIntermed <- (nrow(AvisPos) - nrow(AvisNeg)) / 
                    (nrow(AvisPos) + nrow(AvisNeg))
  IndicElem <- (IndicIntermed/2) + 0.5
  return(round(IndicElem, digits = 2))
}

# Indicateur sur le respect de la réglementation
IndicRespectReg.f   <-  function(tab2){
  AvisPos <- subset(tab2, tab2$respectReg == "oui")
  AvisNeg <- subset(tab2, tab2$respectReg == "non")
  IndicIntermed <- (nrow(AvisPos) - nrow(AvisNeg)) / 
                    (nrow(AvisPos) + nrow(AvisNeg))
  IndicElem  <-  (IndicIntermed/2) + 0.5
  return(round(IndicElem, digits = 2))
}

# Indicateur de pertinence de la règlementation
IndicAdaptReg.f  <- function(tab2){
  AvisBien <- subset(tab2, tab2$adaptReg == "bien")
  AvisInada <- subset(tab2, tab2$adaptReg == "inada")
  AvisTrop <- subset(tab2, tab2$adaptReg == "trop")
  AvisInsu <- subset(tab2, tab2$adaptReg == "insu")
  IndicIntermed <- (nrow(AvisBien) - nrow(AvisInada) - nrow(AvisTrop) - nrow(AvisInsu)) / 
                    (nrow(AvisBien) + nrow(AvisInada) + nrow(AvisTrop) + nrow(AvisInsu))
  IndicElem <- (IndicIntermed/2) + 0.5
  return(round(IndicElem, digits = 2))
}

# Indicateurs élémentaires permettant le calcul de l'indicateur de Perception/Performance 
# par la méthode INSEE pour calculer l'indice de confiance des ménages
# A noter que l'on ne prend pas en compte la modalité neutre et que ce calcul correspond à la version permettant de faire le distinguo
# plutot positif/tres positif et plutot negatif/tres negatif 

# Indicateur de la perception de l'effet sur l'écosystème
IndicEcosyst.f  <-  function(tab){
  tab1 <- subset(tab, tab$existenceAMP == "oui")
#  AvisPos1 <- subset(tab1, tab1$effetEcosyst == "plutot_positif")
#  AvisNeg1 <- subset(tab1, tab1$effetEcosyst == "plutot_negatif")
  AvisPos2 <- subset(tab1, tab1$effetEcosyst == "tres_positif")
  AvisNeg2 <- subset(tab1, tab1$effetEcosyst == "tres_negatif")
  AvisPos <- subset(tab1, tab1$effetEcosyst == "plutot_positif" | tab1$effetEcosyst == "tres_positif")
  AvisNeg <- subset(tab1, tab1$effetEcosyst == "plutot_negatif" | tab1$effetEcosyst == "tres_negatif")
  AvisTous <- subset(tab1, tab1$effetEcosyst == "plutot_negatif" | tab1$effetEcosyst == "tres_negatif" | 
                              tab1$effetEcosyst == "plutot_positif" | tab1$effetEcosyst == "tres_positif")
  
  IndicIntermed <- ((1 + nrow(AvisPos2) / nrow(AvisTous)) * nrow(AvisPos) - 
                     (1 + nrow(AvisNeg2) / nrow(AvisTous)) * nrow(AvisNeg)) /
                   ((1 + nrow(AvisPos2) / nrow(AvisTous)) * nrow(AvisPos) + 
                     (1 + nrow(AvisNeg2) / nrow(AvisTous)) * nrow(AvisNeg))
  
  IndicElem <- IndicIntermed/2 + 0.5
  return(round(IndicElem, digits = 2))
}

# Indicateur de la perception de l'effet sur l'économie locale
IndicEcono.f  <-  function(tab){
  tab1 <- subset(tab, tab$existenceAMP == "oui")
#  AvisPos1 <- subset(tab1, tab1$effetEcno == "plutot_positif")
#  AvisNeg1 <- subset(tab1, tab1$effetEcono == "plutot_negatif")
  AvisPos2 <- subset(tab1, tab1$effetEcono == "tres_positif")
  AvisNeg2 <- subset(tab1, tab1$effetEcono == "tres_negatif")
  AvisPos <- subset(tab1, tab1$effetEcono == "plutot_positif" | tab1$effetEcono == "tres_positif")
  AvisNeg <- subset(tab1, tab1$effetEcono == "plutot_negatif" | tab1$effetEcono == "tres_negatif")
  AvisTous <- subset(tab1, tab1$effetEcono == "plutot_negatif" | tab1$effetEcono == "tres_negatif" | 
                              tab1$effetEcono == "plutot_positif" | tab1$effetEcono == "tres_positif")
  
  IndicIntermed <- ((1 + nrow(AvisPos2) / nrow(AvisTous)) * nrow(AvisPos) - 
                     (1 + nrow(AvisNeg2) / nrow(AvisTous)) * nrow(AvisNeg)) /
                   ((1 + nrow(AvisPos2) / nrow(AvisTous)) * nrow(AvisPos) + 
                     (1 + nrow(AvisNeg2) / nrow(AvisTous)) * nrow(AvisNeg))
  
  IndicElem <- IndicIntermed/2 + 0.5
  return(round(IndicElem, digits = 2))
}

# Indicateur de la perception de l'effet sur l'activité de l'usager
IndicAct.f  <-  function(tab){
  tab1 <- subset(tab, tab$existenceAMP == "oui")
#  AvisPos1 <- subset(tab1, tab1$effetAct == "plutot_positif")
#  AvisNeg1 <- subset(tab1, tab1$effetAct == "plutot_negatif")
  AvisPos2 <- subset(tab1, tab1$effetAct == "tres_positif")
  AvisNeg2 <- subset(tab1, tab1$effetAct == "tres_negatif")
  AvisPos <- subset(tab1, tab1$effetAct == "plutot_positif" | tab1$effetAct == "tres_positif")
  AvisNeg <- subset(tab1, tab1$effetAct == "plutot_negatif" | tab1$effetAct == "tres_negatif")
  AvisTous <- subset(tab1, tab1$effetAct == "plutot_negatif" | tab1$effetAct == "tres_negatif" | 
                              tab1$effetAct == "plutot_positif" | tab1$effetAct == "tres_positif")
  
  IndicIntermed <- ((1 + nrow(AvisPos2) / nrow(AvisTous)) * nrow(AvisPos) - 
                     (1 + nrow(AvisNeg2) / nrow(AvisTous)) * nrow(AvisNeg)) /
                   ((1 + nrow(AvisPos2) / nrow(AvisTous)) * nrow(AvisPos) + 
                     (1 + nrow(AvisNeg2) / nrow(AvisTous)) * nrow(AvisNeg))
  
  IndicElem <- IndicIntermed/2 + 0.5
  return(round(IndicElem, digits = 2))
}


# Création de la fonction permettant le calcul à partir des données de l'indice de Perception/Connaissance
IndicConnaissance.f <- function(tab2, Weight){
  IndicElemExistAMP <- IndicExistenceAMP.f(tab = tab2)
  IndicElemSuffiInfo  <- IndicSuffiInfo.f(tab = tab2)
  IndicElemRespectReg <- IndicRespectReg.f(tab = tab2)
  IndicElemAdaptReg  <- IndicAdaptReg.f(tab = tab2)
  IndicComposite <- round(Weight[1]*IndicElemExistAMP + Weight[2]*IndicElemSuffiInfo + Weight[3]*IndicElemRespectReg + Weight[4]*IndicElemAdaptReg, digits=2)
  return(IndicComposite)
}


# Création de la fonction permettant le calcul à partir des données de l'indice de Perception/Performance#
IndicPerformance.f <- function(tab,Weight){
  tab1 <- subset(tab, tab$existenceAMP == "oui")
  IndicElemEcosyst  <- IndicEcosyst.f(tab = tab1)
  IndicElemEcono  <- IndicEcono.f(tab = tab1)
  IndicElemAct  <- IndicAct.f(tab = tab1)
  IndicComposite <- round(Weight[1]*IndicElemEcosyst + Weight[2]*IndicElemEcono + Weight[3]*IndicElemAct, digits=2)
  return(IndicComposite)
}

# Méthode permettant de calculé la pondération en fonction du taux de NSP
# Pondération pour l'indicateur  de Perception/Connaissance
 WeightConnais.f  <-  function (tab){
 NSPexist <- subset(tab, tab$existenceAMP == "nsp" | tab$existenceAMP == "NSP")
 NSPinfo <- subset(tab, tab$suffiInfo == "nsp" | tab$suffiInfo == "NSP")
 NSPrespect <- subset(tab, tab$respectReg == "nsp" | tab$respectReg == "NSP")
 NSPadapt <- subset(tab, tab$adaptReg == "nsp" | tab$adaptReg == "NSP")
 OpinionExist <- length(tab[,1]) - length(NSPexist[,1])
 OpinionInfo <- length(tab[,1]) - length(NSPinfo[,1])
 OpinionRespect <- length(tab[,1]) - length(NSPrespect[,1])
 OpinionAdapt <- length(tab[,1]) - length(NSPadapt[,1])
 
 Weight <- c(OpinionExist / (OpinionExist+OpinionInfo+OpinionRespect+OpinionAdapt),
             OpinionInfo / (OpinionExist+OpinionInfo+OpinionRespect+OpinionAdapt),
             OpinionRespect / (OpinionExist+OpinionInfo+OpinionRespect+OpinionAdapt),
             OpinionAdapt /(OpinionExist+OpinionInfo+OpinionRespect+OpinionAdapt))
 return(Weight)
}
 
# Pondération pour l'indicateur de Perception/Performance
 WeightPerfo.f  <-  function (tab){
  tab1 <- subset(tab, tab$existenceAMP == "oui")
  NSPecosyst <- subset(tab1, tab1$effetEcosyst == "nsp" | tab1$effetEcosyst == "NSP")
  NSPecono <- subset(tab1, tab1$effetEcono == "nsp" | tab1$effetEcono == "NSP")
  NSPact <- subset(tab1, tab1$effetAct == "nsp" | tab1$effetAct == "NSP")
  OpinionEcosyst <- length(tab1[,1]) - length(NSPecosyst[,1])
  OpinionEcono <- length(tab1[,1]) - length(NSPecono[,1])
  OpinionAct <- length(tab1[,1]) - length(NSPact[,1])
  
  Weight <- c(OpinionEcosyst / (OpinionEcosyst + OpinionEcono + OpinionAct),
              OpinionEcono / (OpinionEcosyst + OpinionEcono + OpinionAct),
              OpinionAct / (OpinionEcosyst + OpinionEcono + OpinionAct))
  return(Weight)
}

# Ré-échantillonage Bootstrap
AMPboot.f  <-  function(tab){
# Création d'un dataframe vide disposant des mêmes noms de colonne que la table pêche
  tabVide <- tab[1,][-1,]
  numTirage <- as.numeric(rownames(tab))  # définition des numéros pour le tirage
  # Echantillon bootstrap issu d'un tirage aléatoire avec remise
  echant <- sample(numTirage, length(numTirage), replace = T)
  tabBoot <- rbind.data.frame(tabVide, tab[echant,])
  return(tabBoot)
}

# Méthode permettant de calculer la valeur des indicateurs individuels ainsi que 
# des indicateurs composites pour les échantillons bootstraps créés puis de 
# l'estimation des bornes de l'Intervalle de Confiance
# Génération de 500 échantillons bootstrap ce qui est suffisant pour l'estimation 
# des centiles de la distribution des indicateurs élémentaires et composites
IndicBoot.f  <-  function(tab){
  TabEchant <- as.data.frame(matrix(0,500,9))
  colnames(TabEchant)<-c("existenceAMP","suffiInfo","respectReg","adaptReg","effetEcosyst","effetEcono","effetAct","indicConnais","indicPerf")
    for(i in (1:500)){
      tabBoot  <-  AMPboot.f(tab)
      TabEchant[i, "existenceAMP"]  <-  IndicExistenceAMP.f(tab = tabBoot)
      TabEchant[i, "suffiInfo"]  <-  IndicSuffiInfo.f (tab = tabBoot)
      TabEchant[i, "respectReg"]  <-  IndicRespectReg.f(tab = tabBoot)
      TabEchant[i, "adaptReg"]  <-  IndicAdaptReg.f(tab = tabBoot)
      TabEchant[i, "effetEcosyst"]  <-  IndicEcosyst.f(tab = tabBoot)
      TabEchant[i, "effetEcono"]  <-  IndicEcono.f(tab = tabBoot)
      TabEchant[i, "effetAct"]  <-  IndicAct.f(tab = tabBoot)
      TabEchant[i, "indicConnais"]  <-  IndicConnaissance.f(tab = tabBoot, 
                                                            WeightConnais.f(tab = tabBoot))
      TabEchant[i, "indicPerf"]  <-  IndicPerformance.f(tab = tabBoot,
                                                        WeightPerfo.f(tab = tabBoot))
    }
    return(TabEchant)
}

#Méthode renvoyant la matrice contenant la valeur des indicateurs individuels et composite ainsi que l'écart tMatrice_Ipe entre les indicateurs individuels#
CalculIndicateurCompo.f  <-  function(tab){

  tkmessageBox(message="Cette fonction met un peu de temps à tourner!\n Soyez patients.",icon="warning",type="ok")

  tab <- Nettoyage.f(tab)
  TabEchantBoot <- IndicBoot.f(tab = tab)
  TabIndicateurs <- as.data.frame(matrix(0,9,2))
  rownames(TabIndicateurs) <- c("ExistenceAMP","SuffiInfo","RespectReg","AdaptReg","EffetEcosyst","EffetEcono","EffetAct","IndicConnaissance","IndicPerformance")
  colnames(TabIndicateurs) <- c("valeurs","IC")
  TabIndicateurs["ExistenceAMP","valeurs"]  <-  IndicExistenceAMP.f(tab = tab)
  TabIndicateurs["SuffiInfo","valeurs"]  <-  IndicSuffiInfo.f (tab = tab)
  TabIndicateurs["RespectReg","valeurs"]  <-  IndicRespectReg.f(tab = tab)
  TabIndicateurs["AdaptReg","valeurs"]  <-  IndicAdaptReg.f(tab = tab)
  TabIndicateurs["EffetEcosyst","valeurs"]  <-  IndicEcosyst.f(tab = tab)
  TabIndicateurs["EffetEcono","valeurs"]  <-  IndicEcono.f(tab = tab)
  TabIndicateurs["EffetAct","valeurs"]  <-  IndicAct.f(tab = tab)
  TabIndicateurs["IndicConnaissance","valeurs"]  <-  IndicConnaissance.f(tab = tab, WeightConnais.f(tab = tab))
  TabIndicateurs["IndicPerformance","valeurs"]  <-  IndicPerformance.f(tab = tab,WeightPerfo.f(tab = tab))  
  TabIndicateurs["ExistenceAMP","IC"]  <-  paste("[",round(quantile(TabEchantBoot$existenceAMP, probs = 0.025),digits = 2), ",",
                                                  round(quantile(TabEchantBoot$existenceAMP, probs = 0.975), digits = 2), "]")
  TabIndicateurs["SuffiInfo","IC"]  <-  paste("[",round(quantile(TabEchantBoot$suffiInfo, probs = 0.025), digits = 2), ",",
                                                round(quantile(TabEchantBoot$suffiInfo, probs = 0.975), digits = 2), "]")
  TabIndicateurs["RespectReg","IC"]  <-  paste("[",round(quantile(TabEchantBoot$respectReg, probs = 0.025), digits=2), ",",
                                                round(quantile(TabEchantBoot$respectReg, probs = 0.975), digits = 2), "]")
  TabIndicateurs["AdaptReg","IC"]  <-  paste("[",round(quantile(TabEchantBoot$adaptReg, probs = 0.025), digits = 2), ",",
                                              round(quantile(TabEchantBoot$adaptReg, probs = 0.975), digits = 2), "]")
  TabIndicateurs["EffetEcosyst","IC"]  <-  paste("[",round(quantile(TabEchantBoot$effetEcosyst, probs = 0.025), digits = 2), ",",
                                                  round(quantile(TabEchantBoot$effetEcosyst, probs = 0.975), digits = 2), "]")
  TabIndicateurs["EffetEcono","IC"]  <-  paste("[",round(quantile(TabEchantBoot$effetEcono, probs = 0.025), digits = 2), ",",
                                                 round(quantile(TabEchantBoot$effetEcono, probs = 0.975), digits = 2), "]")
  TabIndicateurs["EffetAct","IC"]  <-  paste("[",round(quantile(TabEchantBoot$effetAct, probs = 0.025), digits = 2),",",
                                               round(quantile(TabEchantBoot$effetAct, probs = 0.975), digits = 2), "]")
  TabIndicateurs["IndicConnaissance","IC"]  <-  paste("[",round(quantile(TabEchantBoot$indicConnais, probs = 0.025), digits = 2), ",",
                                                       round(quantile(TabEchantBoot$indicConnais, probs = 0.975), digits = 2), "]")
  TabIndicateurs["IndicPerformance","IC"]  <-  paste("[",round(quantile(TabEchantBoot$indicPerf, probs = 0.025), digits = 2), ",",
                                                      round(quantile(TabEchantBoot$indicPerf, probs = 0.975), digits = 2), "]")

  # Radar plot des indicateurs élémentaires de connaissance 
  x11(width=60,height=40,pointsize=10)
  radial.plot(TabIndicateurs[1:4, 1], 
              labels = c("Existence de l'AMP","Suffisance de l'information","Respect de la réglementation","Pertinence de la réglementation"), 
              rp.type="p", radial.lim=c(0,1), start = 0, clockwise = TRUE, line.col = "light blue",
              main = "Radar plot Indicateurs élémentaires de Connaissance de l'AMP et de sa réglementation", lwd=3, label.prop=1.1)
 savePlot(filename="C:/PAMPA/resultats_Usages/IndicateurComposite/Radar plot indicateurs élémentaires Connaissance", type =c("bmp"))

  # Radar plot des indicateurs élémentaires de performance 
  x11(width=60,height=40,pointsize=10)
  radial.plot(TabIndicateurs[5:7, 1], 
              labels = c("Effet de l'AMP sur l'écosystème","Effet de l'AMP sur l'économie locale","Effet de l'AMP sur leur activité"), 
              rp.type="p", radial.lim=c(0,1), start = 0, clockwise = TRUE, line.col = "light blue",
              main = "Radar plot Indicateurs élémentaires de Performance de l'AMP", lwd=3, label.prop=1.1)
 savePlot(filename="C:/PAMPA/resultats_Usages/IndicateurComposite/Radar plot indicateurs élémentaires performance", type =c("bmp"))

  # Tableau contenant les indicateurs élémentaires et ainsi que leurs intervalles de confiance respectifs
 write.table(TabIndicateurs,"C:/PAMPA/resultats_Usages/IndicateurComposite/Tableau indicateurs élémentaires Connaissance et Performance.csv",sep=";",quote=T)
 return(TabIndicateurs)
}
##### Appel de la fonction : Attention le ré_échantillonage bootstrap est un processus couteux en temps de calcul#######
# CalculIndicateurCompo.f  (tab = peche)



# Calcul de l'indicateur composite de performance via la méthode du système de notation : The OCDE way

# Méthode permettant le recodage des variable d'intérêt dans le cadre du calcul 
# de l'indicateur de performance, Ainsi que le calcul des indicateurs élémentaires par individu
note.f <- function(tab){
  tab1 <- subset(tab, tab$existenceAMP == "oui")
    VecteurElementaire <- as.data.frame(matrix(0,length(tab1[,1]),1))
  colnames(Vecteur_I_elementaire) <- c("C")
  
  # recodage des variables de performance, les modalité deviennent des notes -> Variables qualitatives ordonnées
  # Nsp = NA par commodité pour les calculs des indicateurs, 
  # nsp est tout de même pris en compte dans la pondération
  tab1$ecosystRecod <- NA
  tab1$ecosystRecod[which(tab1$effetEcosyst == "tres_positif")] <- 1 
  tab1$ecosystRecod[which(tab1$effetEcosyst == "plutot_positif")] <- 0.75 
  tab1$ecosystRecod[which(tab1$effetEcosyst == "neutre")] <- 0.5 
  tab1$ecosystRecod[which(tab1$effetEcosyst == "plutot_negatif")] <- 0.25 
  tab1$ecosystRecod[which(tab1$effetEcosyst == "tres_negatif")] <- 0
  
  tab1$econoRecod <- NA
  tab1$econoRecod[which(tab1$effetEcono == "tres_positif")] <- 1 
  tab1$econoRecod[which(tab1$effetEcono == "plutot_positif")] <- 0.75 
  tab1$econoRecod[which(tab1$effetEcono == "neutre")] <- 0.5 
  tab1$econoRecod[which(tab1$effetEcono == "plutot_negatif")] <- 0.25 
  tab1$econoRecod[which(tab1$effetEcono == "tres_negatif")] <- 0
  
  tab1$actRecod <- NA
  tab1$actRecod[which(tab1$effetAct == "tres_positif")] <- 1 
  tab1$actRecod[which(tab1$effetAct == "plutot_positif")] <- 0.75 
  tab1$actRecod[which(tab1$effetAct == "neutre")] <- 0.5 
  tab1$actRecod[which(tab1$effetAct == "plutot_negatif")] <- 0.25 
  tab1$actRecod[which(tab1$effetAct == "tres_negatif")] <- 0
  
  for (i in (1:nrow(tab1))){
    vect <- c(tab1$ecosystRecod[i],tab1$econoRecod[i],tab1$actRecod[i])
    # Calcul de l'indicateur élémenataire par individu
    VecteurElementaire$C[i] <- round(mean(vect, na.rm=T), digits=2)
  }
  MatriceElementaire <- cbind(tab1, VecteurElementaire)
  return(MatriceElementaire)
}
       
# Méthode renvoyant l'intervalle de confiance pour l'indicateur de 
# perception/Performance en testant la moyenne des indicateurs élémentaires
IC_Performance_note.f <- function(tab){
  Matrice_Indicateur_note <- note.f(tab = tab)
  IC <- t.test(Matrice_Indicateur_note$C,conf.level=0.95)$conf.int
  return(IC)
}

#Méthode renvoyant l'intervalle de confiance pour l'indicateur de Perception/Performance en testant la moyenne des indicateurs élémentaires#
IC_Ecosyst_note.f <- function(tab){
Matrice_Indicateur_note <- note.f(tab = tab)
IC <- t.test(Matrice_Indicateur_note$ecosystRecod,conf.level=0.95)$conf.int
return(IC)
}
#Méthode renvoyant l'intervalle de confiance pour l'indicateur de Perception/Performance en testant la moyenne des indicateurs élémentaires#
IC_Econo_note.f <- function(tab){
Matrice_Indicateur_note <- note.f(tab = tab)
IC <- t.test(Matrice_Indicateur_note$econoRecod,conf.level=0.95)$conf.int
return(IC)
}
#Méthode renvoyant l'intervalle de confiance pour l'indicateur de Perception/Performance en testant la moyenne des indicateurs élémentaires#
IC_Peche_note.f <- function(tab){
Matrice_Indicateur_note <- note.f(tab = tab)
IC <- t.test(Matrice_Indicateur_note$actRecod,conf.level=0.95)$conf.int
return(IC)
}

#Méthode permettant le calcul des indicateurs élémentaires par variables, de l'indicateur composite ainsi que leur IC respectifs, 
#graphiques pour les indicateurs élémetaires (Histogramme + boxplot), radar pour les indicateurs élémentaires par variables#
 
Matrice_Indicateur_note.f  <-  function(tab){
Matrice_I <- as.data.frame(matrix(0,4,2))
rownames(Matrice_I) <- c("EffetEcosyst","EffetEcono","EffetAct","IndicateurPerformance")
colnames(Matrice_I) <- c("valeurs","IC")
Matrice_Indicateur_note <- note.f(tab = tab)

# Histogramme et boxplot pour les indicateurs élémentaires par individu
x11(width=60,height=40,pointsize=10)
par(mfrow= c(1,2))
hist(Matrice_Indicateur_note$C, main="Distribution des indicateurs élémentaires par individu",
                                xlab="Indicateur élémentaire",col="light blue")
boxplot(Matrice_Indicateur_note$C, data=Matrice_Indicateur_note, ylab="Indicateur élémentaire",
                                   main="Boxplot des indicateurs élémentaires par individu", col="light blue")
savePlot(filename="C:/PAMPA/resultats_USages/IndicateurComposite/Distribution indicateurs élémentaires par individus", type =c("bmp"))

#Calcul des indicateurs élémentaires et de l'indicateur composite ainsi que de leurs intervalles de confiance
  Matrice_I["EffetEcosyst", "valeurs"]  <-  round(mean(Matrice_Indicateur_note$ecosystRecod, na.rm=T), digits=2)
  Matrice_I["EffetEcono", "valeurs"]  <-  round(mean(Matrice_Indicateur_note$econoRecod, na.rm=T), digits=2)
  Matrice_I["EffetAct", "valeurs"]  <-  round(mean(Matrice_Indicateur_note$actRecod, na.rm=T), digits=2)
  Matrice_I["IndicateurPerformance", "valeurs"]  <-  round(mean(Matrice_Indicateur_note$C, na.rm=T), digits=2)
  Matrice_I["EffetEcosyst", "IC"]  <-  paste("[",round(IC_Ecosyst_note.f(tab = tab1), digits = 2)[1], ",",
                                                round(IC_Ecosyst_note.f(tab = tab1), digits = 2)[2], "]")
  Matrice_I["EffetEcono", "IC"]  <-  paste("[",round(IC_Econo_note.f(tab = tab1), digits = 2)[1], ",",
                                              round(IC_Econo_note.f(tab = tab1), digits = 2)[2], "]")
  Matrice_I["EffetAct", "IC"]  <-  paste("[",round(IC_Peche_note.f(tab = tab1), digits = 2)[1], ",",
                                            round(IC_Peche_note.f(tab = tab1), digits = 2)[2], "]")
  Matrice_I["IndicateurPerformance", "IC"]  <-  paste("[",round(IC_Performance_note.f(tab = tab1), digits = 2)[1], ",",
                                                         round(IC_Performance_note.f(tab = tab1), digits = 2)[2], "]")

# Diagramme en radar des indicateurs élémentaires par variable
  x11(width=60,height=40,pointsize=10) 
  radial.plot(Matrice_I[1:3, 1], labels = c("Effet sur l'écosystème","Effet sur l'économie locale","Effet sur la pêche"), 
              rp.type="p", radial.lim=c(0,1), start = 0, clockwise = TRUE, line.col = "light blue", 
              main = "Radar plot Indicateurs élémentaires (notes)",lwd=3,label.prop=1.21)
 savePlot(filename="C:/PAMPA/resultats_Usages/IndicateurComposite/Radar plot indicateurs élémentaires notes", type =c("bmp"))
 # Tableau contenant les indicateurs élémentaires et ainsi que leur intervalle de confiance respectifs
 write.table(Matrice_I,"C:/PAMPA/resultats_USages/IndicateurComposite/Tableau indicateurs élémentaires Performance Note.csv",sep=";",quote=T)
  
  return(Matrice_I)
}

######Appel de la fonction######
# Matrice_Indicateur_note.f(tab=peche)


#####Appel des fonctions avec une distinction réalisée entre les pêcheurs, les plongeurs et les plaisanciers#######

#Calcul des indicateurs de connaissance de l'AMP et de sa règlementation#
Indicateur_Connaissance_TypeActivite.f  <-  function(tableau,acti){
tableau_TypeActivite <- subset(tableau,tableau$activite == "acti")
Matrice_Indicateur <- Matrice_Indicateur2.f(tableau_TypeActivite)
return( Matrice_Indicateur)
}

#Calcul des indicateurs de performance#
Indicateur_Performance_TypeActivite.f  <-  function(tableau,acti){
tableau_TypeActivite <- subset(tableau,tableau$activite == "acti")
Matrice_Indicateur <- Matrice_Indicateur_note.f(tableau_TypeActivite)
return(Matrice_Indicateur)
}

#####Appel des fonctions avec une distinction réalisée entre les résidents et les non résidents#######
#Pour les résidents#
#Calcul des indicateurs de connaissance de l'AMP et de sa règlementation#
Indicateur_Connaissance_Resident.f  <-  function(tableau,acti){
tableau_Resident <- subset(tableau,tableau$resident == "resident")
Matrice_Indicateur <- Matrice_Indicateur2.f(tableau_Resident)
return( Matrice_Indicateur)
}

#Calcul des indicateurs de performance#
Indicateur_Performance_Resident  <-  function(tableau,acti){
tableau_Resident <- subset(tableau,tableau$resident == "resident")
Matrice_Indicateur <- Matrice_Indicateur_note.f(tableau_Resident)
return(Matrice_Indicateur)
}

#Pour les non résidents#
#Calcul des indicateurs de connaissance de l'AMP et de sa règlementation#
Indicateur_Connaissance_nonresident.f  <-  function(tableau,acti){
tableau_nonresident <- subset(tableau,tableau$resident == "non-resident")
Matrice_Indicateur <- Matrice_Indicateur2.f(tableau_nonresident)
return( Matrice_Indicateur)
}

#Calcul des indicateurs de performance#
Indicateur_Performance_nonresident  <-  function(tableau,acti){
tableau_nonresident <- subset(tableau,tableau$resident == "non-resident")
Matrice_Indicateur <- Matrice_Indicateur_note.f(tableau_nonresident)
return(Matrice_Indicateur)
}
