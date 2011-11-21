################################################################################
# Nom               : Fonctions et calculs obligatoires
# Type              : Programme
# Objet             : Ce programme contient Fonctions génériques nécessaires pour le calcul des métriques.
#                     Il réalise également les calculs génériques nécessaires pour le calcul des métriques.
# Input             : TXT
# Output            : Data
# Auteur            : Elodie GAMP
# R version         : 2.8.1
# Date de création  : Avril 2011
# Sources
################################################################################

stepInnerProgressBar.f(n=1, msg="Chargement des fonctions obligatoires")  # progression du chargement

#### fonctions nécessaires pour le script  

########################################################################################################################
dropLevels.f <- function(df, which=NULL)
{
    ## Purpose: Supprimer les 'levels' non utilisés des facteurs d'une
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

########################################################################################################################

# fonction pour remplacer les valeurs par des données binaires
lengthnb.f = function(x){x=1}

# fonction pour compter les lignes sans NA
lengthnna.f = function(x){length(x[!is.na(x)])}

# fonction pr réaliser le tableau recap (min, max, moy, etc) pour les sorties
tabSRecap.f = function(x, nbSortie, liste=NA){
  if(is.null(dim(x))==TRUE) {    # si le tableau envoyé ne contient qu'une colonne (n'est plus considéré comme un dataframe donc calcul différent)
    tableau<-matrix(NA, 9, 1)
    colnames(tableau)<-liste
    freqTransfo<-matrix(NA, max(nbSortie, na.rm=T), 1)
    colnames(freqTransfo)<-colnames(x)
    manquant<-nbSortie[match(liste, names(nbSortie))]-lengthnna.f(x)         # lorsque des activités sont sélectionnées, vérifie que les données comportent toutes les sorties
      if (manquant<0){
        tab<- sort(x)[abs(manquant):length(x)]
        freqTransfo[1:nbSortie] <- sort(x)[abs(manquant):length(x)]
      } else {
        tab<-c(x, rep(0, manquant))
        freqTransfo[1:nbSortie] <- c(as.numeric(x[which(is.na(x)==FALSE)]), rep(0, manquant))
      }
      tableau[1]<-min(tab, na.rm=T)
      tableau[2]<-max(tab, na.rm=T)
      tableau[3]<-sum(tab, na.rm=T)/nbSortie[match(liste, names(nbSortie))]
      tableau[4]<-median(tab, na.rm=T)
      tableau[5]<-sd(tab, na.rm=T)
      tableau[6]<-nbSortie[match(liste, names(nbSortie))]
      tableau[7] <-tableau[3]*tableau[6]
      rownames(tableau)<-c("minimum", "maximum", "moyenne", "mediane", "ecart-type", "nb sortie", "nb total recensé", "IC-", "IC+")
    ## calcul des intervalles de confiance
      moy <- sum(tab, na.rm=T)/nbSortie[match(liste, names(nbSortie))]
      variance <- var(tab, na.rm=T)
      nbTot <- nbSortie[match(liste, names(nbSortie))]
      tableau[8] <- moy + qt(0.025, (nbTot-1)) * sqrt(variance/(nbTot-1))
      tableau[9] <- moy + qt(0.975, (nbTot-1)) * sqrt(variance/(nbTot-1))
  } else {
    nbSortie<-nbSortie[names(nbSortie)%in%colnames(x)]   # le calcul du nb de sorties/zone considère ttes les act, la fréquentation seulement cnes act (zones =/=)
    tableau<-matrix(NA, 9, ncol(x))
    colnames(tableau)<-colnames(x)
    freqTransfo<-matrix(NA, max(nbSortie, na.rm=T), ncol(x))
    colnames(freqTransfo)<-colnames(x)
    for (i in 1 : ncol(x)){
      manquant<-as.numeric(nbSortie[match(colnames(x)[i], names(nbSortie))])-lengthnna.f(x[, i])
      if (manquant<0){
#        print(paste("pbA", i))
        freqTransfo[1:nbSortie[match(colnames(x)[i], names(nbSortie))], i] <- sort(x[, i])[(abs(manquant)+1):length(x[, i])]
        tab <- sort(x[, i])[abs(manquant):length(x[, i])]
      }
      if (manquant >0) {
#        print(paste("pbB", i))
        freqTransfo[1:nbSortie[match(colnames(x)[i], names(nbSortie))], i] <-c(as.numeric(x[which(is.na(x[, i])==FALSE), i]), rep(0, manquant))
        tab<-c(x[, i], rep(0, manquant))
      }
      if (manquant == 0) {
#        print(paste("pbC", i))
        freqTransfo[1:nbSortie[match(colnames(x)[i], names(nbSortie))], i]<-sort(x[, i])
        tab<-x[, i]
      }
      tableau[1, i]<-min(tab, na.rm=T)
      tableau[2, i]<-max(tab, na.rm=T)
      tableau[3, i]<-sum(tab, na.rm=T)/nbSortie[match(colnames(x)[i], names(nbSortie))]
      tableau[4, i]<-median(tab, na.rm=T)
      tableau[5, i]<-sd(tab, na.rm=T)
      tableau[6, i]<-nbSortie[match(colnames(x)[i], names(nbSortie))]
      tableau[7, i]<-tableau[3, i]*tableau[6, i]
      rownames(tableau)<-c("minimum", "maximum", "moyenne", "mediane", "ecart-type", "nb sortie", "nb total recensé", "IC-", "IC+")
          ## calcul des intervalles de confiance
      moy <- sum(tab, na.rm=T)/nbSortie[match(colnames(x)[i], names(nbSortie))]
      variance <- var(tab, na.rm=T)
      nbTot <- nbSortie[match(colnames(x)[i], names(nbSortie))]
      tableau[8, i] <- moy + qt(0.025, (nbTot-1)) * sqrt(variance/(nbTot-1))
      tableau[8, i][which(tableau[8, i] < 0)] <- 0                           # évite les IC négatif
      tableau[9, i] <- moy + qt(0.975, (nbTot-1)) * sqrt(variance/(nbTot-1))
    }
  }
  assign("freqTransfo", freqTransfo, envir=.GlobalEnv)
  print(tableau)
}

### calcul du nombre de sorties supérieur au seuil
FreqSupSeuil.f = function(tableau, liste, seuil=SeuilFreq){
  if(is.null(dim(tableau))==TRUE) {    # si le tableau envoyé ne contient qu'une colonne (n'est plus considéré comme un dataframe donc calcul différent)
    supSeuil<-matrix(NA, 5, 1)
    colnames(supSeuil)<-liste
    rownames(supSeuil)<-c("", "seuil", "maximum", "valeur seuil", "nb sorties sup au seuil")
    supSeuil[1]<-rep("", 1)
    supSeuil[2]<-rep(seuil, 1)
    supSeuil[3]<-max(tableau, na.rm=T)
    effectif<-length(tableau)                     # calcul de l'effectif
    tabOrd<-sort(tableau)                   # mise en ordre des données dans un ordre croissant
    rang<-round(effectif*seuil, digits=0)          #calcul du rang correspondant au percentile choisi par l'utilisateur (ex : à 80% de l'effectif(N/5*4) )
    supSeuil[4]<-tabOrd[rang]                 # valeur seuil correspondante
    supSeuil[5]<-length(which(tabOrd>tabOrd[rang]))  # nombre de sorties supérieures à 80%
  } else {
    supSeuil<-matrix(NA, 5, ncol(tableau))
    colnames(supSeuil)<-colnames(tableau)
    rownames(supSeuil)<-c("", "seuil", "maximum", "valeur seuil", "nb sorties sup au seuil")
    supSeuil[1, ]<-rep("", ncol(tableau))
    supSeuil[2, ]<-rep(seuil, ncol(tableau))
    for (i in 1 : ncol(tableau)){
      supSeuil[3, i]<-max(tableau[, i], na.rm=T)
      effectif<-lengthnna.f(tableau[, i])                     # calcul de l'effectif
      tabOrd<-sort(tableau[, i])                   # mise en ordre des données dans un ordre croissant
      rang<-round(effectif*seuil, digits=0)          #calcul du rang correspondant au percentile choisi par l'utilisateur (ex : à 80% de l'effectif(N/5*4) )
      supSeuil[4, i]<-tabOrd[rang]                 # valeur seuil correspondante
      supSeuil[5, i]<-length(which(tabOrd>tabOrd[rang]))  # nombre de sorties supérieures à 80%
    }
  }
  print(supSeuil)
}

# fonction pour réaliser le tableau recap pour les captures
tabRecapCaptures.f = function (capture) {
  moy<-apply(capture, 2, mean, na.rm=T)
  mini<-apply(capture, 2, min, na.rm=T)
  maxi<-apply(capture, 2, max, na.rm=T)
  mediane<-apply(capture, 2, median, na.rm=T)
  sde<-apply(capture, 2, sd, na.rm=T)
  sde[is.na(sde)]<-0
  tauxRep<-lengthnna.f(captures[, "nb"])*100/nrow(captures)
  tabAnalyseCapt<-rbind(mini, maxi, moy, mediane, sde, tauxRep)
  rownames(tabAnalyseCapt)<-c("minimum", "maximum", "moyenne", "médiane", "ecart-type", "taux de réponse")
  print(tabAnalyseCapt)
  }

tabRecapCapturesComplet.f = function(x, nbEnqZone, liste=NA){
  if(is.null(dim(x))==TRUE) {    # si le tableau envoyé ne contient qu'une colonne (n'est plus considéré comme un dataframe donc calcul différent)
    tableau<-matrix(NA, 8, 1)
    colnames(tableau)<-liste
    capturesTransfo<-matrix(NA, max(nbEnqZone, na.rm=T), 1)
    colnames(capturesTransfo)<-colnames(x)
    manquant<-nbEnqZone[match(liste, names(nbEnqZone))]-lengthnna.f(x)       # lorsqu'il y a sélection de cns taxons, vérifie que toutes les enquêtes sont présentes
      if (manquant<0){
        tab<- sort(x)[abs(manquant):length(x)]
        capturesTransfo[1:nbEnqZone] <- sort(x)[abs(manquant):length(x)]
      } else {
        tab<-c(x, rep(0, manquant))
        capturesTransfo[1:nbEnqZone] <-c(as.numeric(x[which(is.na(x)==FALSE)]), rep(0, manquant))
      }
      tableau[1]<-min(tab, na.rm=T)
      tableau[2]<-max(tab, na.rm=T)
      tableau[3]<-sum(tab, na.rm=T)/nbEnqZone[match(liste, names(nbEnqZone))]
      tableau[4]<-median(tab, na.rm=T)
      tableau[5]<-sd(tab, na.rm=T)
      tableau[6]<-nbEnqZone[match(liste, names(nbEnqZone))]
      rownames(tableau)<-c("minimum", "maximum", "moyenne", "mediane", "ecart-type", "nb enquêtes", "IC-", "IC+")
    ## calcul des intervalles de confiance
      moy <- sum(tab, na.rm=T)/nbEnqZone[match(liste, names(nbEnqZone))]
      variance <- var(tab, na.rm=T)
      nbTot <- nbEnqZone[match(liste, names(nbEnqZone))]
      tableau[7] <- moy + qt(0.025, (nbTot-1)) * sqrt(variance/(nbTot-1))
      tableau[7][which(tableau[7] < 0)] <- 0
      tableau[8] <- moy + qt(0.975, (nbTot-1)) * sqrt(variance/(nbTot-1))
     # tableau[7] <- paste("[", borneInf, "-", borneSup, "]")
  } else {
    nbEnqZone<-nbEnqZone[names(nbEnqZone)%in%colnames(x)]   # le calcul du nb de sorties/zone considère ttes les act, la fréquentation seulement cnes act (zones =/=)
    tableau<-matrix(NA, 8, ncol(x))
    colnames(tableau)<-colnames(x)
    capturesTransfo<-matrix(NA, max(nbEnqZone, na.rm=T), ncol(x))
    colnames(capturesTransfo)<-colnames(x)
    for (i in 1 : ncol(x)){
      manquant<-as.numeric(nbEnqZone[match(colnames(x)[i], names(nbEnqZone))])-lengthnna.f(x[, i])
      if (manquant<0){
        capturesTransfo[1:nbEnqZone[match(colnames(x)[i], names(nbEnqZone))], i] <- sort(x[, i])[(abs(manquant)+1):length(x[, i])]
        tab <- sort(x[, i])[abs(manquant):length(x[, i])]
      }
      if (manquant >0) {
        capturesTransfo[1:nbEnqZone[match(colnames(x)[i], names(nbEnqZone))], i] <-c(as.numeric(x[which(is.na(x[, i])==FALSE), i]), rep(0, manquant))
        tab<-c(x[, i], rep(0, manquant))
      }
      if (manquant == 0) {
        capturesTransfo[1:nbEnqZone[match(colnames(x)[i], names(nbEnqZone))], i]<-sort(x[, i])
        tab<-x[, i]
      }
      tableau[1, i]<-min(tab, na.rm=T)
      tableau[2, i]<-max(tab, na.rm=T)
      tableau[3, i]<-sum(tab, na.rm=T)/nbEnqZone[match(colnames(x)[i], names(nbEnqZone))]
      tableau[4, i]<-median(tab, na.rm=T)
      tableau[5, i]<-sd(tab, na.rm=T)
      tableau[6, i]<-nbEnqZone[match(colnames(x)[i], names(nbEnqZone))]
#      tableau<-rbind(minimum, maximum, moyenne, mediane, ecartType, nbSortie[match(colnames(x), names(nbSortie))])
      rownames(tableau)<-c("minimum", "maximum", "moyenne", "mediane", "ecart-type", "nb enquêtes", "IC-", "IC+")
    ## calcul des intervalles de confiance
      moy <- sum(tab, na.rm=T)/nbEnqZone[match(colnames(x)[i], names(nbEnqZone))]
      variance <- var(tab, na.rm=T)
      nbTot <- nbEnqZone[match(colnames(x)[i], names(nbEnqZone))]
      tableau[7, i] <- moy + qt(0.025, (nbTot-1)) * sqrt(variance/(nbTot-1))
      tableau[7, i][which(tableau[7, i] < 0)] <- 0
      tableau[8, i] <- moy + qt(0.975, (nbTot-1)) * sqrt(variance/(nbTot-1))
    }
  }
  assign("capturesTransfo", capturesTransfo, envir=.GlobalEnv)
  print(tableau)
}


# fonction pour rassembler les réponses dans un seul tableau pour 2 activités ou réponses sur deux champs
tableau2act.f = function(listeChoix, act, donneesAct1, donneesAct2) {
  bid=matrix(0, length(listeChoix), 2) #as.data.frame(matrix(0, length(listeChoix), 2))
  colnames(bid)=act
  rownames(bid)<-listeChoix
  bid[, 1]=(donneesAct1[match(dimnames(bid)[[1]], names(donneesAct1))])
  bid[, 2]=(donneesAct2[match(dimnames(bid)[[1]], names(donneesAct2))])
  bid[is.na(bid)]<-0
  print(bid)
}

# fonction pour rassembler les réponses dans un seul tableau pour 3 activités ou réponses sur 3champs
tableau3act.f = function(listeChoix, donneesAct1, donneesAct2, donneesAct3) {
  bid3=matrix(0, length(listeChoix), 3)  #as.data.frame(matrix(0, length(listeChoix), 3))
  colnames(bid3)=c("facChoix1", "facChoix2", "facChoix3")
  rownames(bid3)<-listeChoix
  bid3[, 1]=(donneesAct1[match(dimnames(bid3)[[1]], names(donneesAct1))])
  bid3[, 2]=(donneesAct2[match(dimnames(bid3)[[1]], names(donneesAct2))])
  bid3[, 3]=(donneesAct3[match(dimnames(bid3)[[1]], names(donneesAct3))])
  bid3[is.na(bid3)]<-0
  print(bid3)
}


# fonction pour ajouter les compléments d'information aux boxplot
infoBoxplot.f=function(tableau, x, nbSortie)  {
    text(1.2:(ncol(tableau)+0.2), tableau["moyenne", ], labels=round(tableau["moyenne", ], digits=1), col="red", cex=1.2)
    text(1.2:(ncol(tableau)+0.2), tableau["maximum", ], labels=round(tableau["maximum", ], digits=1), col="blue", cex=1.2)
    axis(1, at=1:length(tableau["maximum", ]), labels=nbSortie[match(colnames(x), names(nbSortie))], pos=(-7*max(x, na.rm=T)/100), cex.axis=0.7, lwd=0)
}


# choix d'extraire les valeurs extrêmes des graph
extremes.f = function () {
  choixExtremes <- tktoplevel()
  tkwm.title(choixExtremes, "Extrêmes")
  cb <- tkcheckbutton(choixExtremes)
  cbValue <- tclVar("0")
  tkconfigure(cb, variable=cbValue)
  tkgrid(tklabel(choixExtremes, text="Extraire les valeurs extrêmes de la représentation graphiques"), cb)
  OnOK <- function()
    {
    cbVal <- as.character(tclvalue(cbValue))
    tkdestroy(choixExtremes)
    if (cbVal=="1") {
      choix<-1
     	tkmessageBox(message="Les valeurs extrêmes sont retirées de la représentation graphique", icon="info")
      }
    if (cbVal=="0") {
      choix<-0
     	tkmessageBox(message="Les valeurs extrêmes sont conservées dans la représentation graphique", icon="info")
      }
    assign("choix", choix, envir=.GlobalEnv)
    }
  OK.but <- tkbutton(choixExtremes, text="OK", command=OnOK)
  tkgrid(OK.but)
  tkfocus(choixExtremes)
  tkwait.window(choixExtremes)
}


### suppression  ou non des valeurs extrêmes
ApplicationExtremes.f = function (tabExtremes) {
  if (choix == 1) {                # choisi dans la fonction (extremes.f)
    print(max(tabExtremes, na.rm=T))
    if (mode(tabExtremes) =="list") {
      tabExtremes[tabExtremes>0.95*max(tabExtremes, na.rm=T)]<-NA
    } else {
      tabExtremes[which(tabExtremes>0.95*max(tabExtremes, na.rm=T))]<-NA
    }
  } else {
      tabExtremes[which(tabExtremes>max(tabExtremes, na.rm=T))]<-NA
 	}
 	    assign("SansExtremes", tabExtremes, envir=.GlobalEnv)
}


# fonction pour le calcul des réponses qualitatives (questionnaires)
QuQualitatives.f=function(question, tableauTot) {
  repo<-table(question)*100/lengthnna.f(question)
  repAct<-apply(table(question, tableauTot$activite), 2, sum, na.rm=T)
  actConcernees<-names(repAct)[which(repAct!=0)]
  nbQuest<-sum(table(tableauTot$activite)[actConcernees], na.rm=T)     ## sous tousQuest, les questionnaires n'ont pas tous les mêmes questions, j'enlève donc dans le taux de réponse, les lignes correspondantes
  tauxRep<-lengthnna.f(question)/nbQuest*100

    modalitePos <- c("assez", "oui", "decisif", "bien", "tout_a_fait", "plutot_dac", "tres_positif", "plutot_positif", "positif", "bonnes", "TJRS", "SVT")
    freqOui <- length(question[which(is.element(question, modalitePos))])/lengthnna.f(question)
#  }
  total <- lengthnna.f(question)
  borneInf <- (freqOui + qnorm(0.025)*sqrt(freqOui*(1-freqOui)/total))*100
  borneInf[which(borneInf < 0)] <- 0
  borneSup <- (freqOui + qnorm(0.975)*sqrt(freqOui*(1-freqOui)/total))*100
  borneSup[which(borneSup > 100)] <- 100
  reponse <- c(repo, tauxRep, borneInf, borneSup)
  names(reponse)[(length(reponse)-2):length(reponse)]<-c("taux de réponse", "IC-", "IC+")
  print(reponse)
}
# fonction pour le calcul des réponses qualitatives (questionnaires) pour un seul questionnaire (pêche)
QuQualitativesUnique.f=function(question) {
  repo<-table(question)*100/lengthnna.f(question)
  tauxRep<-lengthnna.f(question)/length(question)*100
  reponse<-c(repo, tauxRep)
  names(reponse)[length(reponse)]<-"taux de réponse"
  ## IC des réponses positives
  modalitePos <- c("oui", "decisif", "bien", "tout_a_fait", "plutot_dac", "tres_positif", "plutot_positif", "positif", "bonnes", "TJRS", "SVT")
  freqOui <- length(question[which(is.element(question, modalitePos))])/lengthnna.f(question)
  total <- lengthnna.f(question)
  borneInf <- (freqOui + qnorm(0.025)*sqrt(freqOui*(1-freqOui)/total))*100
  borneInf[which(borneInf < 0)] <- 0
  borneSup <- (freqOui + qnorm(0.975)*sqrt(freqOui*(1-freqOui)/total))*100
  borneSup[which(borneSup > 100)] <- 100
  reponse <- c(repo, tauxRep, borneInf, borneSup)
  names(reponse)[(length(reponse)-2):length(reponse)]<-c("taux de réponse", "IC-", "IC+")
  print(reponse)
}


# fonction pr réaliser le tableau recap (min, max, moy, etc) pour les questionnaires
tabQRecap.f=function(question){
  moyenne<-mean(question, na.rm=T)
  minimum<-min(question, na.rm=T)
  maximum<-max(question, na.rm=T)
  mediane<-median(question, na.rm=T)
  ecartType<-sd(question, na.rm=T)
  tauxRep<-lengthnna.f(question)*100/length(question)
  tableau<-rbind(minimum, maximum, moyenne, mediane, ecartType, tauxRep)
  names(tableau)<-c("minimum", "maximum", "moyenne", "mediane", "ecart-type", "taux de réponse")
  ## calcul des intervalles de confiance
  variance <- var(question, na.rm=T)
  nbTot <- lengthnna.f(question)
  tableau[7] <- moyenne + qt(0.025, (nbTot-1)) * sqrt(variance/(nbTot-1))
  tableau[8] <- moyenne + qt(0.975, (nbTot-1)) * sqrt(variance/(nbTot-1))
  print(tableau)
}


# fonction pour faire le barplot par année (1graph par année) et par usagers
BarplotQuQualitatives.f=function(question, activite, titre, periode, limite) {
  repo<-table(question, activite)
  repQuest<-apply(repo, 2, sum, na.rm=T)
  reponse<-repo*100/(rep(repQuest, each=nrow(repo)))
  tauxRep<-repQuest/table(activite)*100
 # assign("repo", repo, envir=.GlobalEnv)
  if (sum(repo, na.rm=T)==0) {
    print("rien")
  } else {
    if (dim(repo)[2]==1) {                  # une seule activité disponible : adaptation pour éviter que le barplot se fasse en beside = T
      reponse<-cbind(reponse, rep(0, nrow(reponse)))
      ordre<-order(rownames(reponse), decreasing=T)
      colorGraph<-couleur.f(reponse, ordre)
      graph<-barplot(reponse[ordre, ], col=colorGraph, main=paste(titre, periode), width=0.7, xlim=limite, ylim=c(0, 120), legend=T)
#      axis(1, at=graph, labels=c(paste(round(tauxRep, digits=2), "%"), ""), pos=115, lwd=0)
    } else {
      ordre<-order(rownames(reponse), decreasing=T)
      colorGraph<-couleur.f(reponse, ordre)
      graph<-barplot(reponse[ordre, ], col=colorGraph, main=paste(titre, periode), width=0.7, xlim=limite, ylim=c(0, 120), legend=T)
      axis(1, at=graph, labels=paste(round(tauxRep, digits=2), "%"), pos=115, lwd=0)
    }
  }
}


### fonction pour barplot par année toutes activités ensemble
BarplotAnnee.f=function(tableauTot, tableau, titre, limite){
  if (length(colnames(tableau))==1) {
    tableaub<-cbind(tableau, rep(0, nrow(tableau)))
    tableau2<-tableaub[-c(nrow(tableaub)-2, nrow(tableaub)-1, nrow(tableaub)), ]   # le tableau contient les IC, à ne pas prendre dans la représentation graphique
    ordre<-order(rownames(tableaub)[1:nrow(tableau)-1], decreasing=T)
    if (length(rownames(tableau))==2) {
      colorGraph<-"antiqueWhite1"                 # adaptation si une seule activité et une seule réponse
      graph1<-barplot(as.matrix(tableaub[1]), col=colorGraph, main=titre, ylim=c(0, 120), legend=T)
      (print(graph1))
#      axis(1, at=graph1, labels=c(paste(round(tableaub[2], digits=2), "%"), ""), pos=115, lwd=0)
    } else {                                   # adaptation si une seule activité et pls réponses
      colorGraph<-couleur.f(tableau2, ordre)
      graph1<-barplot(as.matrix(tableau2[ordre, ]), col=colorGraph, main=titre, ylim=c(0, 120), legend=T)
      (print(graph1))
      axis(1, at=graph1, labels=c(paste(round(tableaub[nrow(tableaub)-2, 1], digits=2), "%"), ""), pos=115, lwd=0)
    }
  } else {
      if ((length(rownames(tableau))-2)==2) {         # -2 pour enlever les deux lignes de IC
        colorGraph<-"antiqueWhite1"
        graph1<-barplot(tableau[1, ], col=colorGraph, main=titre, ylim=c(0, 120), legend=T)
        (print(graph1))
        axis(1, at=graph1, labels=paste(round(tableau[nrow(tableau)-2, ], digits=2), "%"), pos=115, lwd=0)
      } else {
        tableau2<-tableau[-c(nrow(tableau)-2, nrow(tableau)-1, nrow(tableau)), ]
        ordre<-order(rownames(tableau2), decreasing=T)
        colorGraph<-couleur.f(tableau2, ordre)
        graph1<-barplot(tableau2[ordre, ], col=colorGraph, main=titre, ylim=c(0, 120), legend=T, width=0.7, xlim=limite)
        axis(1, at=graph1, labels=paste(round(tableau[nrow(tableau)-2, ], digits=2), "%"), pos=115, lwd=0)
    }
  }
}


# fonction pour faire le pie par année (1graph par année) et par usagers
PieQuQualitatives.f=function(question, activite, titre, periode, titre2) {
  repo<-table(question, activite)
  repQuest<-apply(repo, 2, sum, na.rm=T)
  reponse<-repo*100/(rep(repQuest, each=nrow(repo)))
  tauxRep<-repQuest/table(activite)*100
  if (sum(repo, na.rm=T)==0) {
    print("rien")
  } else {
    ordre<-order(rownames(reponse), decreasing=T)
    colorGraph<-couleur.f(reponse, ordre)
    for (i in 1:ncol(reponse)) {
      if (repQuest[i]!=0) {
        pie(reponse[ordre, i], cex=1.6,   #labels=paste(rownames(reponse)[ordre], round(reponse[ordre, i], digits=2), "%"),
          col=colorGraph, main=paste(titre, periode, titre2, colnames(reponse)[i], "\n (taux de réponse :", round(tauxRep[i], digits=2), "%)"))
      }
    }
  }
}
# fonction pour faire le pie grand nombre par année (1graph par année) et par usagers
GdPieQuQualitatives.f=function(question, activite, titre, periode, titre2="") {       # modalités trop diverses pour avoir des couleurs définines associées
  repo<-table(question, activite)
  repQuest<-apply(repo, 2, sum, na.rm=T)
  reponse<-repo*100/(rep(repQuest, each=nrow(repo)))
  tauxRep<-repQuest/table(activite)*100
  if (dim(repo)[1]==0) {
    print("rien")
  } else {
    for (i in 1:ncol(repo)) {
      if (sum(repo[, i], na.rm=T)!=0) {
        pie(reponse[, i], labels=paste(rownames(reponse), round(reponse[, i], digits=2), "%"), main=paste(titre, periode, titre2, colnames(reponse)[i], "\n (taux de réponse :", round(tauxRep[i], digits=2), "%)"))
      }
    }
  }
}
# fonction pour faire le pie par année (1graph par année)
PieQualitatives.f=function(question, titre) {
  if (sum(question["taux de réponse", ], na.rm=T)==0) {
    print("rien")
  } else {
    tableau<-question[1:(nrow(question)-3), ]
    if (ncol(question)==1){
      ordre<-order(names(tableau), decreasing=T)
      colorGraph<-couleur.f(data.frame(question[1:(nrow(question)-3), ]), ordre)
      pie(tableau[ordre], cex=1.6, col=colorGraph, #labels=paste(names(tableau)[ordre], round(tableau[ordre], digits=2), "%"),
        main=paste(titre, colnames(question), "\n (taux de réponse :", round(question["taux de réponse", ], digits=2), "%)"))
    } else {
      if ((length(rownames(question))-2)==2) {
        colorGraph<-"antiqueWhite1"
        for (i in 1:ncol(question)){
          if (sum(question[, i], na.rm=T)!=0) {
            pie(tableau[i], col=colorGraph, cex=1.6,  # labels=paste(rownames(question)[1], round(question[2, i], digits=2), "%"),
              main=paste(titre, colnames(question)[i], "\n (taux de réponse :", round(question["taux de réponse", i], digits=2), "%)"))
          }
        }
      } else {
        ordre<-order(rownames(tableau), decreasing=T)
        colorGraph<-couleur.f(question[1:(nrow(question)-3), ], ordre)
        for (i in 1:ncol(question)){
          if (sum(question[, i], na.rm=T)!=0) {
            pie(tableau[ordre, i], col=colorGraph, cex=1.6,  # labels=paste(rownames(tableau)[ordre], round(tableau[ordre, i], digits=2), "%"),
              main=paste(titre, colnames(tableau)[i], "\n (taux de réponse :", round(question["taux de réponse", i], digits=2), "%)"))
          }
        }
      }
    }
  }
}


### couleur des barplot (oui, non, nsp)
couleur.f<-function(reponse, ordre){
  tabcol<-matrix(NA, 90, 1)
  rownames(tabcol)<-c("oui", "non", "nsp", "NSP", "protection", "regle", "sensibilisation", "ressources", "bien", "inada", "insu", "trop",
	"pas_du_tout", "plutot_pas", "plutot_dac", "tout_a_fait", "tres_negatif", "plutot_negatif", "neutre", "positif", "plutot_positif", "tres_positif",
	"JMS", "RARE", "SVT", "TJRS", "NYVP", "assez", "mal_repartie", "pas_assez", "tournante", "<1an", ">20ans", ">30ans", "11a20ans", "1a5ans", "21a30ans", "6a10ans",
	"occa", "reg", "tr_reg", "bapteme", "debutant", "intermediaire", "expert", "0%", "1a25%", "26a50%", "51a75%", "76a100%", "IND", "annee", "ete", "hiver", "JS", "VAC", "WE",
	"AM", "PM", "SOIR", "NUIT", "LUNE", "MAR", "MET", "faible_nul", "modere", "decisif", "besoin", "manger", "plaisir", "vendre", "moins", "équivalentes", "plus",
	"F", "M", "actif", "agri", "artisan", "cadre", "employe", "etudiant", "intermed", "ouvrier", "retraite", "sans_act", "liberal", "NA", "resident", "non-resident")
  colnames(tabcol)<-"couleur"
# couleur réponses binaires
  tabcol["oui", 1]<-"darkseagreen1"
  tabcol["non", 1]<-"lightcoral"
  tabcol["nsp", 1]<-"gray85"
  tabcol["NSP", 1]<-"gray85"
# couleur réponse defAMP
  tabcol["protection", 1]<-"cornsilk"
  tabcol["regle", 1]<-"mistyrose"
  tabcol["sensibilisation", 1]<-"lightcyan"
  tabcol["ressources", 1]<-"lavender"
# couleur adéquation réglementation
  tabcol["bien", 1]<-"darkolivegreen2"
  tabcol["inada", 1]<-"darksalmon"
  tabcol["insu", 1]<-"lightcoral"
  tabcol["trop", 1]<-"peachpuff"
# couleur association processus
  tabcol["pas_du_tout", 1]<-"lightcoral"
  tabcol["plutot_pas", 1]<-"peachpuff"
  tabcol["plutot_dac", 1]<-"darkseagreen1"
  tabcol["tout_a_fait", 1]<-"lightseagreen"
# couleur effets
  tabcol["tres_negatif", 1]<-"lightcoral"
  tabcol["plutot_negatif", 1]<-"peachpuff"
  tabcol["neutre", 1]<-"lightgoldenrodyellow"
  tabcol["positif", 1]<-"lightgreen"
  tabcol["plutot_positif", 1]<-"darkseagreen1"
  tabcol["tres_positif", 1]<-"lightseagreen"
# couleur utilisation des corps morts
  tabcol["JMS", 1]<-"lightcoral"
  tabcol["RARE", 1]<-"peachpuff"
  tabcol["SVT", 1]<-"darkseagreen1"
  tabcol["TJRS", 1]<-"lightseagreen"
  tabcol["NYVP", 1]<-"lightgoldenrodyellow"
# couleur config AMP (trop et bien déjà codés)
  tabcol["assez", 1]<-"darkseagreen1"
  tabcol["mal_repartie", 1]<-"mistyrose"
  tabcol["pas_assez", 1]<-"lightcyan"
  tabcol["tournante", 1]<-"lavender"
# couleur ancienneté
  tabcol["<1an", 1]<-"lightcoral"
  tabcol[">20ans", 1]<-"cornsilk"
  tabcol[">30ans", 1]<-"lavender"
  tabcol["11a20ans", 1]<-"mistyrose"
  tabcol["1a5ans", 1]<-"lightcyan"
  tabcol["21a30ans", 1]<-"lightgoldenrodyellow"
  tabcol["6a10ans", 1]<-"lightblue"
# couleur pratique
  tabcol["occa", 1]<-"lavender"
  tabcol["reg", 1]<-"lightcyan"
  tabcol["tr_reg", 1]<-"lightblue"
  tabcol["bapteme", 1]<-"cornsilk"
  tabcol["debutant", 1]<-"lavender"
  tabcol["intermediaire", 1]<-"lightcyan"
  tabcol["expert", 1]<-"lightblue"
# couleur partAMP
  tabcol["0%", 1]<-"lightcoral"
  tabcol["1a25%", 1]<-"peachpuff"
  tabcol["26a50%", 1]<-"darkseagreen1"
  tabcol["51a75%", 1]<-"lightseagreen"
  tabcol["76a100%", 1]<-"lightgreen"
# couleur planification des sorties de pêche
  tabcol["IND", 1]<-"cornsilk"
  tabcol["annee", 1]<-"lightblue"
  tabcol["ete", 1]<-"lavender"
  tabcol["hiver", 1]<-"lightcyan"
  tabcol["JS", 1]<-"lightblue"
  tabcol["VAC", 1]<-"lavender"
  tabcol["WE", 1]<-"lightcyan"
  tabcol["AM", 1]<-"lightblue"
  tabcol["PM", 1]<-"lavender"
  tabcol["SOIR", 1]<-"lightcyan"
  tabcol["NUIT", 1]<-"mistyrose"
  tabcol["LUNE", 1]<-"lightblue"
  tabcol["MAR", 1]<-"lavender"
  tabcol["MET", 1]<-"lightcyan"
# couleur influence act et AMP
  tabcol["faible_nul", 1]<-"peachpuff"
  tabcol["modere", 1]<-"palegreen"
  tabcol["decisif", 1]<-"darkseagreen1"
# couleur raison pêche
  tabcol["besoin", 1]<-"cornsilk"
  tabcol["manger", 1]<-"mistyrose"
  tabcol["plaisir", 1]<-"lightcyan"
  tabcol["vendre", 1]<-"lavender"
# couleur evolution captures
  tabcol["moins", 1]<-"peachpuff"
  tabcol["équivalentes", 1]<-"palegoldenrod"
  tabcol["plus", 1]<-"darkseagreen1"
# couleur sexe
  tabcol["F", 1]<-"mistyrose"
  tabcol["M", 1]<-"mintcream"
# couleur CSP
  tabcol["actif", 1]<-"wheat"
  tabcol["agri", 1]<-"thistle"
  tabcol["artisan", 1]<-"slategray1"
  tabcol["cadre", 1]<-"lightpink"
  tabcol["employe", 1]<-"papayawhip"
  tabcol["etudiant", 1]<-"palegoldenrod"
  tabcol["intermed", 1]<-"lightcyan"
  tabcol["ouvrier", 1]<-"lightsalmon"
  tabcol["retraite", 1]<-"lavender"
  tabcol["sans_act", 1]<-"lightyellow"
  tabcol["liberal", 1]<-"lightblue"
  tabcol["NA", 1]<-"white"
# couleur résidents
  tabcol["resident", 1]<-"lightyellow"
  tabcol["non-resident", 1]<-"lightblue"
# choix de la couleur
  colorG<-factor(tabcol[, 1][match(rownames(reponse)[ordre], rownames(tabcol))])
  colorGraph<-as.character(colorG)
  print(colorGraph)
}

### couleur des grandes catégories d'activités
couleurActivites.f<-function(reponse, ordre){
  tabcolAct<-matrix(NA, 13, 1)
  rownames(tabcolAct)<-c("peche", "plaisance", "plongee", "excursion", "EMB", "CHAS", "BORD", "RAMA", "EXCU", "SSM", "resident", "non-resident", "NA")
  tabcolAct["peche", 1]<-"azure2"
  tabcolAct["plaisance", 1]<-"beige"
  tabcolAct["plongee", 1]<-"bisque"
  tabcolAct["excursion", 1]<-"cornsilk"
  tabcolAct["EMB", 1]<-"green"
  tabcolAct["CHAS", 1]<-"blue"
  tabcolAct["BORD", 1]<-"purple"
  tabcolAct["RAMA", 1]<-"orange"
  tabcolAct["EXCU", 1]<-"yellow"
  tabcolAct["SSM", 1]<-"red"
  tabcolAct["resident", 1]<-"orange"
  tabcolAct["non-resident", 1]<-"yellow"
  tabcolAct["NA", 1]<-"white"
# choix de la couleur
print(ordre)
  colorG<-factor(tabcolAct[, 1][match(reponse[ordre], rownames(tabcolAct))])
  colorAct<-as.character(colorG)
  print(colorAct)
}


## Fonction permettant de retirer les activités sans données des légendes des boxplot
EnleverZero.f = function (tableau, question, activite) {
  trans<-tapply(question, activite, lengthnna.f)
  nonNul<-names(trans)[which(trans!=0)]
  tableau2<-tableau[1, ]
  for (i in 1:length(nonNul)){
    tableau2<-rbind(tableau2, subset(tableau, activite==nonNul[i]))
  }
  tableau2<-tableau2[-1, ]
  return(tableau2)
}


## Fonction permettant de transformer les listes en matrix pour envoyer dans les barplot
Transfo.f = function (question, liste) {
  modalite<-sort(unique(question))
  toto<-matrix(0, (length(modalite)+1), length(names(liste)))
  rownames(toto)<-c(modalite, "taux de réponse")
  colnames(toto)<-names(liste)
  for (i in 1 : ncol(toto)) {
    toto[, i]<-data.frame(liste[i])[match(rownames(toto), rownames(data.frame(liste[i]))), 1]
  }
  toto[is.na(toto)==TRUE]<-0
  return(toto)
}


## message d'info de fin de programmation des fonctions
infoLoading.f(msg="Les fonctions ont bien été programmées.", icon="info")

################################################################################

stepInnerProgressBar.f(n=1, msg="Calculs et analyses préalables en cours")  # progression du chargement
#### pré analyses/calculs obligatoires pour le calcul des métriques

## mouillage des bateaux sur chacun des sites
  nbbatMouillage<-tapply(freqtot$immat, list(freqtot$numSortie, freqtot$zone, freqtot$mouillage), lengthnna.f) # ligne=num_sortie, col=zone, 3D=mouillage
  nbbatMouillage[which(is.na(nbbatMouillage)=="TRUE")]<-0
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
  Interet=refEspeces[, c("codeSp", paste("interetChasse", siteEtudie, sep=""), paste("interetLigne", siteEtudie, sep=""),
                      paste("interetFilet", siteEtudie, sep=""), paste("interetCasier", siteEtudie, sep=""),
                      paste("interetPied", siteEtudie, sep=""))]
colnames(Interet)<-c("spCode", "interetCHA", "interetLG", "interetFIL", "interetCAS", "interetPP")


## préalables au calcul des CPUE
prealableCPUE.f = function () {
if (nrow(peche)!=0) {
  peche <- peche[which(is.na(peche$heure)=="FALSE"), ]   # enlève les lignes dont la durée de pêche est inconnue (début de pêche-heure enquête)
  peche <- peche[which(is.na(peche$debutPec)=="FALSE"), ]
  peche$nbEngin1[is.na(peche$nbEngin1)]<-0
  peche$nbEngin2[is.na(peche$nbEngin2)]<-0
  peche$nbEngin3[is.na(peche$nbEngin3)]<-0

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

## pour calcul en surface
   surface3.f = function(tableau, surfaceConsideree) {
     surface3<-matrix(0, dim(tableau)[1], dim(tableau)[2])
     for (i in 1:(dim(tableau)[1])) {
      surface3[i, ]<-surfaceConsideree[match(dimnames(tableau)[[2]], names(surfaceConsideree))]
      }
     assign("surface3", surface3, envir=.GlobalEnv)
    }

## Fonctions de choix de l'utilisateur
#ChoixAnnee.f()   # à supprimer par as.numeric(format(Sys.Date(), "%Y"))
#ChoixSeuilFreq.f() # à mettre en choix d'option dans l'interface.

################################################################################
### construction d'une table pour les questions communes des questionnaires
stepInnerProgressBar.f(n=1, msg="Construction de la table 'tousQuest'")  # progression du chargement

anneeActuelle <- as.numeric(format(Sys.Date(),"%Y"))
## recup pêche
  pecheTrans2<-pecheQ
  pecheTrans2$activite<-rep("peche", nrow(pecheTrans2))
  pecheTrans2$activiteSpe<-pecheTrans2$actPeche1
  pecheTrans2$suffiCM<-rep(factor(NA), nrow(pecheTrans2))
  pecheTrans2$nbSortie1[is.na(pecheTrans2$nbSortie1)]<-0
  pecheTrans2$nbSortie2[is.na(pecheTrans2$nbSortie2)]<-0
  pecheTrans2$nbSortie<-pecheTrans2$nbSortie1+pecheTrans2$nbSortie2
  if (siteEtudie == "CB" || siteEtudie == "RUN") {
    pecheTrans2$nbSortie<-pecheTrans2$nbSortieTot
  }
  pecheTrans<-pecheTrans2[, c("AMP", "quest", "numSortie", "activite", "activiteSpe", "periodEchant", "dureeSortie", "pratique",
                             "anciennete", "nbSortie", "partAMP", "choixSite1", "choixSite2", "choixSite3", "budgetAn",
                             "existenceAMP", "influAMP", "defAMP", "connaisRegAMP", "suffiInfo", "adaptReg", "respectReg",
                             "assoProces", "effetEcosyst", "effetEcono", "effetAct", "relationPP", "relationPA", "relationCH",
                             "relationPL", "relationPS", "relationJS", "relationKS", "avisAMP", "connaisCM", "utilCM", "suffiCM",
                             "sexe", "anNais", "CSP", "pays", "commune", "resident", "nbNuit", "nbPersVoy", "depTot", "premVisi",
                             "anNb", "nbVisitAn", "revenir", "influAct")]

## recup plaisance
plaisance2<-plaisance
  plaisance2$AMP<-rep(siteEtudie, nrow(plaisance2))
  plaisance2$activite<-rep("plaisance", nrow(plaisance2))
  plaisance2$activiteSpe<-rep("plaisance", nrow(plaisance2))
  plaisance2$pratique<-rep(NA, nrow(plaisance2))
  plaisance2$anciennete<-rep(NA, nrow(plaisance2))
  plaisance2$nbSortie<-rep(NA, nrow(plaisance2))
  plaisance2$choixSite3<-rep(NA, nrow(plaisance2))
  plaisance2$budgetAn<-rep(NA, nrow(plaisance2))
  plaisance2$nbPersVoy<-rep(NA, nrow(plaisance2))
  plaisance2$influAct<-rep(NA, nrow(plaisance2))
  plaisanceTrans<-plaisance2[, c("AMP", "quest", "numSortie", "activite", "activiteSpe", "periodEchant", "dureeSortie", "pratique",
                                "anciennete", "nbSortie", "partAMP", "choixSite1", "choixSite2" , "choixSite3", "budgetAn",
                                "existenceAMP", "influAMP", "defAMP", "connaisRegAMP", "suffiInfo", "adaptReg", "respectReg",
                                "assoProces", "effetEcosyst", "effetEcono", "effetAct", "relationPP", "relationPA", "relationCH",
                                "relationPL", "relationPS", "relationJS", "relationKS", "avisAMP", "connaisCM", "utilCM", "suffiCM",
                                "sexe", "anNais", "CSP", "pays", "commune", "resident", "nbNuit", "nbPersVoy", "depTot", "premVisi",
                                "anNb", "nbVisitAn", "revenir", "influAct")]

## recup plongée
plongee2<-plongee
  plongee2$numSortie<-rep(NA, nrow(plongee2))
  plongee2$nbSortie<-plongee2$nbAn
  plongee2$activite<-rep("plongee", nrow(plongee2))
  plongee2$activiteSpe<-rep("plongee", nrow(plongee2))
  plongee2$ancien<-anneeActuelle-plongee2$anciennete
    plongee2$ancien2<-plongee2$ancien
    if (nrow(plongee)!=0){
    plongee2$ancien2[plongee2$ancien > 20] = ">20ans"
    plongee2$ancien2[11<=plongee2$ancien & plongee2$ancien<=20] = "11a20ans"
    plongee2$ancien2[6<=plongee2$ancien & plongee2$ancien<=10] = "6a10ans"
    plongee2$ancien2[1<plongee2$ancien & plongee2$ancien<=5] = "1a5ans"
    plongee2$ancien2[plongee2$ancien<=1]= "<1an"
    }
  plongee2$anciennete <- plongee2$ancien2
  plongee2$dureeSortie<-rep(NA, nrow(plongee2))
    plongee2$partAMP<-(plongee2$nbVisitAn/plongee2$nbAn)*100
    plongee2$partAMP2[plongee2$partAMP >= 76] = "76a100%"
    plongee2$partAMP2[51<=plongee2$partAMP & plongee2$partAMP<=75] = "51a75%"
    plongee2$partAMP2[26<=plongee2$partAMP & plongee2$partAMP<=50] = "26a50%"
    plongee2$partAMP2[1<=plongee2$partAMP & plongee2$partAMP<=25] = "1a25%"
    plongee2$partAMP2[plongee2$partAMP==0]= "0%"
  plongee2$partAMP <- plongee2$partAMP2
  plongee2$defAMP<-rep(NA, nrow(plongee2))
  plongee2$connaisRegAMP<-rep(NA, nrow(plongee2))
  plongee2$avisAMP<-rep(NA, nrow(plongee2))
  plongee2$connaisCM<-rep(NA, nrow(plongee2))
  plongee2$utilCM<-rep(NA, nrow(plongee2))
  plongee2$suffiCM<-rep(NA, nrow(plongee2))
  if (is.na(unique(plongee2$effetEcosyst)[1])==TRUE){
  plongee2$effetEcosyst<-plongee2$effetEcosystB}
  plongee2$premVisi<-rep(NA, nrow(plongee2))
  plongeeTrans<-plongee2[, c("AMP", "quest", "numSortie", "activite", "activiteSpe", "periodEchant", "dureeSortie", "pratique",
                            "anciennete", "nbSortie", "partAMP", "choixSite1", "choixSite2", "choixSite3", "budgetAn",
                            "existenceAMP", "influAMP", "defAMP", "connaisRegAMP", "suffiInfo", "adaptReg", "respectReg",
                            "assoProces", "effetEcosyst", "effetEcono", "effetAct", "relationPP", "relationPA",
                            "relationCH", "relationPL", "relationPS", "relationJS", "relationKS", "avisAMP", "connaisCM",
                            "utilCM", "suffiCM", "sexe", "anNais", "CSP", "pays", "commune", "resident", "nbNuit", "nbPersVoy",
                            "depTot", "premVisi", "anNb", "nbVisitAn", "revenir", "influAct")]

## recup excursion
excursion2<-excursion
  excursion2$numSortie<-rep(NA, nrow(excursion2))
  excursion2$activite<-rep("excursion", nrow(excursion2))
  excursion2$activiteSpe<-rep("EXCU", nrow(excursion2))
  excursion2$activiteSpe[excursion2$parcoursGuide=="oui"]<-"SSM"
  excursion2$dureeSortie<-rep(NA, nrow(excursion2))
  excursion2$pratique<-rep(NA, nrow(excursion2))
  excursion2$ancien<-anneeActuelle-excursion2$anciennete
    excursion2$ancien2<-excursion2$ancien
    if (nrow(excursion)!=0){
    excursion2$ancien2[excursion2$ancien > 20] = ">20ans"
    excursion2$ancien2[11<=excursion2$ancien & excursion2$ancien<=20] = "11a20ans"
    excursion2$ancien2[6<=excursion2$ancien & excursion2$ancien<=10] = "6a10ans"
    excursion2$ancien2[1<excursion2$ancien & excursion2$ancien<=5] = "1a5ans"
    excursion2$ancien2[excursion2$ancien<=1]= "<1an"
    }
  excursion2$anciennete <- excursion2$ancien2
  excursion2$nbSortie<-rep(NA, nrow(excursion2))
  excursion2$partAMP<-rep(NA, nrow(excursion2))
  excursion2$choixSite3<-rep(NA, nrow(excursion2))
  excursion2$budgetAn<-rep(NA, nrow(excursion2))
  excursion2$adaptReg<-rep(NA, nrow(excursion2))
  excursion2$respectReg<-rep(NA, nrow(excursion2))
  excursion2$assoProces<-rep(NA, nrow(excursion2))
  excursion2$effetEcono<-rep(NA, nrow(excursion2))
  excursion2$effetAct<-rep(NA, nrow(excursion2))
  excursion2$relationPP<-rep(NA, nrow(excursion2))
  excursion2$relationPA<-rep(NA, nrow(excursion2))
  excursion2$relationCH<-rep(NA, nrow(excursion2))
  excursion2$relationPL<-rep(NA, nrow(excursion2))
  excursion2$relationPS<-rep(NA, nrow(excursion2))
  excursion2$relationJS<-rep(NA, nrow(excursion2))
  excursion2$relationKS<-rep(NA, nrow(excursion2))
  excursion2$anNb<-rep(NA, nrow(excursion2))
  excursionTrans<-excursion2[, c("AMP", "quest", "numSortie", "activite", "activiteSpe", "periodEchant", "dureeSortie", "pratique",
                                "anciennete", "nbSortie", "partAMP", "choixSite1", "choixSite2", "choixSite3", "budgetAn",
                                "existenceAMP", "influAMP", "defAMP", "connaisRegAMP", "suffiInfo", "adaptReg", "respectReg",
                                "assoProces", "effetEcosyst", "effetEcono", "effetAct", "relationPP", "relationPA", "relationCH",
                                "relationPL", "relationPS", "relationJS", "relationKS", "avisAMP", "connaisCM", "utilCM", "suffiCM",
                                "sexe", "anNais", "CSP", "pays", "commune", "resident", "nbNuit", "nbPersVoy", "depTot", "premVisi",
                                "anNb", "nbVisitAn", "revenir", "influAct")]
## compil tous questionnaires
tousQuest<-rbind(pecheTrans, plaisanceTrans, plongeeTrans, excursionTrans)
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
limActPeche<-c(0, length(unique(peche$act_peche1)))
limResPeche<-c(0, length(unique(peche$resident)))
limActTot<-c(0, length(unique(tousQuest$activite)))
limActDet<-c(0, length(unique(tousQuest$activitePec)))
limResTot<-c(0, length(unique(tousQuest$resident)))

### sauvegarde de tous les fichiers importés
FREQTOT<-freqtot      # fichiers initiaux
PECHE<-peche
PECHEQ<-pecheQ
CAPTURES<-captures
CAPTURES2<-captures2
CAPTURESAN<-capturesAn
PLAISANCE<-plaisance
PLONGEE<-plongee
EXCURSION<-excursion
TOUSQUEST<-tousQuest
freqtotRef<-freqtot
freqtotModif<-freqtot  # fichiers après un choix de l'utilisateur (ex : un usage, une année, etc.)
pecheModif<-peche
pecheQModif<-pecheQ
capturesModif<-captures
captures2Modif<-captures2
capturesAnModif<-capturesAn
plaisanceModif<-plaisance
plongeeModif<-plongee
excursionModif<-excursion
tousQuestModif<-tousQuest
freqtotRefModif<-freqtot

#mise à jour de l'interface
MiseajourTableau.f(tclarray)
MiseajourTableauInfo.f(tclarray2)
tkconfigure(ResumerAMP, text=paste("\n Vous avez importé les jeux de données de", siteEtudie, " \n \n"))
gestionMSGaide.f("traitements")

## Fin des informations de chargement (demande de confirmation utilisateur) :
    stepInnerProgressBar.f(n=0, msg="Fin de chargement !",
                           font=tkfont.create(weight="bold", size=9), foreground="darkred")
    infoLoading.f(msg="Les pré-Analyses ont bien été réalisées ! \n Vous pouvez maintenant procéder à la programmation des metriques", icon="info")
    infoLoading.f(button=TRUE)


################################################################################
