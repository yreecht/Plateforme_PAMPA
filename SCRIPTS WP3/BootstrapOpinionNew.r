################################################################################
# Nom               : BootstrapOpinion.r
# Type              : Programme
# Objet             : Fonctions bootstrap des métriques d'opinion à partir 
#                     des questionnaires des différents usagers
# Input             : data importées en txt
# Output            : tableaux et graphs
# Auteur            : Elodie Gamp
# R version         : 2.8.1
# Date de création  : novembre 2011
# Sources
################################################################################


########################################################################################################################
BootstrapOpinionQuali.f <- function(tableau, facteur, metrique)
{
    ## Purpose: Lance le calcul des intervalles de confiance pour les questions
    ##          d'enquêtes qualitatives et retourne les IC estimés
    ## ----------------------------------------------------------------------
    ## Arguments: tableau : table de données (data.frame)
    ##            facteur : nom du facteur de séparation (character)
    ##            metrique : nom de la métrique (character)
    ## ----------------------------------------------------------------------
    ## Author: Elodie Gamp, Date: 16 novembre 2011

  # variables nécessaires pour le bootstrap
    Nboot = 1000  # taille du bootstrap (nb de réplications)
    activiteDispo <- unique( tableau [ , facteur ] )
    nbActivite <- length(activiteDispo)
    tableau [ , facteur ] <- as.factor(tableau [ , facteur ])
    
  ## déclaration du tableau de stockage pour les résultats bootstrap
    questBoot <- array(NA,c(nlevels(tableau [ , metrique ]),nlevels(tableau [ , facteur ]),Nboot), 
                       dimnames=list(levels(tableau [ , metrique ]),levels(tableau [ , facteur ]),c(1:Nboot)))
  
  # fonction de rééchantillonnage toutes activités confondues : calcul des proportions par activité et par métriques
  for (b in 1 : Nboot) {
      QuestTire <- sample(tableau$quest,replace=T,size=nrow(tableau))
      QuestEchant <- tableau[match(QuestTire,tableau$quest),]
      rsltEchant <- table(QuestEchant[,metrique],QuestEchant[, facteur])
      propRsltEchant <- round(prop.table(rsltEchant,2)*100,digits=2)
    
      questBoot [,,b] <- propRsltEchant
  }
  
  ### estimation existence de l'AMP
  moyboot <- apply(questBoot,c(1,2),mean,na.rm=T)
  ICinfboot <- apply(questBoot,c(1,2),quantile,probs=0.025,na.rm=T)
  ICsupboot <- apply(questBoot,c(1,2),quantile,probs=0.975,na.rm=T)
  estim <- array(c(ICinfboot,moyboot,ICsupboot),
                 c(nlevels(tableau[, metrique]),nlevels(tableau[, facteur]),3))
  dimnames(estim) <- list (rownames(moyboot),colnames(moyboot),c("ICInf","Moy","ICSup"))
  return(estim)
  
}


################################################################################################################################
BootstrapOpinionQuanti.f <- function(tableau, facteur, metrique)
{
    ## Purpose: Lance le calcul des intervalles de confiance pour les questions
    ##          d'enquêtes quantitatives et retourne les IC estimés
    ## ----------------------------------------------------------------------
    ## Arguments: tableau : table de données (data.frame)
    ##            facteur : nom du facteur de séparation (character)
    ##            metrique : nom de la métrique (character)
    ## ----------------------------------------------------------------------
    ## Author: Elodie Gamp, Date: 16 novembre 2011

  # variables nécessaires pour le bootstrap
    Nboot = 1000  # taille du bootstrap (nb de réplications)
    activiteDispo <- unique( tableau [ , facteur ] )
    nbActivite <- length(activiteDispo)
    
  ## déclaration du tableau de stockage pour les résultats bootstrap
    questBoot <- array(NA,c(1,nlevels(tableau [ , facteur ]),Nboot), 
                       dimnames=list("valeur",levels(tableau [ , facteur ]),c(1:Nboot)))
  
  # fonction de rééchantillonnage toutes activités confondues : calcul des proportions par activité et par métriques
  for (b in 1 : Nboot) {
      QuestTire <- sample(tableau$quest,replace=T,size=nrow(tableau))
      QuestEchant <- tableau[match(QuestTire,tableau$quest),]
      rsltEchant <- tapply(QuestEchant[,metrique],QuestEchant[, facteur],mean,na.rm=T)    
      questBoot [,,b] <- rsltEchant
  }
  
  ### estimation existence de l'AMP
  moyboot <- apply(questBoot,c(1,2),mean,na.rm=T)
  ICinfboot <- apply(questBoot,c(1,2),quantile,probs=0.025,na.rm=T)
  ICsupboot <- apply(questBoot,c(1,2),quantile,probs=0.975,na.rm=T)
  estim <- array(c(ICinfboot,moyboot,ICsupboot),
                 c(1,nlevels(tableau[, facteur]),3))
  dimnames(estim) <- list (rownames(moyboot),colnames(moyboot),c("ICInf","Moy","ICSup"))
  return(estim)
  
}


################################################################################################################################
