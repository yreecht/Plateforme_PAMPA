################################################################################
# Nom               : TestsStatistiquesEnquetes.r
# Type              : Programme
# Objet             : Ce programme comporte toutes les fonctions d'analyses 
#                     statistiques pour l'étude des données d'enquêtes (qualitatives)
#                     Ces fonctions seront appelées dans l'interface relative aux 
#                     traitements statistiques des données d'enquêtes
# Input             : tableaux importés par le topMenu.r
# Output            : lancement de fonctions
# Auteur            : Elodie Gamp
# R version         : 2.11.1
# Date de création  : janvier 2012
# Sources
################################################################################


################################################################################

###                Pour les variables qualitatives binomiales                ###

################################################################################
resFileGLM.f <- function(nomData, metrique, listFact, prefix=NULL) {

    ## Purpose: Définit les noms du fichiers pour les résultats des modèles
    ##          linéaires généralisés. L'extension et un prefixe peuvent être 
    ##          précisés, mais par défaut, c'est le fichier de sorties texte 
    ##          qui est créé.
    ## ----------------------------------------------------------------------
    ## Arguments: 
    ##            nomData : nom de la table de données
    ##            metrique : nom de la métrique analysée
    ##            listFact : vecteur des noms de facteurs de l'analyse
    ##            prefix : préfixe du nom de fichier (binom ou multinom)
    ## ----------------------------------------------------------------------
    ## Author: Elodie Gamp, Date:  janvier 2012

    ## Nom de fichier :
    filename <- paste("C:/PAMPA/Resultats_Usages/stats/", prefix, "_",
                      ## nom du tableau utilisé
                      nomData, "_",                      
                      ## Métrique analysée :
                      metrique, "_",
                      ## liste des facteurs de l'analyse
                      paste(listFact, collapse="-"),
                      ## Extension du fichier :
                      ".txt", sep="")

    ## Ouverture de la connection (retourne l'objet de type 'connection'
        return(resFile <- file(filename, open="w"))
}

################################################################################
modelBIqualiWP3.f <- function(metrique=metrique, listFact=listFact, Data=Data, resFile=resFile) {

    ## Purpose: Gérer les différentes étapes des modèles linéaires généralisés 
    #           binomiaux pour les métriques Usages
    ## ----------------------------------------------------------------------
    ## Arguments: 
    ##            metrique : la métrique choisie
    ##            listFact : liste du (des) facteur(s) de regroupement
    ##            Data : les données utilisées
    ##            resFile : fichier de sortie
    ## ----------------------------------------------------------------------
    ## Author: Elodie Gamp, Date: janvier 2012
  
  ## transformation des données (suppression NA et variables en facteur)
    Data <- Data[!is.na(Data[,metrique]),]
    listFactOriginal <- listFact
    aGarder = NULL
  
  ## ne garde les facteurs que si le nombre de niveaux est sup à 1  
    for (i in 1 : length(listFact)) {
      Data[,listFact[i]] <- as.factor(Data[,listFact[i]])
      if(length(levels(Data[,listFact[i]])) > 1) {aGarder=c(aGarder,i)}
    }
    listFact <- listFact[aGarder]  # nouvelle liste de facteurs à tester
    
    if (length(listFact)==0) {     # si aucun facteur retenu
      cat(paste("Il est impossible de tester les facteurs", listFactOriginal, "sur la métrique", metrique), file=resFile, fill=1)
    } else {
    
    ## définition de la formule et du modèle binomial
      formule <- eval(parse(text=paste(metrique, "~", paste(listFact, collapse=" * "))))
      glmtest <- glm(formule, data=Data, family="binomial")
      
      ## Formule de modèle lisible:
        glmtest$call$formula <- formule
#        formule <<- formule    ## assignation dans l'environnement global
        sumLM <- summary.glm(glmtest)
      
    ## Enregistrement des informations sur le modèle :
      cat("---------------------------------------------------------------------------\n", file=resFile)
      cat("Modèle ajusté :", file=resFile, fill=1)
      cat("\t", deparse(glmtest$call), "\n\n", file=resFile, sep="")
  
    ## Anova globale du modèle
      anovaglm <- anova(glmtest, test="Chisq") 
      capture.output(print.anova.fr(anovaglm), file=resFile)   
      
    ## Significativités des paramètres :
      cat("\n\n---------------------------------------------------------------------------\n", file=resFile)
      cat("Significativités des paramètres ",
          "\n(seuls ceux correspondant à des facteurs/intéractions significatifs sont représentés) :\n\n",
          file=resFile)
  
      capture.output(printCoefmat.red(sumLM$coef, anovaLM=anovaglm, objLM=glmtest), file=resFile)
    }
}


################################################################################
sortiesBINOM.f <- function(metrique, listFact, Data, nomData, prefix="binom") {

    ## Purpose: Formater les résultats de glm binomial et les écrire dans un fichier
    ## ----------------------------------------------------------------------
    ## Arguments: 
    ##            metrique : la métrique choisie.
    ##            listFact : liste du (des) facteur(s) de regroupement.
    ##            Data : les données utilisées.
    ##            nomData : nom de la table de données
    ##            prefix : préfixe du nom de fichier (binom ou multinom)
    ## ----------------------------------------------------------------------
    ## Author: Elodie Gamp, Date: janvier 2012

    ## longueur des lignes pour les sorties textes :
    oOpt <- options()
    on.exit(options(oOpt))

    options(width=120)

    ## Chemin et nom de fichier :
    resFile <- resFileGLM.f(nomData=nomData,
                            metrique=metrique, 
                            listFact=listFact,
                            prefix=prefix)
    on.exit(close(resFile), add=TRUE)

    modelBIqualiWP3.f (metrique=metrique, listFact=listFact, Data=Data, resFile=resFile)

}


################################################################################

###              Pour les variables qualitatives multinomiales               ###

################################################################################
resFileMULT.f <- function(nomData, metrique, listFact, prefix=NULL){

    ## Purpose: Définit les noms du fichiers pour les résultats des modèles
    ##          multinomiaux. L'extension et un prefixe peuvent êtres précisés,
    ##          mais par défaut, c'est le fichier de sorties texte qui est
    ##          créé.
    ## ----------------------------------------------------------------------
    ## Arguments: 
    ##            nomData : nom de la table de données
    ##            metrique : nom de la métrique analysée.
    ##            listFact : vecteur des noms de facteurs de l'analyse.
    ##            prefix : préfixe du nom de fichier (binom ou multinom)
    ## ----------------------------------------------------------------------
    ## Author: Elodie Gamp, Date:  janvier 2012

    ## Nom de fichier :
    filename <- paste("C:/PAMPA/Resultats_Usages/stats/", prefix, "_",
                      ## nom du tableau utilisé
                      nomData, "_",
                      ## Métrique analysée :
                      metrique, "_",
                      ## liste des facteurs de l'analyse
                      paste(listFact, collapse="-"),
                      ## Extension du fichier :
                      ".txt", sep="")

    ## Ouverture de la connection (retourne l'objet de type 'connection'
        return(resFile <- file(filename, open="w"))
}


################################################################################
modelMULTIqualiWP3.f <- function(metrique, listFact, Data, resFile=resFile){

    ## Purpose: Gérer les différentes étapes des modèles linéaires généralisés 
    #           multinomiaux pour les métriques Usages
    ## ----------------------------------------------------------------------
    ## Arguments: 
    ##            metrique : la métrique choisie.
    ##            listFact : liste du (des) facteur(s) de regroupement.
    ##            Data : les données utilisées.
    ##            resFile : fichier de sortie
    ## ----------------------------------------------------------------------
    ## Author: Elodie Gamp, Date: janvier 2012

#    Data <- Data[!is.na(Data[,metrique]),]
    Data <- Data[apply(as.data.frame(Data[,c(metrique,listFact)]), 1, FUN = function(x) {all(!is.na(x))}),]    
    listFactOriginal<-listFact
    aGarder=NULL
    
    ## vérifie que les facteurs choisis ont plus d'un niveau
    for (i in 1 : length(listFact)) {
      Data[,listFact[i]]<-as.factor(Data[,listFact[i]])
      if(length(levels(Data[,listFact[i]])) > 1) {aGarder=c(aGarder,i)}
    }
    listFact<-listFact[aGarder]   ## nouvelle liste de facteur

    if (length(listFact)==0) {     ## vérifie que la nouvelle liste est non nulle
      cat(paste("Il est impossible de tester les facteurs", listFactOriginal, "sur la métrique", metrique), file=resFile, fill=1)
    } else { 

  #metrique<-"effetEcosyst"
  #listFact<-c("periodEchant","activite")
  #Data<-tousQuest

  ### formules utilisées pour construire les différents modèles
      exprML0 <- eval(parse(text=paste(metrique, "~", 1)))    
      exprML1 <- eval(parse(text=paste(metrique, "~", listFact[1])))    
    if (length(listFact)>1) { 
      exprML3 <- eval(parse(text=paste(metrique, "~", paste(listFact, collapse=" + "))))    
      exprML4 <- eval(parse(text=paste(metrique, "~", paste(listFact, collapse=" * "))))    
    }
       # mod0<-multnom(Y~1)
      multi0<-multinom(exprML0, data=Data)  # modèle nul pour l'effet du premier facteur 
      ## Formule de modèle lisible:
      multi0$call$formula <- exprML0
#     exprML0 <<- exprML0
      sumMulti0 <- summary(multi0)
      
       # mod1<-multnom(Y~X)
      multi1<-multinom(exprML1, data=Data)  # modèle avec facteur 1 
      ## Formule de modèle lisible:
      multi1$call$formula <- exprML1
#     exprML1 <<- exprML1
      sumMulti1 <- summary(multi1)
    
    if (length(listFact)>1) {        
       # mod2<-multnom(Y~X+Z)  
      multi2<-multinom(exprML3, data=Data)  # modèle avec les deux facteurs sans intéraction
      ## Formule de modèle lisible:
      multi2$call$formula <- exprML3
#     exprML3 <<- exprML3
      sumMulti2 <- summary(multi2)
      
       # mod3<-multnom(Y~X*Z)
      multi3<-multinom(exprML4, data=Data)   # modèle avec les deux facteurs et intéraction
      ## Formule de modèle lisible:
      multi3$call$formula <- exprML4
#     exprML4 <<- exprML4
      sumMulti3 <- summary(multi3)
    }
        ## Enregistrement des informations sur les modèles :
      cat("---------------------------------------------------------------------------\n", file=resFile)
      cat("Modèles ajustés :", file=resFile, fill=1)
      cat("\t", deparse(sumMulti0$call), "\n\n", file=resFile, sep="")
      cat("\t", deparse(sumMulti1$call), "\n\n", file=resFile, sep="")
    if (length(listFact)>1) {        
      cat("\t", deparse(sumMulti2$call), "\n\n", file=resFile, sep="")
      cat("\t", deparse(sumMulti3$call), "\n\n", file=resFile, sep="")
    }
  
     #les 4 modèles emboités du plus simple au plus complexe pour voir les effets à chaque pas par anova entre 2 modèles  
      anovamulti1 <- anova(multi1, multi0)        # test de l'effet du premier facteur
      cat("\n\n---------------------------------------------------------------------------\n", file=resFile)
      cat(paste("\neffet du premier facteur :",listFact[1],"\n"), file=resFile)
      capture.output(print.anova.fr(anovamulti1), file=resFile)   
    if (length(listFact)>1) {            
      anovamulti2 <- anova(multi2, multi1)        # test de l'effet du deuxième facteur
      cat("\n---------------------------------------------------------------------------\n", file=resFile)
      cat(paste("\neffet du deuxième facteur :",listFact[2],"\n"), file=resFile)   
      capture.output(print.anova.fr(anovamulti2), file=resFile)  
       
      anovamultiInt <- anova(multi3, multi2)        # test de l'effet de l'interaction entre les deux facteurs
      cat("\n---------------------------------------------------------------------------\n", file=resFile)
      cat(paste("\neffet de l'intéraction des facteurs :",listFact[1],listFact[2],"\n"), file=resFile)   
      capture.output(print.anova.fr(anovamultiInt), file=resFile) 
      cat("\n---------------------------------------------------------------------------\n", file=resFile)     
    }
  }
}


###############################################################################
sortiesMULTI.f <- function(metrique, listFact, Data, nomData, prefix="multinom"){

    ## Purpose: Formater les résultats de lm et les écrire dans un fichier
    ## ----------------------------------------------------------------------
    ## Arguments: 
    ##            metrique : la métrique choisie.
    ##            listFact : liste du (des) facteur(s) de regroupement.
    ##            Data : les données utilisées.
    ##            nomData : nom de la table de données
    ##            prefix : préfixe du nom de fichier (binom ou multinom)
    ## ----------------------------------------------------------------------
    ## Author: Elodie Gamp, Date: janvier 2012

    ## longueur des lignes pour les sorties textes :
    oOpt <- options()
    on.exit(options(oOpt))

    options(width=120)

    ## Chemin et nom de fichier :
    resFile <- resFileMULT.f(nomData=nomData, 
                             metrique=metrique, 
                             listFact=listFact,
                             prefix=prefix)
    on.exit(close(resFile), add=TRUE)

    modelMULTIqualiWP3.f (metrique=metrique, listFact=listFact, Data=Data, resFile=resFile)

}


################################################################################





