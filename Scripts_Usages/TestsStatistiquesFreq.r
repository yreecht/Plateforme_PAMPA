################################################################################
# Nom               : TestsStatistiquesFreq.r
# Type              : Programme
# Objet             : Ce programme comporte toutes les fonctions d'analyses 
#                     statistiques pour l'étude des métriques de fréquentation
#                     Ces fonctions seront appelées dans l'interface relative aux 
#                     traitements statistiques des données quantitatives
# Input             : aucun
# Output            : lancement de fonctions
# Auteur            : Elodie Gamp
# R version         : 2.11.1
# Date de création  : janvier 2012
# Sources
################################################################################


############               LES ANALYSES DE VARIANCE                 ############
###                    Pour les variables quantitatives                      ###


################################################################################
subsetToutesTables.f <- function(variable, listFact, facteurs, tableMetrique) {
    ## Purpose: Former le tableau de données nécessaires pour les analyses de variance, 
    ##          d'après les métriques et facteur(s) séléctionnés
    ## ----------------------------------------------------------------------
    ## Arguments: variable : la métrique choisie
    ##            listFact : les facteurs sélectionnés
    ##            facteurs : les facteurs sélectionnés + numSortie ou numQuest
    ##            tableMetrique : le nom de la table de données
    ## ----------------------------------------------------------------------
    ## Author: Elodie Gamp, Date:  janvier 2012

#  variable = "nbBat"
#  facteurs = c("numSortie","periodEchant","zone")
#  listFact = c("zone","periodEchant")
#  tableMetrique = "freqtot"         # ne contient que le nom du tableau considéré
  dataMetrique <- eval(parse(text=tableMetrique))      # donc dataMetrique = freqtot par exemple

      # FreqJour <- tapply(dataMetrique[,variable],list(factor(dataMetrique[,listFact[1]]),
      #                                    factor(dataMetrique[,listFact[2]]), factor(dataMetrique$numSortie)),
      #                                    sum , na.rm=T)
      # FreqJourList <- unlist(apply(FreqJour, 3, list), recursive=FALSE)
      # do.call(rbind, FreqJourList)
      
        
  ## Subset en fonction de la table de métrique
  switch(tableMetrique,                                    # permet de faire des traitements différents selon le nom du tableau considéré
      ## Cas de la table de fréquentation
      freqtot2 = {
             FreqJour <- with(dataMetrique,
                              tapply(dataMetrique[ , variable],
                              as.list(dataMetrique[, c("numSortie", listFact)]),          # listFact correspond aux facteurs à tester
                              sum, na.rm=TRUE))
    
          ## Formation du nouveau tableau pour y appliquer les analyses de variances
          ## selon les facteurs choisis
            if (length(listFact) == 2) {
                tmp <- as.data.frame(matrix(0,length(FreqJour),4))  
                colnames(tmp) <- c(names(dimnames(FreqJour)), variable)
                tmp [,1] <- rep(as.vector(unlist(dimnames(FreqJour)["numSortie"])),
                                time=(length(FreqJour)/dim(FreqJour)[1]))              # répéte le numéro de sortie
                tmp [,2] <- rep (as.vector(unlist(dimnames(FreqJour)[listFact[1]])),
                                 each=dim(FreqJour)[1], times = dim(FreqJour)[3])      # répéte le code de la zone
                tmp [,3] <- rep (as.vector(unlist(dimnames(FreqJour)[listFact[2]])),
                                 each=(dim(FreqJour)[1]*dim(FreqJour)[2]))             # répéte le code de la zone
                tmp [,4] <- c(FreqJour)
            }
            if (length(listFact) == 1) { 
                tmp <- as.data.frame(matrix(0,length(FreqJour),3))  
                colnames(tmp) <- c(names(dimnames(FreqJour)), variable)
                tmp [,1] <- rep(as.vector(unlist(dimnames(FreqJour)["numSortie"])),
                                time=(length(FreqJour)/dim(FreqJour)[1]))              # répéte le numéro de sortie
                tmp [,2] <- rep (as.vector(unlist(dimnames(FreqJour)[listFact[1]])),
                                 each=(length(FreqJour)/dim(FreqJour)[2]))             # répéte le code de la zone
                tmp [,3] <- c(FreqJour)
            }
      },
      ### cas des tables d'enquêtes
      peche = {
        tmp = peche [,c(facteurs, variable)]
      },
      pecheQ = {                                         
        tmp = pecheQ [,c(facteurs, variable)]
      },
      tousQuest = {
        tmp = tousQuest [,c(facteurs, variable)]
      },
      plaisance = {
        tmp = plaisance [,c(facteurs, variable)]
      },
      excursion = {
        tmp = excursion [,c(facteurs, variable)]
      },
      plongee = {
        tmp = plongee [,c(facteurs, variable)]
      },
      ## Autres cas :
        tkmessageBox(message="Erreur dans la sélection de la table", icon="info")
    )
    
    ### transformation des valeurs numériques en integer si valeurs entières
      aa <- round(tmp[,variable])
      a <- tmp[,variable]
      if (all(na.omit(a==aa))){tmp[,variable] <- as.integer(tmp[,variable])}     
      tmp <- tmp[!is.na(tmp[,variable]),]       ## suppression des NA = valeurs non observées
      
    ## transformation des variables explicatives en facteur
    for (i in 1 : length(listFact)){
      tmp[,listFact[i]] <- as.factor(tmp[,listFact[i]])
    }
    
    return(tmp)
}


################################################################################
modeleLineaireWP3.f <- function(variable, facteurs, listFact, tableMetrique, sufixe=NULL, ...) {
    ## Purpose: Gestions des différentes étapes des modèles linéaires.
    ## ----------------------------------------------------------------------
    ## Arguments: variable : la métrique choisie
    ##            listFact : liste du (des) facteur(s) explicatifs à tester
    ##            facteurs : liste des facteurs à tester (listFact) + numSortie ou numQuest
    ##            tableMetrique : nom de la table de métriques
    ## ----------------------------------------------------------------------
    ## Author: Elodie Gamp, Date: janvier 2012

    # facteurs <- facteurs
    # listFact <- listFact
    listFactOriginal <- listFact
    aGarder=NULL
    # vérification de la présence de plus d'un niveau pour chaque facteur choisi
    tableTest <- eval(parse(text = tableMetrique)) 
    for (i in 1 : length(listFact)) {
      if(length(levels(as.factor(tableTest[,listFact[i]]))) > 1 ) {aGarder = c(aGarder,i)}
    }
    listFact <- listFact[aGarder]  # nouvelle liste de facteur
    # si la liste des nouveaux facteurs est vide
    if (length(listFact)==0) {
      print(paste("Il est impossible de tester les facteurs", listFactOriginal, "sur la métrique", variable))
    } else {
      ## Données pour la série d'analyses :
      tmpData <- subsetToutesTables.f(variable = variable, 
                                      facteurs = facteurs, 
                                      listFact = listFact, 
                                      tableMetrique = tableMetrique)
  
      ## Formules pour différents modèles (avec ou sans transformation log)
      exprML <- eval(parse(text=paste(variable, "~", paste(listFact, collapse=" * "))))
      logExprML <- eval(parse(text=paste("log(", variable, ") ~", paste(listFact, collapse=" * "))))
  
      ## Sauvegarde temporaire des données utilisées pour les analyses 
      ## (attention : écrasée à chaque nouvelle série de graphiques)
      DataBackup <<- list()
      tmpDataMod <- tmpData  # sauvegarde
  
  
      ## Aide au choix du type d'analyse
      loiChoisie <- choixDistri.f(variable = variable, 
                                  Data = tmpDataMod[ , variable, drop=FALSE])
  
          if (!is.null(loiChoisie)) {          
              message("Loi de distribution choisie = ", loiChoisie)
                                                                        
              Log <- FALSE
              formule <- exprML
  
              switch(loiChoisie,
                     ## Modèle linéaire :
                     NO = {
                         res <- lm(exprML, data = tmpDataMod)
                    },
                     ## Modèle linéaire, données log-transformées
                     LOGNO = {
                         ## Ajout d'une constante à la métrique si contient des zéros
                         if (sum(tmpDataMod[ , variable] == 0, na.rm=TRUE))
                         {
                             tmpDataMod[ , variable] <- tmpDataMod[ , variable] +
                                 ((min(tmpDataMod[ , variable], na.rm=TRUE) + 1) / 1000)
                         } else {}
  
                         res <- lm(logExprML, data=tmpDataMod)
                         ## Mise en forme :
                         Log <- TRUE
                         formule <- logExprML
                     },
                     ## GLM, distribution de Poisson
                     PO = {
                         res <- glm(exprML, data=tmpDataMod, family="poisson")
                     },
                     ## GLM, distribution binomiale négative
                     NBI = {
                         res <- glm.nb(exprML, data=tmpDataMod)
                     },)
  
                res <<- res
                              
                  sortiesLM.f(objLM = res, formule = formule, variable = variable,
                          listFact = listFact, Data = tmpDataMod, Log = Log, sufixe = sufixe)
  
              resid.out <- boxplot(residuals(res), plot=FALSE)$out
  
          } else {
              message("Annulé !")
          }
      }
}


################################################################################
choixDistri.f <- function(variable, Data) {
    ## Purpose: Aider l'utilisateur dans le choix d'une distribution de la
    ##          métrique et lancer les analyses adéquates.
    ## ----------------------------------------------------------------------
    ## Arguments: variable : le nom de la métrique (variable dépendant)
    ##                       choisie
    ##            Data : le jeu de données contenant la métrique
    ## ----------------------------------------------------------------------
    ## Author: Elodie Gamp, Date: janvier 2012

    ## Systématiquement détruire la fenêtre en quitant :
    on.exit(tkdestroy(WinDistri))


    ## ##################################################
    ## Variables :
    env <- environment()                # environnement courant.
    Done <- tclVar(0)                   # État d'exécution.
    LoiChoisie <- tclVar("NO")          # Variable pour le choix de distribution théorique.
    vscale <- 0.6                      # dimension verticale des graphiques.
    hscale <- 1.05                       # dimension horizontale des graphiques.
    pointsize <- 10                     # taille du point pour les graphiques
    distList <- list()                  # liste pour le stockage des AIC et autres.


    ## ##################################################
    ## Éléments graphiques :
    WinDistri <- tktoplevel()           # Fenêtre principale.
    tkwm.title(WinDistri, paste("Choix de distribution théorique de la métrique '", variable, "'", sep=""))

    tmp <- tktoplevel(WinDistri)
    tkfocus(tmp)
    tkdestroy(tmp)

    ## Frame d'aide :
    FrameHelp <- tkframe(WinDistri)
    T.help <- tktext(FrameHelp, bg="#fae18d", font="arial", width=100,
                     height=4, relief="groove", borderwidth=2)


    ## Frame pour la loi Normale :
    FrameN <- tkframe(WinDistri, borderwidth=2, relief="groove")
    Img.N <- tkrplot(FrameN,            # Création de l'image.
                     fun=function()
                 {
                     plotDist.f(y = Data[ , variable], family="NO", variable=variable, env=env)
                 },
                     vscale=vscale, hscale=hscale, pointsize=pointsize)

    RB.N <- tkradiobutton(FrameN, variable=LoiChoisie, value="NO",   # bouton de sélection.
                          text=paste("loi Normale (AIC=", round(distList[["NO"]]$aic, 0), "). ", sep=""))


    ## Frame pour la loi log-Normale :
    FrameLogN <- tkframe(WinDistri, borderwidth=2, relief="groove")
    Img.LogN <- tkrplot(FrameLogN, fun=function()   # Création de l'image.
                    {
                        plotDist.f(y = Data[ , variable], family="LOGNO", variable=variable, env=env)
                    },
                        vscale=vscale, hscale=hscale, pointsize=pointsize)

    RB.LogN <- tkradiobutton(FrameLogN, variable=LoiChoisie, value="LOGNO",   # bouton de sélection.
                             text=paste("loi log-Normale (AIC=", round(distList[["LOGNO"]]$aic, 0), "). ", sep=""))

    if (is.integer(Data[ , variable]))
    {
        ## Frame pour la loi de Poisson :
        FramePois <- tkframe(WinDistri, borderwidth=2, relief="groove")
        Img.Pois <- tkrplot(FramePois,    # Création de l'image.
                            fun=function()
                        {
                            plotDist.f(y = Data[ , variable], family="PO", variable=variable, env=env)
                        },
                            vscale=vscale, hscale=hscale, pointsize=pointsize)

        RB.Pois <- tkradiobutton(FramePois, variable=LoiChoisie, value="PO",   # bouton de sélection.
                                 text=paste("loi de Poisson (AIC=", round(distList[["PO"]]$aic, 0), "). ", sep=""))

        ## Frame pour la loi bionomiale négative :
        FrameNBinom <- tkframe(WinDistri, borderwidth=2, relief="groove")

        Img.NBinom <- tkrplot(FrameNBinom,    # Création de l'image.
                              fun=function()
                          {
                              plotDist.f(y = Data[ , variable], family="NBI", variable=variable, env=env)
                          },
                              vscale=vscale, hscale=hscale, pointsize=pointsize)

        RB.NBinom <- tkradiobutton(FrameNBinom, variable=LoiChoisie, value="NBI",   # bouton de sélection.
                                   text=paste("loi Binomiale négative (AIC=",
                                              round(distList[["NBI"]]$aic, 0), "). ", sep=""))
    } else {}

    ## Boutons :
    FrameB <- tkframe(WinDistri)
    B.OK <- tkbutton(FrameB, text="     OK     ", command=function(){tclvalue(Done) <- "1"})
    B.Cancel <- tkbutton(FrameB, text="   Annuler   ", command=function(){tclvalue(Done) <- "2"})

    ## ##################################################
    ## Placement des éléments sur la grille :

    tkgrid(tklabel(WinDistri, text=" "))
    tkinsert(T.help, "end", paste("INFO :\n", # texte de l'aide.
                                  "Cette fenêtre vous permet de choisir la distribution",
                                  " la plus adaptée pour faire vos analyses.\n",
                                  "La distribution (courbe rouge) s'ajustant le mieux à vos données (histogramme) d'après \n",
                                  "le critère d'information de Akaike (AIC ; doit être le plus petit possible) est pré-sélectionnée.", sep=""))
    tkgrid(T.help)
    tkgrid(FrameHelp, column=1, columnspan=3)

    tkgrid(tklabel(WinDistri, text=" "))
    tkgrid(Img.N, columnspan=2)
    tkgrid(RB.N, row=1, sticky="e")
    tkgrid(tklabel(FrameN, text=" Modèle : ANOVA", fg="red"), row=1, column=1, sticky="w")
    tkgrid(Img.LogN, columnspan=2)
    tkgrid(RB.LogN, sticky="e")
    tkgrid(tklabel(FrameLogN, text=" Modèle : ANOVA, données log-transformées", fg="red"), row=1, column=1, sticky="w")
    tkgrid(tklabel(WinDistri, text=" "), FrameN, tklabel(WinDistri, text=" "), FrameLogN, tklabel(WinDistri, text=" "),
           sticky="ew")
    tkgrid(tklabel(WinDistri, text=" "))

    ## Évènements : sélections en cliquant sur les graphiques :
    tkbind(Img.N, "<Button-1>", function(){tclvalue(LoiChoisie) <- "NO"})
    tkbind(Img.LogN, "<Button-1>", function(){tclvalue(LoiChoisie) <- "LOGNO"})


    ## Pour les données entières seulement :
    if (is.integer(Data[ , variable])) {
    
        tkgrid(Img.Pois, columnspan=2)
        tkgrid(RB.Pois, sticky="e")
        tkgrid(tklabel(FramePois, text=" Modèle : GLM, famille 'Poisson'", fg="red"), row=1, column=1, sticky="w")
        tkgrid(Img.NBinom, columnspan=2)
        tkgrid(RB.NBinom, sticky="e")
        tkgrid(tklabel(FrameNBinom, text=" Modèle : GLM, famille 'Binomiale négative'", fg="red"), row=1, column=1, sticky="w")
        tkgrid(tklabel(WinDistri, text=" "), FramePois, tklabel(WinDistri, text=" "), FrameNBinom,
               tklabel(WinDistri, text=" "), sticky="ew")
        tkgrid(tklabel(WinDistri, text=" "))

        ## Évènements : sélections en cliquant sur les graphiques :
        tkbind(Img.Pois, "<Button-1>", function(){tclvalue(LoiChoisie) <- "PO"})
        tkbind(Img.NBinom, "<Button-1>", function(){tclvalue(LoiChoisie) <- "NBI"})
    } else {}

    ## Boutons :
    tkgrid(FrameB, column=1, columnspan=3)
    tkgrid(B.OK, tklabel(FrameB, text="                         "), B.Cancel)
    tkgrid(tklabel(WinDistri, text=" "))

    ## ##################################################
    ## Autres évènements :
    tkbind(WinDistri, "<Destroy>", function(){tclvalue(Done) <- "2"}) # en cas de destruction de la fenêtre.

    ## Présélection de la distribution avec le plus petit AIC :
    tclvalue(LoiChoisie) <- names(distList)[which.min(sapply(distList, function(x){x$aic}))]
    ## flush.console()

    tkwait.variable(Done)               # Attente d'une action de l'utilisateur.

    if (tclvalue(Done) == "1") {    
        return(tclvalue(LoiChoisie))
    } else {
        return(NULL)
    }

}


########################################################################################################################
resFileLM.f <- function(objLM, variable, listFact, Log=FALSE,  prefix=NULL, ext="txt", sufixe=NULL) {
    ## Purpose: Définit les noms du fichiers pour les résultats des modèles
    ##          linéaires. L'extension et un prefixe peuvent êtres précisés,
    ##          mais par défaut, c'est le fichier de sorties texte qui est
    ##          créé.
    ## ----------------------------------------------------------------------
    ## Arguments: objLM : un objet de classe 'lm' ou 'glm'
    ##            variable : nom de la métrique analysée
    ##            listFact : vecteur des noms de facteurs de l'analyse
    ##            Log : Est-ce que les données sont log-transformées
    ##            prefix : préfixe du nom de fichier
    ##            sufixe : un sufixe pour le nom de fichier
    ##            ext : extension du fichier
    ## ----------------------------------------------------------------------
    ## Author: Elodie Gamp, Date: janvier 2012

    ## si pas de préfix fourni :
    if (is.null(prefix)){
        prefix <- ifelse(length(grep("^lm\\(", deparse(objLM$call), perl=TRUE)) > 0,
                         paste("LM", ifelse(Log, "-log", ""), sep=""),
                         ifelse(length(grep("^glm\\.nb", deparse(objLM$call), perl=TRUE)) > 0,
                                "GLM-NB",
                                ifelse(length(grep("^glm.*poisson", deparse(objLM$call), perl=TRUE)) > 0,
                                       "GLM-P",
                                       "Unknown-model")))
    } else {}

    ## Nom de fichier :
    filename <- paste("C:/PAMPA/Resultats_Usages/stats/", prefix, "_",
                      ## Tableau de données :
#                      nomTable, "_",
                      ## Métrique analysée :
                      variable, "_",
                      ## liste des facteurs de l'analyse
                      paste(listFact, collapse="-"),
                      ## sufixe :
                      ifelse(is.null(sufixe), "", paste("_", sufixe, sep="")),
                      ## Extension du fichier :
                      ".", gsub("^\\.([^.]*)", "\\1", ext[1], perl=TRUE), # nettoyage de l'extension si besoin.
                      sep="")

    ## Ouverture de la connection (retourne l'objet de type 'connection') si pas un fichier avec extension graphique,
    ## retourne le nom de fichier sinon :
    if (!is.element(gsub("^\\.([^.]*)", "\\1", ext[1], perl=TRUE),
                    c("pdf", "PDF", "png", "PNG", "jpg", "JPG"))) {
        return(resFile <- file(filename, open="w"))
    } else {
        return(filename)
    }
}


########################################################################################################################
valPreditesLM.f <- function(objLM, Data, listFact, resFile){
    ## Purpose:
    ## ----------------------------------------------------------------------
    ## Arguments: objLM : objet de classe 'lm' ou 'glm'
    ##            Data : les données utilisées pour ajuster le modèle
    ##            listFact : un vecteur donnant la liste des noms de
    ##                       facteurs
    ##            resFile : la connection au fichier résultat
    ## ----------------------------------------------------------------------
    ## Author: Elodie Gamp, Date: janvier 2012


    ## ##################################################
    ## Valeurs prédites :
    OrdreNivFact <- sapply(unique(Data[ , listFact]), as.numeric)

    if (!is.matrix(OrdreNivFact)) {      # Si un seul facteur, on transforme le vecteur d'ordre des niveaux en matrice.
        OrdreNivFact <- matrix(OrdreNivFact, ncol=1, dimnames=list(NULL, listFact))
    } else {}

    ## Valeurs prédites pour chaque combinaison réalisée des facteurs :
    if (length(grep("^glm", objLM$call)) > 0) {    
        valPredites <- predict(objLM, newdata=unique(Data[ , listFact, drop=FALSE]), type="response")
    } else {
        valPredites <- predict(objLM, newdata=unique(Data[ , listFact, drop=FALSE]))
    }

    ## Noms des valeurs prédites (combinaisons des différents niveaux de facteurs) :
    nomCoefs <- unique(apply(Data[ , listFact, drop=FALSE], 1, paste, collapse=":"))
    names(valPredites) <- nomCoefs

    ## On remet les modalités en ordre :
    valPredites <- valPredites[eval(parse(text=paste("order(",
                                          paste("OrdreNivFact[ , ", 1:ncol(OrdreNivFact), "]", sep="", collapse=", "),
                                          ")", sep="")))]

    ## Écriture de l'en-tête :
    cat("\n\n\n---------------------------------------------------------------------------",
        "\nValeurs prédites par le modèle :\n\n",
        file=resFile)

    ## Écriture du résultat :
    capture.output(print(valPredites), file=resFile)
}


########################################################################################################################
infoStatLM.f <- function(objLM, resFile) {

    ## Purpose: Écrit les informations sur le modèle insi que les
    ##          statistiques globale dans un fichier résultat
    ## ----------------------------------------------------------------------
    ## Arguments: objLM un objet de classe 'lm' ou 'glm'
    ##            resFile : une connection pour les sorties
    ## ----------------------------------------------------------------------
    ## Author: Elodie Gamp, Date: janvier 2012

    ## [!!!] Attention, il arrive que les calculs bloquent ici lors du premier lancement (origine inconnue)
    sumLM <- switch(class(objLM)[1],
                    lm = summary.lm(objLM),
                    glm = summary.glm(objLM),
                    negbin = MASS:::summary.negbin(objLM),
                    summary(objLM))

    ## Informations sur le modèle :
    cat("Modèle ajusté :", file=resFile, fill=1)
    cat("\t", deparse(objLM$call), "\n\n\n", file=resFile, sep="")

    ## Stats globales :
    if (length(grep("^glm", objLM$call)) == 0) {
    
        cat("Statistique de Fisher Globale et R^2 :\n\n", file=resFile)
        cat("\tR^2 multiple : ", format(sumLM$r.squared, digits=3),
            " ;\tR^2 ajusté : ", format(sumLM$adj.r.squared, digits=3), "\n", file=resFile, sep="")

        cat("\tF-statistique : ",
            paste(sapply(sumLM$fstatistic, format, digits=4, nsmall=0),
                  c(" sur ", " et ", " DL,"), sep=""),
            "\tP-valeur : ",
            format.pval(pf(sumLM$fstatistic[1L], sumLM$fstatistic[2L], sumLM$fstatistic[3L], lower.tail = FALSE), digits=4),
            "\n\n\n", file=resFile, sep="")
    } else { }
}


########################################################################################################################
signifParamLM.f <- function(objLM, resFile)  {
    ## Purpose: Écrire les résultats de l'anova globale du modèle et
    ##          l'estimation de significativités des coefficients du modèle.
    ## ----------------------------------------------------------------------
    ## Arguments: objLM un objet de classe 'lm' ou 'glm'
    ##            resFile : une connection pour les sorties
    ## ----------------------------------------------------------------------
    ## Author: Elodie Gamp, Date: janvier 2012

    ## Anovas et résumés :
    if (length(grep("^glm", objLM$call)) > 0) {# Pour les GLMs.    
        anovaLM <- anova(objLM, test="Chisq") # Pour les LMs.
    } else {
        anovaLM <- anova(objLM)
    }
    sumLM <- summary(objLM)

    ## Anova globale du modèle :
    capture.output(print.anova.fr(anovaLM), file=resFile)

    ## Significativités des paramètres :
    cat("\n\nSignificativités des paramètres ",
        "\n(seuls ceux correspondant à des facteurs/intéractions significatifs sont représentés) :\n\n",
        file=resFile)

    capture.output(printCoefmat.red(sumLM$coef, anovaLM=anovaLM, objLM=objLM), file=resFile)
}


########################################################################################################################
sortiesLM.f <- function(objLM, formule, variable, listFact, Data, Log=FALSE, sufixe=NULL) {
    ## Purpose: Formater les résultats de lm et les écrire dans un fichier
    ## ----------------------------------------------------------------------
    ## Arguments: objLM : un objet de classe lm
    ##            formule : la formule utilisée (pas lisible dans le call)
    ##            variable : la métrique choisie
    ##            listFact : liste du (des) facteur(s) à tester
    ##            Data : les données utilisées
    ##            Log : données log-transformées ou non (booléen)
    ##            sufixe : un sufixe pour le nom de fichier
    ## ----------------------------------------------------------------------
    ## Author: Elodie Gamp, Date: janvier 2012

    ## longueur des lignes pour les sorties textes :
    oOpt <- options()
    on.exit(options(oOpt))

    options(width=120)

    ## Formule de modèle lisible:
    objLM$call$formula <- formule
    formule <<- formule

    ## Chemin et nom de fichier :
    resFile <- resFileLM.f(objLM=objLM, variable=variable, listFact=listFact,
                           Log=Log, sufixe=sufixe)
    on.exit(close(resFile), add=TRUE)


    ## Informations et statistiques globales sur le modèle :
    infoStatLM.f(objLM=objLM, resFile=resFile)


    ## Anova globale du modèle + significativité des coefficients :
    signifParamLM.f(objLM=objLM, resFile=resFile)


    ## ##################################################
    ## Valeurs prédites par le modèle :
    valPreditesLM.f(objLM=objLM, Data=Data, listFact=listFact, resFile=resFile)

    ## ##################################################
    # interaction plot si deux facteurs explicatifs
    if (length(listFact)==2){
         x11(width=50,height=30,pointsize=10)
#          par(mar=c(7, 6, 6, 2), mgp=c(4.5, 0.5, 0))

      cols <- nrow(unique(Data[listFact[2]]))
      if (Log) {                  # Les sens de variations peuvent changer en log (sur les moyennes) =>
                                        # besoin d'un graphique adapté :             
             with(Data, {
                 eval(parse(text=paste("interaction.plot(", listFact[1], ", ", listFact[2],", log(", variable, "), ylab=\"",
                                       paste("log(", variable, ") moyen", sep=""), "\",col=1:cols)", sep="")))
              })                         

              } else {
              
             with(Data, {
                 eval(parse(text=paste("interaction.plot(", listFact[1], ", ", listFact[2],", ", variable, ", ylab=\"",
                                       paste("nombre(", variable, ") moyen", sep=""), "\",col=1:cols)", sep="")))
              })
              mtext("Interaction plot", side=3, cex=2) 
             }
    }     

}

########################################################################################################################
