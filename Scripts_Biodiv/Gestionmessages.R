## le fichier gestionmessages.r a besoin du fichier Config.R pour être executé
## les passages à la ligne se font à la fin des messages
## on retourne encore à la ligne avant une erreur

gestionMSGerreur.f <- function (nameerror, variable, env=.GlobalEnv)
{
    runLog.f(msg=c("Envoie d'un message d'erreur dans l'interface :"))

    ## Message :
    MSG <-
        switch(nameerror,
               "recouvrementsansLIT"={
                   paste("Le champ obs$type n'est pas 'LIT', vous ne pouvez pas calculer",
                         " un % de recouvrement avec des espèces non benthiques\n", sep="")
               },
               "noWorkspace"={
                   "Aucun espace de travail n'est choisi ou opérationnel\n"
               },
               "nbChampUnitobs"={
                   paste("Votre fichier 'Unites d'observation' ne comporte pas le bon nombre de champs!",
                         " Il devrait en contenir 35. Corrigez le et recommencez l'importation.\n", sep="")
               },
               "nbChampUnitobs"={
                   paste("Votre fichier 'Unites d'observation' ne comporte pas le bon nombre de champs!",
                         " Il devrait en contenir 35. Corrigez le et recommencez l'importation.\n", sep="")
               },
               "nbChampEsp"={
                   paste("Votre fichier 'référentiel espèces' ne comporte pas le bon nombre de champs!",
                         " Il devrait en contenir 124. Corrigez le et recommencez l'importation.\n", sep="")
               },
               "nbChampObs"={
                   paste("Votre fichier 'observations' ne comporte pas le bon nombre de champs!",
                         " Il devrait en contenir 11 . Corrigez le et recommencez l'importation.\n", sep="")
               },
               "UnitobsDsObsUnitobs"={
                   paste("Votre fichier 'observations' ne comporte pas les mêmes unitobs que votre fichier",
                         " 'Unites d'observation'. Corrigez les et recommencez l'importation.\n", sep="")
               },
               "CaractereInterditDsObs"={
                   "Votre fichier observations contient des \"&\". Corrigez les et recommencez l'importation.\n"
               },
               "CaractereInterditDsUnitObs"={
                   "Votre fichier unités d'observations contient des \".\" Corrigez les et recommencez l'importation.\n"
               },
               "ZeroEnregistrement"={
                   "Graphique impossible - pas d'enregistrements dans votre sélection.\n"
               },
               "UneSeuleValeurRegroupement"={
                   "Graphique sans intérêt - votre critère de regroupement n'a qu'une seule valeur.\n"
               },
               "CritereMalRenseigne50"={
                   paste("Graphique sans intérêt - le critère de regroupement sélectionné",
                         " est renseigné pour moins de 50% des observations.\n", sep="")
               },
               "CaractereInterdit"={
                   paste("Votre table ", variable,
                         " contient des caractères déconseillés par le référentiel des données ('espaces',',',';' ",
                         "\n-> vous devez corriger le problème pour ne pas rencontrer d'erreurs.\n", sep="")
               },
               ## Message par défaut :
               "message à renseigner")

    ## gestionMSGerreur.f(nbChampUnitobs)
    ## langue = EN

    tkinsert(get("helpframe", envir=env), "end", paste("\nERREUR : ", MSG, sep=""))
    tkyview.moveto(get("helpframe", envir=env), 1)
}

gestionMSGaide.f <- function (namemsg, env=.GlobalEnv)
{
    runLog.f(msg=c("Envoie d'un message d'aide dans l'interface :"))

    ## Message :
    MSG <-
        switch(namemsg,
               "ZeroEnregistrement"={
                   paste("Attention : votre sélection ne contient plus d'enregistrement",
                         ", veuillez restaurer ou recharger les données (CTRL + a) !\n", sep="")
               },
               "etapeImport"={
                   paste("  * charger les \"Dossiers et fichiers par défaut\" (CTRL + a).",
                         "\n  * choisir les dossier/fichiers un à un (CTRL + n).",
                         "\n\t(actions également accessibles par le menu \"Données\")",
                         sep="")
               },
               "SelectionOuTraitement"={
                   paste("  * restreindre votre sélection de données (menu \"Sélection et recalcul\").\n",
                         "  * commencer les traitements standards (graphiques & analyses statistiques).",
                         "\n", sep="")
               },
               "startsansficher"={
                   paste("Si les fichiers par défauts paramétrés dans 'Config.R'- ", fileNameUnitobs, " - ", fileNameObs, " - ",
                         fileNameRefesp, " ne sont pas les bons, Veuillez les modifier\n", sep="")
               },
               "etapeselected"={
                   paste("  * restaurer les données originales (sans sélection) :",
                         " menu \"Sélection et recalcul\" ou bouton ci-dessous à droite.",
                         "\n  * faire d'autres sélections (imbriquées avec les sélections courantes).",
                         "\n  * commencer les traitements standards (graphiques & analyses statistiques).",
                         sep="")
               },
               "message à définir")

    tkinsert(get("helpframe", envir=env), "end", paste("\n", MSG, "", sep=""))
    tkyview.moveto(get("helpframe", envir=env), 1)
}

########################################################################################################################
add.logFrame.f <- function(msgID, env=dataEnv,...)
{
    ## Purpose: Ajout de messages dans le cadre d'info sur les chargements
    ##          et sélections.
    ## ----------------------------------------------------------------------
    ## Arguments: msgID : identifiant du type de message.
    ##            env : environnement où est définit le cadre d'information
    ##                  (interface).
    ##            ... : arguments supplémentaires (dont l'existence est
    ##                  testée en fonction du type de message choisi.)
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date:  9 nov. 2011, 16:01

    ## On récupère les arguments supplémentaires sous une forme facilement utilisable (list) :
    argsSup <- list(...)

    ## Traitement des différents cas de message :
    msg <- switch(msgID,
                  "dataLoadingNew"={
                      if (any(!is.element("filePathes",
                                          names(argsSup))))
                      {
                          stop("Arguments incorrects !")
                      }else{
                          paste("",
                                paste(rep("=", 100), collapse=""),
                                paste("\nChargement de données  (",
                                      format(Sys.time(), "%d/%m/%Y\t%H:%M:%S"),
                                      ")", sep=""),

                                paste("\n   Fichier d'observation :", argsSup$filePathes["obs"]),
                                paste("\n   Fichier d'unités d'observation :", argsSup$filePathes["unitobs"]),
                                paste("\n   Fichier du référentiel espèces :", argsSup$filePathes["refesp"]),
                                ifelse(is.na(argsSup$filePathes["refspa"]), "",
                                       paste("\n   Fichier du référentiel spatial :", argsSup$filePathes["refspa"])),
                                paste("\n   Répertoire des résultats et des exports :", argsSup$filePathes["results"]),
                                "\n", sep="")
                      }
                  },
                  "restauration"={
                      paste("",
                            paste(rep("-", 80), collapse=""),
                            paste("Restauration des données originales  (sans sélection ; ",
                                  format(Sys.time(), "%d/%m/%Y\t%H:%M:%S"),
                                  ")", sep=""),
                            "\n", sep="\n")
                  },
                  "selection"={
                      if (any(!is.element(c("facteur", "selection", "results", "referentiel", "has.SzCl"),
                                          names(argsSup))))
                      {
                          stop("Arguments incorrects !")
                      }else{
                          paste("\n",
                                paste(rep("-", 100), collapse=""),
                                paste("\nSélection des observations selon un critère",
                                      ifelse(argsSup$referentiel == "especes",
                                             " du référentiel espèces",
                                             " des unités d'observation"),
                                      " (",
                                      format(Sys.time(), "%d/%m/%Y\t%H:%M:%S"),
                                      ")", sep=""),
                                paste("\n\n   Facteur :", argsSup$facteur),
                                paste("\n   Modalités :",
                                      paste(argsSup$selection, collapse=", ")),
                                paste("\n\nFichiers exportés dans ", argsSup$results, " :", sep=""),
                                ifelse(isTRUE(argsSup$has.SzCl),
                                       paste("\n   - métriques par unité d'observation / esp. / cl. de taille :",
                                             "UnitobsEspeceClassetailleMetriques_selection.csv"),
                                       ""),
                                paste("\n   - métriques par unité d'observation / esp. :",
                                      "UnitobsEspeceMetriques_selection.csv"),
                                paste("\n   - métriques par unité d'observation :",
                                      "UnitobsMetriques_selection.csv"),
                                paste("\n   - plan d'échantillonnage basique (année - statut de protection) :",
                                      "PlanEchantillonnage_basique_selection.csv"),
                                "\n\n", sep="")
                      }
                  },
                  "fichiers"={
                      if (any(!is.element(c("results", "has.SzCl"),
                                          names(argsSup))))
                      {
                          stop("Arguments incorrects !")
                      }else{
                          paste("\n",
                                paste(rep("-", 100), collapse=""),
                                paste("\nFichiers exportés dans ", argsSup$results, sep=""),
                                paste("\n   (avant ", format(Sys.time(), "%d/%m/%Y\t%H:%M:%S"), ") :", sep=""),
                                ifelse(isTRUE(argsSup$has.SzCl),
                                       paste("\n   - métriques par unité d'observation / esp. / cl. de taille :",
                                             "UnitobsEspeceClassetailleMetriques.csv"),
                                       ""),
                                paste("\n   - métriques par unité d'observation / esp. :",
                                      "UnitobsEspeceMetriques.csv"),
                                paste("\n   - métriques par unité d'observation :",
                                      "UnitobsMetriques.csv"),
                                paste("\n   - plan d'échantillonnage basique (année - statut de protection) :",
                                      "PlanEchantillonnage_basique.csv"),
                                "\n\n", sep="")
                      }
                  },
                  "InfoRefSpeEnregistre"={
                      if (any(!is.element(c("file"),
                                          names(argsSup))))
                      {
                          stop("Arguments incorrects !")
                      }else{
                          paste("",
                                paste(rep("-", 100), collapse=""),
                                "Enregistrement des informations sur le référentiel espèce dans le fichier :",
                                paste("   ", argsSup$file, format(Sys.time(), "  (%d/%m/%Y\t%H:%M:%S)"), sep=""),
                                "\n", sep="\n")
                      }
                  },
                  "")

    ## Ajout du message :
    tkinsert(get("txt.w", envir=env), "end", msg)
    tkyview.moveto(get("txt.w", envir=env), 1)
}




