RestaurerDonnees.f <- function ()
{
    on.exit(winRaise.f(tm))

    runLog.f(msg=c("Restauration des données originales :"))

    if (Jeuxdonnescoupe==1)
    {
        assign("obs", SAUVobs, envir=.GlobalEnv)
        assign("unitobs", SAUVunitobs, envir=.GlobalEnv)
        if (exists("SAUVcontingence", envir=.GlobalEnv))
        {
            assign("contingence", SAUVcontingence, envir=.GlobalEnv)
        }else{}
        assign("unitesp", SAUVunitesp, envir=.GlobalEnv)
        assign("unit", SAUVunit, envir=.GlobalEnv)
        assign("listespunit", SAUVlistespunit, envir=.GlobalEnv)
        assign("TableBiodiv", SAUVTableBiodiv, envir=.GlobalEnv)
        assign("TableMetrique", SAUVTableMetrique, envir=.GlobalEnv)

        if (!is.benthos.f())               # unique(unitobs$type) != "LIT"
        {  # car pas de classes de tailles avec les recouvrements
            assign("unitespta", SAUVunitespta, envir=.GlobalEnv)
        }

        message("données sauvées réinitialisées dans les tables de base")
        ModifierInterfaceApresRestore.f("Tout", "Toutes")
        assign("Jeuxdonnescoupe", 0, envir=.GlobalEnv)

        add.logFrame.f(msgID="restauration", env = .GlobalEnv)

        tkmessageBox(message=paste("Données originales restaurée."## , dim(obs)[1],
                                   ## "enregistrements dans la table des observations"
                     ))

        gestionMSGaide.f("SelectionOuTraitement")
    }
}


## ################################################################################
## Nom    : SelectionUnCritereEsp.f()
## Objet  : exécution de la sélection par critere, choix de la valeur de sélection
## et écrasement des données dans "obs"
## ################################################################################

SelectionUnCritereEsp.f <- function ()
{
    on.exit(winRaise.f(tm))

    runLog.f(msg=c("Sélection des enregistrement selon un critère du référentiel espèces :"))

    selection <- UnCritereEspDansObs.f()

    infoGeneral.f(msg="Sélection et recalcul selon un critère du référentiel espèces :",
                  font=tkfont.create(weight="bold", size=9), foreground="darkred")

    if (!is.null(selection))
    {
        assign("obs", obs <- selection[["obs"]], envir=.GlobalEnv)

        keptEspeces <- as.character(especes$code_espece[is.element(especes[ , selection[["facteur"]]],
                                                                   selection[["selection"]])])

        ## Réduction des tables de données (au espèces sélectionnées) :
        if (exists("unitespta", envir=.GlobalEnv) && ncol(unitespta))
        {
            assign("unitespta",
                   dropLevels.f(unitespta[is.element(unitespta$code_espece, keptEspeces), , drop=FALSE],
                                which="code_espece"),
                   envir=.GlobalEnv)

            ## #### Écriture des fichiers :
            tmp1 <- unitespta

            ## Certaines métriques (densités) sont ramenées à /100m² :
            if (any(is.element(colnames(tmp1),
                               colTmp <- c("densite", "densiteMax", "densiteSD",
                                           "biomasse", "biomassMax", "biomasseSD"))))
            {
                tmp1[ , is.element(colnames(tmp1),
                                   colTmp)] <- sweep(tmp1[ , is.element(colnames(tmp1),
                                                                        colTmp),
                                                          drop=FALSE],
                                                     2, 100, "*")
            }else{}

            ## Sauvegarde dans un fichier :
            write.csv2(merge(merge(tmp1,
                                   unitobs[ , c("unite_observation", paste("habitat", 1:3, sep=""))],
                                   by=c("unite_observation")),
                             especes[ , c("code_espece", "Famille", "Genre", "espece")],
                             by="code_espece"),
                       file=paste(nameWorkspace,
                                  "/FichiersSortie/UnitobsEspeceClassetailleMetriques",
                                  ifelse(Jeuxdonnescoupe, "_selection", ""),
                                  ".csv", sep=""),
                       row.names = FALSE)
             ## ####
        }else{}

        assign("unitesp",
               dropLevels.f(unitesp[is.element(unitesp$code_espece, keptEspeces), , drop=FALSE],
                            which="code_espece"),
               envir=.GlobalEnv)

        assign("listespunit",
               dropLevels.f(listespunit[is.element(listespunit$code_espece, keptEspeces), , drop=FALSE],
                            which="code_espece"),
               envir=.GlobalEnv)

        ## #### Écriture des fichiers :
        ## Certaines métriques (densités) sont ramenées à /100m² pour les sorties fichier :
        tmp2 <- unitesp

        if (any(is.element(colnames(tmp2),
                           colTmp <- c("densite", "densiteMax", "densiteSD",
                                       "biomasse", "biomassMax", "biomasseSD"))))
        {
            tmp2[ , is.element(colnames(tmp2),
                               colTmp)] <- sweep(tmp2[ , is.element(colnames(tmp2),
                                                                    colTmp),
                                                      drop=FALSE],
                                                 2, 100, "*")
        }else{}

        ## Sauvegarde dans un fichier :
        write.csv2(merge(merge(tmp2,
                               unitobs[ , c("unite_observation", paste("habitat", 1:3, sep=""))],
                               by=c("unite_observation")),
                         especes[ , c("code_espece", "Famille", "Genre", "espece")],
                         by="code_espece"),
                   file=paste(NomDossierTravail, "UnitobsEspeceMetriques",
                              ifelse(Jeuxdonnescoupe, "_selection", ""),
                              ".csv", sep=""), row.names = FALSE)
        ## ####

        if (exists("contingence", envir=.GlobalEnv, frame, mode="any", inherits=TRUE))
        {
            assign("contingence",
                   contingence[ , is.element(colnames(contingence), keptEspeces), drop=FALSE],
                   envir=.GlobalEnv)

            write.csv2(contingence,
                       file=paste(NomDossierTravail, "ContingenceUnitObsEspeces",
                                  ifelse(Jeuxdonnescoupe, "_selection", ""),
                                  ".csv", sep=""))
        }

        ## Plan d'échantillonnage basic :
        PlanEchantillonnageBasic.f(tabUnitobs=unitobs, tabObs=obs)

        ## Recalcul des indices de biodiversité :
        unit.f()

        ## Information de l'utilisateur :
        infoLoading.f(msg=paste("Les métriques ont été",
                      " recalculées sur le jeu de données sélectionné.",
                      sep=""),
                      icon="info",
                      font=tkfont.create(weight="bold", size=9))


        ## Recréation des tables de calcul :
        creationTablesCalcul.f()
        ModifierInterfaceApresSelection.f(paste(selection[["facteur"]], ":",
                                                paste(selection[["selection"]], collapse=", ")), dim(obs)[1])

        ## Ajout d'info dans le log de sélection :
        add.logFrame.f(msgID="selection", env = .GlobalEnv,
                       facteur=selection[["facteur"]], selection=selection[["selection"]],
                       workSpace=nameWorkspace, referentiel="especes")


        infoLoading.f(button=TRUE)
    }else{
        infoLoading.f(msg="Abandon !")
        infoLoading.f(button=TRUE)
    }
}

## ################################################################################
## Nom    : SelectionUnCritereUnitobs.f()
## Objet  : exécution de la sélection par critere, choix de la valeur de sélection
## et écrasement des données dans "obs"
## ################################################################################

SelectionUnCritereUnitobs.f <- function ()
{
    on.exit(winRaise.f(tm))

    runLog.f(msg=c("Sélection des enregistrement selon un critère du référentiel des unités d'observation :"))

    selection <- UnCritereUnitobsDansObs.f()

    infoGeneral.f(msg="Sélection et recalcul selon un critère du référentiel d'unités d'observation :",
                  font=tkfont.create(weight="bold", size=9), foreground="darkred")

    if (!is.null(selection))
    {
        assign("obs", selection[["obs"]], envir=.GlobalEnv)

        keptUnitobs <- as.character(unitobs$unite_observation[is.element(unitobs[ , selection[["facteur"]]],
                                                                         selection[["selection"]])])

        ## Réduction des tables de données (au espèces sélectionnées) :
        if (exists("unitespta", envir=.GlobalEnv) && ncol(unitespta))
        {
            assign("unitespta",
                   dropLevels.f(unitespta[is.element(unitespta$unite_observation,
                                                     keptUnitobs),
                                          , drop=FALSE],
                                which="unite_observation"),
                   envir=.GlobalEnv)

            ## #### Écriture des fichiers :
            tmp1 <- unitespta

            ## Certaines métriques (densités) sont ramenées à /100m² :
            if (any(is.element(colnames(tmp1),
                               colTmp <- c("densite", "densiteMax", "densiteSD",
                                           "biomasse", "biomassMax", "biomasseSD"))))
            {
                tmp1[ , is.element(colnames(tmp1),
                                   colTmp)] <- sweep(tmp1[ , is.element(colnames(tmp1),
                                                                        colTmp),
                                                          drop=FALSE],
                                                     2, 100, "*")
            }else{}

            ## Sauvegarde dans un fichier :
            write.csv2(merge(merge(tmp1,
                                   unitobs[ , c("unite_observation", paste("habitat", 1:3, sep=""))],
                                   by=c("unite_observation")),
                             especes[ , c("code_espece", "Famille", "Genre", "espece")],
                             by="code_espece"),
                       file=paste(nameWorkspace,
                                  "/FichiersSortie/UnitobsEspeceClassetailleMetriques",
                                  ifelse(Jeuxdonnescoupe, "_selection", ""),
                                  ".csv", sep=""),
                       row.names = FALSE)
             ## ####
        }else{}

        assign("unitesp",
               dropLevels.f(unitesp[is.element(unitesp$unite_observation,
                                               keptUnitobs),
                                    , drop=FALSE],
                            which="unite_observation"),
               envir=.GlobalEnv)

        assign("listespunit",
               dropLevels.f(listespunit[is.element(listespunit$unite_observation,
                                                   keptUnitobs),
                                        , drop=FALSE],
                            which="unite_observation"),
               envir=.GlobalEnv)

        ## #### Écriture des fichiers :
        tmp2 <- unitesp

        ## Certaines métriques (densités) sont ramenées à /100m² pour les sorties fichier :
        if (any(is.element(colnames(tmp2),
                           colTmp <- c("densite", "densiteMax", "densiteSD",
                                       "biomasse", "biomassMax", "biomasseSD"))))
        {
            tmp2[ , is.element(colnames(tmp2),
                               colTmp)] <- sweep(tmp2[ , is.element(colnames(tmp2),
                                                                    colTmp),
                                                      drop=FALSE],
                                                 2, 100, "*")
        }else{}

        ## Sauvegarde dans un fichier :
        write.csv2(merge(merge(tmp2,
                               unitobs[ , c("unite_observation", paste("habitat", 1:3, sep=""))],
                               by=c("unite_observation")),
                         especes[ , c("code_espece", "Famille", "Genre", "espece")],
                         by="code_espece"),
                   file=paste(NomDossierTravail, "UnitobsEspeceMetriques",
                              ifelse(Jeuxdonnescoupe, "_selection", ""),
                              ".csv", sep=""), row.names = FALSE)
        ## ####

        assign("unit",
               dropLevels.f(unit[is.element(unit$unitobs,
                                            keptUnitobs),
                                 , drop=FALSE],
                            which="unitobs"),
               envir=.GlobalEnv)

        ## #### Écriture des fichiers :
        ## Certaines métriques (densités) sont ramenées à /100m² pour les sorties fichier :
        tmp3 <- unit

        ## Certaines métriques (densités) sont ramenées à /100m² :
        if (any(is.element(colnames(tmp3),
                           colTmp <- c("densite", "densiteMax", "densiteSD",
                                       "biomasse", "biomassMax", "biomasseSD"))))
        {
            tmp3[ , is.element(colnames(tmp3),
                               colTmp)] <- sweep(tmp3[ , is.element(colnames(tmp3),
                                                                    colTmp),
                                                      drop=FALSE],
                                           2, 100, "*")
        }else{}

        ## Sauvegarde dans un fichier :
        write.csv2(merge(tmp3,
                         unitobs[ , c("unite_observation", paste("habitat", 1:3, sep=""))],
                         by.x="unitobs", by.y=c("unite_observation")),
                   file=paste(NomDossierTravail, "UnitobsMetriques",
                              ifelse(Jeuxdonnescoupe, "_selection", ""),
                              ".csv", sep=""), row.names = FALSE)
        ## ####

        if (exists("contingence", envir=.GlobalEnv, frame, mode="any", inherits=TRUE))
        {
            assign("contingence",
                   contingence[is.element(row.names(contingence),
                                          keptUnitobs),
                               , drop=FALSE],
                   envir=.GlobalEnv)

            write.csv2(contingence,
                       file=paste(NomDossierTravail, "ContingenceUnitObsEspeces",
                                  ifelse(Jeuxdonnescoupe, "_selection", ""),
                                  ".csv", sep=""))
        }

        ## Plan d'échantillonnage basic :
        PlanEchantillonnageBasic.f(tabUnitobs=unitobs, tabObs=obs)

        ## Information de l'utilisateur :
        infoLoading.f(msg=paste("Les métriques ont été",
                      " recalculées sur le jeu de données sélectionné.",
                      sep=""),
                      icon="info",
                      font=tkfont.create(weight="bold", size=9))

        ## Recréation des tables de calcul :
        creationTablesCalcul.f()

        ModifierInterfaceApresSelection.f(paste(selection[["facteur"]], ":",
                                                paste(selection[["selection"]], collapse=", ")), dim(obs)[1])

        ## Ajout d'info dans le log de sélection :
        add.logFrame.f(msgID="selection", env = .GlobalEnv,
                       facteur=selection[["facteur"]], selection=selection[["selection"]],
                       workSpace=nameWorkspace, referentiel="unitobs")

        infoLoading.f(button=TRUE)
    }else{
        infoLoading.f(msg="Abandon !")
        infoLoading.f(button=TRUE)
    }
}

