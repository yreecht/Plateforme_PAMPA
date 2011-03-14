RestaurerDonnees.f <- function ()
{
    on.exit(winRaise.f(tm))

    runLog.f(msg=c("Restauration des données originales :"))

    if (Jeuxdonnescoupe==1)
    {
        assign("obs", SAUVobs, envir=.GlobalEnv)
        assign("unitobs", SAUVunitobs, envir=.GlobalEnv)
        assign("contingence", SAUVcontingence, envir=.GlobalEnv)
        assign("unitesp", SAUVunitesp, envir=.GlobalEnv)
        assign("unit", SAUVunit, envir=.GlobalEnv)
        assign("listespunit", SAUVlistespunit, envir=.GlobalEnv)
        assign("TableBiodiv", SAUVTableBiodiv, envir=.GlobalEnv)
        assign("TableMetrique", SAUVTableMetrique, envir=.GlobalEnv)

        if (!is.benthos.f())               # unique(unitobs$type) != "LIT"
        {  # car pas de classes de tailles avec les recouvrements
            ## unitespta <- SAUVunitespta
            assign("unitespta", SAUVunitespta, envir=.GlobalEnv)
        }

        message("données sauvées réinitialisées dans les tables de base")
        ModifierInterfaceApresRestore.f("Aucun", "Aucune")
        assign("Jeuxdonnescoupe", 0, envir=.GlobalEnv)

        gestionMSGinfo.f("Jeuxdedonnerestore", dim(obs)[1])
        tkmessageBox(message=paste("Jeu de données restauré \n", dim(obs)[1],
                                   "enregistrements dans la table observation"))
        message("Jeu de données restauré")
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
    assign("obs", obs <- selection[["obs"]], envir=.GlobalEnv)

    infoGeneral.f(msg="Sélection et recalcul selon un critère du référentiel espèces :",
                  font=tkfont.create(weight="bold", size=9), foreground="darkred")

    keptEspeces <- as.character(especes$code_espece[is.element(especes[ , selection[["facteur"]]],
                                                                     selection[["selection"]])])

    ## Réduction des tables de données (au espèces sélectionnées) :
    if (exists("unitespta", envir=.GlobalEnv))
    {
        assign("unitespta",
               dropLevels.f(unitespta[is.element(unitespta$code_espece, keptEspeces), , drop=FALSE],
                            which="code_espece"),
               envir=.GlobalEnv)
    }else{}

    assign("unitesp",
           dropLevels.f(unitesp[is.element(unitesp$code_espece, keptEspeces), , drop=FALSE],
                        which="code_espece"),
           envir=.GlobalEnv)

    assign("listespunit",
           dropLevels.f(listespunit[is.element(listespunit$code_espece, keptEspeces), , drop=FALSE],
                        which="code_espece"),
           envir=.GlobalEnv)

    assign("contingence",
           contingence[ , is.element(colnames(contingence), keptEspeces), drop=FALSE],
           envir=.GlobalEnv)

    ## Recalcul des indices de biodiversité :
    unit.f()

    ## Information de l'utilisateur :
    infoLoading.f(msg=paste("Les métriques ont été",
                  " recalculées sur le jeu de données sélectionné.",
                  sep=""),
                  icon="info",
                  font=tkfont.create(weight="bold", size=9))

    gestionMSGinfo.f("CalculSelectionFait")

    ## Recréation des tables de calcul :
    ## creationTablesBase.f()
    creationTablesCalcul.f()
    ModifierInterfaceApresSelection.f(paste(factesp[1], ":", paste(selectfactesp, collapse=", ")), dim(obs)[1])
    ## gestionMSGinfo.f("Critereselectionne", dim(obs)[1])

    infoLoading.f(button=TRUE)
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
    assign("obs", selection[["obs"]], envir=.GlobalEnv)

    infoGeneral.f(msg="Sélection et recalcul selon un critère du référentiel d'unités d'observation :",
                  font=tkfont.create(weight="bold", size=9), foreground="darkred")

    keptUnitobs <- as.character(unitobs$unite_observation[is.element(unitobs[ , selection[["facteur"]]],
                                                                     selection[["selection"]])])

    ## Réduction des tables de données (au espèces sélectionnées) :
    if (exists("unitespta", envir=.GlobalEnv))
    {
        assign("unitespta",
               dropLevels.f(unitespta[is.element(unitespta$unite_observation,
                                                 keptUnitobs),
                                      , drop=FALSE],
                            which="unite_observation"),
               envir=.GlobalEnv)
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

    assign("unit",
           dropLevels.f(unit[is.element(unit$unitobs,
                                        keptUnitobs),
                             , drop=FALSE],
                        which="unitobs"),
           envir=.GlobalEnv)

    assign("contingence",
           contingence[is.element(row.names(contingence),
                                  keptUnitobs),
                       , drop=FALSE],
           envir=.GlobalEnv)

    ## Information de l'utilisateur :
    infoLoading.f(msg=paste("Les métriques ont été",
                  " recalculées sur le jeu de données sélectionné.",
                  sep=""),
                  icon="info",
                  font=tkfont.create(weight="bold", size=9))

    gestionMSGinfo.f("CalculSelectionFait")

    ## Recréation des tables de calcul :
    ## creationTablesBase.f()
    creationTablesCalcul.f()
    ModifierInterfaceApresSelection.f(paste(fact[1], ":", paste(selectfactunitobs, collapse=", ")), dim(obs)[1])
    ## gestionMSGinfo.f("Critereselectionne", dim(obs)[1])

    infoLoading.f(button=TRUE)
}

