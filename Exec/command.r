RestaurerDonnees.f <- function ()
{
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
        Jeuxdonnescoupe <- 0
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
    runLog.f(msg=c("Sélection des enregistrement selon un critère du référentiel espèces :"))

    obs <- UnCritereEspDansObs.f()
    assign("obs", obs, envir=.GlobalEnv)
    creationTablesBase.f()
    creationTablesCalcul.f()
    ModifierInterfaceApresSelection.f(paste(factesp[1], ":", selectfactesp), dim(obs)[1])
    ## gestionMSGinfo.f("Critereselectionne", dim(obs)[1])
}

## ################################################################################
## Nom    : SelectionUnCritereUnitobs.f()
## Objet  : exécution de la sélection par critere, choix de la valeur de sélection
## et écrasement des données dans "obs"
## ################################################################################

SelectionUnCritereUnitobs.f <- function ()
{
    runLog.f(msg=c("Sélection des enregistrement selon un critère du référentiel des unités d'observation :"))

    obs <- UnCritereUnitobsDansObs.f()
    assign("obs", obs, envir=.GlobalEnv)
    creationTablesBase.f()
    creationTablesCalcul.f()
    ModifierInterfaceApresSelection.f(paste(fact[1], ":", selectfactunitobs), dim(obs)[1])
    ## gestionMSGinfo.f("Critereselectionne", dim(obs)[1])
}

