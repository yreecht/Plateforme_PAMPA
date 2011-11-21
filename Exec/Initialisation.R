#-*- coding: latin-1 -*-

### File: Initialisation.R
### Time-stamp: <2011-11-17 10:25:16 yreecht>
###
### Author: Yves Reecht
###
####################################################################################################
### Description:
###
### Script destiné à recevoir les initialisations précédemment faites dans config.r
####################################################################################################


pathMaker.f <- function(nameWorkspace, fileName1, fileName2, fileName3)
{
    ## Purpose: Redéfinir les chemins (par exemple après changement du
    ##          dossier de travail)
    ## ----------------------------------------------------------------------
    ## Arguments: aucun
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 28 sept. 2010, 10:05

    assign("nameWorkspace", nameWorkspace, envir=.GlobalEnv)
    assign("fileName1", fileName1, envir=.GlobalEnv)
    assign("fileName2", fileName2, envir=.GlobalEnv)
    assign("fileName3", fileName3, envir=.GlobalEnv)

    assign("NomDossierTravail", paste(nameWorkspace, "/FichiersSortie/", sep=""), envir=.GlobalEnv)
    assign("NomDossierData", paste(nameWorkspace, "/Data/", sep=""), envir=.GlobalEnv)   # sert a concaténer les
                                        # variables fileNameUnitObs fileNameObs   fileNameRefEsp fileNameRefSpa
    assign("fileNameUnitObs", paste(NomDossierData, fileName1, sep=""), envir=.GlobalEnv)
    assign("fileNameObs", paste(NomDossierData, fileName2, sep=""), envir=.GlobalEnv)
    assign("fileNameRefEsp", paste(NomDossierData, fileName3, sep=""), envir=.GlobalEnv)
    ## assign("fileNameRefSpa", paste(NomDossierData, fileNameRefSpa, sep=""), envir=.GlobalEnv)
}

## Vérification de l'existances de la configuration :
assign("requiredVar",
       c(## "siteEtudie",
         "fileName1", "fileName2", "fileName3", "nameWorkspace"),
       envir=.GlobalEnv)

##################### Initialisation des variables globales ####################
Jeuxdonnescoupe <- 0
assign("Jeuxdonnescoupe", Jeuxdonnescoupe, envir=.GlobalEnv)

#### Logo :
fileimage <- "./Exec/img/pampa2.GIF"
assign("fileimage", fileimage, envir=.GlobalEnv)


## ## variables d'environnement pour l'interface
## lang <- "FR"


########################################################################################################################
## Ajouts pour les graphs génériques [yr: 13/08/2010] :

## Noms d'usage des variables des principales tables de données
## (référentiels compris) :
init.GraphLang.f <- function()
{
    ## Purpose: Initialisation de la langue utilisée pour les noms de
    ## variables sur les graphiques.
    ## ----------------------------------------------------------------------
    ## Arguments: aucun (basé sur les options).
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 17 nov. 2011, 10:22

    assign("varNames",
           read.csv(paste(basePath, "/Exec/NomsVariables_",
                          tolower(getOption("P.lang")), ".csv",
                          sep=""),
                    header=TRUE, row.names=1, stringsAsFactors=FALSE),
           envir=.GlobalEnv)
}

## Remplacer "/Exec/NomsVariables_fr.csv" par "/Exec/NomsVariables_en.csv" pour des axes et noms de variables en
## anglais.
## Affecte uniquement les sorties !








### Local Variables:
### ispell-local-dictionary: "english"
### fill-column: 120
### End:
