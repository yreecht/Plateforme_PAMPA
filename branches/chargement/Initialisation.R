#-*- coding: latin-1 -*-

### File: Initialisation.R
### Time-stamp: <2011-11-24 17:41:23 yreecht>
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
         unitobs="fileName1", obs="fileName2", esp="fileName3", ws="nameWorkspace"),
       envir=.GlobalEnv)

##################### Initialisation des variables globales ####################
Jeuxdonnescoupe <- 0
assign("Jeuxdonnescoupe", Jeuxdonnescoupe, envir=.GlobalEnv)

#### Logo :
fileimage <- "./Scripts_Biodiv/img/pampa2.GIF"
assign("fileimage", fileimage, envir=.GlobalEnv)


########################################################################################################################
testVar.f <- function(requiredVar, env=.GlobalEnv)
{
    ## Purpose: Test l'existence des variables requises (noms de fichiers)
    ##          et crée les chemins le cas échéant.
    ## ----------------------------------------------------------------------
    ## Arguments:
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 21 nov. 2011, 15:39

    existVar <- sapply(requiredVar, exists, envir=env)

    if (any(! existVar))                    # Si au moins une des variables n'est pas définie.
    {
        pluriel <- sum(! existVar) > 1

        ## Demande pour l'ouverture du fichier de configuration :
        if(tclvalue(tkmessageBox(message=paste(ifelse(pluriel,
                                                      "Les variables suivantes ne sont pas définies ",
                                                      "La variable suivante n'est pas définie "),
                                               "dans votre fichier \"", basePath, "/Scripts_Biodiv/config.r\" :\n\n\t*  ",
                                               paste(requiredVar[! existVar], collapse="\n\t*  "),
                                               "\n\nVoulez-vous éditer ce fichier ?",
                                               "\n\t(ouverture automatiquement de la sauvegarde également, si elle existe).",
                                               sep=""),
                                 icon="warning", type="yesno", title="Configuration imcomplète",
                                 default="no")) == "yes")
        {
            shell.exec(paste(basePath, "/Scripts_Biodiv/config.r", sep=""))

            if (file.exists(fileTmp <- paste(basePath, "/Scripts_Biodiv/config.bak.r", sep="")))
            {
                shell.exec(fileTmp)
            }else{}
        }else{}
    }else{
        pathMaker.f(nameWorkspace=get(requiredVar["ws"], envir=env),
                    fileName1=get(requiredVar["unitobs"], envir=env),
                    fileName2=get(requiredVar["obs"], envir=env),
                    fileName3=get(requiredVar["esp"], envir=env))
    }
}

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
           read.csv(paste(basePath, "/Scripts_Biodiv/NomsVariables_",
                          tolower(getOption("P.lang")), ".csv",
                          sep=""),
                    header=TRUE, row.names=1, stringsAsFactors=FALSE),
           envir=.GlobalEnv)
}

## Remplacer "/Scripts_Biodiv/NomsVariables_fr.csv" par "/Scripts_Biodiv/NomsVariables_en.csv" pour des axes et noms de variables en
## anglais.
## Affecte uniquement les sorties !








### Local Variables:
### ispell-local-dictionary: "english"
### fill-column: 120
### End:
