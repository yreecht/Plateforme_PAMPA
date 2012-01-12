#-*- coding: latin-1 -*-

### File: Initialisation.R
### Time-stamp: <2012-01-04 13:26:20 yreecht>
###
### Author: Yves Reecht
###
####################################################################################################
### Description:
###
### Script destiné à recevoir les initialisations précédemment faites dans config.r
####################################################################################################


## ## Vérification de l'existances de la configuration :
## assign(".requiredVar",
##        c(## "siteEtudie",
##          unitobs="fileNameUnitobs", obs="fileNameObs", refesp="fileNameRefesp", ws="nameWorkspace"),
##        envir=.GlobalEnv)

options(P.requiredVar=c(unitobs="fileNameUnitobs",
                        obs="fileNameObs",
                        refesp="fileNameRefesp",
                        ws="nameWorkspace"))

## ##################### Initialisation des variables globales ####################
## Jeuxdonnescoupe <- 0
## assign("Jeuxdonnescoupe", Jeuxdonnescoupe, envir=.GlobalEnv) # [!!!] Changer de system   [yr: 8/12/2011]

#### Logo :
.fileimage <- "./Scripts_Biodiv/img/pampa2.GIF"
assign(".fileimage", .fileimage, envir=.GlobalEnv)


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
                    fileNameUnitobs=get(requiredVar["unitobs"], envir=env),
                    fileNameObs=get(requiredVar["obs"], envir=env),
                    fileNameRefesp=get(requiredVar["esp"], envir=env))
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
