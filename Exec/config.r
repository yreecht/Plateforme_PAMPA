## noms de fichiers utilisés par le chargement automatique

########################################################################################################################
## Zone éditable :

## #########################################################################################################
## Exemple : faites de même avec vos jeux de donnée (sans les commentaires -- "## " -- en début de ligne.) :

## #### RUN :
## siteEtudie <- "RUN"
## fileName1 <- "unitobspampaGCRMNpoisson-100810.txt"
## fileName2 <- "obspampaGCRMNpoisson-100810.txt"
## fileName3 <- "PAMPA-WP1-Meth-4-RefSpOM 110810.txt"
## nameWorkspace <- "C:/PAMPA"




     ######################################
     ### Copiez votre configuration ici ###
     ######################################





## Fin de la zone éditable
########################################################################################################################

existVar <- sapply(requiredVar, exists)

if (any(! existVar))                    # Si au moins une des variables n'est pas définie.
{
    pluriel <- sum(! existVar) > 1

    ## Demande pour l'ouverture du fichier de configuration :
    if(tclvalue(tkmessageBox(message=paste(ifelse(pluriel,
                                                  "Les variables suivantes ne sont pas définies ",
                                                  "La variable suivante n'est pas définie "),
                                           "dans votre fichier \"", basePath, "/Exec/config.r\" :\n\n\t*  ",
                                           paste(requiredVar[! existVar], collapse="\n\t*  "),
                                           "\n\nVoulez-vous éditer ce fichier ?",
                                           "\n\t(ouverture automatiquement de la sauvegarde également, si elle existe).",
                                           sep=""),
                             icon="warning", type="yesno", title="Configuration imcomplète",
                             default="no")) == "yes")
    {
        shell.exec(paste(basePath, "/Exec/config.r", sep=""))

        if (file.exists(fileTmp <- paste(basePath, "/Exec/config.bak.r", sep="")))
        {
            shell.exec(fileTmp)
        }else{}
    }else{}
}else{
    pathMaker.f(nameWorkspace=nameWorkspace,
                fileName1=fileName1,
                fileName2=fileName2,
                fileName3=fileName3)
}




