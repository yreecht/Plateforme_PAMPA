## noms de fichiers utilisés par le chargement automatique


## fileName1 <- "unitobsBAPoissons.txt"
## fileName2 <- "obsBAPoissons.txt"
## fileName3 <- "refEspecesMED.txt"
## fileName1 <- "TableUnitesObservationCR2007-2008-2009.txt"
## fileName2 <- "TableObservationCR2007-2008-2009.txt"
## fileName3 <- "refEspecesOM.txt"
## fileName1 <- "CB_UnitobsUVC_Couronne.txt"
## fileName2 <- "CB_ObsUVC_Couronne.txt"
## peut être remplacé par : "RUN" , "MAY" , "BA" , "BO" , "CB" , "CR" , "STM" , "NC" selon le sigle du site que vous avez dans le fichier unitobs

## lignes temporaires pour copier/coller

#### CB
SiteEtudie <- "CB"
fileName1 <- "CB_UnitobsUVC_Couronne.txt"
fileName2 <- "CB_ObsUVC_Couronne.txt"
fileName3 <- "refEspecesMED.txt"
nameWorkspace <- "C:/PAMPA/WD"
## fileNameRefSpa <- "REFSPA_CB.txt"


## #### NC Staviro :
## SiteEtudie <- "NC"
## fileName1 <- "UniteObs_Staviro_NCcorrige07072010.txt"
## ## fileName1 <- "UnitObs_Staviro_NC_statutOK230910.txt"
## fileName2 <- "Obs_Staviro_NC.txt"
## ## fileName2 <- "Obs_staviro_NC_tous220910.txt"
## fileName3 <- "PAMPA-WP1-Meth-4-RefSpOM 210910.txt"
## nameWorkspace <- "C:/PAMPA/WD2"


## #### STM :
## SiteEtudie <- "STM"
## ## fileName1 <- "unitobspampaSTMpoisson-100809.txt"
## ## fileName2 <- "obspampaSTMpoisson-100809.txt"
## ## fileName1 <- "unitobspampaSTMrec-100809.txt"
## ## fileName2 <- "obspampaSTMrec-100809.txt"
## ## fileName1 <- "unitobspampaSTMlambi-100809.txt"
## ## fileName2 <- "obspampaSTMlambi-100809.txt"
## ## fileName1 <- "unitobspampaSTMbenthos-100809.txt"
## ## fileName2 <- "obspampaSTMbenthos-100809.txt"
## fileName1 <- "unitobspampaSTMoursin-100809.txt"
## fileName2 <- "obspampaSTMoursin-100809.txt"
## ## fileName1 <- "unitobspampaSTMmacroalgue-100809.txt"
## ## fileName2 <- "obspampaSTMmacroalgue-100809.txt"
## ## fileName1 <- "unitobspampaSTMherbier-100809.txt"
## ## fileName2 <- "obspampaSTMherbier-100809.txt"
## fileName3 <- "PAMPA-WP1-Meth-4-RefSpOM 110810.txt"
## nameWorkspace <- "C:/PAMPA"
## ## fileNameRefSpa <- "REFSPA_NC.txt"       # pas bon mais y'en faut bien un (pas utilisé de toute façon)

## #### RUN :
## SiteEtudie <- "RUN"
## fileName1 <- "unitobspampaGCRMNpoisson-100810.txt"
## fileName2 <- "obspampaGCRMNpoisson-100810.txt"
## ## fileName1 <- "unitobsGCRMNPoissons.txt"
## ## fileName2 <- "obsGCRMNPoissons.txt"
## ## fileName1 <- "unitobsGCRMNBenthos.txt"
## ## fileName2 <- "obsGCRMNBenthos.txt"
## fileName3 <- "PAMPA-WP1-Meth-4-RefSpOM 110810.txt"
## nameWorkspace <- "C:/PAMPA"
## ## fileNameRefSpa <- "refSpatialRUN.txt"


## ## NC Jérémy Drelon :
## SiteEtudie <- "NC"
## fileName1 <- "UnitObs_Staviro_NC_JD.txt"
## fileName2 <- "Obs_Staviro_NC_tous_JD2.txt"
## fileName3 <- "REFSPEOutremer_JD.txt"
## fileNameRefSpa <- "REFSPA_NC.txt"

## fileName1 <- "unitObsMerra.txt"
## fileName2 <- "obsMerra.txt"

## fileNameRefSpa <- "refSpatialCR.txt"
## fileNameRefSpa <- "REFSPA_MAY.txt"
## fileNameRefSpa <- "REFSPA_BA.txt"
## fileNameRefSpa <- "REFSPA_BO.txt"
## fileNameRefSpa <- "REFSPA_CB.txt"
## fileNameRefSpa <- "REFSPA_CR.txt"
## fileNameRefSpa <- "REFSPA_STM.txt"
## fileNameRefSpa <- "REFSPA_NC.txt"

#### Logo :
fileimage <- "./Exec/img/pampa2.GIF"


##################### Initialisation des variables globales ####################
## variables d'environnement pour les graphiques (couleurs et colonnes)
nbColMax <- 30
GraphPartMax <- 0.95
choixPDF <- 0
Jeuxdonnescoupe <- 0
## variables d'environnement pour l'interface
lang <- "FR"

pathMaker.f <- function()
{
    ## Purpose: Redéfinir les chemins (par exemple après changement du
    ##          dossier de travail)
    ## ----------------------------------------------------------------------
    ## Arguments:
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 28 sept. 2010, 10:05
    assign("nameWorkspace", nameWorkspace, envir=.GlobalEnv)
    assign("NomDossierTravail", paste(nameWorkspace, "/FichiersSortie/", sep=""), envir=.GlobalEnv)
    assign("NomDossierData", paste(nameWorkspace, "/Data/", sep=""), envir=.GlobalEnv)   # sert a concaténer les
                                        # variables fileNameUnitObs fileNameObs   fileNameRefEsp fileNameRefSpa
    assign("fileNameUnitObs", paste(NomDossierData, fileName1, sep=""), envir=.GlobalEnv)
    assign("fileNameObs", paste(NomDossierData, fileName2, sep=""), envir=.GlobalEnv)
    assign("fileNameRefEsp", paste(NomDossierData, fileName3, sep=""), envir=.GlobalEnv)
    ## assign("fileNameRefSpa", paste(NomDossierData, fileNameRefSpa, sep=""), envir=.GlobalEnv)
}

pathMaker.f()

assign("siteEtudie", SiteEtudie, envir=.GlobalEnv)
assign("fileimage", fileimage, envir=.GlobalEnv)
assign("Jeuxdonnescoupe", Jeuxdonnescoupe, envir=.GlobalEnv)

assign("nbColMax", nbColMax, envir=.GlobalEnv)
assign("GraphPartMax", GraphPartMax, envir=.GlobalEnv)

typePeche <- ""
assign("typePeche", typePeche)

########################################################################################################################
## Ajouts pour les graphs génériques [yr: 13/08/2010] :

## Noms d'usage des variables des principales tables de données (référentiels compris) :
assign("varNames", read.csv(paste(basePath, "/Exec/NomsVariables.csv", sep=""),
                            header=TRUE, row.names=1, stringsAsFactors=FALSE),
       envir=.GlobalEnv)


## ! cette variable sert visiblement à choisir le type de graphique. le code ci dessous est dupliqué dans plusieurs fonctions
## !#on renomme densite en CPUE pour les jeux de données pêche
## !if (length(typePeche)>1) {
## !   unit$CPUE <- unit$densite
## !   unit$densite = NULL
## ! }
