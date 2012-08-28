#-*- coding: latin-1 -*-

## Plateforme PAMPA de calcul d'indicateurs de ressources & biodiversité
##   Copyright (C) 2008-2010 Ifremer - Tous droits réservés.
##
##   Ce programme est un logiciel libre ; vous pouvez le redistribuer ou le
##   modifier suivant les termes de la "GNU General Public License" telle que
##   publiée par la Free Software Foundation : soit la version 2 de cette
##   licence, soit (à votre gré) toute version ultérieure.
##
##   Ce programme est distribué dans l'espoir qu'il vous sera utile, mais SANS
##   AUCUNE GARANTIE : sans même la garantie implicite de COMMERCIALISABILITÉ
##   ni d'ADÉQUATION À UN OBJECTIF PARTICULIER. Consultez la Licence Générale
##   Publique GNU pour plus de détails.
##
##   Vous devriez avoir reçu une copie de la Licence Générale Publique GNU avec
##   ce programme ; si ce n'est pas le cas, consultez :
##   <http://www.gnu.org/licenses/>.

### File: Chargement_fichiers.R
### Time-stamp: <2012-02-24 20:34:02 Yves>
###
### Author: Yves Reecht
###
####################################################################################################
### Description:
###
### Routines de chargement des fichiers de données.
####################################################################################################




########################################################################################################################
reorderStatus.f <- function(Data, which="statut_protection")
{
    ## Purpose: Réordonner les nivaux du status de protection
    ## ----------------------------------------------------------------------
    ## Arguments: Data : la table de données.
    ##            which : l'indice de la colonne (de préférence un nom).
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date:  1 juin 2011, 14:10

    if ( ! is.factor(Data[ , which]))
    {
        Data[ , which] <- as.factor(Data[ , which])
    }else{}

    if (is.factor(Data[ , which]))
    {
        Data[ , which] <- factor(Data[ , which],
                                 levels=c(getOption("P.statusOrder")[is.element(getOption("P.statusOrder"),
                                                                                levels(Data[ , which]))],
                                          levels(Data[ , which])[!is.element(levels(Data[ , which]),
                                                                             getOption("P.statusOrder"))]))

        return(Data)
    }else{
        stop("Pas un facteur !")
    }
}

########################################################################################################################
dimobsPecRec.f <- function(refUnitobs)
{
    ## Purpose: Transforme les DimObs en données exploitables
    ## (temps de pêche) pour la pêche récréative
    ## ----------------------------------------------------------------------
    ## Arguments: refUnitobs : table d'unitobs
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date:  5 déc. 2011, 11:46

    ## Calcul de l'heure d'enquête :
    x.lt <- as.POSIXlt(as.character(refUnitobs$heure), format="%Hh%M")
    refUnitobs$heureEnq <- x.lt$hour + x.lt$min/60 + x.lt$sec/3600

    ## Calcul de l'heure de début de pêche :
    x.lt <- as.POSIXlt(as.character(refUnitobs$DimObs1), format="%Hh%M")
    refUnitobs$heureDeb <- x.lt$hour + x.lt$min/60 + x.lt$sec/3600

    ## Calcul du temps de pêche :
    refUnitobs$DimObs1 <- sapply(seq(length.out=nrow(refUnitobs)),
                              function(i)
                          {
                              switch(as.character(refUnitobs$heureEnq[i] < refUnitobs$heureDeb[i]),
                                     ## Début le jour précédent (pas d'autre hypothèse sur plusieurs jours) :
                                     "TRUE"=(24 - refUnitobs$heureDeb[i]) + refUnitobs$heureEnq[i],
                                     ## Début le jour même :
                                     "FALSE"=refUnitobs$heureEnq[i] - refUnitobs$heureDeb[i],
                                     ## Incalculable :
                                     "NA"=NA)
                          })

    ## Temps calculé == 0 -> NA
    refUnitobs$DimObs1[refUnitobs$DimObs1 == 0] <- NA

    return(refUnitobs)
}


########################################################################################################################
PlanEchantillonnageBasic.f <- function(tabUnitobs, tabObs, filePathes)
{
    ## Purpose: Écrire le plan d'échantillonnage basic dans un fichier.
    ## ----------------------------------------------------------------------
    ## Arguments: tabUnitobs : table des unités d'observation (data.frame).
    ##            tabObs : table des observations (data.frame).
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 21 sept. 2011, 13:53

    PlanEchantillonnage <- with(dropLevels.f(tabUnitobs[is.element(tabUnitobs$unite_observation,
                                                                   levels(tabObs$unite_observation)), ]),
                                table(an, statut_protection, exclude = NA))

    attr(PlanEchantillonnage, "class") <- "array" # Pour un affichage en "tableau".

    write.csv2(PlanEchantillonnage,
               file=paste(filePathes["results"],
                          "PlanEchantillonnage_basique",
                          ifelse(getOption("P.selection"), "_selection", ""),
                          ".csv", sep=""), row.names=TRUE)
}

########################################################################################################################
scaleMetrics.f <- function(Data, unitobs, refesp,
                           supl=c("an", "site", "statut_protection", "biotope", "latitude", "longitude",
                                  "annee.campagne", "habitat1", "habitat2", "habitat3",
                                  "Identifiant", "Famille", "Genre", "espece"),
                           scale=TRUE)
{
    ## Purpose:
    ## ----------------------------------------------------------------------
    ## Arguments:
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date:  4 janv. 2012, 20:30

    if ( ! is.null(Data) &&
        prod(dim(Data)))                # i.e. ncol et nrow > 0.
    {
        ## Champs à ajouter par référentiel :
        suplUnitobs <- supl[is.element(supl, colnames(unitobs)) &
                            ! is.element(supl, colnames(Data))]
        suplRefesp <- supl[is.element(supl, colnames(refesp)) &
                           ! is.element(supl, colnames(Data))]

        ## Ajout des champs supplémentaires des unitobs :
        if (length(suplUnitobs))
        {
            Data <- merge(Data,
                          unitobs[ , unique(c("unite_observation", suplUnitobs)), drop=FALSE],
                          by=c("unite_observation"))
        }else{}

        ## Ajout des champs supplémentaires du référentiel espèces :
        if (length(suplRefesp) && is.element("code_espece", colnames(Data)))
        {
            Data <- merge(Data,
                          refesp[ , unique(c("code_espece", suplRefesp)), drop=FALSE],
                          by=c("code_espece"))
        }else{}

        ## Scalling : certaines métriques (densités) doivent être ramenées à /100m² :
        if (scale &&
            any(is.element(colnames(Data),
                           colTmp <- c("densite", "densiteMax", "densiteSD",
                                       "biomasse", "biomasseMax", "biomasseSD"))))
        {
            Data[ , is.element(colnames(Data),
                               colTmp)] <- sweep(Data[ , is.element(colnames(Data),
                                                                    colTmp),
                                                      drop=FALSE],
                                                 2, 100, "*")
        }else{}
    }else{}

    return(Data)
}


########################################################################################################################
exportMetrics.f <- function(unitSpSz, unitSp, unit, obs, unitobs, refesp, filePathes, baseEnv)
{
    ## Purpose: Exporter
    ##            * les tables de métriques avec des colonnes supplémentaires
    ##              dans l'environnement global + les sauvegarder dans des
    ##              fichiers.
    ##            * exporter les tables de données dans l'environnement
    ##              global.
    ##          Les noms utilisés dans l'environnement global ne doivent
    ##          pas être les noms internes pour éviter des bugs
    ##          in-déboguables.
    ## ----------------------------------------------------------------------
    ## Arguments: unitSpSz : table de métriques /CT/esp/unitobs.
    ##            unitSp : table de métriques /esp/unitobs.
    ##            unit : table de métriques /unitobs.
    ##            obs : table des données d'observation.
    ##            unitobs : référentiel des unités d'observation.
    ##            refesp : référentiel espèces.
    ##            filePathes : chemins des fichiers/dossiers.
    ##            baseEnv : environnement de l'interface principale.
    ##
    ## Output: Rien !
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date:  4 janv. 2012, 20:08

    PlanEchantillonnageBasic.f(tabUnitobs=unitobs, tabObs=obs, filePathes=filePathes)

    ## Certaines métriques (densités) sont ramenées à /100m² (avec ajout des colonnes par défaut) :
    unitSpSz <- scaleMetrics.f(Data=unitSpSz, unitobs=unitobs, refesp=refesp, scale = TRUE)
    unitSp <- scaleMetrics.f(Data=unitSp, unitobs=unitobs, refesp=refesp, scale = TRUE)
    unit <- scaleMetrics.f(Data=unit, unitobs=unitobs, refesp=refesp, scale = TRUE)

    ## Export des principales tables dans l'environnement global :
    assign("TableUnitSpSz", unitSpSz, envir=.GlobalEnv)
    assign("TableUnitSp", unitSp, envir=.GlobalEnv)
    assign("TableUnit", unit, envir=.GlobalEnv)
    assign("DataObs", obs, envir=.GlobalEnv)
    assign("DataUnitobs", unitobs, envir=.GlobalEnv)
    assign("DataRefesp", refesp, envir=.GlobalEnv)

    infoLoading.f(msg=paste("Des tables supplémentaires (pour calculs aditionnels) on été créées :",
                            ifelse(! is.null(unitSpSz) && prod(dim(unitSpSz)),
                                   "\n\t* TableUnitSpSz : métriques par classe de taille/espèce/unité d'observation.",
                                   ""),
                            "\n\t* TableUnitSp : métriques par espèce/unité d'observation.",
                            "\n\t* TableUnit : métriques (dont biodiversité) par unité d'observation.",
                            "\n\t* DataObs, DataUnitobs, DataRefesp : tables de données.", sep=""),
                  icon="info")

    ## Sauvegardes dans des fichiers :
    if ( ! is.null(unitSpSz) &&
        prod(dim(unitSpSz)))            # i.e. nrow et ncol > 0.
        ## Table unitSpSz si elle existe :
    {
        tryCatch(write.csv2(unitSpSz,
                            file=(fileNm <- paste(filePathes["results"],
                                                  "UnitobsEspeceClassetailleMetriques",
                                                  ifelse(getOption("P.selection"), "_selection", ""),
                                                  ".csv", sep="")),
                            row.names = FALSE),
                 error=function(e)
             {
                 infoLoading.f(msg=paste("Impossible d'écrire le fichier ", fileNm,
                                         ".\nIl est possible qu'il soit ouvert par une autre application", sep=""),
                               icon="warning")

                 errorLog.f(error=e, niv=-4)
             })
    }else{}                             # Sinon rien !

    ## Table unitSp :
    tryCatch(write.csv2(unitSp,
                        file=(fileNm <- paste(filePathes["results"],
                                              "UnitobsEspeceMetriques",
                                              ifelse(getOption("P.selection"), "_selection", ""),
                                              ".csv", sep="")),
                        row.names = FALSE),
             error=function(e)
         {
             infoLoading.f(msg=paste("Impossible d'écrire le fichier ", fileNm,
                                     ".\nIl est possible qu'il soit ouvert par une autre application", sep=""),
                           icon="warning")

             errorLog.f(error=e, niv=-4)
         })

    ## Table unit :
    tryCatch(write.csv2(unit,
                        file=(fileNm <- paste(filePathes["results"],
                                              "UnitobsMetriques",
                                              ifelse(getOption("P.selection"), "_selection", ""),
                                              ".csv", sep="")),
                        row.names = FALSE),
             error=function(e)
         {
             infoLoading.f(msg=paste("Impossible d'écrire le fichier ", fileNm,
                                     ".\nIl est possible qu'il soit ouvert par une autre application", sep=""),
                           icon="warning")

             errorLog.f(error=e, niv=-4)
         })
}


########################################################################################################################
selectionObs.SVR.f <- function()
{
    ## Purpose: Définir le seuil de Dmin (en m) au-dessus duquel les
    ##          observations ne sont pas prises en compte.
    ## ----------------------------------------------------------------------
    ## Arguments: aucun
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date:  8 nov. 2010, 10:29

    onOK.selectionObs.SVR.f <- function()
    {
        ## Purpose: Action lorsque le bouton de choix de seuil pour les SVR est
        ##          cliqué.
        ## ----------------------------------------------------------------------
        ## Arguments: aucun
        ## ----------------------------------------------------------------------
        ## Author: Yves Reecht, Date:  8 nov. 2010, 12:01

        if (tclvalue(Suppr) == "1")
        {
            if (!is.na(as.numeric(tclvalue(Level))))
            {
                tclvalue(Done) <- "1"
            }else{
                tclvalue(Done) <- "2"
            }
        }else{
            tclvalue(Level) <- Inf
            tclvalue(Done) <- "1"
        }
    }

    dminDefault <- 5                    # 5m par défaut.

    Done <- tclVar("0")
    Suppr <- tclVar("1")                # Seuil utilisé par défaut.
    Level <- tclVar(dminDefault)        # tclVar pour le seuil (initialisée au défaut).

    WinSVR <- tktoplevel()
    tkwm.title(WinSVR, "Sélection des observations")

    FrameInfo <- tkframe(WinSVR, borderwidth=2, relief="groove")

    CB.supprObs <- tkcheckbutton(WinSVR, variable=Suppr)
    E.supprLevel <- tkentry(WinSVR, width=3, textvariable=Level)

    FrameBT <- tkframe(WinSVR)
    BT.OK <- tkbutton(FrameBT, text="   OK   ",
                      command=onOK.selectionObs.SVR.f)

    tkbind(WinSVR, "<Destroy>", function(){tclvalue(Done) <- "3"}) # En cas de destruction de fenêtre.
    tkbind(E.supprLevel, "<Return>", onOK.selectionObs.SVR.f)

    ## Placement des éléments graphiques :
    tkgrid(tklabel(WinSVR, text=""))

    tkgrid(FrameInfo, column=2, columnspan=2, sticky="we")
    tkgrid(tklabel(FrameInfo,
                   text=paste("Information\n\n Types d'interpolations : ",
                   ifelse(getOption("PAMPA.SVR.interp") == "extended",
                          "étendues !",
                          "simples !"), "\n", sep=""), justify="left"), sticky="w")

    tkgrid(tklabel(WinSVR, text=""))

    tkgrid(tklabel(WinSVR, text="\t"),
           CB.supprObs,
           tklabel(WinSVR, text="  Ne conserver que les observations pour lesquelles Dmin =< "),
           E.supprLevel,
           tklabel(WinSVR, text="m ?\t "))

    tkgrid(tklabel(FrameBT, text="\n\t"),
           BT.OK,
           tklabel(FrameBT, text="\t\n"))
    tkgrid(FrameBT, column=0, columnspan=5)

    tkfocus(E.supprLevel)

    winSmartPlace.f(WinSVR, xoffset=-200, yoffset=-50)

    repeat
    {
        tkwait.variable(Done)           # attente d'une modification de statut.

        switch(tclvalue(Done),
               "1"={                    # Statut OK :
                   tkdestroy(WinSVR)
                   return(as.numeric(tclvalue(Level)))
               },
               "2"={                    # Le seuil n'est pas numérique :
                   tkmessageBox(message="Vous devez choisir un seuil numérique (séparateur '.')",
                                icon="error")
                   tclvalue(Done) <- "0"
                   tkfocus(E.supprLevel)
                   winRaise.f(WinSVR)
                   next()
               },
               "3"={                    # Destruction de la fenêtre :
                   return(NULL)
               })
    }

    tkdestroy(WinSVR)
}

########################################################################################################################
testFiles.f <- function(filePathes)
{
    ## Purpose: Tester l'existence des fichiers et dossiers (créés si besoin)
    ## ----------------------------------------------------------------------
    ## Arguments: filePathes
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 14 déc. 2011, 16:43

    required <- names(getOption("P.requiredVar"))

    ## Tests :
    if (any(idx <- (sapply(filePathes[required], file.access, mode=2) != 0))) # un quelconque fichier
                                        # non accessible en lecture.
    {
        ## Plusieurs fichiers ? :
        pluriel <- sum(idx) > 1

        ## Message d'erreur dans les infos de chargement :
        infoLoading.f(msg=paste(ifelse(pluriel,
                                       "Les fichiers suivants n'existent pas ou ne sont pas accessibles :",
                                       "Le fichier suivant n'existe pas ou n'est pas accessible :"),
                                "\n\t",
                                paste(filePathes[required[idx]], collapse="\n\t"),
                                sep=""),
                      icon="error")

        stop("Erreur de fichier (inexistant ou non lisible)")
    }else{
        ## ... sinon on passe au tests sur le répertoire de résultats :
        if (file.access(filePathes["results"], mode=0) == -1)
        {
            ## Créations s'il n'existe pas :
            dir.create(filePathes["results"])
        }else{
            ## ... sinon test du dossier de résultats pour l'écriture :
            if (file.access(filePathes["results"], mode=4) == -1)
            {
                ## Message d'erreur dans les infos de chargement s'il n'existe pas :
                infoLoading.f(msg=paste("Impossible d'écrire dans le dossier de résultats :\n",
                                        filePathes["results"],
                                        sep=""),
                              icon="error")

                stop("Dossier de résultats non accessible en écriture")
            }else{}
        }

        ## Test d'existence du référentiel spatial (sinon remplacé par NA) :
        if (! is.null(filePathes["refspa"]) && ! is.na(filePathes["refspa"]))
        {
            if (file.access(filePathes["refspa"], 2) == -1)
            {
                filePathes["refspa"] <- NA
            }else{}
        }else{
            filePathes["refspa"] <- NA
        }

        return(filePathes)
    }
}


########################################################################################################################
testConfig.f <- function(requiredVar, fileNames=NULL, dataEnv=NULL)
{
    ## Purpose: Test l'existence des variables requises (noms de fichiers)
    ##          et crée les chemins le cas échéant.
    ## ----------------------------------------------------------------------
    ## Arguments: requiredVar : vecteur de nom des variables requises (noms
    ##                          d'entrées dans fileNames).
    ##            fileNames : vecteur de noms des fichiers et dossiers.
    ##            dataEnv : enironnement où sont stockées les données (si
    ##                      fileNames=NULL).
    ##            baseEnv : environnement de l'interface principale.
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 08 dec. 2011, 15:10

    ## Récupération de fileNames (si NULL) :
    if (all(is.null(c(fileNames, dataEnv))))
    {                                   # Erreur si pas définit
        stop("\"fileNames\" ou \"dataEnv\" doit être défini !")
    }else{
        ## Dans l'environnement des données si non passé en paramètre :
        if (is.null(fileNames))
        {
            fileNames <- tryCatch(get("fileNames", envir=dataEnv),
                                  error=function(e) stop("FileName n'existe pas dans l'environnement des données !"))
        }else{}
    }

    ## Indices des variables définies :
    existVar <- ! (is.na(fileNames[names(requiredVar)]) | is.null(fileNames[names(requiredVar)]))

    if (any(! existVar))                    # Si au moins une des variables n'est pas définie.
    {
        pluriel <- sum(! existVar) > 1

        ## Demande pour l'ouverture du fichier de configuration :
        if(tclvalue(tkmessageBox(message=paste(ifelse(pluriel,
                                                      "Les variables suivantes ne sont pas définies ",
                                                      "La variable suivante n'est pas définie "),
                                               "dans votre fichier \"", basePath, "/Scripts_Biodiv/Config.R\" :\n\n\t*  ",
                                               paste(requiredVar[! existVar], collapse="\n\t*  "),
                                               "\n\nVoulez-vous éditer ce fichier ?",
                                               "\n\t(ouverture automatiquement de la sauvegarde également, si elle existe).",
                                               sep=""),
                                 icon="warning", type="yesno", title="Configuration imcomplète",
                                 default="no")) == "yes")
        {
            if (exists("shell.exec", mode="function"))
            {
                shell.exec(paste(basePath, "/Scripts_Biodiv/Config.R", sep=""))

                if (file.exists(fileTmp <- paste(basePath, "/Scripts_Biodiv/Config.bak.R", sep="")))
                {
                    shell.exec(fileTmp)
                }else{}
            }else{
                file.edit(paste(basePath, "/Scripts_Biodiv/Config.R", sep=""))

                if (file.exists(fileTmp <- paste(basePath, "/Scripts_Biodiv/Config.bak.R", sep="")))
                {
                    file.edit(fileTmp)
                }else{}
            }
        }else{}

        filePathes <- NULL
    }else{
        filePathes <- pathMaker.f(fileNames=fileNames, dataEnv = dataEnv)
    }

    ## Vérification de l'existence des fichiers et dossiers :
    filePathes <- testFiles.f(filePathes=filePathes)

    return(filePathes)
}

########################################################################################################################
loadRefEspeces.f <- function (pathRefesp, baseEnv=.GlobalEnv)
{
    ## rm(especes)
    runLog.f(msg=c("Chargement du référentiel espèces :"))

    ## Importation des caracteristiques des especes
    especes <- read.table(pathRefesp, sep="\t", dec=".", quote="", header=TRUE, encoding="latin1")
    names(especes) <- c("code_espece", "GrSIH", "CodeSIH", "IssCaap", "TaxoCode", "CodeFAO", "CodeFB", "Phylum",
                        "Cat_benthique", "Classe", "Ordre", "Famille", "Genre", "espece", "Identifiant", "ObsNC",
                        "ObsRUN", "ObsMAY", "ObsSTM", "ObsCB", "ObsBA", "ObsBO", "ObsCR", "taillemax", "L50",
                        "cryptique", "mobilite", "territorial", "nocturne", "comportement.grp", "agreg.saison",
                        "position.col.eau", "strategie.demo", "Type.ponte", "Habitat.Prefere", "Changement.sexe",
                        "regim.alim", "interet.chasseNC", "interet.chasseRUN", "interet.chasseMAY", "interet.chasseSTM",
                        "interet.chasseCB", "interet.chasseBA", "interet.chasseBO", "interet.chasseCR",
                        "interet.ligneNC", "interet.ligneRUN", "interet.ligneMAY", "interet.ligneSTM",
                        "interet.ligneCB", "interet.ligneBA", "interet.ligneBO", "interet.ligneCR", "interet.filetNC",
                        "interet.filetRUN", "interet.filetMAY", "interet.filetSTM", "interet.filetCB",
                        "interet.filetBA", "interet.filetBO", "interet.filetCR", "interet.casierNC",
                        "interet.casierRUN", "interet.casierMAY", "interet.casierSTM", "interet.casierCB",
                        "interet.casierBA", "interet.casierBO", "interet.casierCR", "interet.piedNC", "interet.piedRUN",
                        "interet.piedMAY", "interet.piedSTM", "interet.piedCB", "interet.piedBA", "interet.piedBO",
                        "interet.piedCR", "interetComMAY", "Coeff.a.Med", "Coeff.b.Med", "Coeff.a.NC", "Coeff.a.MAY",
                        "Coeff.b.NC", "Coeff.b.MAY", "poids.moyen.petits", "poids.moyen.moyens", "poids.moyen.gros",
                        "taille_max_petits", "taille_max_moyens", "niveau.a.et.b.MED", "niveau.a.et.b.NC",
                        "niveau.a.et.b.MAY", "emblematiqueNC", "emblematiqueRUN", "emblematiqueMAY", "emblematiqueSTM",
                        "emblematiqueCB", "emblematiqueBA", "emblematiqueBO", "emblematiqueCR", "stat.IUCN",
                        "autre.statutNC", "autre.statutRUN", "autre.statutMAY", "autre.statutSTM", "autre.statutCB",
                        "autre.statutBA", "autre.statutBO", "autre.statutCR", "etat.pop.localNC", "etat.pop.localRUN",
                        "etat.pop.localMAY", "etat.pop.localSTM", "etat.pop.localCB", "etat.pop.localBA",
                        "etat.pop.localBO", "etat.pop.localCR", "endemiqueNC", "endemiqueRUN", "endemiqueMAY",
                        "endemiqueSTM", "endemiqueCB", "endemiqueBA", "endemiqueBO", "endemiqueCR")


    ## Verification du nombre de colonnes:
    if (ncol(especes) != 125)
    {
        rm(especes)
        gestionMSGerreur.f("nbChampEsp", env=baseEnv)
    }
    if (nrow(especes)!=0)
    {
        especes[especes=="-999"] <- NA
    }

    ## Ajout de cathégories benthiques supplémentaires lues dans un fichier de correspondance :
    correspCatBenthique <- read.csv(paste(basePath,
                                          "/Scripts_Biodiv/corresp-cat-benth.csv", sep=""),
                                    row.names=1)

    especes <- cbind(especes, correspCatBenthique[as.character(especes$Cat_benthique), , drop=FALSE])

    ## Pour vérif :
    ## na.omit(especes[as.integer(runif(50,min=1, max=3553)), c("Cat_benthique", "CategB_general", "CategB_groupe")])

    ## Suppression de la ligne en NA
    especes <- subset(especes, !is.na(especes$code_espece))
    ## assign("especes", especes, envir=.GlobalEnv)
    return(especes)
}

########################################################################################################################
chooseInList.f <- function(modList, fieldName, selectMode, ordered)
{
    ## Purpose:
    ## ----------------------------------------------------------------------
    ## Arguments:
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 12 janv. 2012, 18:00

    W.fact <- tktoplevel(width = 80)
    tkwm.title(W.fact, paste("Selection des valeurs de ", fieldName, sep=""))

    scr <- tkscrollbar(W.fact, repeatinterval=5, command=function(...)tkyview(tl, ...))

    tl <- tklistbox(W.fact, height=20, width=50, selectmode=selectMode,
                    yscrollcommand=function(...)tkset(scr, ...), background="white")

    tkgrid(tklabel(W.fact, text=paste("Liste des valeurs de \"", fieldName,
                                      "\" :\n",
                                      ifelse(selectMode == "single",
                                             "Une seule sélection possible.",
                                             "Plusieurs sélections possibles."), sep="")))

    tkgrid(tl, scr)
    tkgrid.configure(scr, rowspan=4, sticky="ensw")
    tkgrid.configure(tl, rowspan=4, sticky="ensw")

    if (ordered)
    {
        maliste <- sort(as.character(unique(modList)))
    }

    ## On remplit la liste de choix :
    for (i in seq(along=maliste))
    {
        tkinsert(tl, "end", maliste[i])
    }

    OnOK <- function ()
    {
        assign("selectfact",
               eval(maliste[as.numeric(tkcurselection(tl))+1], envir=parent.env(environment())),
               envir=parent.env(environment()))

        tkdestroy(W.fact)
    }

    OK.but <-tkbutton(W.fact, text="OK", command=OnOK)
    tkgrid(OK.but, pady=5)

    winSmartPlace.f(W.fact)
    tkfocus(tl)

    tkwait.window(W.fact)

    if (exists("selectfact") && length(selectfact))
    {
        return(selectfact)
    }else{
        return(NULL)
    }
}


########################################################################################################################
checkType.unitobs.f <- function(unitobs)
{
    ## Purpose: test l'existence d'un seul type, sinon demande à
    ##          l'utilisateur d'en choisir un pour réduire le jeu de données.
    ##          Défini le type d'observation dans les options.
    ## ----------------------------------------------------------------------
    ## Arguments: unitobs : la table d'unités d'observation.
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 13 déc. 2011, 11:16

    if (length(unique(unitobs$type)) > 1)
    {
        tkmessageBox(message=paste("Un seul type d'observation à la fois peut être analysé :\n\n",
                                   "Choisissez le type d'observation que vous souhaitez analyser.", sep=""),
                     icon="warning", type="ok")

        while (is.null(selectType <- chooseInList.f(modList=unitobs[ ,"type"],
                                                    fieldName="type",
                                                    selectMode="single",
                                                    ordered=TRUE)))
        {}

        message(paste("Type d'observation sélectionné :", selectType))

        ## Suppression des niveaux de facteur inutilisés :
        unitobs <- dropLevels.f(subset(unitobs, unitobs$type == selectType))

        ## assign("obs", obs, envir=.GlobalEnv)
        ## assign("unitobs", unitobs, envir=.GlobalEnv)

        ## ## Reconfiguration des infos sur l'AMP sélectionnée et le type d'observations analysées :
        ## tkconfigure(ResumerAMPetType,
        ##             text=paste("Aire Marine Protégée : ", unique(unitobs$AMP), " ; type d'observation : ",
        ##             unique(unitobs$type), sep=""))

    }

    options(P.obsType=unique(as.character(unitobs$type)))

    return(unitobs)
}

########################################################################################################################
checkUnitobs.in.obs.f <- function(obs, unitobs)
{
    ## Purpose: Vérifie que toutes les observations correspondent à des
    ##          unitobs existantes. Réduit le jeu de données si nécessaire.
    ## ----------------------------------------------------------------------
    ## Arguments: obs : la table d'observations.
    ##            unitobs : la table d'unités d'observation.
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 13 déc. 2011, 11:34

    ## obs$type <- unitobs$type[match(obs$unite_observation, unitobs$unite_observation), drop=TRUE]

    if ( ! all(idxTmp <- is.element(obs$unite_observation, unitobs$unite_observation)))
    {
        ## Ajout du message pour le chargement :
        infoLoading.f(msg=paste("Attention, la Table obs contient ",
                                sum( ! idxTmp),
                                " (sur ",
                                nrow(obs),
                                ") enregistrements ",
                                "\navec des unités d'observation absentes dans la table unitobs.",
                                "\nCes observations ont été supprimées.",
                                sep=""),
                     icon="warning")

        obs <- dropLevels.f(obs[idxTmp, ])
    }else{}

    return(obs)
}


########################################################################################################################
loadUnitobs.f <- function(pathUnitobs)
{
    ## Purpose: Chargement du fichier d'unités d'observations
    ## ----------------------------------------------------------------------
    ## Arguments: pathUnitobs : chemin du fichier
    ##            dataEnv : environnement de l'interface
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date:  5 déc. 2011, 11:33

    ## Lecture du fichier :
    unitobs <- read.table(pathUnitobs, sep="\t", dec=".", header=TRUE, encoding="latin1")

    ## Changement des noms de colonnes :
    colnames(unitobs) <- c("AMP", "unite_observation", "type", "site", "station", "caracteristique_1", "caracteristique_2",
         "fraction_echantillonnee", "jour", "mois", "an", "heure", "nebulosite", "direction_vent", "force_vent",
         "etat_mer", "courant", "maree", "phase_lunaire", "latitude", "longitude", "statut_protection", "avant_apres",
         "biotope", "biotope_2", "habitat1", "habitat2", "habitat3", "visibilite", "prof_min", "prof_max", "DimObs1",
         "DimObs2", "nb_plong", "plongeur")



    ## Traitement des NAs pour "caracteristique_1" : (est-ce bien la peine [yr: 5/12/2011] ?)
    levels(unitobs$caracteristique_1) <- c(levels(unitobs$caracteristique_1), "NA") # bon ça corrige l'erreur ci-dessous
                                        # mais est-ce bien nécessaire ? [yr: 23/08/2010]

    unitobs$caracteristique_1[is.na(unitobs$caracteristique_1)] <- "NA"

    ## Vérification du type d'observations :
    unitobs <- checkType.unitobs.f(unitobs)

    ## Si les unités d'observation sont ne sont pas des facteurs, on les force en facteur :
    ## (typiquement si numérique)
    if (!is.factor(unitobs$unite_observation))
    {
        unitobs$unite_observation <- factor(as.character(unitobs$unite_observation))
    }

    ## Si caracteristique_2 est au format année de campagne, renommer la colonne :
    if (is.temporal.f("caracteristique_2", unitobs))
    {
        colnames(unitobs)[colnames(unitobs) == "caracteristique_2"] <- "annee.campagne"
    }

    if (getOption("P.obsType")=="PecRec")
    {
        unitobs <- dimobsPecRec.f(refUnitobs=unitobs)
    }

    if (nrow(unitobs)!=0)
    {
        unitobs[unitobs=="-999"] <- NA
    }

    ## Reorganisation des niveaux de protection :
    unitobs <- reorderStatus.f(Data=unitobs, which="statut_protection")

    ## Années : integer -> factor (nécessaire pour les analyses stats):
    unitobs$an <- factor(unitobs$an)

    return(unitobs)
}

########################################################################################################################
loadRefspa.f <- function(pathRefspa, baseEnv=.GlobalEnv)
{
    ## Purpose: chargement du référentiel spatial s'il existe.
    ## ----------------------------------------------------------------------
    ## Arguments: pathRefspa : chemin vers le référentiel spatial.
    ##            baseEnv : environnement de l'interface principale.
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date:  7 déc. 2011, 14:22

    if (missing(pathRefspa) || is.null(pathRefspa) || is.na(pathRefspa) || length(pathRefspa) == 0 ||
        ! file.exists(pathRefspa))
    {
        infoLoading.f(msg="Pas de référentiel spatial défini ou fichier inexistant !", icon="warning")
        return(NULL)
    }else{
        refSpatial <- read.table(pathRefspa, sep="\t", dec=".", header=TRUE, encoding="latin1")

        if (ncol(refSpatial) == 15)     # [!!!] à vérifier  [yr: 7/12/2011]
        {
            colnames(refSpatial) <- c("code_zone", "zone", "AMP", "site", "station", "groupe", "longitude_zone",
                                      "latitude_zone", "surface", "lineaire_cotier", "statut_protection",
                                      "zonage_peche", "code_SIH", "statut_PAMPA", "nbCM")

            return(refSpatial)
        }else{
            infoLoading.f(msg="Référentiel spatial incorrect (non pris en compte) !", icon="warning")
            return(NULL)
        }
    }
}

########################################################################################################################
loadObservations.f <- function(pathObs)
{
    ## Purpose: Chargement du fichier d'observations
    ## ----------------------------------------------------------------------
    ## Arguments: pathObs : chemin (character) vers le fichier
    ##                      d'observations.
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 12 déc. 2011, 14:38

    obs <- read.table(pathObs, sep="\t", dec=".", header=TRUE, encoding="latin1")

    if (getOption("P.obsType") != "SVR") # [!!!] à modifier (et porter dans une autre fonction ?)  [yr: 12/12/2011]
    {
        colnames(obs) <- c("unite_observation", "secteur", "code_espece", "sexe", "taille", "classe_taille", "poids",
                           "nombre", "dmin", "dmax")
    }else{
        ## On renomme les colonnes + identification du type d'interpolation :
        switch(as.character(ncol(obs)),
               "11"={
                   colnames(obs) <- c("unite_observation", "rotation", "secteur", "code_espece", "sexe", "taille",
                                      "classe_taille", "poids", "nombre", "dmin", "dmax")

                   options(PAMPA.SVR.interp="extended")
               },
               "14"={
                   colnames(obs) <- c("unite_observation", "rotation", "sec.valides", "sec.ciel", "sec.sol",
                                      "sec.obstrue", "code_espece", "sexe", "taille",
                                      "classe_taille", "poids", "nombre", "dmin", "dmax")

                   options(PAMPA.SVR.interp="basic")
               },
               stop("Le fichier d'observations ne contient pas le bon nombre de colonnes"))

        obs$rotation <- as.numeric(obs$rotation)

        dminMax <- NULL
        while (is.null(dminMax))
        {
            dminMax <- selectionObs.SVR.f()
        }

        ## On ne tient pas compte des observations à une distance > dminMax
        ## (pas de subset car tendance à faire disparaître des unitobs des analyses) :
        idxSupr <- obs$dmin > dminMax

        obs$nombre[idxSupr] <- 0
        obs$poids[idxSupr] <- NA
        obs$taille[idxSupr] <- NA

        ## obs <- subset(obs, dmin <= dminMax)
    }

    ## remplacement des -999 en NA
    if (as.logical(nrow(obs)))                      # !=0
    {
        obs[obs == "-999"] <- NA
    }

    ## nombre : numeric -> factor (nécessaire pour une bonne prise en compte dans les analyses stat)...
    ## uniquement si == entier :
    if (isTRUE(all.equal(obs$nombre, as.integer(obs$nombre))))
    {
        obs$nombre <- as.integer(obs$nombre)
    }else{}

    ## Si les unités d'observation sont ne sont pas des facteurs :
    if (!is.factor(obs$unite_observation))
    {
        obs$unite_observation <- factor(as.character(obs$unite_observation))
    }

    return(obs)
}


########################################################################################################################
mergeSpaUnitobs.f <- function(unitobs, refspa)
{
    ## Purpose: Fusion des la table des unitobs et du référentiel spatial si
    ##          adapté.
    ## ----------------------------------------------------------------------
    ## Arguments:
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date:  7 déc. 2011, 17:23

    res <- merge(unitobs, refspa, by.x="site", by.y="code_zone", all.x=TRUE, all.y=FALSE,
                 suffixes = c(".KEEP",".SUPR"))

    res <- res[ , ! grepl("\\.SUPR$", colnames(res))]
    res <- res[ , colnames(res) != "station.1"]

    colnames(res) <- sub("\\.KEEP$", "", colnames(res))

    res <- res[ , c(colnames(unitobs),
                    colnames(res)[!is.element(colnames(res), colnames(unitobs))])]

    res <- reorderStatus.f(Data=dropLevels.f(df=res, which="statut_PAMPA"),
                           which = "statut_PAMPA")

    return(res)
}



########################################################################################################################
loadConfig.f <- function(dataEnv=NULL)
{
    ## Purpose: (re-)charger la configuration :
    ## ----------------------------------------------------------------------
    ## Arguments: dataEnv : l'environnement où stocker les données lues.
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date:  5 déc. 2011, 14:39

    source("./Scripts_Biodiv/Config.R", encoding="latin1", local=TRUE)

    confNames <- c(unitobs=ifelse(exists("fileNameUnitobs"), fileNameUnitobs, NA),
                   obs=ifelse(exists("fileNameObs"), fileNameObs, NA),
                   refesp=ifelse(exists("fileNameRefesp"), fileNameRefesp, NA),
                   refspa=ifelse(exists("fileNameRefspa"), fileNameRefspa, NA),
                   ws=ifelse(exists("nameWorkspace"), nameWorkspace, NA))

    ## Si un environnement indiqué, les données y sont exportées
    if (!is.null(dataEnv))
    {
        assign("fileNames", confNames, env=dataEnv)
    }else{}

    return(confNames)
}


########################################################################################################################
pathMaker.f <- function(fileNames,
                        dataEnv=NULL)
{
    ## Purpose: Redéfinir les chemins (par exemple après changement du
    ##          dossier de travail)
    ## ----------------------------------------------------------------------
    ## Arguments: fileNames : vecteur nommé des noms de dossier et fichiers
    ##                        de données, avec
    ##                        - unitobs : fichier d'unitobs.
    ##                        - obs : fichier d'observations.
    ##                        - refesp : référentiel espèces.
    ##                        - refspa : référentiel spatial (pas utilisé
    ##                                   actuellement).
    ##                        - ws : dossier de travail.
    ##            dataEnv : environnement des données.
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 28 sept. 2010, 10:05

    ## Répertoires de travail :
    directories <- c(results=paste(fileNames["ws"], "/Results/", sep=""),
                     data=paste(fileNames["ws"], "/Data/", sep=""),
                     ws=unname(fileNames["ws"]))

    ## Fichiers de données :
    filePathes <- c(unitobs=paste(directories["data"], fileNames["unitobs"], sep=""),
                    obs=paste(directories["data"], fileNames["obs"], sep=""),
                    refesp=paste(directories["data"], fileNames["refesp"], sep=""),
                    refspa=unname(ifelse(is.na(fileNames["refspa"]),
                                         NA,
                                         paste(directories["data"], fileNames["refspa"], sep=""))),
                    directories)

    ## Export vers l'environnement de données si défini :
    if (!is.null(dataEnv))
    {
        assign("filePathes", filePathes, envir=dataEnv)
    }else{}

    return(filePathes)
}

########################################################################################################################
loadData.f <- function(filePathes, dataEnv, baseEnv=.GlobalEnv)
{
    ## Purpose: Lancement des différentes étapes du chargement des données.
    ## ----------------------------------------------------------------------
    ## Arguments: filePathes : vecteur nommé des chemins de fichiers de
    ##                         données et dossiers, avec
    ##                         - unitobs : fichier d'unitobs.
    ##                         - obs : fichier d'observations.
    ##                         - refesp : référentiel espèces.
    ##                         - refspa : référentiel spatial (pas utilisé
    ##                                    actuellement).
    ##                         - ws : dossier de travail.
    ##                         - ...
    ##            dataEnv : environnement où stocker les données.
    ##            baseEnv : environnement parent (interface).
    ##
    ## Output: liste nommé des tables de données, avec
    ##         - unitSpSz : table agrégée /classe de taille/sp/unitobs.
    ##         - unitSp : table agrégée /espèce/unitobs.
    ##         - unit : table agrégée /unitobs.
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date:  2 déc. 2011, 10:59

    ## Le profilage de la fonction est activé si l'option adéquate est sur TRUE :
    pampaProfilingStart.f()

    runLog.f(msg=c("--------------------------------------------------------------------------------",
                   "Nouveau chargement des données :"))


    add.logFrame.f(msgID="dataLoadingNew", env = baseEnv,
                   filePathes=filePathes)

    ## Réinitialisation de l'indicateur de sélection :
    options(P.selection=FALSE)

    ## Informations de chargement (initialisation) :
    ## [!!!] travaille dans l'environnement global pour l'instant. À terme, modifier + fonctions associées pour
    ## travailler dans .baseEnv [!!!]  [yr: 5/12/2011]
    infoGeneral.f(msg="      Chargement des données      ",
                  waitCursor=TRUE,
                  font=tkfont.create(weight="bold", size=9), foreground="darkred")

    initInnerTkProgressBar.f(initial=0, max=22, width=450)


    ## ##################################################
    ## Chargement des fichiers :

    ## Fichier d'unités d'observations :
    stepInnerProgressBar.f(n=0, msg="Chargement du référentiel d'unités d'observation")

    refUnitobs <- loadUnitobs.f(pathUnitobs=filePathes["unitobs"])

    ## Éventuelle reconfiguration de la barre de progression du chargement en fonction du type d'unitobs :
    switch(getOption("P.obsType"),
           "SVR"={},                    # rien à faire.
           "LIT"={
               reconfigureInnerProgressBar.f(max=12)
           },                           # Pour le benthos on ne calcule pas les métriques / classe de taille.
           {
               reconfigureInnerProgressBar.f(max=13) # Dans tous les autres cas : 12
           })

    ## Info sur les AMP du jeu actuel :
    options(P.MPA=as.character(unique(refUnitobs[ , "AMP"])))

    ## Information sur l'(les) AMP sélectionnées et le type d'observations analysées :
    tkconfigure(get("ResumerAMPetType", envir=baseEnv),
                text=paste(ifelse(length(getOption("P.MPA")) < 2,
                                  "Aire Marine Protégée : ",
                                  "Aires Marines Protégées : "),
                           paste(getOption("P.MPA"), collapse=", "),
                           " ; type d'observation : ",
                           getOption("P.obsType"), sep=""))

    ## Fichier de référentiel spatial :
    stepInnerProgressBar.f(n=1, msg="Chargement du référentiel spatial...")

    refSpatial <- loadRefspa.f(pathRefspa=filePathes["refspa"])

    ## Fusion de la table d'unités d'observation et de celle du référentiel spatial :
    if (!is.null(refSpatial))
    {
        refUnitobs <- mergeSpaUnitobs.f(unitobs=refUnitobs, refspa=refSpatial)
    }else{}

    ## Fichier d'observations :
    stepInnerProgressBar.f(n=1, msg="Chargement du fichier d'observations...")

    tabObs <- loadObservations.f(pathObs=filePathes["obs"])

    tabObs <- checkUnitobs.in.obs.f(obs=tabObs, unitobs=refUnitobs) # Réduction aux unitobs existantes.

    ## Fichier du référentiel espèces :
    stepInnerProgressBar.f(n=1, msg="Chargement du référentiel espèces")

    refEspeces <- loadRefEspeces.f(pathRefesp=filePathes["refesp"], baseEnv=baseEnv)

    ## Interaction avec l'interface :
    tkconfigure(get("ResumerEspaceTravail", envir=baseEnv), # [!!!] déplacer vers la fin  [yr: 13/12/2011]
                text=paste("Espace de travail : ", filePathes["ws"]))

    return(list(obs=tabObs, unitobs=refUnitobs, refesp=refEspeces))

    ## ##################################################
    ## Calcul des tables de métriques :

    pampaProfilingEnd.f()
}

########################################################################################################################
loadDefault.f <- function(baseEnv, dataEnv)
{
    ## Purpose: Chargement des données en utilisant la configuration.
    ## ----------------------------------------------------------------------
    ## Arguments: baseEnv : environnement de l'interface principale.
    ##            dataEnv : environnement des données.
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 14 déc. 2011, 17:33

    ## Sauvegarde et suppression des noms de fichiers, options, etc. :
    dataEnvTmp <- backupEnv.f(dataEnv) # On sauvegarde temporairement l'environnement de données.
    obsTypeTmp <- getOption("P.obsType") # ...et le type d'observations.

    suppressWarnings(rm(list=ls(envir=dataEnv), envir=dataEnv)) # [!!!] revoir  [yr: 13/12/2011]

    ## Rechargement de la configuration :
    fileNames <- loadConfig.f(dataEnv = dataEnv)

    ## Vérification de la configuration :
    filePathes <- testConfig.f(requiredVar=getOption("P.requiredVar"),
                               fileNames = fileNames,
                               dataEnv = dataEnv)

    ## chargement (conditionnel) des données :
    if (! is.null(filePathes))
    {
        Data <- loadData.f(filePathes=filePathes, dataEnv=dataEnv, baseEnv = baseEnv)

        ## MàJ du tableau d'informations de l'interface principale :
        updateSummaryTable.f(get("tclarray", envir=baseEnv),
                             fileNames, Data,
                             get("table1", envir=baseEnv))
    }else{
        stop("Problème de configuration")
    }

    ## Calculs des poids (faits par AMP) :
    if ( ! is.benthos.f())
    {
        Data <- calcWeight.f(Data=Data)
    }else{}

    ## Assignement des données dans l'environnement adéquat :
    listInEnv.f(list=Data, env=dataEnv)

    ## assign("Data", Data, envir=.GlobalEnv) # [tmp]  [yr: 20/12/2011]

    ## Calcul des tables de métriques :
    metrics <- calcTables.f(obs=Data$obs, unitobs=Data$unitobs, refesp=Data$refesp, dataEnv=dataEnv)

    stepInnerProgressBar.f(n=2, msg="Finalisation du calcul des tables de métriques")

    ## Assignement des tables de métriques dans l'environnement adéquat :
    listInEnv.f(list=metrics, env=dataEnv)

    ## Sauvegarde pour restauration ultérieure :
    assign("backup", c(metrics, list(obs=Data$obs)), envir=dataEnv)

    ## Export des tables de métriques :
    stepInnerProgressBar.f(n=1, msg="Export des tables de métriques dans des fichiers")

    exportMetrics.f(unitSpSz=metrics$unitSpSz, unitSp=metrics$unitSp, unit=metrics$unit,
                    obs=Data$obs, unitobs=Data$unitobs, refesp=Data$refesp,
                    filePathes=filePathes, baseEnv=baseEnv)

    ## Ajout des fichiers créés au log de chargement :
    add.logFrame.f(msgID="fichiers", env = baseEnv,
                   results=filePathes["results"],
                   has.SzCl=( ! is.null(metrics$unitSpSz) &&
                             prod(dim(metrics$unitSpSz))))

    ## Fin des informations de chargement (demande de confirmation utilisateur) :
    stepInnerProgressBar.f(n=2, msg="Fin de chargement !",
                           font=tkfont.create(weight="bold", size=9), foreground="darkred")

    updateInterface.load.f(baseEnv=baseEnv, tabObs=Data$obs)

    gestionMSGaide.f(namemsg="SelectionOuTraitement", env=baseEnv)

    infoLoading.f(button=TRUE, WinRaise=get("W.main", envir=baseEnv))

    ## [!!!] ajouter réinitialisation des menus si échec  [yr: 14/12/2011]
    ## return(Data)
}


########################################################################################################################
tryCatchLoad.f <- function(expr, baseEnv=.GlobalEnv,...)
{
    ## Purpose: Gestion des exceptions du chargement de données.
    ## ----------------------------------------------------------------------
    ## Arguments: expr : cf. tryCatch()
    ##            ... arguments supplémentaires (excepté error).
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 14 déc. 2011, 18:12

    tryCatch(expr=expr,
             error=function(e)
         {
             infoLoading.f(msg=paste("Il y a eu une erreur lors du chargement des données.",
                                     "\nVérifiez vos jeux de données ou contactez le développeur",
                                     "\nsi le problème persiste.",
                                     sep=""),
                           icon="error")

             infoLoading.f(button=TRUE, WinRaise=get("W.main", envir=baseEnv))

             message(e)
         },...)
}






### Local Variables:
### ispell-local-dictionary: "english"
### fill-column: 120
### End:
