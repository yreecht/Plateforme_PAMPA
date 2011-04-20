################################################################################
## Nom     : critereespref.f
## Objet   : choix d'un Facteur dans le référentiel espèce par l'utilisateur
## Input   : especes
## Output  : champs sélectionné
################################################################################

critereespref.f <- function ()
{
    runLog.f(msg=c("Choix d'un Facteur dans le référentiel espèces :"))

    aa <- tktoplevel()
    tkwm.title(aa, "Selection du facteur du référentiel des espèces")

    ## Ascenceur :
    scr <- tkscrollbar(aa, repeatinterval=5, command=function(...)tkyview(tl, ...))

    tl <- tklistbox(aa, height=20, width=50, selectmode="single",
                    yscrollcommand=function(...)tkset(scr, ...),
                    background="white")

    ## Placement des éléments :
    tkgrid(tklabel(aa, text="Liste des facteurs du référentiel des espèces"))

    tkgrid(tl, scr)
    tkgrid.configure(scr, rowspan=4, sticky="ensw")
    tkgrid.configure(tl, rowspan=4, sticky="ensw")

    ## Réduction aux facteurs contenant de l'information : [yr: 30/09/2010]
    esptmp <- especes[is.element(especes$code_espece, obs$code_espece), ] # sélection des lignes correspondant aux
                                        # obs.
    esptmp <- esptmp[ , sapply(esptmp, function(x){!all(is.na(x))})] # sélection des champs qui contiennent autre
                                        # chose qu'uniquement des NAs.

    facts <- sort(names(esptmp))

    ## ici, on liste les AMP qui ne correspondent pas au jeu de données :
    listeSite <- c("RUN" , "MAY" , "BA" , "BO" , "CB" , "CR" , "STM" , "NC")
    listeSiteExclus <- subset(listeSite, listeSite!=SiteEtudie)

    ## On retire les champs contenant les lettres des sites exclus :
    for (k in (seq(along=listeSiteExclus)))
    { # On peut faire plus simple [yr: 03/08/2010]
        facts <- facts[ ! grepl(paste(listeSiteExclus[k], "$", sep=""),
                                facts)]
    }

    ## Ajout des facteur dans la liste :
    for (i in seq(along=facts))
    {
        tkinsert(tl, "end", facts[i])
    }

    OnOK <- function ()
    {
        factesp <- facts[as.numeric(tkcurselection(tl))+1]
        assign("factesp", factesp, envir=.GlobalEnv)
        tkdestroy(aa)
    }

    OK.but <- tkbutton(aa, text="OK", command=OnOK)

    tkgrid(OK.but, pady=5)

    winSmartPlace.f(aa)
    tkfocus(tl)

    tkwait.window(aa)

    ## [!!!] [inc] On peut faire mieux, notamment pour les annulations.
} # fin critereespref.f

################################################################################
## Nom     : ChoixFacteurSelect.f
## Objet   : choix d'un Facteur de sélection par l'utilisateur
## Input   : table, nom de table, nom de champs, un nombre de sélection ("single" ou "multiple"
## Output  : valeur(s) du champs sélectionné
################################################################################

ChoixFacteurSelect.f <- function (tableselect, monchamp, Nbselectmax, ordre, mavar)
{
    winfac <- tktoplevel(width = 80)
    tkwm.title(winfac, paste("Selection des valeurs de ", monchamp, sep=""))

    scr <- tkscrollbar(winfac, repeatinterval=5, command=function(...)tkyview(tl, ...))

    if (Nbselectmax=="single"||Nbselectmax=="multiple") # [???] à quoi ça sert de faire ça, puisque la fonction ne gère
                                        # pas les erreurs ! [yr: 30/09/2010]
    {
        tl <- tklistbox(winfac, height=20, width=50, selectmode=Nbselectmax,
                        yscrollcommand=function(...)tkset(scr, ...), background="white")
    }
    tkgrid(tklabel(winfac, text=paste("Liste des valeurs de ", monchamp,
                           " presents\n Plusieurs sélections POSSIBLES", sep="")))

    tkgrid(tl, scr)
    tkgrid.configure(scr, rowspan=4, sticky="ensw")
    tkgrid.configure(tl, rowspan=4, sticky="ensw")

    if (ordre==1)
    {
        maliste <- sort(as.character(unique(tableselect)))
    }

    ## On remplit la liste de choix :
    for (i in seq(along=maliste))
    {
        tkinsert(tl, "end", maliste[i])
    }

    OnOK <- function ()
    {
        selectfact <- (maliste[as.numeric(tkcurselection(tl))+1])
        tkdestroy(winfac)
        ## return(selectfact)
        assign(mavar, selectfact, envir=.GlobalEnv)
    }

    OK.but <-tkbutton(winfac, text="OK", command=OnOK)
    tkgrid(OK.but, pady=5)

    tkfocus(winfac)
    winSmartPlace.f(winfac)

    tkwait.window(winfac)

    ## !améliorations possibles
    ## a utiliser si concaténation de monchamp et matable
    ## mavar=paste(matable, "$", monchamp, sep="")
    ## mavar=unique(eval(parse(text=mavar)))
    ## ssi return possible,
    ## as character sinon, la variable retournée est de mode "externalptr"
}


################################################################################
## Nom    : choixunfacteurUnitobs.f()
## Objet  : choix du facteur de groupement des unités d'observations
## Input  : tables "unit" et "unitobs"
## Output : table "unit" + le facteur de la table unitobs choisi
################################################################################

choixunfacteurUnitobs.f <- function ()
{
    runLog.f(msg=c("Choix d'un Facteur dans le référentiel des unités d'observation :"))

    aa <- tktoplevel()
    tkwm.title(aa, "Selection du facteur de groupement des unites d'observation")

    scr <- tkscrollbar(aa, repeatinterval=5,
                       command=function(...)tkyview(tl, ...))

    tl <- tklistbox(aa, height=20, width=50, selectmode="single",
                    yscrollcommand=function(...)tkset(scr, ...),
                    background="white")

    tkgrid(tklabel(aa, text="Liste des facteurs de groupement"))
    tkgrid(tl, scr)
    tkgrid.configure(scr, rowspan=4, sticky="ensw")
    tkgrid.configure(tl, rowspan=4, sticky="ensw")

    ## Réduction aux facteurs contenant de l'information : [yr: 30/09/2010]
    uobstmp <- unitobs[is.element(unitobs$unite_observation, obs$unite_observation), ] # sélection des lignes
                                        # correspondant aux obs.
    uobstmp <- uobstmp[ , sapply(uobstmp, function(x){!all(is.na(x))})] # sélection des champs qui contiennent autre
                                        # chose qu'uniquement des NAs.

    facts <- sort(names(uobstmp))

    ## On remplit la liste de choix :
    for (i in (seq(along=facts)))
    {
        tkinsert(tl, "end", facts[i])
    }

    OnOK <- function ()
    {
        fact <- facts[as.numeric(tkcurselection(tl))+1]
        assign("fact",
               fact, envir=.GlobalEnv)
        tkdestroy(aa)
    }

    OK.but <-tkbutton(aa, text="OK", command=OnOK)

    tkgrid(OK.but, pady=5)

    winSmartPlace.f(aa)
    tkfocus(tl)

    tkwait.window(aa)
} # fin choixunfacteurUnitobs

################################################################################
## Nom     : choixespeces.f
## Objet   : sélection d'un fichier espèces par l'utilisateur
## Input   : fichier "especes" au même format que le référentiel
## Output  : table "especes" modifiée
################################################################################

choixespeces.f <- function()
{
    runLog.f(msg=c("Chargement d'une liste d'espèces à conserver (fichier) :"))

    ## fenetre de chargement du fichier des especes à analyser
    nameFichierEspecesAnalyser <- tclvalue(tkgetOpenFile())
    if (!nchar(nameFichierEspecesAnalyser))
    {
        tkmessageBox(message="Aucun fichier n'a ete selectionne!")
    }else{
        assign("fileNameRefEsp", nameFichierEspecesAnalyser, envir=.GlobalEnv)
        lectureFichierEspeces.f()
    }

    ## filtre de la table observations
    obs <- subset(obs, obs$code_espece %in% especes$code_espece)
    obs.genre <- subset(obs, obs$Genre %in% especes$Genre)
    obs.espece <- subset(obs, obs$espece %in% especes$espece)

    assign("obs", obs, envir=.GlobalEnv)

    ## filtre de la table unite d'observations
    listeEspecesUnitobsAnalyse <- unique(obs$unite_observation)
    unitobs <- subset(unitobs, unitobs$unite_observation %in% listeEspecesUnitobsAnalyse)
    assign("unitobs", unitobs, envir=.GlobalEnv)

    ## on refait la table de contingence
    contingence <- tapply(obs$nombre,
                          list(obs$unite_observation, obs$code_espece),
                          sum, na.rm=TRUE)

    contingence[is.na(contingence)] <- 0
    ## Suppression des especes qui ne sont jamais vues
    ## Sinon problemes pour les calculs d'indices de diversité.
    a <- which(apply(contingence, 2, sum, na.rm=TRUE) == 0)

    if (length(a) != 0)
    {
        contingence <- contingence[, -a, drop=FALSE]
    }
    rm(a)

    ## idem
    b <- which(apply(contingence, 1, sum, na.rm=TRUE) == 0)
    if (length(b) != 0)
    {
        contingence <- contingence[-b, , drop=FALSE]
    }
    rm(b)
    assign("contingence", contingence, envir=.GlobalEnv)

    write.csv(contingence, file=paste(nameWorkspace, "/FichiersSortie/ContingenceUnitObsEspeces.csv", sep=""))

    ## on recrée les tables de base
    creationTablesBase.f()
    Jeuxdonnescoupe <- 1

    tkmessageBox(message=paste("ATTENTION, les tables 'Observations' et 'Unites observations'",
                               " ont ete reduites aux especes selectionnees .", sep=""),
                 icon="warning", type="ok")
} # fin choixespeces.f()



################################################################################
## Nom    : affichageMetriques.f
## Objet  : affichage des métriques analysables par ANOVA
## Input  : table "unit"
## Output : liste de métriques différentes de 0 ou NA
################################################################################

## ! ce fichier est à améliorer afin de sélectionner les paramètres les plus pertinents en premier,
## ! et de ne pas laisser la possibilité de "choix impossibles"

affichageMetriques.f <- function ()
{
    bb <- tktoplevel(width = 80)
    tkwm.title(bb, "Selection de la metrique à analyser")

    scr <- tkscrollbar(bb, repeatinterval=5,
                       command=function(...)tkyview(tl, ...))

    tl <- tklistbox(bb, height=20, width=30, selectmode="single",
                    yscrollcommand=function(...)tkset(scr, ...),
                    background="white")

    tkgrid(tklabel(bb, text="Liste des metriques"))

    tkgrid(tl, scr)
    tkgrid.configure(scr, rowspan=4, sticky="nsw")

    met <- sort(names(unit[2:9]))

    ## création de la liste des métriques différentes de 0 ou NA
    listeMetriquesOK <-"pas de metrique"

    j <- 1
    for (i in seq(along=met))
    {
        if (sum(unit[, met[i]], na.rm=TRUE) != 0) # ((sum(unit[, met[i]], na.rm=TRUE)==0)==FALSE) [!!!]
        {
            listeMetriquesOK[j] <- met[i]
            j <- j+1
        }
    }

    for (i in seq(along=met))
    {
        tkinsert(tl, "end", listeMetriquesOK[i])
    }

    tkselection.set(tl, 0)

    OnOK <- function ()
    {
        me <- listeMetriquesOK[as.numeric(tkcurselection(tl))+1]
        assign("me", me, envir=.GlobalEnv)
        tkdestroy(bb)
    }

    OK.but <-tkbutton(bb, text="OK", command=OnOK)
    tkgrid(OK.but, pady=5)

    tkfocus(bb)
    winSmartPlace.f(bb)

    tkwait.window(bb)
} # fin affichageMetriques



################################################################################
## Nom    : UnCritereEspDansObs.f
## Objet  : Restreindre le fichier obs à uniquement un critere du referentiel spécifique
## Input  : table "obs"
## Output : table obs pour une valeur d'un champs du referentiel spécifique
################################################################################

UnCritereEspDansObs.f <- function ()
{
    runLog.f(msg=c("Sélection sur un critère du référentiel espèces :"))

    critereespref.f()

    if (length(factesp) == 0 || is.null(factesp))
    {
        selectfactesp <- NULL
    }else{
        obs[, factesp] <- especes[, factesp][match(obs$code_espece, especes$code_espece)]

        levelsTmp <- levels(obs$code_espece)

        selectfactesp <- selectModWindow.f(factesp, obs, selectmode="extended")
        assign("selectfactesp", selectfactesp, envir=.GlobalEnv)
    }

    if (!is.null(selectfactesp))
    {
        obs <- dropLevels.f(subset(obs, is.element(obs[, factesp], selectfactesp)), which="code_espece")

        ## Réintégration des niveaux sélectionnés mais plus présents dans les données :
        levelsTmp <- levelsTmp[is.element(levelsTmp,
                                          especes$code_espece[is.element(especes[ , factesp],
                                                                         selectfactesp)])]

        obs$code_espece <- factor(obs$code_espece, levels=levelsTmp)

        gestionMSGaide.f("etapeselected")

        ## On définit globalement que l'on travaille sur une sélection :
        assign("Jeuxdonnescoupe", 1, envir=.GlobalEnv)

        return(list(facteur=factesp,
                    selection=selectfactesp,
                    obs=obs))
    }
}



################################################################################
## Nom    : UnCritereUnitobsDansObs.f
## Objet  : Restreindre le fichier obs à uniquement un critere du referentiel spécifique
## Input  : table "obs"
## Output : table obs pour une valeur d'un champs du referentiel spécifique
################################################################################

UnCritereUnitobsDansObs.f <- function ()
{
    runLog.f(msg=c("Sélection sur un critère du référentiel des unités d'observation :"))

    choixunfacteurUnitobs.f()
    factunitobs <- fact

    if (length(factunitobs) == 0 || is.null(factunitobs))
    {
        selectfactunitobs <- NULL
    }else{
        obs[, factunitobs] <- unitobs[, factunitobs][match(obs$unite_observation, unitobs$unite_observation)]

        levelsTmp <- levels(obs$unite_observation)

        selectfactunitobs <- selectModWindow.f(factunitobs, obs, selectmode="extended")
        assign("selectfactunitobs", selectfactunitobs, envir=.GlobalEnv)
    }

    if (!is.null(selectfactunitobs))
    {
        obs <- dropLevels.f(subset(obs, is.element(obs[, factunitobs], selectfactunitobs)),
                            which="unite_observation") # Vérifier si c'est correct [!!!]

        ## Réintégration des niveaux sélectionnés mais plus présents dans les données :
        levelsTmp <- levelsTmp[is.element(levelsTmp,
                                          unitobs$unite_observation[is.element(unitobs[ , factunitobs],
                                                                               selectfactunitobs)])]

        obs$unite_observation <- factor(obs$unite_observation, levels=levelsTmp)

        gestionMSGaide.f("etapeselected")

        ## On définit globalement que l'on travaille sur une sélection :
        assign("Jeuxdonnescoupe", 1, envir=.GlobalEnv)

        return(list(facteur=factunitobs,
                    selection=selectfactunitobs,
                    obs=obs))
    }
}
