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
    scr <- tkscrollbar(aa, repeatinterval=5, command=function(...)tkyview(tl, ...))
    tl <- tklistbox(aa, height=20, width=50, selectmode="single",
                    yscrollcommand=function(...)tkset(scr, ...), background="white")

    tkgrid(tklabel(aa, text="Liste des facteurs du référentiel des espèces"))
    tkgrid(tl, scr)
    tkgrid.configure(scr, rowspan=4, sticky="nsw")

    ## Réduction aux facteurs contenant de l'information : [yr: 30/09/2010]
    esptmp <- especes[is.element(especes$code_espece, obs$code_espece), ] # sélection des lignes correspondant aux
                                        # obs.
    esptmp <- esptmp[ , sapply(esptmp, function(x){!all(is.na(x))})] # sélection des champs qui contiennent autre
                                        # chose qu'uniquement des NAs.

    facts <- sort(names(esptmp))

    ## ici, on liste les AMP qui ne correspondent pas au jeu de données :
    listeSite <- c("RUN" , "MAY" , "BA" , "BO" , "CB" , "CR" , "STM" , "NC")
    listeSiteExclus <- subset(listeSite, listeSite!=SiteEtudie)
    ## grep(pattern=paste("^", SiteEtudie, "$", sep=""), x=listeSite, value=TRUE, invert=TRUE)
    message("les sites exclus sont :")
    message(listeSiteExclus)
    ## on retire les champs contenant les lettres des sites exclus
    for (k in (seq(along=listeSiteExclus)))
    { # On peut faire plus simple [yr: 03/08/2010]
        message(listeSiteExclus[k])
        facts <- facts[ ! grepl(listeSiteExclus[k], facts)] # ajouter que le motif doit être en fin de chaîne
                                        # [yr: 03/08/2010]
        ## message(facts)
    }

    ## a <- length(facts)                 # écriture inutile [yr: 26/07/2010]
    for (i in seq(along=facts))         # remplace 1:a [yr: 30/09/2010]
    {
        tkinsert(tl, "end", facts[i])
    }
    ## tkselection.set(tl, 0)

    OnOK <- function ()
    {
        factesp <- facts[as.numeric(tkcurselection(tl))+1]
        assign("factesp", factesp, envir=.GlobalEnv)
        tkdestroy(aa)
    }
    OK.but <-tkbutton(aa, text="OK", command=OnOK)
    tkgrid(OK.but)
    tkfocus(aa)
    winSmartPlace.f(aa)

    tkwait.window(aa)
    ## rm(a)
} # fin critereespref.f

################################################################################
## Nom     : ChoixFacteurSelect.f
## Objet   : choix d'un Facteur de sélection par l'utilisateur
## Input   : table, nom de table, nom de champs, un nombre de sélection ("single" ou "multiple"
## Output  : valeur(s) du champs sélectionné
################################################################################

ChoixFacteurSelect.f <- function (tableselect, monchamp, Nbselectmax, ordre, mavar)
{

    ## message("fonction ChoixFacteurSelect activée")

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
                           " presents\n Plusieurs sélections POSSIBLES\n\nATTENTION :",
                           " première valeur sélectionnée par défaut", sep="")))
    tkgrid(tl, scr)
    tkgrid.configure(scr, rowspan=4, sticky="nsw")

    if (ordre==1)
    {
        maliste <- sort(as.character(unique(tableselect)))
    }

    a <- length(maliste)

    for (i in (1:a))
    {
        tkinsert(tl, "end", maliste[i])
    }
    ## tkselection.set(tl, 0)


    OnOK <- function ()
    {
        selectfact <- (maliste[as.numeric(tkcurselection(tl))+1])
        tkdestroy(winfac)
        ## return(selectfact)
        assign(mavar, selectfact, envir=.GlobalEnv)
    }
    OK.but <-tkbutton(winfac, text="OK", command=OnOK)
    tkgrid(OK.but)
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
    scr <- tkscrollbar(aa, repeatinterval=5, command=function(...)tkyview(tl, ...))
    tl <- tklistbox(aa, height=20, selectmode="single",
                    yscrollcommand=function(...)tkset(scr, ...), background="white")

    tkgrid(tklabel(aa, text="Liste des facteurs de groupement"))
    tkgrid(tl, scr)
    tkgrid.configure(scr, rowspan=4, sticky="nsw")

    ## Réduction aux facteurs contenant de l'information : [yr: 30/09/2010]
    uobstmp <- unitobs[is.element(unitobs$unite_observation, obs$unite_observation), ] # sélection des lignes
                                        # correspondant aux obs.
    uobstmp <- uobstmp[ , sapply(uobstmp, function(x){!all(is.na(x))})] # sélection des champs qui contiennent autre
                                        # chose qu'uniquement des NAs.

    facts <- sort(names(uobstmp))

    for (i in (seq(along=facts)))
    {
        tkinsert(tl, "end", facts[i])
    }
    ## tkselection.set(tl, 0)

    OnOK <- function ()
    {
        fact <- facts[as.numeric(tkcurselection(tl))+1]
        assign("fact", fact, envir=.GlobalEnv)
        tkdestroy(aa)
        unit[, fact] <- unitobs[, fact][match(unit$unitobs, unitobs$unite_observation)] # [???] unitobs ou uobstmp ?
                                        # [!!!]
        assign("unit", unit, envir=.GlobalEnv)
    }
    OK.but <-tkbutton(aa, text="OK", command=OnOK)
    tkgrid(OK.but)
    tkfocus(aa)
    winSmartPlace.f(aa)

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
    ## Sinon problemes pour les calculs d'indices de diversite.
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

    message("Table de contingence unités d'observation/espèces créée : ContingenceUnitObsEspeces.csv")
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
    scr <- tkscrollbar(bb, repeatinterval=5, command=function(...)tkyview(tl, ...))
    tl <- tklistbox(bb, height=20, width=30, selectmode="single",
                    yscrollcommand=function(...)tkset(scr, ...), background="white")

    tkgrid(tklabel(bb, text="Liste des metriques"))
    tkgrid(tl, scr)
    tkgrid.configure(scr, rowspan=4, sticky="nsw")
    met <- sort(names(unit[2:9]))
    a <- length(met)
    ## création de la liste des métriques différentes de 0 ou NA
    listeMetriquesOK <-"pas de metrique"

    j <- 1
    for (i in (1:a))
    {
        if (sum(unit[, met[i]], na.rm=TRUE) != 0) # ((sum(unit[, met[i]], na.rm=TRUE)==0)==FALSE) [!!!]
        {
            listeMetriquesOK[j] <- met[i]
            j <- j+1
        }
    }

    b <- length(listeMetriquesOK)
    for (i in (1:b))
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
    tkgrid(OK.but)
    tkfocus(bb)
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
    obs[, factesp] <- especes[, factesp][match(obs$code_espece, especes$code_espece)]

    levelsTmp <- levels(obs$code_espece)

    ## message(head(obs))
    ## ChoixFacteurSelect.f(tableselect=obs[, factesp], monchamp=factesp,
    ##                      Nbselectmax="multiple", ordre=1, mavar="selectfactesp")
    selectfactesp <- selectModWindow.f(factesp, obs, selectmode="extended")
    assign("selectfactesp", selectfactesp, envir=.GlobalEnv)
    ## message(selectfactesp)

    obs <- dropLevels.f(subset(obs, is.element(obs[, factesp], selectfactesp)), which="code_espece")

    ## Réintégration des niveaux sélectionnés mais plus présents dans les données :
    levels(obs$code_espece) <- levelsTmp[is.element(levelsTmp,
                                                    especes$code_espece[is.element(especes[ , factesp],
                                                                                   selectfactesp)])]

    gestionMSGaide.f("etapeselected")
    ## Jeuxdonnescoupe <- 1
    assign("Jeuxdonnescoupe", 1, envir=.GlobalEnv)
    return(list(facteur=factesp,
                selection=selectfactesp,
                obs=obs))
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
    obs[, factunitobs] <- unitobs[, factunitobs][match(obs$unite_observation, unitobs$unite_observation)]

    levelsTmp <- levels(obs$unite_observation)
    ## message(head(obs))
    ## ChoixFacteurSelect.f(obs[, factunitobs], factunitobs, "multiple", 1, "selectfactunitobs")

    selectfactunitobs <- selectModWindow.f(factunitobs, obs, selectmode="extended")
    assign("selectfactunitobs", selectfactunitobs, envir=.GlobalEnv)
    ## message(selectfactunitobs)

    obs <- dropLevels.f(subset(obs, is.element(obs[, factunitobs], selectfactunitobs)),
                        which="unite_observation") # Vérifier si c'est correct [!!!]

    ## Réintégration des niveaux sélectionnés mais plus présents dans les données :
    levels(obs$unite_observation) <-
        levelsTmp[is.element(levelsTmp,
                             unitobs$unite_observation[is.element(unitobs[ , factunitobs],
                                                                  selectfactunitobs)])]

    gestionMSGaide.f("etapeselected")
    assign("Jeuxdonnescoupe", 1, envir=.GlobalEnv)
    return(list(facteur=factunitobs,
                selection=selectfactunitobs,
                obs=obs))
}
