################################################################################
## Nom     : critereespref.f
## Objet   : choix d'un Facteur dans le référentiel espèce par l'utilisateur
## Input   : especes
## Output  : champs sélectionné
################################################################################

critereespref.f <- function ()
{

    print("fonction critereespref.f activée")
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
    print("les sites exclus sont :")
    print(listeSiteExclus)
    ## on retire les champs contenant les lettres des sites exclus
    for (k in (seq(along=listeSiteExclus)))
    { # On peut faire plus simple [yr: 03/08/2010]
        print(listeSiteExclus[k])
        facts <- facts[ ! grepl(listeSiteExclus[k], facts)] # ajouter que le motif doit être en fin de chaîne
                                        # [yr: 03/08/2010]
        ## print(facts)
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
## Nom     : critere2espref.f
## Objet   : choix d'un Facteur dans le référentiel espèce par l'utilisateur
## Input   : especes
## Output  : champs sélectionné
################################################################################

critere2espref.f <- function ()
{

    print("fonction critere2espref activée")
    aa <- tktoplevel()
    tkwm.title(aa, "Selection du facteur du référentiel des espèces")
    scr <- tkscrollbar(aa, repeatinterval=5, command=function(...)tkyview(tl, ...))
    tl <- tklistbox(aa, height=20, width=50, selectmode="single",
                    yscrollcommand=function(...)tkset(scr, ...), background="white")

    tkgrid(tklabel(aa, text="Liste des facteurs du référentiel des espèces"))
    tkgrid(tl, scr)
    tkgrid.configure(scr, rowspan=4, sticky="nsw")
    facts <- sort(names(especes))
    ## ici, on liste les AMP qui ne correspondent pas au jeu de données
    listeSite <- c("RUN" , "MAY" , "BA" , "BO" , "CB" , "CR" , "STM" , "NC")
    listeSiteExclus <- subset(listeSite, listeSite!=SiteEtudie)
    print("les sites exclus sont :")
    print(listeSiteExclus)
    ## on retire les champs contenant les lettres des sites exclus
    for (k in (1:length(listeSiteExclus)))
    {
        ## print(listeSiteExclus[k])
        facts <- facts[-grep(listeSiteExclus[k], facts)]
        ## print(facts)
    }
    a <- length(facts)
    for (i in (1:a))
    {
        tkinsert(tl, "end", facts[i])
    }
    tkselection.set(tl, 0)

    OnOK <- function ()
    {
        factesp2 <- facts[as.numeric(tkcurselection(tl))+1]
        assign("factesp2", factesp2, envir=.GlobalEnv)
        tkdestroy(aa)
    }
    OK.but <-tkbutton(aa, text="OK", command=OnOK)
    tkgrid(OK.but)
    tkfocus(aa)
    tkwait.window(aa)
    rm(a)
} # fin critereespref.f


################################################################################
## Nom     : ChoixFacteurSelect.f
## Objet   : choix d'un Facteur de sélection par l'utilisateur
## Input   : table, nom de table, nom de champs, un nombre de sélection ("single" ou "multiple"
## Output  : valeur(s) du champs sélectionné
################################################################################

ChoixFacteurSelect.f <- function (tableselect, monchamp, Nbselectmax, ordre, mavar)
{

    ## print("fonction ChoixFacteurSelect activée")

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
## Nom     : choixchamptable.f
## Objet   : choix d'un Champ de matable à représenter
## Input   : matable
## Output  : champs sélectionné
################################################################################

choixchamptable.f <- function (matable)
{

    print("fonction choixchamptable activée")
    print(matable)
    aa <- tktoplevel()
    tkwm.title(aa, paste("Métrique de", matable, "à représenter"))
    scr <- tkscrollbar(aa, repeatinterval=5, command=function(...)tkyview(tl, ...))
    tl <- tklistbox(aa, height=20, width=15, selectmode="single",
                    yscrollcommand=function(...)tkset(scr, ...), background="white")

    tkgrid(tklabel(aa, text=paste("Métrique à représenter de ", matable)))
    tkgrid(tklabel(aa, text="ATTENTION, selectionnez\n uniquement des champs numériques (***)"))
    tkgrid(tl, scr)
    tkgrid.configure(scr, rowspan=4, sticky="nsw")
    objtable <- eval(parse(text=matable))

    facts <- sort(names(objtable)[names(objtable)!="an"])
    a <- length(facts)
    for (i in (1:a))
    {
        if (is.numeric(objtable[, facts[i]]))  #on signale les champs non numériques de la liste
        {
            tkinsert(tl, "end", paste(" *** ", facts[i]))
        }else{
            tkinsert(tl, "end", paste("     ", facts[i]))}
    }
    tkselection.set(tl, 0)

    OnOK <- function ()
    {
        champtrouve <- facts[as.numeric(tkcurselection(tl))+1]
        assign("champtrouve", champtrouve, envir=.GlobalEnv)
        tkdestroy(aa)
    }
    OK.but <-tkbutton(aa, text="OK", command=OnOK)
    tkgrid(OK.but)
    tkfocus(aa)
    tkwait.window(aa)
    rm(a)
} # fin choixchamptable.f



################################################################################
## Nom    : choixunfacteurUnitobs.f()
## Objet  : choix du facteur de groupement des unités d'observations
## Input  : tables "unit" et "unitobs"
## Output : table "unit" + le facteur de la table unitobs choisi
################################################################################

choixunfacteurUnitobs.f <- function ()
{

    print("fonction choixunfacteurUnitobs.f activée")
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
## Nom     : ChoixUneEspece.f
## Objet   : choix d'une espèce par l'utilisateur
## Input   : liste des espèces présentes dans la table "unitesp"
## Output  : codes espèce sélectionné sp
################################################################################

ChoixUneEspece.f <- function ()
{

    print("fonction ChoixUneEspece activée")
    ee <- tktoplevel(width = 80)
    tkwm.title(ee, "Selection d'une espece")
    scr <- tkscrollbar(ee, repeatinterval=5, command=function(...)tkyview(tl, ...))
    tl <- tklistbox(ee, height=20, width=30, selectmode="single",
                    yscrollcommand=function(...)tkset(scr, ...), background="white")

    tkgrid(tklabel(ee, text="Liste des especes presentes"))
    tkgrid(tl, scr)
    tkgrid.configure(scr, rowspan=4, sticky="nsw")
    ## liste des codes espèces
    especesPresentes <- subset(unitesp, unitesp$pres_abs==1)$code_espece
    spe <- sort(as.character(unique(especesPresentes)))

    ## liste des identifiants à afficher
    identSp <- as.character(especes$Identifiant[match(spe, especes$code_espece)])
    ## affichage des identifiants
    a <- length(identSp)
    for (i in (1:a))
    {
        tkinsert(tl, "end", identSp[i])
    }
    tkselection.set(tl, 0)

    OnOK <- function ()
    {
        sp <- spe[as.numeric(tkcurselection(tl))+1]
        print(sp)
        assign("sp", sp, envir=.GlobalEnv)
        tkdestroy(ee)
    }
    OK.but <-tkbutton(ee, text="OK", command=OnOK)
    tkgrid(OK.but)
    tkfocus(ee)
    tkwait.window(ee)
}


################################################################################
## Nom     : ChoixDesEspeces.f
## Objet   : choix des espèces par l'utilisateur
## Input   : liste des espèces présentes dans la table "unitesp"
## Output  : codes espèce sélectionné sp
################################################################################

ChoixDesEspeces.f <- function ()
{

    print("fonction ChoixDesEspeces activée")
    ee <- tktoplevel(width = 80)
    tkwm.title(ee, "Selection des espèces")
    scr <- tkscrollbar(ee, repeatinterval=5, command=function(...)tkyview(tl, ...))
    tl <- tklistbox(ee, height=20, width=30, selectmode="multiple",
                    yscrollcommand=function(...)tkset(scr, ...), background="white")

    tkgrid(tklabel(ee, text="Liste des espèces presentes"))
    tkgrid(tl, scr)
    tkgrid.configure(scr, rowspan=4, sticky="nsw")
    ## liste des codes espèces
    especesPresentes <- subset(unitesp, unitesp$pres_abs==1)$code_espece
    spe <- sort(as.character(unique(especesPresentes)))

    ## liste des identifiants à afficher
    identSp <- as.character(especes$Identifiant[match(spe, especes$code_espece)])
    ## affichage des identifiants
    a <- length(identSp)
    for (i in (1:a))
    {
        tkinsert(tl, "end", identSp[i])
    }
    tkselection.set(tl, 0)

    OnOK <- function ()
    {
        sp <- spe[as.numeric(tkcurselection(tl))+1]
        print(sp)
        assign("sp", sp, envir=.GlobalEnv)
        tkdestroy(ee)
    }
    OK.but <-tkbutton(ee, text="OK", command=OnOK)
    tkgrid(OK.but)
    tkfocus(ee)
    tkwait.window(ee)
}

################################################################################
## Nom     : ChoixUneFamille.f
## Objet   : choix d'une famille par l'utilisateur
## Input   : liste des familles présentes dans la table "especes"
## Output  : codes famille sélectionnée fa
################################################################################

ChoixUneFamille.f <- function ()
{
    print("fonction ChoixUneFamille activée")

    ef <- tktoplevel(width = 80)
    tkwm.title(ef, "Selection d'une Famille")
    scr <- tkscrollbar(ef, repeatinterval=5, command=function(...)tkyview(tl, ...))
    tl <- tklistbox(ef, height=20, width=30, selectmode="single",
                    yscrollcommand=function(...)tkset(scr, ...), background="white")

    tkgrid(tklabel(ef, text="Liste des familles presentes"))
    tkgrid(tl, scr)
    tkgrid.configure(scr, rowspan=4, sticky="nsw")
    ## liste des familles
    especesPresentes <- subset(unitesp, unitesp$pres_abs==1)$code_espece
    famillesPresentes <- especes$Famille[match(especesPresentes, especes$code_espece)]
    listeFamille <- sort(as.character(unique(famillesPresentes)))
    ## affichage des familles
    a <- length(listeFamille)
    for (i in (1:a))
    {
        tkinsert(tl, "end", listeFamille[i])
    }
    tkselection.set(tl, 0)

    OnOK <- function ()
    {
        fa <- listeFamille[as.numeric(tkcurselection(tl))+1]
        ## print(ListeEspFamilleSelectionnee)
        assign("fa", fa, envir=.GlobalEnv)
        tkdestroy(ef)
    }
    OK.but <-tkbutton(ef, text="OK", command=OnOK)
    tkgrid(OK.but)
    tkfocus(ef)
    tkwait.window(ef)

}

################################################################################
## Nom     : ChoixUnPhylum.f
## Objet   : choix d'un phylum par l'utilisateur
## Input   : liste des phylums présentes dans la table "especes"
## Output  : codes phylums sélectionnée phy
################################################################################

ChoixUnPhylum.f <- function ()
{
    print("fonction ChoixUnPhylum.f activée")

    ef <- tktoplevel(width = 80)
    tkwm.title(ef, "Selection d'un Phylum")
    scr <- tkscrollbar(ef, repeatinterval=5, command=function(...)tkyview(tl, ...))
    tl <- tklistbox(ef, height=20, width=30, selectmode="single",
                    yscrollcommand=function(...)tkset(scr, ...), background="white")

    tkgrid(tklabel(ef, text="Liste des phylums presents"))
    tkgrid(tl, scr)
    tkgrid.configure(scr, rowspan=4, sticky="nsw")
    ## liste des phylums
    especesPresentes <- subset(unitesp, unitesp$pres_abs==1)$code_espece
    phylumsPresents <- especes$Phylum[match(especesPresentes, especes$code_espece)]
    listePhylums <- sort(as.character(unique(phylumsPresents)))
    ## affichage des familles
    a <- length(listePhylums)
    for (i in (1:a))
    {
        tkinsert(tl, "end", listePhylums[i])
    }
    tkselection.set(tl, 0)

    OnOK <- function ()
    {
        phy <- listePhylums[as.numeric(tkcurselection(tl))+1]
        ## print(ListeEspFamilleSelectionnee)
        assign("phy", phy, envir=.GlobalEnv)
        tkdestroy(ef)
    }
    OK.but <-tkbutton(ef, text="OK", command=OnOK)
    tkgrid(OK.but)
    tkfocus(ef)
    tkwait.window(ef)
}

################################################################################
## Nom     : ChoixUnOrdre.f
## Objet   : choix d'un ordre par l'utilisateur
## Input   : liste des ordres présentes dans la table "especes"
## Output  : codes ordres sélectionnée phy
################################################################################

ChoixUnOrdre.f <- function ()
{
    print("fonction ChoixUnOrdre.f activée")

    ef <- tktoplevel(width = 80)
    tkwm.title(ef, "Selection d'un Ordre")
    scr <- tkscrollbar(ef, repeatinterval=5, command=function(...)tkyview(tl, ...))
    tl <- tklistbox(ef, height=20, width=30, selectmode="single",
                    yscrollcommand=function(...)tkset(scr, ...), background="white")

    tkgrid(tklabel(ef, text="Liste des ordres presents"))
    tkgrid(tl, scr)
    tkgrid.configure(scr, rowspan=4, sticky="nsw")
    ## liste des ordres
    especesPresentes <- subset(unitesp, unitesp$pres_abs==1)$code_espece
    ordresPresents <- especes$Ordre[match(especesPresentes, especes$code_espece)]
    listeOrdres <- sort(as.character(unique(ordresPresents)))
    ## affichage des familles
    a <- length(listeOrdres)
    for (i in (1:a))
    {
        tkinsert(tl, "end", listeOrdres[i])
    }
    tkselection.set(tl, 0)

    OnOK <- function ()
    {
        ord <- listeOrdres[as.numeric(tkcurselection(tl))+1]
        ## print(ListeEspFamilleSelectionnee)
        assign("ord", ord, envir=.GlobalEnv)
        tkdestroy(ef)
    }
    OK.but <-tkbutton(ef, text="OK", command=OnOK)
    tkgrid(OK.but)
    tkfocus(ef)
    tkwait.window(ef)
}

ChoixUneClasse.f <- function ()
{
    print("fonction ChoixUneClasse.f activée")

    ef <- tktoplevel(width = 80)
    tkwm.title(ef, "Selection d'une Classe")
    scr <- tkscrollbar(ef, repeatinterval=5, command=function(...)tkyview(tl, ...))
    tl <- tklistbox(ef, height=20, width=30, selectmode="single",
                    yscrollcommand=function(...)tkset(scr, ...), background="white")

    tkgrid(tklabel(ef, text="Liste des classes presents"))
    tkgrid(tl, scr)
    tkgrid.configure(scr, rowspan=4, sticky="nsw")
    ## liste des ordres
    especesPresentes <- subset(unitesp, unitesp$pres_abs==1)$code_espece
    classesPresents <- especes$Classe[match(especesPresentes, especes$code_espece)]
    listeClasses <- sort(as.character(unique(classesPresents)))
    ## affichage des familles
    a <- length(listeClasses)
    for (i in (1:a))
    {
        tkinsert(tl, "end", listeClasses[i])
    }
    tkselection.set(tl, 0)

    OnOK <- function ()
    {
        cla <- listeClasses[as.numeric(tkcurselection(tl))+1]
        ## print(ListeEspFamilleSelectionnee)
        assign("cla", cla, envir=.GlobalEnv)
        tkdestroy(ef)
    }
    OK.but <-tkbutton(ef, text="OK", command=OnOK)
    tkgrid(OK.but)
    tkfocus(ef)
    tkwait.window(ef)
}

ChoixUneUnitobs.f <- function ()
{

    print("fonction ChoixUneUnitobs activée")
    eu <- tktoplevel(width = 80)
    tkwm.title(eu, "Selection d'une Unité d'observation")
    scr <- tkscrollbar(eu, repeatinterval=5, command=function(...)tkyview(tl, ...))
    tl <- tklistbox(eu, height=20, width=30, selectmode="single",
                    yscrollcommand=function(...)tkset(scr, ...), background="white")

    tkgrid(tklabel(eu, text="Liste des Unités d'observations presentes"))
    tkgrid(tl, scr)
    tkgrid.configure(scr, rowspan=4, sticky="nsw")
    ## affichage de la liste des unités d'observations
    maliste<- sort(as.character(unique(unitobs$unite_observation)))
    a <- length(unitobs$unite_observation)
    for (i in (1:a))
    {
        tkinsert(tl, "end", paste(unitobs$unite_observation[i], " ", unitobs$site[i]))
    }
    tkselection.set(tl, 0)

    OnOK <- function ()
    {
        uo <- maliste[as.numeric(tkcurselection(tl))+1]
        si <- (unitobs$site[as.numeric(tkcurselection(tl))+1])
        print(si)
        assign("uo", uo, envir=.GlobalEnv)
        assign("si", si, envir=.GlobalEnv)
        tkdestroy(eu)
    }
    OK.but <-tkbutton(eu, text="OK", command=OnOK)
    tkgrid(OK.but)
    tkfocus(eu)
    tkwait.window(eu)
}

## les fonctions ci dessous sont à factoriser
ChoixUnSite.f <- function ()
{

    print("fonction ChoixUnSite activée")
    eu <- tktoplevel(width = 80)
    tkwm.title(eu, "Selection d'un site d'observation")
    scr <- tkscrollbar(eu, repeatinterval=5, command=function(...)tkyview(tl, ...))
    tl <- tklistbox(eu, height=20, width=30, selectmode="single",
                    yscrollcommand=function(...)tkset(scr, ...), background="white")

    tkgrid(tklabel(eu, text="Liste des Sites presents"))
    tkgrid(tl, scr)
    tkgrid.configure(scr, rowspan=4, sticky="nsw")
    ## affichage de la liste des unités d'observations
    maliste<- as.character(unique(unitobs$site))
    a <- length(maliste)
    for (i in (1:a))
    {
        tkinsert(tl, "end", maliste[i])
    }
    tkselection.set(tl, 0)

    OnOK <- function ()
    {
        si <- (maliste[as.numeric(tkcurselection(tl))+1])
        print(si)
        assign("si", si, envir=.GlobalEnv)
        tkdestroy(eu)
    }
    OK.but <-tkbutton(eu, text="OK", command=OnOK)
    tkgrid(OK.but)
    tkfocus(eu)
    tkwait.window(eu)
}

ChoixUneAnnee.f <- function ()
{
    print("fonction ChoixUneAnnee activée")

    eu <- tktoplevel(width = 80)
    tkwm.title(eu, "Selection d'une année")
    scr <- tkscrollbar(eu, repeatinterval=5, command=function(...)tkyview(tl, ...))
    tl <- tklistbox(eu, height=20, width=30, selectmode="single",
                    yscrollcommand=function(...)tkset(scr, ...), background="white")

    tkgrid(tklabel(eu, text="Liste des années"))
    tkgrid(tl, scr)
    tkgrid.configure(scr, rowspan=4, sticky="nsw")
    ## affichage de la liste des années
    maliste<- unique(unitobs$an[order(unitobs$an, decreasing=FALSE)])
    a <- length(maliste)
    for (i in (1:a))
    {
        tkinsert(tl, "end", maliste[i])
    }
    tkselection.set(tl, 0)

    OnOK <- function ()
    {
        varAn <- (maliste[as.numeric(tkcurselection(tl))+1])
        print(varAn)
        assign("varAn", varAn, envir=.GlobalEnv)
        tkdestroy(eu)
    }
    OK.but <-tkbutton(eu, text="OK", command=OnOK)
    tkgrid(OK.but)
    tkfocus(eu)
    tkwait.window(eu)
}

ChoixUnhabitat1.f <- function ()
{

    print("fonction ChoixUnhabitat1 activée")

    eu <- tktoplevel(width = 80)
    tkwm.title(eu, "Selection d'un habitat")
    scr <- tkscrollbar(eu, repeatinterval=5, command=function(...)tkyview(tl, ...))
    tl <- tklistbox(eu, height=20, width=30, selectmode="single",
                    yscrollcommand=function(...)tkset(scr, ...), background="white")

    tkgrid(tklabel(eu, text="Liste des habitats presents"))
    tkgrid(tl, scr)
    tkgrid.configure(scr, rowspan=4, sticky="nsw")
    ## affichage de la liste des unités d'observations
    maliste<- as.character(unique(unitobs$habitat1))
    a <- length(maliste)
    for (i in (1:a))
    {
        tkinsert(tl, "end", maliste[i])
    }
    tkselection.set(tl, 0)

    OnOK <- function ()
    {
        ha <- (maliste[as.numeric(tkcurselection(tl))+1])
        print(ha)
        assign("ha", ha, envir=.GlobalEnv)
        tkdestroy(eu)
    }
    OK.but <-tkbutton(eu, text="OK", command=OnOK)
    tkgrid(OK.but)
    tkfocus(eu)
    tkwait.window(eu)
}

ChoixUnbiotope.f <- function ()
{
    print("fonction ChoixUnbiotope activée")

    eu <- tktoplevel(width = 80)
    tkwm.title(eu, "Selection d'un habitat")
    scr <- tkscrollbar(eu, repeatinterval=5, command=function(...)tkyview(tl, ...))
    tl <- tklistbox(eu, height=20, width=30, selectmode="single",
                    yscrollcommand=function(...)tkset(scr, ...), background="white")

    tkgrid(tklabel(eu, text="Liste des habitats presents"))
    tkgrid(tl, scr)
    tkgrid.configure(scr, rowspan=4, sticky="nsw")
    ## affichage de la liste des unités d'observations
    maliste<- as.character(unique(unitobs$biotope))
    a <- length(maliste)
    for (i in (1:a))
    {
        tkinsert(tl, "end", maliste[i])
    }
    tkselection.set(tl, 0)

    OnOK <- function ()
    {
        bio <- (maliste[as.numeric(tkcurselection(tl))+1])
        print(bio)
        assign("bio", bio, envir=.GlobalEnv)
        tkdestroy(eu)
    }
    OK.but <-tkbutton(eu, text="OK", command=OnOK)
    tkgrid(OK.but)
    tkfocus(eu)
    tkwait.window(eu)
}

## [sup] [yr:12/01/2011]:

## selectionEspeceStatut.f <- function ()
## {
##     print("fonction selectionEspeceStatut activée")
##     ## teste si les tailles sont renseignees dans la table observation
##     ## if (length(unique(obs$taille))>1) {} [inc][???]
## }

################################################################################
## Nom     : choixespeces.f
## Objet   : sélection d'un fichier espèces par l'utilisateur
## Input   : fichier "especes" au même format que le référentiel
## Output  : table "especes" modifiée
################################################################################

choixespeces.f <- function()
{
    print("fonction choixespeces activée")

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
    calcPresAbs.f()

    print("Table de contingence unite d'observations/especes creee : ContingenceUnitObsEspeces.csv")
    write.csv(contingence, file=paste(nameWorkspace, "/FichiersSortie/ContingenceUnitObsEspeces.csv", sep=""))

    ## on recrée les tables de base
    creationTablesBase.f()
    Jeuxdonnescoupe <- 1

    tkmessageBox(message=paste("ATTENTION, les tables 'Observations' et 'Unites observations'",
                               " ont ete reduites aux especes selectionnees .", sep=""),
                 icon="warning", type="ok")
} # fin choixespeces.f()

################################################################################
## Nom    : grpesp.f
## Objet  : sélection du facteur de regroupement des espèces
## Input  : listes des champs du référentiel espèces
## Output : facteur de groupement sélectionné
################################################################################

## [sup] [yr:12/01/2011]:

## grpesp.f <- function ()
## {

##     print("fonction grpesp activée")
##     aa <- tktoplevel()
##     tkwm.title(aa, "Selection du facteur de groupement des especes")
##     scr <- tkscrollbar(aa, repeatinterval=5, command=function(...)tkyview(tl, ...))
##     tl <- tklistbox(aa, height=20, selectmode="single",
##                     yscrollcommand=function(...)tkset(scr, ...), background="white")

##     tkgrid(tklabel(aa, text="Liste des facteurs de groupement"))
##     tkgrid(tl, scr)
##     tkgrid.configure(scr, rowspan=4, sticky="nsw")
##     facts <- sort(names(especes))
##     a <- length(facts)
##     for (i in (1:a))
##     {
##         tkinsert(tl, "end", facts[i])
##     }
##     tkselection.set(tl, 0)

##     OnOK <- function ()
##     {
##         factesp <- facts[as.numeric(tkcurselection(tl))+1]
##         assign("factesp", factesp, envir=.GlobalEnv)
##         tkdestroy(aa)
##         grpespcalc.f(factesp)
##     }
##     OK.but <-tkbutton(aa, text="OK", command=OnOK)
##     tkgrid(OK.but)
##     tkfocus(aa)
##     tkwait.window(aa)
##     rm(a)
## } # fin grpesp.f

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
## Nom    : UnStatutDansObs.f
## Objet  : Restreindre le fichier obs à uniquement un Statut
## Input  : table "obs"
## Output : table obs pour un seul statut
################################################################################

## [sup] [yr:12/01/2011]:

## UnStatutDansObs.f <- function ()
## {

##     print("fonction UnStatutDansObs activée")
##     ## AMPEtudie=unique(unitobs$AMP)
##     if (length(unique(unitobs$AMP))> 1)   gestionMSGinfo.f("plusieursAMP")
##     suffixeAMP <- unique(unitobs$AMP)
##     ## match(obs$code_espece, subset(especes, especes$emblematiqueRUN=="oui")) -> il n'y a pas d'espèces emblématiques
##     ## pour le benthos

##     if (tclvalue(SelectIUCN)!="0")
##     {
##         statutchoisi <- "stat.IUCN"
##         ChampStatutSelect <- paste("especes$", statutchoisi, sep="")
##         obs$statut <- especes$stat.IUCN[match(obs$code_espece, especes$code_espece)]
##     }else{
##         ## [!!!]
##         if (tclvalue(SelectEmble)!="0") statutchoisi="emblematique" #puis griser le widget et le remettre à 0 pour
##                                         #permettre les analyses successives
##         if (tclvalue(SelectEndem)!="0") statutchoisi="endemique"
##         if (tclvalue(SelectMenace)!="0") statutchoisi="etat.pop.local"
##         if (tclvalue(SelectAutreStatut)!="0") statutchoisi="autre.statut"

##         ChampStatutSelect <- paste("obs$statut=especes$", statutchoisi,
##                                    suffixeAMP, "[match(obs$code_espece, especes$code_espece)]", sep="")
##         print(ChampStatutSelect)
##         eval(parse(text=ChampStatutSelect)) #permet d'executer le contenu de la chaîne de caractères

##     }
##     print(paste("Sélection faite sur ", ChampStatutSelect, sep=""))
##     obs <- subset(obs, obs$statut!=NA)
##     ##
##     gestionMSGaide.f("etapeselected")
##     Jeuxdonnescoupe <- 1
##     assign("Jeuxdonnescoupe", Jeuxdonnescoupe, envir=.GlobalEnv)
##     return(obs)

##     ## obs$statut=especes$statut_protection[match(obs$code_espece, especes$code_espece)]
##     ## obs=subset(obs, obs$statut_protection==statutchoisi)
## }
################################################################################
## Nom    : UnBiotopeDansObs.f
## Objet  : Restreindre le fichier obs à uniquement une famille
## Input  : table "obs"
## Output : table obs pour une seule famille
################################################################################

## [sup] [yr: 11/01/2011]

## UnBiotopeDansObs.f <- function ()
## {

##     print("fonction UnBiotopeDansObs activée")
##     ChoixUnbiotope.f()
##     obs$biotope <- unit$biotope[match(obs$unite_observation, unit$unitobs)]
##     obs <- subset(obs, obs$biotope==bio)
##     gestionMSGaide.f("biotopeselected")
##     Jeuxdonnescoupe <- 1
##     assign("Jeuxdonnescoupe", Jeuxdonnescoupe, envir=.GlobalEnv)
##     return(obs)
## }

################################################################################
## Nom    : UneFamilleDansObs.f
## Objet  : Restreindre le fichier obs à uniquement une famille
## Input  : table "obs"
## Output : table obs pour une seule famille
################################################################################

## [sup] [yr: 11/01/2011]

## UneFamilleDansObs.f <- function ()
## {

##     print("fonction UneFamilleDansObs activée")
##     ChoixUneFamille.f()
##     obs$famille <- especes$Famille[match(obs$code_espece, especes$code_espece)]
##     obs <- subset(obs, obs$famille==fa)
##     gestionMSGaide.f("etapeselected")
##     Jeuxdonnescoupe <- 1
##     assign("Jeuxdonnescoupe", Jeuxdonnescoupe, envir=.GlobalEnv)
##     return(obs)
## }

################################################################################
## Nom    : UnPhylumDansObs.f
## Objet  : Restreindre le fichier obs à uniquement un phylum
## Input  : table "obs"
## Output : table obs pour un seul phylum
################################################################################

## [sup] [yr: 11/01/2011]

## UnPhylumDansObs.f <- function ()
## {

##     print("fonction UnPhylumDansObs.f activée")
##     ChoixUnPhylum.f()
##     obs$phylum <- especes$Phylum[match(obs$code_espece, especes$code_espece)]
##     obs <- subset(obs, obs$phylum==phy)
##     gestionMSGaide.f("etapeselected")
##     Jeuxdonnescoupe <- 1
##     assign("Jeuxdonnescoupe", Jeuxdonnescoupe, envir=.GlobalEnv)
##     return(obs)

## }

################################################################################
## Nom    : UneClasseDansObs.f
## Objet  : Restreindre le fichier obs à uniquement une classe
## Input  : table "obs"
## Output : table obs pour une seule classe
################################################################################

## [sup] [yr: 11/01/2011]

## UneClasseDansObs.f <- function ()
## {

##     print("fonction UneClasseDansObs.f activée")
##     ChoixUneClasse.f()
##     obs$classe <- especes$Classe[match(obs$code_espece, especes$code_espece)]
##     obs <- subset(obs, obs$classe==cla)
##     gestionMSGaide.f("etapeselected")
##     Jeuxdonnescoupe <- 1
##     assign("Jeuxdonnescoupe", Jeuxdonnescoupe, envir=.GlobalEnv)
##     return(obs)

## }

################################################################################
## Nom    : UnOrdreDansObs.f
## Objet  : Restreindre le fichier obs à uniquement un ordre
## Input  : table "obs"
## Output : table obs pour un seul ordre
################################################################################

## [sup] [yr: 11/01/2011]

## UnOrdreDansObs.f <- function ()
## {

##     print("fonction UnOrdreDansObs.f activée")
##     ChoixUnOrdre.f()
##     obs$ordre <- especes$Ordre[match(obs$code_espece, especes$code_espece)]
##     obs <- subset(obs, obs$ordre==ord)
##     gestionMSGaide.f("etapeselected")
##     Jeuxdonnescoupe <- 1
##     return(obs)
##     assign("Jeuxdonnescoupe", Jeuxdonnescoupe, envir=.GlobalEnv)
## }

################################################################################
## Nom    : UneCatBenthDansObs.f
## Objet  : Restreindre le fichier obs à uniquement une famille
## Input  : table "obs"
## Output : table obs pour une seule famille
################################################################################

## [sup] [yr: 11/01/2011]

## UneCatBenthDansObs.f <- function ()
## {

##     print("fonction UneCatBenthDansObs activée")
##     obs$Catbent <- especes$Cath_benthique[match(obs$code_espece, especes$code_espece)]
##     ChoixFacteurSelect.f(obs$Catbent, "Cath_benthique", "single", 1, "selectcb")
##     obs <- subset(obs, obs$Catbent==selectcb) #pour l'instant, variables en "assign"
##     gestionMSGaide.f("etapeselected")
##     Jeuxdonnescoupe <- 1
##     assign("Jeuxdonnescoupe", Jeuxdonnescoupe, envir=.GlobalEnv)
##     return(obs)

## }
## , "Phylum", "Cath_benthique", "Classe", "Ordre", "Famille", à rendre générique

################################################################################
## Nom    : UnCritereEspDansObs.f
## Objet  : Restreindre le fichier obs à uniquement un critere du referentiel spécifique
## Input  : table "obs"
## Output : table obs pour une valeur d'un champs du referentiel spécifique
################################################################################

UnCritereEspDansObs.f <- function ()
{

    print("fonction UnCritereEspDansObs.f activée")

    critereespref.f()
    obs[, factesp] <- especes[, factesp][match(obs$code_espece, especes$code_espece)]

    ## print(head(obs))
    ## ChoixFacteurSelect.f(tableselect=obs[, factesp], monchamp=factesp,
    ##                      Nbselectmax="multiple", ordre=1, mavar="selectfactesp")
    selectfactesp <- selectModWindow.f(factesp, obs, selectmode="extended")
    assign("selectfactesp", selectfactesp, envir=.GlobalEnv)
    ## print(selectfactesp)
    obs <- dropLevels.f(subset(obs, is.element(obs[, factesp], selectfactesp)), which="code_espece")
    gestionMSGaide.f("etapeselected")
    ## Jeuxdonnescoupe <- 1
    assign("Jeuxdonnescoupe", 1, envir=.GlobalEnv)
    return(obs)
}



################################################################################
## Nom    : UnCritereUnitobsDansObs.f
## Objet  : Restreindre le fichier obs à uniquement un critere du referentiel spécifique
## Input  : table "obs"
## Output : table obs pour une valeur d'un champs du referentiel spécifique
################################################################################

UnCritereUnitobsDansObs.f <- function ()
{

    print("fonction UnCritereUnitobsDansObs.f activée")

    choixunfacteurUnitobs.f()
    factunitobs <- fact
    obs[, factunitobs] <- unitobs[, factunitobs][match(obs$unite_observation, unitobs$unite_observation)]
    ## print(head(obs))
    ## ChoixFacteurSelect.f(obs[, factunitobs], factunitobs, "multiple", 1, "selectfactunitobs")

    selectfactunitobs <- selectModWindow.f(factunitobs, obs, selectmode="extended")
    assign("selectfactunitobs", selectfactunitobs, envir=.GlobalEnv)
    print(selectfactunitobs)

    obs <- dropLevels.f(subset(obs, is.element(obs[, factunitobs], selectfactunitobs)),
                        which="unite_observation") # Vérifier si c'est correct [!!!]

    gestionMSGaide.f("etapeselected")
    assign("Jeuxdonnescoupe", 1, envir=.GlobalEnv)
    return(obs)
}
