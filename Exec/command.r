RestaurerDonnees.f <- function ()
{
    print("fonction RestaurerDonnees.f activée")
    if (Jeuxdonnescoupe==1)
    {
        ## obs <- SAUVobs
        ## unitobs <- SAUVunitobs
        ## contingence <- SAUVcontingence
        ## unitesp <- SAUVunitesp
        ## unit <- SAUVunit
        assign("obs", SAUVobs, envir=.GlobalEnv)
        assign("unitobs", SAUVunitobs, envir=.GlobalEnv)
        assign("contingence", SAUVcontingence, envir=.GlobalEnv)
        assign("unitesp", SAUVunitesp, envir=.GlobalEnv)
        assign("unit", SAUVunit, envir=.GlobalEnv)
        assign("TablePresAbs", SAUVTablePresAbs, envir=.GlobalEnv)

        if (unique(unitobs$type) != "LIT")
        {  # car pas de classes de tailles avec les recouvrements
            ## unitespta <- SAUVunitespta
            assign("unitespta", SAUVunitespta, envir=.GlobalEnv)
        }
        ## si SVR calcul des metriques par rotation
        if (unique(unitobs$type) == "SVR")
        {
            unitesptar <- SAUVunitesptar
            unitespr <- SAUVunitespr
            unitr <- SAUVunitr
            assign("unitesptar", SAUVunitesptar, envir=.GlobalEnv)
            assign("unitespr", SAUVunitespr, envir=.GlobalEnv)
            assign("unitr", SAUVunitr, envir=.GlobalEnv)
        }
        print("données sauvées réinitialisées dans les tables de base")
        ModifierInterfaceApresRestore.f("Aucun", "Aucune")
        Jeuxdonnescoupe <- 0
        gestionMSGinfo.f("Jeuxdedonnerestore", dim(obs)[1])
        tkmessageBox(message=paste("Jeu de données restauré \n", dim(obs)[1],
                                   "enregistrements dans la table observation"))
        print("Jeu de données restauré")
    }
}

## ################################################################################
## Nom    : RegroupementDeuxFactUnitobs.f()
## Objet  : exécution des fonctions choixdeuxFacteurs.f et gra2.f
## ################################################################################

RegroupementDeuxFactUnitobs.f <- function ()
{

    print("fonction RegroupementDeuxFactUnitobs.f activée")
    ## ChoixUneEspece.f()
    ## choixDeuxFacteursUnitobs.f()
    GraphGroup2factUnitobs.f()
    ## gra2.f(fact21, fact22)
}

## ################################################################################
## Nom    : RegroupementUnFactUnitobs.f()
## Objet  : exécution des fonctions choixUnFacteur.f et gra1.f
## ################################################################################

RegroupementUnFactUnitobs.f <- function ()
{

    print("fonction RegroupementUnFactUnitobs.f activée")
    GraphGroup1factUnitobs.f()

}

## ################################################################################
## Nom    : SelectionUnBiotope.f()
## Objet  : exécution de la sélection par fam et écrasement des données dans "obs"
## ################################################################################

SelectionUnBiotope.f <- function ()
{

    print("fonction SelectionUnBiotope.f activée")
    obs <- UnBiotopeDansObs.f()
    assign("obs", obs, envir=.GlobalEnv)
    creationTablesBase.f()
    ModifierInterfaceApresSelection.f(bio, dim(obs)[1])
    gestionMSGinfo.f("Biotopeselectionne", dim(obs)[1])
}



## ################################################################################
## Nom    : SelectionUnCritereEsp.f()
## Objet  : exécution de la sélection par critere, choix de la valeur de sélection
## et écrasement des données dans "obs"
## ################################################################################

SelectionUnCritereEsp.f <- function ()
{

    print("fonction SelectionUnCritere.f activée")
    obs <- UnCritereEspDansObs.f()
    assign("obs", obs, envir=.GlobalEnv)
    creationTablesBase.f()
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

    print("fonction SelectionUnCritereUnitobs.f activée")
    obs <- UnCritereUnitobsDansObs.f()
    assign("obs", obs, envir=.GlobalEnv)
    creationTablesBase.f()
    ModifierInterfaceApresSelection.f(paste(fact[1], ":", selectfactunitobs), dim(obs)[1])
    ## gestionMSGinfo.f("Critereselectionne", dim(obs)[1])
}


## ################################################################################
## Nom    : SelectionUneFamille.f()
## Objet  : exécution de la sélection par fam et écrasement des données dans "obs"
## ################################################################################

SelectionUneFamille.f <- function ()
{

    print("fonction SelectionUneFamille.f activée")
    obs <- UneFamilleDansObs.f()
    assign("obs", obs, envir=.GlobalEnv)
    creationTablesBase.f()
    ModifierInterfaceApresSelection.f(fa, dim(obs)[1])
    gestionMSGinfo.f("Familleselectionne", dim(obs)[1])
}

## ################################################################################
## Nom    : SelectionUnPhylum.f()
## Objet  : exécution de la sélection par phylum et écrasement des données
##          dans "obs"
## ################################################################################

SelectionUnPhylum.f <- function ()
{

    print("fonction SelectionUnPhylum.f activée")
    obs <- UnPhylumDansObs.f()
    assign("obs", obs, envir=.GlobalEnv)
    creationTablesBase.f()
    ModifierInterfaceApresSelection.f(phy, dim(obs)[1])
    gestionMSGinfo.f("Phylumselectionne", dim(obs)[1])
}

## ################################################################################
## Nom    : SelectionUneClasse.f()
## Objet  : exécution de la sélection par classe et écrasement des données
##          dans "obs"
## ################################################################################

SelectionUneClasse.f <- function ()
{

    print("fonction SelectionUneClasse.f activée")
    obs <- UneClasseDansObs.f()
    assign("obs", obs, envir=.GlobalEnv)
    creationTablesBase.f()
    ModifierInterfaceApresSelection.f(cla, dim(obs)[1])
    gestionMSGinfo.f("Classeselectionne", dim(obs)[1])
}
## ################################################################################
## Nom    : SelectionUnOrdre.f()
## Objet  : exécution de la sélection par ordre et écrasement des données dans "obs"
## ################################################################################

SelectionUnOrdre.f <- function ()
{

    print("fonction SelectionUnOrdre.f activée")
    obs <- UnOrdreDansObs.f()
    assign("obs", obs, envir=.GlobalEnv)
    creationTablesBase.f()
    ModifierInterfaceApresSelection.f(ord, dim(obs)[1])
    gestionMSGinfo.f("Ordreselectionne", dim(obs)[1])
}

## ################################################################################
## Nom    : SelectionUneCatBenth.f()
## Objet  : exécution de la sélection par cathégorie benthique et écrasement des
##          données dans "obs"
## ################################################################################
SelectionUneCatBenth.f <- function ()
{

    print("fonction SelectionUneCatBenth.f activée")
    obs <- UneCatBenthDansObs.f()
    assign("obs", obs, envir=.GlobalEnv)
    creationTablesBase.f()
    ModifierInterfaceApresSelection.f(selectcb, dim(obs)[1])
    gestionMSGinfo.f("CatBenthselectionne", dim(obs)[1])
}

## ################################################################################
## Nom    : selectionEspeceStatut.f()
## Objet  : exécution de la sélection par statut et écrasement des données
##          dans "obs"
## ################################################################################

selectionEspeceStatut.f <- function ()
{

    print("fonction selectionEspeceStatut.f activée")
    obs <- UnStatutDansObs.f()
    assign("obs", obs, envir=.GlobalEnv)
    creationTablesBase.f()
    ModifierInterfaceApresSelection.f(statut, dim(obs)[1])
    gestionMSGinfo.f("Statutselectionne", dim(obs)[1])
}

## ################################################################################
## Nom    : benthosUneEspece.f()
## Objet  : exécution des fonctions ChoixUneEspece.f et ___.f
                                        #A FINIR
## ################################################################################
benthosUneEspece.f <- function ()
{
    ChoixUneEspece.f()
} # fin graphuneespece.f()

## ################################################################################
## Nom    : changerUnitobs.f()      changerObservations.f
## Objet  :
## ################################################################################

changerUnitobs.f <- function ()
{
    tkmessageBox(message="Choisissez tout d'abord votre fichier unités d'observation")
    openUnitobs.f()
    tkmessageBox(message="Choisissez ensuite votre fichier d'observations")
    openObservations.f()
    tkmessageBox(message="assurez vous que le référentiel utilisé soit adapté à votre fichier d'observations")
    opendefault.f()
    ## message : les fichiers source sont tous rechargés
}
changerObservations.f <- function ()
{
    ## on ne se sert plus de cette fonction car lorsque l'on change unitobs, on change aussi obs en même temps
    openObservations.f()
    opendefault.f()
    ## message : les fichiers source sont tous rechargés
}
changerListespeces.f <- function ()
{
    openListespeces.f()
    opendefault.f()
    ## message : les fichiers source sont tous rechargés
}

## ################################################################################
## Nom    : unFacteur.f()
## Objet  : exécution des fonctions choixUnFacteur.f, grp1.f, graph1.f et
##          occurrence.f
## ################################################################################

unfacteur.f <- function ()
{
    print("fonction unfacteur.f activée")
    choixunfacteurUnitobs.f()
    grp1.f(fact)
    graph1.f(fact)
    occurrence.f(fact)
} #fin unfacteur.f

## ################################################################################
## Nom    : graphuneespece.f()
## Objet  : exécution des fonctions ChoixUneEspece.f et grpunitobsUneEspece.f
## ################################################################################

graphuneespece.f <- function ()
{
    ChoixUneEspece.f()
    grpunitobsUneEspece.f()
} # fin graphuneespece.f()

## ################################################################################
## Nom    : unf.f()
## Objet  : exécution des fonctions choixUnFacteur.f et gra1.f
## ################################################################################

unf.f <- function ()
{
    choixunfacteurUnitobs.f()
    gra1.f(fact)
}

## ################################################################################
## Nom    : deuxf.f()
## Objet  : exécution des fonctions choixdeuxFacteurs.f et gra2.f
## ################################################################################

deuxf.f <- function ()
{
    ChoixUneEspece.f()
    choixDeuxFacteursUnitobs.f()
    gra2.f(fact21, fact22)
}

## ################################################################################
## Nom    : troisf.f()
## Objet  : exécution des fonctions choixtroisFacteurs.f et gra3.f
## ################################################################################

troisf.f <- function ()
{
    choixtroisfacteurs.f ()
    gra3.f (fact31, fact32, fact33)
}

## ################################################################################
## Nom    : unFacteur.f()
## Objet  : exécution des fonctions choixUnFacteurCT.f, choixCT, grpCT1.f et
##          graphCT1.f
## ################################################################################

unfacteurCT.f <- function ()
{
    choixunfacteurCT.f()
    choixCT.f()
    grpCT1.f(fact)
    graphCT1.f(fact)
} # fin unfacteurCT.f

## ################################################################################
## Nom    : deuxFacteur.f()
## Objet  : exécution des fonctions choixDeuxFacteur.f, grp2f.f,
##          graph2.f et occurrence2.f
## ################################################################################

deuxfacteur.f <- function ()
{
    choixDeuxFacteursUnitobs.f()
    grp2f.f(fact21, fact22)
    graph2.f(fact21, fact22)
    occurrence2.f(fact21, fact22)
} # fin deuxfacteur.f

## ################################################################################
## Nom    : troisFacteurCT.f()
## Objet  : exécution des fonctions choixtroisFacteurCT.f, grp3fCT.f, graphCT3.f
##          et graphCT3.f
## ################################################################################

troisfacteurCT.f <- function ()
{
    choixtroisfacteursCT.f()
    choixCT.f()
    grp3fCT.f(fact31, fact32, fact33)
    graphCT3.f(fact31, fact32, fact33)
}
## ################################################################################
## Nom    : taillemoyenne.f
## Objet  : fonction de calcul de la taille moyenne
##          dans le cas où la taille est renseignée
## Input  : obs$taille, obs$classe_taille
##          especes$poids.moyens, especes$Coeff.a, especes$Coeff.b
## Output : obs$biomasse
## ################################################################################
## ! que fait cette fonction?

taillemoyenne.f <- function ()
{          # [!!!] [inc] Fonction fausse / pas finie ! [yr: 13/08/2010]

    print("fonction taillemoyenne activée")
    ## il faut que la taille soit renseignée
    if (ct == 1)  obs$taillemoyenne =
        assign("obs", obs, envir=.GlobalEnv)
}
## fin taillemoyenne.f


## ################################################################################
## Nom    : grpunitobsCT.f()
## Objet  : exécution des fonctions unFacteurCT.f, deuxfacteurCT.f
##          et troisfacteurCT.f
## ################################################################################
## ! CT nom peu explicite
## ! doit executer les fonctions : place dans interface?

grpunitobsCT.f <- function ()
{
    bb <- tktoplevel()
    tkwm.title(bb, "Choix des groupes d'unites d'observation, toutes especes par classe de taille")
    quit <- tclVar(0)
    f.but <- tkbutton(bb, text="Grouper selon 1 seul facteur", command=unfacteurCT.f)
    ff.but <- tkbutton(bb, text="Grouper en croisant 2 facteurs", command=deuxfacteurCT.f)
    fff.but <- tkbutton(bb, text="Grouper en croisant 3 facteurs", command=troisfacteurCT.f)
    quit.but <- tkbutton(bb, text="Quitter", command=function() tclvalue(quit) <- 1)
    tkpack(f.but, ff.but, fff.but, quit.but)
    tkwait.variable(quit)
    tkdestroy(bb)
} # fin grpunitobsCT.f()

## ################################################################################
## Nom    : grpunitobsUneEspece.f()
## Objet  : exécution des fonctions unf.f, deuxf.f et troisf.f
## ################################################################################
## ! doit executer les fonctions : place dans interface?

grpunitobsUneEspece.f <- function ()
{
    bb <- tktoplevel()
    tkwm.title(bb, "Choix des groupes d'unites d'observation")
    quit <- tclVar(0)
    f.but <- tkbutton(bb, text="Grouper selon 1 seul facteur", command=unf.f)
    ff.but <- tkbutton(bb, text="Grouper en croisant 2 facteurs", command=deuxf.f)
    fff.but <- tkbutton(bb, text="Grouper en croisant 3 facteurs", command=troisf.f)
    quit.but <- tkbutton(bb, text="Quitter", command=function() tclvalue(quit) <- 1)
    tkpack(f.but, ff.but, fff.but, quit.but)
    tkwait.variable(quit)
    tkdestroy(bb)
}

## ################################################################################
## Nom    : deuxFacteurCT.f()
## Objet  : exécution des fonctions choixdeuxFacteursCT.f, choixCT, grp2fCT.f et
##          graphCT2.f
## ################################################################################

deuxfacteurCT.f <- function ()
{
    choixdeuxfacteursCT.f()
    choixCT.f()
    grp2fCT.f(fact21, fact22)
    graphCT2.f(fact21, fact22)
} # fin deuxfacteurCT.f

## ################################################################################
## Nom    : troisFacteur.f()
## Objet  : exécution des fonctions choixtroisFacteur.f, grp3f.f, graph3.f et
##          occurrence3.f
## ################################################################################

troisfacteur.f <- function ()
{
    choixtroisfacteurs.f()
    grp3f.f(fact31, fact32, fact33)
    graph3.f(fact31, fact32, fact33)
    occurrence3.f(fact31, fact32, fact33)
} # fin troisfacteur.f

## ################################################################################
## Nom    : grpunitobs.f()
## Objet  : exécution des fonctions unfacteur.f, deuxfacteur.f et troisfacteur.f
## ################################################################################

grpunitobs.f <- function ()
{

    print("fonction grpunitobs.f activée")
    bb <- tktoplevel()
    tkwm.title(bb, "Choix des groupes d'unites d'observation, toutes especes")
    quit <- tclVar(0)
    f.but <- tkbutton(bb, text="Grouper selon 1 seul facteur", command=unfacteur.f)
    ff.but <- tkbutton(bb, text="Grouper en croisant 2 facteurs", command=deuxfacteur.f)
    fff.but <- tkbutton(bb, text="Grouper en croisant 3 facteurs", command=troisfacteur.f)
    quit.but <- tkbutton(bb, text="Quitter", command=function(){tclvalue(quit) <- 1})
    tkpack(f.but, ff.but, fff.but, quit.but)
    tkwait.variable(quit)
    tkdestroy(bb)
} #fin grpunitobs.f()

## ################################################################################
## Nom    : Organisation du menu Recouvrement
## Objet  : exécution des fonctions graphiques du Benthos
## ################################################################################

GraphRecouvrementParUnitobs.f <- function ()
{
    print("fonction GraphRecouvrementParUnitobs.f activée")
    ## permet de choisir la ou les Cath_benthiques
    Graphbenthos.f("recouvrement", "unite_observation")
}

GraphRecouvrementParStation.f <- function ()
{
    print("fonction GraphRecouvrementParStation.f activée")
    Graphbenthos.f("recouvrement", "station")
}

GraphRecouvrementParSite.f <- function ()
{
    print("fonction GraphRecouvrementParSite.f activée")
    Graphbenthos.f("recouvrement", "site")
}

GraphRecouvrementParCathegorieBenthique.f <- function ()
{
    print("fonction GraphRecouvrementParCathegorieBenthique.f activée")
    Graphbenthos.f("recouvrement", "Cath_benthique")
}

GraphRecouvrementParFamille.f <- function ()
{
    print("fonction GraphRecouvrementParFamille.f activée")
    Graphbenthos.f("recouvrement", "Famille")
}

GraphRecouvrementParGenre.f <- function ()
{
    print("fonction GraphRecouvrementParGenre.f activée")
    Graphbenthos.f("recouvrement", "Genre")
}

GraphRecouvrementParEspece.f <- function ()
{
    print("fonction GraphRecouvrementParEspece.f activée")
    Graphbenthos.f("recouvrement", "espece")
}

GraphColonieParUnitobs.f <- function ()
{
    print("fonction GraphColonieParUnitobs.f activée")
    ## permet de choisir la ou les Cath_benthiques
    Graphbenthos.f("colonie", "unite_observation")
}

GraphColonieParStation.f <- function ()
{
    print("fonction GraphColonieParStation.f activée")
    Graphbenthos.f("colonie", "station")
}

GraphColonieParSite.f <- function ()
{
    print("fonction GraphColonieParSite.f activée")
    Graphbenthos.f("colonie", "site")
}

GraphColonieParCathegorieBenthique.f <- function ()
{
    print("fonction GraphColonieParCathegorieBenthique.f activée")
    Graphbenthos.f("colonie", "Cath_benthique")
}

GraphColonieParFamille.f <- function ()
{
    print("fonction GraphColonieParFamille.f activée")
    Graphbenthos.f("colonie", "Famille")
}

GraphColonieParGenre.f <- function ()
{
    print("fonction GraphColonieParGenre.f activée")
    Graphbenthos.f("colonie", "Genre")
}

GraphColonieParEspece.f <- function ()
{
    print("fonction GraphColonieParEspece.f activée")
    Graphbenthos.f("colonie", "espece")
}

GraphMetriqueParFacteurEspece.f <- function ()
{        # a mettre en fin de menu graphbenthos (interface.r), tester puis généraliser dans Graph...
    print("fonction GraphMetriqueParFacteurEspece.f activée")
    matable <- "listespunit"
    choixchamptable.f(matable)
    metrique <- champtrouve
    print(metrique)
    critereespref.f()
    print(factesp)
    Graphbenthos.f(metrique, facteurMenu=factesp, 1)
}

GraphMetriqueParFacteurUnitobs.f <- function ()
{       # a mettre en fin de menu graphbenthos (interface.r)
    print("fonction GraphMetriqueParFacteurEspece.f activée")
    matable <- "listespunit"
    choixchamptable.f(matable)
    metrique <- champtrouve
    print(metrique)
    choixunfacteurUnitobs.f()
    print(fact)
    Graphbenthos.f(metrique, facteurMenu=fact, 2)
}
