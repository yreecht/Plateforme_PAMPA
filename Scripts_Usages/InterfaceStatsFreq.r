################################################################################
# Nom               : InterfaceStatsFreq.r
# Type              : Programme
# Objet             : Fonctions permettant de créer l'interface pour les traitements
#                     statistiques des données de fréquentation.
#                     Cette interface permet à l'utilisateur de faire ses choix 
#                     pour les traitements.
# Input             : clic souris
# Output            : lancement de fonctions
# Auteur            : Elodie Gamp
# R version         : 2.11.1
# Date de création  : janvier 2012
# Sources
################################################################################


########################################################################################################################
selectModWindow.f <- function(champ, tab, selectmode="multiple", sort=TRUE, preselect=NULL, title=NULL, label=NULL)
{
    ## Purpose: Ouvre une fenêtre pour le choix des modalités d'un facteur
    ## ----------------------------------------------------------------------
    ## Arguments: champ : le nom de la colonne du facteur
    ##            data : la table de données
    ##            selectmode : mode de sélection (parmi "single" et
    ##                         "multiple")
    ##            sort : ordonner les modalités ? (booléen)
    ##            preselect : un vecteur de modalités à présélectionner (pour
    ##                        des sélections persistantes).
    ##            title : titre de la fenêtre.
    ##            label : texte d'explication.
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date:  5 août 2010, 09:38

    if(champ == "")                     # condition d'arrêt (pas de champ sélectionné).
    {
        return(NULL)
    } else {
        if (all(is.na(tab[ , champ])))  # Cas des champs vides (ajouter un message).
        {
            return(NULL)
        }
    }
    selection <- NULL

    ## ########## Définition des éléments graphiques ##########
    winfac <- tktoplevel()   ## (width = 80)

    if (is.null(title))
    {
        tkwm.title(winfac, paste("Selection des valeurs de ", champ, sep=""))
    } else {
        tkwm.title(winfac, title)
    }

    ## Assenceur vertical :
    SCR.y <- tkscrollbar(winfac, repeatinterval=5, command=function(...){tkyview(LB, ...)})

    ## List Box de sélection :
    LB <- tklistbox(winfac, height=20, width=50, selectmode=selectmode,
                    yscrollcommand=function(...)tkset(SCR.y, ...), background="white")

    ## Boutons :
    FrameB <- tkframe(winfac)
    B.OK <- tkbutton(FrameB, text=" OK ", command=function()
                {
                    assign("selection", listMod[as.numeric(tkcurselection(LB))+1], parent.env(environment()))
                    ## assign("tmptk", tkcurselection(LB), envir=.GlobalEnv)
                    tkdestroy(winfac)
                })
    B.Cancel <- tkbutton(FrameB, text=" Annuler ", command=function()
                     {
                         assign("selection", NULL, parent.env(environment()))
                         tkdestroy(winfac)
                     })

    ## ########## Placement des éléments sur la grille ##########
    ## Explications :
    if (is.null(label))
    {
        tkgrid(tklabel(winfac, text=paste("Liste des valeurs de '", champ,
                               "' présentes.\n ",
                               sep="")), columnspan=2)
    } else {
        tkgrid(tklabel(winfac, text=label), columnspan=2)
    }

    ## Avertissement 'plusieurs sélections possibles' :
    if (is.element(selectmode, c("extended", "multiple")))
    {
        tkgrid(tklabel(winfac, text="Plusieurs sélections POSSIBLES.\n"), columnspan=2)
    } else {}

    ## Avertissement mode de sélection étendu :
    if (selectmode == "extended")
    {
        tkgrid(tklabel(winfac,
                       text=paste("!!Nouveau!! mode de sélection étendu : \n",
                       "*  utilisez Ctrl et Maj pour les sélections multiples.\n",
                       "*  Ctrl+a pour tout sélectionner\n", sep=""),
                       fg="red"), columnspan=2, rowspan=2)
    } else {}

    tkgrid(LB, SCR.y)
    tkgrid.configure(SCR.y, rowspan=4, sticky="nsw")
    tkgrid(FrameB, columnspan=2, sticky="")
    tkgrid(B.OK, tklabel(FrameB, text="        "), B.Cancel, sticky="", pady=5)

    ## Configuration de la liste :
    if (sort)
    {
        listMod <- unique(as.character(sort(tab[ , champ])))
    } else {
        listMod <- unique(as.character(tab[ , champ]))
    }

    invisible(sapply(listMod, function(x){tkinsert(LB, "end", x)}))

    ## Sélections persistantes :
    if (!is.null(preselect))
    {
        sapply(which(is.element(listMod, preselect)) - 1,
               function(i){tkselection.set(LB, i)})
    }
    ## tkselection.set(LB, 0)

    tkbind(winfac, "<Control-a>",       # Tout sélectionner
           function()
       {
           sapply(seq(from=0, length.out=length(listMod)),
                  function(i) {tkselection.set(LB, i)})
       })

    ## Affichage/attente :
    tkfocus(LB)
    winSmartPlace.f(winfac, xoffset=50, yoffset=-100)

    tkwait.window(winfac)
    return(selection)
}


########################################################################################################################
listeNivSpatialStat.f <- function()
{
    ## Purpose: Retourne la liste des niveaux spatiaux disponibles pour le site Etudie.
    ## ----------------------------------------------------------------------
    ## Arguments: aucun.
    ## ----------------------------------------------------------------------
    ## Author: Elodie Gamp, Date: 15 sept. 2011

    ## Niveaux spatiaux disponibles dans le référentiel spatial
    listeNiveauxSpa <- c("AMP" , "codeZone" , "site" , "station" , "groupe" , "statutProtec" ,
                     "zonagePeche" , "codeSIH" , "zonagePAMPA")

    ## Identification des champs non vides du référentiel du site étudié
    champsVide <- ! sapply(refSpatial,
                          function(x){all(is.na(x))})

    listNivSpatial <- listeNiveauxSpa[is.element(listeNiveauxSpa,names(champsVide)[champsVide])]

    return(listNivSpatial)          # ne prend que les niveaux spatiaux renseignés

}


########################################################################################################################
listeNivTemporelStat.f <- function()                
{
    ## Purpose: Retourne la liste des niveaux temporels disponibles pour le site Etudie.
    ## ----------------------------------------------------------------------
    ## Arguments: aucun.
    ## ----------------------------------------------------------------------
    ## Author: Elodie Gamp, Date: 15 sept. 2011

    ## Niveaux temporels disponibles dans le calendrier
    listNivTemporel <- c("periodEchant" , "mois" , "trimestre" , "semestre", "typeJ", "saison")

    return(listNivTemporel)          # actuellement tout est renseigné pour tous

}


########################################################################################################################
listeFacteursMeteoStat.f <- function()           
{
    ## Purpose: Retourne la liste des facteurs météo disponibles pour le site Etudie.
    ## ----------------------------------------------------------------------
    ## Arguments: aucun.
    ## ----------------------------------------------------------------------
    ## Author: Elodie Gamp, Date: janvier 2012

    ## Facteurs météo disponibles
    facteursMeteo <- sort(c("meteo" , "nebulosite" , "directionVent" , "forceVent" , "etatMer" , "lune"))

    ## Identification des champs non vides de la table de fréquentation
    champsVide <- ! sapply(freqtot,
                          function(x){all(is.na(x))})

    listFactMeteo <- facteursMeteo[is.element(facteursMeteo,names(champsVide)[champsVide])]

    return(listFactMeteo)          # ne prend que les facteurs de séparation renseignés

}


########################################################################################################################
listeFacteursFreqStat.f <- function()                   
{
    ## Purpose: Retourne la liste des facteurs de séparation disponibles pour le site Etudie.
    ## ----------------------------------------------------------------------
    ## Arguments: aucun.
    ## ----------------------------------------------------------------------
    ## Author: Elodie Gamp, Date: 15 sept. 2011

    ## Facteurs de séparation disponibles
    facteursFreq <- sort(c("typeBat" , "tailleBat" , "mouillage" , "natureFond" , "act1" , "categAct1"))

    ## Identification des champs non vides de la table de fréquentation
    champsVide <- ! sapply(freqtot,
                          function(x){all(is.na(x))})

    listFactFreq <- c("aucun",facteursFreq[is.element(facteursFreq,names(champsVide)[champsVide])])

    return(listFactFreq)          # ne prend que les facteurs de séparation renseignés

}


########################################################################################################################
listeFacteursStat.f <- function()
{
    ## Purpose: Génère la liste complète des facteurs disponibles pour l'analyse des
    ##          données de fréquentation (spatial, temporel, météo, caractéristiques bateaux)
    ## ----------------------------------------------------------------------
    ## Arguments: facteur : facteur de séparation choisi
    ## ----------------------------------------------------------------------
    ## Author: Elodie Gamp, Date: 15 sept. 2011

    listeSpatial <- listeNivSpatialStat.f () 
    listeTemporel <- listeNivTemporelStat.f () 
    listeMeteo <- listeFacteursMeteoStat.f () 
    listeCarac <- listeFacteursFreqStat.f ()
    listeFacteurs <- c("aucun", listeSpatial, listeTemporel, listeMeteo, listeCarac)  
    
    return (listeFacteurs)
}


########################################################################################################################
listeModalitesStat.f <- function(facteur, env, ...)
{
   ## Purpose: si facteur != "aucun" et choix de sélection de certaines modalités,
    ##          ouvre une fenêtre pour choisir la liste des modalités de ce facteur
    ##          à considérer pour les calculs
    ##          Retourne la liste des modalités choisies par l'utilisateur
    ## ----------------------------------------------------------------------
    ## Arguments: facteur : facteur de séparation choisi
    ##            env : environnement de l'interface
    ## ----------------------------------------------------------------------
    ## Author: Elodie Gamp, Date: 15 sept. 2011

 
    ## vérification de la présence des facteurs explicatifs dans la table freqtot
    ## (parfois absent si ce sont des niveaux du référentiel spatial ou temporel)
        if (!is.element(facteur, 
                      names(freqtot)))
    {
        if (is.element(facteur, 
                      names(refSpatial)))
        {
          freqtot[, facteur] <- refSpatial[, facteur][match(freqtot$zone , refSpatial$codeZone)] 
        } else {
          freqtot[, facteur] <- calendrierGeneral[ , facteur][match(freqtot$moisAn , calendrierGeneral$moisAn)]
        }
    } else {}

    ## Niveaux du facteur de séparation disponibles
    if (facteur == "aucun")
    {
        listNivFactFreq <- ""
    } else {
        listNivFactFreq <- selectModWindow.f(champ=facteur, tab=freqtot, selectmode="extended", ...) 
    }

    return(listNivFactFreq)          # retourne les valeurs disponibles dans le champ facteurSep

}


########################################################################################################################
listeVariablesStat.f <- function()
{
    ## Purpose: retourne un vecteur de caractères donnant les variables
    ##          disponibles selon le facteur de séparation choisi
    ## ----------------------------------------------------------------------
    ## Arguments: aucun
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 13 sept. 2011, 14:40

    listVariable <- c("nbBat", "nbPers", "nbLigne")

    return(listVariable)
}


########################################################################################################################
LancementAnalyse.f <- function(variable, facteur1, facteur2, tableMetrique,
                                 modalites1, modalites2, sufixe=NULL)
{
    ## Purpose: Lance les fonctions d'analyse statistique selon les choix
    ##          effectués par l'utilisateur
    ## ----------------------------------------------------------------------
    ## Arguments: variable : la variable choisie pour le calcul (character)
    ##            facteur1 : le premier facteur à tester
    ##            facteur1 : le second facteur à tester
    ##            tableMetrique : la table de données à considérer (freqtot)
    ##            modalites1 : les niveaux du facteur1 à considérer (uniquement)
    ##            modalites2 : les niveaux du facteur2 à considérer (uniquement)
    ##            env : environnement de l'interface
    ##            sufixe=NULL : un sufixe pour le nom de fichier (utilisé pour l'enregistrement)
    ## ----------------------------------------------------------------------
    ## Author: Elodie Gamp, Date: 16 sept. 2011

    ## vérification de la présence des facteurs explicatifs dans la table freqtot
    ## (parfois absent si ce sont des niveaux du référentiel spatial ou temporel)
        if (!is.element(facteur1, 
                      c("aucun", names(freqtot))))
    {
        if (is.element(facteur1, 
                      names(refSpatial)))
        {
          freqtot[, facteur1] <- refSpatial[, facteur1][match(freqtot$zone , refSpatial$codeZone)] 
        } else {
          freqtot[, facteur1] <- calendrierGeneral[ , facteur1][match(freqtot$moisAn , calendrierGeneral$moisAn)]
        }
    } else {}

        ## vérification de la présence des facteurs explicatifs dans la table freqtot
    ## (parfois absent si ce sont des niveaux du référentiel spatial ou temporel)
        if (!is.element(facteur2, 
                      c("aucun", names(freqtot))))
    {
        if (is.element(facteur2, 
                      names(refSpatial)))
        {
          freqtot[, facteur2] <- refSpatial[, facteur2][match(freqtot$zone , refSpatial$codeZone)] 
        } else {
          freqtot[, facteur2] <- calendrierGeneral[ , facteur2][match(freqtot$moisAn , calendrierGeneral$moisAn)]
        }
    } else {}

    freqtot$aucun <- rep(NA, nrow(freqtot))
    assign("freqtot2", freqtot, envir=.GlobalEnv)


    ## formation du vecteur listFact
    if (facteur2 == "aucun") {
      listFact <- facteur1
    } 
    if (facteur1 =="aucun") {
      listFact <- facteur2
    } 
    if ((facteur1 != "aucun") && (facteur2 != "aucun")) {
      listFact <- c(facteur1, facteur2)
    }
    facteurs <- c("numSortie", listFact)
    
        
        ## si sélection de certaines modalités, restriction du tableau à ces modalités
        tableauCh <- subset(freqtot2, is.element(freqtot2[, facteur1], modalites1))
        tableauCh1 <- subset(tableauCh, is.element(tableauCh[, facteur2], modalites2))
        assign("freqtot2", tableauCh1, envir=.GlobalEnv)
        
        ## lancement de la fonction d'analyse
        modeleLineaireWP3.f ( variable = variable,
                              facteurs = facteurs,
                              listFact = listFact,
                              tableMetrique = "freqtot2",                                   # get(tableMetrique, envir=env)
                              sufixe = NULL)
    
}


########################################################################################################################
updateListFacteursStat1.f <- function(env)
{
    ## Purpose: Mise à jour du choix des niveaux de facteur 1(toutes ou certaines) 
    ##          selon le facteur choisi
    ## ----------------------------------------------------------------------
    ## Arguments: env : l'environnement où faire les MàJ.
    ## ----------------------------------------------------------------------
    ## Author: Elodie Gamp, Date: 22 sept. 2011
       
    ## récupération du facteur dans l'environnement 'env'
    Facteur1 <- get("Facteur1", envir=env)
       
    ## Grisage du choix du choix des niveaux si inapproprié
    evalq(if (is.element(tclvalue(Facteur1),
                          c("aucun")))
      {
          tkconfigure(RB.toutesModalites1, state="disabled")
          tkconfigure(RB.selecModalites1, state="disabled")
          tkconfigure(B.selecModalites1, state="disabled")
          tkconfigure(L.modalites1, state="disabled")
      } else {
          tkconfigure(RB.toutesModalites1, state="normal")
          tkconfigure(RB.selecModalites1, state="normal")
          tkconfigure(L.modalites1, state="normal")
          updateChoixModalitesStat1.f(env=env)
      }, envir=env)

    assign("modalitesChoisies1", NULL, envir=env)

}

########################################################################################################################
updateChoixModalitesStat1.f <- function(env)
{
    ## Purpose: Activer/désactiver le bouton de choix de niveaux de facteur 1
    ## ----------------------------------------------------------------------
    ## Arguments: env : l'environnement de l'interface
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 28 sept. 2011, 10:30

    ## récupération du facteur dans l'environnement 'env'
       Facteur1 <- get("Facteur1", envir=env)
       
    ## vérification de la présence des facteurs explicatifs dans la table freqtot
    ## (parfois absent si ce sont des niveaux du référentiel spatial ou temporel)
        if (!is.element(tclvalue(Facteur1), 
                      c("aucun", names(freqtot))))
    {
        if (is.element(tclvalue(Facteur1), 
                      names(refSpatial)))
        {
          freqtot[, tclvalue(Facteur1)] <- refSpatial[, tclvalue(Facteur1)][match(freqtot$zone , refSpatial$codeZone)] 
        } else {
          freqtot[, tclvalue(Facteur1)] <- calendrierGeneral[ , tclvalue(Facteur1)][match(freqtot$moisAn , calendrierGeneral$moisAn)]
        }
    } else {}

    assign("freqtot2", freqtot, envir=.GlobalEnv)
        
    evalq(if (!is.element(tclvalue(Facteur1), "aucun"))   
          {
            if (is.element(tclvalue(SelecModalites1),
                           c("toutesModalites")))
            {
                tkconfigure(B.selecModalites1, state="disabled")
                 
                modalitesChoisies1 <- unique(freqtot2[, tclvalue(Facteur1)])
            } else {
                tkconfigure(B.selecModalites1, state="normal")
                
                if (any(!is.element(modalitesChoisies1, 
                                    unique(freqtot2[, tclvalue(Facteur1)]))))
                {
                    modalitesChoisies1 <- NULL
                } else {}
       #       assign("modalitesChoisies1", NULL, envir=env)
            }
          } else {}, envir=env)
}


########################################################################################################################
updateListFacteursStat2.f <- function(env)
{
    ## Purpose: Mise à jour du choix des niveaux de facteur 2(toutes ou certaines) 
    ##          selon le facteur choisi
    ## ----------------------------------------------------------------------
    ## Arguments: env : l'environnement où faire les MàJ.
    ## ----------------------------------------------------------------------
    ## Author: Elodie Gamp, Date: 22 sept. 2011

    ## récupération du facteur dans l'environnement 'env'
    Facteur2 <- get("Facteur2", envir=env)
    
    ## Grisage du choix des zones si inapproprié :
    evalq(if (is.element(tclvalue(Facteur2),
                          c("aucun")))
      {
          tkconfigure(RB.toutesModalites2, state="disabled")
          tkconfigure(RB.selecModalites2, state="disabled")
          tkconfigure(B.selecModalites2, state="disabled")
          tkconfigure(L.modalites2, state="disabled")
      } else {
          tkconfigure(RB.toutesModalites2, state="normal")
          tkconfigure(RB.selecModalites2, state="normal")
          tkconfigure(L.modalites2, state="normal")
          updateChoixModalitesStat2.f(env=env)
      }, envir=env)

    assign("modalitesChoisies2", NULL, envir=env)

}

########################################################################################################################
updateChoixModalitesStat2.f <- function(env)
{
    ## Purpose: Activer/désactiver le bouton de choix de niveaux de facteur 2
    ## ----------------------------------------------------------------------
    ## Arguments: env : l'environnement de l'interface.
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 28 sept. 2011, 10:30

    ## récupération du facteur dans l'environnement 'env'
    Facteur2 <- get("Facteur2", envir=env)
    freqtot <- freqtot2
        
    ## vérification de la présence des facteurs explicatifs dans la table freqtot
    ## (parfois absent si ce sont des niveaux du référentiel spatial ou temporel)
        if (!is.element(tclvalue(Facteur2), 
                      c("aucun", names(freqtot))))
    {
        if (is.element(tclvalue(Facteur2), 
                      names(refSpatial)))
        {
          freqtot[, tclvalue(Facteur2)] <- refSpatial[, tclvalue(Facteur2)][match(freqtot$zone , refSpatial$codeZone)] 
        } else {
          freqtot[, tclvalue(Facteur2)] <- calendrierGeneral[ , tclvalue(Facteur2)][match(freqtot$moisAn , calendrierGeneral$moisAn)]
        }
    } else {}
    
    assign("freqtot2", freqtot, envir=.GlobalEnv)
          
    evalq(if (!is.element(tclvalue(Facteur2), "aucun"))   
          {
            if (is.element(tclvalue(SelecModalites2),
                           c("toutesModalites")))
            {
                tkconfigure(B.selecModalites2, state="disabled")
                 
                modalitesChoisies2 <- unique(freqtot2[, tclvalue(Facteur2)])
            } else {
                tkconfigure(B.selecModalites2, state="normal")
                
                if (any(!is.element(modalitesChoisies2, 
                                    unique(freqtot2[, tclvalue(Facteur2)]))))
                {
                    modalitesChoisies2 <- NULL
                } else {}
       #       assign("modalitesChoisies2", NULL, envir=env)
            }
          } else {}, envir=env)
}


########################################################################################################################
interfaceStatsFrequentation.f <- function()
{
    ## Purpose: créer l'interface pour les analyses stat de la  fréquentation
    ##          et lancer les analyses
    ## ----------------------------------------------------------------------
    ## Arguments: aucun
    ## ----------------------------------------------------------------------
    ## Author: Elodie Gamp, Date: 15 sept. 2011

    ## Variables :
    env <- environment()                    # Environnement courant
    Done <- tclVar(0)                       # Statut d'exécution
    modalitesChoisies1 <- NULL               # vecteur de stockage des modalites choisies du facteur 1
    modalitesChoisies2 <- NULL               # vecteur de stockage des modalites choisies du facteur 2
    
    ## Liste de choix du premier facteur
    listFacteur1 <- listeFacteursStat.f()
    Facteur1 <- tclVar(listFacteur1[1])              

    ## Liste de choix du second facteur
    listFacteur2 <- listeFacteursStat.f()
    Facteur2 <- tclVar(listFacteur2[1])            

    ## Liste des variables :
    listVariables <- listeVariablesStat.f()
    VariableChoisie <- tclVar("")                       

    ## Sélection des modalités du premier facteur
    SelecModalites1 <- tclVar("toutesModalites") ## ou "selectModalites"

    ## Sélection des modalités du second facteur
    SelecModalites2 <- tclVar("toutesModalites") ## ou "selectModalites"


    ## ########################
    ## Éléments graphiques :
    WinStatFrequentation <- tktoplevel()          # Fenêtre principale
    tkwm.title(WinStatFrequentation, "Sélections pour l'analyse stat de la fréquentation")

    F.main1 <- tkframe(WinStatFrequentation, width=30)       # facteur 1
    F.main2 <- tkframe(WinStatFrequentation, width=30)       # facteur 2
    F.main3 <- tkframe(WinStatFrequentation, width=30)       # variable
    F.button1 <- tkframe(F.main1, borderwidth=2, relief="groove")         # sélection niveaux facteurs 1
    F.button2 <- tkframe(F.main2, borderwidth=2, relief="groove")         # sélection niveaux facteurs 2

    ## Éléments graphiques :
    CB.facteur1 <- ttkcombobox(F.main1, value=listFacteur1, textvariable=Facteur1,
                          state="readonly")

    CB.facteur2 <- ttkcombobox(F.main2, value=listFacteur2, textvariable=Facteur2,
                          state="readonly")

    CB.variable <- ttkcombobox(F.main3, value=listVariables, textvariable=VariableChoisie,
                          state="readonly")


    B.selecModalites1 <- tkbutton(F.button1, text="choisir les modalités du facteur 1",
                                 command=function()
                           {
                               assign("modalitesChoisies1", listeModalitesStat.f (facteur = tclvalue(Facteur1), env=env, preselect=modalitesChoisies1), envir=env)
                               winRaise.f(WinStatFrequentation)
                           })

    RB.toutesModalites1 <- tkradiobutton(F.button1, variable=SelecModalites1, value="toutesModalites", text="toutes les modalités")
    RB.selecModalites1 <- tkradiobutton(F.button1, variable=SelecModalites1, value="selecModalites", text="sélection de certaines modalités")


    B.selecModalites2 <- tkbutton(F.button2, text="choisir les modalités du facteur 2",
                                 command=function()
                           {
                               assign("modalitesChoisies2", listeModalitesStat.f (facteur = tclvalue(Facteur2), env=env, preselect=modalitesChoisies2), envir=env)
                               winRaise.f(WinStatFrequentation)
                           })

    RB.toutesModalites2 <- tkradiobutton(F.button2, variable=SelecModalites2, value="toutesModalites", text="toutes les modalités")
    RB.selecModalites2 <- tkradiobutton(F.button2, variable=SelecModalites2, value="selecModalites", text="sélection de certaines modalités")


    ## barre de boutons
    FrameBT <- tkframe(WinStatFrequentation)
    B.OK <- tkbutton(FrameBT, text="  Lancer  ", command=function(){tclvalue(Done) <- 1})
    B.Cancel <- tkbutton(FrameBT, text=" Quitter ", command=function(){tclvalue(Done) <- 2})
    
    ## Définition des actions

    tkbind(WinStatFrequentation, "<Destroy>", function(){tclvalue(Done) <- 2})

    tkbind(CB.facteur1, "<FocusIn>", function(){updateListFacteursStat1.f(env=env)})

    tkbind(RB.selecModalites1, "<Leave>", function(){updateChoixModalitesStat1.f(env=env)})
    tkbind(RB.toutesModalites1, "<Leave>", function(){updateChoixModalitesStat1.f(env=env)})

    tkbind(CB.facteur2, "<FocusIn>", function(){updateListFacteursStat2.f(env=env)})

    tkbind(RB.selecModalites2, "<Leave>", function(){updateChoixModalitesStat2.f(env=env)})
    tkbind(RB.toutesModalites2, "<Leave>", function(){updateChoixModalitesStat2.f(env=env)})

    ## Placement des éléments sur l'interface :
    tkgrid(tklabel(F.main1, text="Premier facteur à tester"),
           CB.facteur1, ## column=1, columnspan=3,
           sticky="w", padx=5, pady=5)

    tkgrid(L.modalites1 <- tklabel(F.button1, text="Choix des modalités du facteur 1"),
           column=0, columnspan=3, pady=5, sticky="ew")
    tkgrid(ttkseparator(F.button1, orient = "horizontal"), column=0, columnspan=3, sticky="ew")
    tkgrid(RB.toutesModalites1, RB.selecModalites1, B.selecModalites1, padx=5, pady=5, sticky="w")
    
    tkgrid(tklabel(F.main2, text="Second facteur à tester"),
           CB.facteur2, ## column=1, columnspan=3,
           sticky="w", padx=5, pady=5)

    tkgrid(L.modalites2 <- tklabel(F.button2, text="Choix des modalités du facteur 2"),
           column=0, columnspan=3, pady=5, sticky="ew")
    tkgrid(ttkseparator(F.button2, orient = "horizontal"), column=0, columnspan=3, sticky="ew")
    tkgrid(RB.toutesModalites2, RB.selecModalites2, B.selecModalites2, padx=5, pady=5, sticky="w")
    
    tkgrid(tklabel(F.main3, text="Variable"),
           CB.variable, ## column=1, columnspan=3,
           sticky="w", padx=5, pady=5)


    tkgrid(F.main1, padx=10, pady=10)
    tkgrid(F.button1, columnspan=2)
    tkgrid(F.main2, padx=10, pady=10)
    tkgrid(F.button2, columnspan=2)
    tkgrid(F.main3, padx=10, pady=10)

    ## Barre de boutons :
    tkgrid(FrameBT, column=0, columnspan=3, padx=2, pady=5)
    tkgrid(B.OK, tklabel(FrameBT, text="      "), B.Cancel,
           tklabel(FrameBT, text="               "))

    ## Mise à jour des cadres à activation "dynamique" :
    updateListFacteursStat1.f(env = env)
    updateListFacteursStat2.f(env = env)
        
    ## tkfocus(WinEnquete)
    winSmartPlace.f(WinStatFrequentation)

    ## Update des fenêtres :
    tcl("update")

    ## Tant que l'utilisateur ne ferme pas la fenêtre... :
    repeat
    {
        tkwait.variable(Done)           # attente d'une modification de statut.

        if (tclvalue(Done) == "1")      # statut exécution OK.
        {
            tableauCh <- freqtot
            nomTableauCh <- "freqtot"
            facteur1Ch <- tclvalue(Facteur1)
            facteur2Ch <- tclvalue(Facteur2)
            variableCh <- tclvalue(VariableChoisie)
            modSelec1 <- tclvalue(SelecModalites1)
            modSelec2 <- tclvalue(SelecModalites2)

                        
            tabVerif <- freqtot2
            
            if (tclvalue(SelecModalites1) == "toutesModalites" && tclvalue(Facteur1) != "aucun") 
            {
            modalitesChoisies1 <- unique(tabVerif[, tclvalue(Facteur1)])
            } else {}

            if (tclvalue(SelecModalites2) == "toutesModalites" && tclvalue(Facteur2) != "aucun") 
            {
            modalitesChoisies2 <- unique(tabVerif[, tclvalue(Facteur2)])
            } else {}
            
            
            ## Vérifications des variables (si bonnes, le statut reste 1) :
            tclvalue(Done) <- verifVariablesEnquete.f(tableauCh,
                                                      facteur1Ch,
                                                      facteur2Ch,
                                                      variableCh,
                                                      modSelec1,
                                                      modSelec2,
                                                      modalitesChoisies1,
                                                      modalitesChoisies2,
                                                      ParentWin = WinStatFrequentation)

            if (tclvalue(Done) != "1") {next()} # traitement en fonction du statut : itération
                                        # suivante si les variables ne sont pas bonnes.


            ## ##################################################
            ## Fonctions pour lancer les analyses

              LancementAnalyse.f(variable = variableCh, 
                                 facteur1 = facteur1Ch, 
                                 facteur2 = facteur2Ch, 
                                 tableMetrique = nomTableauCh,
                                 modalites1 = modalitesChoisies1, 
                                 modalites2 = modalitesChoisies2,
                                 sufixe=NULL)



            ## ##################################################

            ## Fenêtre de sélection ramenée au premier plan une fois l'étape finie :
            winSmartPlace.f(WinStatFrequentation)
        } else {}

        if (tclvalue(Done) == "2") {break()} # statut d'exécution 'abandon' : on sort de la boucle.
    }


    tkdestroy(WinStatFrequentation)             # destruction de la fenêtre.

}


