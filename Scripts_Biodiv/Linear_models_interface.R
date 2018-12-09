#-*- coding: latin-1 -*-
# Time-stamp: <2018-12-09 09:37:42 yreecht>

## Plateforme PAMPA de calcul d'indicateurs de ressources & biodiversité
##   Copyright (C) 2008-2013 Ifremer - Tous droits réservés.
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

### File: Linear_models_interface.R
### Created: <2010-12-16 11:32:33 yreecht>
###
### Author: Yves Reecht
###
####################################################################################################
### Description:
###
###
####################################################################################################

########################################################################################################################
## Fonctions pour ajouter des barres de défilement dans une fenêtre principale :

.RDEnv <- new.env(hash=TRUE)                   # Private environment

rdenv <- function() {
    return(.RDEnv)
}


scrollframeCreate.f <- function(parent,env=rdenv(),...)
{
    stopifnot(is.tkwin(parent))

    frame <- tkframe(parent,
                     class="ScrollFrame",...)
    xscroll <- tkscrollbar(frame,
                           repeatinterval=5,
                           orient="horizontal",
                           command=function(...) tkxview(vport, ...))
    yscroll <- tkscrollbar(frame,
                           repeatinterval=5,
                           orient="vertical",
                           command=function(...) tkyview(vport, ...))
    vport <- tkcanvas(frame)
    tkconfigure(vport, xscrollcommand=function(...) tkset(xscroll, ...))
    tkconfigure(vport, yscrollcommand=function(...) tkset(yscroll, ...))

    pady <- paste("0", tclvalue(tkwinfo("reqheight", xscroll)))
    tkpack(yscroll, side="right", fill="y", pady=pady)
    tkpack(xscroll, side="bottom", fill="x")
    tkpack(vport, side="left", fill="both", expand=TRUE)

    int.frame <- tkframe(vport,
                         borderwidth=4,
                         relief="groove")
    tkcreate(vport, "window", "0 0", anchor="nw", window=int.frame$ID)
    tkbind(int.frame, "<Configure>", function() scrollframeResize(int.frame))

    ## Save this so items can be put in it
    assign("interior.frame", int.frame, envir=env)

    return(frame)
}


scrollframeResize <- function(iframe) {
    stopifnot(tclvalue(tkwinfo("class", iframe)) == "Frame")

    w <- tkwinfo("width", iframe)
    h <- tkwinfo("height", iframe)

    vport <- tkwinfo("parent", iframe)
    stopifnot(tclvalue(tkwinfo("class", vport)) == "Canvas")
    bbox <- tkbbox(vport, "all")

    tkconfigure(vport,
                width=w,
                height=h,
                scrollregion=bbox,
                xscrollincrement="0.1i",
                yscrollincrement="0.1i")
}


scrollframeInterior <- function(env=rdenv()) {
    return(get("interior.frame", envir=env))
}


########################################################################################################################
choixDistri.f <- function(metrique, Data)
{
    ## Purpose: Aider l'utilisateur dans le choix d'une distribution de la
    ##          métrique et lancer les analyses adéquates.
    ## ----------------------------------------------------------------------
    ## Arguments: metrique : le nom de la métrique (variable dépendant)
    ##                       choisie.
    ##            Data : le jeu de données contenant la métrique.
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 18 août 2010, 16:19

    ## Systématiquement détruire la fenêtre en quitant :
    on.exit(tkdestroy(WinDistri))
    ## on.exit(print("WinDistri détruite !"), add=TRUE)


    ## ##################################################
    ## Variables :
    env <- environment()                # environnement courant.
    Done <- tclVar(0)                   # État d'exécution.
    LoiChoisie <- tclVar("NO")          # Variable pour le choix de distribution théorique.
    vscale <- 0.48                      # dimension verticale des graphiques.
    hscale <- 0.95                      # dimension horizontale des graphiques.
    pointsize <- 10                     # taille du point pour les graphiques
    distList <- list()                  # liste pour le stockage des AIC et autres.


    ## ##################################################
    ## Éléments graphiques :
    WinDistri <- tktoplevel()           # Fenêtre principale.
    tkwm.title(WinDistri, paste(mltext("choixDistri.WT")," '", metrique, "'", sep=""))

    ## tkfocus(WinDistri)

    ## Frame d'aide :
    FrameHelp <- tkframe(WinDistri)
    T.help <- tktext(FrameHelp, bg="#fae18d", font="arial", width=100,
                     height=4, relief="groove", borderwidth=2)


    ## Frame pour la loi Normale :
    FrameN <- tkframe(WinDistri, borderwidth=2, relief="groove")
    Img.N <- tkrplot(FrameN,            # Création de l'image.
                     fun=function()
                 {
                     plotDist.f(y=Data[ , metrique], family="NO", metrique=metrique, env=env)
                 },
                     vscale=vscale, hscale=hscale, pointsize=pointsize)

    RB.N <- tkradiobutton(FrameN, variable=LoiChoisie, value="NO", # bouton de sélection.
                          text=paste(mltext("choixDistri.dist.NO"),
                                     " (AIC=", round(distList[["NO"]]$aic, 0), "). ", sep=""))


    ## Frame pour la loi log-Normale :
    FrameLogN <- tkframe(WinDistri, borderwidth=2, relief="groove")
    Img.LogN <- tkrplot(FrameLogN, fun=function() # Création de l'image.
                    {
                        plotDist.f(y=Data[ , metrique], family="LOGNO", metrique=metrique, env=env)
                    },
                        vscale=vscale, hscale=hscale, pointsize=pointsize)

    RB.LogN <- tkradiobutton(FrameLogN, variable=LoiChoisie, value="LOGNO", # bouton de sélection.
                             text=paste(mltext("choixDistri.dist.LogNO"),
                                        " (AIC=", round(distList[["LOGNO"]]$aic, 0), "). ", sep=""))

    ## Frame pour la loi Gamma :
    FrameGa <- tkframe(WinDistri, borderwidth=2, relief="groove")
    Img.Ga <- tkrplot(FrameGa, fun=function() # Création de l'image.
                    {
                        plotDist.f(y=Data[ , metrique], family="GA", metrique=metrique, env=env)
                    },
                        vscale=vscale, hscale=hscale, pointsize=pointsize)

    RB.Ga <- tkradiobutton(FrameGa, variable=LoiChoisie, value="GA", # bouton de sélection.
                           text=paste(mltext("choixDistri.dist.G"),
                                      " (AIC=", round(distList[["GA"]]$aic, 0), "). ", sep=""))

    if (is.integer(Data[ , metrique]))
    {
        ## ## Frame pour la loi de Poisson :
        ## FramePois <- tkframe(WinDistri, borderwidth=2, relief="groove")
        ## Img.Pois <- tkrplot(FramePois,  # Création de l'image.
        ##                     fun=function()
        ##                 {
        ##                     plotDist.f(y=Data[ , metrique], family="PO", metrique=metrique, env=env)
        ##                 },
        ##                     vscale=vscale, hscale=hscale, pointsize=pointsize)

        ## RB.Pois <- tkradiobutton(FramePois, variable=LoiChoisie, value="PO", # bouton de sélection.
        ##                          text=paste("loi de Poisson (AIC=", round(distList[["PO"]]$aic, 0), "). ", sep=""))

        ## Frame pour la loi bionomiale négative :
        FrameNBinom <- tkframe(WinDistri, borderwidth=2, relief="groove")

        Img.NBinom <- tkrplot(FrameNBinom, # Création de l'image.
                              fun=function()
                          {
                              plotDist.f(y=Data[ , metrique], family="NBI", metrique=metrique, env=env)
                          },
                              vscale=vscale, hscale=hscale, pointsize=pointsize)

        RB.NBinom <- tkradiobutton(FrameNBinom, variable=LoiChoisie, value="NBI", # bouton de sélection.
                                   text=paste(mltext("choixDistri.dist.NB"),
                                              " (AIC=",
                                              round(distList[["NBI"]]$aic, 0), "). ", sep=""))
    }else{}

    ## Boutons :
    FrameB <- tkframe(WinDistri)
    B.OK <- tkbutton(FrameB, text=mltext("OK.button"),
                     command=function(){tclvalue(Done) <- "1"})
    B.Cancel <- tkbutton(FrameB, text=mltext("Cancel.button"),
                         command=function(){tclvalue(Done) <- "2"})

    ## ##################################################
    ## Placement des éléments sur la grille :

    tkgrid(tklabel(WinDistri, text=" "))
    tkinsert(T.help, "end", paste(mltext("choixDistri.dist.Help.1"), # texte de l'aide.
                                  mltext("choixDistri.dist.Help.2"),
                                  mltext("choixDistri.dist.Help.3"),
                                  mltext("choixDistri.dist.Help.4"),
                                  mltext("choixDistri.dist.Help.5"),
                                  mltext("choixDistri.dist.Help.6"), sep=""))
    tkgrid(T.help)
    tkgrid(FrameHelp, column=1, columnspan=3)

    tkgrid(tklabel(WinDistri, text=" "))
    tkgrid(Img.N, columnspan=2)
    tkgrid(RB.N, row=1, sticky="e")
    tkgrid(tklabel(FrameN, text=mltext("choixDistri.model.dist.NO"), fg="red"),
           row=1, column=1, sticky="w")

    tkgrid(Img.LogN, columnspan=2)
    tkgrid(RB.LogN, sticky="e")
    tkgrid(tklabel(FrameLogN, text=mltext("choixDistri.model.dist.LogNO"), fg="red"),
           row=1, column=1, sticky="w")
    tkgrid(tklabel(WinDistri, text=" "), FrameN, tklabel(WinDistri, text=" "), FrameLogN, tklabel(WinDistri, text="
    "),
    sticky="ew")

    tkgrid(tklabel(WinDistri, text=" "))

    tkgrid(tklabel(WinDistri, text=" "), FrameN, tklabel(WinDistri, text=" "), FrameLogN, tklabel(WinDistri, text=" "),
           sticky="ew")

    tkgrid(Img.Ga, columnspan=2)
    tkgrid(RB.Ga, sticky="e")
    tkgrid(tklabel(FrameGa, text=mltext("choixDistri.model.dist.G"), fg="red"),
           row=1, column=1, sticky="w")


    ## Évènements : sélections en cliquant sur les graphiques :
    tkbind(Img.N, "<Button-1>", function(){tclvalue(LoiChoisie) <- "NO"})
    tkbind(Img.LogN, "<Button-1>", function(){tclvalue(LoiChoisie) <- "LOGNO"})
    tkbind(Img.Ga, "<Button-1>", function(){tclvalue(LoiChoisie) <- "GA"})

    ## Pour les données entières seulement :
    if (is.integer(Data[ , metrique]))
    {
        ## tkgrid(Img.Pois, columnspan=2)
        ## tkgrid(RB.Pois, sticky="e")
        ## tkgrid(tklabel(FramePois, text=" Modèle : GLM, famille 'Poisson'", fg="red"), row=1, column=1, sticky="w")
        tkgrid(Img.NBinom, columnspan=2)
        tkgrid(RB.NBinom, sticky="e")
        tkgrid(tklabel(FrameNBinom, text=mltext("choixDistri.model.dist.NB"), fg="red"),
               row=1, column=1, sticky="w")
        tkgrid(tklabel(WinDistri, text=" "), FrameGa, ## FramePois,
               tklabel(WinDistri, text=" "), FrameNBinom,
               tklabel(WinDistri, text=" "), sticky="ew")
        tkgrid(tklabel(WinDistri, text=" "))

        ## Évènements : sélections en cliquant sur les graphiques :
        ## tkbind(Img.Pois, "<Button-1>", function(){tclvalue(LoiChoisie) <- "PO"})
        tkbind(Img.NBinom, "<Button-1>", function(){tclvalue(LoiChoisie) <- "NBI"})
    }else{
        tkgrid(tklabel(WinDistri, text=" "), FrameGa, ## FramePois,
               tklabel(WinDistri, text=" "), tklabel(WinDistri, text=" "),
               tklabel(WinDistri, text=" "), sticky="ew")
        tkgrid(tklabel(WinDistri, text=" "))
    }

    ## Boutons :
    tkgrid(FrameB, column=1, columnspan=3)
    tkgrid(B.OK, tklabel(FrameB, text="                         "), B.Cancel)
    tkgrid(tklabel(WinDistri, text=" "))

    ## ##################################################
    ## Autres évènements :
    tkbind(WinDistri, "<Destroy>", function(){tclvalue(Done) <- "2"}) # en cas de destruction de la fenêtre.

    ## Présélection de la distribution avec le plus petit AIC :
    tclvalue(LoiChoisie) <- names(distList)[which.min(sapply(distList, function(x){x$aic}))]
    ## flush.console()

    ## Placement et mise au premier plan de la fenêtre :
    winSmartPlace.f(WinDistri)

    tkwait.variable(Done)               # Attente d'une action de l'utilisateur.

    if (tclvalue(Done) == "1")
    {
        return(tclvalue(LoiChoisie))
    }else{
        return(NULL)
    }

}

########################################################################################################################
supprimeObs.f <- function(residus)
{
    ## Purpose: Choisir des observations à supprimer d'après leur résidus.
    ## ----------------------------------------------------------------------
    ## Arguments: residus : un vecteur de résidus avec les numéros
    ##                        d'observations en noms (obtenus par la fonction
    ##                        'boxplot(...)$out' par exemple).
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 17 sept. 2010, 13:52

    Done <- tclVar("0")
    res <- NULL

    WinSuppr <- tktoplevel()
    tkwm.title(WinSuppr, mltext("supprimeObs.WT"))

    FrameB <- tkframe(WinSuppr)
    B.Oui <- tkbutton(FrameB, text=paste0("    ", mltext("KW.yes"), "    "),
                      command=function(){tclvalue(Done) <- "1"})
    B.Non <- tkbutton(FrameB, text=paste0("    ", mltext("KW.no"), "    "),
                      command=function(){tclvalue(Done) <- "2"})

    tkgrid(tklabel(WinSuppr, text="\t"),
           tklabel(WinSuppr,
                   text=paste("\n",
                              mltext("supprimeObs.L.1"),
                              "\n",
                              mltext("supprimeObs.L.2"),
                              "\n", sep="")),
           tklabel(WinSuppr, text="\t"),
           sticky="w")

    tkgrid(FrameB, column=1)
    tkgrid(B.Oui, tklabel(FrameB, text="\t\t\n"), B.Non)


    tkbind(WinSuppr, "<Destroy>", function(){tclvalue(Done) <- "2"})

    tkfocus(WinSuppr)
    ## Placement et mise au premier plan de la fenêtre :
    winSmartPlace.f(WinSuppr)

    tkwait.variable(Done)

    if (tclvalue(Done) == "1")
    {
        tkdestroy(WinSuppr)
        ## Sélection des observations par l'utilisateur :
        select <-
            selectModWindow.f("residus",
                              data= if (any(is.na(tryCatch(as.integer(names(residus)), warning=function(w){NA}))))
                                    {   # Si les noms de lignes correspondent à des noms d'unitobs...
                                        data.frame(residus=paste("Unitobs. ",
                                                  names(sort(abs(residus), decreasing=TRUE)),
                                                   "  (",
                                                   format(residus[order(abs(residus), decreasing=TRUE)],
                                                          digits=3),
                                                   ")",
                                                   sep=""))
                                    }else{ # ...si ce sont des numéros :
                                        data.frame(residus=paste("Obs. ",
                                                   format(as.integer(names(sort(abs(residus), decreasing=TRUE))),
                                                          width=ceiling(log(max(as.integer(names(residus))), 10)),
                                                          justify="right"),
                                                   "  (",
                                                   format(residus[order(abs(residus), decreasing=TRUE)],
                                                          digits=3),
                                                   ")",
                                                   sep=""))
                                    },
                              sort=FALSE, selectmode="extended",
                              title=mltext("supprimeObs.selectModWindow.WT"),
                              label=paste(mltext("supprimeObs.selectModWindow.L.1"),
                                          mltext("supprimeObs.selectModWindow.L.2"), sep=""))

        if (length(select))
        {
            if (any(is.na(tryCatch(as.integer(names(residus)), warning=function(w){NA}))))
            {   # Si les noms de lignes correspondent à des noms d'unitobs...
                res <- sub("^Unitobs.[[:blank:]]+([^[:blank:]]+)[[:blank:]]+.*", "\\1", select, perl=TRUE)
            }else{
                ## ...sinon, numéro des observations à supprimer : [!!!] on pourrait s'en passer (à régler plus tard).
                res <- as.integer(sub("^Obs.[[:blank:]]+([[:digit:]]+)[[:blank:]]+.*", "\\1", select, perl=TRUE))
            }
        }else{}
    }else{
         tkdestroy(WinSuppr)
     }

    return(res)
}






### Local Variables:
### ispell-local-dictionary: "english"
### fill-column: 120
### End:
