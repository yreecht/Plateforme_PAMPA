#-*- coding: latin-1 -*-

### File: modeles_lineaires_interface.R
### Time-stamp: <2010-09-08 13:53:31 yreecht>
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
choixDistri.f <- function(metrique, data)
{
    ## Purpose: Aider l'utilisateur dans le choix d'une distribution de la
    ##          métrique et lancer les analyses adéquates.
    ## ----------------------------------------------------------------------
    ## Arguments: metrique : le nom de la métrique (variable dépendant)
    ##                       choisie.
    ##            data : le jeu de données contenant la métrique.
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
    vscale <- 0.75                      # dimension verticale des graphiques.
    hscale <- 1.2                       # dimension horizontale des graphiques.
    pointsize <- 10                     # taille du point pour les graphiques
    distList <- list()                  # liste pour le stockage des AIC et autres.


    ## ##################################################
    ## Éléments graphiques :
    WinDistri <- tktoplevel()           # Fenêtre principale.
    tkwm.title(WinDistri, paste("Choix de distribution théorique de la métrique '", metrique, "'", sep=""))

    ## Frame d'aide :
    FrameHelp <- tkframe(WinDistri)
    T.help <- tktext(FrameHelp, bg="#fae18d", font="arial", width=100,
                     height=4, relief="groove", borderwidth=2)



    ## Frame pour la loi Normale :
    FrameN <- tkframe(WinDistri, borderwidth=2, relief="groove")
    Img.N <- tkrplot(FrameN,            # Création de l'image.
                     fun=function()
                 {
                     plotDist.f(y=data[ , metrique], family="NO", metrique=metrique, env=env)
                 },
                     vscale=vscale, hscale=hscale, pointsize=pointsize)

    RB.N <- tkradiobutton(FrameN, variable=LoiChoisie, value="NO", # bouton de sélection.
                          text=paste("loi Normale (AIC=", round(distList[["NO"]]$aic, 0), "). ", sep=""))


    ## Frame pour la loi log-Normale :
    FrameLogN <- tkframe(WinDistri, borderwidth=2, relief="groove")
    Img.LogN <- tkrplot(FrameLogN, fun=function() # Création de l'image.
                    {
                        plotDist.f(y=data[ , metrique], family="LOGNO", metrique=metrique, env=env)
                    },
                        vscale=vscale, hscale=hscale, pointsize=pointsize)

    RB.LogN <- tkradiobutton(FrameLogN, variable=LoiChoisie, value="LOGNO", # bouton de sélection.
                             text=paste("loi log-Normale (AIC=", round(distList[["LOGNO"]]$aic, 0), "). ", sep=""))

    if (is.integer(data[ , metrique]))
    {
        ## Frame pour la loi de Poisson :
        FramePois <- tkframe(WinDistri, borderwidth=2, relief="groove")
        Img.Pois <- tkrplot(FramePois,  # Création de l'image.
                            fun=function()
                        {
                            plotDist.f(y=data[ , metrique], family="PO", metrique=metrique, env=env)
                        },
                            vscale=vscale, hscale=hscale, pointsize=pointsize)

        RB.Pois <- tkradiobutton(FramePois, variable=LoiChoisie, value="PO", # bouton de sélection.
                                 text=paste("loi de Poisson (AIC=", round(distList[["PO"]]$aic, 0), "). ", sep=""))

        ## Frame pour la loi bionomiale négative :
        FrameNBinom <- tkframe(WinDistri, borderwidth=2, relief="groove")
        Img.NBinom <- tkrplot(FrameNBinom, # Création de l'image.
                              fun=function()
                          {
                              plotDist.f(y=data[ , metrique], family="NBI", metrique=metrique, env=env)
                          },
                              vscale=vscale, hscale=hscale, pointsize=pointsize)

        RB.NBinom <- tkradiobutton(FrameNBinom, variable=LoiChoisie, value="NBI", # bouton de sélection.
                                   text=paste("loi Binomiale négative (AIC=",
                                              round(distList[["NBI"]]$aic, 0), "). ", sep=""))
    }else{}

    ## Boutons :
    FrameB <- tkframe(WinDistri)
    B.OK <- tkbutton(FrameB, text="     OK     ", command=function(){tclvalue(Done) <- "1"})
    B.Cancel <- tkbutton(FrameB, text="   Annuler   ", command=function(){tclvalue(Done) <- "2"})

    ## ##################################################
    ## Placement des éléments sur la grille :

    tkgrid(tklabel(WinDistri, text=" "))
    tkinsert(T.help, "end", paste("INFO :\n", # texte de l'aide.
                                  "Cette fenêtre vous permet de choisir la distribution",
                                  " la plus adaptée pour faire vos analyses.\n",
                                  "La distribution (courbe rouge) s'ajustant le mieux à vos données (histogramme) d'après \n",
                                  "le critère d'information de Akaike (AIC ; doit être le plus petit possible) est pré-sélectionnée.", sep=""))
    tkgrid(T.help)
    tkgrid(FrameHelp, column=1, columnspan=3)

    tkgrid(tklabel(WinDistri, text=" "))
    tkgrid(Img.N, columnspan=2)
    tkgrid(RB.N, row=1, sticky="e")
    tkgrid(tklabel(FrameN, text=" Modèle : ANOVA", fg="red"), row=1, column=1, sticky="w")
    tkgrid(Img.LogN, columnspan=2)
    tkgrid(RB.LogN, sticky="e")
    tkgrid(tklabel(FrameLogN, text=" Modèle : ANOVA, données log-transformées", fg="red"), row=1, column=1, sticky="w")
    tkgrid(tklabel(WinDistri, text=" "), FrameN, tklabel(WinDistri, text=" "), FrameLogN, tklabel(WinDistri, text=" "),
           sticky="ew")
    tkgrid(tklabel(WinDistri, text=" "))

    ## Évènements : sélections en cliquant sur les graphiques :
    tkbind(Img.N, "<Button-1>", function(){tclvalue(LoiChoisie) <- "NO"})
    tkbind(Img.LogN, "<Button-1>", function(){tclvalue(LoiChoisie) <- "LOGNO"})

    ## Pour les données entières seulement :
    if (is.integer(data[ , metrique]))
    {
        tkgrid(Img.Pois, columnspan=2)
        tkgrid(RB.Pois, sticky="e")
        tkgrid(tklabel(FramePois, text=" Modèle : GLM, famille 'Poisson'", fg="red"), row=1, column=1, sticky="w")
        tkgrid(Img.NBinom, columnspan=2)
        tkgrid(RB.NBinom, sticky="e")
        tkgrid(tklabel(FrameNBinom, text=" Modèle : GLM, famille 'Binomiale négative'", fg="red"), row=1, column=1, sticky="w")
        tkgrid(tklabel(WinDistri, text=" "), FramePois, tklabel(WinDistri, text=" "), FrameNBinom,
               tklabel(WinDistri, text=" "), sticky="ew")
        tkgrid(tklabel(WinDistri, text=" "))

        ## Évènements : sélections en cliquant sur les graphiques :
        tkbind(Img.Pois, "<Button-1>", function(){tclvalue(LoiChoisie) <- "PO"})
        tkbind(Img.NBinom, "<Button-1>", function(){tclvalue(LoiChoisie) <- "NBI"})
    }else{}

    ## Boutons :
    tkgrid(FrameB, column=1, columnspan=3)
    tkgrid(B.OK, tklabel(FrameB, text="                         "), B.Cancel)
    tkgrid(tklabel(WinDistri, text=" "))

    ## ##################################################
    ## Autres évènements :
    tkbind(WinDistri, "<Destroy>", function(){tclvalue(Done) <- "2"}) # en cas de destruction de la fenêtre.

    ## Présélection de la distribution avec le plus petit AIC :
    tclvalue(LoiChoisie) <- names(distList)[which.min(sapply(distList, function(x){x$aic}))]
    flush.console()

    tkwait.variable(Done)               # Attente d'une action de l'utilisateur.

    if (tclvalue(Done) == "1")
    {
        return(tclvalue(LoiChoisie))
    }else{
        return(NULL)
    }

}







### Local Variables:
### ispell-local-dictionary: "english"
### fill-column: 120
### End:
