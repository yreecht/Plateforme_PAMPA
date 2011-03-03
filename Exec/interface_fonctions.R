#-*- coding: latin-1 -*-

### File: interface_fonctions.R
### Time-stamp: <2011-02-28 15:31:45 yreecht>
###
### Author: Yves Reecht
###
####################################################################################################
### Description:
###
### Fonctions diverses directement liées à la gestion de l'interface (aspect, position, etc.)
####################################################################################################

########################################################################################################################
winSmartPlace.f <- function(win, xoffset=0, yoffset=0)
{
    ## Purpose: Placement "intelligent" des fenêtres (centrées en fonction de
    ##          leur taille) + apparaissent au premier plan.
    ## ----------------------------------------------------------------------
    ## Arguments: win : un objet de la classe tktoplevel
    ##            xoffset : décalage horizontal (pixels)
    ##            yoffset : décalage vertical (pixels)
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 28 sept. 2010, 14:14

    if (! is.tkwin(win))
    {
        warning("Non mais c'est quoi ce programmeur qui essaye de déplacer des non-fenêtres ?!")
    }else{
        if (! as.logical(as.integer(tclvalue(tkwinfo("exists", win)))))
        {
            warning("Tentative de déplacer une fenêtre déjà détruite !")
        }else{
            ## Largeur de la fenêtre :
            width <- as.integer(tclvalue(tkwinfo("width", win)))
            ## Hauteur de la fenêtre :
            height <- as.integer(tclvalue(tkwinfo("height", win)))
            ## calcul du décalage horizontal :
            x <- as.integer((as.numeric(tclvalue(tkwinfo("screenwidth", win))) - width) / 2) + as.integer(xoffset)
            ## ... et du décalage vertical :
            y <- as.integer((as.numeric(tclvalue(tkwinfo("screenheight", win))) - 60 # pour tenir compte de la barre de
                                        # tache généralement en bas.
                             - height) / 2) + as.integer(yoffset)

            ## print(tkwm.geometry(win))
            ## configuration de la nouvelle géométrie :
            tkwm.geometry(win, paste(width, "x", height, "+", x, "+", y, sep=""))
            tkwm.geometry(win, "")      # pour conserver le redimentionnement automatique.

            ## print(paste(width, "x", height, "+", x, "+", y, sep=""))

            ## Mettre la fenêtre au premier plan :
            tkwm.deiconify(win)
        }
    }
}

########################################################################################################################
winRaise.f <- function(win)
{
    ## Purpose: Remettre une fenêtre au premier plan
    ## ----------------------------------------------------------------------
    ## Arguments: win : un objet de la classe tktoplevel
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 28 sept. 2010, 16:41

    if (! is.tkwin(win))
    {
        warning("Non mais c'est quoi ce programmeur qui essaye de déplacer des non-fenêtres ?!")
    }else{
        if (! as.logical(as.integer(tclvalue(tkwinfo("exists", win)))))
        {
            warning("Tentative de déplacer une fenêtre déjà détruite !")
        }else{
            ## Mettre la fenêtre au premier plan :
            tkwm.deiconify(win)
        }
    }
}

########################################################################################################################
quitConfirm.f <- function(win)
{
    ## Purpose: Confirmer avant de quitter le programme (ou une fenêtre
    ##          quelconque).
    ## ----------------------------------------------------------------------
    ## Arguments: win : l'objet fenêtre.
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 19 janv. 2011, 11:47

    Done <- tclVar("0")
    WinConfirm <- tktoplevel()

    tkwm.title(WinConfirm, "Confirmation...")

    ## Boutons :
    OK.but <- tkbutton(WinConfirm, text = "   Oui   ",
                       command = function() tclvalue(Done) <- 1)
    Cancel.but <- tkbutton(WinConfirm, text = "   Non   ",
                           command = function() tclvalue(Done) <- 2)

    ## Placement des éléments graphiques :

    tkgrid(tklabel(WinConfirm, text="\n "), row=1)

    ## Question sensible au contexte (à améliorer) :
    tkgrid(tklabel(WinConfirm, text=ifelse(win$ID == ".1",
                                           "Voulez vous vraiment quitter le programme ?",
                                           "Voulez vous vraiment fermer cette fenêtre ?")),
           row=1, column=1, columnspan=3)

    tkgrid(tklabel(WinConfirm, text=" "), row=1, column=4)

    tkgrid(tklabel(WinConfirm, text=" \n"), OK.but, ## tklabel(WinConfirm, text=" \n"),
           sticky="e", row=3)
    tkgrid(Cancel.but, sticky="w", row=3, column=3)

    ## Configuration :
    tkbind(WinConfirm, "<Destroy>", function() tclvalue(Done) <- 2)

    winSmartPlace.f(WinConfirm)         # placement de la fenêtre.

    ## Attente d'une action de l'utilisateur :
    tkwait.variable(Done)

    ## Stockage de la valeur obtenue :
    doneVal <- tclvalue(Done)           # nécessaire pour éviter d'avoir systématiquement "2" après destruction
                                        # automatique de la fenêtre.

    ## Destruction automatique de la fenêtre :
    tkdestroy(WinConfirm)

    ## Si c'est le souhait de l'utilisateur, destruction de la fenêtre :
    if (doneVal == "1")
    {
        tkdestroy(win)
    }else{}
}

########################################################################################################################
infoGeneral.f <- function(msg)
{
    ## Purpose: Afficher un cadre général dans la fenêtre d'info.
    ## ----------------------------------------------------------------------
    ## Arguments: msg : message.
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 16 févr. 2011, 15:10

    ## Environnement de stockage des infos
    if (! exists(".InfoLoading", envir=.GlobalEnv))
    {
        assign(".InfoLoading", environment(), envir=.GlobalEnv)
    }else{
        .InfoLoading <- get(".InfoLoading", envir=.GlobalEnv)
    }

    ## Fenêtre graphique :
    if (! exists("WinInfoLoading", envir=.InfoLoading, inherits=FALSE) ||
         ! as.numeric(tclvalue(tkwinfo("exists",
                                       ## Assignation simultannée dans l'environnement courant (pour le cas où le test
                                       ## est FALSE) :
                                       WinInfoLoading <- get("WinInfoLoading",
                                                             envir=.InfoLoading,
                                                             inherits=FALSE)))))
    {
        message("Création de fenêtre")

        ## Création de la fenêtre :
        assign("WinInfoLoading",
               WinInfoLoading <- tktoplevel(), # Assignation simultannée dans l'environnement courant.
               envir=.InfoLoading)

        ## Titre de fenêtre :
        tkwm.title(WinInfoLoading, "Infos de chargement")

        ## Il faudra refaire le cadre principal d'info de chargement :
        assign("makeGlobalFrame", TRUE, envir=.InfoLoading)
    }else{
        ## Note : objet de fenêtre déjà chargé lors du test !
    }


    ## nom du cadre :
    frameName <- paste("Frame", round(runif(1, 0, 2000)), sep="")

    ## Création du cadre :
    assign(frameName,
           FrameTmp <- tkframe(parent = WinInfoLoading), # Assignation simultannée dans l'environnement courant.
           envir=.InfoLoading)

    ## Placement du cadre :
    tkgrid(tklabel(WinInfoLoading, text="\n "),
           FrameTmp,
           tklabel(WinInfoLoading, text=" \n"))

    ## On imprime le message :
    LabMsg <- tklabel(FrameTmp, text=msg)
    tkgrid(LabMsg)

    tkwait.visibility(LabMsg)

    winSmartPlace.f(WinInfoLoading)
    winRaise.f(WinInfoLoading)
    tkfocus(FrameTmp)

}


########################################################################################################################
infoLoading.f <- function(msg="", icon="info", button=FALSE,
                          command=function(){tkdestroy(WinInfoLoading)},...)
{
    ## Purpose: Afficher les informations sur le chargement des données
    ## ----------------------------------------------------------------------
    ## Arguments: msg : message à afficher.
    ##            icon : icone à afficher à gauche du text.
    ##            button : afficher le boutton "OK".
    ##            command : commande associée au bouton.
    ##            ... : paramètres supplémentaires pour le texte.
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 11 févr. 2011, 15:24

    ## Environnement de stockage des infos
    if (! exists(".InfoLoading", envir=.GlobalEnv))
    {
        assign(".InfoLoading", environment(), envir=.GlobalEnv)
    }else{
        .InfoLoading <- get(".InfoLoading", envir=.GlobalEnv)
    }

    ## browser()

    ## Fenêtre graphique :
    if (! exists("WinInfoLoading", envir=.InfoLoading, inherits=FALSE) ||
         ! as.numeric(tclvalue(tkwinfo("exists",
                                       ## Assignation simultannée dans l'environnement courant (pour le cas où le test
                                       ## est FALSE) :
                                       WinInfoLoading <- get("WinInfoLoading",
                                                             envir=.InfoLoading,
                                                             inherits=FALSE)))))
    {
        message("Création de fenêtre")
        ## Création de la fenêtre :
        assign("WinInfoLoading",
               WinInfoLoading <- tktoplevel(), # Assignation simultannée dans l'environnement courant.
               envir=.InfoLoading)

        ## Titre de fenêtre :
        tkwm.title(WinInfoLoading, "Infos de chargement")

        ## Il faudra refaire le cadre principal d'info de chargement :
        assign("makeGlobalFrame", TRUE, envir=.InfoLoading)
    }else{
        ## Note : objet de fenêtre déjà chargé lors du test !
    }

    if (! exists("FramePrinc", envir=.InfoLoading, inherits=FALSE) ||
        tryCatch(get("makeGlobalFrame", envir=.InfoLoading),
                 error=function(e){FALSE}))
    {
        ## Création du cadre principal :
        assign("FramePrinc",
               FramePrinc <- tkframe(parent = WinInfoLoading), # Assignation simultannée dans l'environnement courant.
               envir=.InfoLoading)

        ## Placement du cadre principal :
        tkgrid(tklabel(WinInfoLoading, text="\n "),
               FramePrinc,
               tklabel(WinInfoLoading, text=" \n"))

        ## plus la peine de refaire le cadre principal d'info de chargement :
        assign("makeGlobalFrame", FALSE, envir=.InfoLoading)
    }else{
        ## Si le cadre principal existe, on le charge simplement :
        FramePrinc <- get("FramePrinc", envir=.InfoLoading, inherits=FALSE)
    }

    ## Fenêtre au premier plan :
    winRaise.f(WinInfoLoading)

    ## Label vide affiché en dernier (pour pouvoir forcer l'affichage de la fenêtre) :
    LabVide <- tklabel(FramePrinc, text="")

    if (! button)
    {
        ## Ecriture de la ligne de message :
        tkgrid(
               if (! is.na(icon))
               {
                   ## Affichage de l'icone :
                   tklabel(FramePrinc, image=loadIcon.f(icon))
               }else{
                   tklabel(FramePrinc, text=" \t ")
               },
               ## Séparation :
               tklabel(FramePrinc, text="\t"),
               ## Message :
               LabTmp <- tklabel(FramePrinc, text=msg, justify="left", ...), sticky="nw")

        ## Ligne vide :
        tkgrid(LabVide)
    }else{
        ## Création du bouton "OK" :
        OK.button <- tkbutton(FramePrinc,
                              text="   OK   ",
                              command=command)

        tkgrid(OK.button, columnspan=3)

        ## Ligne vide :
        tkgrid(LabVide)
    }

    ## Forcer l'affichage de la fenêtre avant la suite :
    tkwait.visibility(LabVide)

    ## Placement de la fenêtre :
    winSmartPlace.f(win=WinInfoLoading)
    winRaise.f(win=WinInfoLoading)
    tkfocus(FramePrinc)

    if (button)
    {
        tkwait.window(WinInfoLoading)
    }else{
    tkfocus(LabTmp)}
}

########################################################################################################################
loadIcon.f <- function(icon="info")
{
    ## Purpose: Charger les icones tcl/tk comme images
    ## ----------------------------------------------------------------------
    ## Arguments: icon : nom de l'icone à charger
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 15 févr. 2011, 16:27

    ## Environnement de stockage des infos
    if (! exists(".InfoLoading", envir=.GlobalEnv))
    {
        assign(".InfoLoading", environment(), envir=.GlobalEnv)
    }else{}

    ## Si l'icone n'est pas encore chargée, on la crée :
    if (! exists(icon, envir=.InfoLoading, inherits=FALSE))
    {
        ## Création de la variable contenant l'image :
        assign(icon, tclVar(), envir=.InfoLoading)

        ## Stockage temporaire du nom dans ".InfoLoading" (nécessaire pour utiliser evalq) :
        assign("icon.tmp", icon, envir=.InfoLoading)

        ## Création de l'image à partir des fichiers tcl de la distribution
        evalq(tcl("image", "create", "photo", eval(parse(text=icon.tmp)),
                 file=paste(R.home(), "/Tcl/lib/BWidget/images/", icon.tmp, ".gif", sep="")),
             envir=.InfoLoading)
    }else{}

    ## On retourne un lien vers l'image :
    return(get(icon, envir=.InfoLoading, inherits=FALSE))
}

########################################################################################################################
apropos.f <- function()
{
    ## Purpose:
    ## ----------------------------------------------------------------------
    ## Arguments:
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 21 févr. 2011, 15:51

    WinApropos <- tktoplevel()

    tkwm.title(WinApropos, "À propos de la plateforme")

    tkgrid(tklabel(WinApropos,
                   text=paste("Plateforme PAMPA WP2 : version ", getOption("versionPAMPA"), sep=""),
                   padx=40, pady=20))



    tkgrid(tkbutton(WinApropos, text="   OK   ",
                    command = function(){tkdestroy(WinApropos)}))

    tkgrid(tmp <- tklabel(WinApropos, text=""))

    winSmartPlace.f(WinApropos)
    winRaise.f(WinApropos)
}


### Local Variables:
### ispell-local-dictionary: "english"
### fill-column: 120
### End:
