#-*- coding: latin-1 -*-

### File: interface_fonctions.R
### Time-stamp: <2011-01-19 13:59:03 yreecht>
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





### Local Variables:
### ispell-local-dictionary: "english"
### fill-column: 120
### End:
