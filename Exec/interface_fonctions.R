#-*- coding: latin-1 -*-

### File: interface_fonctions.R
### Time-stamp: <2010-09-28 16:42:28 yreecht>
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

            ## configuration de la nouvelle géométrie :
            tkwm.geometry(win, paste(width, "x", height, "+", x, "+", y, sep=""))
            tkwm.geometry(win, "")      # pour conserver le redimentionnement automatique.

            print(paste(width, "x", height, "+", x, "+", y, sep=""))

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







### Local Variables:
### ispell-local-dictionary: "english"
### fill-column: 120
### End:
