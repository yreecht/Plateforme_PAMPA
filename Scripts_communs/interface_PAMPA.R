#-*- coding: latin-1 -*-

### File: interface_PAMPA.R
### Time-stamp: <2011-11-23 15:43:56 yreecht>
###
### Author: Yves Reecht
###
####################################################################################################
### Description:
###
### Script d'interface d'accueil commune pour les interfaces "Usages" et
### "Ressources & biodiversité".
####################################################################################################

source("Scripts_Biodiv/interface_fonctions.R", encoding="latin1")

interface.PAMPA.f <- function()
{
    ## Purpose: Crée une interface commune qui redirige l'utilisateur au
    ##          choix vers l'interface "Usages" ou
    ##          "Ressources & Biodiversité".
    ## ----------------------------------------------------------------------
    ## Arguments: aucun
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 14 nov. 2011, 16:39

    require(tcltk)
    ## tclRequire("Img")


    F.main <- tktoplevel(background="white")

    tkwm.title(F.main, "Interfaces PAMPA")

    ## images :
    I.biodiv <- tclVar()
    I.usage <- tclVar()

    ## Objets Tk.Image
    tcl("image", "create", "photo", I.biodiv,
        file=paste("./scripts_communs/img/biodiv", p1 <- sample(1:2, 1), ".gif", sep=""))

    tcl("image", "create", "photo", I.usage,
        file=paste("./scripts_communs/img/usage", p2 <- sample(1:2, 1), ".gif", sep=""))


    B.biodiv <- tkbutton(F.main, image=I.biodiv,
                         command=function()
                     {
                         ## tkwm.iconify(F.main)
                         tkdestroy(F.main)

                         source("./Scripts_Biodiv/Global.r", encoding="latin1")
                     },
                         ## height=300,
                         text="Ressources & Biodiversité",
                         borderwidth=4)

    B.usages <- tkbutton(F.main, image=I.usage,
                         command=function()
                     {
                         ## tkwm.iconify(F.main)
                         tkdestroy(F.main)

                         source("./SCRIPTS WP3/TopMenu.r"##, encoding="latin1"
                                )
                     },
                         text="Usages & Perception",
                         borderwidth=4)

    L.titre <- tklabel(F.main, text="Choix d'une interface de calcul",
                       foreground="darkred", background="#FFFBCF",
                       font=tkfont.create(weight="bold", size=9))

    L.info <- tklabel(F.main, text="Passez la souris sur un bouton pour plus d'informations...",
                      background="#FFFBCF", height=8, justify="left", padx=15)

    L.creditsPhoto <- tklabel(F.main,
                              text=paste("Credits photo : ",
                                         c("David CARON", "Julien WICKEL")[p1],
                                         ", ",
                                         c("Parc Marin de la Côte Bleue", "Réserve Naturelle de Cerbère-Banyuls")[p2]),
                              background="white")

    ## Configuration des actions :
    tkbind(B.biodiv, "<Enter>",
           function()
       {
           tkconfigure(L.info,
                       text=paste("Calcul d'indicateurs relatifs aux ressources et à la biodiversité.",
                                  "\n\nTraitement de données de :",
                                  "\n\t* comptages visuels sous-marins (UVC ; poissons, benthos,...).",
                                  "\n\t* enquêtes auprès des pêcheurs (captures).",
                                  "\n\t* vidéos rotatives.",
                                  "\n\t* ...",
                                  sep=""),
                       anchor="w")
       })

    tkbind(B.biodiv, "<Leave>",
           function()
       {
           tkconfigure(L.info,
                       text="Passez la souris sur un bouton pour plus d'informations...",
                       anchor="center")
       })

    tkbind(B.usages, "<Enter>",
           function()
       {
           tkconfigure(L.info,
                       text=paste("Calcul d'indicateurs relatifs aux usages.",
                                  "\n\nTraitement de données d'enquêtes :",
                                  "\n\t* de fréquentation.",
                                  "\n\t* de perception.",
                                  "\n\t* auprès des pêcheurs (captures).",
                                  "\n\t* ...",
                                  sep=""),
                       anchor="e")
       })

    tkbind(B.usages, "<Leave>",
           function()
       {
           tkconfigure(L.info,
                       text="Passez la souris sur un bouton pour plus d'informations...",
                       anchor="center")
       })

    ## Placement des éléments graphiques :
    tkgrid(L.titre, columnspan=2, sticky="ew")

    tkgrid(B.biodiv, B.usages, padx=10, pady=10)

    tkgrid(L.info, columnspan=2, padx=10, pady=10, sticky="ew")

    tkgrid(L.creditsPhoto, columnspan=2, sticky="ew")


    tcl("update")
    winSmartPlace.f(F.main)
    winRaise.f(F.main)
    ## imgAsLabel <- tklabel(F.aide, image=imageAMP, bg="white") # -> label avec image.
}

interface.PAMPA.f()






### Local Variables:
### ispell-local-dictionary: "english"
### fill-column: 120
### End:
