#-*- coding: latin-1 -*-
# Time-stamp: <2019-02-03 18:34:52 yreecht>

## Plateforme PAMPA de calcul d'indicateurs de ressources & biodiversité
##   Copyright (C) 2008-2018 Ifremer - Tous droits réservés.
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

### File: interface_fonctions.R
### Created: <2012-01-17 14:24:57 yves>
###
### Author: Yves Reecht
###
####################################################################################################
### Description:
###
### Fonctions diverses directement liées à la gestion de l'interface (aspect, position, etc.)
####################################################################################################

########################################################################################################################
tooltip <- function(text, targetWidget, width = 350, xskip=0, yskip=0, background="white", focus=FALSE, ...)
{
    ## Purpose: créer un pseudo tooltip. L'association se fait par
    ##          tkbind(targetWidget, "<Enter>",
    ##                 expression(tooltip("Texte", targetWidget)))
    ## ----------------------------------------------------------------------
    ## Arguments: text : Le text à afficher.
    ##            targetWidget : le widget parent.
    ##            width : largeur du tooltip.
    ##            xskip : décalage en x (pixels).
    ##            yskip : décalage en y (pixels).
    ##            background : couleur de fond.
    ##            focus : est-il défini pour le focus sur le widget ?
    ##            ... : arguments supplémentaires pour tklabel.
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, d'après la fonction du package "widgetTools",
    ## Date: 21 nov. 2012, 10:02

    end <- function() {
        tkdestroy(base)
    }

    ## Position x :
    tipX <- as.numeric(tkwinfo("rootx", targetWidget)) + as.numeric(tkwinfo("width",
        targetWidget)) + as.numeric(xskip)

    ## Position y :
    tipY <- as.numeric(tkwinfo("rooty", targetWidget)) + as.numeric(yskip)

    ## Création du tooltip :
    tkwm.overrideredirect(base <- tktoplevel(), TRUE)
    on.exit(tkdestroy(base))

    ## Placement du tooltip
    tkwm.geometry(base, paste("+", tipX, "+", tipY, sep = ""))

    F.tip <- tkframe(base, borderwidth=2, relief="groove", bg=background, padx=3, pady=3)
    tkpack(F.tip)
    tip <- tklabel(F.tip, text = text, background = background, wraplength = width, ...)
    tkpack(tip)

    ## if (focus)
    ## {
    ##     ## On quitte le tooltip en en perdant le focus du widget parent :
    ##     tkbind(base, "<FocusOut>", end)
    ## }else{
    ## On quitte le tooltip en même temps que la souris quitte le widget parent :
    tkbind(targetWidget, "<Leave>", end)
    ## }

    ## ...+ en passant la souris dessus (sécurité) :
    tkbind(base, "<Enter>", end)
    tkbind(base, "<FocusOut>", end)
    tkbind(base, "<Destroy>", end)

    tkwait.window(base)
    return(invisible())
}

########################################################################################################################
tooltipWidget.f <- function(text, targetWidget, width = 350, xskip=0, yskip=0, background="#FFEDC4",
                            font=tkfont.create(weight="normal", size=10),
                            foreground="darkred", justify="left", ...)
{
    ## Purpose: lier un widget et un tooltip.
    ## ----------------------------------------------------------------------
    ## Arguments: text : Le text à afficher.
    ##            targetWidget : le widget parent.
    ##            width : largeur du tooltip.
    ##            xskip : décalage en x (pixels).
    ##            yskip : décalage en y (pixels).
    ##            background : couleur de fond.
    ##            ... : arguments supplémentaires pour tklabel.
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 21 nov. 2012, 10:17

    tkbind(targetWidget, "<Enter>",
           expression(tooltip(text=text, targetWidget=targetWidget, width=width,
                              xskip=xskip, yskip=yskip, background=background,
                              font=font, foreground=foreground, justify=justify,...)))


    ## tkbind(targetWidget, "<FocusIn>",
    ##        expression(tooltip(text=text, targetWidget=targetWidget, width=width,
    ##                           xskip=xskip, yskip=yskip, background=background,
    ##                           font=font, foreground=foreground, justify=justify,
    ##                           focus=TRUE,...)))
}


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
        warning(mltext("warn.move.not.window"))
    }else{
        if (! as.logical(as.integer(tclvalue(tkwinfo("exists", win)))))
        {
            warning(mltext("warn.move.destroyed.window"))
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
        warning(mltext("warn.move.not.window"))
    }else{
        if (! as.logical(as.integer(tclvalue(tkwinfo("exists", win)))))
        {
            warning(mltext("warn.move.destroyed.window"))
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

    tkwm.title(WinConfirm, mltext("WinConfirm.title"))

    ## Boutons :
    OK.but <- tkbutton(WinConfirm, text = mltext("WinConfirm.Yes"),
                       command = function() tclvalue(Done) <- 1)

    All.but <- tkbutton(WinConfirm, text = mltext("WinConfirm.Yes.R"),
                        command = function() tclvalue(Done) <- 2)

    Cancel.but <- tkbutton(WinConfirm, text = mltext("WinConfirm.No"),
                           command = function() tclvalue(Done) <- 3)

    ## Placement des éléments graphiques :

    tkgrid(tklabel(WinConfirm, text="\n "), row=1)

    ## Question sensible au contexte (à améliorer) :
    tkgrid(tklabel(WinConfirm, text=ifelse(win$ID == ".1",
                                           mltext("WinConfirm.quitprog.msg"),
                                           mltext("WinConfirm.quitwin.msg"))),
           row=1, column=1, columnspan=3)

    tkgrid(tklabel(WinConfirm, text=" "), row=1, column=4)

    tkgrid(tklabel(WinConfirm, text=" \n"), row=3)
    tkgrid(OK.but, row=3, column=1, padx=4)
    tkgrid(All.but, row=3, column=2, padx=4)
    tkgrid(Cancel.but, row=3, column=3, padx=4)

    ## Configuration :
    tkbind(WinConfirm, "<Destroy>", function() tclvalue(Done) <- 3)

    tcl("update")

    winSmartPlace.f(WinConfirm)         # placement de la fenêtre.
    tkfocus(Cancel.but)

    ## Attente d'une action de l'utilisateur :
    tkwait.variable(Done)

    ## Stockage de la valeur obtenue :
    doneVal <- tclvalue(Done)           # nécessaire pour éviter d'avoir systématiquement "2" après destruction
                                        # automatique de la fenêtre.

    ## Destruction automatique de la fenêtre :
    tkdestroy(WinConfirm)

    ## Si c'est le souhait de l'utilisateur, destruction de la fenêtre :
    if (doneVal == "1" || doneVal == "2")
    {
        tkdestroy(win)
        if (doneVal == "2")
        {
            q()
        }else{
            if (exists("interface.PAMPA.f", envir=.GlobalEnv, mode="function"))
            {
                evalq(interface.PAMPA.f(), envir=.GlobalEnv)
            }
        }
    }else{}
}

########################################################################################################################
infoGeneral.f <- function(msg, waitCursor=FALSE,...)
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

        ## Création de la fenêtre :
        assign("WinInfoLoading",
               WinInfoLoading <- tktoplevel(), # Assignation simultannée dans l'environnement courant.
               envir=.InfoLoading)

        ## Titre de fenêtre :
        tkwm.title(WinInfoLoading, mltext("WinInfoLoading.title.load"))

        ## Il faudra refaire le cadre principal d'info de chargement :
        assign("makeGlobalFrame", TRUE, envir=.InfoLoading)
    }else{
        ## Note : objet de fenêtre déjà chargé lors du test !
    }

    ## Curseur d'attente si besoin :
    if (waitCursor)
    {
        tkconfigure(WinInfoLoading, cursor="watch")
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
    LabMsg <- tklabel(FrameTmp, text=msg,...)
    tkgrid(LabMsg)

    ## Finir l'affichage des éléments :
    tcl("update", "idletasks")

    winSmartPlace.f(WinInfoLoading)
    winRaise.f(WinInfoLoading)

    ## Update des fenêtres :
    tcl("update")
}


########################################################################################################################
infoLoading.f <- function(msg="", icon="info", button=FALSE,
                          WinRaise=tm,
                          command=function()
                      {
                          tkdestroy(WinInfoLoading)
                          winRaise.f(if(is.null(WinRaise)) tm else WinRaise)
                      }, titleType="load",...)
{
    ## Purpose: Afficher les informations sur le chargement des données
    ## ----------------------------------------------------------------------
    ## Arguments: msg : message à afficher.
    ##            icon : icone à afficher à gauche du text.
    ##            button : afficher le boutton "OK".
    ##            WinRaise : fenêtre à remettre au premier plan à la
    ##                       fermeture.
    ##            command : commande associée au bouton.
    ##            titleType : identifiant d'un type de titre de fenêtre.
    ##            ... : paramètres supplémentaires pour le texte.
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 11 févr. 2011, 15:24

    tcl("update")

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

        ## Création de la fenêtre :
        assign("WinInfoLoading",
               WinInfoLoading <- tktoplevel(), # Assignation simultannée dans l'environnement courant.
               envir=.InfoLoading)

        ## Titre de fenêtre :
        tkwm.title(WinInfoLoading,
                   switch(titleType,
                          "load" = mltext("WinInfoLoading.title.load"),
                          "check" = mltext("WinInfoLoading.title.check"),
                          "warning" = mltext("WinInfoLoading.title.warning"),
                          mltext("WinInfoLoading.title.default")))

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
                              text = mltext("OK.button"),
                              command=command)

        tkgrid(OK.button, columnspan=3)

        tkbind(OK.button, "<Return>", command)

        ## Ligne vide :
        tkgrid(LabVide)
    }

    ## Forcer l'affichage de la fenêtre avant la suite :
    ## tkwait.visibility(LabVide)
    if (button)
    {
        tkconfigure(WinInfoLoading, cursor="arrow")
    }else{}

    ## Placement de la fenêtre :
    tcl("update")
    winSmartPlace.f(win=WinInfoLoading)
    winRaise.f(win=WinInfoLoading)

    if (button)
    {
        tkfocus(OK.button)

        tkwait.window(WinInfoLoading)
    }else{
        ## Update des fenêtres :
        tcl("update")
    }

    return(invisible(WinInfoLoading))
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

        ## Chemin vers les images de BWidget :
        if (! exists("WImgPath", envir=.InfoLoading))
        {
            if (.Platform$OS.type == "unix")
            {
                BWidgetVersion <- tclvalue(tclRequire("BWidget"))

                assign("WImgPath",
                       paste("/usr/share/tcltk/bwidget", BWidgetVersion, "/images/", sep=""),
                       envir=.InfoLoading)
            }else{                          # Windows (autres ?).
                assign("WImgPath",
                       paste(R.home(), "/Tcl/lib/BWidget/images/", sep=""),
                       envir=.InfoLoading)
            }
        }else{}

        ## Création de l'image à partir des fichiers tcl de la distribution
        evalq(tcl("image", "create", "photo", eval(parse(text=icon.tmp)),
                  file=paste(WImgPath, icon.tmp, ".gif", sep="")),
              envir=.InfoLoading)
    }else{}

    ## On retourne un lien vers l'image :
    return(get(icon, envir=.InfoLoading, inherits=FALSE))
}

########################################################################################################################
apropos.f <- function()
{
    ## Purpose: Affiche une fenêtre d'"à propos".
    ## ----------------------------------------------------------------------
    ## Arguments: Aucun !
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 21 févr. 2011, 15:51

    .FrameBackground <- "#FFF6BF"
    WinApropos <- tktoplevel(bg="white")

    tkwm.title(WinApropos, mltext("WinApropos.title"))

    TX.develop <- tktext(WinApropos, bg=.FrameBackground,
                         ## state="disabled",
                         height=8)

    TX.finance <- tktext(WinApropos, bg=.FrameBackground, height=8)

    ## Placement des éléments graphiques :
    tkgrid(tklabel(WinApropos,
                   bg="white",
                   text=paste(mltext("WinApropos.version"), getOption("versionPAMPA"), sep=""),
                   padx=10, pady=15))

    tkgrid(TX.develop, sticky="ew", padx=3, pady=10, ipadx=3)
    tkgrid(TX.finance, sticky="ew", padx=3, pady=10, ipadx=3)

    tkgrid(tkbutton(WinApropos, text=mltext("OK.button"),
                    command = function(){tkdestroy(WinApropos)}),
           pady=7)

    ## Textes sur l'équipe de développement :
    licence <- mltext("WinApropos.license")
    tkinsert(TX.develop, "0.0",
             paste(mltext("WinApropos.TX.dev.1"), licence, ",",
                   mltext("WinApropos.TX.dev.2"), sep=""))
    tktag.add(TX.develop, "licence",
              paste("1.end -", nchar(licence) + 2, " chars", sep=""), # calcul du début de plage du texte de licence.
              "1.end -1 chars")                                      # fin de plage du text de licence (- ",").

    tkinsert(TX.develop, "end", mltext("WinApropos.TX.dev.3"))
    tktag.add(TX.develop, "title1", "end -1 lines linestart", "end -1 lines lineend")

    tkinsert(TX.develop, "end",
             "\n\tYves REECHT, Romain DAVID, Jérémie HABASQUE, Bastien PREUSS")
    tktag.add(TX.develop, "text1", "end -2 lines linestart", "end")

    tkinsert(TX.develop, "end", mltext("WinApropos.TX.dev.4"))
    tktag.add(TX.develop, "title2", "end -1 lines linestart", "end")

    email <- "Yves.Reecht@marine.ie"
    tkinsert(TX.develop, "end", email)
    tktag.add(TX.develop, "email",
              paste("end -", nchar(email) + 1, " chars", sep=""), "end -1 chars")
    tkinsert(TX.develop, "end", ", ")
    email2 <- "infopampa@listes.ifremer.fr"
    tkinsert(TX.develop, "end", email2)
    tktag.add(TX.develop, "email2",
              paste("end -", nchar(email2) + 1, " chars", sep=""), "end -1 chars")

    ## ... configuration des différentes parties du texte :
    FT.title <- tkfont.create(family="arial", weight="bold", size=8)
    FT.email <- tkfont.create(family="courier", size=9, underline="true")

    tktag.configure(TX.develop, "licence", foreground="blue")
    tktag.configure(TX.develop, "title1", font=FT.title, foreground="darkred")
    tktag.configure(TX.develop, "title2", font=FT.title, foreground="darkred")
    tktag.configure(TX.develop, "email", font=FT.email, foreground="blue")
    tktag.configure(TX.develop, "email2", font=FT.email, foreground="blue")

    ## ...apparence du texte de licence :
    tktag.bind(TX.develop, "licence", "<1>",
               function() browseURL(mltext("link.GPL2")))
    tktag.bind(TX.develop, "licence", "<Enter>",
               function() tkconfigure(TX.develop, cursor="hand2"))
    tktag.bind(TX.develop, "licence", "<Leave>",
               function() tkconfigure(TX.develop, cursor="arrow"))

    ## ...apparence de l'adresse e-mail :
    tktag.bind(TX.develop, "email", "<1>",
               function() browseURL(paste("mailto:", email,
                                          ",",
                                          email2,
                                          "?subject=Contact%20plateforme%20PAMPA%20Ressources%20et%20Biodiversité",
                                          sep="")))

    tktag.bind(TX.develop, "email", "<Enter>",
               function() tkconfigure(TX.develop, cursor="hand2"))
    tktag.bind(TX.develop, "email", "<Leave>",
               function() tkconfigure(TX.develop, cursor="arrow"))

    ## ...apparence de l'adresse e-mail2 :
    tktag.bind(TX.develop, "email2", "<1>",
               function() browseURL(paste("mailto:", email2,
                                          ",",
                                          email,
                                          "?subject=Contact%20plateforme%20PAMPA%20Ressources%20et%20Biodiversité",
                                          sep="")))
    tktag.bind(TX.develop, "email2", "<Enter>",
               function() tkconfigure(TX.develop, cursor="hand2"))
    tktag.bind(TX.develop, "email2", "<Leave>",
               function() tkconfigure(TX.develop, cursor="arrow"))

    ## La zone doit être non éditable :
    tkconfigure(TX.develop, state="disabled")

    ## ##################################################
    ## Texte "partenaires" :
    tkinsert(TX.finance, "0.0", mltext("WinApropos.TX.fund.1"))
    tktag.add(TX.finance, "financement", "0.0", "1.end")

    financeurs <- c("Projet Liteau III.",
                    "Agence des Aires Marines Protégées.",
                    "Ifrecor.")
    links <- c("http://www1.liteau.net/index.php/projet/liteau-iii",
               mltext("link.AAMP"),
               "http://www.ifrecor.com/")

    tkinsert(TX.finance, "end",
             paste("\n\t* ", paste(financeurs, collapse="\n\t* "), sep=""))

    nbF <- length(financeurs)
    for (i in 1:nbF)
    {
        tktag.add(TX.finance,
                  paste("part", i, sep=""),
                  paste("end -", nbF + 1 -i, " lines linestart +3 chars", sep=""),
                  paste("end -", nbF + 1 -i, " lines lineend -1 chars", sep=""))
    }

    tkinsert(TX.finance, "end", mltext("WinApropos.TX.fund.2"))
    tktag.add(TX.finance, "title2", "end -1 lines linestart", "end -1 lines lineend")

    tkinsert(TX.finance, "end", mltext("WinApropos.TX.fund.3"))

    linkP <- "https://wwz.ifremer.fr/pampa/Partenaires"
    tkinsert(TX.finance, "end", linkP)
    tktag.add(TX.finance, "linkP",
              paste("end -", nchar(linkP) + 1, " chars", sep=""), "end -1 chars")

    ## ...configuration des différentes parties du texte de "partenaires" :
    tktag.configure(TX.finance, "financement", font=FT.title, foreground="darkred")

    ## Configuration des liens des financeurs :
    for (i in 1:nbF)
    {
        tagF <- paste("part", i, sep="")
        tktag.configure(TX.finance,
                        tagF,
                        font=FT.email, foreground="blue")

        ## Liens :
        eval(substitute(tktag.bind(TX.finance, tagF, "<1>",
                                   function() browseURL(link)),
                        list(link=links[i]))) # eval(substitute()) nécessaire car sinon seule la dernière url
                                        # est utilisée.

        tktag.bind(TX.finance, tagF, "<Enter>",
                   function() tkconfigure(TX.finance, cursor="hand2"))
        tktag.bind(TX.finance, tagF, "<Leave>",
                   function() tkconfigure(TX.finance, cursor="arrow"))
    }

    tktag.configure(TX.finance, "title2", font=FT.title, foreground="darkred")
    tktag.configure(TX.finance, "linkP", font=FT.email, foreground="blue")

    ## ...apparence du lien PAMPA :
    tktag.bind(TX.finance, "linkP", "<1>",
               function() browseURL(linkP))
    tktag.bind(TX.finance, "linkP", "<Enter>",
               function() tkconfigure(TX.finance, cursor="hand2"))
    tktag.bind(TX.finance, "linkP", "<Leave>",
               function() tkconfigure(TX.finance, cursor="arrow"))

    ## La zone doit être non éditable :
    tkconfigure(TX.finance, state="disabled", wrap="word")

    winSmartPlace.f(WinApropos)
    winRaise.f(WinApropos)
}

########################################################################################################################
initInnerTkProgressBar.f <- function(title=mltext("initInnerTkProgressBar.title"), min = 0, max = 100,
                                     initial = 0, width = 300)
{
    ## Purpose: Initialisation d'une barre de progression tk sous forme de
    ##          wiget.
    ## ----------------------------------------------------------------------
    ## Arguments: title : Titre de la barre de progression.
    ##            min : valeur minimum.
    ##            max : valeur maximum.
    ##            initial : valeur initiale.
    ##            width : largeur (pixels) de la barre.
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date:  8 mars 2011, 13:13

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
        warning(mltext("warn.no.progress.window"))

    }else{
        ## Note : objet de fenêtre déjà chargé lors du test !

        ## Frame pour accueillir la progresse barre et ses infos
        assign("FramePG",
               FramePG <- tkframe(WinInfoLoading,  borderwidth=2, relief="groove", padx=5, pady=5),
               envir=.InfoLoading)

        ## Création de la variable d'avancement
        assign("ProgressVal",
               ProgressVal <- tclVar(initial),
               envir=.InfoLoading)

        ## Label avec le pourcentage d'avancement :
        assign("Lab.Progress",
               Lab.Progress <- tklabel(FramePG,
                                       text=paste(format(round(100 * (initial - min) / (max - min)), width=3),
                                                  " %", sep="")),
               envir=.InfoLoading)

        ## Label d'info sur l'étape en cours :
        assign("Lab.StepInfo",
               Lab.StepInfo <- tklabel(FramePG,
                                       text="",
                                       wraplength=width+20),
               envir=.InfoLoading)

        ## Stockage des min et max :
        assign("IPG.min", min, envir=.InfoLoading)
        assign("IPG.max", max, envir=.InfoLoading)

        ## Barre de progression :
        assign("InnerPG",
               InnerPG <- tkwidget(FramePG, "ttk::progressbar", variable=ProgressVal,
                                   length=width, maximum=max - min, mode="determinate"),
               envir=.InfoLoading)

        ## Placement des éléments :
        tkgrid(tklabel(FramePG, text=title), columnspan=2, sticky="w")
        tkgrid(InnerPG, Lab.Progress, sticky="w")
        tkgrid(Lab.StepInfo, columnspan=2, sticky="w")

        tkgrid(tklabel(WinInfoLoading, text=""), FramePG)
        tkgrid(tklabel(WinInfoLoading, text=""))

        winSmartPlace.f(WinInfoLoading)
        winRaise.f(WinInfoLoading)

        ## Update des fenêtres :
        tcl("update")
    }
}

########################################################################################################################
stepInnerProgressBar.f <- function(n=1, msg=NULL,...)
{
    ## Purpose: Incrémentation de l'avancement sur la progressBar.
    ## ----------------------------------------------------------------------
    ## Arguments: n : nombre d'incréments.
    ##            msg : message d'information.
    ##            ... : arguments supplémentaires pour le message
    ##                  (font, etc.)
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date:  8 mars 2011, 15:07

    if (exists(".InfoLoading", envir=.GlobalEnv) &&                     # l'environnement d'info existe...
        exists("WinInfoLoading", envir=.InfoLoading, inherits=FALSE) && # l'objet de fenêtre d'info existe...
        as.numeric(tclvalue(tkwinfo("exists",                           # ... et la fenêtre existe...
                                    WinInfoLoading <- get("WinInfoLoading",               #
                                                          envir=.InfoLoading,             #
                                                          inherits=FALSE)))) &&           #
        exists("InnerPG", envir=.InfoLoading) &&                                     # l'objet de ProgressBar existe...
        as.numeric(tclvalue(tkwinfo("exists", get("InnerPG", envir=.InfoLoading))))) # ...et elle est effectivement sur
                                        # la fenêtre d'info.
    {
        ## Récupération des variables :
        min <- get("IPG.min", envir=.InfoLoading)
        max <- get("IPG.max", envir=.InfoLoading)

        ## Récupération des wigets :
        ProgressVal <- get("ProgressVal", envir=.InfoLoading)
        Lab.Progress <- get("Lab.Progress", envir=.InfoLoading)
        Lab.StepInfo <- get("Lab.StepInfo", envir=.InfoLoading)

        ## Progression de la barre :
        tclvalue(ProgressVal) <- as.numeric(tclvalue(ProgressVal)) + n

        ## Label de progression :
        tkconfigure(Lab.Progress,
                    text=paste(format(round(100 * (as.numeric(tclvalue(ProgressVal)) - min) / (max - min)), width=4),
                               " %", sep=""))

        ## Label d'information sur l'étape en cours (suivante si % d'achevé) :
        if (! is.null(msg))
        {
            tkconfigure(Lab.StepInfo, text=msg,...)
        }else{}

        tkfocus(get("WinInfoLoading", envir=.InfoLoading))

        winSmartPlace.f(WinInfoLoading)
        winRaise.f(WinInfoLoading)

        ## Update des fenêtres :
        tcl("update")
    }
}

########################################################################################################################
reconfigureInnerProgressBar.f <- function(min=NULL, max=NULL, ...)
{
    ## Purpose: Reconfiguration d'une barre de progression interne.
    ## ----------------------------------------------------------------------
    ## Arguments: ... : options de reconfiguration
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date:  9 mars 2011, 11:26

    if (exists(".InfoLoading", envir=.GlobalEnv) &&
        exists("InnerPG", envir=.InfoLoading)&& # l'objet de ProgressBar existe...
        as.numeric(tclvalue(tkwinfo("exists", get("InnerPG", envir=.InfoLoading)))))
    {
        ## Changement de maximum :
        if (! is.null(max) || ! is.null(min))
        {
            ## Paramètre de la fonction ou valeur stockée :
            assign("IPG.max",           # Il faut également stocker la "nouvelle" valeur.
                   max <- ifelse(is.null(max), get("IPG.max", envir=.InfoLoading), max),
                   envir=.InfoLoading)

            assign("IPG.min",           # Il faut également stocker la "nouvelle" valeur.
                   min <- ifelse(is.null(min), get("IPG.min", envir=.InfoLoading), min),
                   envir=.InfoLoading)

            ## Reconfiguration de la barre :
            tkconfigure(get("InnerPG", envir=.InfoLoading),
                        maximum=max - min,
                        ...)
            ## Modification du % d'avancement :
            tkconfigure(get("Lab.Progress", envir=.InfoLoading),
                        text=paste(format(round(100 *
                                                (as.numeric(tclvalue(get("ProgressVal", envir=.InfoLoading))) - min) /
                                                (max - min)), width=4),
                                   " %", sep=""))

        }else{
            ## sinon uniquement les changements sont dans ... :
            if (length(as.list(match.call())) > 1)
            {
                tkconfigure(get("InnerPG", envir=.InfoLoading),...)
            }else{
                warning(mltext("warn.no.progress.option"))
            }
        }
    }else{
        ## On ne fait rien si la barre de preogression n'existe pas.
    }
}


########################################################################################################################
tkObj.gridInfo.f <- function(tkObj)
{
    ## Purpose: Présenter sous une forme plus exploitable les info de
    ##          placement d'un objet Tcl/Tk.
    ## ----------------------------------------------------------------------
    ## Arguments: tkObj : l'objet Tk affiché avec "grid"
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 15 mars 2011, 11:19

    return(unlist(lapply(unlist(strsplit(paste(" ", tclvalue(tkgrid.info(tkObj)), sep=""), " -")),
                         function(x)
                     {
                         res <- unlist(strsplit(x, " "))[2]
                         names(res) <- unlist(strsplit(x, " "))[1]
                         return(res)
                     }))[-1])
}

########################################################################################################################
tkCharPixel.f <- function(parent, type=c("width", "height"),
                          toChar=TRUE, toPix=(! toChar),...)
{
    ## Purpose: Donner un facteur de conversion entre caractères et pixels
    ##          pour les largeurs et hauteurs de widgets.
    ## ----------------------------------------------------------------------
    ## Arguments: parent : widget parent.
    ##            type : largeur et/ou hauteur (width/height) ?
    ##            toChar : conversion des pixels vers charactères ?
    ##                     (char/pix).
    ##            toPix : conversion des pixels vers caractères ? (pix/char).
    ##            ... : arguments supplémentaires de mise en forme du label
    ##                  (fonte, etc).
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 28 nov. 2012, 08:45


    ## Cohérence des paramètres mutuellement exclusifs :
    if (! toChar && ! toPix) {toPix <- TRUE}else{}
    if (toChar && toPix){toChar <- FALSE}else{}

    ## Un texte de 26 charactères de large (moyen) et 2 lignes de haut :
    L.test <- tklabel(parent, text="azertyuiopqsdfghjklmwxcvbn\nazertyuiopqsdfghjklmwxcvbn", ...)

    ## Affichage du label (nécessaire pour avoir des tailles correctes) :
    tkgrid(L.test)
    tcl("update")

    ## Largeur et hauteur du label en pixels
    res <- c(width=as.numeric(tkwinfo("width", L.test)),
             height=as.numeric(tkwinfo("height", L.test)))

    ## On le supprime immédiatement
    tkgrid.remove(L.test)
    tcl("update")

    ## Calcul des rapports de conversion :
    if (toChar)
    {
        res <- c(26, 2) / res
    }else{
        res <- res / c(26, 2)
    }

    ## Renvoi du résultat :
    return(res[type])
}


########################################################################################################################
ColAutoWidth.f <- function(TK.table)
{
    ## Purpose: Largeur automatique des colonnes du tableau
    ## ----------------------------------------------------------------------
    ## Arguments: TK.table : un widget tableau tcl.
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 18 avr. 2011, 16:40

    ## Dimensions du tableau (à partir de 0 => +1) :
    dim.array <- as.numeric(unlist(strsplit(tclvalue(tcl(.Tk.ID(TK.table),
                                                         "index", "end")),
                                            ",")))

    ## Redimensionnement des colonnes :
    invisible(sapply(0:dim.array[2],
                     function(j)
                 {
                     ## Identification du max de caractères dans la colonne :
                     tmp <- sapply(0:dim.array[1],
                                   function(i, j)
                               {
                                   suppressWarnings(max(unlist(sapply(strsplit(tclvalue(tcl(TK.table,
                                                                                            "get",
                                                                                            paste(i, ",", j, sep=""))),
                                                                               split="\n|\r", perl=TRUE),
                                                                      nchar))))
                               }, j)

                     ## Définition de la largeur de colonne :
                     tcl(.Tk.ID(TK.table), "width", j,
                         max(c(tmp, 3)) + ifelse(.Platform$OS.type == "windows", 3, 1))
                 }))
}

########################################################################################################################
RowAutoEight.f <- function(TK.table)
{
    ## Purpose: Hauteur automatique des lignes du tableau
    ## ----------------------------------------------------------------------
    ## Arguments: TK.table : un widget tableau tcl.
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 18 avr. 2011, 16:40

    ## Dimensions du tableau (à partir de 0 => +1) :

    dim.array <- as.numeric(unlist(strsplit(tclvalue(tcl(.Tk.ID(TK.table),
                                                         "index", "bottomright")),
                                            ",")))

    ## Redimensionnement des colonnes :
    invisible(sapply(0:dim.array[1],
                     function(i)
                 {
                     tmp <- sapply(0:dim.array[2],
                                   function(j, i)
                               {
                                   sum(unlist(strsplit(tclvalue(tcl(TK.table,
                                                                    "get",
                                                                    paste(i, ",", j, sep=""))),
                                                       split="\n|\r", perl=TRUE)) != "")
                               }, i=i)

                     tcl(.Tk.ID(TK.table), "height", i, max(c(tmp, 1)))
                 }))
}

########################################################################################################################
updateSummaryTable.f <- function(tclarray, filePathes, Data, table1)
{
    ## Purpose: MàJ du tableau de résumé des fichiers chargés.
    ## ----------------------------------------------------------------------
    ## Arguments: tclarray : tableau de valeurs TCL.
    ##            filePathes : chemins des fichiers
    ##            Data : les données chargées.
    ##            table1 : l'objet TCL de type "table".
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 21 déc. 2011, 11:12

    ##  ############# Mise à jour des valeurs dans le tableau #############
    tclarray[["1,1"]] <- basename(filePathes["unitobs"])
    tclarray[["1,2"]] <- ncol(Data$unitobs)
    tclarray[["1,3"]] <- nrow(Data$unitobs)
    tclarray[["2,1"]] <- basename(filePathes["obs"])
    tclarray[["2,2"]] <- ncol(Data$obs)
    tclarray[["2,3"]] <- nrow(Data$obs)
    tclarray[["3,1"]] <- basename(filePathes["refesp"])
    tclarray[["3,2"]] <- ncol(Data$refesp)
    tclarray[["3,3"]] <- nrow(Data$refesp)

    if ( ! is.na(filePathes["locrefesp"]))
    {
        tclarray[["4,1"]] <- basename(filePathes["locrefesp"])
        tclarray[["4,2"]] <- mltext("tclarray.included")
        tclarray[["4,3"]] <- mltext("tclarray.4.3")
    }else{
        tclarray[["4,1"]] <- mltext("tclarray.not.selected")
        tclarray[["4,2"]] <- "-"
        tclarray[["4,3"]] <- "-"
    }

    if ( ! is.na(filePathes["refspa"]))
    {
        tclarray[["5,1"]] <- basename(filePathes["refspa"])
        tclarray[["5,2"]] <- mltext("tclarray.included")
        tclarray[["5,3"]] <- mltext("tclarray.5.3")
    }else{
        tclarray[["5,1"]] <- mltext("tclarray.not.selected")
        tclarray[["5,2"]] <- "-"
        tclarray[["5,3"]] <- "-"
    }

    ColAutoWidth.f(table1)
}

########################################################################################################################
updateInterface.load.f <- function(baseEnv, tabObs)
{
    ## Purpose: Mise à jour de l'interface principale après chargement des
    ##          données. Indique les nombres effectifs d'unités d'observation
    ##          et de codes d'espèces dans la table d'observations.
    ## ----------------------------------------------------------------------
    ## Arguments: baseEnv : environnement de l'interface.
    ##            tabObs : table des observation
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date:  3 janv. 2012, 15:55

    runLog.f(msg=c(mltext("logmsg.modif.menu.load")))


    ## Réactivation des menus qui nécessitent le chargement préalable :
    tkconfigure(get("MB.selection", envir=baseEnv), state="normal")
    tkconfigure(get("MB.traitement", envir=baseEnv), state="normal")
    tkconfigure(get("MB.analyse", envir=baseEnv), state="normal")

    ## Réactivation des entrées du menu "Données" qui nécessitent le chargement préalable :
    tkentryconfigure(get("import", envir=baseEnv), 3, state="normal")
    tkentryconfigure(get("import", envir=baseEnv), 4, state="normal")
    tkentryconfigure(get("import", envir=baseEnv), 5, state="normal")
    tkentryconfigure(get("import", envir=baseEnv), 6, state="normal")

    tkentryconfigure(get("outils", envir=baseEnv), 4, state="normal")
    tkentryconfigure(get("outils", envir=baseEnv), 5, state="normal")

    ## Désactivation du bouton et du menu de restauration des données originales :
    tkconfigure(get("B.DataRestore", envir=baseEnv), state="disabled")
    tkentryconfigure(get("selection", envir=baseEnv), 5, state="disabled")

    ## if (length(getOption("P.MPA")) < 2 && getOption("P.MPA") == "NC")
    ## {
    ##     if (! grepl("^Carte",
    ##                 tclvalue(tkentrycget(get("traitement", envir=baseEnv), "10", "-label"))))
    ##     {
    ##         tkadd(get("traitement", envir=baseEnv),
    ##               "command", label="Carte de métrique /unité d'observation (démonstration)...",
    ##               background="#FFFBCF",
    ##               command=function ()
    ##           {
    ##               selectionVariablesCarte.f(dataEnv=dataEnv) # [!!!]  [yr: 3/1/2012]
    ##               winRaise.f(get("W.main", envir=baseEnv))
    ##           })
    ##     }else{
    ##         tkentryconfigure(get("traitement", envir=baseEnv), 10, state="normal")
    ##     }
    ## }else{
    ##     if (grepl("^Carte",
    ##               tclvalue(tkentrycget(get("traitement", envir=baseEnv), "10", "-label"))))
    ##     {
    ##         tkdelete(get("traitement", envir=baseEnv), "10", "10")
    ##     }else{}
    ## }

    if (is.element("SpatialPolygonsDataFrame", class(baseEnv$.dataEnv$refspa)))
    {
        tkentryconfigure(get("traitement", envir=baseEnv), 5, state="normal")
        tkentryconfigure(get("traitement", envir=baseEnv), 11, state="normal")
    }else{
        tkentryconfigure(get("traitement", envir=baseEnv), 5, state="disabled")
        tkentryconfigure(get("traitement", envir=baseEnv), 11, state="disabled")
    }

    ## Suppression de la colonne "sélections" si besoin :
    if (tryCatch(nchar(tclvalue(get("tclarray", envir=baseEnv)[[0, 4]])),
                 error=function(e){0}) > 3)
    {
        tkdelete(get("table1", envir=baseEnv), "cols", "end", 1)
    }

    tkconfigure(get("MonCritere", envir=baseEnv), text=mltext("criterion.all"))

    ## Titre du cadre de sélection en non-gras :
    tkconfigure(get("L.criteres", envir=baseEnv), font=tkfont.create(size=8))

    ## Nombres effectifs d'espèces et unitobs :
    ResumerSituationEspecesSelectionnees <- get("ResumerSituationEspecesSelectionnees", envir=baseEnv)
    ResumerSituationUnitobsSelectionnees <- get("ResumerSituationUnitobsSelectionnees", envir=baseEnv)

    ## Nombre d'espèces réellement restantes dans les observations :
    tkconfigure(ResumerSituationEspecesSelectionnees,
                text = paste(mltext("sum.nb.sp.obs.1"),
                             ifelse(getOption("P.selection"), mltext("sum.nb.left"), ""),
                             mltext("sum.nb.in.obs"),
                             ifelse(getOption("P.selection"),
                                    mltext("sum.nb.diff"),
                                    paste0(mltext("colon"), " ")),
                             length(unique(tabObs[ , "species.code"])), sep=""),
                state="normal")

    ## Nombre d'espèces réellement restantes dans les observations :
    tkconfigure(ResumerSituationUnitobsSelectionnees,
                text = paste(mltext("sum.nb.unitobs.obs.1"),
                             ifelse(getOption("P.selection"),  mltext("sum.nb.left"), ""),
                             mltext("sum.nb.in.obs"),
                             ifelse(getOption("P.selection"),
                                    mltext("sum.nb.diff"),
                                    paste0(mltext("colon"), " ")),
                             length(unique(tabObs[ , "observation.unit"])), sep=""),
                state="normal")

    winRaise.f(get("W.main", envir=baseEnv))
}

########################################################################################################################
updateInterface.select.f <- function(criterion, tabObs, baseEnv)
{
    ## Purpose: Mise à jour de l'interface principale après sélection des
    ##          données. Ajoute le critère de sélection à un éventuel critère
    ##          existant dans l'espace d'information et
    ##          indique les nombres effectifs restant d'unités d'observation
    ##          et de codes d'espèces dans la table d'observations.
    ## ----------------------------------------------------------------------
    ## Arguments: criterion : le critère de sélection.
    ##            tabObs : la table des observations (après sélection).
    ##            baseEnv : l'environnement de l'interface.
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date:  4 janv. 2012, 11:59

    runLog.f(msg=c("Modification de l'interface suite à une sélection d'enregistrement :"))

    ## Récupérer les objets tclTk :
    W.main <- get("W.main", envir=baseEnv)
    table1 <- get("table1", envir=baseEnv)
    tclarray <- get("tclarray", envir=baseEnv)
    MonCritere <- get("MonCritere", envir=baseEnv)
    L.criteres <- get("L.criteres", envir=baseEnv)
    B.DataRestore <- get("B.DataRestore", envir=baseEnv)
    ResumerSituationEspecesSelectionnees <- get("ResumerSituationEspecesSelectionnees", envir=baseEnv)
    ResumerSituationUnitobsSelectionnees <- get("ResumerSituationUnitobsSelectionnees", envir=baseEnv)
    selection <- get("selection", envir=baseEnv)

    ## Ajout d'une colonne en fin de table avec les informations de sélection :
    if (tryCatch(nchar(tclvalue(tclarray[[0, 4]])),
                 error=function(e){0}) < 3
        && getOption("P.selection"))
    {
        tkinsert(table1, "cols", "end", 1)
        tclarray[[0, 4]] <- mltext("tclarray.0.4")
    }

    tclarray[[1, 4]] <- nlevels(tabObs[ , "observation.unit"]) # Nombre d'unitobs conservées
                                        # (! peut différer du nombre dans le fichier d'observation).
    tclarray[[2, 4]] <- nrow(tabObs)    # Nombre d'observations
    tclarray[[3, 4]] <- nlevels(tabObs[ , "species.code"]) # Nombre d'espèces conservées
                                        # (! peut différer du nombre dans le fichier d'observation).
    tclarray[[4, 4]] <- "-"
    tclarray[[5, 4]] <- "-"

    ## Largeur automatique des colonnes du tableau :
    ColAutoWidth.f(table1)

    ## Information sur les critères :
    if((tmp <- tclvalue(tkcget(MonCritere, "-text"))) == mltext("criterion.all"))
                                        # ou bien : tcl(.Tk.ID(MonCritere), "cget", "-text")
    {                                   # Si pas de sélection précédente, on affiche seulement le nouveau critère...
        tkconfigure(MonCritere, text=criterion)
    }else{                              # ...sinon on l'ajoute aux existants.
        tkconfigure(MonCritere, text=paste(tmp, criterion, sep="\n\n"))
    }

    ## Titre du cadre de sélection en gras :
    if (criterion !=  mltext("criterion.all"))
    {
        tkconfigure(L.criteres, font=tkfont.create(weight="bold", size=8))
    }else{
        tkconfigure(L.criteres, font=tkfont.create(size=8))
    }

    ## Nombre d'espèces réellement restantes dans les observations :
    tkconfigure(ResumerSituationEspecesSelectionnees,
                text = paste(mltext("sum.nb.sp.obs.1"),
                             ifelse(getOption("P.selection"), mltext("sum.nb.left"), ""),
                             mltext("sum.nb.in.obs"),
                             ifelse(getOption("P.selection"),
                                    mltext("sum.nb.diff"),
                                    paste0(mltext("colon"), " ")),
                             length(unique(tabObs[ , "species.code"])), sep=""),
                state="normal")

    ## Nombre d'espèces réellement restantes dans les observations :
    tkconfigure(ResumerSituationUnitobsSelectionnees,
                text = paste(mltext("sum.nb.unitobs.obs.1"),
                             ifelse(getOption("P.selection"),  mltext("sum.nb.left"), ""),
                             mltext("sum.nb.in.obs"),
                             ifelse(getOption("P.selection"),
                                    mltext("sum.nb.diff"),
                                    paste0(mltext("colon"), " ")),
                             length(unique(tabObs[ , "observation.unit"])), sep=""),
                state="normal")

    if (getOption("P.selection")) # Ré-activation du bouton et du menu de restauration des données originales.
    {
        tkconfigure(B.DataRestore, state="normal")
        tkentryconfigure(selection, 5, state="normal")
    }

    winRaise.f(W.main)
}

########################################################################################################################
updateInterface.restore.f <- function(criterion=mltext("criterion.all"), tabObs, baseEnv)
{
    ## Purpose: Mise à jour de l'interface principale après restauration des
    ##          données originales (avant sélection). Réinitialise les
    ##          informations sur les critères de sélection et indique les
    ##          nombres effectifs d'unités d'observation et de codes
    ##          d'espèces dans la table d'observations.
    ## ----------------------------------------------------------------------
    ## Arguments: criterion : le critère de sélection.
    ##            tabObs : la table des observations (après sélection).
    ##            baseEnv : l'environnement de l'interface.
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date:  4 janv. 2012, 13:50

    runLog.f(msg=c("Modification de l'interface après restauration des données originales :"))

    ## Récupération des objets tclTk :
    W.main <- get("W.main", envir=baseEnv)
    table1 <- get("table1", envir=baseEnv)
    tclarray <- get("tclarray", envir=baseEnv)
    MonCritere <- get("MonCritere", envir=baseEnv)
    L.criteres <- get("L.criteres", envir=baseEnv)
    B.DataRestore <- get("B.DataRestore", envir=baseEnv)
    ResumerSituationEspecesSelectionnees <- get("ResumerSituationEspecesSelectionnees", envir=baseEnv)
    ResumerSituationUnitobsSelectionnees <- get("ResumerSituationUnitobsSelectionnees", envir=baseEnv)
    selection <- get("selection", envir=baseEnv)

    ## Suppression de la dernière colonne du tableau d'information :
    tkdelete(table1, "cols", "end", 1)

    tclarray[[0, 4]] <- ""
    tclarray[[2, 4]] <- nrow(tabObs)

    ## Largeur automatique des colonnes du tableau :
    ColAutoWidth.f(table1)

    ## Réinitialisation des critères de sélection :
    tkconfigure(MonCritere, text=criterion)

    ## Titre du cadre de sélection en non-gras :
    tkconfigure(L.criteres, font=tkfont.create(size=8))

    ## Nombre d'espèces réellement restantes dans les observations :
    tkconfigure(ResumerSituationEspecesSelectionnees,
                text = paste(mltext("sum.nb.sp.obs.2"),
                             mltext("sum.nb.in.obs"),
                             mltext("colon"), " ",
                             length(unique(tabObs[ , "species.code"])), sep=""),
                state="normal")

    ## Nombre d'espèces réellement restantes dans les observations :
    tkconfigure(ResumerSituationUnitobsSelectionnees,
                text = paste(mltext("sum.nb.unitobs.obs.1"),
                             mltext("sum.nb.in.obs"),
                             mltext("colon"), " ",
                             length(unique(tabObs[ , "observation.unit"])), sep=""),
                state="normal")

    ## Désactivation du bouton et du menu de restauration des données originales :
    tkconfigure(B.DataRestore, state="disabled")
    tkentryconfigure(selection, 5, state="disabled")

    winRaise.f(W.main)
}



### Local Variables:
### ispell-local-dictionary: "english"
### fill-column: 120
### End:
