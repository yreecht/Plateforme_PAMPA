################################################################################
# Nom               : InfoApropos.r
# Type              : Programme
# Objet             : Fonction générant la fenêtre d'informations de l'icone "A propos"
#                     (version, contact, financement, etc.)
# Input             : data importées en txt
# Output            : tableaux et graphs
# Auteur            : Elodie Gamp (sur la base de Pierre-Olivier Goffard)
# R version         : 2.11.1
# Date de création  : décembre 2011
# Sources
################################################################################


#######################################################################################################
apropos.f <- function()
{
    ## Purpose:
    ## ----------------------------------------------------------------------
    ## Arguments:
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 21 févr. 2011, 15:51

    .FrameBackground <- "#FFF6BF"
    WinApropos <- tktoplevel(bg="white")

    tkwm.title(WinApropos, "À propos de la plateforme")

    TX.develop <- tktext(WinApropos, bg=.FrameBackground,
                         ## state="disabled",
                         height=8)

    TX.finance <- tktext(WinApropos, bg=.FrameBackground, height=8)

    ## Placement des éléments graphiques :
    tkgrid(tklabel(WinApropos,
                   bg="white",
                   text=paste("Plateforme de traitement PAMPA \"Usages \" : version ", 1, sep=""),
                   padx=10, pady=15))

    tkgrid(TX.develop, sticky="ew", padx=3, pady=10, ipadx=3)
    tkgrid(TX.finance, sticky="ew", padx=3, pady=10, ipadx=3)

    tkgrid(tkbutton(WinApropos, text="   OK   ",
                    command = function(){tkdestroy(WinApropos)}),
           pady=7)

    ## Textes sur l'équipe de développement :
    licence <- "GNU GPL version >= 2"
    tkinsert(TX.develop, "0.0",
             paste(" Logiciel distribué sous licence ", licence, ",",
                   "\n développé au sein de l'Ifremer.", sep=""))
    tktag.add(TX.develop, "licence",
              paste("1.end -", nchar(licence) + 2, " chars", sep=""), # calcul du début de plage du texte de licence.
              "1.end -1 chars")                                      # fin de plage du text de licence (- ",").

    tkinsert(TX.develop, "end", "\n\n Developpeurs :")
    tktag.add(TX.develop, "title1", "end -1 lines linestart", "end -1 lines lineend")

    tkinsert(TX.develop, "end",
             "\n\tElodie GAMP")
    tktag.add(TX.develop, "text1", "end -2 lines linestart", "end")

    tkinsert(TX.develop, "end", "\n\n Contact : ")
    tktag.add(TX.develop, "title2", "end -1 lines linestart", "end")

    email <- "elodie.gamp@ifremer.fr"
    tkinsert(TX.develop, "end", email)
    tktag.add(TX.develop, "email",
              paste("end -", nchar(email) + 1, " chars", sep=""), "end -1 chars")

    ## ... configuration des différentes parties du texte :
    FT.title <- tkfont.create(family="arial", weight="bold", size=8)
    FT.email <- tkfont.create(family="courier", size=9, underline="true")

    tktag.configure(TX.develop, "licence", foreground="blue")
    tktag.configure(TX.develop, "title1", font=FT.title, foreground="darkred")
    tktag.configure(TX.develop, "title2", font=FT.title, foreground="darkred")
    tktag.configure(TX.develop, "email", font=FT.email, foreground="blue")

    ## ...apparence du texte de licence :
    tktag.bind(TX.develop, "licence", "<1>",
               function() browseURL("http://www.gnu.org/licenses/licenses.fr.html#GPL"))
    tktag.bind(TX.develop, "licence", "<Enter>",
               function() tkconfigure(TX.develop, cursor="hand2"))
    tktag.bind(TX.develop, "licence", "<Leave>",
               function() tkconfigure(TX.develop, cursor="arrow"))

    ## ...apparence de l'adresse e-mail :
    tktag.bind(TX.develop, "email", "<1>",
               function() browseURL(paste("mailto:", email, "?subject=Contact%20plateforme%20PAMPA", sep="")))
    tktag.bind(TX.develop, "email", "<Enter>",
               function() tkconfigure(TX.develop, cursor="hand2"))
    tktag.bind(TX.develop, "email", "<Leave>",
               function() tkconfigure(TX.develop, cursor="arrow"))

    ## La zone doit être non éditable :
    tkconfigure(TX.develop, state="disabled")

    ## ##################################################
    ## Texte "partenaires" :
    tkinsert(TX.finance, "0.0", " Financements :")
    tktag.add(TX.finance, "financement", "0.0", "1.end")

    financeurs <- c("Projet Liteau III ",
                    "Agence des Aires Marines Protégées ",
                    "Ifrecor ")
    links <- c("http://www1.liteau.net/index.php/projet/liteau-iii",
               "http://www.aires-marines.fr/",
               "http://www.ifrecor.org/")

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

    tkinsert(TX.finance, "end", "\n\n Autres partenaires : ")
    tktag.add(TX.finance, "title2", "end -1 lines linestart", "end -1 lines lineend")

    tkinsert(TX.finance, "end", "\n Retrouvez les sur le site du projet PAMPA : \n ")

    linkP <- "http://wwz.ifremer.fr/pampa/Partenaires"
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