################################################################################
# Nom               : PackagesManquants.r
# Type              : Programme
# Objet             : Teste s'il manque des packages pour l'éxécution des scripts WP3.
#                     Lance l'installation des packages manquants si besoin
# Input             : lecture du script TopMenu.r
# Output            : lancement de l'installation des scripts manquants
# Auteur            : Elodie Gamp
# R version         : 2.8.1
# Date de création  : septembre 2010
# Sources
################################################################################

## Purpose: Installation des packages manquants (appeler dans la fonction suivante)
## ----------------------------------------------------------------------
## Arguments: pack : liste des packages manquants (chaîne de caractères).
## ----------------------------------------------------------------------
installPack.f <- function(pack) {


    ## Packages disponibles en ligne :
    ## (si aucun dépôt n'est défini, le choix est donné à l'utilisateur).
    packDispo <- available.packages()[ , "Package"]

    ## Avertissement si des packages manquants ne sont pas disponibles :
    if (any(!is.element(pack, packDispo))) {
        tkmessageBox(message=paste("Le(s) package(s) : \n\n    * '",
          paste(pack[!is.element(pack, packDispo)], collapse="'\n    * '"),
            "'\n\nn'est (ne sont) pas disponible(s) !", sep=""),
          icon="warning")

        res <- "error"
    } else {
        res <- "ok"
    }

    ## Installation des packages disponibles :
    if (any(is.element(pack, packDispo))) {
        install.packages(pack[is.element(pack, packDispo)], dependencies="Depends")
    } else {}

    return(res)
}


## Purpose: Tester s'il manque des packages par rapport à la liste définie
##           dans le cas de packages manquants, proposer leur installation
## ----------------------------------------------------------------------
## Arguments: aucun
## ----------------------------------------------------------------------
loadPackages.f <- function() {
    require(tcltk)

    ## Packages nécessaires au bon fonctionnement de la plateforme PAMPA WP3 stats :
    requiredPack <- c("tcltk", "tkrplot", "vegan", "MASS", "TeachingDemos", "plotrix",
                      "rpart", "mvpart", "multcomp", "gamlss")

    ## Packages installés :
    installedPack <- installed.packages()[ , "Package"]

    if (any(!is.element(requiredPack, installedPack)))   {
   
        on.exit(tkdestroy(WinInstall))
        Done <- tclVar("0")             # Statut d'action utilisateur.

        ## Packages manquants :
        packManquants <- requiredPack[!is.element(requiredPack, installedPack)]

        ## Éléments d'interface :
        WinInstall <- tktoplevel()
        tkwm.title(WinInstall, "Packages manquants")

        B.Install <- tkbutton(WinInstall, text="   Installer   ", command=function(){tclvalue(Done) <- "1"})
        B.Cancel <- tkbutton(WinInstall, text="   Annuler   ", command=function(){tclvalue(Done) <- "2"})

        tkgrid(tklabel(WinInstall, text=" "))
        tkgrid(tklabel(WinInstall,
            text=paste("Le(s) package(s) suivant(s) est (sont) manquant(s) :\n\    * '",
                paste(packManquants, collapse="'\n    * '"),
                  "'\n\n Vous pouvez lancer leur installation \n",
                  "(connection internet active et droits d'administration requis).", sep=""),
                justify="left"),
               column=1, columnspan=3, sticky="w")

        tkgrid(tklabel(WinInstall, text=" "))

        tkgrid(B.Install, column=1, row=3, sticky="e")
        tkgrid(tklabel(WinInstall, text="       "), column=2, row=3)
        tkgrid(B.Cancel, column=3, row=3, sticky="w")

        tkgrid(tklabel(WinInstall, text=" "), column=4)

        tkbind(WinInstall, "<Destroy>", function(){tclvalue(Done) <- "2"})

        ## Attente d'une action de l'utilisateur :
        tkwait.variable(Done)

        if (tclvalue(Done) == "1") {
        
            tkdestroy(WinInstall)

            ## Installation des packages manquants :
            res <- installPack.f(pack=packManquants)
        } else {
            res <- "abord"
        }

    } else {
        ## Rien à faire, tous les packages requis sont installés.
        res <- "ok"
    }


    ## Traitement en fonction du statut de sortie :
    switch(res,
        ok = invisible(sapply(requiredPack, library, character.only=TRUE)),
        stop(paste("Vous devez installer manuellement le(s) package(s) :\n\n    * '",
            paste(requiredPack[!is.element(requiredPack, installed.packages()[ , "Package"])],
            collapse="\n    * '"),
            "'", sep="")))
}

## On lance le chargement :
loadPackages.f()

### Local Variables:
### ispell-local-dictionary: "english"
### fill-column: 120
### End
