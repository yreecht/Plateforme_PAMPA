#-*- coding: latin-1 -*-

## Plateforme PAMPA de calcul d'indicateurs de ressources & biodiversité
##   Copyright (C) 2008-2010 Ifremer - Tous droits réservés.
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

### File: test_load_packages.R
### Time-stamp: <2012-01-15 14:50:18 yves>
###
### Author: Yves Reecht
###
####################################################################################################
### Description:
###
### Scripts pour le chargement des packages et leur installation si besoin.
####################################################################################################


installPack.f <- function(pack)
{
    ## Purpose: Installation des packages manquants
    ## ----------------------------------------------------------------------
    ## Arguments: pack : liste des packages manquants (chaîne de caractères).
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date:  3 sept. 2010, 13:43

    ## Packages disponibles en ligne :
    ## (si aucun dépôt n'est défini, le choix est donné à l'utilisateur).
    packDispo <- available.packages()[ , "Package"]

    ## Avertissement si des packages manquants ne sont pas disponibles :
    if (any(!is.element(pack, packDispo)))
    {
        tkmessageBox(message=paste("Le(s) package(s) : \n\n    * '",
                                   paste(pack[!is.element(pack, packDispo)], collapse="'\n    * '"),
                                   "'\n\nn'est (ne sont) pas disponible(s) !", sep=""),
                     icon="warning")

        res <- "error"
    }else{
        res <- "ok"
    }

    ## Installation des packages disponibles :
    if (any(is.element(pack, packDispo)))
    {
        install.packages(pack[is.element(pack, packDispo)], dependencies="Depends")
    }else{}

    return(res)
}

loadPackages.f <- function()
{
    ## Purpose: Charger les packages nécessaires et proposer l'installation
    ##          de ceux qui sont manquants.
    ## ----------------------------------------------------------------------
    ## Arguments: aucun
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date:  3 sept. 2010, 12:58


    require(tcltk)

    ## Pour régler un bug sur certaines versions de R (e.g. 2.11.1)
    ## (chemin des packages avec une installation dans les dossiers utilisateurs)
    ## ...soue Windows uniquement :
    if (.Platform$OS.type == "windows")
    {
        env <- environment(.libPaths)
        assign(".lib.loc", shortPathName(get(".lib.loc", envir=env)), envir=env)
        ##  -> tous les chemins en format court !
    }else{}                             # rien sinon.

    ## Packages nécessaires au bon fonctionnement de la plateforme PAMPA WP2 :
    requiredPack <- c("tcltk", "tkrplot", "vegan", "MASS",
                      "mvpart", "multcomp", "gamlss", "maps", "maptools", ## "Hmisc"
                      "TeachingDemos")

    ## Packages installés :
    installedPack <- installed.packages()[ , "Package"]

    if (any(!is.element(requiredPack, installedPack)))
    {
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

        if (tclvalue(Done) == "1")
        {
            tkdestroy(WinInstall)

            ## Installation des packages manquants :
            res <- installPack.f(pack=packManquants)
        }else{
            res <- "abord"
        }

    }else{
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
### End:
