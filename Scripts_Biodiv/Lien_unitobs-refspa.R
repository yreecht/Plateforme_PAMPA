#-*- coding: latin-1 -*-
# Time-stamp: <2013-07-07 19:18:30 yreecht>

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

### File: Lien_unitobs-refspa.R
### Created: <2012-11-22 10:58:17 yves>
###
### Author: Yves Reecht
###
####################################################################################################
### Description:
###
### Scripts permettant de joindre le référentiel spatial à la table des unités d'observations.
####################################################################################################

########################################################################################################################
selectLink.interface.f <- function(unitobs, refspa,
                                   defaultLinks=c(unitobs=getOption("P.linkUnitobs"),
                                                  refspa=getOption("P.linkRefspa")))
{
    ## Purpose: Interface permettant à l'utilisateur de sélectionner les
    ##          champs de correspondance entre table d'unités d'observations
    ##          et référentiel spatial.
    ## ----------------------------------------------------------------------
    ## Arguments: unitobs : table des unités d'observation ("data.frame").
    ##            refspa : référentiel spatial ("data.frame").
    ##            defaultLinks : vecteur des colonnes par défaut
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 22 nov. 2012, 11:01

    runLog.f(msg=c("Interface de choix du lien unités d'observation et référentiel spatial."))

    ## .BGcolor <- "#FFFBCF"
    .BGcolor <- "#F7F5CE"

    ## Fenêtre principale :
    WinLink <- tktoplevel(background="white")
    tkwm.title(WinLink, "Lien unités d'observation -- référentiel spatial")

    ## Variables :
    Done <- tclVar(0)
    env <- environment()

    ## Valeur par défaut du champ pour unitobs :
    ColUnitobs <- if (is.element(defaultLinks["unitobs"],
                                 colnames(unitobs)))
      {
          tclVar(defaultLinks["unitobs"])
      }else{
          tclVar(colnames(unitobs)[1])
      }

    ## Valeur par défaut du champ pour refspa :
    ColRefspa <- if (is.element(defaultLinks["refspa"],
                                colnames(refspa)))
      {
          tclVar(defaultLinks["refspa"])
      }else{
          tclVar(colnames(refspa)[1])
      }

    ## #### définition des éléments de l'interface :
    ## Frame d'aide :
    F.help <- tkwidget(WinLink, "labelframe",
                       text="Aide", padx=4, pady=2,
                       height=30,
                       borderwidth=2, relief="groove",
                       font=tkfont.create(weight="bold", size=10),
                       foreground="black",
                       background=.BGcolor)

    ## F.help <- tkframe(WinLink, borderwidth=2, relief="groove",
    ##                   background=.BGcolor)

    L.help1 <- tklabel(F.help,
                      text=paste("Veuillez choisir les colonnes correspondantes dans les données d'unités",
                                  " d'observation et le référentiel spatial.", sep=""),
                      wraplength=500,
                      font=tkfont.create(weight="normal", size=10),
                      foreground="darkred",
                      background=.BGcolor, justify="left")

    L.help2 <- tklabel(F.help,
                      text=(paste("Dans la colonne du référentiel spatial, la valeur de chaque ligne doit",
                                  " être unique pour que le lien se fasse correctement.",
                      sep="")),
                      wraplength=500,
                      font=tkfont.create(weight="normal", size=10),
                      foreground="darkred",
                      background=.BGcolor, justify="left")

    ## Logo :
    TableLink <- tclVar()
    tcl("image", "create", "photo", TableLink, file=.fileimageLink) # crée un objet Tk image pour l'interface.
    Img.tableLink <- tklabel(F.help, image=TableLink, bg="white") # -> label avec image.

    ## Comboboxes :
    F.unitobs <- tkframe(WinLink, background="white")
    CB.unitobs <- ttkcombobox(F.unitobs, value=colnames(unitobs),
                              textvariable=ColUnitobs,
                              state="readonly", width=max(c(max(sapply(colnames(unitobs), nchar)), 23)),
                              background="white")

    F.refspa <- tkframe(WinLink, background="white")
    CB.refspa <- ttkcombobox(F.refspa, value=colnames(refspa),
                             textvariable=ColRefspa,
                             state="readonly", width=max(c(max(sapply(colnames(refspa), nchar)), 23)),
                             background="white")

    ## Boutons :
    F.BT <- tkframe(WinLink, background="white")

    B.OK <- tkbutton(F.BT, text="  OK  ", # bg=.BGcolor,
                     command=function(){tclvalue(Done) <- 1})

    B.Cancel <- tkbutton(F.BT, text=" Annuler ", # bg=.BGcolor,
                         command=function(){tclvalue(Done) <- 2})

    ## #### Placement des éléments :

    tkgrid(L.help1, sticky="w")
    tkgrid(L.help2, sticky="w")
    tkgrid(Img.tableLink, sticky="ew")

    tkgrid(F.help, sticky="ew", columnspan=2)

    tkgrid(tklabel(F.unitobs, text="Colonne des unités d'observation : ", background="white"),
           padx=4, pady=4, sticky="w")
    tkgrid(CB.unitobs, padx=4, pady=4, sticky="w")

    tkgrid(tklabel(F.refspa, text="Colonne du référentiel spatial : ", background="white"),
           padx=4, pady=4, sticky="w")
    tkgrid(CB.refspa, padx=4, pady=4, sticky="w")

    tkgrid(F.unitobs, F.refspa, sticky="w", padx=10)
    tkgrid.configure(F.refspa, sticky="e")

    tkgrid(tklabel(WinLink, background="white"))

    tkgrid(B.OK,
           tklabel(F.BT, text="           ", background="white"),
           B.Cancel, padx=12, pady=5, sticky="w")
    tkgrid(F.BT, padx=4, pady=8, columnspan=2, sticky="")
    tkgrid.configure(B.Cancel, sticky="e")

    ## #### évènements :

    tkbind(WinLink, "<Destroy>", function(){tclvalue(Done) <- 2}) # En cas de destruction de la
                                        # fenêtre.

    tkbind(CB.unitobs, "<Button-1>",
           expression(tkconfigure(L.help1,
                                  font=tkfont.create(weight="normal", size=10))))
    tkbind(CB.refspa, "<Button-1>",
           expression(tkconfigure(L.help1,
                                  font=tkfont.create(weight="normal", size=10))))

    tkfocus(WinLink)

    tcl("update")

    winSmartPlace.f(WinLink, xoffset=-5, yoffset=-10)

    repeat
    {
        tkwait.variable(Done)

        if (tclvalue(Done) == "1")
        {
            if ( ## Il y a effectivement de correspondances (non-NA) :
                tmpUniq <- any(is.element(tmpColU <- na.omit(unitobs[ , tclvalue(ColUnitobs)]),
                                          tmpColR <- na.omit(refspa[ , tclvalue(ColRefspa)]))) &&
                ## ...et les valeurs correspondantes de refspa sont uniques :
                length(tmpColR[is.element(tmpColR, tmpColU)]) ==
                length(unique(tmpColR[is.element(tmpColR, tmpColU)])))
            {
                links <- c(unitobs=tclvalue(ColUnitobs),
                           refspa=tclvalue(ColRefspa))
                break()
            }else{
                ## Avertissement :
                tkconfigure(L.help1,
                            text=paste(ifelse(tmpUniq,
                                              "Le champs du référentiel spatial n'est pas unique !\n",
                                              "Pas de correspondance trouvée !\n"),
                                       "Veuillez corriger la sélection.", sep=""),
                            font=tkfont.create(weight="bold", size=10),
                            justify="left")

                tkgrid.configure(L.help1, sticky="w")

                tclvalue(Done) <- "0"
            }
        }

        if (tclvalue(Done) == "2")
        {
            links <- NULL
            break()
        }else{}
    }

    ## Destruction si la fenêtre existe encore :
    if (tclvalue(tkwinfo("exists", WinLink)) == "1") tkdestroy(WinLink)

    return(links)
}


########################################################################################################################
selectLink.f <- function(unitobs, refspa, type="auto")
{
    ## Purpose: Sélection des colonnes permettant un lien entre unitobs et
    ##          refspa.
    ## ----------------------------------------------------------------------
    ## Arguments: unitobs : table des unités d'observation ("data.frame").
    ##            refspa : référentiel spatial ("data.frame").
    ##            type : type de lien (entre "auto" et "manual").
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 20 nov. 2012, 18:18

    ## Variable résultat,
    ## indiquant si une correspondance a été trouvée (NULL == non-trouvée) :
    links <- NULL

    ## Liens par défaut depuis les options :
    defaultLinks <- c(unitobs=getOption("P.linkUnitobs"),
                      refspa=getOption("P.linkRefspa"))

    ## Dans le cas "auto" :
    if (type[1] == "auto")
    {
        ## Recherche de correspondances issues d'un shapefile :
        if (is.element("OBJECTID", colnames(unitobs)) &&
            is.element("OBJECTID", colnames(refspa)) &&
            ! all(is.na(unitobs$OBJECTID)) &&
            ! all(is.na(refspa$OBJECTID)) &&
            any(is.element(unitobs$OBJECTID, refspa$OBJECTID)))
          {
              links <- c(unitobs="OBJECTID", refspa="OBJECTID")
          }else{ ## ...Sinon, vérification des défauts (options) :
              if (is.element(defaultLinks["unitobs"], colnames(unitobs)) &&
                  is.element(defaultLinks["refspa"], colnames(refspa)) &&
                  ! all(is.na(unitobs[ , defaultLinks["unitobs"]])) &&
                  ! all(is.na(refspa[ , defaultLinks["refspa"]])) &&
                  any(is.element(unitobs[ , defaultLinks["unitobs"]],
                                 refspa[ , defaultLinks["refspa"]])))
              {
                  links <- defaultLinks
              }else{}
          }
    }else{}

    ## En cas de choix manuel ou échec des précédentes étapes :
    if (type[1] == "manual" || is.null(links))
    {
        links <- selectLink.interface.f(unitobs=unitobs, refspa=refspa, defaultLinks=defaultLinks)
    }else{}

    return(links)
}


########################################################################################################################
mergeSpaUnitobs.f <- function(unitobs, refspa, type="auto")
{
    ## Purpose: Fusion des la table des unitobs et du référentiel spatial si
    ##          adapté.
    ## ----------------------------------------------------------------------
    ## Arguments: unitobs : table des unités d'observation.
    ##            refspa : référentiel spatial ("SpatialPolygonsDataFrame" ou
    ##                     "data.frame").
    ##            type : type de lien (entre "auto" et "manual").
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date:  7 déc. 2011, 17:23

    ## Si issu d'un shapefile
    if (isPoly <- is.element("SpatialPolygonsDataFrame", class(refspa)))
    {
        if (is.element("OBJECTID", colnames(slot(refspa, "data"))))
        {
            slot(refspa, "data")[ , "OBJECTID"] <- as.character(slot(refspa, "data")[ , "OBJECTID"])
        }else{}
        ## Correspondance automatique des unitobs et du référentiel spatial :
        unitobs <- overlayUnitobs.f(unitobs=unitobs, refspa=refspa)

        refspa <- refspa@data
    }else{}

    ## Selection des colonnes de lien :
    links <- selectLink.f(unitobs=unitobs, refspa=refspa, type=type)

    if (is.null(links)) ## Rien n'est fait si pas de lien défini :
    {
        res <- unitobs

        if (type[1] == "auto")
        {
            infoLoading.f(msg=paste("Référentiel spatial incorrect,",
                                    "\nvous ne pourrez pas traiter vos données aux différentes échelles spatiales !",
                                    sep=""),
                          icon="warning")
        }else{}

    }else{ ## ...Sinon, merge des deux tables :
        res <- merge(unitobs, refspa,
                     by.x=links["unitobs"], by.y=links["refspa"],
                     all.x=TRUE, all.y=FALSE,
                     suffixes = c(".KEEP",".SUPR"))

        res <- res[ , ! grepl("\\.SUPR$", colnames(res))]
        res <- res[ , colnames(res) != "station.1"]

        colnames(res) <- sub("\\.KEEP$", "", colnames(res))

        res <- res[ , c(colnames(unitobs),
                        colnames(res)[!is.element(colnames(res), colnames(unitobs))])]

        if (type[1] == "auto")
        {
            if (isPoly)
            {
                infoLoading.f(msg=paste("Le chargement du référentiel spatial géoréférencé a été effectué,",
                                        "\nvous pourrez avoir accès à l'ensemble des fonctionnalités cartographiques.",
                                        sep=""),
                              icon="info")
            }else{
                infoLoading.f(msg=paste("Le référentiel spatial qui a été chargé n'est pas géoréférencé.",
                                        "\nL'ensemble des fonctionnalités cartographiques ne seront pas disponibles.",
                                        sep=""),
                              icon="info")
            }
        }else{}

    }

    return(res)
}




### Local Variables:
### ispell-local-dictionary: "english"
### fill-column: 100
### End:
