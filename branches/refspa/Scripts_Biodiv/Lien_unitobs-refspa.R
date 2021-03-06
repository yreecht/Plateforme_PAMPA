#-*- coding: latin-1 -*-
# Time-stamp: <2012-11-23 10:32:45 yves>

## Plateforme PAMPA de calcul d'indicateurs de ressources & biodiversit�
##   Copyright (C) 2008-2012 Ifremer - Tous droits r�serv�s.
##
##   Ce programme est un logiciel libre ; vous pouvez le redistribuer ou le
##   modifier suivant les termes de la "GNU General Public License" telle que
##   publi�e par la Free Software Foundation : soit la version 2 de cette
##   licence, soit (� votre gr�) toute version ult�rieure.
##
##   Ce programme est distribu� dans l'espoir qu'il vous sera utile, mais SANS
##   AUCUNE GARANTIE : sans m�me la garantie implicite de COMMERCIALISABILIT�
##   ni d'AD�QUATION � UN OBJECTIF PARTICULIER. Consultez la Licence G�n�rale
##   Publique GNU pour plus de d�tails.
##
##   Vous devriez avoir re�u une copie de la Licence G�n�rale Publique GNU avec
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
### Scripts permettant de joindre le r�f�rentiel spatial � la table des unit�s d'observations.
####################################################################################################

########################################################################################################################
selectLink.interface.f <- function(unitobs, refspa,
                                   defaultLinks=c(unitobs=getOption("P.linkUnitobs"),
                                                  refspa=getOption("P.linkRefspa")))
{
    ## Purpose: Interface permettant � l'utilisateur de s�lectionner les
    ##          champs de correspondance entre table d'unit�s d'observations
    ##          et r�f�rentiel spatial.
    ## ----------------------------------------------------------------------
    ## Arguments: unitobs : table des unit�s d'observation ("data.frame").
    ##            refspa : r�f�rentiel spatial ("data.frame").
    ##            defaultLinks : vecteur des colonnes par d�faut
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 22 nov. 2012, 11:01

    runLog.f(msg=c("Interface de choix du lien unit�s d'observation et r�f�rentiel spatial."))

    ## .BGcolor <- "#FFFBCF"
    .BGcolor <- "#F7F5CE"

    ## Fen�tre principale :
    WinLink <- tktoplevel(background="white")
    tkwm.title(WinLink, "Lien unit�s d'observation -- r�f�rentiel spatial")

    ## Variables :
    Done <- tclVar(0)
    env <- environment()

    ColUnitobs <- tclVar(defaultLinks["unitobs"])
    ColRefspa <- tclVar(defaultLinks["refspa"])

    ## #### d�finition des �l�ments de l'interface :
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
                      text=paste("Veuillez choisir les colonnes correspondantes dans les donn�es d'unit�s",
                                  " d'observation et le r�f�rentiel spatial.", sep=""),
                      wraplength=500,
                      font=tkfont.create(weight="normal", size=10),
                      foreground="darkred",
                      background=.BGcolor, justify="left")

    L.help2 <- tklabel(F.help,
                      text=(paste("Dans la colonne du r�f�rentiel spatial, la valeur de chaque ligne doit",
                                  " �tre unique pour que le lien se fasse correctement.",
                      sep="")),
                      wraplength=500,
                      font=tkfont.create(weight="normal", size=10),
                      foreground="darkred",
                      background=.BGcolor, justify="left")

    ## Logo :
    TableLink <- tclVar()
    tcl("image", "create", "photo", TableLink, file=.fileimageLink) # cr�e un objet Tk image pour l'interface.
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

    ## #### Placement des �l�ments :

    tkgrid(L.help1, sticky="w")
    tkgrid(L.help2, sticky="w")
    tkgrid(Img.tableLink, sticky="ew")

    tkgrid(F.help, sticky="ew", columnspan=2)

    tkgrid(tklabel(F.unitobs, text="Colonne des unit�s d'observation : ", background="white"),
           padx=4, pady=4, sticky="w")
    tkgrid(CB.unitobs, padx=4, pady=4, sticky="w")

    tkgrid(tklabel(F.refspa, text="Colonne du r�f�rentiel spatial : ", background="white"),
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

    ## #### �v�nements :

    tkbind(WinLink, "<Destroy>", function(){tclvalue(Done) <- 2}) # En cas de destruction de la
                                        # fen�tre.

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
                                              "Le champs du r�f�rentiel spatial n'est pas unique !\n",
                                              "Pas de correspondance trouv�e !\n"),
                                       "Veuillez corriger la s�lection.", sep=""),
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

    ## Destruction si la fen�tre existe encore :
    if (tclvalue(tkwinfo("exists", WinLink)) == "1") tkdestroy(WinLink)

    return(links)
}


########################################################################################################################
selectLink.f <- function(unitobs, refspa, type="auto")
{
    ## Purpose: S�lection des colonnes permettant un lien entre unitobs et
    ##          refspa.
    ## ----------------------------------------------------------------------
    ## Arguments: unitobs : table des unit�s d'observation ("data.frame").
    ##            refspa : r�f�rentiel spatial ("data.frame").
    ##            type : type de lien (entre "auto" et "manual").
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 20 nov. 2012, 18:18

    ## Variable r�sultat,
    ## indiquant si une correspondance a �t� trouv�e (NULL == non-trouv�e) :
    links <- NULL

    ## Liens par d�faut depuis les options :
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
          }else{ ## ...Sinon, v�rification des d�fauts (options) :
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

    ## En cas de choix manuel ou �chec des pr�c�dentes �tapes :
    if (type[1] == "manual" || is.null(links))
    {
        links <- selectLink.interface.f(unitobs=unitobs, refspa=refspa, defaultLinks=defaultLinks)
    }else{}

    return(links)
}


########################################################################################################################
mergeSpaUnitobs.f <- function(unitobs, refspa, type="auto")
{
    ## Purpose: Fusion des la table des unitobs et du r�f�rentiel spatial si
    ##          adapt�.
    ## ----------------------------------------------------------------------
    ## Arguments: unitobs : table des unit�s d'observation.
    ##            refspa : r�f�rentiel spatial ("SpatialPolygonsDataFrame" ou
    ##                     "data.frame").
    ##            type : type de lien (entre "auto" et "manual").
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date:  7 d�c. 2011, 17:23

    ## Si issu d'un shapefile
    if (is.element("SpatialPolygonsDataFrame", class(refspa)))
    {
        ## Correspondance automatique des unitobs et du r�f�rentiel spatial :
        unitobs <- overlayUnitobs.f(unitobs=unitobs, refspa=refspa)

        refspa <- refspa@data
    }else{}

    ## Selection des colonnes de lien :
    links <- selectLink.f(unitobs=unitobs, refspa=refspa, type=type)

    if (is.null(links)) ## Rien n'est fait si pas de lien d�fini :
    {
        res <- unitobs
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

        res <- reorderStatus.f(Data=dropLevels.f(df=res, which=c("STATUT.PRO")),
                               which = c("STATUT.PRO"))

        res <- reorderStatus.f(Data=dropLevels.f(df=res, which=c("STATUT.PAM")),
                               which = c("STATUT.PAM"))
    }

    return(res)
}




### Local Variables:
### ispell-local-dictionary: "english"
### fill-column: 100
### End:
