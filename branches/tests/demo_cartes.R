#-*- coding: latin-1 -*-

### File: demo_cartes.R
### Time-stamp: <2011-05-26 15:14:41 yreecht>
###
### Author: Yves Reecht
###
####################################################################################################
### Description:
###
### Scripts de démonstartion de ce qui pourra être fait avec des carte.
### Pour l'instant uniquement pour la Nouvelle-Calédonie.
####################################################################################################

########################################################################################################################
selectionVariablesCarte.f <- function()
{
    ## Purpose: Sélection d'un variable pour démo de représentation
    ##          sur une carte (NC).
    ## ----------------------------------------------------------------------
    ## Arguments: aucun.
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 25 mai 2011, 14:29

    ## Variables :
    env <- environment()                    # Environnement courant
    Done <- tclVar(0)                       # Statut d'exécution

    nextStep <- "boxplot.unitobs"       # Nécessaire pour la mise à jour des listes de métriques.

    ## Liste des métriques :
    metriques <- champsMetriques.f("listespunit", nextStep)

    TableMetrique <- tclVar("listespunit")  # Table des métriques.
    MetriqueChoisie <- tclVar("")           # Métrique choisie


    ## ########################
    ## Éléments graphiques :
    WinSelection <- tktoplevel()          # Fenêtre principale
    tkwm.title(WinSelection, paste("Sélection de métrique pour carte", sep=""))

    ## Métriques :
    FrameMetrique <- tkframe(WinSelection, borderwidth=2, relief="groove")

    CB.metrique <- ttkcombobox(FrameMetrique, value=metriques, textvariable=MetriqueChoisie,
                               state="readonly")

    RB.listespunit <- tkradiobutton(FrameMetrique, variable=TableMetrique,
                                    value="listespunit", text="... / unité d'observation")
    RB.TableBiodiv <- tkradiobutton(FrameMetrique, variable=TableMetrique,
                                    value="TableBiodiv", text="...de biodiversité ( / unité d'observation)")

    FrameBT <- tkframe(WinSelection)
    B.OK <- tkbutton(FrameBT, text="  Lancer  ", command=function(){tclvalue(Done) <- 1})
    B.Cancel <- tkbutton(FrameBT, text=" Quitter ", command=function(){tclvalue(Done) <- 2})

    ## ############
    ## Évènements :
    tkbind(RB.listespunit, "<Leave>", function(){updateMetrique.f(nomTable=tclvalue(TableMetrique), env=env)})
    tkbind(RB.TableBiodiv, "<Leave>", function(){updateMetrique.f(nomTable=tclvalue(TableMetrique), env=env)})

    tkbind(WinSelection, "<Destroy>", function(){tclvalue(Done) <- 2})

    ## #############################
    ## Positionnement des éléments :
    tkgrid(tklabel(FrameMetrique, text="Métrique à représenter : "), sticky="w")

    tkgrid(RB.listespunit, sticky="w")
    tkgrid(RB.TableBiodiv, CB.metrique, tklabel(FrameMetrique, text=" \n"), sticky="w")

    tkgrid(FrameMetrique, column=1, columnspan=3, sticky="w", padx=7, pady=7)

    tkgrid(FrameBT, column=1, columnspan=3, padx=7, pady=7)

    tkgrid(B.OK, tklabel(FrameBT, text="      "), B.Cancel,
           tklabel(FrameBT, text="\n"))

    ## Update des fenêtres :
    tcl("update")

    ## tkfocus(WinSelection)
    winSmartPlace.f(WinSelection)

    repeat
    {
        tkwait.variable(Done)           # attente d'une modification de statut.

        if (tclvalue(Done) == "1")      # statut exécution OK.
        {
            ## Vérifications des variables (si bonnes, le statut reste 1) :
            tclvalue(Done) <- ifelse(tclvalue(MetriqueChoisie) == "", "0", "1")

            if (tclvalue(Done) != "1") {next()} # traitement en fonction du statut : itération
                                        # suivante si les variables ne sont pas bonnes.

            boxplotCarte.f(metrique=tclvalue(MetriqueChoisie),
                           tableMetrique=tclvalue(TableMetrique))
        }else{}

        if (tclvalue(Done) == "2") {break()} # statut d'exécution 'abandon' : on sort de la boucle.
    }

    tkdestroy(WinSelection)             # destruction de la fenêtre.
}

########################################################################################################################
boxplotCarte.f <- function(metrique, tableMetrique)
{
    ## Purpose: Créer la carte avec des boxplots sur les sites
    ## ----------------------------------------------------------------------
    ## Arguments: metrique : la métrique choisie.
    ##            tableMetrique : nom de la table de métriques.
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 25 mai 2011, 16:10

    pampaProfilingStart.f()

    ## Données pour la série de boxplots :
    if (tableMetrique == "TableBiodiv")
    {
        ## Pour les indices de biodiversité, il faut travailler sur les nombres... :
        tmpData <- subsetToutesTables.f(metrique="nombre", facteurs=c("site", "statut_protection"),
                                        selections=list(NA, NA), tableMetrique="listespunit",
                                        exclude = NULL, add=c("unite_observation", "code_espece"))
    }else{
        tmpData <- subsetToutesTables.f(metrique=metrique, facteurs=c("site", "statut_protection"),
                                        selections=list(NA, NA), tableMetrique="listespunit",
                                        exclude = NULL, add=c("unite_observation", "code_espece"))
    }

    if (tableMetrique == "TableBiodiv")
    {
        ## Calcul des indices de biodiversité sur sélection d'espèces :
        tmp <- calcBiodiv.f(Data=tmpData,
                            unitobs = "unite_observation", code.especes = "code_espece",
                            nombres = "nombre",
                            indices=metrique)

        ## On rajoute les anciennes colonnes :
        tmpData <- cbind(tmp,
                         tmpData[match(tmp$unite_observation, tmpData$unite_observation),
                                 !is.element(colnames(tmpData), colnames(tmp))])
    }else{
        tmpData <- na.omit(agregationTableParCritere.f(Data=tmpData,
                                                       metrique=metrique,
                                                       facteurs=c("unite_observation"),
                                                       listFact=c("site", "statut_protection")))
    }

    tmpData <- cbind(tmpData,
                     unitobs[match(tmpData$unite_observation, unitobs$unite_observation) ,
                             c("longitude", "latitude", "unite_observation")])

    ## fond de carte NC :
    MapNC <- readShapePoly(paste(basePath, "/shapefiles/NewCaledonia_v7.shp", sep=""),
                           verbose=TRUE, repair=FALSE, delete_null_obj=TRUE)

    ## Périphérique graphique :
    x11(width=50, height=30, pointsize=10)

    par(mgp=c(2, 1, 0))

    plot(MapNC,
         xlim=range(tmpData$longitude, na.rm=TRUE) + 0.05 * c(-1, 1) * diff(range(tmpData$longitude, na.rm=TRUE)),
         ylim=range(tmpData$latitude, na.rm=TRUE) + 0.05 * c(-1, 1) * diff(range(tmpData$latitude, na.rm=TRUE)),
         ## col=ifelse(MapNC$L1_ATTRIB == "oceanic", "lightblue", "lightyellow"),
         col=ifelse(!MapNC$LAND, "lightblue1", "lightyellow"),
         density=NA,
         xaxs="i", yaxs="i", axes=TRUE, bg=c("lightblue"),
         lwd=0.1)

    title(paste(Capitalize.f(varNames[metrique, "nom"]),
                " par statut de protection et par site", sep=""))
    mtext("longitude", side=1, line=3)
    mtext("Lattitude", side=2, line=3)

    X <- tapply(tmpData$longitude, tmpData$site, mean, na.rm=TRUE)
    Y <- tapply(tmpData$latitude, tmpData$site, mean, na.rm=TRUE)

    y <- tapply(tmpData[ , metrique], tmpData$site, function(x)x)
    x <- tapply(tmpData$statut_protection, tmpData$site, function(x)x)


    for (i in seq_along(X))
    {
        subplot(boxplot(y[[i]] ~ x[[i]],
                        ylim=c(0, max(tmpData[ , metrique], na.rm=TRUE)),
                        col=rev(heat.colors(nlevels(tmpData$statut_protection))),
                        main=paste("\n\n", names(x)[i]), sep=""),
                X[i], Y[i], size=c(0.5, 0.5), ## type="plt",
                pars=list(bg="white", fg="black", cex=0.6, xpd=NA, mgp=c(1.5, 0.5, 0),
                          tcl=-0.3))
    }
}






### Local Variables:
### ispell-local-dictionary: "english"
### fill-column: 120
### End:
