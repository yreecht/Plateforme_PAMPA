ChoixOptionsGraphiquesBenthos.f <- function ()
{
    print("fonction ChoixOptionsGraphiques.f activée")

    choixGraphiques <- tktoplevel()
    tkwm.title(choixGraphiques, "Paramètres de sortie graphique")

    cMax <- tkcheckbutton(choixGraphiques)
    cTypeAn <- tkcheckbutton(choixGraphiques)
    cTypeAnStation <- tkcheckbutton(choixGraphiques)
    cTypeAnStatut <- tkcheckbutton(choixGraphiques)
    cTypeAnStationStatut <- tkcheckbutton(choixGraphiques)
    cTypeStation <- tkcheckbutton(choixGraphiques)
    cTypeStatut <- tkcheckbutton(choixGraphiques)
    cTypeStationStatut <- tkcheckbutton(choixGraphiques)
    cTypeSite <- tkcheckbutton(choixGraphiques)
    cTypeAnSite <- tkcheckbutton(choixGraphiques)
    cTypeCarac <- tkcheckbutton(choixGraphiques)
    cTypeAnCarac <- tkcheckbutton(choixGraphiques)
    cTypeCatbent <- tkcheckbutton(choixGraphiques)
    cTypeAnCatbent <- tkcheckbutton(choixGraphiques)
    cAfficheNbObs <- tkcheckbutton(choixGraphiques)
    cAffichePointMoyenne <- tkcheckbutton(choixGraphiques)
    cAfficheValeurMoyenne <- tkcheckbutton(choixGraphiques)
    cEnregistreEnPDF <- tkcheckbutton(choixGraphiques)
    cPlusieursGraphsParPage <- tkcheckbutton(choixGraphiques)
    cSeparateurRegroupement <- tkcheckbutton(choixGraphiques)

    cMaxValue <- tclVar("0")
    cTypeAnValue <- tclVar("0")
    cTypeAnStationValue <- tclVar("0")
    cTypeAnStatutValue <- tclVar("0")
    cTypeAnStationStatutValue <- tclVar("0")
    cTypeStationValue <- tclVar("0")
    cTypeStatutValue <- tclVar("0")
    cTypeStationStatutValue <- tclVar("0")
    cTypeSiteValue <- tclVar("0")
    cTypeAnSiteValue <- tclVar("0")
    cTypeCaracValue <- tclVar("0")
    cTypeAnCaracValue <- tclVar("0")
    cTypeCatbentValue <- tclVar("0")
    cTypeAnCatbentValue <- tclVar("0")
    cAfficheNbObsValue <- tclVar("0")
    cAffichePointMoyenneValue <- tclVar("0")
    cAfficheValeurMoyenneValue <- tclVar("0")
    NbMinObsPourGraphValue <- tclVar("1")
    cEnregistreEnPDFValue <- tclVar("0")
    cPlusieursGraphsParPageValue <- tclVar("0")
    cSeparateurRegroupementValue <- tclVar("1")

    entry.NbMinObsPourGraph <-tkentry(choixGraphiques, width="3", textvariable=NbMinObsPourGraphValue)

    ## Active l'option "plusieurs graphs par page" si la sortie est en pdf :
    OnClicPDF <- function()
    {
        if (tclvalue(cEnregistreEnPDFValue) == "1")
        {
            tkconfigure(cPlusieursGraphsParPage, state="normal")
        }else{
            tkconfigure(cPlusieursGraphsParPage, state="disabled")
        }
    }

    tkconfigure(cMax, variable=cMaxValue)
    tkconfigure(cTypeAn, variable=cTypeAnValue)
    tkconfigure(cTypeAnStation, variable=cTypeAnStationValue)
    tkconfigure(cTypeAnStatut, variable=cTypeAnStatutValue)
    tkconfigure(cTypeAnStationStatut, variable=cTypeAnStationStatutValue)
    tkconfigure(cTypeStation, variable=cTypeStationValue)
    tkconfigure(cTypeStatut, variable=cTypeStatutValue)
    tkconfigure(cTypeStationStatut, variable=cTypeStationStatutValue)
    tkconfigure(cTypeSite, variable=cTypeSiteValue)
    tkconfigure(cTypeAnSite, variable=cTypeAnSiteValue)
    tkconfigure(cTypeCarac, variable=cTypeCaracValue)
    tkconfigure(cTypeAnCarac, variable=cTypeAnCaracValue)
    tkconfigure(cTypeCatbent, variable=cTypeCatbentValue)
    tkconfigure(cTypeAnCatbent, variable=cTypeAnCatbentValue)
    tkconfigure(cAfficheNbObs, variable=cAfficheNbObsValue)
    tkconfigure(cAffichePointMoyenne, variable=cAffichePointMoyenneValue)
    tkconfigure(cAfficheValeurMoyenne, variable=cAfficheValeurMoyenneValue)
    tkconfigure(cEnregistreEnPDF, variable=cEnregistreEnPDFValue, command=OnClicPDF)
    tkconfigure(cPlusieursGraphsParPage, variable=cPlusieursGraphsParPageValue, state="disabled")
    tkconfigure(cSeparateurRegroupement, variable=cSeparateurRegroupementValue)

    tkgrid(tklabel(choixGraphiques, text=paste("Extraire les valeurs extrêmes maximales \nde la représentation",
                                    " graphique \n(supérieures à 95% de la valeur max)", sep="")), cMax, sticky="e")
    tkgrid(tklabel(choixGraphiques, text="\n"))
    tkgrid(tklabel(choixGraphiques, text="Graphiques par année"), cTypeAn, sticky="e")
    tkgrid(tklabel(choixGraphiques, text="Graphiques par année puis station"), cTypeAnStation, sticky="e")
    tkgrid(tklabel(choixGraphiques, text="Graphiques par année puis statut"), cTypeAnStatut, sticky="e")
    tkgrid(tklabel(choixGraphiques, text="Graphiques par année puis station puis statut"),
           cTypeAnStationStatut, sticky="e")
    tkgrid(tklabel(choixGraphiques, text="Graphiques par station"), cTypeStation, sticky="e")
    tkgrid(tklabel(choixGraphiques, text="Graphiques par statut"), cTypeStatut, sticky="e")
    tkgrid(tklabel(choixGraphiques, text="Graphiques par station puis statut"), cTypeStationStatut, sticky="e")
    tkgrid(tklabel(choixGraphiques, text="Graphiques par site"), cTypeSite, sticky="e")
    tkgrid(tklabel(choixGraphiques, text="Graphiques par année puis site"), cTypeAnSite, sticky="e")
    tkgrid(tklabel(choixGraphiques, text="Graphiques par caractéristique (2)"), cTypeCarac, sticky="e")
    tkgrid(tklabel(choixGraphiques, text="Graphiques par année puis  caractéristique (2)"), cTypeAnCarac, sticky="e")
    tkgrid(tklabel(choixGraphiques, text="Graphiques par catégorie benthique"), cTypeCatbent, sticky="e")
    tkgrid(tklabel(choixGraphiques, text="Graphiques par année puis catégorie benthique"), cTypeAnCatbent, sticky="e")
    tkgrid(tklabel(choixGraphiques, text="\n"))
    tkgrid(tklabel(choixGraphiques, text="Afficher les nombres d'enregistrement par boxplot (orange)"),
           cAfficheNbObs, sticky="e")
    tkgrid(tklabel(choixGraphiques, text="Afficher les moyennes sur les boxplot (point bleu)"),
           cAffichePointMoyenne, sticky="e")
    tkgrid(tklabel(choixGraphiques, text="Afficher les valeurs des moyennes sur les boxplot (en bleu)"),
           cAfficheValeurMoyenne, sticky="e")
    tkgrid(tklabel(choixGraphiques, text="Afficher les lignes de séparateurs de regroupement(en rouge)"),
           cSeparateurRegroupement, sticky="e")
    tkgrid(tklabel(choixGraphiques, text="\n"))
    tkgrid(tklabel(choixGraphiques, text="Supprimer les graphiques ayant moins de"))
    tkgrid(entry.NbMinObsPourGraph)
    tkgrid(tklabel(choixGraphiques, text=" Observations pour l'espèce.\n"))
    tkgrid(tklabel(choixGraphiques, text="Enregistrer ces graphiques dans un PDF"), cEnregistreEnPDF, sticky="e")
    tkgrid(tklabel(choixGraphiques, text="Placer plusieurs graphiques (2x2) par page"),
           cPlusieursGraphsParPage, sticky="e")

    if (length(unique(unitobs$caracteristique_2))<=1) # on vérifie que le regroupement puisse avoir lieu sur au moins
                                        # deux valeurs
    {
        tkconfigure(cTypeCarac, state="disabled")
        tkconfigure(cTypeAnCarac, state="disabled")
    }
    if (length(unique(unitobs$an))<=1) # on vérifie que le regroupement puisse avoir lieu sur au moins deux valeurs
    {
        tkconfigure(cTypeAn, state="disabled")
        tkconfigure(cTypeAnBiotope, state="disabled")
        tkconfigure(cTypeAnStatut, state="disabled")
        tkconfigure(cTypeAnBiotopeStatut, state="disabled")
        tkconfigure(cTypeAnSite, state="disabled")
        tkconfigure(cTypeAnCarac, state="disabled")
    }
    if (length(unique(unitobs$site))<=1) # on vérifie que le regroupement puisse avoir lieu sur au moins deux valeurs
    {
        tkconfigure(cTypeSite, state="disabled")
        tkconfigure(cTypeAnSite, state="disabled")
    }
    if (unique(unitobs$type)!="LIT") # on vérifie que le regroupement puisse avoir lieu sur au moins deux valeurs
    {
        tkconfigure(cTypeCatbent, state="disabled")
        tkconfigure(cTypeAnCatbent, state="disabled")
    }

    OnOK <- function()
    {
        cMaxVal <- as.character(tclvalue(cMaxValue))
        cTypeAnVal <- as.character(tclvalue(cTypeAnValue))
        cTypeAnStationVal <- as.character(tclvalue(cTypeAnStationValue))
        cTypeAnStatutVal <- as.character(tclvalue(cTypeAnStatutValue))
        cTypeAnStationStatutVal <- as.character(tclvalue(cTypeAnStationStatutValue))
        cTypeStationVal <- as.character(tclvalue(cTypeStationValue))
        cTypeStatutVal <- as.character(tclvalue(cTypeStatutValue))
        cTypeStationStatutVal <- as.character(tclvalue(cTypeStationStatutValue))
        cTypeSiteVal <- as.character(tclvalue(cTypeSiteValue))
        cTypeAnSiteVal <- as.character(tclvalue(cTypeAnSiteValue))
        cTypeCaracVal <- as.character(tclvalue(cTypeCaracValue))
        cTypeAnCaracVal <- as.character(tclvalue(cTypeAnCaracValue))
        cTypeCatbentVal <- as.character(tclvalue(cTypeCatbentValue))
        cTypeAnCatbentVal <- as.character(tclvalue(cTypeAnCatbentValue))
        cAfficheNbObsVal <- as.character(tclvalue(cAfficheNbObsValue))
        cAffichePointMoyenneVal <- as.character(tclvalue(cAffichePointMoyenneValue))
        cAfficheValeurMoyenneVal <- as.character(tclvalue(cAfficheValeurMoyenneValue))
        NbMinObsPourGraphVal <- tclvalue(NbMinObsPourGraphValue)
        cEnregistreEnPDFVal <- tclvalue(cEnregistreEnPDFValue)
        cPlusieursGraphsParPageVal <- as.integer(tclvalue(cPlusieursGraphsParPageValue))
        cSeparateurRegroupementVal <- tclvalue(cSeparateurRegroupementValue)
        tkdestroy(choixGraphiques)

        choixgraph <- c(
                        "maxExclu"=cMaxVal,    #1
                        "graphAn"=cTypeAnVal,
                        "graphAnStation"=cTypeAnStationVal,
                        "graphAnStatut"=cTypeAnStatutVal,
                        "graphAnStationStatut"=cTypeAnStationStatutVal, #5
                        "graphStation"=cTypeStationVal,
                        "graphStatut"=cTypeStatutVal,
                        "graphStationStatut"=cTypeStationStatutVal,
                        "NbObsOrange"=cAfficheNbObsVal,
                        "PtMoyenneBleu"=cAffichePointMoyenneVal,   #10
                        "ChiffreMoyenneBleu"=cAfficheValeurMoyenneVal,
                        "MinNbObs"=NbMinObsPourGraphVal,
                        "GraphEnPDF"=cEnregistreEnPDFVal,
                        "graphSite"=cTypeSiteVal,
                        "graphAnSite"=cTypeAnSiteVal,              #15
                        "graphCarac"=cTypeCaracVal,
                        "graphAnCarac"=cTypeAnCaracVal,
                        "graphCatbent"=cTypeCatbentVal,
                        "graphAnCatbent"=cTypeAnCatbentVal,
                        "separateurGroupe" = cSeparateurRegroupementVal, #variable 20
                        "plusieursGraphs"=cPlusieursGraphsParPageVal
                        )


        tkmessageBox(message="Executions lancées", icon="info")
        assign("choixgraph", choixgraph, envir=.GlobalEnv)
        print(choixgraph)
    }
    OK.but <- tkbutton(choixGraphiques, text="OK", command=OnOK)
    tkgrid(OK.but)
    tkfocus(choixGraphiques)
    tkwait.window(choixGraphiques)
}

## code

GraphiqueMetriqueXFacteurs.f <- function (metrique, facteurMenu, selectfacteurMenu)
{
    textstatut <- c("HR : Hors Réserve", "PP : Protection Partielle", "RE : En réserve")
    Nbdecimales <- 2
    spunit <- subset(objtable, objtable[, facteurMenu]==selectfacteurMenu)

    print(paste("graphique pour", selectfacteurMenu, " et la métrique", metrique, ": ", length(spunit[, metrique]),
                "observations"))
    spunit <- subset(objtable, objtable$biotope==selectbiotope) # [!!!] pourquoi plusieurs assignations de spunit ???
                                        # [yr: 01/08/2010]
    if (choixgraph["maxExclu"]==1)      # 1
    {
        spunit[, metrique][which(spunit[, metrique]>GraphPartMax*max(spunit[, metrique], na.rm=T))] <- NA
    }
    print(paste("NbObs pour", facteurMenu, " : ", length(spunit[, metrique])))
    if (length(unique(listespunit[, metrique]))>1)
    {
        ## on vérifie que la metrique a été calculée pour le facteur sélectionné
        if (length(unique(spunit[, metrique]))>=choixgraph["MinNbObs"]) # 12
        {


            ## #################### graphique par année ####################
            if (choixgraph["graphAn"] == 1)
            {
                if (choixgraph["GraphEnPDF"] == 0)
                {
                    x11(width=50, height=20, pointsize=10)
                }
                par(mar=c(10, 3, 3, 1), mgp=c(0, 0, 0))
                boxplot(spunit[, metrique] ~spunit$an, data=spunit, varwidth = TRUE, ylab=metrique, las=2,
                        main=paste("Valeurs de", metrique, "\n pour le champ '", facteurMenu, "' =", selectfacteurMenu,
                        " selon l'année \n"))

                if (choixgraph["NbObsOrange"]==1)
                {
                    nbObs <- tapply(spunit[, metrique], spunit$an, length)
                    axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)), col.ticks="orange",
                         col.axis = "orange", lty = 2, lwd = 0.5)
                    legend("topleft", "Nombre d'enregistrement par Boxplot",
                           cex =0.7, col="orange", text.col="orange", merge=FALSE)
                }
                Moyenne <- as.vector(tapply(spunit[, metrique], spunit$an, na.rm = T, mean))
                if (choixgraph["PtMoyenneBleu"]==1)
                {
                    points(Moyenne, pch=19, col="blue")
                }
                if (choixgraph["ChiffreMoyenneBleu"]==1)
                {
                    text(Moyenne+(Moyenne/10), col = "blue", cex = 0.8,
                         labels=as.character(round(Moyenne, digits=Nbdecimales)))
                }
                abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")
                if (choixgraph["maxExclu"]==1)
                {
                    legend("top", "Enregistrements > 95% du maximum retirés",
                           cex =0.7, col="red", text.col="red", merge=FALSE)
                }
            } # fin choixgraph["graphAn"]==1
        }else{
            tkmessageBox(message=paste("Nombres d'enregistrements inssuffisants \n",
                         "d'après votre choix de nombre minimum d'observations pour", MaFamille))
        }
    }
    assign("tablegraph", spunit, envir=.GlobalEnv)
}#fin de fonction  MetriqueUnfacteurMenuDansGraphique

Graphgenerique.f <- function (facteurMenu, facteurmultiGraph, metrique=NULL)
{
    print("fonction Graphgenerique.f activée")

    textstatut <- c("HR : Hors Réserve", "PP : Protection Partielle", "RE : En réserve")
    Nbdecimales <- 2

    matable <- "listespunit"
    objtable <- eval(parse(text=matable))
    if (is.null(metrique))
    {
        choixchamptable.f(matable)
        metrique <- champtrouve
    }
    print(metrique)
    ## Choix du facteur de regroupement factesp pour facteurmultiGraph
    critereespref.f()
    print(factesp)
    objtable[, factesp] <- especes[, factesp][match(objtable$code_espece, especes$code_espece)]
    objtable[, factesp] <- factor(objtable[, factesp])

    especes$Cath_benthique <- factor(especes$Cath_benthique,
                                     levels=c("AA", "CA", "FMA", "HMA", "TA", "ACB", "ACD", "ACE",
                                     "ACS", "ACT", "CB", "CE", "CF", "CM", "CMR", "CS", "CME", "HC",
                                     "CBL", "CHL", "CTU", "OT", "SC", "SP", "ZO", "RKC", "DC",
                                     "DCA", "R", "RC", "S", "SI"))
    objtable$Famille <- especes$Famille[match(objtable$code_espece, especes$code_espece)]
    objtable$Famille <- factor(objtable$Famille)
    objtable$Genre <- especes$Genre[match(objtable$code_espece, especes$code_espece)]
    objtable$Genre <- factor(objtable$Genre)
    objtable$espece <- especes$espece[match(objtable$code_espece, especes$code_espece)]
    objtable$espece <- factor(objtable$espece)
    objtable$Cath_benthique <- especes$Cath_benthique[match(objtable$code_espece, especes$code_espece)]
    objtable$Cath_benthique <- factor(objtable$Cath_benthique)
    objtable$station <- unitobs$station[match(objtable$unite_observation, unitobs$unite_observation)]
    objtable$station <- factor(objtable$station)
    objtable$site <- unitobs$site[match(objtable$unite_observation, unitobs$unite_observation)]
    objtable$site <- factor(objtable$site)

    ## ChoixFacteurSelect.f(objtable$biotope, "biotope", "multiple", 1, "selectbiotope")
    ## objtable = subset(objtable, objtable$biotope==selectbiotope)

    ChoixFacteurSelect.f(objtable[, facteurMenu], facteurMenu, "multiple", 1, "selectfacteurMenu")
    ChoixOptionsGraphiquesBenthos.f()
    print(choixgraph)

    print(paste("Graphique en PDF :", choixgraph["GraphEnPDF"])) # 13
    for (i in 1:length(selectfacteurMenu))
    {
        if (choixgraph["GraphEnPDF"]==1) # 13
        {
            nomPDF <- paste(nameWorkspace, "/FichiersSortie/",
                            selectfacteurMenu[i], "_", unique(unitobs$type), ".pdf", sep="")
            pdf(nomPDF, encoding="ISOLatin1", family="URWHelvetica")

        }
        print(paste("graphique sur", metrique, "selon la valeur",
                    selectfacteurMenu[i], "de", facteurMenu, "avec 1 graph par", facteurmultiGraph, "lancé"))
        ## GraphiqueMetriqueXFacteurs.f(metrique, facteurMenu, selectfacteurMenu[i])
        if (choixgraph["GraphEnPDF"]==1) # 13
        {
            tkmessageBox(message=paste("vos graphiques par an seront enregistrés dans ", nomPDF))
            dev.off()
        }
    }
}#fin graphgenerique

## #############################################################
## #################Graph benthos ###########################
## #############################################################

Graphbenthos.f <- function (metrique, facteurMenu, espouunit=NULL)
{#1 pour esp et 2 pour unit
    print("fonction Graphbenthos.f activée")

    textstatut <- c("HR : Hors Réserve", "PP : Protection Partielle", "RE : En réserve")
    Nbdecimales <- 2

    matable <- "listespunit"
    objtable <- eval(parse(text=matable))

    especes$Cath_benthique <- factor(especes$Cath_benthique,
                                     levels=c("AA", "CA", "FMA", "HMA", "TA", "ACB", "ACD", "ACE", "ACS", "ACT", "CB",
                                     "CE", "CF", "CM", "CMR", "CS", "CME", "HC", "CBL", "CHL", "CTU", "OT", "SC", "SP",
                                     "ZO", "RKC", "DC", "DCA", "R", "RC", "S", "SI"))
    objtable$Famille <- especes$Famille[match(objtable$code_espece, especes$code_espece)]
    objtable$Famille <- factor(objtable$Famille)
    objtable$Genre <- especes$Genre[match(objtable$code_espece, especes$code_espece)]
    objtable$Genre <- factor(objtable$Genre)
    objtable$espece <- especes$espece[match(objtable$code_espece, especes$code_espece)]
    objtable$espece <- factor(objtable$espece)
    objtable$Cath_benthique <- especes$Cath_benthique[match(objtable$code_espece, especes$code_espece)]
    objtable$Cath_benthique <- factor(objtable$Cath_benthique)
    objtable$station <- unitobs$station[match(objtable$unite_observation, unitobs$unite_observation)]
    objtable$station <- factor(objtable$station)
    objtable$site <- unitobs$site[match(objtable$unite_observation, unitobs$unite_observation)]
    objtable$site <- factor(objtable$site)
    if(!is.null(espouunit))   #1 pour esp et 2 pour unit
    {
        if(espouunit==1)
        {
            objtable[, facteurMenu] <- especes[, facteurMenu][match(objtable$code_espece, especes$code_espece)]
            objtable[, facteurMenu] <- factor(objtable[, facteurMenu])
        }
        if(espouunit==2)
        {
            objtable[, facteurMenu] <- unitobs[, facteurMenu][match(objtable$unite_observation,
                                                                    unitobs$unite_observation)]
            objtable[, facteurMenu] <- factor(objtable[, facteurMenu])
        }
    }
    ChoixFacteurSelect.f(objtable$biotope, "biotope", "multiple", 1, "selectbiotope")
    objtable <- subset(objtable, objtable$biotope==selectbiotope)
    ChoixFacteurSelect.f(objtable[, facteurMenu], facteurMenu, "multiple", 1, "selectfacteurMenu")
    ChoixOptionsGraphiquesBenthos.f()
    print(choixgraph)

    MetriqueUnfacteurMenuDansGraphique.f <- function (metrique, facteurMenu, selectfacteurMenu)
    {
        textstatut <- c("HR : Hors Réserve", "PP : Protection Partielle", "RE : En réserve")
        Nbdecimales <- 2
        spunit <- subset(objtable, objtable[, facteurMenu]==selectfacteurMenu)

        print(paste("graphique pour", selectfacteurMenu,
                    " et la métrique", metrique, ": ", length(spunit[, metrique]), "observations"))
        spunit <- subset(objtable, objtable$biotope==selectbiotope) # pourquoi deux lignes de définition de spunit ???
                                        # [!!!] [yr: 01/08/2010]
        if (choixgraph["maxExclu"]==1)  # 1
        {
            spunit[, metrique][which(spunit[, metrique]>GraphPartMax*max(spunit[, metrique], na.rm=T))] <- NA
        }
        print(paste("NbObs pour", facteurMenu, " : ", length(spunit[, metrique])))
        if (length(unique(listespunit[, metrique]))>1)
        {
            ## on vérifie que la metrique a été calculée pour le facteur sélectionné
            if (length(unique(spunit[, metrique]))>=choixgraph["MinNbObs"]) # 12
            {


                ## #################### graphique par année ####################
                if (choixgraph["graphAn"]==1)
                {
                    if (choixgraph["GraphEnPDF"]==0)
                    {
                        x11(width=50, height=20, pointsize=10)
                    }
                    par(mar=c(10, 3, 5, 1), mgp=c(3, 1, 0))
                    boxplot(spunit[, metrique] ~spunit$an, data=spunit, varwidth = TRUE, ylab=metrique, las=2,
                            main=paste("Valeurs de", metrique, "\n pour le champ '", facteurMenu, "' =",
                            selectfacteurMenu, " selon l'année \n"))

                    if (choixgraph["NbObsOrange"]==1)
                    {
                        nbObs <- tapply(spunit[, metrique], spunit$an, length)
                        axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)),
                             col.ticks="orange", col.axis = "orange", lty = 2, lwd = 0.5)
                        legend("topleft", "Nombre d'enregistrement par Boxplot",
                               cex =0.7, col="orange", text.col="orange", merge=FALSE)
                    }
                    Moyenne <- as.vector(tapply(spunit[, metrique], spunit$an, na.rm = T, mean))
                    if (choixgraph["PtMoyenneBleu"]==1)
                    {
                        points(Moyenne, pch=19, col="blue")
                    }
                    if (choixgraph["ChiffreMoyenneBleu"]==1)
                    {
                        text(Moyenne+(Moyenne/10), col = "blue", cex = 0.8,
                             labels=as.character(round(Moyenne, digits=Nbdecimales)))
                    }
                    abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")
                    if (choixgraph["maxExclu"]==1)
                    {
                        legend("top", "Enregistrements > 95% du maximum retirés",
                               cex =0.7, col="red", text.col="red", merge=FALSE)
                    }
                } # fin choixgraph["graphAn"]==1


                ## #################### graphique par AN et PAR STATION ####################
                if (choixgraph["graphAnStation"]==1)
                {
                    if (choixgraph["GraphEnPDF"]==0)
                    {
                        x11(width=50, height=20, pointsize=10)
                    }
                    par(mar=c(14, 3, 5, 1), mgp=c(3, 1, 0))
                    boxplot(spunit[, metrique] ~spunit$station+spunit$an, data=spunit,
                            varwidth = TRUE, ylab=metrique, las=2,
                            col=heat.colors(length(unique(spunit$statut_protection))),
                            main=paste("Valeurs de", metrique, "\n pour le champ '", facteurMenu,
                            "' =", selectfacteurMenu, "\n selon la station et l'année"))
                    if (choixgraph["NbObsOrange"]==1)
                    {
                        nbObs <- tapply(spunit[, metrique], list(spunit$station, spunit$an), length)
                        axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)),
                             col.ticks="orange", col.axis = "orange", lty = 2, lwd = 0.5)
                        legend("topleft", "Nombre d'enregistrement par Boxplot",
                               cex =0.7, col="orange", text.col="orange", merge=FALSE)
                    }
                    Moyenne <- as.vector(tapply(spunit[, metrique], list(spunit$station, spunit$an), mean))
                    if (choixgraph["PtMoyenneBleu"]==1)
                    {
                        points(Moyenne, pch=19, col="blue")
                    }
                    if (choixgraph["ChiffreMoyenneBleu"]==1)
                    {
                        text(Moyenne+(Moyenne/10), col = "blue", cex = 0.8,
                             labels=as.character(round(Moyenne, digits=Nbdecimales)))
                    }
                    abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")
                    if (choixgraph["maxExclu"]==1)
                    {
                        legend("top", "Enregistrements > 95% du maximum retirés",
                               cex =0.7, col="red", text.col="red", merge=FALSE)
                    }
                }

                ## #################### graphique par AN et PAR STATUT ####################
                if (choixgraph["graphAnStatut"]==1)
                {
                    if (choixgraph["GraphEnPDF"]==0) # 13
                    {
                        x11(width=50, height=20, pointsize=10)
                    }
                    par(mar=c(10, 3, 5, 1), mgp=c(3, 1, 0))
                    boxplot(spunit[, metrique] ~spunit$statut_protection+spunit$an, data=spunit,
                            varwidth = TRUE, ylab=metrique, las=2,
                            col=heat.colors(length(unique(spunit$statut_protection))),
                            main=paste("Valeurs de", metrique, "\n pour le champ '", facteurMenu,
                            "' =", selectfacteurMenu, "\n selon le statut et l'année\n"))
                    if (length(unique(spunit$statut_protection))==3)
                    {
                        legend("topright", textstatut, col=heat.colors(length(unique(spunit$statut_protection))),
                               pch = 15, cex =0.9, title="Statuts")
                    }
                    if (choixgraph["NbObsOrange"]==1) # 9
                    {
                        nbObs <- tapply(spunit[, metrique], list(spunit$statut_protection, spunit$an), length)
                        axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)),
                             col.ticks="orange", col.axis = "orange", lty = 2, lwd = 0.5)
                        legend("topleft", "Nombre d'enregistrement par Boxplot",
                               cex =0.7, col="orange", text.col="orange", merge=FALSE)
                    }
                    Moyenne <- as.vector(tapply(spunit[, metrique],
                                                list(spunit$statut_protection, spunit$an), na.rm = T, mean))
                    if (choixgraph["PtMoyenneBleu"]==1) # 10
                    {
                        points(Moyenne, pch=19, col="blue")
                    }
                    if (choixgraph["ChiffreMoyenneBleu"]==1) # 11
                    {
                        text(Moyenne+(Moyenne/10), col = "blue", cex = 0.8,
                             labels=as.character(round(Moyenne, digits=Nbdecimales)))
                    }
                    abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")
                    if (choixgraph["maxExclu"]==1) # 1
                    {
                        legend("top", "Enregistrements > 95% du maximum retirés",
                               cex =0.7, col="red", text.col="red", merge=FALSE)
                    }
                }
                ## #################### graphique par STATUT, PAR AN et par STATION  ####################
                if (choixgraph["graphAnStationStatut"]==1) # 5
                {
                    if (choixgraph["GraphEnPDF"]==0) # 13
                    {
                        x11(width=50, height=20, pointsize=10)
                    }
                    par(mar=c(18, 5, 5, 0), mgp=c(3, 1, 0))
                    spunit$station <- spunit$station[ , drop=TRUE]

                    boxplot(spunit[, metrique] ~spunit$statut_protection+spunit$an+spunit$station, data=spunit,
                            varwidth = TRUE, ylab=metrique, las=2,
                            col=heat.colors(length(unique(spunit$statut_protection))),
                            main=paste("Valeurs de", metrique, "\n pour le champ '", facteurMenu,
                            "' =", selectfacteurMenu, " selon le statut, l'année et la station\n\n"))
                    if (length(unique(spunit$statut_protection))==3)
                    {
                        legend("topright", textstatut, col=heat.colors(length(unique(spunit$statut_protection))),
                               pch = 15, cex =0.9, title="Statuts")
                    }
                    if (choixgraph["NbObsOrange"]==1) # 9
                    {
                        nbObs <- tapply(spunit[, metrique],
                                        list(spunit$statut_protection, spunit$an, spunit$station), length)
                        axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)),
                             col.ticks="orange", col.axis = "orange", lty = 2, lwd = 0.5)
                        legend("topleft", "Nombre d'enregistrement par Boxplot",
                               cex =0.7, col="orange", text.col="orange", merge=FALSE)
                    }
                    Moyenne <- as.vector(tapply(spunit[, metrique],
                                                list(spunit$statut_protection, spunit$an, spunit$station),
                                                na.rm = T, mean)) # [!!!]
                    if (choixgraph["PtMoyenneBleu"]==1)           # 10
                    {
                        points(Moyenne, pch=19, col="blue")
                    }
                    if (choixgraph["ChiffreMoyenneBleu"] == 1)   # 11
                    {
                        text(Moyenne+(Moyenne/10), col = "blue", cex = 0.8,
                             labels=as.character(round(Moyenne, digits=Nbdecimales)))
                    }
                    if (choixgraph["separateurGroupe"]==1)
                    {
                        abline(v = 0.5+(1:(length(as.vector(unique(spunit$station))) *
                               length(as.vector(unique(spunit$an))))) *
                               length(as.vector(unique(spunit$statut_protection))), col = "red")
                    }
                    abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")
                    if (choixgraph["maxExclu"] == 1)   # 1
                    {
                        legend("top", "Enregistrements > 95% du maximum retirés",
                               cex =0.7, col="red", text.col="red", merge=FALSE)
                    }
                }
                ## #################### graphique par STATION  ####################
                if (choixgraph["graphStation"] == 1)   # 6
                {
                    if (choixgraph["GraphEnPDF"] == 0)   # 13
                    {
                        x11(width=50, height=20, pointsize=10)
                    }
                    par(mar=c(14, 3, 5, 1), mgp=c(3, 1, 0))
                    boxplot(spunit[, metrique] ~spunit$station, data=spunit, varwidth = TRUE,
                            ylab=metrique, las=2,
                            main=paste("Valeurs de", metrique, "\n pour le champ '", facteurMenu,
                            "' =", selectfacteurMenu, "\n selon la station \n"))

                    if (choixgraph["NbObsOrange"] == 1)   # 9
                    {
                        nbObs <- tapply(spunit[, metrique], spunit$station, length)
                        axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)),
                             col.ticks="orange", col.axis = "orange", lty = 2, lwd = 0.5)
                        legend("topleft", "Nombre d'enregistrement par Boxplot",
                               cex =0.7, col="orange", text.col="orange", merge=FALSE)
                    }
                    Moyenne <- as.vector(tapply(spunit[, metrique], spunit$station, na.rm = T, mean))
                    if (choixgraph["PtMoyenneBleu"] == 1) # [!!!] inversion ?   # 10
                    {
                        points(Moyenne, pch=19, col="blue")
                    }
                    if (choixgraph["ChiffreMoyenneBleu"] == 1) # [!!!] inversion ?   # 11
                    {
                        text(Moyenne+(Moyenne/10), col = "blue", cex = 0.8,
                             labels=as.character(round(Moyenne, digits=Nbdecimales)))
                    }

                    abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")
                    if (choixgraph["maxExclu"] == 1)   # 1
                    {
                        legend("top", "Enregistrements > 95% du maximum retirés",
                               cex =0.7, col="red", text.col="red", merge=FALSE)
                    }
                }
                ## #################### graphique par STATUT   ####################
                if (choixgraph["graphStatut"] == 1)   # 7
                {
                    if (choixgraph["GraphEnPDF"] == 0)   # 13
                    {
                        x11(width=50, height=20, pointsize=10)
                    }
                    par(mar=c(14, 3, 5, 1), mgp=c(3, 1, 0))
                    ## boxplot(spunit[, metrique] ~spunit$Famille, data=spunit, varwidth = TRUE, ylab=metrique, las=2,
                    ## main=paste(metrique, " \n pour la famille : ", spunit$Famille, "\npour toutes les unités d'obs"))

                    boxplot(spunit[, metrique] ~spunit$statut_protection, data=spunit,
                            varwidth = TRUE, ylab=metrique, las=2,
                            main=paste("Valeurs de", metrique, "\n pour le champ '", facteurMenu,
                            "' =", selectfacteurMenu, "\nselon le statut \n"))
                    if (length(unique(spunit$statut_protection))==3)
                    {
                        legend("topright", textstatut, col=heat.colors(length(unique(spunit$statut_protection))),
                               pch = 15, cex =0.9, title="Statuts")
                    }
                    if (choixgraph["NbObsOrange"] == 1)   # 9
                    {
                        nbObs <- tapply(spunit[, metrique], spunit$statut_protection, length)
                        axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)),
                             col.ticks="orange", col.axis = "orange", lty = 2, lwd = 0.5)
                        legend("topleft", "Nombre d'enregistrement par Boxplot",
                               cex =0.7, col="orange", text.col="orange", merge=FALSE)
                    }
                    Moyenne <- as.vector(tapply(spunit[, metrique], spunit$statut_protection, na.rm = T, mean))
                    if (choixgraph["PtMoyenneBleu"] == 1)   # 10
                    {
                        points(Moyenne, pch=19, col="blue")
                    }
                    if (choixgraph["ChiffreMoyenneBleu"] == 1)   # 11
                    {
                        text(Moyenne+(Moyenne/10), col = "blue", cex = 0.8,
                             labels=as.character(round(Moyenne, digits=Nbdecimales)))
                    }

                    abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")
                    if (choixgraph["maxExclu"] == 1)   # 1
                    {
                        legend("top", "Enregistrements > 95% du maximum retirés",
                               cex =0.7, col="red", text.col="red", merge=FALSE)
                    }
                }
                ## #################### graphique par STATUT et PAR STATION ####################
                if (choixgraph["graphStationStatut"] == 1)   # 8
                {
                    if (choixgraph["GraphEnPDF"] == 0)   # 13
                    {
                        x11(width=50, height=20, pointsize=10)
                    }
                    par(mar=c(14, 3, 5, 1), mgp=c(3, 1, 0))
                    boxplot(spunit[, metrique] ~spunit$statut_protection+spunit$station, data=spunit,
                            varwidth = TRUE, ylab=metrique, las=2,
                            col=heat.colors(length(unique(spunit$statut_protection))),
                            main=paste("Valeurs de", metrique, "\n pour le champ '", facteurMenu,
                            "' =", selectfacteurMenu, "\n selon le statut et la station\n"))
                    if (length(unique(spunit$statut_protection))==3)
                    {
                        legend("topright", textstatut, col=heat.colors(length(unique(spunit$statut_protection))),
                               pch = 15, cex =0.9, title="Statuts")
                    }
                    if (choixgraph["NbObsOrange"] == 1)   # 9
                    {
                        nbObs <- tapply(spunit[, metrique],
                                        list(spunit$statut_protection, spunit$station), length)
                        axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)),
                             col.ticks="orange", col.axis = "orange", lty = 2, lwd = 0.5)
                        legend("topleft", "Nombre d'enregistrement par Boxplot",
                               cex =0.7, col="orange", text.col="orange", merge=FALSE)
                    }
                    Moyenne <- as.vector(tapply(spunit[, metrique],
                                                list(spunit$statut_protection, spunit$station), na.rm = T, mean))
                    if (choixgraph["PtMoyenneBleu"] == 1)   # 10
                    {
                        points(Moyenne, pch=19, col="blue")
                    }
                    if (choixgraph["ChiffreMoyenneBleu"] == 1)   # 11
                    {
                        text(Moyenne+(Moyenne/10), col = "blue", cex = 0.8,
                             labels=as.character(round(Moyenne, digits=Nbdecimales)))
                    }
                    if (choixgraph["separateurGroupe"]==1)
                    {
                        abline(v = 0.5+(1:length(as.vector(unique(spunit$station)))) * # [!!!] préséance de l'opérateur
                               length(as.vector(unique(spunit$statut_protection))), col = "red") # ":" (origine probable
                                        # des bugs sur les lignes verticales)
                    }
                    abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")
                    if (choixgraph["maxExclu"] == 1)   # 1
                    {
                        legend("top", "Enregistrements > 95% du maximum retirés",
                               cex =0.7, col="red", text.col="red", merge=FALSE)
                    }
                }

                ## oDFJKQFKLSDHFGSDLQGHFJKLSDFGHSDLDFGSDLJKGH

                ## #################### graphique par SITE ####################
                if (choixgraph["graphSite"] == 1)   # 14
                {
                    if (choixgraph["GraphEnPDF"] == 0)   # 13
                    {
                        x11(width=50, height=20, pointsize=10)
                    }
                    par(mar=c(14, 3, 5, 1), mgp=c(3, 1, 0))
                    boxplot(spunit[, metrique] ~ spunit$site, data=spunit,
                            varwidth=TRUE, ylab=metrique, las=2,
                            main=paste("Valeurs de", metrique, "\n pour le champ '", facteurMenu,
                            "' =", selectfacteurMenu, "\n selon le site \n"))

                    if (choixgraph["NbObsOrange"] == 1)   # 9
                    {
                        nbObs <- tapply(spunit[, metrique], spunit$site, length)
                        axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)), col.ticks="orange",
                             col.axis = "orange", lty = 2, lwd = 0.5)
                        legend("topleft", "Nombre d'enregistrement par Boxplot", cex =0.7,
                               col="orange", text.col="orange", merge=FALSE)
                    }
                    Moyenne <- as.vector(tapply(spunit[, metrique], spunit$site, mean))
                    if (choixgraph["PtMoyenneBleu"] == 1)   # 10
                    {
                        points(Moyenne, pch=19, col="blue")
                    }
                    if (choixgraph["ChiffreMoyenneBleu"] == 1)   # 11
                    {
                        text(Moyenne+(Moyenne/10), col = "blue", cex = 0.8,
                             labels=as.character(round(Moyenne, digits=Nbdecimales)))
                    }
                    abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")
                    if (choixgraph["maxExclu"] == 1)   # 1
                    {
                        legend("top", "Enregistrements > 95% du maximum retirés", cex =0.7,
                               col="red", text.col="red", merge=FALSE)
                    }
                }
                ## #################### graphique par AN et PAR SITE ####################
                if (choixgraph["graphAnSite"] == 1)   # 15
                {
                    if (choixgraph["GraphEnPDF"] == 0)   # 13
                    {
                        x11(width=50, height=20, pointsize=10)
                    }
                    par(mar=c(14, 3, 5, 1), mgp=c(3, 1, 0))
                    boxplot(spunit[, metrique] ~spunit$site+spunit$an, data=spunit,
                            varwidth = TRUE, ylab=metrique, las=2, col=heat.colors(length(unique(spunit$site))),
                            main=paste("Valeurs de", metrique, "\n pour le champ '", facteurMenu,
                            "' =", selectfacteurMenu, "\n selon le site et l'année"))
                    if (choixgraph["NbObsOrange"] == 1)   # 9
                    {
                        nbObs <- tapply(spunit[, metrique], list(spunit$site, spunit$an), length)
                        axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)),
                             col.ticks="orange", col.axis = "orange", lty = 2, lwd = 0.5)
                        legend("topleft", "Nombre d'enregistrement par Boxplot",
                               cex =0.7, col="orange", text.col="orange", merge=FALSE)
                    }
                    Moyenne <- as.vector(tapply(spunit[, metrique], list(spunit$site, spunit$an), mean))
                    if (choixgraph["PtMoyenneBleu"] == 1)   # 10
                    {
                        points(Moyenne, pch=19, col="blue")
                    }
                    if (choixgraph["ChiffreMoyenneBleu"] == 1)   # 11
                    {
                        text(Moyenne+(Moyenne/10), col = "blue", cex = 0.8,
                             labels=as.character(round(Moyenne, digits=Nbdecimales)))
                    }
                    abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")
                    if (choixgraph["separateurGroupe"]==1)
                    {
                        abline(v = 0.5+(1:length(as.vector(unique(spunit$an))))*length(as.vector(unique(spunit$site))),
                               col = "red") # [!!!]
                    }
                    if (choixgraph["maxExclu"] == 1)   # 1
                    {
                        legend("top", "Enregistrements > 95% du maximum retirés",
                               cex =0.7, col="red", text.col="red", merge=FALSE)
                    }
                }
                ## #################### graphique par Caract2 ####################
                if (choixgraph["graphCarac"] == 1)   # 16
                {
                    if (choixgraph["GraphEnPDF"] == 0)   # 13
                    {
                        x11(width=50, height=20, pointsize=10)
                    }
                    par(mar=c(14, 3, 5, 1), mgp=c(3, 1, 0))
                    boxplot(spunit[, metrique] ~spunit$caracteristique_2, data=spunit,
                            varwidth = TRUE, ylab=metrique, las=2,
                            main=paste("Valeurs de", metrique, "\n pour le champ '", facteurMenu,
                            "' =", selectfacteurMenu, "\n selon le caracteristique_2 \n"))

                    if (choixgraph["NbObsOrange"] == 1)   # 9
                    {
                        nbObs <- tapply(spunit[, metrique], spunit$caracteristique_2, length)
                        axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)),
                             col.ticks="orange", col.axis = "orange", lty = 2, lwd = 0.5)
                        legend("topleft", "Nombre d'enregistrement par Boxplot",
                               cex =0.7, col="orange", text.col="orange", merge=FALSE)
                    }
                    Moyenne <- as.vector(tapply(spunit[, metrique], spunit$caracteristique_2, mean))
                    if (choixgraph["PtMoyenneBleu"] == 1)   # 10
                    {
                        points(Moyenne, pch=19, col="blue")
                    }
                    if (choixgraph["ChiffreMoyenneBleu"] == 1)   # 11
                    {
                        text(Moyenne+(Moyenne/10), col = "blue", cex = 0.8,
                             labels=as.character(round(Moyenne, digits=Nbdecimales)))
                    }
                    abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")
                    if (choixgraph["maxExclu"] == 1)   # 1
                    {
                        legend("top", "Enregistrements > 95% du maximum retirés",
                               cex =0.7, col="red", text.col="red", merge=FALSE)
                    }
                }
                ## #################### graphique par AN et PAR Caract2 ####################
                if (choixgraph["graphAnCarac"] == 1)   # 17
                {
                    if (choixgraph["GraphEnPDF"] == 0)   # 13
                    {
                        x11(width=50, height=20, pointsize=10)
                    }
                    par(mar=c(14, 3, 5, 1), mgp=c(3, 1, 0))
                    boxplot(spunit[, metrique] ~spunit$caracteristique_2+spunit$an, data=spunit,
                            varwidth = TRUE, ylab=metrique, las=2,
                            col=heat.colors(length(unique(spunit$caracteristique_2))),
                            main=paste("Valeurs de", metrique, "\n pour le champ '", facteurMenu,
                            "' =", selectfacteurMenu, "\n selon caracteristique_2 et l'année"))
                    if (choixgraph["NbObsOrange"] == 1)   # 9
                    {
                        nbObs <- tapply(spunit[, metrique], list(spunit$caracteristique_2, spunit$an), length)
                        axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)),
                             col.ticks="orange", col.axis = "orange", lty = 2, lwd = 0.5)
                        legend("topleft", "Nombre d'enregistrement par Boxplot",
                               cex =0.7, col="orange", text.col="orange", merge=FALSE)
                    }
                    Moyenne <- as.vector(tapply(spunit[, metrique], list(spunit$caracteristique_2, spunit$an), mean))
                    if (choixgraph["PtMoyenneBleu"] == 1)   # 10
                    {
                        points(Moyenne, pch=19, col="blue")
                    }
                    if (choixgraph["ChiffreMoyenneBleu"] == 1)   # 11
                    {
                        text(Moyenne+(Moyenne/10), col = "blue", cex = 0.8,
                             labels=as.character(round(Moyenne, digits=Nbdecimales)))
                    }
                    abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")
                    if (choixgraph["separateurGroupe"]==1)
                    {
                        abline(v = 0.5+(1:length(as.vector(unique(spunit$an)))) *
                               length(as.vector(unique(spunit$caracteristique_2))), col = "red")
                    }
                    if (choixgraph["maxExclu"] == 1)   # 1
                    {
                        legend("top", "Enregistrements > 95% du maximum retirés",
                               cex =0.7, col="red", text.col="red", merge=FALSE)
                    }
                }
                ## #################### graphique par Cath_benthique ####################
                if (choixgraph["graphCatbent"] == 1)   # 18
                {
                    if (choixgraph["GraphEnPDF"] == 0)   # 13
                    {
                        x11(width=50, height=20, pointsize=10)
                    }
                    par(mar=c(14, 3, 5, 1), mgp=c(3, 1, 0))
                    boxplot(spunit[, metrique] ~ spunit$Cath_benthique, data=spunit,
                            varwidth = TRUE, ylab=metrique, las=2,
                            main=paste("Valeurs de", metrique, "\n pour le champ '", facteurMenu,
                            "' =", selectfacteurMenu, "\n selon 'Cath_benthique' \n"))

                    if (choixgraph["NbObsOrange"] == 1)   # 9
                    {
                        nbObs <- tapply(spunit[, metrique], spunit$Cath_benthique, length)
                        axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)),
                             col.ticks="orange", col.axis = "orange", lty = 2, lwd = 0.5)
                        legend("topleft", "Nombre d'enregistrement par Boxplot",
                               cex =0.7, col="orange", text.col="orange", merge=FALSE)
                    }
                    Moyenne <- as.vector(tapply(spunit[, metrique], spunit$Cath_benthique, mean))
                    if (choixgraph["PtMoyenneBleu"] == 1)   # 10
                    {
                        points(Moyenne, pch=19, col="blue")
                    }
                    if (choixgraph["ChiffreMoyenneBleu"] == 1)   # 11
                    {
                        text(Moyenne+(Moyenne/10), col = "blue", cex = 0.8,
                             labels=as.character(round(Moyenne, digits=Nbdecimales)))
                    }
                    abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")

                    if (choixgraph["maxExclu"] == 1)   # 1
                    {
                        legend("top", "Enregistrements > 95% du maximum retirés",
                               cex =0.7, col="red", text.col="red", merge=FALSE)
                    }
                }
                ## #################### graphique par AN et PAR Cath_benthique ####################
                if (choixgraph["graphAnCatbent"] == 1)   # 19
                {
                    if (choixgraph["GraphEnPDF"] == 0)   # 13
                    {
                        x11(width=50, height=20, pointsize=10)
                    }
                    par(mar=c(14, 3, 5, 1), mgp=c(3, 1, 0))
                    spunit$Cath_benthique <- spunit$Cath_benthique[ , drop=TRUE]
                    boxplot(spunit[, metrique] ~spunit$Cath_benthique+spunit$an, data=spunit,
                            varwidth = TRUE, ylab=metrique, las=2,
                            col=heat.colors(length(unique(spunit$Cath_benthique))),
                            main=paste("Valeurs de", metrique, "\n pour le champ '", facteurMenu,
                            "' =", selectfacteurMenu, "\n selon 'Cath_benthique' et l'année"))
                    if (choixgraph["NbObsOrange"] == 1)   # 9
                    {
                        nbObs <- tapply(spunit[, metrique], list(spunit$Cath_benthique, spunit$an), length)
                        axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)),
                             col.ticks="orange", col.axis = "orange", lty = 2, lwd = 0.5)
                        legend("topleft", "Nombre d'enregistrement par Boxplot",
                               cex =0.7, col="orange", text.col="orange", merge=FALSE)
                    }
                    Moyenne <- as.vector(tapply(spunit[, metrique], list(spunit$Cath_benthique, spunit$an), mean))
                    if (choixgraph["PtMoyenneBleu"] == 1)   # 10
                    {
                        points(Moyenne, pch=19, col="blue")
                    }
                    if (choixgraph["ChiffreMoyenneBleu"] == 1)   # 11
                    {
                        text(Moyenne+(Moyenne/10), col = "blue", cex = 0.8,
                             labels=as.character(round(Moyenne, digits=Nbdecimales)))
                    }
                    abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")
                    if (choixgraph["separateurGroupe"]==1)
                    {
                        abline(v = 0.5+(1:length(as.vector(unique(spunit$an)))) *
                               length(as.vector(unique(spunit$Cath_benthique))), col = "red")
                    }
                    if (choixgraph["maxExclu"] == 1)   # 1
                    {
                        legend("top", "Enregistrements > 95% du maximum retirés",
                               cex =0.7, col="red", text.col="red", merge=FALSE)
                    }
                }
                ## #################### graphique PAR Cath_benthique PUIS par AN ####################
                if (choixgraph["graphAnCatbent"] == 1)   # 19
                {
                    if (choixgraph["GraphEnPDF"] == 0)   # 13
                    {
                        x11(width=50, height=20, pointsize=10)
                    }
                    par(mar=c(14, 3, 5, 1), mgp=c(3, 1, 0))
                    spunit$Cath_benthique <- spunit$Cath_benthique[ , drop=TRUE]
                    ## browser()
                    boxplot(spunit[ , metrique] ~ spunit$an + spunit$Cath_benthique, data=spunit,
                            varwidth = TRUE, ylab=metrique, las=2,
                            col=heat.colors(length(unique(spunit$an))),
                            main=paste("Valeurs de", metrique, "\n pour le champ '", facteurMenu,
                            "' =", selectfacteurMenu, "\n selon l'année et la 'Cath_benthique'"))
                    if (choixgraph["NbObsOrange"] == 1)   # 9
                    {
                        nbObs <- tapply(spunit[, metrique], list(spunit$an, spunit$Cath_benthique), length)
                        axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)),
                             col.ticks="orange", col.axis = "orange", lty = 2, lwd = 0.5)
                        legend("topleft", "Nombre d'enregistrement par Boxplot",
                               cex =0.7, col="orange", text.col="orange", merge=FALSE)
                    }
                    Moyenne <- as.vector(tapply(spunit[, metrique], list(spunit$an, spunit$Cath_benthique), mean))
                    if (choixgraph["PtMoyenneBleu"] == 1)   # 10
                    {
                        points(Moyenne, pch=19, col="blue")
                    }
                    if (choixgraph["ChiffreMoyenneBleu"] == 1)   # 11
                    {
                        text(Moyenne+(Moyenne/10), col = "blue", cex = 0.8,
                             labels=as.character(round(Moyenne, digits=Nbdecimales)))
                    }
                    abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")
                    if (choixgraph["separateurGroupe"]==1)
                    {
                        abline(v = 0.5+(1:length(as.vector(unique(spunit$Cath_benthique)))) *
                               length(as.vector(unique(spunit$an))), col = "red")
                    }
                    if (choixgraph["maxExclu"] == 1)   # 1
                    {
                        legend("top", "Enregistrements > 95% du maximum retirés",
                               cex =0.7, col="red", text.col="red", merge=FALSE)
                    }
                }
            }else{
                tkmessageBox(message=paste("Nombres d'enregistrements inssuffisants \n",
                             "d'après votre choix de nombre minimum d'observations pour", MaFamille))
            }
        }
        assign("tablegraph", spunit, envir=.GlobalEnv)
    }#fin de fonction  MetriqueUnfacteurMenuDansGraphique

    print(paste("Graphique en PDF :", choixgraph[13]))
    for (i in 1:length(selectfacteurMenu))
    {
        if (choixgraph["GraphEnPDF"] == 1)   # 13
        {
            nomPDF <- paste(nameWorkspace, "/FichiersSortie/",
                            selectfacteurMenu[i], "_", unique(unitobs$type), ".pdf", sep="")
            pdf(nomPDF, encoding="ISOLatin1", family="URWHelvetica")
            if (choixgraph["plusieursGraphs"] == 1 & i == 1)
            {
                ## nomPDF=paste(nameWorkspace, "/FichiersSortie/", selectfacteurMenu[i], ".pdf", sep="")
                ## pdf(nomPDF, encoding="ISOLatin1", family="URWHelvetica")
                par(mfrow=c(2, 2))
            }else{
                ## if (choixgraph["plusieursGraphs"] == 0)
                ## {
                ## nomPDF=paste(nameWorkspace, "/FichiersSortie/", selectfacteurMenu[i], "_", unique(unitobs$type), ".pdf", sep="")
                ## pdf(nomPDF, encoding="ISOLatin1", family="URWHelvetica")
                ## }
            }

        }
        MetriqueUnfacteurMenuDansGraphique.f(metrique, facteurMenu, selectfacteurMenu[i])
        if (choixgraph["GraphEnPDF"] == 1)   # 13
        {
            tkmessageBox(message=paste("vos graphiques par an seront enregistrés dans ", nomPDF))
            dev.off()
        }
    }
}



