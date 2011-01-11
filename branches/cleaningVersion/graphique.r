## choix d'extraire les valeurs extrêmes des graph
extremes.f <- function ()
{
    print("fonction extremes.f activée")
    choixExtremes <- tktoplevel()
    tkwm.title(choixExtremes, "Extrêmes")
    cb <- tkcheckbutton(choixExtremes)
    cbValue <- tclVar("0")
    tkconfigure(cb, variable=cbValue)
    tkgrid(tklabel(choixExtremes, text="Extraire les valeurs extrêmes maximales de la représentation graphique (supérieures à 95% de la valeur max)"), cb)

    OnOK <- function()
    {
        cbVal <- as.character(tclvalue(cbValue))
        tkdestroy(choixExtremes)
        if (cbVal=="1")
        {
            choix <- 1
            tkmessageBox(message="Les valeurs extrêmes maximales sont retirées de la représentation graphique", icon="info")
        }
        if (cbVal=="0")
        {
            choix <- 0
            tkmessageBox(message="Les valeurs extrêmes maximales sont conservées dans la représentation graphique", icon="info")
        }
        assign("choix", choix, envir=.GlobalEnv)
    }
    ## GraphPartMax peut être modifié par un slide puis retourné

    OK.but <- tkbutton(choixExtremes, text="OK", command=OnOK)
    tkgrid(OK.but)
    tkfocus(choixExtremes)
    tkwait.window(choixExtremes)
}

ChoixOptionsGraphiques.f <- function ()
{
    print("fonction ChoixOptionsGraphiques.f activée")

    choixGraphiques <- tktoplevel()
    tkwm.title(choixGraphiques, "Paramètres de sortie graphique")

    cMax <- tkcheckbutton(choixGraphiques)
    cTypeAn <- tkcheckbutton(choixGraphiques)
    cTypeAnBiotope <- tkcheckbutton(choixGraphiques)
    cTypeAnStatut <- tkcheckbutton(choixGraphiques)
    cTypeAnBiotopeStatut <- tkcheckbutton(choixGraphiques)
    cTypeBiotope <- tkcheckbutton(choixGraphiques)
    cTypeStatut <- tkcheckbutton(choixGraphiques)
    cTypeBiotopeStatut <- tkcheckbutton(choixGraphiques)
    cTypeSite <- tkcheckbutton(choixGraphiques)
    cTypeAnSite <- tkcheckbutton(choixGraphiques)
    cTypeCarac <- tkcheckbutton(choixGraphiques)
    cTypeAnCarac <- tkcheckbutton(choixGraphiques)
    cAfficheNbObs <- tkcheckbutton(choixGraphiques)
    cAffichePointMoyenne <- tkcheckbutton(choixGraphiques)
    cAfficheValeurMoyenne <- tkcheckbutton(choixGraphiques)

    cMaxValue <- tclVar("0")
    cTypeAnValue <- tclVar("0")
    cTypeAnBiotopeValue <- tclVar("0")
    cTypeAnStatutValue <- tclVar("0")
    cTypeAnBiotopeStatutValue <- tclVar("0")
    cTypeBiotopeValue <- tclVar("0")
    cTypeStatutValue <- tclVar("0")
    cTypeBiotopeStatutValue <- tclVar("0")
    cTypeSiteValue <- tclVar("0")
    cTypeAnSiteValue <- tclVar("0")
    cTypeCaracValue <- tclVar("0")
    cTypeAnCaracValue <- tclVar("0")
    cAfficheNbObsValue <- tclVar("0")
    cAffichePointMoyenneValue <- tclVar("0")
    cAfficheValeurMoyenneValue <- tclVar("0")
    NbMinObsPourGraphValue <- tclVar("1")

    entry.NbMinObsPourGraph <-tkentry(choixGraphiques, width="3", textvariable=NbMinObsPourGraphValue)

    tkconfigure(cMax, variable=cMaxValue)
    tkconfigure(cTypeAn, variable=cTypeAnValue)
    tkconfigure(cTypeAnBiotope, variable=cTypeAnBiotopeValue)
    tkconfigure(cTypeAnStatut, variable=cTypeAnStatutValue)
    tkconfigure(cTypeAnBiotopeStatut, variable=cTypeAnBiotopeStatutValue)
    tkconfigure(cTypeBiotope, variable=cTypeBiotopeValue)
    tkconfigure(cTypeStatut, variable=cTypeStatutValue)
    tkconfigure(cTypeBiotopeStatut, variable=cTypeBiotopeStatutValue)
    tkconfigure(cTypeSite, variable=cTypeSiteValue)
    tkconfigure(cTypeAnSite, variable=cTypeAnSiteValue)
    tkconfigure(cTypeCarac, variable=cTypeCaracValue)
    tkconfigure(cTypeAnCarac, variable=cTypeAnCaracValue)
    tkconfigure(cAfficheNbObs, variable=cAfficheNbObsValue)
    tkconfigure(cAffichePointMoyenne, variable=cAffichePointMoyenneValue)
    tkconfigure(cAfficheValeurMoyenne, variable=cAfficheValeurMoyenneValue)

    tkgrid(tklabel(choixGraphiques, text="Extraire les valeurs extrêmes maximales \nde la représentation graphique \n(supérieures à 95% de la valeur max)"), cMax, sticky="e")
    tkgrid(tklabel(choixGraphiques, text="\n"))
    tkgrid(tklabel(choixGraphiques, text="LES CHOIX DES FACTEURS NE CONTENANT\n QU'UNE VALEUR SONT DESACTIVES"))
    tkgrid(tklabel(choixGraphiques, text="Graphiques par année"), cTypeAn, sticky="e")
    tkgrid(tklabel(choixGraphiques, text="Graphiques par année puis biotope"), cTypeAnBiotope, sticky="e")
    tkgrid(tklabel(choixGraphiques, text="Graphiques par année puis statut"), cTypeAnStatut, sticky="e")
    tkgrid(tklabel(choixGraphiques, text="Graphiques par année puis biotope puis statut"), cTypeAnBiotopeStatut, sticky="e")
    tkgrid(tklabel(choixGraphiques, text="Graphiques par biotope"), cTypeBiotope, sticky="e")
    tkgrid(tklabel(choixGraphiques, text="Graphiques par statut"), cTypeStatut, sticky="e")
    tkgrid(tklabel(choixGraphiques, text="Graphiques par biotope puis statut"), cTypeBiotopeStatut, sticky="e")
    tkgrid(tklabel(choixGraphiques, text="Graphiques par site"), cTypeSite, sticky="e")
    tkgrid(tklabel(choixGraphiques, text="Graphiques par année puis site"), cTypeAnSite, sticky="e")
    tkgrid(tklabel(choixGraphiques, text="Graphiques par caractéristique (2)"), cTypeCarac, sticky="e")
    tkgrid(tklabel(choixGraphiques, text="Graphiques par année puis  caractéristique (2)"), cTypeAnCarac, sticky="e")
    tkgrid(tklabel(choixGraphiques, text="\n"))
    tkgrid(tklabel(choixGraphiques, text="Afficher les nombres d'enregistrement par boxplot (orange)"), cAfficheNbObs, sticky="e")
    tkgrid(tklabel(choixGraphiques, text="Afficher les moyennes sur les boxplot (point bleu)"), cAffichePointMoyenne, sticky="e")
    tkgrid(tklabel(choixGraphiques, text="Afficher les valeurs des moyennes sur les boxplot (en bleu)"), cAfficheValeurMoyenne, sticky="e")
    tkgrid(tklabel(choixGraphiques, text="\n"))
    tkgrid(tklabel(choixGraphiques, text="Supprimer les graphiques ayant moins de"))
    tkgrid(entry.NbMinObsPourGraph)
    tkgrid(tklabel(choixGraphiques, text=" Observations pour l'espèce.\n"))

    if (length(unique(unitobs$caracteristique_2))<=1) # on vérifie que le regroupement puisse avoir lieu sur au moins deux valeurs
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

    OnOK <- function()
    {
        cMaxVal <- as.character(tclvalue(cMaxValue))
        cTypeAnVal <- as.character(tclvalue(cTypeAnValue))
        cTypeAnBiotopeVal <- as.character(tclvalue(cTypeAnBiotopeValue))
        cTypeAnStatutVal <- as.character(tclvalue(cTypeAnStatutValue))
        cTypeAnBiotopeStatutVal <- as.character(tclvalue(cTypeAnBiotopeStatutValue))
        cTypeBiotopeVal <- as.character(tclvalue(cTypeBiotopeValue))
        cTypeStatutVal <- as.character(tclvalue(cTypeStatutValue))
        cTypeBiotopeStatutVal <- as.character(tclvalue(cTypeBiotopeStatutValue))
        cTypeSiteVal <- as.character(tclvalue(cTypeSiteValue))
        cTypeAnSiteVal <- as.character(tclvalue(cTypeAnSiteValue))
        cTypeCaracVal <- as.character(tclvalue(cTypeCaracValue))
        cTypeAnCaracVal <- as.character(tclvalue(cTypeAnCaracValue))
        cAfficheNbObsVal <- as.character(tclvalue(cAfficheNbObsValue))
        cAffichePointMoyenneVal <- as.character(tclvalue(cAffichePointMoyenneValue))
        cAfficheValeurMoyenneVal <- as.character(tclvalue(cAfficheValeurMoyenneValue))
        NbMinObsPourGraphVal <- tclvalue(NbMinObsPourGraphValue)
        tkdestroy(choixGraphiques)

        ## Nommer les valeurs dans ce qui suit pour rendre l'utilisation plus claire [yr: 23/07/2010]
        choixgraph <- c("maxExclu" = cMaxVal, # 1
                        "graphAn" = cTypeAnVal,
                        "graphAnBiotope" = cTypeAnBiotopeVal, # 3
                        "graphAnStatut" = cTypeAnStatutVal,
                        "graphAnBiotopeStatut" = cTypeAnBiotopeStatutVal, # 5
                        "graphBiotope" = cTypeBiotopeVal,
                        "graphStatut" = cTypeStatutVal, #7
                        "graphBiotopeStatut" = cTypeBiotopeStatutVal,
                        "NbObsOrange" = cAfficheNbObsVal,
                        "PtMoyenneBleu" = cAffichePointMoyenneVal, # 10
                        "ChiffreMoyenneBleu" = cAfficheValeurMoyenneVal,
                        "MinNbObs" = NbMinObsPourGraphVal, # 12
                        "graphSite" = cTypeSiteVal,
                        "graphAnSite" = cTypeAnSiteVal,
                        "graphCarac" = cTypeCaracVal,  # 15
                        "graphAnCarac" = cTypeAnCaracVal,
                        "GraphEnPDF"=cEnregistreEnPDFVal, # 17
                        "separateurGroupe" = cSeparateurRegroupementVal,
                        "plusieursGraphs"=cPlusieursGraphsParPageVal # 19
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


EnleverMaxExtremes.f <- function (tabExtremes)
{
    print("fonction EnleverMaxExtremes.f activée")

    if (choix == 1)
    {
        print(max(tabExtremes, na.rm=T))
        if (mode(tabExtremes) =="list")
        {
            tabExtremes[tabExtremes>GraphPartMax*max(tabExtremes, na.rm=T)] <- NA
        }else{
            tabExtremes[which(tabExtremes>GraphPartMax*max(tabExtremes, na.rm=T))] <- NA
        }
    }else{
        print("pas d'extrêmes enlevés")
        tabExtremes <- tabExtremes
    }
    return(tabExtremes)
}

UneFamilleDansPDF.f <- function ()
{
    print("fonction UneFamilleDansPDF.f activée")

    ChoixOptionsGraphiques.f()
    listespunit$Famille <- especes$Famille[match(listespunit$code_espece, especes$code_espece)]
    FamillesPresentes <- subset(listespunit$Famille, listespunit$pres_abs==1)
    ChoixFacteurSelect.f(FamillesPresentes, "Famille", "multiple", 1, "selectPDFFamille")

    print(choixgraph)
    print(selectPDFFamille)

    MetriqueUneFamilleDansPDF.f <- function (metrique, MaFamille)
    {
        textstatut <- c("HR : Hors Réserve", "PP : Protection Partielle", "RE : En réserve")
        Nbdecimales <- 2
        monspunit <- subset(listespunit, listespunit$Famille==MaFamille)
        spunit <- monspunit
        print(paste("graphique pour", MaFamille, " et la métrique", metrique, ": ", length(spunit[, metrique]), "observations"))
        if (choixgraph["maxExclu"] == 1)   # 1
        {
            spunit[, metrique][which(spunit[, metrique]>GraphPartMax*max(spunit[, metrique], na.rm=T))] <- NA
        }

        if (length(unique(listespunit[, metrique]))>1)
        {
            ## on vérifie que la metrique a été calculée pour l'espèce sélectionnée
            if (length(unique(spunit[, metrique]))>=choixgraph["MinNbObs"])
            {

                ##  #################### graphique par année ####################
                if (choixgraph["graphAn"] == 1)   # 2
                {
                    par(mar=c(9, 4, 5, 1), mgp=c(3, 1, 9))
                    boxplot(spunit[, metrique] ~spunit$an, data=spunit, varwidth = TRUE, ylab=metrique, las=2,
                            main=paste(metrique, "\n pour la famille : ", unique(spunit$Famille), " selon l'année \n"))

                    if (choixgraph["NbObsOrange"] == 1)  # 9
                    {
                        nbObs <- tapply(spunit[, metrique], spunit$an, length)
                        axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)), col.ticks="orange",
                             col.axis = "orange", lty = 2, lwd = 0.5)
                        legend("topleft", "Nombre d'enregistrement par Boxplot", cex =0.7, col="orange",
                               text.col="orange", merge=FALSE)
                    }
                    Moyenne <- as.vector(tapply(spunit[, metrique], spunit$an, mean))
                    if (choixgraph["PtMoyenneBleu"] == 1)  # 10
                    {
                        points(Moyenne, pch=19, col="blue")
                    }
                    if (choixgraph["ChiffreMoyenneBleu"] == 1)  # 11
                    {
                        text(Moyenne+(Moyenne/10), col = "blue", cex = 0.8, labels=as.character(round(Moyenne, digits=Nbdecimales)))
                    }
                    abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")
                    if (choixgraph["maxExclu"] == 1)   # 1
                    {
                        legend("top", "Enregistrements > 95% du maximum retirés", cex =0.7, col="red", text.col="red", merge=FALSE)
                    }
                }
                ##  #################### graphique par AN et PAR BIOTOPE ####################
                if (choixgraph["graphAnBiotope"] == 1)   # 3
                {
                    par(mar=c(15, 4, 5, 1), mgp=c(3, 1, 9))
                    boxplot(spunit[, metrique] ~spunit$biotope+spunit$an, data=spunit,
                            varwidth = TRUE, ylab=metrique, las=2, col=heat.colors(length(unique(spunit$statut_protection))),
                            main=paste(metrique, "\n pour la famille : ", unique(spunit$Famille), "\n selon le biotope et l'année"))
                    if (choixgraph["NbObsOrange"] == 1)  # 9
                    {
                        nbObs <- tapply(spunit[, metrique], list(spunit$biotope, spunit$an), length)
                        axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)), col.ticks="orange",
                             col.axis = "orange", lty = 2, lwd = 0.5)
                        legend("topleft", "Nombre d'enregistrement par Boxplot", cex =0.7, col="orange",
                               text.col="orange", merge=FALSE)
                    }
                    Moyenne <- as.vector(tapply(spunit[, metrique], list(spunit$biotope, spunit$an), mean))
                    if (choixgraph["PtMoyenneBleu"] == 1)  # 10
                    {
                        points(Moyenne, pch=19, col="blue")
                    }
                    if (choixgraph["ChiffreMoyenneBleu"] == 1)  # 11
                    {
                        text(Moyenne+(Moyenne/10), col = "blue", cex = 0.8, labels=as.character(round(Moyenne, digits=Nbdecimales)))
                    }
                    abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")
                    if (choixgraph["maxExclu"] == 1)   # 1
                    {
                        legend("top", "Enregistrements > 95% du maximum retirés", cex =0.7, col="red", text.col="red", merge=FALSE)
                    }
                }
                ##  #################### graphique par AN et PAR STATUT ####################
                if (choixgraph["graphAnStatut"] == 1)   # 4
                {

                    par(mar=c(9, 4, 5, 1), mgp=c(3, 1, 9))
                    boxplot(spunit[, metrique] ~spunit$statut_protection+spunit$an, data=spunit,
                            varwidth = TRUE, ylab=metrique, las=2, col=heat.colors(length(unique(spunit$statut_protection))),
                            main=paste(metrique, " pour la famille : ", unique(spunit$Famille), "\n selon le statut et l'année\n"))
                    if (length(unique(spunit$statut_protection))==3)
                    {
                        legend("topright", textstatut, col=heat.colors(length(unique(spunit$statut_protection))),
                               pch = 15, cex =0.9, title="Statuts")
                    }
                    if (choixgraph["NbObsOrange"] == 1)  # 9
                    {
                        nbObs <- tapply(spunit[, metrique], list(spunit$statut_protection, spunit$an), length)
                        axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)), col.ticks="orange",
                             col.axis = "orange", lty = 2, lwd = 0.5)
                        legend("topleft", "Nombre d'enregistrement par Boxplot", cex =0.7, col="orange",
                               text.col="orange", merge=FALSE)
                    }
                    Moyenne <- as.vector(tapply(spunit[, metrique], list(spunit$statut_protection, spunit$an), na.rm = T, mean))
                    if (choixgraph["PtMoyenneBleu"] == 1)  # 10
                    {
                        points(Moyenne, pch=19, col="blue")
                    }
                    if (choixgraph["ChiffreMoyenneBleu"] == 1)  # 11
                    {
                        text(Moyenne+(Moyenne/10), col = "blue", cex = 0.8, labels=as.character(round(Moyenne, digits=Nbdecimales)))
                    }
                    abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")
                    if (choixgraph["maxExclu"] == 1)   # 1
                    {
                        legend("top", "Enregistrements > 95% du maximum retirés", cex =0.7, col="red", text.col="red", merge=FALSE)
                    }
                }
                ##  #################### graphique par STATUT, PAR AN et par BIOTOPE  ####################
                if (choixgraph["graphAnBiotopeStatut"] == 1)   # 5
                {
                    par(mar=c(9, 4, 5, 1), mgp=c(3, 1, 9))

                    boxplot(spunit[, metrique] ~spunit$statut_protection+spunit$an+spunit$biotope, data=spunit,
                            varwidth = TRUE, ylab=metrique, las=2, col=heat.colors(length(unique(spunit$statut_protection))),
                            main=paste(metrique, " pour la famille : ", unique(spunit$Famille), "\n selon le statut, l'année et le biotope\n"))
                    if (length(unique(spunit$statut_protection))==3)
                    {
                        legend("topright", textstatut, col=heat.colors(length(unique(spunit$statut_protection))),
                               pch = 15, cex =0.9, title="Statuts")
                    }
                    if (choixgraph["NbObsOrange"] == 1)  # 9
                    {
                        nbObs <- tapply(spunit[, metrique], list(spunit$statut_protection, spunit$an, spunit$biotope), length)
                        axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)), col.ticks="orange", col.axis = "orange", lty = 2, lwd = 0.5)
                        legend("topleft", "Nombre d'enregistrement par Boxplot", cex =0.7, col="orange", text.col="orange", merge=FALSE)
                    }
                    Moyenne <- as.vector(tapply(spunit[, metrique], list(spunit$statut_protection, spunit$an, spunit$biotope), na.rm = T, mean))
                    if (choixgraph["PtMoyenneBleu"] == 1)  # 10
                    {
                        points(Moyenne, pch=19, col="blue")
                    }
                    if (choixgraph["ChiffreMoyenneBleu"] == 1)  # 11
                    {
                        text(Moyenne+(Moyenne/10), col = "blue", cex = 0.8, labels=as.character(round(Moyenne, digits=Nbdecimales)))
                    }
                    abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")
                    if (choixgraph["maxExclu"] == 1)   # 1
                    {
                        legend("top", "Enregistrements > 95% du maximum retirés", cex =0.7, col="red", text.col="red", merge=FALSE)
                    }
                }
                ##  #################### graphique par BIOTOPE  ####################
                if (choixgraph["graphBiotope"] == 1)   # 6
                {
                    par(mar=c(9, 4, 5, 1), mgp=c(3, 1, 9))
                    boxplot(spunit[, metrique] ~spunit$biotope, data=spunit, varwidth = TRUE, ylab=metrique, las=2,
                            main=paste(metrique, "\n pour la famille : ", unique(spunit$Famille),
                            " selon le biotope \n"))

                    if (choixgraph["NbObsOrange"] == 1)  # 9
                    {
                        nbObs <- tapply(spunit[, metrique], spunit$biotope, length)
                        axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)), col.ticks="orange", col.axis = "orange", lty = 2, lwd = 0.5)
                        legend("topleft", "Nombre d'enregistrement par Boxplot", cex =0.7, col="orange", text.col="orange", merge=FALSE)
                    }
                    Moyenne <- as.vector(tapply(spunit[, metrique], spunit$biotope, na.rm = T, mean))
                    if (choixgraph["PtMoyenneBleu"] == 1)  # 10
                    {
                        points(Moyenne, pch=19, col="blue")
                    }
                    if (choixgraph["ChiffreMoyenneBleu"] == 1)  # 11
                    {
                        text(Moyenne+(Moyenne/10), col = "blue", cex = 0.8, labels=as.character(round(Moyenne, digits=Nbdecimales)))
                    }
                    abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")
                    if (choixgraph["maxExclu"] == 1)   # 1
                    {
                        legend("top", "Enregistrements > 95% du maximum retirés", cex =0.7, col="red", text.col="red", merge=FALSE)
                    }
                }
                ##  #################### graphique par STATUT   ####################
                if (choixgraph["graphStatut"] == 1)   # 7
                {
                    par(mar=c(9, 4, 5, 1), mgp=c(3, 1, 9))
                    ## boxplot(spunit[, metrique] ~spunit$Famille, data=spunit, varwidth = TRUE, ylab=metrique, las=2,
                    ## main=paste(metrique, " \n pour la famille : ", spunit$Famille, "\npour toutes les unités d'obs"))

                    boxplot(spunit[, metrique] ~spunit$statut_protection, data=spunit, varwidth = TRUE, ylab=metrique,
                            las=2, main=paste(metrique, "\n pour la famille : ", unique(spunit$Famille), "selon le statut \n"))
                    if (length(unique(spunit$statut_protection))==3)
                    {
                        legend("topright", textstatut, col=heat.colors(length(unique(spunit$statut_protection))),
                               pch = 15, cex =0.9, title="Statuts")
                    }
                    if (choixgraph["NbObsOrange"] == 1)  # 9
                    {
                        nbObs <- tapply(spunit[, metrique], spunit$statut_protection, length)
                        axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)), col.ticks="orange",
                             col.axis = "orange", lty = 2, lwd = 0.5)
                        legend("topleft", "Nombre d'enregistrement par Boxplot", cex =0.7, col="orange",
                               text.col="orange", merge=FALSE)
                    }
                    Moyenne <- as.vector(tapply(spunit[, metrique], spunit$statut_protection, na.rm = T, mean))
                    if (choixgraph["PtMoyenneBleu"] == 1)  # 10
                    {
                        points(Moyenne, pch=19, col="blue")
                    }
                    if (choixgraph["ChiffreMoyenneBleu"] == 1)  # 11
                    {
                        text(Moyenne+(Moyenne/10), col = "blue", cex = 0.8, labels=as.character(round(Moyenne, digits=Nbdecimales)))
                    }
                    abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")
                    if (choixgraph["maxExclu"] == 1)   # 1
                    {
                        legend("top", "Enregistrements > 95% du maximum retirés", cex =0.7, col="red", text.col="red", merge=FALSE)
                    }
                }
                ##  #################### graphique par STATUT et PAR BIOTOPE ####################
                if (choixgraph["graphBiotopeStatut"] == 1)  # 8
                {
                    par(mar=c(9, 4, 5, 1), mgp=c(3, 1, 9))
                    boxplot(spunit[, metrique] ~spunit$statut_protection+spunit$biotope, data=spunit,
                            varwidth = TRUE, ylab=metrique, las=2, col=heat.colors(length(unique(spunit$statut_protection))),
                            main=paste(metrique, " pour la famille : ", unique(spunit$Famille), "\n selon le statut et le biotope\n"))
                    if (length(unique(spunit$statut_protection))==3)
                    {
                        legend("topright", textstatut, col=heat.colors(length(unique(spunit$statut_protection))),
                               pch = 15, cex =0.9, title="Statuts")
                    }
                    if (choixgraph["NbObsOrange"] == 1)  # 9
                    {
                        nbObs <- tapply(spunit[, metrique], list(spunit$statut_protection, spunit$biotope), length)
                        axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)), col.ticks="orange", col.axis = "orange", lty = 2, lwd = 0.5)
                        legend("topleft", "Nombre d'enregistrement par Boxplot", cex =0.7, col="orange", text.col="orange", merge=FALSE)
                    }
                    Moyenne <- as.vector(tapply(spunit[, metrique], list(spunit$statut_protection, spunit$biotope), na.rm = T, mean))
                    if (choixgraph["PtMoyenneBleu"] == 1)  # 10
                    {
                        points(Moyenne, pch=19, col="blue")
                    }
                    if (choixgraph["ChiffreMoyenneBleu"] == 1)  # 11
                    {
                        text(Moyenne+(Moyenne/10), col = "blue", cex = 0.8, labels=as.character(round(Moyenne, digits=Nbdecimales)))
                    }
                    abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")
                    if (choixgraph["maxExclu"] == 1)   # 1
                    {
                        legend("top", "Enregistrements > 95% du maximum retirés", cex =0.7, col="red", text.col="red", merge=FALSE)
                    }
                }
                ##  #################### graphique par SITE ####################
                if (choixgraph["graphSite"] == 1)  # 13
                {
                    par(mar=c(9, 4, 5, 1), mgp=c(3, 1, 9))
                    boxplot(spunit[, metrique] ~spunit$site, data=spunit, varwidth = TRUE, ylab=metrique, las=2,
                            main=paste(metrique, "\n pour la famille : ", unique(spunit$Famille), " selon le site \n"))

                    if (choixgraph["NbObsOrange"] == 1)  # 9
                    {
                        nbObs <- tapply(spunit[, metrique], spunit$site, length)
                        axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)), col.ticks="orange", col.axis = "orange", lty = 2, lwd = 0.5)
                        legend("topleft", "Nombre d'enregistrement par Boxplot", cex =0.7, col="orange", text.col="orange", merge=FALSE)
                    }
                    Moyenne <- as.vector(tapply(spunit[, metrique], spunit$site, mean))
                    if (choixgraph["PtMoyenneBleu"] == 1)  # 10
                    {
                        points(Moyenne, pch=19, col="blue")
                    }
                    if (choixgraph["ChiffreMoyenneBleu"] == 1)  # 11
                    {
                        text(Moyenne+(Moyenne/10), col = "blue", cex = 0.8, labels=as.character(round(Moyenne, digits=Nbdecimales)))
                    }
                    abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")
                    if (choixgraph["maxExclu"] == 1)   # 1
                    {
                        legend("top", "Enregistrements > 95% du maximum retirés", cex =0.7, col="red", text.col="red", merge=FALSE)
                    }
                }
                ##  #################### graphique par AN et PAR SITE ####################
                if (choixgraph["graphAnSite"] == 1)  # 14
                {
                    par(mar=c(15, 4, 5, 1), mgp=c(3, 1, 9))
                    boxplot(spunit[, metrique] ~spunit$site+spunit$an, data=spunit,
                            varwidth = TRUE, ylab=metrique, las=2, col=heat.colors(length(unique(spunit$site))),
                            main=paste(metrique, "\n pour la famille : ", unique(spunit$Famille), "\n selon le site et l'année"))
                    if (choixgraph["NbObsOrange"] == 1)  # 9
                    {
                        nbObs <- tapply(spunit[, metrique], list(spunit$site, spunit$an), length)
                        axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)), col.ticks="orange", col.axis = "orange", lty = 2, lwd = 0.5)
                        legend("topleft", "Nombre d'enregistrement par Boxplot", cex =0.7, col="orange", text.col="orange", merge=FALSE)
                    }
                    Moyenne <- as.vector(tapply(spunit[, metrique], list(spunit$site, spunit$an), mean))
                    if (choixgraph["PtMoyenneBleu"] == 1)  # 10
                    {
                        points(Moyenne, pch=19, col="blue")
                    }
                    if (choixgraph["ChiffreMoyenneBleu"] == 1)  # 11
                    {
                        text(Moyenne+(Moyenne/10), col = "blue", cex = 0.8, labels=as.character(round(Moyenne, digits=Nbdecimales)))
                    }
                    abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")
                    if (choixgraph["maxExclu"] == 1)   # 1
                    {
                        legend("top", "Enregistrements > 95% du maximum retirés", cex =0.7, col="red", text.col="red", merge=FALSE)
                    }
                }
                ##  #################### graphique par Caract2 ####################
                if (choixgraph["graphCarac"] == 1)  # 15
                {
                    par(mar=c(9, 4, 5, 1), mgp=c(3, 1, 9))
                    boxplot(spunit[, metrique] ~spunit$caracteristique_2, data=spunit, varwidth = TRUE, ylab=metrique,
                            las=2, main=paste(metrique, "\n pour la famille : ", unique(spunit$Famille),
                                   " selon le caracteristique_2 \n"))

                    if (choixgraph["NbObsOrange"] == 1)  # 9
                    {
                        nbObs <- tapply(spunit[, metrique], spunit$caracteristique_2, length)
                        axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)), col.ticks="orange", col.axis = "orange", lty = 2, lwd = 0.5)
                        legend("topleft", "Nombre d'enregistrement par Boxplot", cex =0.7, col="orange", text.col="orange", merge=FALSE)
                    }
                    Moyenne <- as.vector(tapply(spunit[, metrique], spunit$caracteristique_2, mean))
                    if (choixgraph["PtMoyenneBleu"] == 1)  # 10
                    {
                        points(Moyenne, pch=19, col="blue")
                    }
                    if (choixgraph["ChiffreMoyenneBleu"] == 1)  # 11
                    {
                        text(Moyenne+(Moyenne/10), col = "blue", cex = 0.8, labels=as.character(round(Moyenne, digits=Nbdecimales)))
                    }
                    abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")
                    if (choixgraph["maxExclu"] == 1)   # 1
                    {
                        legend("top", "Enregistrements > 95% du maximum retirés", cex =0.7, col="red", text.col="red", merge=FALSE)
                    }
                }
                ##  #################### graphique par AN et PAR Caract2 ####################
                if (choixgraph["graphAnCarac"] == 1)  # 16
                {
                    par(mar=c(15, 4, 5, 1), mgp=c(3, 1, 9))
                    boxplot(spunit[, metrique] ~spunit$caracteristique_2+spunit$an, data=spunit,
                            varwidth = TRUE, ylab=metrique, las=2, col=heat.colors(length(unique(spunit$caracteristique_2))),
                            main=paste(metrique, "\n pour la famille : ", unique(spunit$Famille), "\n selon le caracteristique_2 et l'année"))
                    if (choixgraph["NbObsOrange"] == 1)  # 9
                    {
                        nbObs <- tapply(spunit[, metrique], list(spunit$caracteristique_2, spunit$an), length)
                        axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)), col.ticks="orange", col.axis = "orange", lty = 2, lwd = 0.5)
                        legend("topleft", "Nombre d'enregistrement par Boxplot", cex =0.7, col="orange", text.col="orange", merge=FALSE)
                    }
                    Moyenne <- as.vector(tapply(spunit[, metrique], list(spunit$caracteristique_2, spunit$an), mean))
                    if (choixgraph["PtMoyenneBleu"] == 1)  # 10
                    {
                        points(Moyenne, pch=19, col="blue")
                    }
                    if (choixgraph["ChiffreMoyenneBleu"] == 1)  # 11
                    {
                        text(Moyenne+(Moyenne/10), col = "blue", cex = 0.8, labels=as.character(round(Moyenne, digits=Nbdecimales)))
                    }
                    abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")
                    if (choixgraph["maxExclu"] == 1)   # 1
                    {
                        legend("top", "Enregistrements > 95% du maximum retirés", cex =0.7, col="red", text.col="red", merge=FALSE)
                    }
                }

            }else{
                tkmessageBox(message=paste("Nombres d'enregistrements inssuffisants \n d'après votre choix de nombre minimum d'observations pour", MaFamille))
            }
        }
    }# fin de MetriqueUneFamilleDansPDF.f

    for (i in 1:length(selectPDFFamille))
    {
        nomPDF <- paste(nameWorkspace, "/FichiersSortie/", selectPDFFamille[i], "_", unique(unitobs$type), ".pdf", sep="")
        pdf(nomPDF, encoding="ISOLatin1", family="URWHelvetica")
        if (unique(unitobs$type) != "LIT")
        {
            MetriqueUneFamilleDansPDF.f("nombre", selectPDFFamille[i])
            MetriqueUneFamilleDansPDF.f("densite", selectPDFFamille[i])
            MetriqueUneFamilleDansPDF.f("biomasse", selectPDFFamille[i])
            MetriqueUneFamilleDansPDF.f("taille_moy", selectPDFFamille[i])
            MetriqueUneFamilleDansPDF.f("poids_moyen", selectPDFFamille[i])
        }else{
            MetriqueUneFamilleDansPDF.f("colonie", selectPDFFamille[i])
            MetriqueUneFamilleDansPDF.f("nombre", selectPDFFamille[i])
            MetriqueUneFamilleDansPDF.f("recouvrement", selectPDFFamille[i])
        }
        tkmessageBox(message=paste("vos graphiques par an seront enregistrés dans ", nomPDF))
        dev.off()
    }
} # fin de UneFamilleDansPDF.f


##############################################################################################
UneEspeceDansPDF.f <- function ()
{
    print("fonction UneEspeceDansPDF.f activée")

    ChoixOptionsGraphiques.f()
    especesPresentes <- subset(unitesp$code_espece, unitesp$pres_abs==1)
    ChoixFacteurSelect.f(especesPresentes, "code_espece", "multiple", 1, "selectPDFespece")

    print(choixgraph)
    print(selectPDFespece)

    MetriqueUneEspeceDansPDF.f <- function (metrique, MonEspece)
    {
        textstatut <- c("HR : Hors Réserve", "PP : Protection Partielle", "RE : En réserve")
        Nbdecimales <- 2
        spunit <- subset(listespunit, listespunit$code_espece==MonEspece)
        if (choixgraph["maxExclu"] == 1)   # 1
        {
            spunit[, metrique][which(spunit[, metrique]>GraphPartMax*max(spunit[, metrique], na.rm=T))] <- NA
        }

        print(paste("NbObs pour", MonEspece, " : ", length(spunit[, metrique])))
        if (length(unique(listespunit[, metrique]))>1)
        {
            ## on vérifie que la metrique a été calculée pour l'espèce sélectionnée
            if (length(unique(spunit[, metrique]))>=choixgraph["MinNbObs"])
            {

                ##  #################### graphique par année ####################
                if (choixgraph["graphAn"] == 1)   # 2
                {
                    par(mar=c(9, 4, 5, 1), mgp=c(3, 1, 9))
                    boxplot(spunit[, metrique] ~spunit$an, data=spunit, varwidth = TRUE, ylab=metrique, las=2,
                            main=paste(metrique, "\n pour l'espèce : ", unique(spunit$code_espece),
                            " selon l'année \n"))

                    if (choixgraph["NbObsOrange"] == 1)  # 9
                    {
                        nbObs <- tapply(spunit[, metrique], spunit$an, length)
                        axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)), col.ticks="orange", col.axis = "orange", lty = 2, lwd = 0.5)
                        legend("topleft", "Nombre d'enregistrement par Boxplot", cex =0.7, col="orange", text.col="orange", merge=FALSE)
                    }
                    Moyenne <- as.vector(tapply(spunit[, metrique], spunit$an, na.rm = T, mean))
                    if (choixgraph["PtMoyenneBleu"] == 1)  # 10
                    {
                        points(Moyenne, pch=19, col="blue")
                    }
                    if (choixgraph["ChiffreMoyenneBleu"] == 1)  # 11
                    {
                        text(Moyenne+(Moyenne/10), col = "blue", cex = 0.8, labels=as.character(round(Moyenne, digits=Nbdecimales)))
                    }
                    abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")
                    if (choixgraph["maxExclu"] == 1)   # 1
                    {
                        legend("top", "Enregistrements > 95% du maximum retirés", cex =0.7, col="red", text.col="red", merge=FALSE)
                    }
                }
                ##  #################### graphique par AN et PAR BIOTOPE ####################
                if (choixgraph["graphAnBiotope"] == 1)   # 3
                {
                    par(mar=c(15, 4, 5, 1), mgp=c(3, 1, 9))
                    boxplot(spunit[, metrique] ~spunit$biotope+spunit$an, data=spunit,
                            varwidth = TRUE, ylab=metrique, las=2, col=heat.colors(length(unique(spunit$biotope))),
                            main=paste(metrique, "\n pour l'espèces : ", unique(spunit$code_espece), "\n selon le biotope et l'année"))
                    if (choixgraph["NbObsOrange"] == 1)  # 9
                    {
                        nbObs <- tapply(spunit[, metrique], list(spunit$biotope, spunit$an), length)
                        axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)), col.ticks="orange", col.axis = "orange", lty = 2, lwd = 0.5)
                        legend("topleft", "Nombre d'enregistrement par Boxplot", cex =0.7, col="orange", text.col="orange", merge=FALSE)
                    }
                    Moyenne <- as.vector(tapply(spunit[, metrique], list(spunit$biotope, spunit$an), na.rm = T, mean))
                    if (choixgraph["PtMoyenneBleu"] == 1)  # 10
                    {
                        points(Moyenne, pch=19, col="blue")
                    }
                    if (choixgraph["ChiffreMoyenneBleu"] == 1)  # 11
                    {
                        text(Moyenne+(Moyenne/10), col = "blue", cex = 0.8, labels=as.character(round(Moyenne, digits=Nbdecimales)))
                    }
                    abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")
                    if (choixgraph["maxExclu"] == 1)   # 1
                    {
                        legend("top", "Enregistrements > 95% du maximum retirés", cex =0.7, col="red", text.col="red", merge=FALSE)
                    }
                }
                ##  #################### graphique par AN et PAR STATUT ####################
                if (choixgraph["graphAnStatut"] == 1)   # 4
                {
                    par(mar=c(9, 4, 5, 1), mgp=c(3, 1, 9))
                    boxplot(spunit[, metrique] ~spunit$statut_protection+spunit$an, data=spunit,
                            varwidth = TRUE, ylab=metrique, las=2, col=heat.colors(length(unique(spunit$statut_protection))),
                            main=paste(metrique, " pour l'espèces : ", unique(spunit$code_espece), "\n selon le statut et l'année\n"))
                    if (length(unique(spunit$statut_protection))==3)
                    {
                        legend("topright", textstatut, col=heat.colors(length(unique(spunit$statut_protection))), pch = 15, cex =0.9, title="Statuts")
                    }
                    if (choixgraph["NbObsOrange"] == 1)  # 9
                    {
                        nbObs <- tapply(spunit[, metrique], list(spunit$statut_protection, spunit$an), length)
                        axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)), col.ticks="orange", col.axis = "orange", lty = 2, lwd = 0.5)
                        legend("topleft", "Nombre d'enregistrement par Boxplot", cex =0.7, col="orange", text.col="orange", merge=FALSE)
                    }
                    Moyenne <- as.vector(tapply(spunit[, metrique], list(spunit$statut_protection, spunit$an), na.rm = T, mean))
                    if (choixgraph["PtMoyenneBleu"] == 1)  # 10
                    {
                        points(Moyenne, pch=19, col="blue")
                    }
                    if (choixgraph["ChiffreMoyenneBleu"] == 1)  # 11
                    {
                        text(Moyenne+(Moyenne/10), col = "blue", cex = 0.8, labels=as.character(round(Moyenne, digits=Nbdecimales)))
                    }
                    abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")
                    if (choixgraph["maxExclu"] == 1)   # 1
                    {
                        legend("top", "Enregistrements > 95% du maximum retirés", cex =0.7, col="red", text.col="red", merge=FALSE)
                    }
                }
                ## #################### graphique par STATUT, PAR AN et par BIOTOPE  ####################
                if (choixgraph["graphAnBiotopeStatut"] == 1)   # 5
                {
                    par(mar=c(9, 4, 5, 1), mgp=c(3, 1, 9))

                    boxplot(spunit[, metrique] ~spunit$statut_protection+spunit$an+spunit$biotope, data=spunit,
                            varwidth = TRUE, ylab=metrique, las=2, col=heat.colors(length(unique(spunit$statut_protection))),
                            main=paste(metrique, " pour l'espèces : ", unique(spunit$code_espece), "\n selon le statut, l'année et le biotope\n"))
                    if (length(unique(spunit$statut_protection))==3)
                    {
                        legend("topright", textstatut, col=heat.colors(length(unique(spunit$statut_protection))), pch = 15, cex =0.9, title="Statuts")
                    }
                    if (choixgraph["NbObsOrange"] == 1)  # 9
                    {
                        nbObs <- tapply(spunit[, metrique], list(spunit$statut_protection, spunit$an, spunit$biotope), length)
                        axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)), col.ticks="orange", col.axis = "orange", lty = 2, lwd = 0.5)
                        legend("topleft", "Nombre d'enregistrement par Boxplot", cex =0.7, col="orange", text.col="orange", merge=FALSE)
                    }
                    Moyenne <- as.vector(tapply(spunit[, metrique], list(spunit$statut_protection, spunit$an, spunit$biotope), na.rm = T, mean))
                    if (choixgraph["PtMoyenneBleu"] == 1)  # 10
                    {
                        points(Moyenne, pch=19, col="blue")
                    }
                    if (choixgraph["ChiffreMoyenneBleu"] == 1)  # 11
                    {
                        text(Moyenne+(Moyenne/10), col = "blue", cex = 0.8, labels=as.character(round(Moyenne, digits=Nbdecimales)))
                    }
                    abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")
                    if (choixgraph["maxExclu"] == 1)   # 1
                    {
                        legend("top", "Enregistrements > 95% du maximum retirés", cex =0.7, col="red", text.col="red", merge=FALSE)
                    }
                }
                ## #################### graphique par BIOTOPE  ####################
                if (choixgraph["graphBiotope"] == 1)   # 6
                {
                    par(mar=c(9, 4, 5, 1), mgp=c(3, 1, 9))
                    boxplot(spunit[, metrique] ~spunit$biotope, data=spunit, varwidth = TRUE, ylab=metrique, las=2,
                            main=paste(metrique, "\n pour l'espèces : ", unique(spunit$code_espece),
                            " selon le biotope \n"))

                    if (choixgraph["NbObsOrange"] == 1)  # 9
                    {
                        nbObs <- tapply(spunit[, metrique], spunit$biotope, length)
                        axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)), col.ticks="orange", col.axis = "orange", lty = 2, lwd = 0.5)
                        legend("topleft", "Nombre d'enregistrement par Boxplot", cex =0.7, col="orange", text.col="orange", merge=FALSE)
                    }
                    Moyenne <- as.vector(tapply(spunit[, metrique], spunit$biotope, na.rm = T, mean))
                    if (choixgraph["PtMoyenneBleu"] == 1)  # 10
                    {
                        points(Moyenne, pch=19, col="blue")
                    }
                    if (choixgraph["ChiffreMoyenneBleu"] == 1)  # 11
                    {
                        text(Moyenne+(Moyenne/10), col = "blue", cex = 0.8, labels=as.character(round(Moyenne, digits=Nbdecimales)))
                    }
                    abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")
                    if (choixgraph["maxExclu"] == 1)   # 1
                    {
                        legend("top", "Enregistrements > 95% du maximum retirés", cex =0.7, col="red", text.col="red", merge=FALSE)
                    }
                }
                ## #################### graphique par STATUT   ####################
                if (choixgraph["graphStatut"] == 1)   # 7
                {
                    par(mar=c(9, 4, 5, 1), mgp=c(3, 1, 9))
                    ## boxplot(spunit[, metrique] ~spunit$code_espece, data=spunit, varwidth = TRUE, ylab=metrique,
                    ## las=2, main=paste(metrique, " \n pour l'espèces : ", spunit$code_espece, "\npour toutes les
                    ## unités d'obs"))

                    boxplot(spunit[, metrique] ~spunit$statut_protection, data=spunit, varwidth = TRUE, ylab=metrique,
                            las=2, main=paste(metrique, "\n pour l'espèces : ", unique(spunit$code_espece),
                                   "selon le statut \n"))
                    if (length(unique(spunit$statut_protection))==3)
                    {
                        legend("topright", textstatut, col=heat.colors(length(unique(spunit$statut_protection))), pch = 15, cex =0.9, title="Statuts")
                    }
                    if (choixgraph["NbObsOrange"] == 1)  # 9
                    {
                        nbObs <- tapply(spunit[, metrique], spunit$statut_protection, length)
                        axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)), col.ticks="orange", col.axis = "orange", lty = 2, lwd = 0.5)
                        legend("topleft", "Nombre d'enregistrement par Boxplot", cex =0.7, col="orange", text.col="orange", merge=FALSE)
                    }
                    Moyenne <- as.vector(tapply(spunit[, metrique], spunit$statut_protection, na.rm = T, mean))
                    if (choixgraph["PtMoyenneBleu"] == 1)  # 10
                    {
                        points(Moyenne, pch=19, col="blue")
                    }
                    if (choixgraph["ChiffreMoyenneBleu"] == 1)  # 11
                    {
                        text(Moyenne+(Moyenne/10), col = "blue", cex = 0.8, labels=as.character(round(Moyenne, digits=Nbdecimales)))
                    }
                    abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")
                    if (choixgraph["maxExclu"] == 1)   # 1
                    {
                        legend("top", "Enregistrements > 95% du maximum retirés", cex =0.7, col="red", text.col="red", merge=FALSE)
                    }
                }
                ## #################### graphique par STATUT et PAR BIOTOPE ####################
                if (choixgraph["graphBiotopeStatut"] == 1)  # 8
                {
                    par(mar=c(9, 4, 5, 1), mgp=c(3, 1, 9))
                    boxplot(spunit[, metrique] ~spunit$statut_protection+spunit$biotope, data=spunit,
                            varwidth = TRUE, ylab=metrique, las=2, col=heat.colors(length(unique(spunit$statut_protection))),
                            main=paste(metrique, " pour l'espèces : ", unique(spunit$code_espece), "\n selon le statut et le biotope\n"))
                    if (length(unique(spunit$statut_protection))==3)
                    {
                        legend("topright", textstatut, col=heat.colors(length(unique(spunit$statut_protection))), pch = 15, cex =0.9, title="Statuts")
                    }
                    if (choixgraph["NbObsOrange"] == 1)  # 9
                    {
                        nbObs <- tapply(spunit[, metrique], list(spunit$statut_protection, spunit$biotope), length)
                        axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)), col.ticks="orange", col.axis = "orange", lty = 2, lwd = 0.5)
                        legend("topleft", "Nombre d'enregistrement par Boxplot", cex =0.7, col="orange", text.col="orange", merge=FALSE)
                    }
                    Moyenne <- as.vector(tapply(spunit[, metrique], list(spunit$statut_protection, spunit$biotope), na.rm = T, mean))
                    if (choixgraph["PtMoyenneBleu"] == 1)  # 10
                    {
                        points(Moyenne, pch=19, col="blue")
                    }
                    if (choixgraph["ChiffreMoyenneBleu"] == 1)  # 11
                    {
                        text(Moyenne+(Moyenne/10), col = "blue", cex = 0.8, labels=as.character(round(Moyenne, digits=Nbdecimales)))
                    }
                    abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")
                    if (choixgraph["maxExclu"] == 1)   # 1
                    {
                        legend("top", "Enregistrements > 95% du maximum retirés", cex =0.7, col="red", text.col="red", merge=FALSE)
                    }
                }
                ## #################### graphique par SITE ####################
                if (choixgraph["graphSite"] == 1)  # 13
                {
                    par(mar=c(9, 4, 5, 1), mgp=c(3, 1, 9))
                    boxplot(spunit[, metrique] ~spunit$site, data=spunit, varwidth = TRUE, ylab=metrique, las=2,
                            main=paste(metrique, "\n pour l'espèce : ", unique(spunit$code_espece),
                            " selon le site \n"))

                    if (choixgraph["NbObsOrange"] == 1)  # 9
                    {
                        nbObs <- tapply(spunit[, metrique], spunit$site, length)
                        axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)), col.ticks="orange", col.axis = "orange", lty = 2, lwd = 0.5)
                        legend("topleft", "Nombre d'enregistrement par Boxplot", cex =0.7, col="orange", text.col="orange", merge=FALSE)
                    }
                    Moyenne <- as.vector(tapply(spunit[, metrique], spunit$site, na.rm = T, mean))
                    if (choixgraph["PtMoyenneBleu"] == 1)  # 10
                    {
                        points(Moyenne, pch=19, col="blue")
                    }
                    if (choixgraph["ChiffreMoyenneBleu"] == 1)  # 11
                    {
                        text(Moyenne+(Moyenne/10), col = "blue", cex = 0.8, labels=as.character(round(Moyenne, digits=Nbdecimales)))
                    }
                    abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")
                    if (choixgraph["maxExclu"] == 1)   # 1
                    {
                        legend("top", "Enregistrements > 95% du maximum retirés", cex =0.7, col="red", text.col="red", merge=FALSE)
                    }
                }
                ## #################### graphique par AN et PAR SITE ####################
                if (choixgraph["graphAnSite"] == 1)  # 14
                {
                    par(mar=c(15, 4, 5, 1), mgp=c(3, 1, 9))
                    boxplot(spunit[, metrique] ~spunit$site+spunit$an, data=spunit,
                            varwidth = TRUE, ylab=metrique, las=2, col=heat.colors(length(unique(spunit$site))),
                            main=paste(metrique, "\n pour l'espèces : ", unique(spunit$code_espece), "\n selon le site et l'année"))
                    if (choixgraph["NbObsOrange"] == 1)  # 9
                    {
                        nbObs <- tapply(spunit[, metrique], list(spunit$biotope, spunit$an), length)
                        axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)), col.ticks="orange", col.axis = "orange", lty = 2, lwd = 0.5)
                        legend("topleft", "Nombre d'enregistrement par Boxplot", cex =0.7, col="orange", text.col="orange", merge=FALSE)
                    }
                    Moyenne <- as.vector(tapply(spunit[, metrique], list(spunit$site, spunit$an), na.rm = T, mean))
                    if (choixgraph["PtMoyenneBleu"] == 1)  # 10
                    {
                        points(Moyenne, pch=19, col="blue")
                    }
                    if (choixgraph["ChiffreMoyenneBleu"] == 1)  # 11
                    {
                        text(Moyenne+(Moyenne/10), col = "blue", cex = 0.8, labels=as.character(round(Moyenne, digits=Nbdecimales)))
                    }
                    abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")
                    if (choixgraph["maxExclu"] == 1)   # 1
                    {
                        legend("top", "Enregistrements > 95% du maximum retirés", cex =0.7, col="red", text.col="red", merge=FALSE)
                    }
                }
                ## #################### graphique par CARAC2 ####################
                if (choixgraph["graphCarac"] == 1)  # 15
                {
                    par(mar=c(9, 4, 5, 1), mgp=c(3, 1, 9))
                    boxplot(spunit[, metrique] ~spunit$caracteristique_2, data=spunit, varwidth = TRUE, ylab=metrique,
                            las=2, main=paste(metrique, "\n pour l'espèce : ", unique(spunit$code_espece),
                                   " selon le caracteristique_2 \n"))

                    if (choixgraph["NbObsOrange"] == 1)  # 9
                    {
                        nbObs <- tapply(spunit[, metrique], spunit$caracteristique_2, length)
                        axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)), col.ticks="orange", col.axis = "orange", lty = 2, lwd = 0.5)
                        legend("topleft", "Nombre d'enregistrement par Boxplot", cex =0.7, col="orange", text.col="orange", merge=FALSE)
                    }
                    Moyenne <- as.vector(tapply(spunit[, metrique], spunit$caracteristique_2, na.rm = T, mean))
                    if (choixgraph["PtMoyenneBleu"] == 1)  # 10
                    {
                        points(Moyenne, pch=19, col="blue")
                    }
                    if (choixgraph["ChiffreMoyenneBleu"] == 1)  # 11
                    {
                        text(Moyenne+(Moyenne/10), col = "blue", cex = 0.8, labels=as.character(round(Moyenne, digits=Nbdecimales)))
                    }
                    abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")
                    if (choixgraph["maxExclu"] == 1)   # 1
                    {
                        legend("top", "Enregistrements > 95% du maximum retirés", cex =0.7, col="red", text.col="red", merge=FALSE)
                    }
                }
                ## #################### graphique par AN et PAR CARAC2 ####################
                if (choixgraph["graphAnCarac"] == 1)  # 16
                {
                    par(mar=c(15, 4, 5, 1), mgp=c(3, 1, 9))
                    boxplot(spunit[, metrique] ~spunit$caracteristique_2+spunit$an, data=spunit,
                            varwidth = TRUE, ylab=metrique, las=2, col=heat.colors(length(unique(spunit$caracteristique_2))),
                            main=paste(metrique, "\n pour l'espèces : ", unique(spunit$code_espece), "\n selon le caracteristique_2 et l'année"))
                    if (choixgraph["NbObsOrange"] == 1)  # 9
                    {
                        nbObs <- tapply(spunit[, metrique], list(spunit$biotope, spunit$an), length)
                        axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)), col.ticks="orange", col.axis = "orange", lty = 2, lwd = 0.5)
                        legend("topleft", "Nombre d'enregistrement par Boxplot", cex =0.7, col="orange", text.col="orange", merge=FALSE)
                    }
                    Moyenne <- as.vector(tapply(spunit[, metrique], list(spunit$caracteristique_2, spunit$an), na.rm = T, mean))
                    if (choixgraph["PtMoyenneBleu"] == 1)  # 10
                    {
                        points(Moyenne, pch=19, col="blue")
                    }
                    if (choixgraph["ChiffreMoyenneBleu"] == 1)  # 11
                    {
                        text(Moyenne+(Moyenne/10), col = "blue", cex = 0.8, labels=as.character(round(Moyenne, digits=Nbdecimales)))
                    }
                    abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")
                    if (choixgraph["maxExclu"] == 1)   # 1
                    {
                        legend("top", "Enregistrements > 95% du maximum retirés", cex =0.7, col="red", text.col="red", merge=FALSE)
                    }
                }
            }else{
                tkmessageBox(message=paste("Nombres d'enregistrements inssuffisants \n d'après votre choix de nombre minimum d'observations pour", MonEspece))
            }
        }
    }# fin de MetriqueUneEspeceDansPDF.f

    for (i in 1:length(selectPDFespece))
    {
        nomPDF <- paste(nameWorkspace, "/FichiersSortie/", selectPDFespece[i], "_", unique(unitobs$type), ".pdf", sep="")
        pdf(nomPDF, encoding="ISOLatin1", family="URWHelvetica")
        if (unique(unitobs$type) != "LIT")
        {
            MetriqueUneEspeceDansPDF.f("nombre", selectPDFespece[i])
            MetriqueUneEspeceDansPDF.f("densite", selectPDFespece[i])
            MetriqueUneEspeceDansPDF.f("biomasse", selectPDFespece[i])
            MetriqueUneEspeceDansPDF.f("taille_moy", selectPDFespece[i])
            MetriqueUneEspeceDansPDF.f("poids_moyen", selectPDFespece[i])
        }else{
            MetriqueUneEspeceDansPDF.f("colonie", selectPDFespece[i])
            MetriqueUneEspeceDansPDF.f("nombre", selectPDFespece[i])
            MetriqueUneEspeceDansPDF.f("recouvrement", selectPDFespece[i])
        }
        tkmessageBox(message=paste("vos graphiques par an seront enregistrés dans ", nomPDF))
        dev.off()
    }
} # fin de UneEspeceDansPDF.f

ChaqueEspeceDansPDF.f <- function (metrique)
{
    print("fonction ChaqueEspeceDansPDF.f activée")

    ## récupération des variables et de la liste d'espèce
    ChoixOptionsGraphiques.f()
    matable <- "listespunit"
    choixchamptable.f(matable)
    metrique <- champtrouve
    print(metrique)
    especesPresentes <- unique(subset(unitesp$code_espece, unitesp$pres_abs==1))

    ## variables pour les graphiques
    textstatut <- c("HR : Hors Réserve", "PP : Protection Partielle", "RE : En réserve")
    Nbdecimales <- 2
    ## choixgraph  contient c(cMaxVal, cTypeAnVal, cTypeAnBiotopeVal, cTypeAnStatutVal, cTypeAnBiotopeStatutVal,
    ## cTypeBiotopeVal, cTypeStatutVal, cTypeBiotopeStatutVal, cAfficheNbObsVal, cAffichePointMoyenneVal, cAfficheValeurMoyenneVal, NbMinObsPourGraphVal)
    ## en fonction de ces variables, on actionne les boucles et on affiche des nb obs et des moyennes dans les graphiques

    ## CAS DES GRAPHIQUE POUR CHAQUE ESPECE PAR AN
    if (choixgraph["graphAn"] == 1)   # 2
    {
        nomPDF <- paste(nameWorkspace, "/FichiersSortie/", metrique, "_ParAn", unique(unitobs$type), "ParEsp", ".pdf", sep="")
        pdf(nomPDF, encoding="ISOLatin1", family="URWHelvetica")

        for (i in 1:length(especesPresentes))
        {
            spunit <- subset(listespunit, listespunit$code_espece==especesPresentes[i])
            if (choixgraph["maxExclu"] == 1)   # 1
            {
                spunit[, metrique][which(spunit[, metrique]>GraphPartMax*max(spunit[, metrique], na.rm=T))] <- NA
            }
            print(paste("NbObs pour", especesPresentes[i], " : ", length(spunit[, metrique])))
            if (length(unique(listespunit[, metrique]))>1)
            {
                ## on vérifie que la metrique a été calculée pour l'espèce sélectionnée
                if (length(unique(spunit[, metrique]))>=choixgraph["MinNbObs"])
                {
                    par(mar=c(9, 4, 5, 1), mgp=c(3, 1, 9))
                    boxplot(spunit[, metrique] ~spunit$an, data=spunit, varwidth = TRUE, ylab=metrique, las=2,
                            main=paste(metrique, "\n pour l'espèces : ", unique(spunit$code_espece),
                            " selon l'année \n"))

                    if (choixgraph["NbObsOrange"] == 1)  # 9
                    {
                        nbObs <- tapply(spunit[, metrique], spunit$an, length)
                        axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)), col.ticks="orange", col.axis = "orange", lty = 2, lwd = 0.5)
                        legend("topleft", "Nombre d'enregistrement par Boxplot", cex =0.7, col="orange", text.col="orange", merge=FALSE)
                    }
                    Moyenne <- as.vector(tapply(spunit[, metrique], spunit$an, na.rm = T, mean))
                    if (choixgraph["PtMoyenneBleu"] == 1)  # 10
                    {
                        points(Moyenne, pch=19, col="blue")
                    }
                    if (choixgraph["ChiffreMoyenneBleu"] == 1)  # 11
                    {
                        text(Moyenne+(Moyenne/10), col = "blue", cex = 0.8, labels=as.character(round(Moyenne, digits=Nbdecimales)))
                    }
                    abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")
                    if (choixgraph["maxExclu"] == 1)   # 1
                    {
                        legend("top", "Enregistrements > 95% du maximum retirés", cex =0.7, col="red", text.col="red", merge=FALSE)
                    }
                }
            }
        }  #fin du for pour chaque espèce
        tkmessageBox(message=paste("vos graphiques par an seront enregistrés dans ", nomPDF, " à la racine de PAMPA"))
        print("boucle POUR CHAQUE ESPECE PAR AN réalisée")
        ## ferme le PDF
        dev.off()
    }#fin de GRAPHIQUE POUR CHAQUE ESPECE PAR AN


    ## CAS DES GRAPHIQUE POUR CHAQUE ESPECE PAR AN et PAR BIOTOPE
    if (choixgraph["graphAnBiotope"] == 1)   # 3
    {
        nomPDF <- paste(nameWorkspace, "/FichiersSortie/", metrique, "_ParStatut", unique(unitobs$type), "ParEsp", ".pdf", sep="")
        pdf(nomPDF, encoding="ISOLatin1", family="URWHelvetica")

        for (i in 1:length(especesPresentes))
        {
            spunit <- subset(listespunit, listespunit$code_espece==especesPresentes[i])
            if (choixgraph["maxExclu"] == 1)   # 1
            {
                ## choix <- 1 #il faut que la variable choix de enlevermax soit à 1 pour que les max soient retirés
                spunit[, metrique][which(spunit[, metrique]>GraphPartMax*max(spunit[, metrique], na.rm=T))] <- NA
            }

            if (length(unique(listespunit[, metrique]))>1)
            {
                ## on vérifie que la metrique a été calculée pour l'espèce sélectionnée
                if (length(unique(spunit[, metrique]))>1)
                {
                    par(mar=c(15, 4, 5, 1), mgp=c(3, 1, 9))
                    boxplot(spunit[, metrique] ~spunit$biotope+spunit$an, data=spunit,
                            varwidth = TRUE, ylab=metrique, las=2, col=heat.colors(length(unique(spunit$biotope))),
                            main=paste(metrique, "\n pour l'espèces : ", unique(spunit$code_espece), "\n selon le biotope, l'année et le statut"))

                    if (choixgraph["NbObsOrange"] == 1)  # 9
                    {
                        nbObs <- tapply(spunit[, metrique], list(spunit$biotope, spunit$an), length)
                        axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)), col.ticks="orange", col.axis = "orange", lty = 2, lwd = 0.5)
                        legend("topleft", "Nombre d'enregistrement par Boxplot", cex =0.7, col="orange", text.col="orange", merge=FALSE)
                    }
                    Moyenne <- as.vector(tapply(spunit[, metrique], list(spunit$biotope, spunit$an), na.rm = T, mean))
                    if (choixgraph["PtMoyenneBleu"] == 1)  # 10
                    {
                        points(Moyenne, pch=19, col="blue")
                    }
                    if (choixgraph["ChiffreMoyenneBleu"] == 1)  # 11
                    {
                        text(Moyenne+(Moyenne/10), col = "blue", cex = 0.8, labels=as.character(round(Moyenne, digits=Nbdecimales)))
                    }
                    abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")
                    if (choixgraph["maxExclu"] == 1)   # 1
                    {
                        legend("top", "Enregistrements > 95% du maximum retirés", cex =0.7, col="red", text.col="red", merge=FALSE)
                    }
                }
            }
        }  #fin du for pour chaque espèce
        tkmessageBox(message=paste("vos graphiques par statut seront enregistrés dans ", nomPDF, " à la racine de PAMPA"))
        print("boucle POUR CHAQUE ESPECE PAR BIOTOPE ET  PAR AN réalisée")
        ## ferme le PDF
        dev.off()

    }#fin de GRAPHIQUE POUR CHAQUE ESPECE PAR BIOTOPE ET  PAR AN


    ## CAS DES GRAPHIQUE POUR CHAQUE ESPECE PAR STATUT ET PAR AN
    if (choixgraph["graphAnStatut"] == 1)   # 4
    {
        nomPDF <- paste(nameWorkspace, "/FichiersSortie/", metrique, "_ParAn_Statut", unique(unitobs$type), "ParEsp", ".pdf", sep="")
        pdf(nomPDF, encoding="ISOLatin1", family="URWHelvetica")

        for (i in 1:length(especesPresentes))
        {
            spunit <- subset(listespunit, listespunit$code_espece==especesPresentes[i])
            if (choixgraph["maxExclu"] == 1)   # 1
            {
                ## choix <- 1 #il faut que la variable choix de enlevermax soit à 1 pour que les max soient retirés
                spunit[, metrique][which(spunit[, metrique]>GraphPartMax*max(spunit[, metrique], na.rm=T))] <- NA
            }

            if (length(unique(listespunit[, metrique]))>1)
            {
                ## on vérifie que la metrique a été calculée pour l'espèce sélectionnée
                if (length(unique(spunit[, metrique]))>1)
                {
                    par(mar=c(9, 4, 5, 1), mgp=c(3, 1, 9))

                    boxplot(spunit[, metrique] ~spunit$statut_protection+spunit$an, data=spunit,
                            varwidth = TRUE, ylab=metrique, las=2, col=heat.colors(length(unique(spunit$statut_protection))),
                            main=paste(metrique, " pour l'espèces : ", unique(spunit$code_espece), "\n selon le statut et l'année\n"))
                    if (length(unique(spunit$statut_protection))==3)
                    {
                        legend("topright", textstatut, col=heat.colors(length(unique(spunit$statut_protection))), pch = 15, cex =0.9, title="Statuts")
                    }
                    if (choixgraph["NbObsOrange"] == 1)  # 9
                    {
                        nbObs <- tapply(spunit[, metrique], list(spunit$statut_protection, spunit$an), length)
                        axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)), col.ticks="orange", col.axis = "orange", lty = 2, lwd = 0.5)
                        legend("topleft", "Nombre d'enregistrement par Boxplot", cex =0.7, col="orange", text.col="orange", merge=FALSE)
                    }
                    Moyenne <- as.vector(tapply(spunit[, metrique], list(spunit$statut_protection, spunit$an), na.rm = T, mean))
                    if (choixgraph["PtMoyenneBleu"] == 1)  # 10
                    {
                        points(Moyenne, pch=19, col="blue")
                    }
                    if (choixgraph["ChiffreMoyenneBleu"] == 1)  # 11
                    {
                        text(Moyenne+(Moyenne/10), col = "blue", cex = 0.8, labels=as.character(round(Moyenne, digits=Nbdecimales)))
                    }
                    abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")
                    if (choixgraph["maxExclu"] == 1)   # 1
                    {
                        legend("top", "Enregistrements > 95% du maximum retirés", cex =0.7, col="red", text.col="red", merge=FALSE)
                    }
                }
            }
        }  #fin du for pour chaque espèce
        tkmessageBox(message=paste("vos graphiques par statut seront enregistrés dans ", nomPDF, " à la racine de PAMPA"))
        print("boucle POUR CHAQUE ESPECE PAR STATUT ET  PAR AN réalisée")
        ## ferme le PDF
        dev.off()

    }#fin de GRAPHIQUE POUR CHAQUE ESPECE PAR STATUT ET  PAR AN


    ## CAS DES GRAPHIQUE POUR CHAQUE ESPECE PAR STATUT, PAR AN ET PAR BIOTOPE
    if (choixgraph["graphAnBiotopeStatut"] == 1)   # 5
    {
        nomPDF <- paste(nameWorkspace, "/FichiersSortie/", metrique, "_ParBiotope_An_Statut", unique(unitobs$type), "ParEsp", ".pdf", sep="")
        pdf(nomPDF, encoding="ISOLatin1", family="URWHelvetica")

        for (i in 1:length(especesPresentes))
        {
            spunit <- subset(listespunit, listespunit$code_espece==especesPresentes[i])
            if (choixgraph["maxExclu"] == 1)   # 1
            {
                spunit[, metrique][which(spunit[, metrique]>GraphPartMax*max(spunit[, metrique], na.rm=T))] <- NA
            }

            if (length(unique(listespunit[, metrique]))>1)
            {
                ## on vérifie que la metrique a été calculée pour l'espèce sélectionnée
                if (length(unique(spunit[, metrique]))>1)
                {
                    par(mar=c(9, 4, 5, 1), mgp=c(3, 1, 9))
                    boxplot(spunit[, metrique] ~spunit$statut_protection+spunit$an+spunit$biotope, data=spunit,
                            varwidth = TRUE, ylab=metrique, las=2, col=heat.colors(length(unique(spunit$statut_protection))),
                            main=paste(metrique, " pour l'espèces : ", unique(spunit$code_espece), "\n selon le statut, l'année et le biotope\n"))
                    if (length(unique(spunit$statut_protection))==3)
                    {
                        legend("topright", textstatut, col=heat.colors(length(unique(spunit$statut_protection))), pch = 15, cex =0.9, title="Statuts")
                    }
                    if (choixgraph["NbObsOrange"] == 1)  # 9
                    {
                        nbObs <- tapply(spunit[, metrique], list(spunit$statut_protection, spunit$an, spunit$biotope), length)
                        axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)), col.ticks="orange", col.axis = "orange", lty = 2, lwd = 0.5)
                        legend("topleft", "Nombre d'enregistrement par Boxplot", cex =0.7, col="orange", text.col="orange", merge=FALSE)
                    }
                    Moyenne <- as.vector(tapply(spunit[, metrique], list(spunit$statut_protection, spunit$an, spunit$biotope), na.rm = T, mean))
                    if (choixgraph["PtMoyenneBleu"] == 1)  # 10
                    {
                        points(Moyenne, pch=19, col="blue")
                    }
                    if (choixgraph["ChiffreMoyenneBleu"] == 1)  # 11
                    {
                        text(Moyenne+(Moyenne/10), col = "blue", cex = 0.8, labels=as.character(round(Moyenne, digits=Nbdecimales)))
                    }
                    abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")
                    if (choixgraph["maxExclu"] == 1)   # 1
                    {
                        legend("top", "Enregistrements > 95% du maximum retirés", cex =0.7, col="red", text.col="red", merge=FALSE)
                    }
                }
            }
        }  #fin du for pour chaque espèce
        tkmessageBox(message=paste("vos graphiques par statut seront enregistrés dans ", nomPDF, " à la racine de PAMPA"))
        print("boucle POUR CHAQUE ESPECE PAR STATUT ET PAR BIOTOPE et PAR AN réalisée")
        ## ferme le PDF
        dev.off()

    }#fin de GRAPHIQUE POUR CHAQUE ESPECE PAR STATUT ET PAR BIOTOPE et PAR AN


    ## CAS DES GRAPHIQUE POUR CHAQUE ESPECE PAR BIOTOPE
    if (choixgraph["graphBiotope"] == 1)   # 6
    {
        nomPDF <- paste(nameWorkspace, "/FichiersSortie/", metrique, "_ParBiotope", unique(unitobs$type), "ParEsp", ".pdf", sep="")
        pdf(nomPDF, encoding="ISOLatin1", family="URWHelvetica")

        for (i in 1:length(especesPresentes))
        {
            spunit <- subset(listespunit, listespunit$code_espece==especesPresentes[i])
            if (choixgraph["maxExclu"] == 1)   # 1
            {
                spunit[, metrique][which(spunit[, metrique]>GraphPartMax*max(spunit[, metrique], na.rm=T))] <- NA
            }

            if (length(unique(listespunit[, metrique]))>1)
            {
                ## on vérifie que la metrique a été calculée pour l'espèce sélectionnée
                if (length(unique(spunit[, metrique]))>1)
                {
                    par(mar=c(9, 4, 5, 1), mgp=c(3, 1, 9))
                    boxplot(spunit[, metrique] ~spunit$biotope, data=spunit, varwidth = TRUE,
                            ylab=metrique, las=2, main=paste(metrique, "\n pour l'espèces : ", unique(spunit$code_espece), " selon le biotope \n"))

                    if (choixgraph["NbObsOrange"] == 1)  # 9
                    {
                        nbObs <- tapply(spunit[, metrique], spunit$biotope, length)
                        axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)), col.ticks="orange", col.axis = "orange", lty = 2, lwd = 0.5)
                        legend("topleft", "Nombre d'enregistrement par Boxplot", cex =0.7, col="orange", text.col="orange", merge=FALSE)
                    }
                    Moyenne <- as.vector(tapply(spunit[, metrique], spunit$biotope, na.rm = T, mean))
                    if (choixgraph["PtMoyenneBleu"] == 1)  # 10
                    {
                        points(Moyenne, pch=19, col="blue")
                    }
                    if (choixgraph["ChiffreMoyenneBleu"] == 1)  # 11
                    {
                        text(Moyenne+(Moyenne/10), col = "blue", cex = 0.8, labels=as.character(round(Moyenne, digits=Nbdecimales)))
                    }
                    abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")
                    if (choixgraph["maxExclu"] == 1)   # 1
                    {
                        legend("top", "Enregistrements > 95% du maximum retirés", cex =0.7, col="red", text.col="red", merge=FALSE)
                    }
                }
            }
        }  #fin du for pour chaque espèce
        tkmessageBox(message=paste("vos graphiques par an seront enregistrés dans ", nomPDF, " à la racine de PAMPA"))
        print("boucle POUR CHAQUE ESPECE PAR BIOTOPE réalisée")
        ## ferme le PDF
        dev.off()

    }#fin de GRAPHIQUE POUR CHAQUE ESPECE PAR BIOTOPE

    ## CAS DES GRAPHIQUE POUR CHAQUE ESPECE PAR STATUT
    if (choixgraph["graphStatut"] == 1)   # 7
    {
        nomPDF <- paste(nameWorkspace, "/FichiersSortie/", metrique, "_ParStatut", unique(unitobs$type), "ParEsp", ".pdf", sep="")
        pdf(nomPDF, encoding="ISOLatin1", family="URWHelvetica")

        for (i in 1:length(especesPresentes))
        {
            spunit <- subset(listespunit, listespunit$code_espece==especesPresentes[i])
            if (choixgraph["maxExclu"] == 1)   # 1
            {
                spunit[, metrique][which(spunit[, metrique]>GraphPartMax*max(spunit[, metrique], na.rm=T))] <- NA
            }

            if (length(unique(listespunit[, metrique]))>1)
            {
                ## on vérifie que la metrique a été calculée pour l'espèce sélectionnée
                if (length(unique(spunit[, metrique]))>1)
                {
                    par(mar=c(9, 4, 5, 1), mgp=c(3, 1, 9))
                    ## boxplot(spunit[, metrique] ~spunit$code_espece, data=spunit, varwidth = TRUE, ylab=metrique,
                    ## las=2, main=paste(metrique, " \n pour l'espèces : ", spunit$code_espece, "\npour toutes les
                    ## unités d'obs"))

                    boxplot(spunit[, metrique] ~spunit$statut_protection, data=spunit, varwidth = TRUE, ylab=metrique,
                            las=2, main=paste(metrique, "\n pour l'espèces : ", unique(spunit$code_espece),
                                   "selon le statut \n"))
                    if (length(unique(spunit$statut_protection))==3)
                    {
                        legend("topright", textstatut, col=heat.colors(length(unique(spunit$statut_protection))), pch = 15, cex =0.9, title="Statuts")
                    }
                    if (choixgraph["NbObsOrange"] == 1)  # 9
                    {
                        nbObs <- tapply(spunit[, metrique], spunit$statut_protection, length)
                        axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)), col.ticks="orange", col.axis = "orange", lty = 2, lwd = 0.5)
                        legend("topleft", "Nombre d'enregistrement par Boxplot", cex =0.7, col="orange", text.col="orange", merge=FALSE)
                    }
                    Moyenne <- as.vector(tapply(spunit[, metrique], spunit$statut_protection, na.rm = T, mean))
                    if (choixgraph["PtMoyenneBleu"] == 1)  # 10
                    {
                        points(Moyenne, pch=19, col="blue")
                    }
                    if (choixgraph["ChiffreMoyenneBleu"] == 1)  # 11
                    {
                        text(Moyenne+(Moyenne/10), col = "blue", cex = 0.8, labels=as.character(round(Moyenne, digits=Nbdecimales)))
                    }
                    abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")
                    if (choixgraph["maxExclu"] == 1)   # 1
                    {
                        legend("top", "Enregistrements > 95% du maximum retirés", cex =0.7, col="red", text.col="red", merge=FALSE)
                    }
                }
            }
        }  #fin du for pour chaque espèce
        tkmessageBox(message=paste("vos graphiques par statut seront enregistrés dans ", nomPDF, " à la racine de PAMPA"))
        print("boucle POUR CHAQUE ESPECE PAR STATUT réalisée")
        ## ferme le PDF
        dev.off()

    }#fin de GRAPHIQUE POUR CHAQUE ESPECE PAR STATUT

    ## CAS DES GRAPHIQUE POUR CHAQUE ESPECE PAR STATUT ET  PAR BIOTOPE
    if (choixgraph["graphBiotopeStatut"] == 1)  # 8
    {
        nomPDF <- paste(nameWorkspace, "/FichiersSortie/", metrique, "_ParBiotope_Statut", unique(unitobs$type), "ParEsp", ".pdf", sep="")
        pdf(nomPDF, encoding="ISOLatin1", family="URWHelvetica")

        for (i in 1:length(especesPresentes))
        {
            spunit <- subset(listespunit, listespunit$code_espece==especesPresentes[i])
            if (choixgraph["maxExclu"] == 1)   # 1
            {
                spunit[, metrique][which(spunit[, metrique]>GraphPartMax*max(spunit[, metrique], na.rm=T))] <- NA
            }

            if (length(unique(listespunit[, metrique]))>1)
            {
                ## on vérifie que la metrique a été calculée pour l'espèce sélectionnée
                if (length(unique(spunit[, metrique]))>1)
                {

                    par(mar=c(9, 4, 5, 1), mgp=c(3, 1, 9))
                    boxplot(spunit[, metrique] ~spunit$statut_protection+spunit$biotope, data=spunit,
                            varwidth = TRUE, ylab=metrique, las=2, col=heat.colors(length(unique(spunit$statut_protection))),
                            main=paste(metrique, " pour l'espèces : ", unique(spunit$code_espece), "\n selon le statut et le biotope\n"))
                    if (length(unique(spunit$statut_protection))==3)
                    {
                        legend("topright", textstatut, col=heat.colors(length(unique(spunit$statut_protection))), pch = 15, cex =0.9, title="Statuts")
                    }
                    if (choixgraph["NbObsOrange"] == 1)  # 9
                    {
                        nbObs <- tapply(spunit[, metrique], list(spunit$statut_protection, spunit$biotope), length)
                        axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)), col.ticks="orange", col.axis = "orange", lty = 2, lwd = 0.5)
                        legend("topleft", "Nombre d'enregistrement par Boxplot", cex =0.7, col="orange", text.col="orange", merge=FALSE)
                    }
                    Moyenne <- as.vector(tapply(spunit[, metrique], list(spunit$statut_protection, spunit$biotope), na.rm = T, mean))
                    if (choixgraph["PtMoyenneBleu"] == 1)  # 10
                    {
                        points(Moyenne, pch=19, col="blue")
                    }
                    if (choixgraph["ChiffreMoyenneBleu"] == 1)  # 11
                    {
                        text(Moyenne+(Moyenne/10), col = "blue", cex = 0.8, labels=as.character(round(Moyenne, digits=Nbdecimales)))
                    }
                    abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")
                    if (choixgraph["maxExclu"] == 1)   # 1
                    {
                        legend("top", "Enregistrements > 95% du maximum retirés", cex =0.7, col="red", text.col="red", merge=FALSE)
                    }
                }
            }
        }  #fin du for pour chaque espèce
        tkmessageBox(message=paste("vos graphiques par statut seront enregistrés dans ", nomPDF, " dans le dossier FichiersSortie"))
        print("boucle POUR CHAQUE ESPECE PAR STATUT ET  PAR BIOTOPE réalisée")
        ## ferme le PDF
        dev.off()
    }#fin de GRAPHIQUE POUR CHAQUE ESPECE PAR STATUT ET  PAR BIOTOPE

    ## CAS DES GRAPHIQUE POUR CHAQUE ESPECE PAR SITE
    if (choixgraph["graphSite"] == 1)  # 13
    {
        nomPDF <- paste(nameWorkspace, "/FichiersSortie/", metrique, "_ParSite", unique(unitobs$type), "ParEsp", ".pdf", sep="")
        pdf(nomPDF, encoding="ISOLatin1", family="URWHelvetica")

        for (i in 1:length(especesPresentes))
        {
            spunit <- subset(listespunit, listespunit$code_espece==especesPresentes[i])
            if (choixgraph["maxExclu"] == 1)   # 1
            {
                spunit[, metrique][which(spunit[, metrique]>GraphPartMax*max(spunit[, metrique], na.rm=T))] <- NA
            }
            print(paste("NbObs pour", especesPresentes[i], " : ", length(spunit[, metrique])))
            if (length(unique(listespunit[, metrique]))>1)
            {
                ## on vérifie que la metrique a été calculée pour l'espèce sélectionnée
                if (length(unique(spunit[, metrique]))>=choixgraph["MinNbObs"])
                {
                    par(mar=c(9, 4, 5, 1), mgp=c(3, 1, 9))
                    boxplot(spunit[, metrique] ~spunit$site, data=spunit, varwidth = TRUE, ylab=metrique, las=2,
                            main=paste(metrique, "\n pour l'espèces : ", unique(spunit$code_espece),
                            " selon l'année \n"))

                    if (choixgraph["NbObsOrange"] == 1)  # 9
                    {
                        nbObs <- tapply(spunit[, metrique], spunit$site, length)
                        axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)), col.ticks="orange", col.axis = "orange", lty = 2, lwd = 0.5)
                        legend("topleft", "Nombre d'enregistrement par Boxplot", cex =0.7, col="orange", text.col="orange", merge=FALSE)
                    }
                    Moyenne <- as.vector(tapply(spunit[, metrique], spunit$site, na.rm = T, mean))
                    if (choixgraph["PtMoyenneBleu"] == 1)  # 10
                    {
                        points(Moyenne, pch=19, col="blue")
                    }
                    if (choixgraph["ChiffreMoyenneBleu"] == 1)  # 11
                    {
                        text(Moyenne+(Moyenne/10), col = "blue", cex = 0.8, labels=as.character(round(Moyenne, digits=Nbdecimales)))
                    }
                    abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")
                    if (choixgraph["maxExclu"] == 1)   # 1
                    {
                        legend("top", "Enregistrements > 95% du maximum retirés", cex =0.7, col="red", text.col="red", merge=FALSE)
                    }
                }
            }
        }  #fin du for pour chaque espèce
        tkmessageBox(message=paste("vos graphiques par an seront enregistrés dans ", nomPDF, " à la racine de PAMPA"))
        print("boucle POUR CHAQUE ESPECE PAR SITE réalisée")
        ## ferme le PDF
        dev.off()
    }#fin de GRAPHIQUE POUR CHAQUE ESPECE PAR SITE


    ## CAS DES GRAPHIQUE POUR CHAQUE ESPECE PAR AN et PAR SITE
    if (choixgraph["graphAnSite"] == 1)  # 14
    {
        nomPDF <- paste(nameWorkspace, "/FichiersSortie/", metrique, "_ParSite", unique(unitobs$type), "ParEsp", ".pdf", sep="")
        pdf(nomPDF, encoding="ISOLatin1", family="URWHelvetica")

        for (i in 1:length(especesPresentes))
        {
            spunit <- subset(listespunit, listespunit$code_espece==especesPresentes[i])
            if (choixgraph["maxExclu"] == 1)   # 1
            {
                ## choix <- 1 #il faut que la variable choix de enlevermax soit à 1 pour que les max soient retirés
                spunit[, metrique][which(spunit[, metrique]>GraphPartMax*max(spunit[, metrique], na.rm=T))] <- NA
            }

            if (length(unique(listespunit[, metrique]))>1)
            {
                ## on vérifie que la metrique a été calculée pour l'espèce sélectionnée
                if (length(unique(spunit[, metrique]))>1)
                {
                    par(mar=c(15, 4, 5, 1), mgp=c(3, 1, 9))
                    boxplot(spunit[, metrique] ~spunit$site+spunit$an, data=spunit,
                            varwidth = TRUE, ylab=metrique, las=2, col=heat.colors(length(unique(spunit$site))),
                            main=paste(metrique, "\n pour l'espèces : ", unique(spunit$code_espece), "\n selon le site et l'année"))

                    if (choixgraph["NbObsOrange"] == 1)  # 9
                    {
                        nbObs <- tapply(spunit[, metrique], list(spunit$site, spunit$an), length)
                        axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)), col.ticks="orange", col.axis = "orange", lty = 2, lwd = 0.5)
                        legend("topleft", "Nombre d'enregistrement par Boxplot", cex =0.7, col="orange", text.col="orange", merge=FALSE)
                    }
                    Moyenne <- as.vector(tapply(spunit[, metrique], list(spunit$site, spunit$an), na.rm = T, mean))
                    if (choixgraph["PtMoyenneBleu"] == 1)  # 10
                    {
                        points(Moyenne, pch=19, col="blue")
                    }
                    if (choixgraph["ChiffreMoyenneBleu"] == 1)  # 11
                    {
                        text(Moyenne+(Moyenne/10), col = "blue", cex = 0.8, labels=as.character(round(Moyenne, digits=Nbdecimales)))
                    }
                    abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")
                    if (choixgraph["maxExclu"] == 1)   # 1
                    {
                        legend("top", "Enregistrements > 95% du maximum retirés", cex =0.7, col="red", text.col="red", merge=FALSE)
                    }
                }
            }
        }  #fin du for pour chaque espèce
        tkmessageBox(message=paste("vos graphiques par statut seront enregistrés dans ", nomPDF, " à la racine de PAMPA"))
        print("boucle POUR CHAQUE ESPECE PAR SITE ET  PAR AN réalisée")
        ## ferme le PDF
        dev.off()

    }#fin de GRAPHIQUE POUR CHAQUE ESPECE PAR SITE ET  PAR AN

    ## CAS DES GRAPHIQUE POUR CHAQUE ESPECE PAR CARACT2
    if (choixgraph["graphCarac"] == 1)  # 15
    {
        nomPDF <- paste(nameWorkspace, "/FichiersSortie/", metrique, "_ParCaract2", unique(unitobs$type), "ParEsp", ".pdf", sep="")
        pdf(nomPDF, encoding="ISOLatin1", family="URWHelvetica")

        for (i in 1:length(especesPresentes))
        {
            spunit <- subset(listespunit, listespunit$code_espece==especesPresentes[i])
            if (choixgraph["maxExclu"] == 1)   # 1
            {
                spunit[, metrique][which(spunit[, metrique]>GraphPartMax*max(spunit[, metrique], na.rm=T))] <- NA
            }
            print(paste("NbObs pour", especesPresentes[i], " : ", length(spunit[, metrique])))
            if (length(unique(listespunit[, metrique]))>1)
            {
                ## on vérifie que la metrique a été calculée pour l'espèce sélectionnée
                if (length(unique(spunit[, metrique]))>=choixgraph["MinNbObs"])
                {
                    par(mar=c(9, 4, 5, 1), mgp=c(3, 1, 9))
                    boxplot(spunit[, metrique] ~spunit$caracteristique_2, data=spunit, varwidth = TRUE, ylab=metrique,
                            las=2, main=paste(metrique, "\n pour l'espèces : ", unique(spunit$code_espece),
                                   " selon l'année \n"))

                    if (choixgraph["NbObsOrange"] == 1)  # 9
                    {
                        nbObs <- tapply(spunit[, metrique], spunit$caracteristique_2, length)
                        axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)), col.ticks="orange", col.axis = "orange", lty = 2, lwd = 0.5)
                        legend("topleft", "Nombre d'enregistrement par Boxplot", cex =0.7, col="orange", text.col="orange", merge=FALSE)
                    }
                    Moyenne <- as.vector(tapply(spunit[, metrique], spunit$caracteristique_2, na.rm = T, mean))
                    if (choixgraph["PtMoyenneBleu"] == 1)  # 10
                    {
                        points(Moyenne, pch=19, col="blue")
                    }
                    if (choixgraph["ChiffreMoyenneBleu"] == 1)  # 11
                    {
                        text(Moyenne+(Moyenne/10), col = "blue", cex = 0.8, labels=as.character(round(Moyenne, digits=Nbdecimales)))
                    }
                    abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")
                    if (choixgraph["maxExclu"] == 1)   # 1
                    {
                        legend("top", "Enregistrements > 95% du maximum retirés", cex =0.7, col="red", text.col="red", merge=FALSE)
                    }
                }
            }
        }  #fin du for pour chaque espèce
        tkmessageBox(message=paste("vos graphiques par an seront enregistrés dans ", nomPDF, " à la racine de PAMPA"))
        print("boucle POUR CHAQUE ESPECE PAR caracteristique_2 réalisée")
        ## ferme le PDF
        dev.off()
    }#fin de GRAPHIQUE POUR CHAQUE ESPECE PAR CARACT2


    ## CAS DES GRAPHIQUE POUR CHAQUE ESPECE PAR AN et PAR CARACT2
    if (choixgraph["graphAnCarac"] == 1)  # 16
    {
        nomPDF <- paste(nameWorkspace, "/FichiersSortie/", metrique, "_ParCaract2", unique(unitobs$type), "ParEsp", ".pdf", sep="")
        pdf(nomPDF, encoding="ISOLatin1", family="URWHelvetica")

        for (i in 1:length(especesPresentes))
        {
            spunit <- subset(listespunit, listespunit$code_espece==especesPresentes[i])
            if (choixgraph["maxExclu"] == 1)   # 1
            {
                ## choix <- 1 #il faut que la variable choix de enlevermax soit à 1 pour que les max soient retirés
                spunit[, metrique][which(spunit[, metrique]>GraphPartMax*max(spunit[, metrique], na.rm=T))] <- NA
            }

            if (length(unique(listespunit[, metrique]))>1)
            {
                ## on vérifie que la metrique a été calculée pour l'espèce sélectionnée
                if (length(unique(spunit[, metrique]))>1)
                {
                    par(mar=c(15, 4, 5, 1), mgp=c(3, 1, 9))
                    boxplot(spunit[, metrique] ~spunit$caracteristique_2+spunit$an, data=spunit,
                            varwidth = TRUE, ylab=metrique, las=2, col=heat.colors(length(unique(spunit$caracteristique_2))),
                            main=paste(metrique, "\n pour l'espèces : ", unique(spunit$code_espece), "\n selon caracteristique_2 et l'année"))

                    if (choixgraph["NbObsOrange"] == 1)  # 9
                    {
                        nbObs <- tapply(spunit[, metrique], list(spunit$caracteristique_2, spunit$an), length)
                        axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)), col.ticks="orange", col.axis = "orange", lty = 2, lwd = 0.5)
                        legend("topleft", "Nombre d'enregistrement par Boxplot", cex =0.7, col="orange", text.col="orange", merge=FALSE)
                    }
                    Moyenne <- as.vector(tapply(spunit[, metrique], list(spunit$caracteristique_2, spunit$an), na.rm = T, mean))
                    if (choixgraph["PtMoyenneBleu"] == 1)  # 10
                    {
                        points(Moyenne, pch=19, col="blue")
                    }
                    if (choixgraph["ChiffreMoyenneBleu"] == 1)  # 11
                    {
                        text(Moyenne+(Moyenne/10), col = "blue", cex = 0.8, labels=as.character(round(Moyenne, digits=Nbdecimales)))
                    }
                    abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")
                    if (choixgraph["maxExclu"] == 1)   # 1
                    {
                        legend("top", "Enregistrements > 95% du maximum retirés", cex =0.7, col="red", text.col="red", merge=FALSE)
                    }
                }
            }
        }  #fin du for pour chaque espèce
        tkmessageBox(message=paste("vos graphiques par statut seront enregistrés dans ", nomPDF, " à la racine de PAMPA"))
        print("boucle POUR CHAQUE ESPECE PAR caracteristique_2 ET  PAR AN réalisée")
        ## ferme le PDF
        dev.off()

    }#fin de GRAPHIQUE POUR CHAQUE ESPECE PAR CARACT2 ET  PAR AN


    if (choixgraph["graphAn"] == 1 || choixgraph["graphAnBiotope"] == 1 || choixgraph["graphAnStatut"] == 1 || choixgraph["graphAnBiotopeStatut"] == 1 || choixgraph["graphBiotope"] == 1 ||   # 2   # 3   # 4   # 5   # 6
        choixgraph["graphStatut"] == 1 || choixgraph["graphBiotopeStatut"] == 1 || choixgraph["graphSite"] == 1 || choixgraph["graphAnSite"] == 1 || choixgraph["graphCarac"] == 1 ||   # 7  # 8  # 13  # 14  # 15
        choixgraph["graphAnCarac"] == 1)              # Sûrement moyen de faire plus simple (avec une somme ?) [yr: 30/07/2010]  # 16
    {
        gestionMSGinfo.f("InfoPDFdansFichierSortie")
    }else{
        gestionMSGinfo.f("AucunPDFdansFichierSortie")
    }
}

################################################################################
## Nom    : GraphGroup1factUnitobs.f()
## Objet  : graphiques regroupant les observations sur une caractéristique des unités d'observations
## Input  : table "listespunit" et un code champ
## Output : boxplot
################################################################################
GraphGroup1factUnitobs.f <- function()
{
    print("fonction GraphGroup1factUnitobs.f activée")

    ## ###############################
    ## pour la table listespunit    #
    ## ###############################
    matable <- "listespunit"

    ## Choix du champ à représenter champobs
    choixchamptable.f(matable)
    print(champtrouve)
    ## Choix du facteur de regroupement
    choixunfacteurUnitobs.f()
    print(fact)
    objtable <- eval(parse(text=matable))
    objtable[, fact] <- unitobs[, fact][match(objtable$unite_observation, unitobs$unite_observation)]
    objtable$an <- unitobs$an[match(objtable$unite_observation, unitobs$unite_observation)]
    objtable$statut <- unitobs$statut[match(objtable$unite_observation, unitobs$unite_observation)]
    obsSansExtreme <- objtable
    extremes.f()
    ## Graphiques sur un champ de obs
    obsSansExtreme[, champtrouve] <- EnleverMaxExtremes.f(obsSansExtreme[, champtrouve])    #  selon choix retourné par extremes.f()
    print(head(obsSansExtreme))
    TauxUnitobsCritereSansNA <- round(100*(length(unique(obsSansExtreme$unite_observation[is.na(obsSansExtreme[, fact])==FALSE]))  # [!!!]
                                           / length(unique(obsSansExtreme$unite_observation))), digits=2)
    TauxObsCritereSansNA <- round(100*(sum(table(obsSansExtreme[, fact]))/length(obsSansExtreme[, fact])), digits=2)
    TauxRenseignement <- paste("Taux de renseignement \ndu champs ", fact, ":\n", TauxObsCritereSansNA,
                               "% des observations\n", TauxUnitobsCritereSansNA, "% des unites d'observation\n")
    if (length(unique(obsSansExtreme[, champtrouve]))>0)     #teste si il y a au moins un enregistrement
    {
        if (round(sum(table(obsSansExtreme[, fact]))/length(obsSansExtreme[, fact]), digits=2)>=0.5)
        {
            if (length(unique(obsSansExtreme[, fact]))>1)  #teste si il y a plus d'une valeur pour le regroupement dans le champs
            {
                ## distribution des enregistrements groupés selon le critère

                x11(width=50, height=20, pointsize=10)
                par(mar=c(9, 4, 5, 1), mgp=c(3, 1, 9))
                boxplot(obsSansExtreme[, champtrouve] ~obsSansExtreme[, fact], data=obsSansExtreme, varwidth = TRUE,
                        las=3, main=paste("Regroupement des ", champtrouve, " selon \"", fact, "\""))
                nbObs <- tapply(obsSansExtreme[, champtrouve], obsSansExtreme[, fact], length)
                axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)), col.ticks="orange", col.axis = "orange", lty = 2, lwd = 0.5)
                legend("topleft", "Nombre d'enregistrements par Boxplot", cex =0.7, col="orange", text.col="orange", merge=FALSE)
                Moyenne <- as.vector(tapply(obsSansExtreme[, champtrouve], obsSansExtreme[, fact], na.rm = T, mean))
                points(Moyenne, pch=19, col="blue")
                text(Moyenne+(Moyenne/10), col = "blue", cex = 0.8, labels=as.character(round(Moyenne, digits=1)))
                abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")
                ## boxplot(spunit[, champtrouve] ~spunit$an+spunit$code_espece, las=2, cex.names=0.6, main=paste("Nombre
                ## d'individus par espèce et par an pour le site ", si, "\n"))
                ## nbObs <- tapply(spunit[, champtrouve], list(spunit$an, spunit$code_espece), length)
                if (choix=="1")
                {
                    legend("top", "Enregistrements > 95% du maximum retirés", cex =0.7, col="red", text.col="red", merge=FALSE)
                }
                legend("topleft", TauxRenseignement, cex =0.8, bty="n", text.col="red", merge=FALSE)

                ## distribution des enregistrements groupés selon le critère et le statut du site

                textstatut <- c("HR : Hors Réserve", "PP : Protection Partielle", "RE : En réserve")
                x11(width=120, height=50, pointsize=10)
                par(mar=c(8, 4, 5, 1), mgp=c(3, 1, 9))
                boxplot(obsSansExtreme[, champtrouve][obsSansExtreme[, champtrouve]!= 0]
                        ~obsSansExtreme$statut[obsSansExtreme[, champtrouve]!= 0]+obsSansExtreme[, fact][obsSansExtreme[,
                                                             champtrouve]!= 0], col=heat.colors(length(unique(obsSansExtreme$statut))), las=2,
                        main=paste(champtrouve, " par enregistrement\n selon le statut et Regroupement selon \"", fact,
                        "\"\n\n"))
                nbObs <- tapply(obsSansExtreme[, champtrouve][obsSansExtreme[, champtrouve]!= 0],
                                list(obsSansExtreme$statut[obsSansExtreme[, champtrouve]!= 0], obsSansExtreme[, fact][obsSansExtreme[,
                                                                         champtrouve]!= 0]), length)
                legend("topleft", "Nombre d'enregistrement par Boxplot", cex =0.7, col="orange", text.col="orange", merge=FALSE)
                legend("topright", textstatut, col=heat.colors(length(unique(obsSansExtreme$statut))), pch = 15, cex =0.9, title="Statuts")
                axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)), col.ticks="orange", col.axis = "orange", lty = 2, lwd = 0.5)
                Moyenne <- as.vector(tapply(obsSansExtreme[, champtrouve][obsSansExtreme[, champtrouve]!= 0],
                                            list(obsSansExtreme$statut[obsSansExtreme[, champtrouve]!= 0], obsSansExtreme[, fact][obsSansExtreme[,
                                                                                     champtrouve]!= 0]), na.rm = T, mean))
                points(Moyenne, pch=19, col="blue")
                abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")
                abline(v = 0.5+(1:length(as.vector(unique(obsSansExtreme[, fact]))))*length(as.vector(unique(obsSansExtreme$statut))), col = "red")
                if (choix=="1")
                {
                    legend("top", "Enregistrements > 95% du maximum retirés", cex =0.7, col="red", text.col="red", merge=FALSE)
                }
                legend("topleft", TauxRenseignement, cex =0.8, bty="n", col="red", text.col="red", merge=FALSE)

                ## distribution des enregistrements groupés selon le critère et l'année de l'observation

                x11(width=120, height=50, pointsize=10)
                par(mar=c(8, 4, 5, 1), mgp=c(3, 1, 9))
                boxplot(obsSansExtreme[, champtrouve][obsSansExtreme[, champtrouve]!= 0]
                        ~obsSansExtreme$statut[obsSansExtreme[, champtrouve]!= 0]+obsSansExtreme$an[obsSansExtreme[,
                                                             champtrouve]!= 0]+obsSansExtreme[, fact][obsSansExtreme[, champtrouve]!= 0],
                        col=heat.colors(length(unique(obsSansExtreme$statut))), las=2,
                        main=paste(champtrouve,
                        " par enregistrement\n selon le statut puis l'année et Regroupement selon \"", fact, "\"\n\n"))
                nbObs <- tapply(obsSansExtreme[, champtrouve][obsSansExtreme[, champtrouve]!= 0],
                                list(obsSansExtreme$statut[obsSansExtreme[, champtrouve]!= 0],
                                     obsSansExtreme$an[obsSansExtreme[, champtrouve]!= 0], obsSansExtreme[,
                                                                     fact][obsSansExtreme[, champtrouve]!= 0]), length)
                legend("topleft", "Nombre d'enregistrement par Boxplot", cex =0.7, col="orange", text.col="orange", merge=FALSE)
                legend("topright", textstatut, col=heat.colors(length(unique(obsSansExtreme$statut))), pch = 15, cex =0.9, title="Statuts")
                axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)), col.ticks="orange", col.axis = "orange", lty = 2, lwd = 0.5, cex =0.7)
                Moyenne <- as.vector(tapply(obsSansExtreme[, champtrouve][obsSansExtreme[, champtrouve]!= 0],
                                            list(obsSansExtreme$statut[obsSansExtreme[, champtrouve]!= 0],
                                                 obsSansExtreme$an[obsSansExtreme[, champtrouve]!= 0],
                                                 obsSansExtreme[, fact][obsSansExtreme[, champtrouve]!= 0]),
                                            na.rm = T, mean))
                points(Moyenne, pch=19, col="blue")
                abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")
                abline(v = 0.5+(1:(length(as.vector(unique(obsSansExtreme$an))) *
                       length(as.vector(unique(obsSansExtreme[, fact]))))) *
                       length(as.vector(unique(obsSansExtreme$statut))), col = "red")
                if (choix=="1")
                {
                    legend("top", "Enregistrements > 95% du maximum retirés", cex =0.7, col="red", text.col="red", merge=FALSE)
                }
                legend("topleft", TauxRenseignement, cex =0.8, col="red", bty="n", text.col="red", merge=FALSE)

                ## distribution des enregistrements groupés selon le critère et le statut l'année de l'observation

                x11(width=120, height=50, pointsize=10)
                par(mar=c(8, 4, 5, 1), mgp=c(3, 1, 9))
                boxplot(obsSansExtreme[, champtrouve][obsSansExtreme[, champtrouve]!= 0] ~
                        obsSansExtreme$an[obsSansExtreme[, champtrouve]!= 0] +
                        obsSansExtreme$statut[obsSansExtreme[, champtrouve]!= 0] +
                        obsSansExtreme[, fact][obsSansExtreme[, champtrouve]!= 0],
                        col=heat.colors(length(unique(obsSansExtreme$statut))*length(unique(obsSansExtreme$an))),
                        las=2,
                        main=paste(champtrouve, " par enregistrement\n selon l'année puis le statut et Regroupement selon \"", fact, "\"\n\n"))
                nbObs <- tapply(obsSansExtreme[, champtrouve][obsSansExtreme[, champtrouve]!= 0],
                                list(obsSansExtreme$an[obsSansExtreme[, champtrouve]!= 0],
                                     obsSansExtreme$statut[obsSansExtreme[, champtrouve]!= 0],
                                     obsSansExtreme[, fact][obsSansExtreme[, champtrouve]!= 0]),
                                length)
                legend("topleft", "Nombre d'enregistrement par Boxplot", cex =0.7, col="orange", text.col="orange", merge=FALSE)
                legend("topright", textstatut, col=heat.colors(length(unique(obsSansExtreme$statut))), pch = 15, cex =0.9, title="Statuts")
                axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)), col.ticks="orange", col.axis = "orange", lty = 2, lwd = 0.5, cex =0.7)
                Moyenne <- as.vector(tapply(obsSansExtreme[, champtrouve][obsSansExtreme[, champtrouve]!= 0],
                                            list(obsSansExtreme$an[obsSansExtreme[, champtrouve]!= 0],
                                                 obsSansExtreme$statut[obsSansExtreme[, champtrouve]!= 0],
                                                 obsSansExtreme[, fact][obsSansExtreme[, champtrouve]!= 0]),
                                            na.rm = TRUE, mean))
                points(Moyenne, pch=19, col="blue")
                abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")
                abline(v = 0.5+(1:(length(as.vector(unique(obsSansExtreme$statut)))*
                       length(as.vector(unique(obsSansExtreme[, fact])))))*
                       length(as.vector(unique(obsSansExtreme$an))), col = "red")
                if (choix=="1")
                {
                    legend("top", "Enregistrements > 95% du maximum retirés", cex =0.7, col="red", text.col="red", merge=FALSE)
                }
                legend("topleft", TauxRenseignement, cex =0.8, col="red", bty="n", text.col="red", merge=FALSE)

                ## sélection d'espèces

            }else{             #si pas plus d'une valeur
                tkmessageBox(message=paste("Le champ", fact, "contient toujours la même valeur :", unique(obsSansExtreme[, fact]), "\n regroupement inefficace, sélectionnez un autre facteur"), icon="info")
                gestionMSGerreur.f("UneSeuleValeurRegroupement")
            }
        }else{          #si moins de 50% du critère renseigné
            tkmessageBox(message=paste("Graphique sans intérêt - plus de la moitié \n des enregistrements du critère de regroupement ", fact, "ne sont pas renseignés (NA)"))
            gestionMSGerreur.f("CritereMalRenseigne50")
        }
    }else{
        tkmessageBox(message="Graphique impossible - pas d'enregistrements dans votre sélection")
        gestionMSGerreur.f("ZeroEnregistrement")
    }
    choix <- "0"
}  #fin de GraphGroup1factUnitobs.f

################################################################################
## Nom    : GraphGroup2factUnitobs.f()
## Objet  : graphiques regroupant les observations sur une caractéristique du référentiel espèce
## Input  : table "listespunit" et un code champ
## Output : boxplot
################################################################################

GraphGroup2factUnitobs.f <- function ()
{

    print("fonction GraphGroup2factUnitobs.f activée")

    ## ###############################
    ## pour la table listespunit    #
    ## ###############################
    matable <- "listespunit"

    ## Choix du champ à représenter champobs
    choixchamptable.f(matable)
    print(champtrouve)
    ## Choix du facteur de regroupement
    choixDeuxFacteursUnitobs.f()
    print(fact21)
    print(fact22)
    objtable <- eval(parse(text=matable))
    objtable[, fact21] <- unitobs[, fact21][match(objtable$unite_observation, unitobs$unite_observation)]
    objtable[, fact22] <- unitobs[, fact22][match(objtable$unite_observation, unitobs$unite_observation)]
    objtable$an <- unitobs$an[match(objtable$unite_observation, unitobs$unite_observation)]
    objtable$statut <- unitobs$statut[match(objtable$unite_observation, unitobs$unite_observation)]
    obsSansExtreme <- objtable
    extremes.f()
    ## Graphiques sur un champ de obs
    obsSansExtreme[, champtrouve] <- EnleverMaxExtremes.f(obsSansExtreme[, champtrouve])    #  selon choix retourné par extremes.f()
    print(head(obsSansExtreme))
    TauxUnitobsCritereSansNA1 <- round(100*(length(unique(obsSansExtreme$unite_observation[is.na(obsSansExtreme[, fact21])==FALSE]))/length(unique(obsSansExtreme$unite_observation))), digits=2)
    TauxUnitobsCritereSansNA2 <- round(100*(length(unique(obsSansExtreme$unite_observation[is.na(obsSansExtreme[, fact22])==FALSE]))/length(unique(obsSansExtreme$unite_observation))), digits=2)
    TauxObsCritereSansNA1 <- round(100*(sum(table(obsSansExtreme[, fact21]))/length(obsSansExtreme[, fact21])), digits=2)
    TauxObsCritereSansNA2 <- round(100*(sum(table(obsSansExtreme[, fact22]))/length(obsSansExtreme[, fact22])), digits=2)
    TauxRenseignement <- paste("Taux de renseignement \ndu champs ", fact21, ":\n", TauxObsCritereSansNA1,
                               "% des observations \n", TauxUnitobsCritereSansNA1, "% des espèces\n", "du champs ", fact22, ":\n",
                               TauxObsCritereSansNA2, "% des observations\n", TauxUnitobsCritereSansNA2, "% des espèces\n")
    print(TauxRenseignement)
    if (length(unique(obsSansExtreme[, champtrouve]))>0)     #teste si il y a au moins un enregistrement
    {
        if (round(sum(table(list(obsSansExtreme[, fact21], obsSansExtreme[, fact22])))/length(list(obsSansExtreme[, fact21], obsSansExtreme[, fact22])), digits=2)>=0.5)
        {
            if (length(unique(list(obsSansExtreme[, fact21], obsSansExtreme[, fact22])))>1)  #teste si il y a plus d'une valeur pour le regroupement dans le champs
            {
                ## distribution des enregistrements groupés selon fact21 , fact22

                x11(width=50, height=20, pointsize=10)
                par(mar=c(9, 4, 5, 1), mgp=c(3, 1, 9))
                boxplot(obsSansExtreme[, champtrouve] ~obsSansExtreme[, fact21]+obsSansExtreme[, fact22], data=obsSansExtreme, varwidth = TRUE, las=3, main=paste("Regroupement des ", champtrouve, " selon \"", fact21, "\"et \"", fact22, "\""))
                nbObs <- tapply(obsSansExtreme[, champtrouve], list(obsSansExtreme[, fact21], obsSansExtreme[, fact22]), length)
                axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)), col.ticks="orange", col.axis = "orange", lty = 2, lwd = 0.5)
                legend("topleft", "Nombre d'enregistrements par Boxplot", cex =0.7, col="orange", text.col="orange", merge=FALSE)
                Moyenne <- as.vector(tapply(obsSansExtreme[, champtrouve], list(obsSansExtreme[, fact21], obsSansExtreme[, fact22]), na.rm = T, mean))
                points(Moyenne, pch=19, col="blue")
                text(Moyenne+(Moyenne/10), col = "blue", cex = 0.8, labels=as.character(round(Moyenne, digits=1)))
                abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")
                ## boxplot(spunit[, champtrouve] ~spunit$an+spunit$code_espece, las=2, cex.names=0.6, main=paste("Nombre d'individus par espèce et par an pour le site ", si, "\n"))
                ## nbObs <- tapply(spunit[, champtrouve], list(spunit$an, spunit$code_espece), length)
                if (choix=="1")
                {
                    legend("top", "Enregistrements > 95% du maximum retirés", cex =0.7, col="red", text.col="red", merge=FALSE)
                }
                legend("topleft", TauxRenseignement, cex =0.8, col="red", bty="n", text.col="red", merge=FALSE)
                print(paste("graphique de distribution des enregistrements groupés selon", fact21 , fact22, " réalisé"))

                ## distribution des enregistrements groupés selon le statut du site et fact21 , fact22

                textstatut <- c("HR : Hors Réserve", "PP : Protection Partielle", "RE : En réserve")
                x11(width=120, height=50, pointsize=10)
                par(mar=c(8, 4, 5, 1), mgp=c(3, 1, 9))
                boxplot(obsSansExtreme[, champtrouve][obsSansExtreme[, champtrouve]!= 0] ~obsSansExtreme$statut[obsSansExtreme[, champtrouve]!= 0]+obsSansExtreme[, fact21][obsSansExtreme[, champtrouve]!= 0]+obsSansExtreme[, fact22][obsSansExtreme[, champtrouve]!= 0], col=heat.colors(length(unique(obsSansExtreme$statut))), las=2, main=paste(champtrouve, " par enregistrement\n selon le statut et Regroupement selon \"", fact21, "\"et \"", fact22, "\"\n"))
                nbObs <- tapply(obsSansExtreme[, champtrouve][obsSansExtreme[, champtrouve]!= 0], list(obsSansExtreme$statut[obsSansExtreme[, champtrouve]!= 0], obsSansExtreme[, fact21][obsSansExtreme[, champtrouve]!= 0], obsSansExtreme[, fact22][obsSansExtreme[, champtrouve]!= 0]), length)
                legend("topleft", "Nombre d'enregistrement par Boxplot", cex =0.7, col="orange", text.col="orange", merge=FALSE)
                legend("topright", textstatut, col=heat.colors(length(unique(obsSansExtreme$statut))), pch = 15, cex =0.9, title="Statuts")
                axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)), col.ticks="orange", col.axis = "orange", lty = 2, lwd = 0.5)
                Moyenne <- as.vector(tapply(obsSansExtreme[, champtrouve][obsSansExtreme[, champtrouve]!= 0], list(obsSansExtreme$statut[obsSansExtreme[, champtrouve]!= 0], obsSansExtreme[, fact21][obsSansExtreme[, champtrouve]!= 0], obsSansExtreme[, fact22][obsSansExtreme[, champtrouve]!= 0]), na.rm = T, mean))
                points(Moyenne, pch=19, col="blue")
                if (length(Moyenne)>150)
                {
                    legend("center", "plus de 150 possibilités de boxplot\nFaites une sélection préalable sur votre jeux de données", cex =1, col="blue", text.col="blue", merge=FALSE)
                    print("plus de 150 possibilités de boxplot\nFaites une sélection préalable dans votre jeu de données\n...")
                }else{
                    abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")
                    abline(v = 0.5+(1:length(as.vector(unique(list(obsSansExtreme[, fact21], obsSansExtreme[, fact22])))))*length(as.vector(unique(obsSansExtreme$statut))), col = "red")
                }
                if (choix=="1")
                {
                    legend("top", "Enregistrements > 95% du maximum retirés", cex =0.7, col="red", text.col="red", merge=FALSE)
                }
                legend("topleft", TauxRenseignement, cex =0.8, col="red", bty="n", text.col="red", merge=FALSE)
                print(paste("graphique de distribution des enregistrements groupés selon le statut du site et", fact21 , fact22, " réalisé"))

                ## distribution des enregistrements groupés selon l'année de l'observation  et fact21 , fact22

                x11(width=120, height=50, pointsize=10)
                par(mar=c(8, 4, 5, 1), mgp=c(3, 1, 9))
                boxplot(obsSansExtreme[, champtrouve][obsSansExtreme[, champtrouve]!= 0] ~obsSansExtreme$statut[obsSansExtreme[, champtrouve]!= 0]+obsSansExtreme$an[obsSansExtreme[, champtrouve]!= 0]+obsSansExtreme[, fact21][obsSansExtreme[, champtrouve]!= 0]+obsSansExtreme[, fact22][obsSansExtreme[, champtrouve]!= 0], col=heat.colors(length(unique(obsSansExtreme$statut))), las=2, main=paste(champtrouve, " par enregistrement\n selon le statut puis l'année et Regroupement selon \"", fact21, "\"et \"", fact22, "\"\n"))
                nbObs <- tapply(obsSansExtreme[, champtrouve][obsSansExtreme[, champtrouve]!= 0], list(obsSansExtreme$statut[obsSansExtreme[, champtrouve]!= 0], obsSansExtreme$an[obsSansExtreme[, champtrouve]!= 0], obsSansExtreme[, fact21][obsSansExtreme[, champtrouve]!= 0], obsSansExtreme[, fact22][obsSansExtreme[, champtrouve]!= 0]), length)
                legend("topleft", "Nombre d'enregistrement par Boxplot", cex =0.7, col="orange", text.col="orange", merge=FALSE)
                legend("topright", textstatut, col=heat.colors(length(unique(obsSansExtreme$statut))), pch = 15, cex =0.9, title="Statuts")
                axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)), col.ticks="orange", col.axis = "orange", lty = 2, lwd = 0.5, cex =0.7)
                Moyenne <- as.vector(tapply(obsSansExtreme[, champtrouve][obsSansExtreme[, champtrouve]!= 0], list(obsSansExtreme$statut[obsSansExtreme[, champtrouve]!= 0], obsSansExtreme$an[obsSansExtreme[, champtrouve]!= 0], obsSansExtreme[, fact21][obsSansExtreme[, champtrouve]!= 0], obsSansExtreme[, fact22][obsSansExtreme[, champtrouve]!= 0]), na.rm = T, mean))
                points(Moyenne, pch=19, col="blue")
                if (length(Moyenne)>150)
                {
                    legend("center", "plus de 150 possibilités de boxplot\nFaites une sélection préalable sur votre jeux de données", cex =1, col="blue", text.col="blue", merge=FALSE)
                    print("plus de 150 possibilités de boxplot\nFaites une sélection préalable dans votre jeu de données\n...")
                }else{
                    abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")
                    abline(v = 0.5+(1:(length(as.vector(unique(obsSansExtreme$an)))*length(as.vector(unique(list(obsSansExtreme[, fact21][obsSansExtreme[, champtrouve]!= 0], obsSansExtreme[, fact22][obsSansExtreme[, champtrouve]!= 0]))))))*length(as.vector(unique(obsSansExtreme$statut))), col = "red")
                }
                if (choix=="1")
                {
                    legend("top", "Enregistrements > 95% du maximum retirés", cex =0.7, col="red", text.col="red", merge=FALSE)
                }
                legend("topleft", TauxRenseignement, cex =0.8, col="red", bty="n", text.col="red", merge=FALSE)
                print(paste("graphique de distribution des enregistrements groupés selon l'année de l'observation  et", fact21 , fact22, " réalisé"))

                ## distribution des enregistrements groupés selon le statut, l'année de l'observation  et fact21 , fact22

                x11(width=120, height=50, pointsize=10)
                par(mar=c(8, 4, 5, 1), mgp=c(3, 1, 9))
                boxplot(obsSansExtreme[, champtrouve][obsSansExtreme[, champtrouve]!= 0] ~obsSansExtreme$an[obsSansExtreme[, champtrouve]!= 0]+obsSansExtreme$statut[obsSansExtreme[, champtrouve]!= 0]+obsSansExtreme[, fact21][obsSansExtreme[, champtrouve]!= 0]+obsSansExtreme[, fact22][obsSansExtreme[, champtrouve]!= 0], col=heat.colors(length(unique(obsSansExtreme$statut))*length(unique(obsSansExtreme$an))), las=2, main=paste(champtrouve, " par enregistrement\n selon l'année puis le statut et Regroupement selon \"", fact21, "\"et \"", fact22, "\"\n"))
                nbObs <- tapply(obsSansExtreme[, champtrouve][obsSansExtreme[, champtrouve]!= 0], list(obsSansExtreme$an[obsSansExtreme[, champtrouve]!= 0], obsSansExtreme$statut[obsSansExtreme[, champtrouve]!= 0], obsSansExtreme[, fact21][obsSansExtreme[, champtrouve]!= 0], obsSansExtreme[, fact22][obsSansExtreme[, champtrouve]!= 0]), length)
                legend("topleft", "Nombre d'enregistrement par Boxplot", cex =0.7, col="orange", text.col="orange", merge=FALSE)
                legend("topright", textstatut, col=heat.colors(length(unique(obsSansExtreme$statut))), pch = 15, cex =0.9, title="Statuts")
                axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)), col.ticks="orange", col.axis = "orange", lty = 2, lwd = 0.5, cex =0.7)
                Moyenne <- as.vector(tapply(obsSansExtreme[, champtrouve][obsSansExtreme[, champtrouve]!= 0], list(obsSansExtreme$an[obsSansExtreme[, champtrouve]!= 0], obsSansExtreme$statut[obsSansExtreme[, champtrouve]!= 0], obsSansExtreme[, fact21][obsSansExtreme[, champtrouve]!= 0], obsSansExtreme[, fact22][obsSansExtreme[, champtrouve]!= 0]), na.rm = T, mean))
                points(Moyenne, pch=19, col="blue")
                if (length(Moyenne)>150)
                {
                    legend("center", "plus de 150 possibilités de boxplot\nFaites une sélection préalable sur votre jeux de données", cex =1, col="blue", text.col="blue", merge=FALSE)
                    print("plus de 150 possibilités de boxplot\nFaites une sélection préalable dans votre jeu de données\n...")
                }else{
                    abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")
                    abline(v = 0.5+(1:(length(as.vector(unique(obsSansExtreme$statut)))*length(as.vector(unique(list(obsSansExtreme[, fact21][obsSansExtreme[, champtrouve]!= 0], obsSansExtreme[, fact22][obsSansExtreme[, champtrouve]!= 0]))))))*length(as.vector(unique(obsSansExtreme$an))), col = "red")
                }
                if (choix=="1")
                {
                    legend("top", "Enregistrements > 95% du maximum retirés", cex =0.7, col="red", text.col="red", merge=FALSE)
                }
                legend("topleft", TauxRenseignement, cex =0.8, col="red", bty="n", text.col="red", merge=FALSE)
                print(paste("graphique de distribution des enregistrements le statut, l'année de l'observation  et", fact21 , fact22, " réalisé"))

                ## évolution de champtrouvé trouve en fonction des deux facteurs
                x11(width=12, height=8, pointsize=12)
                par(mar=c(5, 6, 4, 1))
                interaction.plot(obsSansExtreme[, fact21], obsSansExtreme[, fact22], obsSansExtreme[, champtrouve],
                                 lwd=2, col=cl[seq(550, (550+(4*(length(split(obsSansExtreme, obsSansExtreme[, fact21]))-1))), by=4)],
                                 type="b", fun=mean, trace.label = fact21, xlab=fact22, ylab=champtrouve,
                                 main=paste("Evolution des valeurs de ", champtrouve, " par", fact21))
            }else{             #si pas plus d'une valeur
                tkmessageBox(message=paste("Les champs", fact21, "et", fact22, "contiennent toujours la même valeur :", unique(obsSansExtreme[, fact21][, fact22]), "\n regroupement inefficace, sélectionnez un autre facteur"), icon="info")
                gestionMSGerreur.f("UneSeuleValeurRegroupement")
            }
        }else{          #si moins de 50% du critère renseigné
            tkmessageBox(message=paste("Graphique sans intérêt - plus de la moitié \n des enregistrements des critères de regroupement ", fact21, "et", fact22, "ne sont pas renseignés (NA)"))
            gestionMSGerreur.f("CritereMalRenseigne50")
        }
    }else{
        tkmessageBox(message="Graphique impossible - pas d'enregistrements dans votre sélection")
        gestionMSGerreur.f("ZeroEnregistrement")
    }
    choix <- "0"
}         #fin de GraphGroup2factUnitobs.f

################################################################################
## Nom    : GraphGroup1factEsp.f()
## Objet  : graphiques regroupant les observations sur une caractéristique du référentiel espèce
## Input  : table "listespunit" et un code champ
## Output : boxplot
################################################################################
GraphGroup1factEsp.f <- function ()
{

    print("fonction GraphGroup1factEsp.f activée")
    ## ###############################
    ## pour la table obs    #
    ## ###############################
    ## matable="obs"

    ## ###############################
    ## pour la table listespunit    #
    ## ###############################
    matable <- "listespunit"

    ## Choix du champ à représenter champobs
    choixchamptable.f(matable)
    print(champtrouve)
    ## Choix du facteur de regroupement
    critereespref.f()
    print(factesp)
    objtable <- eval(parse(text=matable))
    objtable[, factesp] <- especes[, factesp][match(objtable$code_espece, especes$code_espece)]
    objtable$an <- unitobs$an[match(objtable$unite_observation, unitobs$unite_observation)]
    objtable$statut <- unitobs$statut[match(objtable$unite_observation, unitobs$unite_observation)]
    obsSansExtreme <- objtable
    extremes.f()
    ## Graphiques sur un champ de obs
    obsSansExtreme[, champtrouve] <- EnleverMaxExtremes.f(obsSansExtreme[, champtrouve])    #  selon choix retourné par extremes.f()
    print(head(obsSansExtreme))
    TauxEspCritereSansNA <- round(100*(length(unique(obsSansExtreme$code_espece[is.na(obsSansExtreme[, factesp])==FALSE]))/length(unique(obsSansExtreme$code_espece))), digits=2)
    TauxObsCritereSansNA <- round(100*(sum(table(obsSansExtreme[, factesp]))/length(obsSansExtreme[, factesp])), digits=2)
    TauxRenseignement <- paste("Taux de renseignement \ndu champs ", factesp, ":\n", TauxObsCritereSansNA, "% des observations\n", TauxEspCritereSansNA, "% des espèces\n")
    if (length(unique(obsSansExtreme[, champtrouve]))>0)     #teste si il y a au moins un enregistrement
    {
        if (round(sum(table(obsSansExtreme[, factesp]))/length(obsSansExtreme[, factesp]), digits=2)>=0.5)
        {
            if (length(unique(obsSansExtreme[, factesp]))>1)  #teste si il y a plus d'une valeur pour le regroupement dans le champs
            {
                ## distribution des enregistrements groupés selon le critère

                x11(width=50, height=20, pointsize=10)
                par(mar=c(9, 4, 5, 1), mgp=c(3, 1, 9))
                boxplot(obsSansExtreme[, champtrouve] ~obsSansExtreme[, factesp], data=obsSansExtreme, varwidth = TRUE, las=3, main=paste("Regroupement des ", champtrouve, " selon \"", factesp, "\""))
                nbObs <- tapply(obsSansExtreme[, champtrouve], obsSansExtreme[, factesp], length)
                axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)), col.ticks="orange", col.axis = "orange", lty = 2, lwd = 0.5)
                legend("topleft", "Nombre d'enregistrements par Boxplot", cex =0.7, col="orange", text.col="orange", merge=FALSE)
                Moyenne <- as.vector(tapply(obsSansExtreme[, champtrouve], obsSansExtreme[, factesp], na.rm = T, mean))
                points(Moyenne, pch=19, col="blue")
                text(Moyenne+(Moyenne/10), col = "blue", cex = 0.8, labels=as.character(round(Moyenne, digits=1)))
                abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")
                ## boxplot(spunit[, champtrouve] ~spunit$an+spunit$code_espece, las=2, cex.names=0.6, main=paste("Nombre d'individus par espèce et par an pour le site ", si, "\n"))
                ## nbObs <- tapply(spunit[, champtrouve], list(spunit$an, spunit$code_espece), length)
                if (choix=="1")
                {
                    legend("top", "Enregistrements > 95% du maximum retirés", cex =0.7, col="red", text.col="red", merge=FALSE)
                }
                legend("topleft", TauxRenseignement, cex =0.8, bty="n", text.col="red", merge=FALSE)

                ## distribution des enregistrements groupés selon le critère et le statut du site

                textstatut <- c("HR : Hors Réserve", "PP : Protection Partielle", "RE : En réserve")
                x11(width=120, height=50, pointsize=10)
                par(mar=c(8, 4, 5, 1), mgp=c(3, 1, 9))
                boxplot(obsSansExtreme[, champtrouve][obsSansExtreme[, champtrouve]!= 0] ~obsSansExtreme$statut[obsSansExtreme[, champtrouve]!= 0]+obsSansExtreme[, factesp][obsSansExtreme[, champtrouve]!= 0], col=heat.colors(length(unique(obsSansExtreme$statut))), las=2, main=paste(champtrouve, " par enregistrement\n selon le statut et Regroupement selon \"", factesp, "\"\n\n"))
                nbObs <- tapply(obsSansExtreme[, champtrouve][obsSansExtreme[, champtrouve]!= 0], list(obsSansExtreme$statut[obsSansExtreme[, champtrouve]!= 0], obsSansExtreme[, factesp][obsSansExtreme[, champtrouve]!= 0]), length)
                legend("topleft", "Nombre d'enregistrement par Boxplot", cex =0.7, col="orange", text.col="orange", merge=FALSE)
                legend("topright", textstatut, col=heat.colors(length(unique(obsSansExtreme$statut))), pch = 15, cex =0.9, title="Statuts")
                axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)), col.ticks="orange", col.axis = "orange", lty = 2, lwd = 0.5)
                Moyenne <- as.vector(tapply(obsSansExtreme[, champtrouve][obsSansExtreme[, champtrouve]!= 0], list(obsSansExtreme$statut[obsSansExtreme[, champtrouve]!= 0], obsSansExtreme[, factesp][obsSansExtreme[, champtrouve]!= 0]), na.rm = T, mean))
                points(Moyenne, pch=19, col="blue")
                abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")
                abline(v = 0.5+(1:length(as.vector(unique(obsSansExtreme[, factesp]))))*length(as.vector(unique(obsSansExtreme$statut))), col = "red")
                if (choix=="1")
                {
                    legend("top", "Enregistrements > 95% du maximum retirés", cex =0.7, col="red", text.col="red", merge=FALSE)
                }
                legend("topleft", TauxRenseignement, cex =0.8, bty="n", col="red", text.col="red", merge=FALSE)

                ## distribution des enregistrements groupés selon le critère et l'année de l'observation

                x11(width=120, height=50, pointsize=10)
                par(mar=c(8, 4, 5, 1), mgp=c(3, 1, 9))
                boxplot(obsSansExtreme[, champtrouve][obsSansExtreme[, champtrouve]!= 0] ~obsSansExtreme$statut[obsSansExtreme[, champtrouve]!= 0]+obsSansExtreme$an[obsSansExtreme[, champtrouve]!= 0]+obsSansExtreme[, factesp][obsSansExtreme[, champtrouve]!= 0], col=heat.colors(length(unique(obsSansExtreme$statut))), las=2, main=paste(champtrouve, " par enregistrement\n selon le statut puis l'année et Regroupement selon \"", factesp, "\"\n\n"))
                nbObs <- tapply(obsSansExtreme[, champtrouve][obsSansExtreme[, champtrouve]!= 0], list(obsSansExtreme$statut[obsSansExtreme[, champtrouve]!= 0], obsSansExtreme$an[obsSansExtreme[, champtrouve]!= 0], obsSansExtreme[, factesp][obsSansExtreme[, champtrouve]!= 0]), length)
                legend("topleft", "Nombre d'enregistrement par Boxplot", cex =0.7, col="orange", text.col="orange", merge=FALSE)
                legend("topright", textstatut, col=heat.colors(length(unique(obsSansExtreme$statut))), pch = 15, cex =0.9, title="Statuts")
                axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)), col.ticks="orange", col.axis = "orange", lty = 2, lwd = 0.5, cex =0.7)
                Moyenne <- as.vector(tapply(obsSansExtreme[, champtrouve][obsSansExtreme[, champtrouve]!= 0], list(obsSansExtreme$statut[obsSansExtreme[, champtrouve]!= 0], obsSansExtreme$an[obsSansExtreme[, champtrouve]!= 0], obsSansExtreme[, factesp][obsSansExtreme[, champtrouve]!= 0]), na.rm = T, mean))
                points(Moyenne, pch=19, col="blue")
                abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")
                abline(v = 0.5+(1:(length(as.vector(unique(obsSansExtreme$an)))*length(as.vector(unique(obsSansExtreme[, factesp])))))*length(as.vector(unique(obsSansExtreme$statut))), col = "red")
                if (choix=="1")
                {
                    legend("top", "Enregistrements > 95% du maximum retirés", cex =0.7, col="red", text.col="red", merge=FALSE)
                }
                legend("topleft", TauxRenseignement, cex =0.8, col="red", bty="n", text.col="red", merge=FALSE)

                ## distribution des enregistrements groupés selon le critère et le statut l'année de l'observation

                x11(width=120, height=50, pointsize=10)
                par(mar=c(8, 4, 5, 1), mgp=c(3, 1, 9))
                boxplot(obsSansExtreme[, champtrouve][obsSansExtreme[, champtrouve]!= 0] ~obsSansExtreme$an[obsSansExtreme[, champtrouve]!= 0]+obsSansExtreme$statut[obsSansExtreme[, champtrouve]!= 0]+obsSansExtreme[, factesp][obsSansExtreme[, champtrouve]!= 0], col=heat.colors(length(unique(obsSansExtreme$statut))*length(unique(obsSansExtreme$an))), las=2, main=paste(champtrouve, " par enregistrement\n selon l'année puis le statut et Regroupement selon \"", factesp, "\"\n\n"))
                nbObs <- tapply(obsSansExtreme[, champtrouve][obsSansExtreme[, champtrouve]!= 0], list(obsSansExtreme$an[obsSansExtreme[, champtrouve]!= 0], obsSansExtreme$statut[obsSansExtreme[, champtrouve]!= 0], obsSansExtreme[, factesp][obsSansExtreme[, champtrouve]!= 0]), length)
                legend("topleft", "Nombre d'enregistrement par Boxplot", cex =0.7, col="orange", text.col="orange", merge=FALSE)
                legend("topright", textstatut, col=heat.colors(length(unique(obsSansExtreme$statut))), pch = 15, cex =0.9, title="Statuts")
                axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)), col.ticks="orange", col.axis = "orange", lty = 2, lwd = 0.5, cex =0.7)
                Moyenne <- as.vector(tapply(obsSansExtreme[, champtrouve][obsSansExtreme[, champtrouve]!= 0], list(obsSansExtreme$an[obsSansExtreme[, champtrouve]!= 0], obsSansExtreme$statut[obsSansExtreme[, champtrouve]!= 0], obsSansExtreme[, factesp][obsSansExtreme[, champtrouve]!= 0]), na.rm = T, mean))
                points(Moyenne, pch=19, col="blue")
                abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")
                abline(v = 0.5+(1:(length(as.vector(unique(obsSansExtreme$statut)))*length(as.vector(unique(obsSansExtreme[, factesp])))))*length(as.vector(unique(obsSansExtreme$an))), col = "red")
                if (choix=="1")
                {
                    legend("top", "Enregistrements > 95% du maximum retirés", cex =0.7, col="red", text.col="red", merge=FALSE)
                }
                legend("topleft", TauxRenseignement, cex =0.8, col="red", bty="n", text.col="red", merge=FALSE)

                interaction.plot(obsSansExtreme$an, obsSansExtreme[, factesp], obsSansExtreme[, champtrouve], lwd=2, col=cl[seq(550, (550+(4*(length(split(obsSansExtreme, obsSansExtreme$an))-1))), by=4)], type="b",
                                 fun=mean, trace.label = factesp, xlab="Annee", ylab=champtrouve, main=paste("Evolution des valeurs de ", champtrouve, " par ", factesp))

            }else{             #si pas plus d'une valeur
                tkmessageBox(message=paste("Le champ", factesp, "contient toujours la même valeur :", unique(obsSansExtreme[, factesp]), "\n regroupement inefficace, sélectionnez un autre facteur"), icon="info")
                gestionMSGerreur.f("UneSeuleValeurRegroupement")
            }
        }else{          #si moins de 50% du critère renseigné
            tkmessageBox(message=paste("Graphique sans intérêt - plus de la moitié \n des enregistrements du critère de regroupement ", factesp, "ne sont pas renseignés (NA)"))
            gestionMSGerreur.f("CritereMalRenseigne50")
        }
    }else{
        tkmessageBox(message="Graphique impossible - pas d'enregistrements dans votre sélection")
        gestionMSGerreur.f("ZeroEnregistrement")
    }
    choix <- "0"
}
################################################################################
## Nom    : GraphGroup2factEsp.f()
## Objet  : graphiques regroupant les observations sur une caractéristique du référentiel espèce
## Input  : table "listespunit" et un code champ
## Output : boxplot
################################################################################

GraphGroup2factEsp.f <- function ()
{

    print("fonction GraphGroup2factEsp.f activée")

    ## ###############################
    ## pour la table obs    #
    ## ###############################
    ## matable="obs"

    ## ###############################
    ## pour la table listespunit    #
    ## ###############################
    matable <- "listespunit"

    ## ###############################
    ## pour la table listespunit    #
    ## ###############################
    ## matable="unitobs"

    ## Choix du champ à représenter champobs
    choixchamptable.f(matable)
    print(champtrouve)
    ## Choix du facteur de regroupement
    critereespref.f()
    critere2espref.f()


    print(factesp)
    print(factesp2)

    objtable <- eval(parse(text=matable))
    objtable[, factesp] <- especes[, factesp][match(objtable$code_espece, especes$code_espece)]
    objtable[, factesp2] <- especes[, factesp2][match(objtable$code_espece, especes$code_espece)]
    objtable$an <- unitobs$an[match(objtable$unite_observation, unitobs$unite_observation)]
    objtable$statut <- unitobs$statut[match(objtable$unite_observation, unitobs$unite_observation)]
    obsSansExtreme <- objtable
    extremes.f()
    ## Graphiques sur un champ de obs
    obsSansExtreme[, champtrouve] <- EnleverMaxExtremes.f(obsSansExtreme[, champtrouve])    #  selon choix retourné par extremes.f()
    print(head(obsSansExtreme))
    TauxEspCritereSansNA1 <- round(100*(length(unique(obsSansExtreme$code_espece[is.na(obsSansExtreme[, factesp])==FALSE]))/length(unique(obsSansExtreme$code_espece))), digits=2)
    TauxEspCritereSansNA2 <- round(100*(length(unique(obsSansExtreme$code_espece[is.na(obsSansExtreme[, factesp2])==FALSE]))/length(unique(obsSansExtreme$code_espece))), digits=2)
    TauxObsCritereSansNA1 <- round(100*(sum(table(obsSansExtreme[, factesp]))/length(obsSansExtreme[, factesp])), digits=2)
    TauxObsCritereSansNA2 <- round(100*(sum(table(obsSansExtreme[, factesp2]))/length(obsSansExtreme[, factesp2])), digits=2)
    TauxRenseignement <- paste("Taux de renseignement \ndu champs ", factesp, ":\n", TauxObsCritereSansNA1, "% des observations \n", TauxEspCritereSansNA1, "% des espèces\n", "du champs ", factesp2, ":\n", TauxObsCritereSansNA2, "% des observations\n", TauxEspCritereSansNA2, "% des espèces\n")
    print(TauxRenseignement)
    if (length(unique(obsSansExtreme[, champtrouve]))>0)     #teste si il y a au moins un enregistrement
    {
        if (round(sum(table(list(obsSansExtreme[, factesp], obsSansExtreme[, factesp2])))/length(list(obsSansExtreme[, factesp], obsSansExtreme[, factesp2])), digits=2)>=0.5)
        {
            if (length(unique(list(obsSansExtreme[, factesp], obsSansExtreme[, factesp2])))>1)  #teste si il y a plus d'une valeur pour le regroupement dans le champs
            {
                ## distribution des enregistrements groupés selon factesp , factesp2

                x11(width=80, height=50, pointsize=10)
                par(mar=c(9, 4, 5, 1), mgp=c(3, 1, 9))
                boxplot(obsSansExtreme[, champtrouve] ~obsSansExtreme[, factesp]+obsSansExtreme[, factesp2], data=obsSansExtreme, varwidth = TRUE, las=3, main=paste("Regroupement des ", champtrouve, " selon \"", factesp, "\"et \"", factesp2, "\""))
                nbObs <- tapply(obsSansExtreme[, champtrouve], list(obsSansExtreme[, factesp], obsSansExtreme[, factesp2]), length)
                axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)), col.ticks="orange", col.axis = "orange", lty = 2, lwd = 0.5)
                legend("topleft", "Nombre d'enregistrements par Boxplot", cex =0.7, col="orange", text.col="orange", merge=FALSE)
                Moyenne <- as.vector(tapply(obsSansExtreme[, champtrouve], list(obsSansExtreme[, factesp], obsSansExtreme[, factesp2]), na.rm = T, mean))
                points(Moyenne, pch=19, col="blue")
                text(Moyenne+(Moyenne/10), col = "blue", cex = 0.8, labels=as.character(round(Moyenne, digits=1)))
                abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")
                ## boxplot(spunit[, champtrouve] ~spunit$an+spunit$code_espece, las=2, cex.names=0.6, main=paste("Nombre d'individus par espèce et par an pour le site ", si, "\n"))
                ## nbObs <- tapply(spunit[, champtrouve], list(spunit$an, spunit$code_espece), length)
                if (choix=="1")
                {
                    legend("top", "Enregistrements > 95% du maximum retirés", cex =0.7, col="red", text.col="red", merge=FALSE)
                }
                legend("topleft", TauxRenseignement, cex =0.8, col="red", bty="n", text.col="red", merge=FALSE)
                print(paste("graphique de distribution des enregistrements groupés selon", factesp , factesp2, " réalisé"))

                ## distribution des enregistrements groupés selon le statut du site et factesp , factesp2

                textstatut <- c("HR : Hors Réserve", "PP : Protection Partielle", "RE : En réserve")
                x11(width=120, height=50, pointsize=10)
                par(mar=c(8, 4, 5, 1), mgp=c(3, 1, 9))
                boxplot(obsSansExtreme[, champtrouve][obsSansExtreme[, champtrouve]!= 0] ~obsSansExtreme$statut[obsSansExtreme[, champtrouve]!= 0]+obsSansExtreme[, factesp][obsSansExtreme[, champtrouve]!= 0]+obsSansExtreme[, factesp2][obsSansExtreme[, champtrouve]!= 0], col=heat.colors(length(unique(obsSansExtreme$statut))), las=2, main=paste(champtrouve, " par enregistrement\n selon le statut et Regroupement selon \"", factesp, "\"et \"", factesp2, "\"\n"))
                nbObs <- tapply(obsSansExtreme[, champtrouve][obsSansExtreme[, champtrouve]!= 0], list(obsSansExtreme$statut[obsSansExtreme[, champtrouve]!= 0], obsSansExtreme[, factesp][obsSansExtreme[, champtrouve]!= 0], obsSansExtreme[, factesp2][obsSansExtreme[, champtrouve]!= 0]), length)
                legend("topleft", "Nombre d'enregistrement par Boxplot", cex =0.7, col="orange", text.col="orange", merge=FALSE)
                legend("topright", textstatut, col=heat.colors(length(unique(obsSansExtreme$statut))), pch = 15, cex =0.9, title="Statuts")
                axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)), col.ticks="orange", col.axis = "orange", lty = 2, lwd = 0.5)
                Moyenne <- as.vector(tapply(obsSansExtreme[, champtrouve][obsSansExtreme[, champtrouve]!= 0], list(obsSansExtreme$statut[obsSansExtreme[, champtrouve]!= 0], obsSansExtreme[, factesp][obsSansExtreme[, champtrouve]!= 0], obsSansExtreme[, factesp2][obsSansExtreme[, champtrouve]!= 0]), na.rm = T, mean))
                points(Moyenne, pch=19, col="blue")
                if (length(Moyenne)>150)
                {
                    legend("center", "plus de 150 possibilités de boxplot\nFaites une sélection préalable sur votre jeux de données", cex =1, col="blue", text.col="blue", merge=FALSE)
                    print("plus de 150 possibilités de boxplot\nFaites une sélection préalable dans votre jeu de données\n...")
                }else{
                    abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")
                    abline(v = 0.5+(1:length(as.vector(unique(list(obsSansExtreme[, factesp], obsSansExtreme[, factesp2])))))*length(as.vector(unique(obsSansExtreme$statut))), col = "red")
                }
                if (choix=="1")
                {
                    legend("top", "Enregistrements > 95% du maximum retirés", cex =0.7, col="red", text.col="red", merge=FALSE)
                }
                legend("topleft", TauxRenseignement, cex =0.8, col="red", bty="n", text.col="red", merge=FALSE)
                print(paste("graphique de distribution des enregistrements groupés selon le statut du site et", factesp , factesp2, " réalisé"))

                ## distribution des enregistrements groupés selon l'année de l'observation  et factesp , factesp2

                x11(width=120, height=50, pointsize=10)
                par(mar=c(8, 4, 5, 1), mgp=c(3, 1, 9))
                boxplot(obsSansExtreme[, champtrouve][obsSansExtreme[, champtrouve]!= 0] ~obsSansExtreme$statut[obsSansExtreme[, champtrouve]!= 0]+obsSansExtreme$an[obsSansExtreme[, champtrouve]!= 0]+obsSansExtreme[, factesp][obsSansExtreme[, champtrouve]!= 0]+obsSansExtreme[, factesp2][obsSansExtreme[, champtrouve]!= 0], col=heat.colors(length(unique(obsSansExtreme$statut))), las=2, main=paste(champtrouve, " par enregistrement\n selon le statut puis l'année et Regroupement selon \"", factesp, "\"et \"", factesp2, "\"\n"))
                nbObs <- tapply(obsSansExtreme[, champtrouve][obsSansExtreme[, champtrouve]!= 0], list(obsSansExtreme$statut[obsSansExtreme[, champtrouve]!= 0], obsSansExtreme$an[obsSansExtreme[, champtrouve]!= 0], obsSansExtreme[, factesp][obsSansExtreme[, champtrouve]!= 0], obsSansExtreme[, factesp2][obsSansExtreme[, champtrouve]!= 0]), length)
                legend("topleft", "Nombre d'enregistrement par Boxplot", cex =0.7, col="orange", text.col="orange", merge=FALSE)
                legend("topright", textstatut, col=heat.colors(length(unique(obsSansExtreme$statut))), pch = 15, cex =0.9, title="Statuts")
                axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)), col.ticks="orange", col.axis = "orange", lty = 2, lwd = 0.5, cex =0.7)
                Moyenne <- as.vector(tapply(obsSansExtreme[, champtrouve][obsSansExtreme[, champtrouve]!= 0], list(obsSansExtreme$statut[obsSansExtreme[, champtrouve]!= 0], obsSansExtreme$an[obsSansExtreme[, champtrouve]!= 0], obsSansExtreme[, factesp][obsSansExtreme[, champtrouve]!= 0], obsSansExtreme[, factesp2][obsSansExtreme[, champtrouve]!= 0]), na.rm = T, mean))
                points(Moyenne, pch=19, col="blue")
                if (length(Moyenne)>150)
                {
                    legend("center", "plus de 150 possibilités de boxplot\nFaites une sélection préalable sur votre jeux de données", cex =1, col="blue", text.col="blue", merge=FALSE)
                    print("plus de 150 possibilités de boxplot\nFaites une sélection préalable dans votre jeu de données\n...")
                }else{
                    abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")
                    abline(v = 0.5+(1:(length(as.vector(unique(obsSansExtreme$an)))*length(as.vector(unique(list(obsSansExtreme[, factesp][obsSansExtreme[, champtrouve]!= 0], obsSansExtreme[, factesp2][obsSansExtreme[, champtrouve]!= 0]))))))*length(as.vector(unique(obsSansExtreme$statut))), col = "red")
                }
                if (choix=="1")
                {
                    legend("top", "Enregistrements > 95% du maximum retirés", cex =0.7, col="red", text.col="red", merge=FALSE)
                }
                legend("topleft", TauxRenseignement, cex =0.8, col="red", bty="n", text.col="red", merge=FALSE)
                print(paste("graphique de distribution des enregistrements groupés selon l'année de l'observation  et", factesp , factesp2, " réalisé"))

                ## distribution des enregistrements groupés selon le statut, l'année de l'observation  et factesp , factesp2

                x11(width=120, height=50, pointsize=10)
                par(mar=c(8, 4, 5, 1), mgp=c(3, 1, 9))
                boxplot(obsSansExtreme[, champtrouve][obsSansExtreme[, champtrouve]!= 0] ~obsSansExtreme$an[obsSansExtreme[, champtrouve]!= 0]+obsSansExtreme$statut[obsSansExtreme[, champtrouve]!= 0]+obsSansExtreme[, factesp][obsSansExtreme[, champtrouve]!= 0]+obsSansExtreme[, factesp2][obsSansExtreme[, champtrouve]!= 0], col=heat.colors(length(unique(obsSansExtreme$statut))*length(unique(obsSansExtreme$an))), las=2, main=paste(champtrouve, " par enregistrement\n selon l'année puis le statut et Regroupement selon \"", factesp, "\"et \"", factesp2, "\"\n"))
                nbObs <- tapply(obsSansExtreme[, champtrouve][obsSansExtreme[, champtrouve]!= 0], list(obsSansExtreme$an[obsSansExtreme[, champtrouve]!= 0], obsSansExtreme$statut[obsSansExtreme[, champtrouve]!= 0], obsSansExtreme[, factesp][obsSansExtreme[, champtrouve]!= 0], obsSansExtreme[, factesp2][obsSansExtreme[, champtrouve]!= 0]), length)
                legend("topleft", "Nombre d'enregistrement par Boxplot", cex =0.7, col="orange", text.col="orange", merge=FALSE)
                legend("topright", textstatut, col=heat.colors(length(unique(obsSansExtreme$statut))), pch = 15, cex =0.9, title="Statuts")
                axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)), col.ticks="orange", col.axis = "orange", lty = 2, lwd = 0.5, cex =0.7)
                Moyenne <- as.vector(tapply(obsSansExtreme[, champtrouve][obsSansExtreme[, champtrouve]!= 0], list(obsSansExtreme$an[obsSansExtreme[, champtrouve]!= 0], obsSansExtreme$statut[obsSansExtreme[, champtrouve]!= 0], obsSansExtreme[, factesp][obsSansExtreme[, champtrouve]!= 0], obsSansExtreme[, factesp2][obsSansExtreme[, champtrouve]!= 0]), na.rm = T, mean))
                points(Moyenne, pch=19, col="blue")
                if (length(Moyenne)>150)
                {
                    legend("center", "plus de 150 possibilités de boxplot\nFaites une sélection préalable sur votre jeux de données", cex =1, col="blue", text.col="blue", merge=FALSE)
                    print("plus de 150 possibilités de boxplot\nFaites une sélection préalable dans votre jeu de données\n...")
                }else{
                    abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")
                    abline(v = 0.5+(1:(length(as.vector(unique(obsSansExtreme$statut)))*length(as.vector(unique(list(obsSansExtreme[, factesp][obsSansExtreme[, champtrouve]!= 0], obsSansExtreme[, factesp2][obsSansExtreme[, champtrouve]!= 0]))))))*length(as.vector(unique(obsSansExtreme$an))), col = "red")
                }
                if (choix=="1")
                {
                    legend("top", "Enregistrements > 95% du maximum retirés", cex =0.7, col="red", text.col="red", merge=FALSE)
                }
                legend("topleft", TauxRenseignement, cex =0.8, col="red", bty="n", text.col="red", merge=FALSE)
                print(paste("graphique de distribution des enregistrements le statut, l'année de l'observation  et", factesp , ", ", factesp2, " réalisé"))

                interaction.plot(obsSansExtreme$an, obsSansExtreme[, factesp], obsSansExtreme[, champtrouve], lwd=2, col=cl[seq(550, (550+(4*(length(split(obsSansExtreme, obsSansExtreme$an))-1))), by=4)], type="b",
                                 fun=mean, trace.label = factesp, xlab="Annee", ylab=champtrouve, main=paste("Evolution des valeurs de ", champtrouve, " par ", factesp))

                interaction.plot(obsSansExtreme$an, obsSansExtreme[, factesp2], obsSansExtreme[, champtrouve], lwd=2, col=cl[seq(550, (550+(4*(length(split(obsSansExtreme, obsSansExtreme$an))-1))), by=4)], type="b",
                                 fun=mean, trace.label = factesp2, xlab="Annee", ylab=champtrouve, main=paste("Evolution des valeurs de ", champtrouve, " par ", factesp2))

            }else{             #si pas plus d'une valeur
                tkmessageBox(message=paste("Les champs", factesp, "et", factesp2, "contiennent toujours la même valeur :", unique(obsSansExtreme[, factesp][, factesp2]), "\n regroupement inefficace, sélectionnez un autre facteur"), icon="info")
                gestionMSGerreur.f("UneSeuleValeurRegroupement")
            }
        }else{          #si moins de 50% du critère renseigné
            tkmessageBox(message=paste("Graphique sans intérêt - plus de la moitié \n des enregistrements des critères de regroupement ", factesp, "et", factesp2, "ne sont pas renseignés (NA)"))
            gestionMSGerreur.f("CritereMalRenseigne50")
        }
    }else{
        tkmessageBox(message="Graphique impossible - pas d'enregistrements dans votre sélection")
        gestionMSGerreur.f("ZeroEnregistrement")
    }
    choix <- "0"
}

################################################################################
## Nom    : GraphGroup2factEspUnitobs.f()
## Objet  : graphiques regroupant les observations sur une caractéristique du référentiel espèce
## Input  : table "listespunit" et un code champ
## Output : boxplot
################################################################################

GraphGroup2factEspUnitobs.f <- function ()
{

    print("fonction GraphGroup2factEspUnitobs.f activée")

    ## ###############################
    ## pour la table listespunit    #
    ## ###############################
    matable <- "listespunit"

    ## Choix du champ à représenter champobs
    choixchamptable.f(matable)
    print(champtrouve)
    ## Choix du facteur de regroupement
    critereespref.f()
    print(factesp)
    choixunfacteurUnitobs.f()
    print(fact)

    objtable <- eval(parse(text=matable))
    objtable[, fact] <- unitobs[, fact][match(objtable$unite_observation, unitobs$unite_observation)]
    objtable$an <- unitobs$an[match(objtable$unite_observation, unitobs$unite_observation)]
    objtable$statut <- unitobs$statut[match(objtable$unite_observation, unitobs$unite_observation)]
    objtable[, factesp] <- especes[, factesp][match(objtable$code_espece, especes$code_espece)]
    obsSansExtreme <- objtable
    extremes.f()
    ## Graphiques sur un champ de obs
    obsSansExtreme[, champtrouve] <- EnleverMaxExtremes.f(obsSansExtreme[, champtrouve])    #  selon choix retourné par extremes.f()
    print(head(obsSansExtreme))

    TauxUnitobsCritereSansNA <- round(100*(length(unique(obsSansExtreme$unite_observation[is.na(obsSansExtreme[, fact])==FALSE]))/length(unique(obsSansExtreme$unite_observation))), digits=2)
    TauxObsCritereSansNA <- round(100*(sum(table(obsSansExtreme[, fact]))/length(obsSansExtreme[, fact])), digits=2)
    TauxEspCritereSansNA1 <- round(100*(length(unique(obsSansExtreme$code_espece[is.na(obsSansExtreme[, factesp])==FALSE]))/length(unique(obsSansExtreme$code_espece))), digits=2)
    TauxObsCritereSansNA1 <- round(100*(sum(table(obsSansExtreme[, factesp]))/length(obsSansExtreme[, factesp])), digits=2)

    TauxRenseignement <- paste("Taux de renseignement \ndu champs ", factesp, ":\n", TauxObsCritereSansNA1, "% des observations \n", TauxEspCritereSansNA1, "% des espèces\n", "du champs ", fact, ":\n", TauxObsCritereSansNA, "% des observations\n", TauxUnitobsCritereSansNA, "% des unités d'observations\n")
    print(TauxRenseignement)
    if (length(unique(obsSansExtreme[, champtrouve]))>0)     #teste si il y a au moins un enregistrement
    {
        if (round(sum(table(list(obsSansExtreme[, factesp], obsSansExtreme[, fact])))/length(list(obsSansExtreme[, factesp], obsSansExtreme[, fact])), digits=2)>=0.5)
        {
            if (length(unique(list(obsSansExtreme[, factesp], obsSansExtreme[, fact])))>1)  #teste si il y a plus d'une valeur pour le regroupement dans le champs
            {
                ## distribution des enregistrements groupés selon factesp , fact

                x11(width=50, height=50, pointsize=10)
                par(mar=c(9, 4, 5, 1), mgp=c(3, 1, 9))
                boxplot(obsSansExtreme[, champtrouve] ~obsSansExtreme[, factesp]+obsSansExtreme[, fact], data=obsSansExtreme, varwidth = TRUE, las=3, main=paste("Regroupement des ", champtrouve, " selon \"", factesp, "\"et \"", fact, "\""))
                nbObs <- tapply(obsSansExtreme[, champtrouve], list(obsSansExtreme[, factesp], obsSansExtreme[, fact]), length)
                axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)), col.ticks="orange", col.axis = "orange", lty = 2, lwd = 0.5)
                legend("topleft", "Nombre d'enregistrements par Boxplot", cex =0.7, col="orange", text.col="orange", merge=FALSE)
                Moyenne <- as.vector(tapply(obsSansExtreme[, champtrouve], list(obsSansExtreme[, factesp], obsSansExtreme[, fact]), na.rm = T, mean))
                points(Moyenne, pch=19, col="blue")
                text(Moyenne+(Moyenne/10), col = "blue", cex = 0.8, labels=as.character(round(Moyenne, digits=1)))
                abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")
                ## boxplot(spunit[, champtrouve] ~spunit$an+spunit$code_espece, las=2, cex.names=0.6, main=paste("Nombre d'individus par espèce et par an pour le site ", si, "\n"))
                ## nbObs <- tapply(spunit[, champtrouve], list(spunit$an, spunit$code_espece), length)
                if (choix=="1")
                {
                    legend("top", "Enregistrements > 95% du maximum retirés", cex =0.7, col="red", text.col="red", merge=FALSE)
                }
                legend("topleft", TauxRenseignement, cex =0.8, col="red", bty="n", text.col="red", merge=FALSE)
                print(paste("graphique de distribution des enregistrements groupés selon", factesp , fact, " réalisé"))

                ## distribution des enregistrements groupés selon le statut du site et factesp , fact

                textstatut <- c("HR : Hors Réserve", "PP : Protection Partielle", "RE : En réserve")
                x11(width=120, height=50, pointsize=10)
                par(mar=c(8, 4, 5, 1), mgp=c(3, 1, 9))
                boxplot(obsSansExtreme[, champtrouve][obsSansExtreme[, champtrouve]!= 0] ~obsSansExtreme$statut[obsSansExtreme[, champtrouve]!= 0]+obsSansExtreme[, factesp][obsSansExtreme[, champtrouve]!= 0]+obsSansExtreme[, fact][obsSansExtreme[, champtrouve]!= 0], col=heat.colors(length(unique(obsSansExtreme$statut))), las=2, main=paste(champtrouve, " par enregistrement\n selon le statut et Regroupement selon \"", factesp, "\"et \"", fact, "\"\n"))
                nbObs <- tapply(obsSansExtreme[, champtrouve][obsSansExtreme[, champtrouve]!= 0], list(obsSansExtreme$statut[obsSansExtreme[, champtrouve]!= 0], obsSansExtreme[, factesp][obsSansExtreme[, champtrouve]!= 0], obsSansExtreme[, fact][obsSansExtreme[, champtrouve]!= 0]), length)
                legend("topleft", "Nombre d'enregistrement par Boxplot", cex =0.7, col="orange", text.col="orange", merge=FALSE)
                legend("topright", textstatut, col=heat.colors(length(unique(obsSansExtreme$statut))), pch = 15, cex =0.9, title="Statuts")
                axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)), col.ticks="orange", col.axis = "orange", lty = 2, lwd = 0.5)
                Moyenne <- as.vector(tapply(obsSansExtreme[, champtrouve][obsSansExtreme[, champtrouve]!= 0], list(obsSansExtreme$statut[obsSansExtreme[, champtrouve]!= 0], obsSansExtreme[, factesp][obsSansExtreme[, champtrouve]!= 0], obsSansExtreme[, fact][obsSansExtreme[, champtrouve]!= 0]), na.rm = T, mean))
                points(Moyenne, pch=19, col="blue")
                if (length(Moyenne)>150)
                {
                    legend("center", "plus de 150 possibilités de boxplot\nFaites une sélection préalable sur votre jeux de données", cex =1, col="blue", text.col="blue", merge=FALSE)
                    print("plus de 150 possibilités de boxplot\nFaites une sélection préalable dans votre jeu de données\n...")
                }else{
                    abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")
                    abline(v = 0.5+(1:length(as.vector(unique(list(obsSansExtreme[, factesp], obsSansExtreme[, fact])))))*length(as.vector(unique(obsSansExtreme$statut))), col = "red")
                }
                if (choix=="1")
                {
                    legend("top", "Enregistrements > 95% du maximum retirés", cex =0.7, col="red", text.col="red", merge=FALSE)
                }
                legend("topleft", TauxRenseignement, cex =0.8, col="red", bty="n", text.col="red", merge=FALSE)
                print(paste("graphique de distribution des enregistrements groupés selon le statut du site et", factesp , fact, " réalisé"))

                ## distribution des enregistrements groupés selon l'année de l'observation  et factesp , fact

                x11(width=120, height=50, pointsize=10)
                par(mar=c(8, 4, 5, 1), mgp=c(3, 1, 9))
                boxplot(obsSansExtreme[, champtrouve][obsSansExtreme[, champtrouve]!= 0] ~obsSansExtreme$statut[obsSansExtreme[, champtrouve]!= 0]+obsSansExtreme$an[obsSansExtreme[, champtrouve]!= 0]+obsSansExtreme[, factesp][obsSansExtreme[, champtrouve]!= 0]+obsSansExtreme[, fact][obsSansExtreme[, champtrouve]!= 0], col=heat.colors(length(unique(obsSansExtreme$statut))), las=2, main=paste(champtrouve, " par enregistrement\n selon le statut puis l'année et Regroupement selon \"", factesp, "\"et \"", fact, "\"\n"))
                nbObs <- tapply(obsSansExtreme[, champtrouve][obsSansExtreme[, champtrouve]!= 0], list(obsSansExtreme$statut[obsSansExtreme[, champtrouve]!= 0], obsSansExtreme$an[obsSansExtreme[, champtrouve]!= 0], obsSansExtreme[, factesp][obsSansExtreme[, champtrouve]!= 0], obsSansExtreme[, fact][obsSansExtreme[, champtrouve]!= 0]), length)
                legend("topleft", "Nombre d'enregistrement par Boxplot", cex =0.7, col="orange", text.col="orange", merge=FALSE)
                legend("topright", textstatut, col=heat.colors(length(unique(obsSansExtreme$statut))), pch = 15, cex =0.9, title="Statuts")
                axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)), col.ticks="orange", col.axis = "orange", lty = 2, lwd = 0.5, cex =0.7)
                Moyenne <- as.vector(tapply(obsSansExtreme[, champtrouve][obsSansExtreme[, champtrouve]!= 0], list(obsSansExtreme$statut[obsSansExtreme[, champtrouve]!= 0], obsSansExtreme$an[obsSansExtreme[, champtrouve]!= 0], obsSansExtreme[, factesp][obsSansExtreme[, champtrouve]!= 0], obsSansExtreme[, fact][obsSansExtreme[, champtrouve]!= 0]), na.rm = T, mean))
                points(Moyenne, pch=19, col="blue")
                if (length(Moyenne)>150)
                {
                    legend("center", "plus de 150 possibilités de boxplot\nFaites une sélection préalable sur votre jeux de données", cex =1, col="blue", text.col="blue", merge=FALSE)
                    print("plus de 150 possibilités de boxplot\nFaites une sélection préalable dans votre jeu de données\n...")
                }else{
                    abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")
                    abline(v = 0.5+(1:(length(as.vector(unique(obsSansExtreme$an)))*length(as.vector(unique(list(obsSansExtreme[, factesp][obsSansExtreme[, champtrouve]!= 0], obsSansExtreme[, fact][obsSansExtreme[, champtrouve]!= 0]))))))*length(as.vector(unique(obsSansExtreme$statut))), col = "red")
                }
                if (choix=="1")
                {
                    legend("top", "Enregistrements > 95% du maximum retirés", cex =0.7, col="red", text.col="red", merge=FALSE)
                }
                legend("topleft", TauxRenseignement, cex =0.8, col="red", bty="n", text.col="red", merge=FALSE)
                print(paste("graphique de distribution des enregistrements groupés selon l'année de l'observation  et", factesp , fact, " réalisé"))

                ## distribution des enregistrements groupés selon le statut, l'année de l'observation  et factesp , fact

                x11(width=120, height=50, pointsize=10)
                par(mar=c(8, 4, 5, 1), mgp=c(3, 1, 9))
                boxplot(obsSansExtreme[, champtrouve][obsSansExtreme[, champtrouve]!= 0] ~obsSansExtreme$an[obsSansExtreme[, champtrouve]!= 0]+obsSansExtreme$statut[obsSansExtreme[, champtrouve]!= 0]+obsSansExtreme[, factesp][obsSansExtreme[, champtrouve]!= 0]+obsSansExtreme[, fact][obsSansExtreme[, champtrouve]!= 0], col=heat.colors(length(unique(obsSansExtreme$statut))*length(unique(obsSansExtreme$an))), las=2, main=paste(champtrouve, " par enregistrement\n selon l'année puis le statut et Regroupement selon \"", factesp, "\"et \"", fact, "\"\n"))
                nbObs <- tapply(obsSansExtreme[, champtrouve][obsSansExtreme[, champtrouve]!= 0], list(obsSansExtreme$an[obsSansExtreme[, champtrouve]!= 0], obsSansExtreme$statut[obsSansExtreme[, champtrouve]!= 0], obsSansExtreme[, factesp][obsSansExtreme[, champtrouve]!= 0], obsSansExtreme[, fact][obsSansExtreme[, champtrouve]!= 0]), length)
                legend("topleft", "Nombre d'enregistrement par Boxplot", cex =0.7, col="orange", text.col="orange", merge=FALSE)
                legend("topright", textstatut, col=heat.colors(length(unique(obsSansExtreme$statut))), pch = 15, cex =0.9, title="Statuts")
                axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)), col.ticks="orange", col.axis = "orange", lty = 2, lwd = 0.5, cex =0.7)
                Moyenne <- as.vector(tapply(obsSansExtreme[, champtrouve][obsSansExtreme[, champtrouve]!= 0], list(obsSansExtreme$an[obsSansExtreme[, champtrouve]!= 0], obsSansExtreme$statut[obsSansExtreme[, champtrouve]!= 0], obsSansExtreme[, factesp][obsSansExtreme[, champtrouve]!= 0], obsSansExtreme[, fact][obsSansExtreme[, champtrouve]!= 0]), na.rm = T, mean))
                points(Moyenne, pch=19, col="blue")
                if (length(Moyenne)>150)
                {
                    legend("center", "plus de 150 possibilités de boxplot\nFaites une sélection préalable sur votre jeux de données", cex =1, col="blue", text.col="blue", merge=FALSE)
                    print("plus de 150 possibilités de boxplot\nFaites une sélection préalable dans votre jeu de données\n...")
                }else{
                    abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")
                    abline(v = 0.5+(1:(length(as.vector(unique(obsSansExtreme$statut)))*length(as.vector(unique(list(obsSansExtreme[, factesp][obsSansExtreme[, champtrouve]!= 0], obsSansExtreme[, fact][obsSansExtreme[, champtrouve]!= 0]))))))*length(as.vector(unique(obsSansExtreme$an))), col = "red")
                }
                if (choix=="1")
                {
                    legend("top", "Enregistrements > 95% du maximum retirés", cex =0.7, col="red", text.col="red", merge=FALSE)
                }
                legend("topleft", TauxRenseignement, cex =0.8, col="red", bty="n", text.col="red", merge=FALSE)
                print(paste("graphique de distribution des enregistrements le statut, l'année de l'observation  et", factesp, ", ", fact, " réalisé"))

                interaction.plot(obsSansExtreme[, fact], obsSansExtreme[, factesp], obsSansExtreme[, champtrouve], lwd=2, col=cl[seq(550, (550+(4*(length(split(obsSansExtreme, obsSansExtreme[, fact]))-1))), by=4)], type="b",
                                 fun=mean, trace.label = fact, xlab=factesp, ylab=champtrouve, main=paste("Interaction des valeurs de ", champtrouve, " par", fact))

            }else{             #si pas plus d'une valeur
                tkmessageBox(message=paste("Les champs", factesp, "et", fact, "contiennent toujours la même valeur :", unique(obsSansExtreme[, factesp][, fact]), "\n regroupement inefficace, sélectionnez un autre facteur"), icon="info")
                gestionMSGerreur.f("UneSeuleValeurRegroupement")
            }
        }else{          #si moins de 50% du critère renseigné
            tkmessageBox(message=paste("Graphique sans intérêt - plus de la moitié \n des enregistrements des critères de regroupement ", factesp, "et", fact, "ne sont pas renseignés (NA)"))
            gestionMSGerreur.f("CritereMalRenseigne50")
        }
    }else{
        tkmessageBox(message="Graphique impossible - pas d'enregistrements dans votre sélection")
        gestionMSGerreur.f("ZeroEnregistrement")
    }
    choix <- "0"
}


################################################################################
## Nom    : GraphBiomasseParEspece.f()
## Objet  : graphiques des Biomasses par unité d'observation sur une espèce sélectionnée
## Input  : table "listespunit" et un code espèce sp
## Output : boxplot
################################################################################

GraphBiomasseParEspece.f <- function ()
{
    print("fonction GraphBiomasseParEspece activée")
    extremes.f()      #ici on coisit si l'on veut garder les x % des valeurs extremes
    especesPresentes <- subset(unitesp$code_espece, unitesp$pres_abs==1)
    ChoixFacteurSelect.f(especesPresentes, "code_espece", "multiple", 1, "selectbiomasse")

    spunit <- subset(listespunit, listespunit$code_espece==selectbiomasse)
    spunit <- EnleverMaxExtremes.f(spunit)
    textstatut <- c("HR : Hors Réserve", "PP : Protection Partielle", "RE : En réserve")

    ## la biomasse n'est pas calculée sur tous les jeux de données  (avec données benthos? vérifier la création de listespunit)
    if (length(unique(listespunit$biomasse))>1)
    {
        ## on vérifie que la biomasse a été calculée pour l'espèce sélectionnée
        if (length(unique(spunit$biomasse))>1)
        {
            x11(width=50, height=20, pointsize=10)
            par(mar=c(12, 4, 5, 1), mgp=c(3, 1, 9))
            if (tclvalue(GraphBiomasse)=="1")
            {
                boxplot(spunit$biomasse ~spunit$code_espece, data=spunit, varwidth = TRUE, ylab=expression("Biomasse "(g/m^2)), las=3,
                        main <- paste("Biomasse\n pour un choix d'espèces\npour toutes les unités d'obs"))
            }
            if (tclvalue(GraphBiomasse)=="2")
            {
                boxplot(spunit$biomasse ~spunit$statut_protection+spunit$code_espece, data=spunit, varwidth = TRUE, ylab=expression("Biomasse "(g/m^2)), las=3, main=paste("Biomasse\n pour un choix d'espèces\n selon le statut des unités d'obs la concernant"))
                legend("topright", textstatut, col=heat.colors(length(unique(spunit$statut_protection))), pch = 15, cex =0.9, title="Statuts")
            }
            if (tclvalue(GraphBiomasse)=="3")
            {
                boxplot(spunit$biomasse ~spunit$an+spunit$code_espece, data=spunit, varwidth = TRUE,
                        ylab=expression("Biomasse "(g/m^2)), las=3,
                        main=paste("Biomasse\n pour un choix d'espèces\n selon l'année des unités d'obs la concernant"))
            }
            if (tclvalue(GraphBiomasse)=="4")
            {
                boxplot(spunit$biomasse ~spunit$statut_protection+spunit$an+spunit$code_espece, data=spunit,
                        varwidth = TRUE, ylab=expression("Biomasse "(g/m^2)), las=3, col=heat.colors(length(unique(spunit$statut_protection))),
                        main=paste("Biomasse\n pour un choix d'espèces\n selon le statut et l'année des unités d'obs la concernant"))
                legend("topright", textstatut, col=heat.colors(length(unique(spunit$statut_protection))), pch = 15, cex =0.9, title="Statuts")
            }
            if (tclvalue(GraphBiomasse)=="5")
            {
                par(mar=c(15, 4, 5, 1), mgp=c(3, 1, 9))
                boxplot(spunit$biomasse ~spunit$biotope+spunit$an+spunit$code_espece, data=spunit,
                        varwidth = TRUE, ylab=expression("Biomasse "(g/m^2)), las=3, col=heat.colors(length(unique(spunit$statut_protection))),
                        main=paste("Biomasse\n pour un choix d'espèces\n selon le biotope, l'année et le statut des unités d'obs la concernant"))
                legend("topright", textstatut, col=heat.colors(length(unique(spunit$statut_protection))), pch = 15, cex =0.9, title="Statuts")
            }
            tclvalue(GraphBiomasse)="0"
        }else{
            ## à mettre en MSGERROR
            tkmessageBox(message="Calcul de biomasse impossible - Coefficients a et b manquants dans le referentiel especes")
            gestionMSGerreur.f("ZeroEnregistrement")
        }
    }
    if (choix=="1")
    {
        legend("top", "Enregistrements > 95% du maximum retirés", cex =0.7, col="red", text.col="red", merge=FALSE)
    }
    choix <- "0"
}

################################################################################
## Nom    : GraphDensiteParUnitObs.f()
## Objet  : graphiques des Densite par unité d'observation sur une espèce sélectionnée
## Input  : table "unit"
## Output : boxplot densite
################################################################################

GraphDensiteParUnitObs.f <- function ()
{
    print("fonction GraphDensiteParUnitObs activée")
    extremes.f()
    unit$densite <- EnleverMaxExtremes.f(unit$densite)
    if (length(unique(unit$densite))>0)
    {
        x11(width=120, height=50, pointsize=10)
        if (tclvalue(GraphDensite)=="1")
        {
            boxplot(unit$densite, data=unit, varwidth = TRUE, ylab="Densité (ind/m2)", las=3,
                    main=paste("Densité globale"))
        }
        if (tclvalue(GraphDensite)=="2")
        {
            boxplot(unit$densite ~unit$an, data=unit, col=heat.colors(length(unique(unit$an))), varwidth = TRUE,
                    ylab="Densité (ind/m2)", las=3, main=paste("Densité selon l'année"))
        }
        if (tclvalue(GraphDensite)=="3")
        {
            boxplot(unit$densite ~unit$statut_protection, data=unit,
                    col=heat.colors(length(unique(unit$statut_protection))), varwidth = TRUE, ylab="Densité (ind/m2)", las=3,
                    main=paste("Densité  selon le statut"))
        }
        if (tclvalue(GraphDensite)=="4")
        {
            boxplot(unit$densite ~unit$statut_protection+unit$an, data=unit,
                    varwidth = TRUE, ylab="Densité (ind/m2)", las=3, col=heat.colors(length(unique(unit$statut_protection))),
                    main=paste("Densité par an \n selon le statut et l'année"))
        }
        if (tclvalue(GraphDensite)=="5")
        {
            boxplot(unit$densite ~unit$statut_protection+unit$an+unit$biotope, data=unit,
                    varwidth = TRUE, ylab="Densité (ind/m2)", las=3, col=heat.colors(length(unique(unit$statut_protection))),
                    main=paste("Densité  par an \n selon le statut l'année et le biotope"))
        }
    }else{
        tkmessageBox(message="Graphique impossible - pas d'enregistrements dans votre sélection")
        gestionMSGerreur.f("ZeroEnregistrement")
    }
    tclvalue(GraphDensite)="0"
    choix <- "0"
}
################################################################################
## Nom    : GraphBiomasseParUnitObs.f()
## Objet  : graphiques des Biomasse par unité d'observation sur une espèce sélectionnée
## Input  : table "unit"
## Output : boxplot biomasse
################################################################################

GraphBiomasseParUnitObs.f <- function ()
{
    print("fonction GraphBiomasseParUnitObs activée")
    extremes.f()
    unit$biomasse <- EnleverMaxExtremes.f(unit$biomasse)
    if (length(unique(unit$biomasse))>0)
    {
        x11(width=120, height=50, pointsize=10)
        if (tclvalue(GraphBiomasse)=="1")
        {
            boxplot(unit$biomasse, data=unit, varwidth = TRUE, ylab=expression("Biomasse "(g/m^2)), las=3,
                    main=paste("Biomasse (g/m²) globale"))
        }
        if (tclvalue(GraphBiomasse)=="2")
        {
            boxplot(unit$biomasse ~unit$an, data=unit, col=heat.colors(length(unique(unit$an))), varwidth = TRUE,
                    ylab=expression("Biomasse "(g/m^2)), las=3, main=paste("Biomasse (g/m²) selon l'année"))
        }
        if (tclvalue(GraphBiomasse)=="3")
        {
            boxplot(unit$biomasse ~unit$statut_protection, data=unit,
                    col=heat.colors(length(unique(unit$statut_protection))), varwidth = TRUE, ylab=expression("Biomasse "(g/m^2)), las=3,
                    main=paste("Biomasse (g/m²)  selon le statut"))
        }
        if (tclvalue(GraphBiomasse)=="4")
        {
            boxplot(unit$biomasse ~unit$statut_protection+unit$an, data=unit,
                    varwidth = TRUE, ylab="biomasse", las=3, col=heat.colors(length(unique(unit$statut_protection))),
                    main=paste("Biomasse (g/m²) par an \n selon le statut et l'année"))
        }
        if (tclvalue(GraphBiomasse)=="5")
        {
            boxplot(unit$biomasse ~unit$statut_protection+unit$an+unit$biotope, data=unit,
                    varwidth = TRUE, ylab=expression("Biomasse "(g/m^2)), las=3, col=heat.colors(length(unique(unit$statut_protection))),
                    main=paste("Biomasse (g/m²) par an \n selon le statut l'année et le biotope"))
        }
    }else{
        tkmessageBox(message="Graphique impossible - pas d'enregistrements dans votre sélection")
        gestionMSGerreur.f("ZeroEnregistrement")
    }
    tclvalue(GraphBiomasse)="0"
    choix <- "0"
}


################################################################################
## Nom    : GraphHillParUnitObs.f()  GraphPielouParUnitObs()
## Objet  : graphiques des Densite par unité d'observation sur une espèce sélectionnée
## Input  : table "unit"
## Output : boxplot densite
################################################################################

GraphHillParUnitObs.f <- function ()
{
    print("fonction GraphHillParUnitObs activée")
    extremes.f()
    unit$hill <- EnleverMaxExtremes.f(unit$hill)
    if (length(unique(unit$hill))>0)
    {
        x11(width=120, height=50, pointsize=10)
        if (tclvalue(GraphHill)=="1")
        {
            boxplot(unit$hill, data=unit, varwidth = TRUE, ylab="indice de Hill", las=3,
                    main=paste("Indice de Hill tout confondu"))
        }
        if (tclvalue(GraphHill)=="2")
        {
            boxplot(unit$hill ~unit$an, data=unit, col=heat.colors(length(unique(unit$an))), varwidth = TRUE,
                    ylab="Indice de Hill ", las=3, main=paste("Indice de Hill selon l'année"))
        }
        if (tclvalue(GraphHill)=="3")
        {
            boxplot(unit$hill ~unit$statut_protection, data=unit, col=heat.colors(length(unique(unit$statut_protection))),
                    varwidth = TRUE, ylab="Indice de Hill ", las=3, main=paste("Indice de Hill selon le statut"))
        }
        if (tclvalue(GraphHill)=="4")
        {
            boxplot(unit$hill ~unit$statut_protection+unit$an, data=unit,
                    varwidth = TRUE, ylab="Indice de Hill ", las=3, col=heat.colors(length(unique(unit$statut_protection))),
                    main=paste("Indice de Hill par an \n selon le statut et l'année"))
        }
    }else{
        tkmessageBox(message="Graphique impossible - pas d'enregistrements dans votre sélection")
        gestionMSGerreur.f("ZeroEnregistrement")
    }
    tclvalue(GraphHill)="0"
    choix <- "0"
}

GraphPielouParUnitObs.f <- function ()
{
    extremes.f()
    print("fonction GraphPielouParUnitObs activée")
    unit$pielou <- EnleverMaxExtremes.f(unit$pielou)
    if (length(unique(unit$hill))>0)
    {
        x11(width=120, height=50, pointsize=10)
        if (tclvalue(GraphPielou)=="1")
        {
            boxplot(unit$pielou, data=unit, varwidth = TRUE, ylab="indice de Pielou", las=3,
                    main=paste("Indice de Pielou tout confondu"))
        }
        if (tclvalue(GraphPielou)=="2")
        {
            boxplot(unit$pielou ~unit$an, data=unit, col=heat.colors(length(unique(unit$an))), varwidth = TRUE,
                    ylab="Indice de Pielou ", las=3, main=paste("Indice de Pielou selon l'année"))
        }
        if (tclvalue(GraphPielou)=="3")
        {
            boxplot(unit$pielou ~unit$statut_protection, data=unit,
                    col=heat.colors(length(unique(unit$statut_protection))), varwidth = TRUE, ylab="Indice de Pielou ", las=3,
                    main=paste("Indice de Pielou selon le statut"))
        }
        if (tclvalue(GraphPielou)=="4")
        {
            boxplot(unit$pielou ~unit$statut_protection+unit$an, data=unit,
                    varwidth = TRUE, ylab="Indice de Pielou ", las=3, col=heat.colors(length(unique(unit$statut_protection))),
                    main=paste("Indice de Pielou par an \n selon le statut et l'année"))
        }
    }else{
        tkmessageBox(message="Graphique impossible - pas d'enregistrements dans votre sélection")
        gestionMSGerreur.f("ZeroEnregistrement")
    }
    tclvalue(GraphPielou)="0"
    choix <- "0"
}

GraphL.simpsonParUnitObs.f <- function ()
{

    print("fonction GraphL.simpsonParUnitObs activée")
    extremes.f()
    unit$l.simpson <- EnleverMaxExtremes.f(unit$l.simpson)
    if (length(unique(unit$hill))>0)
    {
        x11(width=120, height=50, pointsize=10)
        if (tclvalue(GraphL.simpson)=="1")
        {
            boxplot(unit$l.simpson , data=unit, varwidth = TRUE, ylab="indice de L.simpson", las=3,
                    main=paste("Indice de L.simpson tout confondu"))
        }
        if (tclvalue(GraphL.simpson)=="2")
        {
            boxplot(unit$l.simpson  ~unit$an, data=unit, col=heat.colors(length(unique(unit$an))), varwidth = TRUE,
                    ylab="Indice de L.simpson ", las=3, main=paste("Indice de L.simpson selon l'année"))
        }
        if (tclvalue(GraphL.simpson)=="3")
        {
            boxplot(unit$l.simpson  ~unit$statut_protection, data=unit,
                    col=heat.colors(length(unique(unit$statut_protection))), varwidth = TRUE, ylab="Indice de L.simpson ", las=3,
                    main=paste("Indice de L.simpson selon le statut"))
        }
        if (tclvalue(GraphL.simpson)=="4")
        {
            boxplot(unit$l.simpson  ~unit$statut_protection+unit$an, data=unit,
                    varwidth = TRUE, ylab="Indice de L.simpson ", las=3, col=heat.colors(length(unique(unit$statut_protection))),
                    main=paste("Indice de L.simpson par an \n selon le statut et l'année"))
        }
    }else{
        tkmessageBox(message="Graphique impossible - pas d'enregistrements dans votre sélection")
        gestionMSGerreur.f("ZeroEnregistrement")
    }
    tclvalue(GraphL.simpson)="0"
    choix <- "0"
}

GraphSimpsonParUnitObs.f <- function ()
{
    extremes.f()
    unit$simpson <- EnleverMaxExtremes.f(unit$simpson)
    print("fonction GraphSimpsonParUnitObs activée")
    if (length(unique(unit$hill))>0)
    {
        x11(width=120, height=50, pointsize=10)
        if (tclvalue(GraphSimpson)=="1")
        {
            boxplot(unit$simpson , data=unit, varwidth = TRUE, ylab="indice de Simpson", las=3,
                    main=paste("Indice de Simpson tout confondu"))
        }
        if (tclvalue(GraphSimpson)=="2")
        {
            boxplot(unit$simpson  ~unit$an, data=unit, col=heat.colors(length(unique(unit$an))), varwidth = TRUE,
                    ylab="Indice de Simpson ", las=3, main=paste("Indice de Simpson selon l'année"))
        }
        if (tclvalue(GraphSimpson)=="3")
        {
            boxplot(unit$simpson  ~unit$statut_protection, data=unit,
                    col=heat.colors(length(unique(unit$statut_protection))), varwidth = TRUE, ylab="Indice de Simpson ", las=3,
                    main=paste("Indice de Simpson selon le statut"))
        }
        if (tclvalue(GraphSimpson)=="4")
        {
            boxplot(unit$simpson  ~unit$statut_protection+unit$an, data=unit,
                    varwidth = TRUE, ylab="Indice de Simpson ", las=3, col=heat.colors(length(unique(unit$statut_protection))),
                    main=paste("Indice de Simpson par an \n selon le statut et l'année"))
        }
    }else{
        tkmessageBox(message="Graphique impossible - pas d'enregistrements dans votre sélection")
        gestionMSGerreur.f("ZeroEnregistrement")
    }
    tclvalue(GraphSimpson)="0"
    choix <- "0"
}


GraphRichesse_specifiqueParUnitObs.f <- function ()
{
    print("fonction GraphRichesse_specifiqueParUnitObs activée")
    extremes.f()
    spunit <- unit
    spunit$richesse_specifique <- EnleverMaxExtremes.f(spunit$richesse_specifique)
    if (length(unique(spunit$richesse_specifique))>0)
    {
        x11(width=120, height=50, pointsize=10)
        if (tclvalue(GraphRichesse_specifique)=="1")
        {
            boxplot(spunit$richesse_specifique , data=spunit, varwidth = TRUE, ylab="indice de Richesse_specifique", las=3,
                    main=paste("Indice de Richesse_specifique tout confondu"))
        }
        if (tclvalue(GraphRichesse_specifique)=="2")
        {
            boxplot(spunit$richesse_specifique  ~spunit$an, data=spunit, col=heat.colors(length(unique(unit$an))),
                    varwidth = TRUE, ylab="Indice de Richesse_specifique ", las=3,
                    main=paste("Indice de Richesse_specifique selon l'année"))
        }
        if (tclvalue(GraphRichesse_specifique)=="3")
        {
            boxplot(spunit$richesse_specifique  ~spunit$statut_protection, data=spunit,
                    col=heat.colors(length(unique(spunit$statut_protection))), varwidth = TRUE,
                    ylab="Indice de Richesse_specifique ", las=3, main=paste("Indice de Richesse_specifique selon le statut"))
        }
        if (tclvalue(GraphRichesse_specifique)=="4")
        {
            boxplot(spunit$richesse_specifique  ~spunit$statut_protection+spunit$an, data=spunit,
                    varwidth = TRUE, ylab="Indice de Richesse_specifique ", las=3, col=heat.colors(length(unique(unit$statut_protection))),
                    main=paste("Indice de Richesse_specifique par an \n selon le statut et l'année"))
        }
    }else{
        tkmessageBox(message="Graphique impossible - pas d'enregistrements dans votre sélection")
        gestionMSGerreur.f("ZeroEnregistrement")
    }
    tclvalue(GraphRichesse_specifique)="0"
    choix <- "0"
}

################################################################################
## Nom    : GraphDensiteParEspece.f()
## Objet  : graphiques des Densités sur une espèce sélectionnée
## Input  : table "listespunit" et un code espèce sp
## Output : boxplot Densités
################################################################################

GraphDensiteParEspece.f <- function ()
{

    extremes.f()
    print("fonction GraphDensiteParEspece activée")

    especesPresentes <- subset(unitesp$code_espece, unitesp$pres_abs==1)
    ChoixFacteurSelect.f(especesPresentes, "code_espece", "multiple", 1, "selectdensite")
    spunit <- subset(listespunit, listespunit$code_espece==selectdensite)
    spunit$densite <- EnleverMaxExtremes.f(spunit$densite)
    textstatut <- c("HR : Hors Réserve", "PP : Protection Partielle", "RE : En réserve")
    ## la densite n'est pas calculée sur tous les jeux de données  (avec données benthos? vérifier la création de listespunit)
    if (length(unique(listespunit$densite))>1)
    {
        ## on vérifie que la densite a été calculée pour l'espèce sélectionnée
        if (length(unique(spunit$densite))>0)
        {
            x11(width=50, height=20, pointsize=10)
            par(mar=c(12, 4, 5, 1), mgp=c(3, 1, 9))
            if (tclvalue(GraphDensiteEsp)=="1")
            {
                boxplot(spunit$densite ~spunit$code_espece, data=spunit, varwidth = TRUE, ylab="Densité (%)", las=3,
                        main=paste("Densité\n pour un choix d'espèces\npour toutes les unités d'obs"))
            }
            if (tclvalue(GraphDensiteEsp)=="2")
            {
                boxplot(spunit$densite ~spunit$statut_protection+spunit$code_espece, data=spunit, varwidth = TRUE, ylab="Densité (%)", las=3, main=paste("Densité\n pour un choix d'espèces\n selon le statut des unités d'obs la concernant"))
                legend("topright", textstatut, col=heat.colors(length(unique(spunit$statut_protection))), pch = 15, cex =0.9, title="Statuts")
            }
            if (tclvalue(GraphDensiteEsp)=="3")
            {
                boxplot(spunit$densite ~spunit$an+spunit$code_espece, data=spunit, varwidth = TRUE, ylab="Densité (%)", las=3,
                        main=paste("Densité\n pour un choix d'espèces\n selon l'année des unités d'obs la concernant"))
            }
            if (tclvalue(GraphDensiteEsp)=="4")
            {
                boxplot(spunit$densite ~spunit$statut_protection+spunit$an+spunit$code_espece, data=spunit,
                        varwidth = TRUE, ylab="Densité (%)", las=3, col=heat.colors(length(unique(spunit$statut_protection))),
                        main=paste("Densité\n pour un choix d'espèces\n selon le statut et l'année des unités d'obs la concernant"))
                legend("topright", textstatut, col=heat.colors(length(unique(spunit$statut_protection))), pch = 15, cex =0.9, title="Statuts")
            }
            if (tclvalue(GraphDensiteEsp)=="5")
            {
                par(mar=c(15, 4, 5, 1), mgp=c(3, 1, 9))
                boxplot(spunit$densite ~spunit$biotope+spunit$an+spunit$code_espece, data=spunit,
                        varwidth = TRUE, ylab="Densité (%)", las=3, col=heat.colors(length(unique(spunit$statut_protection))),
                        main=paste("Densité\n pour un choix d'espèces\n selon le biotope, l'année et le statut des unités d'obs la concernant"))
                legend("topright", textstatut, col=heat.colors(length(unique(spunit$statut_protection))), pch = 15, cex =0.9, title="Statuts")
            }
            tclvalue(GraphDensiteEsp)="0"
        }else{
            ## à mettre en MSGERROR
            tkmessageBox(message="Graphique impossible - pas d'enregistrements dans votre sélection")
            gestionMSGerreur.f("ZeroEnregistrement")
        }
    }
    if (choix=="1")
    {
        legend("top", "Enregistrements > 95% du maximum retirés", cex =0.7, col="red", text.col="red", merge=FALSE)
    }
    choix <- "0"
}
#################################################


GraphDensiteParFamille.f <- function ()
{
    print("fonction GraphDensiteParFamille activée")
    ChoixUneFamille.f()
    extremes.f()
    ## on restreint la table "listespunit" à l'espèce sélectionnée
    listespunit$Famille <- especes$Famille[match(listespunit$code_espece, especes$code_espece)]
    spunit <- subset(listespunit, listespunit$Famille==fa)
    spunit$densite <- EnleverMaxExtremes.f(spunit$densite)
    if (length(unique(spunit$densite))>0)
    {
        x11(width=120, height=50, pointsize=10)
        if (tclvalue(GraphDensiteFam)=="1")
        {
            boxplot(spunit$densite, data=spunit, varwidth = TRUE, ylab="Densité", las=3,
                    main=paste("Densité\n pour la famille ", fa, "\npour toutes les unités d'obs"))
        }
        if (tclvalue(GraphDensiteFam)=="2")
        {
            boxplot(spunit$densite ~spunit$statut_protection, data=spunit, varwidth = TRUE, ylab="Densité", las=3,
                    main=paste("Densité\n pour la famille ", fa, "\n selon le statut des unités d'obs"))
        }
        if (tclvalue(GraphDensiteFam)=="3")
        {
            boxplot(spunit$densite ~spunit$an, data=spunit, col=heat.colors(length(unique(spunit$an))), varwidth = TRUE,
                    ylab="Densité", las=3, main=paste("Densité\n la famille ", fa, "\n selon l'année"))
        }
        if (tclvalue(GraphDensiteFam)=="4")
        {
            boxplot(spunit$densite ~spunit$statut_protection+spunit$an, data=spunit,
                    varwidth = TRUE, ylab="Densité", las=3, col=heat.colors(length(unique(spunit$statut_protection))),
                    main=paste("Densité\n par an pour la famille ", fa, "\n selon le statut des unités d'obs"))}
    }
    if (tclvalue(GraphDensiteFam)=="5")
    {
        par(mar=c(15, 4, 5, 1), mgp=c(3, 1, 9))
        boxplot(spunit$densite ~spunit$biotope+spunit$an+spunit$statut_protection, data=spunit,
                varwidth = TRUE, ylab="Densité (%)", las=3, col=heat.colors(length(unique(spunit$statut_protection))),
                main=paste("Densité\n pour un choix d'espèces\n selon le biotope, l'année et le statut des unités d'obs la concernant"))
        legend("topright", textstatut, col=heat.colors(length(unique(spunit$statut_protection))), pch = 15, cex =0.9, title="Statuts")
    }else{
        tkmessageBox(message="Graphique impossible - pas d'enregistrements dans votre sélection")
        gestionMSGerreur.f("ZeroEnregistrement")
    }
    tclvalue(GraphDensiteFam)="0"
    choix <- "0"
}

#################################################
GraphBiomasseParFamille.f <- function ()
{
    print("fonction GraphBiomasseParFamille activée")
    ChoixUneFamille.f()
    extremes.f()
    ## on restreint la table "listespunit" à l'espèce sélectionnée
    listespunit$Famille <- especes$Famille[match(listespunit$code_espece, especes$code_espece)]
    spunit <- subset(listespunit, listespunit$Famille==fa)
    spunit$biomasse <- EnleverMaxExtremes.f(spunit$biomasse)
    if (length(unique(spunit$biomasse))>0)
    {
        x11(width=120, height=50, pointsize=10)
        if (tclvalue(GraphBiomasse)=="1")
        {
            boxplot(spunit$biomasse, data=spunit, varwidth = TRUE, ylab=expression("Biomasse "(g/m^2)), las=3,
                    main=paste("Biomasse (g/m²)\n pour la famille ", fa, "\npour toutes les unités d'obs"))
        }
        if (tclvalue(GraphBiomasse)=="2")
        {
            boxplot(spunit$biomasse ~spunit$statut_protection, data=spunit, varwidth = TRUE, ylab=expression("Biomasse "(g/m^2)), las=3,
                    main=paste("Biomasse (g/m²)\n pour la famille ", fa, "\n selon le statut des unités d'obs"))
        }
        if (tclvalue(GraphBiomasse)=="3")
        {
            boxplot(spunit$biomasse ~spunit$an, data=spunit, col=heat.colors(length(unique(spunit$an))), varwidth = TRUE,
                    ylab=expression("Biomasse "(g/m^2)), las=3, main=paste("Biomasse (g/m²)\n pour la famille ", fa, "\n selon l'année"))
        }
        if (tclvalue(GraphBiomasse)=="4")
        {
            boxplot(spunit$biomasse ~spunit$statut_protection+spunit$an, data=spunit,
                    varwidth = TRUE, ylab=expression("Biomasse "(g/m^2)), las=3, col=heat.colors(length(unique(spunit$statut_protection))),
                    main=paste("Biomasse (g/m²)\n par an pour la famille ", fa, "\n selon le statut des unités d'obs"))
        }
    }
    if (tclvalue(GraphBiomasse)=="5")
    {
        par(mar=c(15, 4, 5, 1), mgp=c(3, 1, 9))
        boxplot(spunit$biomasse ~spunit$biotope+spunit$an+spunit$statut_protection, data=spunit,
                varwidth = TRUE, ylab=expression("Biomasse "(g/m^2)), las=3, col=heat.colors(length(unique(spunit$statut_protection))),
                main=paste("Biomasse (g/m²)\n pour un choix d'espèces\n selon le biotope, l'année et le statut des unités d'obs la concernant"))
        legend("topright", textstatut, col=heat.colors(length(unique(spunit$statut_protection))), pch = 15, cex =0.9, title="Statuts")
    }else{
        tkmessageBox(message="Graphique impossible - pas d'enregistrements dans votre sélection")
        gestionMSGerreur.f("ZeroEnregistrement")
    }
    tclvalue(GraphBiomasse)="0"
    choix <- "0"
}

################################################################################
## Nom    : graphNbreParEsp.f()
## Objet  : graphiques par unité d'observation sur une espèce sélectionnée
## Input  : table "listespunit" et un code espèce sp
## Output : boxplot densité et biomasse
################################################################################
## !!!!!!!!!!!!!!!!!!!    spunit$densite <- EnleverMaxExtremes.f(spunit$densite) a décliner sur deux facteurs
graphNbreParEsp.f <- function ()
{
    print("fonction graphNbreParEsp activée")
    ChoixUneEspece.f()
    ## on restreint la table "listespunit" à l'espèce sélectionnée
    extremes.f()
    spunit <- subset(listespunit, listespunit$code_espece==sp)
    X11()
    boxplot(spunit$densite ~ spunit$code_espece, data=spunit, varwidth = TRUE, ylab=expression("Densite "(individus/m^2)), main=paste("Densite de ", sp, sep=""), las=3)
    ## la biomasse n'est pas calculée sur tous les jeux de données
    if (length(unique(listespunit$biomasse))>1)
    {
        ## on vérifie que la biomasse a été calculée pour l'espèce sélectionnée
        if (length(unique(spunit$biomasse))>1)
        {
            X11()
            boxplot(spunit$biomasse ~ spunit$code_espece, col=heat.colors(length(unique(spunit$code_espece))), data=spunit, varwidth = TRUE, ylab=expression("Biomasse "(g/m^2)), main=paste("Biomasse de ", sp, sep=""), las=3)
        }else{
            tkmessageBox(message="Calcul de biomasse impossible - Coefficients a et b manquants dans le referentiel especes")
        }
    }
    choix <- "0"
}

################################################################################
## Nom    : Graphiquetraitementsimple.f()
## Objet  : graphiques pour chaque espèce
## Input  : aucun
## Output : Graphiques générés
################################################################################

## [sup] [yr: 11/01/2011]

##################Graphiques concernant les nombres d'observation OU le recouvrement################################
## GraphNbObsParEspece.f <- function ()
## {
##     ## A finir et mettre aux couleurs

##     print("fonction GraphNbObsParEspece activée")
##     extremes.f()      #ici on coisit si l'on veut garder les x % des valeurs extremes
##     NbObsParEsp <- tapply(unitesp$nombre, list(unitesp$unite_observation, unitesp$code_espece), sum, na.rm=TRUE)
##     NbObsParEsp <- EnleverMaxExtremes.f(NbObsParEsp)    #  selon choix retourné par extremes.f()
##     col1 <- rgb(1, 0, 0, 0.5)

##     x11(width=120, height=50, pointsize=10)
##     boxplot(as.data.frame(NbObsParEsp), col = row(as.matrix(NbObsParEsp)), las=2, main="Nombre d'observations par espèce pour toutes les unités d'obs confondues\n")
##     nbObs <- NbObsParEsp
##     nbObs[nbObs!=0] <- 1
##     nbObs <- apply(nbObs, 2, sum, na.rm=T)
##     axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)), col.ticks="orange", col.axis = "orange", lty = 2, lwd = 0.5)
##     legend("topleft", "Nombre d'enregistrement par Boxplot", cex =0.7, col="orange", text.col="orange", merge=FALSE)
##     Moyenne <- apply(NbObsParEsp, 2, mean, na.rm=T)
##     points(Moyenne, pch=19, col="blue")
##     abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")
##     if (choix=="1")
##     {
##         legend("top", "Enregistrements > 95% du maximum retirés", cex =0.7, col="red", text.col="red", merge=FALSE)
##     }

##     SumObsParEsp <- apply(NbObsParEsp, 2, sum, na.rm=T)
##     ordreSumObsParEsp <- order(SumObsParEsp, decreasing=T)

##     ## x11(width=100, height=50, pointsize=10)
##     ## emplacement <- barplot(Moyenne)[, 1]
##     ## Sd = as.vector(tapply(obs$nombre, list(obs$statut, obs$an), na.rm = T, sd))
##     ## Rbarplot <- barplot(Moyenne, cex.lab=1.2, col=heat.colors(length(unique(obs$statut))), main=paste("Moyenne du nombre d'observations par enregistrement\n selon le statut et l'année\n\n"))
##     ## legend("topleft", "Nombre d'enregistrement par Barre", cex =0.7, col="orange", text.col="orange", merge=FALSE)
##     ## legend("topright", textstatut, col=heat.colors(length(unique(obs$statut))), pch = 15, cex =0.9, title="Statuts")
##     ## axis(3, as.vector(nbObs), at=emplacement, col.ticks="orange", col.axis = "orange", lty = 2, lwd = 0.5, cex =0.7)
##     ## malegende <- unique(paste(obs$an, obs$statut))
##     ## axis(1, malegende[order(malegende)], at=emplacement, col.ticks="black", cex =0.7, las=2)
##     ## abline(v = emplacement+0.7 , col = "lightgray", lty = "dotted")
##     ## arrows(Rbarplot, Moyenne - Sd, Rbarplot, Moyenne + Sd, code = 3, col = "purple", angle = 90, length = .1)


##     x11(width=120, height=50, pointsize=10)
##     ## Moyenne = apply(NbObsParEsp, 2, mean, na.rm=T)
##     ## Sd = apply(NbObsParEsp, 2, sd, na.rm=T)
##     ## emplacement <- barplot(Moyenne)[, 1]
##     ## Rbarplot <-
##     barplot(SumObsParEsp[ordreSumObsParEsp], col = row(as.matrix(SumObsParEsp[ordreSumObsParEsp])), las=2, main="Nombre d'observations par espèce pour toutes les unités d'obs confondues")

##     if (choix=="1")
##     {
##         legend("top", "Enregistrements > 95% du maximum retirés", cex =0.7, col="red", text.col="red", merge=FALSE)
##     }
##     x11(width=120, height=50, pointsize=10)
##     barplot(SumObsParEsp[ordreSumObsParEsp][0:20], col = row(as.matrix(SumObsParEsp[ordreSumObsParEsp])), las=2, main="Nombre d'observations par espèce pour les 20 espèces les + répendues \n pour toutes les unités d'obs confondues")
##     if (choix=="1")
##     {
##         legend("top", "Enregistrements > 95% du maximum retirés", cex =0.7, col="red", text.col="red", merge=FALSE)
##     }


##     x11(width=120, height=50, pointsize=10)
##     boxplot(as.data.frame(NbObsParEsp[, ordreSumObsParEsp][, 0:20]), col = row(as.matrix(NbObsParEsp[, ordreSumObsParEsp][, 0:20])), las=2, main="Nombre d'observations par espèce \n pour les 20 espèces les plus présentes \n\n  ")
##     nbObs <- NbObsParEsp[, ordreSumObsParEsp][, 0:20]
##     nbObs[nbObs!=0] <- 1
##     nbObs <- apply(nbObs, 2, sum, na.rm=T)
##     axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)), col.ticks="orange", col.axis = "orange", lty = 2, lwd = 0.5)
##     legend("topleft", "Nombre d'enregistrement par Boxplot", cex =0.7, col="orange", text.col="orange", merge=FALSE)
##     Moyenne <- apply(NbObsParEsp[, ordreSumObsParEsp][, 0:20], 2, mean, na.rm=T)
##     points(Moyenne, pch=19, col="blue")
##     abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")
##     if (choix=="1")
##     {
##         legend("top", "Enregistrements > 95% du maximum retirés", cex =0.7, col="red", text.col="red", merge=FALSE)
##     }

##     ## Mean, sd et ordre à intégrer aux graphiques
##     MeanObsParEsp <- apply(NbObsParEsp, 2, mean, na.rm=T)
##     SdMeanObsParEsp <- apply(NbObsParEsp, 2, mean, na.rm=T)
##     ordreMeanObsParEsp <- order(MeanObsParEsp, decreasing=T)

##     x11(width=120, height=50, pointsize=10)
##     if (unique(unitobs$type) != "LIT")
##     {
##         barplot(SumObsParEsp[ordreSumObsParEsp], col = row(as.matrix(SumObsParEsp)), cex.lab=1.2, las=2, main="Nombre d'observations totales par espèce pour toutes les unités d'obs confondues")
##         if (choix=="1")
##         {
##             legend("top", "Enregistrements > 95% du maximum retirés", cex =0.7, col="red", text.col="red", merge=FALSE)
##         }
##     }else{
##         barplot(SumObsParEsp[ordreSumObsParEsp], col = row(as.matrix(SumObsParEsp)), cex.lab=1.2, las=2, main="Recouvrement par espèce pour toutes les unités d'obs confondues")
##     }
##     if (choix=="1")
##     {
##         legend("top", "Enregistrements > 95% du maximum retirés", cex =0.7, col="red", text.col="red", merge=FALSE)
##     }
##     x11(width=70, height=50, pointsize=10)
##     par(mfrow=c(2, (1+length(unique(unitesp$statut_protection)))/2))
##     ordreSumObsParEsp <- order(SumObsParEsp, decreasing=T)
##     pie(SumObsParEsp[ordreSumObsParEsp][0:20], col = row(as.matrix(SumObsParEsp)), main="part des observations pour les 20 espèces les + répendues \npour toutes les unités d'obs confondues")
##     if (choix=="1")
##     {
##         legend("top", "Enregistrements > 95% du maximum retirés", cex =0.7, col="red", text.col="red", merge=FALSE)
##     }
##     for (i in 1:length(unique(unitesp$statut_protection)))
##     {
##         SelectSelonStatut <- subset(unitesp, unitesp$statut_protection==unique(unitesp$statut_protection)[i])
##         if (nrow(SelectSelonStatut)!=0)
##         {
##             NbObsParEsp <- tapply(SelectSelonStatut$nombre, list(SelectSelonStatut$unite_observation, SelectSelonStatut$code_espece), sum, na.rm=TRUE)
##             SumObsParEsp <- apply(NbObsParEsp, 2, sum, na.rm=T)
##             ordreSumObsParEsp <- order(SumObsParEsp, decreasing=T)
##             pie(SumObsParEsp[ordreSumObsParEsp][0:20], col = row(as.matrix(order(SumObsParEsp, decreasing=T))), main=paste("Part des observations pour les 20 espèces \npour toutes les unités d'obs du statut", unique(unitesp$statut_protection)[i]))
##             if (choix=="1")
##             {
##                 legend("top", "Enregistrements > 95% du maximum retirés", cex =0.7, col="red", text.col="red", merge=FALSE)
##             }
##         }
##     }
##     choix <- "0"
## }

GraphRecouvrementPourUneEspece.f <- function ()
{
    print("fonction GraphRecouvrementPourUneEspece activée")
    ## il faudrait mettre des couleurs #FONCTION OK mais peu générique
    ChoixUneEspece.f()
    extremes.f()
    print(paste("espece considérée :", sp))
    ## on restreint la table "listespunit" à l'espèce sélectionnée
    spunit <- subset(listespunit, listespunit$code_espece==sp)
    spunit$recouvrement <- EnleverMaxExtremes.f(spunit$recouvrement)
    if (length(unique(spunit$recouvrement))>0)
    {
        boxplot(spunit$recouvrement, main=paste("Recouvrement\n pour l'espèce", sp, "\npour toutes les unités d'obs la concernant"))
        nbObs <- tapply(spunit$recouvrement, length)
        axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)))
    }else{
        tkmessageBox(message="Graphique impossible - pas d'enregistrements dans votre sélection")
        gestionMSGerreur.f("ZeroEnregistrement")
    }
    choix <- "0"
}

## [sup] [yr: 11/01/2011]

## GraphNbObsPourUneEspece.f <- function ()
## {
##     print("fonction GraphNbObsPourUneEspece activée")
##     ## il faudrait mettre des couleurs #FONCTION OK mais peu générique
##     ChoixUneEspece.f()
##     extremes.f()
##     ## on restreint la table "listespunit" à l'espèce sélectionnée
##     spunit <- subset(listespunit, listespunit$code_espece==sp)
##     spunit$nombre <- EnleverMaxExtremes.f(spunit$nombre)
##     if (length(unique(spunit$nombre))>0)
##     {
##         par(mfrow=c(2, 2))
##         x11(width=120, height=50, pointsize=10)
##         boxplot(spunit$nombre, main=paste("Nombre d'observations\n pour l'espèce", sp, "\npour toutes les unités d'obs la concernant"))
##         nbObs <- length(spunit$nombre)
##         axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)), col.ticks="orange", col.axis = "orange", lty = 2, lwd = 0.5)

##         x11(width=120, height=50, pointsize=10)
##         boxplot(spunit$nombre ~spunit$statut_protection, main=paste("Nombre d'observations\n pour l'espèce", sp, "\n selon le statut des unités d'obs la concernant"))
##         nbObs <- tapply(spunit$nombre, spunit$statut_protection, length)
##         axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)))

##         x11(width=120, height=50, pointsize=10)
##         boxplot(spunit$nombre ~spunit$an, main=paste("Nombre d'observations\n pour l'espèce", sp, "\n selon le statut des unités d'obs la concernant"))
##         nbObs <- tapply(spunit$nombre, spunit$an, length)
##         axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)))

##         x11(width=120, height=50, pointsize=10)
##         boxplot(spunit$nombre ~spunit$statut_protection+spunit$an, col=heat.colors(spunit$an), main=paste("Nombre d'observations\n par an pour l'espèce", sp, "\n selon le statut des unités d'obs la concernant"))
##         nbObs <- tapply(spunit$nombre, list(spunit$statut_protection, spunit$an), length)
##         axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)))
##     }else{
##         tkmessageBox(message="Graphique impossible - pas d'enregistrements dans votre sélection")
##         gestionMSGerreur.f("ZeroEnregistrement")
##     }
##     choix <- "0"
## }

GraphNbEspeceParUnitobs.f <- function ()
{
    print("fonction GraphNbEspeceParUnitobs activée")
    ChoixUneUnitobs.f()
    extremes.f()
    print(paste("site considéré :", si))
    ## on restreint la table "listespunit" à l'unite d'observation sélectionnée
    spunit <- subset(listespunit, listespunit$site==si)
    spunit$nombre <- EnleverMaxExtremes.f(spunit$nombre)
    if (length(unique(spunit$nombre))>0)
    {
        x11(width=50, height=20, pointsize=10)
        par(mar=c(9, 4, 5, 1), mgp=c(3, 1, 9))
        boxplot(spunit$nombre ~spunit$an+spunit$code_espece, las=2, cex.names=0.6, main=paste("Nombre d'individus par espèce et par an pour le site ", si, "\n"))
        nbObs <- tapply(spunit$nombre, list(spunit$an, spunit$code_espece), length)
        axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)), col.ticks="orange", col.axis = "orange", lty = 2, lwd = 0.5)
        legend("topleft", "Nombre d'enregistrement par Boxplot", cex =0.7, col="orange", text.col="orange", merge=FALSE)
        Moyenne <- as.vector(tapply(spunit$nombre, list(spunit$an, spunit$code_espece), na.rm = T, mean))
        points(Moyenne, pch=19, col="blue")
        text(Moyenne+(Moyenne/10), col = "blue", cex = 0.8, labels=as.character(round(Moyenne, digits=1)))
        abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")
    }else{
        tkmessageBox(message="Graphique impossible - pas d'enregistrements dans votre sélection")
        gestionMSGerreur.f("ZeroEnregistrement")
    }
    choix <- "0"
}

GraphNbEspeceParHabitat.f <- function ()
{
    print("fonction ChoixUnhabitat1 activée")
    ChoixUnhabitat1.f()
    print(paste("habitat considéré :", ha))
    extremes.f()
    ## Il faut rajouter la colonne habitat1 à listespunit
    listespunit$habitat1 <- unitobs$habitat1[match(listespunit$unite_observation, unitobs$unite_observation)]
    ## on restreint la table "listespunit" à l'habitat sélectionnée
    spunit <- subset(listespunit, listespunit$habitat1==ha)
    spunit$nombre <- EnleverMaxExtremes.f(spunit$nombre)
    if (length(unique(spunit$habitat1))>0)
    {

        x11(width=50, height=20, pointsize=10)
        par(mar=c(9, 4, 5, 1), mgp=c(3, 1, 9))
        boxplot(spunit$nombre ~spunit$an+spunit$code_espece, las=2, cex.names=0.6, main=paste("Nombre d'individus par espèce et par an pour l'habitat ", ha, "\n"))
        nbObs <- tapply(spunit$nombre, list(spunit$an, spunit$code_espece), length)
        axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)), col.ticks="orange", col.axis = "orange", lty = 2, lwd = 0.5)
        legend("topleft", "Nombre d'enregistrement par Boxplot", cex =0.7, col="orange", text.col="orange", merge=FALSE)
        Moyenne <- as.vector(tapply(spunit$nombre, list(spunit$an, spunit$code_espece), na.rm = T, mean))
        points(Moyenne, pch=19, col="blue")
        text(Moyenne+(Moyenne/10), col = "blue", cex = 0.8, labels=as.character(round(Moyenne, digits=1)))
        abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")

        x11(width=50, height=20, pointsize=10)
        par(mar=c(9, 4, 5, 1), mgp=c(3, 1, 9))
        boxplot(spunit$nombre ~spunit$site+spunit$code_espece, las=2, cex.names=0.6, main=paste("Nombre d'individus par espèce et par site pour l'habitat ", ha, "\n"))
        nbObs <- tapply(spunit$nombre, list(spunit$site, spunit$code_espece), length)
        axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)), col.ticks="orange", col.axis = "orange", lty = 2, lwd = 0.5)
        legend("topleft", "Nombre d'enregistrement par Boxplot", cex =0.7, col="orange", text.col="orange", merge=FALSE)
        Moyenne <- as.vector(tapply(spunit$nombre, list(spunit$site, spunit$code_espece), na.rm = T, mean))
        points(Moyenne, pch=19, col="blue")
        text(Moyenne+(Moyenne/10), col = "blue", cex = 0.8, labels=as.character(round(Moyenne, digits=1)))
        abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")

        textstatut <- c("HR : Hors Réserve", "PP : Protection Partielle", "RE : En réserve")

        x11(width=50, height=20, pointsize=10)
        par(mar=c(9, 4, 5, 1), mgp=c(3, 1, 9))
        boxplot(spunit$nombre ~spunit$statut_protection+spunit$code_espece, las=2, cex.names=0.6, col=heat.colors(length(unique(spunit$statut_protection))), main=paste("Nombre d'individus par espèce et par statut de protection de site pour l'habitat ", ha, "\n"))
        nbObs <- tapply(spunit$nombre, list(spunit$statut_protection, spunit$code_espece), length)
        axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)), col.ticks="orange", col.axis = "orange", lty = 2, lwd = 0.5)
        legend("topleft", "Nombre d'enregistrement par Boxplot", cex =0.7, col="orange", text.col="orange", merge=FALSE)
        Moyenne <- as.vector(tapply(spunit$nombre, list(spunit$statut_protection, spunit$code_espece), na.rm = T, mean))
        points(Moyenne, pch=19, col="blue")
        text(Moyenne+(Moyenne/10), col = "blue", cex = 0.8, labels=as.character(round(Moyenne, digits=1)))
        abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")
        abline(v = 0.5+(1:length(as.vector(unique(spunit$code_espece))))*length(as.vector(unique(spunit$statut_protection))), col = "red")
        legend("topright", textstatut, col=heat.colors(length(unique(spunit$statut_protection))), pch = 15, cex =0.9, title="Statuts")

        ## lorsqu'il y aura beaucoup de données, rajouter habitat/an/statut

    }else{
        tkmessageBox(message="Graphique impossible - pas d'enregistrements dans votre sélection")
        gestionMSGerreur.f("ZeroEnregistrement")
    }
    choix <- "0"
}

GraphNbEspeceParFamille.f <- function ()
{
    print("fonction GraphNbEspeceParFamille activée")
    ChoixUneFamille.f()
    print(paste("famille considérée :", fa))
    extremes.f()
    ## Il faut rajouter la colonne famille à listespunit
    listespunit$Famille <- especes$Famille[match(listespunit$code_espece, especes$code_espece)]
    ## on restreint la table "listespunit" à la famille sélectionnée
    spunit <- subset(listespunit, listespunit$Famille==fa)
    spunit$nombre <- EnleverMaxExtremes.f(spunit$nombre)
    ## si aucun enregistrement ne correspond à la famille sélectionnée, message d'erreur, sinon le boxplot est réalisé
    if (length(unique(spunit$Famille))>0)
    {
        x11(width=50, height=20, pointsize=10)
        par(mar=c(9, 4, 5, 1), mgp=c(3, 1, 9))
        boxplot(spunit$nombre ~spunit$an+spunit$code_espece, las=2, cex.names=0.6, main=paste("Nombre d'individus par espèce et par an pour la famille ", fa, "\n"))
        nbObs <- tapply(spunit$nombre, list(spunit$spunit$an+code_espece), length)
        axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)), col.ticks="orange", col.axis = "orange", lty = 2, lwd = 0.5)
        legend("topleft", "Nombre d'enregistrement par Boxplot", cex =0.7, col="orange", text.col="orange", merge=FALSE)
        Moyenne <- as.vector(tapply(spunit$nombre, list(spunit$an, spunit$code_espece), na.rm = T, mean))
        points(Moyenne, pch=19, col="blue")
        text(Moyenne+(Moyenne/10), col = "blue", cex = 0.8, labels=as.character(round(Moyenne, digits=1)))
        abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")

        x11(width=50, height=20, pointsize=10)
        par(mar=c(9, 4, 5, 1), mgp=c(3, 1, 9))
        boxplot(spunit$nombre ~spunit$site+spunit$code_espece, las=2, cex.names=0.6, main=paste("Nombre d'individus par espèce et par site pour la famille ", fa, "\n"))
        nbObs <- tapply(spunit$nombre, list(spunit$site, spunit$code_espece), length)
        axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)), col.ticks="orange", col.axis = "orange", lty = 2, lwd = 0.5)
        legend("topleft", "Nombre d'enregistrement par Boxplot", cex =0.7, col="orange", text.col="orange", merge=FALSE)
        Moyenne <- as.vector(tapply(spunit$nombre, list(spunit$site, spunit$code_espece), na.rm = T, mean))
        points(Moyenne, pch=19, col="blue")
        text(Moyenne+(Moyenne/10), col = "blue", cex = 0.8, labels=as.character(round(Moyenne, digits=1)))
        abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")

        textstatut <- c("HR : Hors Réserve", "PP : Protection Partielle", "RE : En réserve")

        x11(width=50, height=20, pointsize=10)
        par(mar=c(9, 4, 5, 1), mgp=c(3, 1, 9))
        boxplot(spunit$nombre ~spunit$statut_protection+spunit$code_espece, las=2, cex.names=0.6, col=heat.colors(length(unique(spunit$statut_protection))), main=paste("Nombre d'individus par espèce et par statut de protection de site pour la famille ", fa, "\n"))
        nbObs <- tapply(spunit$nombre, list(spunit$statut_protection, spunit$code_espece), length)
        axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)), col.ticks="orange", col.axis = "orange", lty = 2, lwd = 0.5)
        legend("topleft", "Nombre d'enregistrement par Boxplot", cex =0.7, col="orange", text.col="orange", merge=FALSE)
        Moyenne <- as.vector(tapply(spunit$nombre, list(spunit$statut_protection, spunit$code_espece), na.rm = T, mean))
        points(Moyenne, pch=19, col="blue")
        text(Moyenne+(Moyenne/10), col = "blue", cex = 0.8, labels=as.character(round(Moyenne, digits=1)))
        abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")
        abline(v = 0.5+(1:length(as.vector(unique(spunit$code_espece))))*length(as.vector(unique(spunit$statut_protection))), col = "red")
        legend("topright", textstatut, col=heat.colors(length(unique(spunit$statut_protection))), pch = 15, cex =0.9, title="Statuts")

        ## lorsqu'il y aura beaucoup de données, rajouter famille/an/statut

    }else{
        tkmessageBox(message="Graphique impossible - pas d'enregistrements dans votre sélection")
        gestionMSGerreur.f("ZeroEnregistrement")
    }
    choix <- "0"
}

GraphchoixNbEspeceParUnitobs.f <- function ()
{
    print("fonction GraphchoixNbEspeceParUnitobs activée")
    ChoixUneUnitobs.f()
    ChoixDesEspeces.f()
    extremes.f()
    spunit <- subset(listespunit, listespunit$site==si)
    spunitSelection <- subset(spunit, spunit$code_espece==sp)
    spunitSelection$nombre <- EnleverMaxExtremes.f(spunitSelection$nombre)
    if (length(unique(spunitSelection$nombre))>0)
    {
        x11(width=50, height=20, pointsize=10)
        par(mar=c(9, 4, 5, 1), mgp=c(3, 1, 9))
        boxplot(spunitSelection$nombre ~spunitSelection$an+spunitSelection$code_espece, las=2, cex.names=0.6, main=paste("Nombre d'individus par an pour les espèces sélectionnées pour le site ", si))
        nbObs <- tapply(spunitSelection$unite_observation, list(spunitSelection$an, spunitSelection$code_espece), length)
        axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)), col.ticks="orange", col.axis = "orange", lty = 2, lwd = 0.5)
        legend("topleft", "Nombre d'enregistrement par Boxplot", cex =0.7, col="orange", text.col="orange", merge=FALSE)
        Moyenne <- as.vector(tapply(spunitSelection$nombre, list(spunitSelection$an, spunitSelection$code_espece), na.rm = T, mean))
        points(Moyenne, pch=19, col="blue")
        text(Moyenne+(Moyenne/10), col = "blue", cex = 1, labels=as.character(round(Moyenne, digits=1)))
        abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")

        x11(width=50, height=20, pointsize=10)
        par(mar=c(9, 4, 5, 1), mgp=c(3, 1, 9))
        boxplot(spunitSelection$nombre ~spunitSelection$statut_protection+spunitSelection$an+spunitSelection$code_espece, col=heat.colors(length(unique(spunitSelection$statut_protection))), fg=heat.colors(length(unique(spunitSelection$statut_protection))), las=2, cex.names=0.6, main=paste("Nombre d'individus par an pour les espèces sélectionnées pour le site ", si))
        nbObsStatut <- tapply(spunitSelection$unite_observation, list(spunitSelection$statut_protection, spunitSelection$an, spunitSelection$code_espece), length)
        axis(3, as.vector(nbObsStatut), at=1:length(as.vector(nbObsStatut)), col.ticks="orange", col.axis = "orange", lty = 2, lwd = 0.5)
        legend("topleft", "Nombre d'enregistrement par Boxplot", cex =0.7, col="orange", text.col="orange", merge=FALSE)
        textstatut <- c("HR : Hors Réserve", "PP : Protection Partielle", "RE : En réserve")
        legend("topright", textstatut, col=heat.colors(length(unique(obs$statut))), pch = 15, cex =0.9, title="Statuts")
        Moyenne <- as.vector(tapply(spunitSelection$nombre, list(spunitSelection$statut_protection, spunitSelection$an, spunitSelection$code_espece), na.rm = T, mean))
        points(Moyenne, pch=19, col="blue")
        text(Moyenne+(Moyenne/10), col = "blue", cex = 1, labels=as.character(round(Moyenne, digits=1)))
        abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")
        abline(v = 0.5+(1:(length(as.vector(unique(spunitSelection$an)))*length(as.vector(unique(spunitSelection$code_espece))))*length(as.vector(unique(unit$statut_protection)))), col = "red")
    }else{
        tkmessageBox(message="Graphique impossible - pas d'enregistrements dans votre sélection")
        gestionMSGerreur.f("ZeroEnregistrement")
    }
    choix <- "0"
}

GraphNbEspeceParAn.f <- function ()
{
    print("fonction GraphNbEspeceParAn activée")
    ChoixUneAnnee.f()
    print(paste("année considérée :", varAn))
    extremes.f()
    ## on restreint la table "listespunit" à l'année sélectionnée
    spunit <- subset(listespunit, listespunit$an==varAn)
    if (length(unique(spunit$nombre))>0)
    {
        x11(width=50, height=20, pointsize=10)
        par(mar=c(9, 4, 5, 1), mgp=c(3, 1, 9))
        boxplot(spunit$nombre ~spunit$biotope+spunit$code_espece, las=2, cex.names=0.6, main=paste("Nombre d'individus par espèce et par biotope pour l'année ", varAn, "\n"))
        nbObs <- tapply(spunit$nombre, list(spunit$biotope, spunit$code_espece), length)
        axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)), col.ticks="orange", col.axis = "orange", lty = 2, lwd = 0.5, cex = 0.6)
        Moyenne <- as.vector(tapply(spunit$nombre, list(spunit$biotope, spunit$code_espece), na.rm = T, mean))
        points(Moyenne, pch=19, col="blue")
        text(Moyenne+(Moyenne/5), col = "blue", cex = 0.6, labels=as.character(round(Moyenne, digits=1)))
        abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")

        x11(width=50, height=20, pointsize=10)
        par(mar=c(9, 4, 5, 1), mgp=c(3, 1, 9))
        boxplot(spunit$nombre ~spunit$code_espece+spunit$site, las=2, cex.names=0.6, main=paste("Nombre d'individus par espèce et par site pour l'année ", varAn))
        nbObs <- tapply(spunit$nombre, list(spunit$code_espece, spunit$site), length)
        axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)), col.ticks="orange", col.axis = "orange", lty = 2, lwd = 0.5)
        legend("topleft", "Nombre d'enregistrement par Boxplot", cex =0.7, col="orange", text.col="orange", merge=FALSE)
        Moyenne <- as.vector(tapply(spunitSelection$nombre, list(spunitSelection$code_espece, spunitSelection$site), na.rm = T, mean))
        points(Moyenne, pch=19, col="blue")
        text(Moyenne+(Moyenne/10), col = "blue", cex = 0.6, labels=as.character(round(Moyenne, digits=1)))
        abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")

        x11(width=50, height=20, pointsize=10)
        par(mar=c(9, 4, 5, 1), mgp=c(3, 1, 9))
        boxplot(spunit$nombre ~spunit$statut_protection+spunit$code_espece, las=2, cex.names=0.6, col=heat.colors(length(unique(spunit$statut_protection))), main=paste("Nombre d'individus par espèce et par statut de protection pour l'année ", varAn))
        nbObsStatut <- tapply(spunit$nombre, list(spunit$statut_protection, spunit$code_espece), length)
        axis(3, as.vector(nbObsStatut), at=1:length(as.vector(nbObsStatut)), col.ticks="orange", col.axis = "orange", lty = 2, lwd = 0.5)
        legend("topleft", "Nombre d'enregistrement par Boxplot", cex =0.7, col="orange", text.col="orange", merge=FALSE)
        textstatut <- c("HR : Hors Réserve", "PP : Protection Partielle", "RE : En réserve")
        legend("topright", textstatut, col=heat.colors(length(unique(spunit$statut_protection))), pch = 15, cex =0.9, title="Statuts")
        Moyenne <- as.vector(tapply(spunit$nombre, list(spunit$statut_protection, spunit$code_espece), na.rm = T, mean))
        points(Moyenne, pch=19, col="blue")
        text(Moyenne+(Moyenne/10), col = "blue", cex = 1, labels=as.character(round(Moyenne, digits=1)))
        abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")
        abline(v = 0.5+(1:length(as.vector(unique(spunit$code_espece))))*length(as.vector(unique(spunit$statut_protection))), col = "red")
    }else{
        tkmessageBox(message="Graphique impossible - pas d'enregistrements dans votre sélection")
        gestionMSGerreur.f("ZeroEnregistrement")
    }
    choix <- "0"
}

GraphchoixNbEspeceParAn.f <- function ()
{
    print("fonction GraphchoixNbEspeceParAn activée")
    ChoixUneAnnee.f()
    ChoixDesEspeces.f()
    extremes.f()
    spunit <- subset(listespunit, listespunit$an==varAn)
    spunitSelection <- subset(spunit, spunit$code_espece==sp)

    if (length(unique(spunit$nombre))>0)
    {

        textstatut <- c("HR : Hors Réserve", "PP : Protection Partielle", "RE : En réserve")
        ## Espèces par biotope pour l'année
        x11(width=50, height=20, pointsize=10)
        par(mar=c(13, 4, 5, 1), mgp=c(3, 1, 15))
        boxplot(spunitSelection$nombre ~spunitSelection$code_espece+spunitSelection$biotope, las=2, cex.names=0.6, main=paste("Nombre d'individus par biotope pour les espèces sélectionnées pour l'année ", varAn))
        nbObs <- tapply(spunitSelection$unite_observation, list(spunitSelection$code_espece, spunitSelection$biotope), length)
        axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)), col.ticks="orange", col.axis = "orange", lty = 2, lwd = 0.5)
        Moyenne <- as.vector(tapply(spunitSelection$nombre, list(spunitSelection$code_espece, spunitSelection$biotope), na.rm = T, mean))
        points(Moyenne, pch=19, col="blue")
        text(Moyenne+(Moyenne/10), col = "blue", cex = 1, labels=as.character(round(Moyenne, digits=1)))
        abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")

        ## Espèces par statut de protection et par biotope pour l'année
        x11(width=50, height=20, pointsize=10)
        par(mar=c(13, 4, 5, 1), mgp=c(3, 1, 15))
        boxplot(spunitSelection$nombre ~spunitSelection$statut_protection+spunitSelection$biotope+spunitSelection$code_espece, col=heat.colors(length(unique(spunitSelection$statut_protection))), las=2, cex.names=0.6, main=paste("Nombre d'individus par biotope pour les espèces sélectionnées pour l'année ", varAn))
        nbObsStatut <- tapply(spunitSelection$unite_observation, list(spunitSelection$statut_protection, spunitSelection$biotope, spunitSelection$code_espece), length)
        axis(3, as.vector(nbObsStatut), at=1:length(as.vector(nbObsStatut)), col.ticks="orange", col.axis = "orange", lty = 2, lwd = 0.5)
        legend("topleft", "Nombre d'enregistrement par Boxplot", cex =0.7, col="orange", text.col="orange", merge=FALSE)
        legend("topright", textstatut, col=heat.colors(length(unique(spunitSelection$statut_protection))), pch = 15, cex =0.9, title="Statuts")
        Moyenne <- as.vector(tapply(spunitSelection$nombre, list(spunitSelection$statut_protection, spunitSelection$biotope, spunitSelection$code_espece), na.rm = T, mean))
        points(Moyenne, pch=19, col="blue")
        text(Moyenne+(Moyenne/10), col = "blue", cex = 1, labels=as.character(round(Moyenne, digits=1)))
        abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")
        abline(v = 0.5+(1:length(as.vector(unique(spunitSelection$biotope))))*length(as.vector(unique(spunitSelection$statut_protection))), col = "red")

        ## Espèces par site pour l'année
        x11(width=50, height=20, pointsize=10)
        par(mar=c(13, 4, 5, 1), mgp=c(3, 1, 15))
        boxplot(spunitSelection$nombre ~spunitSelection$code_espece+spunitSelection$site, las=2, cex.names=0.6, main=paste("Nombre d'individus par site pour les espèces sélectionnées pour l'année ", varAn))
        nbObs <- tapply(spunitSelection$unite_observation, list(spunitSelection$code_espece, spunitSelection$site), length)
        axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)), col.ticks="orange", col.axis = "orange", lty = 2, lwd = 0.5)
        legend("topleft", "Nombre d'enregistrement par Boxplot", cex =0.7, col="orange", text.col="orange", merge=FALSE)
        Moyenne <- as.vector(tapply(spunitSelection$nombre, list(spunitSelection$code_espece, spunitSelection$site), na.rm = T, mean))
        points(Moyenne, pch=19, col="blue")
        text(Moyenne+(Moyenne/10), col = "blue", cex = 1, labels=as.character(round(Moyenne, digits=1)))
        abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")

        ## Espèces par statut de protection et par site pour l'année
        x11(width=50, height=20, pointsize=10)
        par(mar=c(13, 4, 5, 1), mgp=c(3, 1, 15))
        boxplot(spunitSelection$nombre ~spunitSelection$statut_protection+spunitSelection$site+spunitSelection$code_espece, col=heat.colors(length(unique(spunitSelection$statut_protection))), las=2, cex.names=0.6, main=paste("Nombre d'individus par site pour les espèces sélectionnées pour l'année ", varAn))
        nbObsStatut <- tapply(spunitSelection$unite_observation, list(spunitSelection$statut_protection, spunitSelection$site, spunitSelection$code_espece), length)
        axis(3, as.vector(nbObsStatut), at=1:length(as.vector(nbObsStatut)), col.ticks="orange", col.axis = "orange", lty = 2, lwd = 0.5)
        legend("topleft", "Nombre d'enregistrement par Boxplot", cex =0.7, col="orange", text.col="orange", merge=FALSE)
        legend("topright", textstatut, col=heat.colors(length(unique(obs$statut))), pch = 15, cex =0.9, title="Statuts")
        Moyenne <- as.vector(tapply(spunitSelection$nombre, list(spunitSelection$statut_protection, spunitSelection$site, spunitSelection$code_espece), na.rm = T, mean))
        points(Moyenne, pch=19, col="blue")
        text(Moyenne+(Moyenne/10), col = "blue", cex = 1, labels=as.character(round(Moyenne, digits=1)))
        abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")
        abline(v = 0.5+(1:length(as.vector(unique(spunitSelection$site))))*length(as.vector(unique(spunitSelection$statut_protection))), col = "red")

    }else{
        tkmessageBox(message="Graphique impossible - pas d'enregistrements dans votre sélection")
        gestionMSGerreur.f("ZeroEnregistrement")
    }
    choix <- "0"
}

## [sup] [yr: 11/01/2011]

## GraphNbObsParSite.f <- function ()
## {

##     ## A FINIR

##     print("fonction GraphNbObsParSite activée")
##     ChoixUnSite.f()
##     extremes.f()
##     obssite <- subset(unitesp, unitesp$site==si)
##     obssite$AnBiotope <- paste(obssite$an, obssite$biotope, sep="\n")
##     NbObsParsite <- tapply(obssite$nombre, list(obssite$code_espece, obssite$unite_observation), sum, na.rm=TRUE)
##     if (length(unique(obssite$nombre))>0)
##     {
##         x11(width=120, height=50, pointsize=10)
##         emplacement <- barplot(NbObsParsite, axisnames=FALSE, cex.names=0.8, las=2, main=paste("Nombre d'observation de chaque espèce par unitobs pour le site ", si))
##         axis(1, at=emplacement, las=2, labels=obssite$AnBiotope[match(colnames(NbObsParsite), obssite$unite_observation)], cex.axis=0.7, lwd=0)

##         x11(width=70, height=50, pointsize=10)
##         boxplot(obssite$nombre[obssite$nombre!=0] ~obssite$an[obssite$nombre!=0], main=paste("Nombre d'observations\n pour le site", si, " selon l'année\n\n"))
##         nbObs <- tapply(obssite$nombre[obssite$nombre!=0], obssite$an[obssite$nombre!=0], length)
##         axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)), col.ticks="orange", col.axis = "orange", lty = 2, lwd = 0.5)
##         legend("topleft", "Nombre d'enregistrement par Boxplot", cex =0.7, col="orange", text.col="orange", merge=FALSE)

##         tkmessageBox(message="D'autres graphiques sont en cours de développement")
##     }else{
##         tkmessageBox(message="Graphique impossible - pas d'enregistrements dans votre sélection")
##         gestionMSGerreur.f("ZeroEnregistrement")
##     }
##     choix <- "0"
## }

## [sup] [yr: 11/01/2011]

## GraphNbObsParbiotope.f <- function ()
## {

##     ## A FINIR

##     print("fonction GraphNbObsParbiotope activée")
##     ChoixUnbiotope.f()
##     extremes.f()
##     obsbiotope <- subset(unitesp, unitesp$biotope==bio)
##     obsbiotope$AnBiotope <- paste(obsbiotope$an, obsbiotope$unite_observation, sep="\n")
##     NbObsParbiotope <- tapply(obsbiotope$nombre, list(obsbiotope$code_espece, obsbiotope$unite_observation), sum, na.rm=TRUE)
##     if (length(unique(obsbiotope$nombre))>0)
##     {
##         x11(width=120, height=50, pointsize=10)
##         emplacement <- barplot(NbObsParbiotope, axisnames=FALSE, cex.names=0.8, las=2, main=paste("Nombre d'observation de chaque espèce pour une unitobs pour le biotope ", bio))
##         axis(1, at=emplacement, las=2, labels=obsbiotope$AnBiotope[match(colnames(NbObsParbiotope), obsbiotope$unite_observation)], cex.axis=0.7, lwd=0)

##         x11(width=70, height=50, pointsize=10)
##         boxplot(obsbiotope$nombre[obsbiotope$nombre!=0] ~obsbiotope$an[obsbiotope$nombre!=0], main=paste("Nombre d'observations\n pour le biotope", bio, " selon l'année\n\n"))
##         nbObs <- tapply(obsbiotope$nombre[obsbiotope$nombre!=0], obsbiotope$an[obsbiotope$nombre!=0], length)
##         axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)), col.ticks="orange", col.axis = "orange", lty = 2, lwd = 0.5)
##         legend("topleft", "Nombre d'enregistrement par Boxplot", cex =0.7, col="orange", text.col="orange", merge=FALSE)

##         tkmessageBox(message="D'autres graphiques sont en cours de développement")
##     }else{
##         tkmessageBox(message="Graphique impossible - pas d'enregistrements dans votre sélection")
##         gestionMSGerreur.f("ZeroEnregistrement")
##     }
##     choix <- "0"
## }

## [sup] [yr: 11/01/2011]

## GraphNbObsParhabitat1.f <- function ()
## {

##     ## A FINIR

##     print("fonction GraphNbObsParhabitat1 activée")
##     ChoixUnhabitat1.f()
##     extremes.f()
##     obshabitat1 <- subset(unitesp, unitesp$habitat1==ha)
##     obshabitat1$AnBiotope <- paste(obshabitat1$an, obshabitat1$biotope, sep="\n")
##     NbObsParhabitat1 <- tapply(obshabitat1$nombre, list(obshabitat1$code_espece, obshabitat1$unite_observation), sum, na.rm=TRUE)
##     if (length(unique(obshabitat1$nombre))>0)
##     {
##         x11(width=120, height=50, pointsize=10)
##         boxplot(obshabitat1$nombre[obshabitat1$nombre!=0] ~obshabitat1$an[obshabitat1$nombre!=0], main=paste("Nombre d'observations\n pour l'habitat", ha, "\n selon l'année"))
##         x11(width=120, height=50, pointsize=10)
##         axis(1, at=barplot(NbObsParhabitat1, axisnames=FALSE, cex.names=0.8, las=2, main=paste("Nombre d'observation de chaque espèce pour une unitobs pour l'habitat ", ha)), las=2, labels=obshabitat1$AnBiotope[match(colnames(NbObsParhabitat1), obshabitat1$unite_observation)], cex.axis=0.7, lwd=0)
##     }else{
##         tkmessageBox(message="Graphique impossible - pas d'enregistrements dans votre sélection")
##         gestionMSGerreur.f("ZeroEnregistrement")
##     }
##     choix <- "0"
## }

## [sup] [yr: 11/01/2011]

## GraphNbObsParfamille.f <- function ()
## {

##     print("fonction GraphNbObsParfamille activée")
##     extremes.f()
##     obs$famille <- especes$Famille[match(obs$code_espece, especes$code_espece)]
##     obs$an <- unitobs$an[match(obs$unite_observation, unitobs$unite_observation)]
##     if (length(unique(obs$nombre))>0)
##     {
##         textstatut <- c("HR : Hors Réserve", "PP : Protection Partielle", "RE : En réserve")

##         x11(width=50, height=50, pointsize=10)
##         boxplot(obs$nombre[obs$famille!= "fa."] ~obs$an[obs$famille!= "fa."], main=paste("Nombre d'observations\n par famille \n selon l'année"))
##         nbObs <- tapply(obs$nombre[obs$famille!= "fa."], obs$an[obs$famille!= "fa."], length)
##         axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)), col.ticks="orange", col.axis = "orange", lty = 2, lwd = 0.5)
##         legend("topleft", "Nombre d'enregistrement par Boxplot", cex =0.7, col="orange", text.col="orange", merge=FALSE)
##         Moyenne <- as.vector(tapply(obs$nombre[obs$famille!= "fa."], obs$an[obs$famille!= "fa."], na.rm = T, mean))
##         points(Moyenne, pch=19, col="blue")
##         abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")

##         ChoixUneFamille.f()
##         x11(width=50, height=50, pointsize=10)
##         boxplot(obs$nombre[obs$famille==fa] ~obs$an[obs$famille==fa], main=paste("Nombre d'observations pour la famille", fa, "\n selon l'année\n\n"))
##         nbObs <- tapply(obs$nombre[obs$famille==fa], obs$an[obs$famille==fa], length)
##         axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)), col.ticks="orange", col.axis = "orange", lty = 2, lwd = 0.5)
##         legend("topleft", "Nombre d'enregistrement par Boxplot", cex =0.7, col="orange", text.col="orange", merge=FALSE)
##         Moyenne <- as.vector(tapply(obs$nombre[obs$famille==fa], obs$an[obs$famille==fa], na.rm = T, mean))
##         points(Moyenne, pch=19, col="blue")
##         text(Moyenne+(Moyenne/10), col = "blue", cex = 1, labels=as.character(round(Moyenne, digits=1)))
##         abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")

##         x11(width=120, height=50, pointsize=10)
##         boxplot(obs$nombre[obs$famille==fa] ~obs$statut[obs$famille==fa]+obs$an[obs$famille==fa], col=heat.colors(length(unique(obs$statut))), las=2, main=paste("Nombre d'observations pour la famille ", fa, "\n selon le statut et l'année \n\n"))
##         nbObs <- tapply(obs$nombre[obs$famille==fa], list(obs$statut[obs$famille==fa], obs$an[obs$famille==fa]), length)
##         axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)), col.ticks="orange", col.axis = "orange", lty = 2, lwd = 0.5)
##         legend("topleft", "Nombre d'enregistrement par Boxplot", cex =0.7, col="orange", text.col="orange", merge=FALSE)
##         legend("topright", textstatut, col=heat.colors(length(unique(obs$statut))), pch = 15, cex =0.9, title="Statuts")
##         Moyenne <- as.vector(tapply(obs$nombre[obs$famille==fa], list(obs$statut[obs$famille==fa], obs$an[obs$famille==fa]), na.rm = T, mean))
##         points(Moyenne, pch=19, col="blue")
##         text(Moyenne+(Moyenne/10), col = "blue", cex = 1, labels=as.character(round(Moyenne, digits=1)))
##         abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")
##         abline(v = 0.5+(1:length(as.vector(unique(obs$an))))*length(as.vector(unique(obs$statut))), col = "red")
##     }else{
##         tkmessageBox(message="Graphique impossible - pas d'enregistrements dans votre sélection")
##         gestionMSGerreur.f("ZeroEnregistrement")
##     }
##     choix <- "0"
## }

## [sup] [yr: 11/01/2011]

## GraphNbObsParAn.f <- function ()
## {
##     print("fonction GraphNbObsParAn activée")
##     extremes.f()
##     obs$an <- unitobs$an[match(obs$unite_observation, unitobs$unite_observation)]
##     if (length(unique(obs$nombre))>0)
##     {
##         x11(width=100, height=50, pointsize=10)
##         boxplot(obs$nombre[obs$nombre!= 0] ~obs$an[obs$nombre!= 0], main=paste("Nombre d'observations par enregistrement\n selon l'année\n\n"))
##         nbObs <- tapply(obs$nombre[obs$nombre!= 0], obs$an[obs$nombre!= 0], length)
##         legend("topleft", "Nombre d'enregistrement par Boxplot", cex =0.7, col="orange", text.col="orange", merge=FALSE)
##         axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)), col.ticks="orange", col.axis = "orange", lty = 2, lwd = 0.5)
##         Moyenne <- as.vector(tapply(obs$nombre[obs$nombre!= 0], obs$an[obs$nombre!= 0], na.rm = T, mean))
##         points(Moyenne, pch=19, col="blue")
##         abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")
##     }else{
##         tkmessageBox(message="Graphique impossible - pas d'enregistrements dans votre sélection")
##         gestionMSGerreur.f("ZeroEnregistrement")
##     }
##     choix <- "0"
## }

## [sup] [yr: 11/01/2011]

## GraphNbObsParStatut.f <- function ()
## {
##     print("fonction GraphNbObsParStatut activée")
##     extremes.f()
##     obs$statut <- unitobs$statut[match(obs$unite_observation, unitobs$unite_observation)]
##     if (length(unique(obs$nombre))>0)
##     {
##         textstatut <- c("HR : Hors Réserve", "PP : Protection Partielle", "RE : En réserve")
##         x11(width=50, height=50, pointsize=10)
##         boxplot(obs$nombre[obs$nombre!= 0] ~obs$statut[obs$nombre!= 0], col=heat.colors(length(unique(obs$statut))), main=paste("Nombre d'observations par enregistrement\n selon le statut\n\n"))
##         nbObs <- tapply(obs$nombre[obs$nombre!= 0], obs$statut[obs$nombre!= 0], length)
##         legend("topleft", "Nombre d'enregistrement par Boxplot", cex =0.7, col="orange", text.col="orange", merge=FALSE)
##         legend("bottomright", textstatut, col=heat.colors(length(unique(obs$statut))), pch = 15, cex =0.9, title="Statuts")
##         axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)), col.ticks="orange", col.axis = "orange", lty = 2, lwd = 0.5)
##         Moyenne <- as.vector(tapply(obs$nombre[obs$nombre!= 0], obs$statut[obs$nombre!= 0], na.rm = T, mean))
##         points(Moyenne, pch=19, col="blue")
##         abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")
##     }else{
##         tkmessageBox(message="Graphique impossible - pas d'enregistrements dans votre sélection")
##         gestionMSGerreur.f("ZeroEnregistrement")
##     }
##     choix <- "0"
## }

## [sup] [yr: 11/01/2011]

## GraphNbObsParAnStatut.f <- function ()
## {
##     print("fonction GraphNbObsParAnStatut activée")
##     obs$an <- unitobs$an[match(obs$unite_observation, unitobs$unite_observation)]
##     obs$statut <- unitobs$statut[match(obs$unite_observation, unitobs$unite_observation)]
##     extremes.f()      #ici on coisit si l'on veut garder les x % des valeurs extremes
##     obs$nombre <- EnleverMaxExtremes.f(obs$nombre)    #  selon choix retourné par extremes.f()

##     if (length(unique(obs$nombre))>0)
##     {
##         textstatut <- c("HR : Hors Réserve", "PP : Protection Partielle", "RE : En réserve")

##         x11(width=120, height=50, pointsize=10)
##         boxplot(obs$nombre[obs$nombre!= 0] ~obs$statut[obs$nombre!= 0]+obs$an[obs$nombre!= 0], col=heat.colors(length(unique(obs$statut))), las=2, main=paste("Nombre d'observations par enregistrement\n selon le statut et l'année\n\n"))
##         nbObs <- tapply(obs$nombre[obs$nombre!= 0], list(obs$statut[obs$nombre!= 0], obs$an[obs$nombre!= 0]), length)
##         legend("topleft", "Nombre d'enregistrement par Boxplot", cex =0.7, col="orange", text.col="orange", merge=FALSE)
##         legend("bottomright", textstatut, col=heat.colors(length(unique(obs$statut))), pch = 15, cex =0.9, title="Statuts")
##         axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)), col.ticks="orange", col.axis = "orange", lty = 2, lwd = 0.5, cex =0.7)
##         ## Moyenne = as.vector(tapply(obs$nombre[obs$nombre!= 0], list(obs$statut[obs$nombre!= 0], obs$an[obs$nombre!= 0]), na.rm = T, mean))
##         Moyenne <- as.vector(tapply(obs$nombre, list(obs$statut, obs$an), na.rm = T, mean))
##         points(Moyenne, pch=19, col="blue")
##         abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")
##         abline(v = 0.5+(1:length(as.vector(unique(obs$an))))*length(as.vector(unique(obs$statut))), col = "red")
##         if (choix=="1")
##         {
##             legend("top", "Enregistrements > 95% du maximum retirés", cex =0.7, col="red", text.col="red", merge=FALSE)
##         }

##         x11(width=100, height=50, pointsize=10)
##         emplacement <- barplot(Moyenne)[, 1]
##         Sd <- as.vector(tapply(obs$nombre, list(obs$statut, obs$an), na.rm = T, sd))
##         Rbarplot <- barplot(Moyenne, cex.lab=1.2, col=heat.colors(length(unique(obs$statut))), main=paste("Moyenne du nombre d'observations par enregistrement\n selon le statut et l'année\n\n"))
##         legend("topleft", "Nombre d'enregistrement par Barre", cex =0.7, col="orange", text.col="orange", merge=FALSE)
##         legend("topright", textstatut, col=heat.colors(length(unique(obs$statut))), pch = 15, cex =0.9, title="Statuts")
##         axis(3, as.vector(nbObs), at=emplacement, col.ticks="orange", col.axis = "orange", lty = 2, lwd = 0.5, cex =0.7)
##         malegende <- unique(paste(obs$an, obs$statut))
##         axis(1, malegende[order(malegende)], at=emplacement, col.ticks="black", cex =0.7, las=2)
##         abline(v = emplacement+0.7 , col = "lightgray", lty = "dotted")
##         arrows(Rbarplot, Moyenne - Sd, Rbarplot, Moyenne + Sd, code = 3, col = "purple", angle = 90, length = .1)
##         if (choix=="1")
##         {
##             legend("top", "Enregistrements > 95% du maximum retirés", cex =0.7, col="red", text.col="red", merge=FALSE)
##         }
##         ## abline(v = (0.5+(1:length(as.vector(unique(obs$an))))*length(as.vector(unique(obs$statut))))*emplacement, col = "red") #plus tard
##     }else{
##         tkmessageBox(message="Graphique impossible - pas d'enregistrements dans votre sélection")
##         gestionMSGerreur.f("ZeroEnregistrement")
##     }
##     choix <- "0"
## }

GraphDelta.f <- function ()
{
    print("fonction GraphDelta activée")
    extremes.f()

    if (unique(unitobs$type)=="LIT")
    {
        tkmessageBox(message="ATTENTION, veillez à ce que le référentiel espèce ne contienne que des espèces \n (pas de catégories benthiques) pour calculer un indice corect")
    }

    if (length(unit$Delta)>0)
    {
        textstatut <- c("HR : Hors Réserve", "PP : Protection Partielle", "RE : En réserve")

        x11(width=120, height=50, pointsize=10)
        boxplot(unit$Delta, main=paste("Delta pour les ", dim(unit)[1], " unités d'observation"))
        Moyenne <- as.vector(mean(unit$Delta, na.rm = T))
        points(Moyenne, pch=19, col="blue")

        x11(width=120, height=50, pointsize=10)
        boxplot(unit$Delta ~unit$an,  las=2, col=heat.colors(length(unique(unit$an))), main=paste("Delta pour les ", dim(unit)[1], " unités d'observation selon l'année\n\n"))
        nbObs <- tapply(unit$Delta, unit$an, length)
        axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)), col.ticks="orange", col.axis = "orange", lty = 2, lwd = 0.5)
        Moyenne <- as.vector(tapply(unit$Delta, unit$an, na.rm = T, mean))
        points(Moyenne, pch=19, col="blue")
        text(Moyenne+(Moyenne/10), col = "blue", cex = 1, labels=as.character(round(Moyenne, digits=1)))
        abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")
        legend("topleft", "Nombre d'enregistrement par Boxplot", cex =0.7, col="orange", text.col="orange", merge=FALSE)

        x11(width=120, height=50, pointsize=10)
        boxplot(unit$Delta ~unit$statut,  las=2, col=heat.colors(length(unique(unit$statut))), main=paste("Delta pour les ", dim(unit)[1], " unités d'observation selon le statut\n\n"))
        nbObs <- tapply(unit$Delta, unit$statut, length)
        axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)), col.ticks="orange", col.axis = "orange", lty = 2, lwd = 0.5)
        Moyenne <- as.vector(tapply(unit$Delta, unit$statut, na.rm = T, mean))
        points(Moyenne, pch=19, col="blue")
        text(Moyenne+(Moyenne/10), col = "blue", cex = 1, labels=as.character(round(Moyenne, digits=1)))
        abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")
        textstatut <- c("HR : Hors Réserve", "PP : Protection Partielle", "RE : En réserve")
        legend("bottomright", textstatut, col=heat.colors(length(unique(unit$statut))), pch = 15, cex =0.9, title="Statuts")
        legend("topleft", "Nombre d'enregistrement par Boxplot", cex =0.7, col="orange", text.col="orange", merge=FALSE)

        x11(width=120, height=50, pointsize=10)
        boxplot(unit$Delta ~unit$statut+unit$an, las=2, col=heat.colors(length(unique(unit$statut))), main=paste("Delta pour les ", dim(unit)[1], " unités d'observation selon l'année et le statut\n\n"))
        nbObs <- tapply(unit$Delta, list(unit$statut, unit$an), length)
        axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)), col.ticks="orange", col.axis = "orange", lty = 2, lwd = 0.5)
        Moyenne <- as.vector(tapply(unit$Delta, list(unit$statut, unit$an), na.rm = T, mean))
        points(Moyenne, pch=19, col="blue")
        text(Moyenne+(Moyenne/10), col = "blue", cex = 1, labels=as.character(round(Moyenne, digits=1)))
        abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")
        abline(v = 0.5+(1:length(as.vector(unique(unit$an))))*length(as.vector(unique(unit$statut))), col = "red")
        textstatut <- c("HR : Hors Réserve", "PP : Protection Partielle", "RE : En réserve")
        legend("bottomright", textstatut, col=heat.colors(length(unique(unit$statut))), pch = 15, cex =0.9, title="Statuts")
        legend("topleft", "Nombre d'enregistrement par Boxplot", cex =0.7, col="orange", text.col="orange", merge=FALSE)
    }else{
        tkmessageBox(message="Graphique impossible - pas d'enregistrements dans votre sélection")
        gestionMSGerreur.f("ZeroEnregistrement")
    }
    choix <- "0"
}

GraphDeltaEtoile.f <- function ()
{
    print("fonction GraphDeltaEtoile activée")
    extremes.f()
    if (unique(unitobs$type)=="LIT")
    {
        tkmessageBox(message="ATTENTION, veillez à ce que le référentiel espèce ne contienne que des espèces \n (pas de catégories benthiques) pour calculer un indice corect")
    }
    if (length(unit$DeltaEtoile)>0)
    {
        textstatut <- c("HR : Hors Réserve", "PP : Protection Partielle", "RE : En réserve")

        x11(width=120, height=50, pointsize=10)
        boxplot(unit$DeltaEtoile, main=paste("Delta * pour les ", dim(unit)[1], " unités d'observation"))
        Moyenne <- as.vector(mean(unit$DeltaEtoile, na.rm = T))
        points(Moyenne, pch=19, col="blue")
        text(Moyenne+(Moyenne/10), col = "blue", cex = 1, labels=as.character(round(Moyenne, digits=1)))
        abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")
        legend("topleft", "Nombre d'enregistrement par Boxplot", cex =0.7, col="orange", text.col="orange", merge=FALSE)

        x11(width=120, height=50, pointsize=10)
        boxplot(unit$DeltaEtoile ~unit$an,  las=2, col=heat.colors(length(unique(unit$an))), main=paste("Delta * pour les ", dim(unit)[1], " unités d'observation selon l'année\n\n"))
        nbObs <- tapply(unit$DeltaEtoile, unit$an, length)
        axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)), col.ticks="orange", col.axis = "orange", lty = 2, lwd = 0.5)
        Moyenne <- as.vector(tapply(unit$DeltaEtoile, unit$an, na.rm = T, mean))
        points(Moyenne, pch=19, col="blue")
        text(Moyenne+(Moyenne/10), col = "blue", cex = 1, labels=as.character(round(Moyenne, digits=1)))
        abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")

        x11(width=120, height=50, pointsize=10)
        boxplot(unit$DeltaEtoile ~unit$statut,  las=2, col=heat.colors(length(unique(unit$statut))), main=paste("Delta * pour les ", dim(unit)[1], " unités d'observation selon le statut\n\n"))
        nbObs <- tapply(unit$DeltaEtoile, unit$statut, length)
        axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)), col.ticks="orange", col.axis = "orange", lty = 2, lwd = 0.5)
        Moyenne <- as.vector(tapply(unit$DeltaEtoile, unit$statut, na.rm = T, mean))
        points(Moyenne, pch=19, col="blue")
        text(Moyenne+(Moyenne/10), col = "blue", cex = 1, labels=as.character(round(Moyenne, digits=1)))
        abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")
        textstatut <- c("HR : Hors Réserve", "PP : Protection Partielle", "RE : En réserve")
        legend("bottomright", textstatut, col=heat.colors(length(unique(unit$statut))), pch = 15, cex =0.9, title="Statuts")
        legend("topleft", "Nombre d'enregistrement par Boxplot", cex =0.7, col="orange", text.col="orange", merge=FALSE)


        x11(width=120, height=50, pointsize=10)
        boxplot(unit$DeltaEtoile ~unit$statut+unit$an, las=2, legend.text=TRUE, args.legend = list(x = "topright", title = "Statut"), col=heat.colors(length(unique(unit$statut))), main=paste("Delta * pour les ", dim(unit)[1], " unités d'observation selon l'année et le statut\n\n"))
        nbObs <- tapply(unit$DeltaEtoile, list(unit$statut, unit$an), length)
        axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)), col.ticks="orange", col.axis = "orange", lty = 2, lwd = 0.5)
        Moyenne <- as.vector(tapply(unit$DeltaEtoile, list(unit$statut, unit$an), na.rm = T, mean))
        points(Moyenne, pch=19, col="blue")
        text(Moyenne+(Moyenne/10), col = "blue", cex = 1, labels=as.character(round(Moyenne, digits=1)))
        abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")
        abline(v = 0.5+(1:length(as.vector(unique(unit$an))))*length(as.vector(unique(unit$statut))), col = "red")
        textstatut <- c("HR : Hors Réserve", "PP : Protection Partielle", "RE : En réserve")
        legend("bottomright", textstatut, col=heat.colors(length(unique(unit$statut))), pch = 15, cex =0.9, title="Statuts")
        legend("topleft", "Nombre d'enregistrement par Boxplot", cex =0.7, col="orange", text.col="orange", merge=FALSE)
    }else{
        tkmessageBox(message="Graphique impossible - pas d'enregistrements dans votre sélection")
        gestionMSGerreur.f("ZeroEnregistrement")
    }
    choix <- "0"
}

GraphDeltaPlus.f <- function ()
{
    print("fonction GraphDeltaPlus activée")
    extremes.f()
    if (unique(unitobs$type)=="LIT")
    {
        tkmessageBox(message="ATTENTION, veillez à ce que le référentiel espèce ne contienne que des espèces \n (pas de catégories benthiques) pour calculer un indice corect")
    }
    if (length(unit$DeltaPlus)>0)
    {

        textstatut <- c("HR : Hors Réserve", "PP : Protection Partielle", "RE : En réserve")

        x11(width=120, height=50, pointsize=10)
        boxplot(unit$DeltaPlus, main=paste("Delta + pour les ", dim(unit)[1], " unités d'observation"))
        Moyenne <- as.vector(mean(unit$DeltaPlus, na.rm = T))
        points(Moyenne, pch=19, col="blue")
        text(Moyenne+(Moyenne/10), col = "blue", cex = 1, labels=as.character(round(Moyenne, digits=1)))
        abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")
        legend("topleft", "Nombre d'enregistrement par Boxplot", cex =0.7, col="orange", text.col="orange", merge=FALSE)

        x11(width=120, height=50, pointsize=10)
        boxplot(unit$DeltaPlus ~unit$an,  las=2, col=heat.colors(length(unique(unit$an))), main=paste("Delta + pour les ", dim(unit)[1], " unités d'observation selon l'année\n\n"))
        nbObs <- tapply(unit$DeltaPlus, unit$an, length)
        axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)), col.ticks="orange", col.axis = "orange", lty = 2, lwd = 0.5)
        Moyenne <- as.vector(tapply(unit$DeltaPlus, unit$an, na.rm = T, mean))
        points(Moyenne, pch=19, col="blue")
        text(Moyenne+(Moyenne/10), col = "blue", cex = 1, labels=as.character(round(Moyenne, digits=1)))
        abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")

        x11(width=120, height=50, pointsize=10)
        boxplot(unit$DeltaPlus ~unit$statut,  las=2, col=heat.colors(length(unique(unit$statut))), main=paste("Delta + pour les ", dim(unit)[1], " unités d'observation selon le statut\n\n"))
        nbObs <- tapply(unit$DeltaPlus, unit$statut, length)
        axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)), col.ticks="orange", col.axis = "orange", lty = 2, lwd = 0.5)
        Moyenne <- as.vector(tapply(unit$DeltaPlus, unit$statut, na.rm = T, mean))
        points(Moyenne, pch=19, col="blue")
        text(Moyenne+(Moyenne/10), col = "blue", cex = 1, labels=as.character(round(Moyenne, digits=1)))
        abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")
        textstatut <- c("HR : Hors Réserve", "PP : Protection Partielle", "RE : En réserve")
        legend("bottomright", textstatut, col=heat.colors(length(unique(unit$statut))), pch = 15, cex =0.9, title="Statuts")
        legend("topleft", "Nombre d'enregistrement par Boxplot", cex =0.7, col="orange", text.col="orange", merge=FALSE)


        x11(width=120, height=50, pointsize=10)
        boxplot(unit$DeltaPlus ~unit$statut+unit$an, las=2, legend.text=TRUE, args.legend = list(x = "topright", title = "Statut"), col=heat.colors(length(unique(unit$statut))), main=paste("Delta + pour les ", dim(unit)[1], " unités d'observation selon l'année et le statut\n\n"))
        nbObs <- tapply(unit$DeltaPlus, list(unit$statut, unit$an), length)
        axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)), col.ticks="orange", col.axis = "orange", lty = 2, lwd = 0.5)
        Moyenne <- as.vector(tapply(unit$DeltaPlus, list(unit$statut, unit$an), na.rm = T, mean))
        points(Moyenne, pch=19, col="blue")
        text(Moyenne+(Moyenne/10), col = "blue", cex = 1, labels=as.character(round(Moyenne, digits=1)))
        abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")
        abline(v = 0.5+(1:length(as.vector(unique(unit$an))))*length(as.vector(unique(unit$statut))), col = "red")
        textstatut <- c("HR : Hors Réserve", "PP : Protection Partielle", "RE : En réserve")
        legend("bottomright", textstatut, col=heat.colors(length(unique(unit$statut))), pch = 15, cex =0.9, title="Statuts")
        legend("topleft", "Nombre d'enregistrement par Boxplot", cex =0.7, col="orange", text.col="orange", merge=FALSE)
    }else{
        tkmessageBox(message="Graphique impossible - pas d'enregistrements dans votre sélection")
        gestionMSGerreur.f("ZeroEnregistrement")
    }
    choix <- "0"
}

GraphSDeltaPlus.f <- function ()
{
    print("fonction GraphSDeltaPlus activée")
    extremes.f()
    if (unique(unitobs$type)=="LIT")
    {
        tkmessageBox(message="ATTENTION, veillez à ce que le référentiel espèce ne contienne que des espèces \n (pas de catégories benthiques) pour calculer un indice corect")
    }
    if (length(unit$DeltaPlus)>0)
    {
        textstatut <- c("HR : Hors Réserve", "PP : Protection Partielle", "RE : En réserve")

        x11(width=120, height=50, pointsize=10)
        boxplot(unit$SDeltaPlus, main=paste("SDelta + pour les ", dim(unit)[1], " unités d'observation"))
        Moyenne <- as.vector(mean(unit$SDeltaPlus, na.rm = T))
        points(Moyenne, pch=19, col="blue")
        text(Moyenne+(Moyenne/10), col = "blue", cex = 1, labels=as.character(round(Moyenne, digits=1)))
        abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")
        legend("topleft", "Nombre d'enregistrement par Boxplot", cex =0.7, col="orange", text.col="orange", merge=FALSE)

        x11(width=120, height=50, pointsize=10)
        boxplot(unit$SDeltaPlus ~unit$an,  las=2, col=heat.colors(length(unique(unit$an))), main=paste("SDelta + pour les ", dim(unit)[1], " unités d'observation selon l'année\n\n"))
        nbObs <- tapply(unit$SDeltaPlus, unit$an, length)
        axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)), col.ticks="orange", col.axis = "orange", lty = 2, lwd = 0.5)
        Moyenne <- as.vector(tapply(unit$SDeltaPlus, unit$an, na.rm = T, mean))
        points(Moyenne, pch=19, col="blue")
        text(Moyenne+(Moyenne/10), col = "blue", cex = 1, labels=as.character(round(Moyenne, digits=1)))
        abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")

        x11(width=120, height=50, pointsize=10)
        boxplot(unit$SDeltaPlus ~unit$statut,  las=2, col=heat.colors(length(unique(unit$statut))), main=paste("SDelta + pour les ", dim(unit)[1], " unités d'observation selon le statut\n\n"))
        nbObs <- tapply(unit$SDeltaPlus, unit$statut, length)
        axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)), col.ticks="orange", col.axis = "orange", lty = 2, lwd = 0.5)
        Moyenne <- as.vector(tapply(unit$SDeltaPlus, unit$statut, na.rm = T, mean))
        points(Moyenne, pch=19, col="blue")
        text(Moyenne+(Moyenne/10), col = "blue", cex = 1, labels=as.character(round(Moyenne, digits=1)))
        abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")
        textstatut <- c("HR : Hors Réserve", "PP : Protection Partielle", "RE : En réserve")
        legend("bottomright", textstatut, col=heat.colors(length(unique(unit$statut))), pch = 15, cex =0.9, title="Statuts")
        legend("topleft", "Nombre d'enregistrement par Boxplot", cex =0.7, col="orange", text.col="orange", merge=FALSE)

        x11(width=120, height=50, pointsize=10)
        boxplot(unit$SDeltaPlus ~unit$statut+unit$an, las=2, legend.text=TRUE, args.legend = list(x = "topright", title = "Statut"), col=heat.colors(length(unique(unit$statut))), main=paste("SDelta + pour les ", dim(unit)[1], " unités d'observation selon l'année et le statut\n\n"))
        nbObs <- tapply(unit$SDeltaPlus, list(unit$statut, unit$an), length)
        axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)), col.ticks="orange", col.axis = "orange", lty = 2, lwd = 0.5)
        Moyenne <- as.vector(tapply(unit$SDeltaPlus, list(unit$statut, unit$an), na.rm = T, mean))
        points(Moyenne, pch=19, col="blue")
        text(Moyenne+(Moyenne/10), col = "blue", cex = 1, labels=as.character(round(Moyenne, digits=1)))
        abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")
        abline(v = 0.5+(1:length(as.vector(unique(unit$an))))*length(as.vector(unique(unit$statut))), col = "red")
        textstatut <- c("HR : Hors Réserve", "PP : Protection Partielle", "RE : En réserve")
        legend("bottomright", textstatut, col=heat.colors(length(unique(unit$statut))), pch = 15, cex =0.9, title="Statuts")
        legend("topleft", "Nombre d'enregistrement par Boxplot", cex =0.7, col="orange", text.col="orange", merge=FALSE)
    }else{
        tkmessageBox(message="Graphique impossible - pas d'enregistrements dans votre sélection")
        gestionMSGerreur.f("ZeroEnregistrement")
    }
    choix <- "0"
}

GraphLambdaPlus.f <- function ()
{
    print("fonction GraphLambdaPlus activée")
    extremes.f()
    if (unique(unitobs$type)=="LIT")
    {
        tkmessageBox(message="ATTENTION, veillez à ce que le référentiel espèce ne contienne que des espèces \n (pas de catégories benthiques) pour calculer un indice corect")
    }
    if (length(unit$LambdaPlus)>0)
    {
        textstatut <- c("HR : Hors Réserve", "PP : Protection Partielle", "RE : En réserve")

        x11(width=120, height=50, pointsize=10)
        boxplot(unit$LambdaPlus, main=paste("Lambda + pour les ", dim(unit)[1], " unités d'observation"))
        Moyenne <- as.vector(mean(unit$LambdaPlus, na.rm = T))
        points(Moyenne, pch=19, col="blue")
        text(Moyenne+(Moyenne/10), col = "blue", cex = 1, labels=as.character(round(Moyenne, digits=1)))
        abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")
        legend("topleft", "Nombre d'enregistrement par Boxplot", cex =0.7, col="orange", text.col="orange", merge=FALSE)

        x11(width=120, height=50, pointsize=10)
        boxplot(unit$LambdaPlus ~unit$an,  las=2, col=heat.colors(length(unique(unit$an))), main=paste("Lambda + pour les ", dim(unit)[1], " unités d'observation selon l'année\n\n"))
        nbObs <- tapply(unit$LambdaPlus, unit$an, length)
        axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)), col.ticks="orange", col.axis = "orange", lty = 2, lwd = 0.5)
        Moyenne <- as.vector(tapply(unit$LambdaPlus, unit$an, na.rm = T, mean))
        points(Moyenne, pch=19, col="blue")
        text(Moyenne+(Moyenne/10), col = "blue", cex = 1, labels=as.character(round(Moyenne, digits=1)))
        abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")

        x11(width=120, height=50, pointsize=10)
        boxplot(unit$LambdaPlus ~unit$statut,  las=2, col=heat.colors(length(unique(unit$statut))), main=paste("Lambda + pour les ", dim(unit)[1], " unités d'observation selon le statut\n\n"))
        nbObs <- tapply(unit$LambdaPlus, unit$statut, length)
        axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)), col.ticks="orange", col.axis = "orange", lty = 2, lwd = 0.5)
        Moyenne <- as.vector(tapply(unit$LambdaPlus, unit$statut, na.rm = T, mean))
        points(Moyenne, pch=19, col="blue")
        text(Moyenne+(Moyenne/10), col = "blue", cex = 1, labels=as.character(round(Moyenne, digits=1)))
        abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")
        textstatut <- c("HR : Hors Réserve", "PP : Protection Partielle", "RE : En réserve")
        legend("bottomright", textstatut, col=heat.colors(length(unique(unit$statut))), pch = 15, cex =0.9, title="Statuts")
        legend("topleft", "Nombre d'enregistrement par Boxplot", cex =0.7, col="orange", text.col="orange", merge=FALSE)

        x11(width=120, height=50, pointsize=10)
        boxplot(unit$LambdaPlus ~unit$statut+unit$an, las=2, legend.text=TRUE, args.legend = list(x = "topright", title = "Statut"), col=heat.colors(length(unique(unit$statut))), main=paste("Lambda + pour les ", dim(unit)[1], " unités d'observation selon l'année et le statut\n\n"))
        nbObs <- tapply(unit$LambdaPlus, list(unit$statut, unit$an), length)
        axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)), col.ticks="orange", col.axis = "orange", lty = 2, lwd = 0.5)
        Moyenne <- as.vector(tapply(unit$LambdaPlus, list(unit$statut, unit$an), na.rm = T, mean))
        points(Moyenne, pch=19, col="blue")
        text(Moyenne+(Moyenne/10), col = "blue", cex = 1, labels=as.character(round(Moyenne, digits=1)))
        abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")
        abline(v = 0.5+(1:length(as.vector(unique(unit$an))))*length(as.vector(unique(unit$statut))), col = "red")
        textstatut <- c("HR : Hors Réserve", "PP : Protection Partielle", "RE : En réserve")
        legend("bottomright", textstatut, col=heat.colors(length(unique(unit$statut))), pch = 15, cex =0.9, title="Statuts")
        legend("topleft", "Nombre d'enregistrement par Boxplot", cex =0.7, col="orange", text.col="orange", merge=FALSE)
    }else{
        tkmessageBox(message="Graphique impossible - pas d'enregistrements dans votre sélection")
        gestionMSGerreur.f("ZeroEnregistrement")
    }
    choix <- "0"
}

## ! a revoir demain    la suite


################################################################################
## Nom    : GraphiqueRecouvrement.f()
## Objet  : graphiques recouvrement diagramme moustache sur chaque espèce
## Input  : matrice "matricerecouvrement"
## Output : boxplot recouvrement
################################################################################

PartRecouvrementUnitobs.f <- function (matricerecouvrement, typegraph)
{
    print("fonction PartRecouvrementUnitobs activée")
    extremes.f()
    ## nbColMax=30
    PartRecouvrementParUnitobs <- tapply(unitesp$nombre, list(unitesp$code_espece, unitesp$unite_observation), sum, na.rm=TRUE)

    if (length(PartRecouvrementParUnitobs[PartRecouvrementParUnitobs!=0])>0) # si il y a des recouvrements de renseignés, on fait les graphiques
    {
        ReturnVal <- gestionMSGchoix("Définiser le nombre de colones pour le graphique", paste("Votre graphique contient plus de", nbColMax, " colonnes, voulez vous le diviser ?"), nbColMax)
        if (ReturnVal=="ID_CANCEL")
        {
            x11(width=120, height=50, pointsize=10)
            par(mar=c(7, 6, 2, 8), mgp=c(4.5, 0.5, 0))
            barplot(PartRecouvrementParUnitobs, col = row(as.matrix(PartRecouvrementParUnitobs)), cex.lab=1.2, las=2, legend.text=TRUE, args.legend = list(x = "topright", title = "Espèces"), main="recouvrements par unite d'observation")
            return()
        }
        tkmessageBox(title="Nombre de colonne défini", message=paste("Votre nombre de colonne est défini à ", ReturnVal, ".", sep=""))
        if (dim(PartRecouvrementParUnitobs)[1]<(nbColMax+1))
        {
            barplot(PartRecouvrementParUnitobs, col = row(PartRecouvrementParUnitobs)[, 0], legend.text=TRUE, main="recouvrements par unite d'observation")
        }else{  # condition de partage du graphique en plusieurs parties

            barplot(PartRecouvrementParUnitobs, col = row(as.matrix(PartRecouvrementParUnitobs)), legend.text=TRUE, main="recouvrements par unite d'observation")
            nbgraphe <- as.integer(dim(PartRecouvrementParUnitobs)[2]/nbColMax+1)
            gestionMSGinfo.f("MSGnbgraphe", nbgraphe)
            for (i in 0:(nbgraphe-1))
            {
                debutcol <- i*nbColMax+1
                fincol <- (i+1)*nbColMax
                X11()
                if (i<(nbgraphe-1))
                {
                    barplot(PartRecouvrementParUnitobs[, debutcol:fincol], col = row(as.matrix(PartRecouvrementParUnitobs)), legend.text=TRUE, main=paste("recouvrements par unite d'observation ", debutcol, ":", fincol))
                }else{
                    barplot(PartRecouvrementParUnitobs[, debutcol:dim(PartRecouvrementParUnitobs)[2]], col = row(as.matrix(PartRecouvrementParUnitobs)), legend.text=TRUE, main=paste("recouvrements par unite d'observation ", debutcol, ":", dim(PartRecouvrementParUnitobs)))}
            }# fin du for
        }
    }else{
        tkmessageBox(message="Graphique impossible - pas d'enregistrements dans votre sélection")
        gestionMSGerreur.f("ZeroEnregistrement")
    }
    choix <- "0"
}

GraphiqueRecouvrement.f <- function (matricerecouvrement, typegraph)
{
    matricerecouvrement <- CalculRecouvrement.f()
    extremes.f()

    typegraph <- "tout"
    if (typegraph=="tout")
    {
        x11(width=50, height=30, pointsize=10)
        boxplot(as.data.frame(matricerecouvrement), col=row(matricerecouvrement), las=2, cex.names=0.8, main = "Recouvrement (en %) par espèce")
        par(las=1)# all axis labels horizontal
        for (i in 1:length(unique(unitobs$statut_protection)))
        {
            unitstatut <- unitobs$unite_observation[which(unitobs$statut_protection==unique(unitobs$statut_protection)[i])]
            unitselect <- matricerecouvrement[match(unitstatut, rownames(matricerecouvrement)), ]
            x11(width=50, height=30, pointsize=10)
            boxplot(as.data.frame(matricerecouvrement), col=row(matricerecouvrement), main = paste("Recouvrement (en %) par espèce pour le statut", unique(unitobs$statut_protection)[i]), horizontal = TRUE)
            legend("right", "histogramme de part de recouvrement toutes espèces confondues")
            x11(width=50, height=30, pointsize=10)
            boxplot(matricerecouvrement, col=row(matricerecouvrement), cex.lab=1.2, las=2, xlab="Espèces présentes", ylab="part de recouvrement", main=paste("Recouvrement (en %) par espèce pour les observations du statut", unique(unitobs$statut_protection)[i]))
        }
    }
    typegraph <- "paran"
    if (typegraph=="paran")
    {
        par(las=1)# all axis labels horizontal
        for (i in 1:length(unique(unitobs$an)))
        {
            unitan <- unitobs$unite_observation[which(unitobs$an==unique(unitobs$an)[i])]
            unitselectan <- matricerecouvrement[match(unitan, rownames(matricerecouvrement)), ]
            x11(width=50, height=30, pointsize=10)
            boxplot(as.data.frame(unitselectan), col=row(unitselectan), main = paste("Recouvrement (en %) par espèce pour l'année ", unique(unitobs$an)[i]), horizontal = TRUE)
            col2 <- rgb(1, 0, 1, 0.5)
            legend("right", "histogramme de part de recouvrement toutes espèces confondues")
            x11(width=50, height=30, pointsize=10)
            boxplot(unitselectan, col=row(unitselectan), cex.lab=1.2, las=2, xlab="Espèces présentes", ylab="part de recouvrement", main=paste("Recouvrement (en %) par espèce pour les observations de l'année", unique(unitobs$an)[i]))
        }
    }

    typegraph <- "parfamille"
    if (typegraph=="parfamille")
    {
        ChoixUneFamille.f()
        ListeEspFamilleSelectionnee <- especes$code_espece[which(especes$Famille==fa)]
        ## on met dans une liste les espèces de la famille sélectionnée
        if (sum(match(ListeEspFamilleSelectionnee, colnames(matricerecouvrement)), na.rm=T)!=0)
        {
            unitselect <- matricerecouvrement[match(ListeEspFamilleSelectionnee, colnames(matricerecouvrement)), ]
            x11(width=50, height=30, pointsize=10)
            par(las=1)# all axis labels horizontal
            unitselect2 <- unitselect[, which(apply(unitselect, 2, sum, na.rm=T)!=0)]
            boxplot(as.data.frame(unitselect2), col=row(unitselect2), main = paste("Recouvrement (en %) par espèce pour la famille", fa), horizontal = TRUE)
            boxplot(as.data.frame(unitselect2), col=row(unitselect2), main = paste("Recouvrement (en %) par espèce pour la famille", fa), horizontal = TRUE)
            legend("right", paste("Part de recouvrement pour les espèces de la famille", fa))
            x11(width=50, height=30, pointsize=10)
            boxplot(unitselect[, which( apply(unitselect, 2, sum, na.rm=T)!=0)], col=row(unitselect[, which( apply(unitselect, 2, sum, na.rm=T)!=0)]), cex.lab=1.2, las=2, xlab="Espèces présentes", ylab="Part de recouvrement", main=paste("Recouvrement (en %) par espèce pour les observations de la famille", fa))
        }
    }
    choix <- "0"
}

PartRecouvrementTot.f <- function (matricerecouvrement, typegraph)
{
    print("fonction PartRecouvrementTot activée")
    extremes.f()

    col2 <- rgb(0, 1, 1, 0.5)
    hist(as.matrix(matricerecouvrement), col = col2)
    choix <- "0"
}

PartRecouvrementEsp.f <- function (matricerecouvrement, typegraph)
{
    print("fonction PartRecouvrementEsp activée")}


## switch(typegraph,
## typegraph=="1" print("1")
## typegraph=="2" print("2")
## )
## bottomright, bottom, bottomleft, left, topleft, top, topright, right, center
## boxplot(matricerecouvrement ~ row(matricerecouvrement), col = col2, varwidth = TRUE)
## camembert: pie(paysPla[1:length(paysPla)-1], labels=paste(names(paysPla), round(paysPla[1:length(paysPla)-1], digits=2), "%"), main=paste("Pays de résidence des plaisanciers"))
## jout des écart types sur un barplot (appelé ici Rbarplot) :
## arrows(Rbarplot, tabPersZone["moyenne", ] - tabPersZone["ecart-type", ], Rbarplot, tabPersZone["moyenne", ] + tabPersZone["ecart-type", ], code = 3, col = "purple", angle = 90, length = .1)

## barplot(existAMP[1:(nrow(existAMP)-1), ], axisnames=TRUE, axis.lty=1, legend.text=T, main="Connaissance de l'existence de l'AMP selon l'activité pratiquée", font.main = 4, ylim=c(0, 120), ylab="pourcentage de réponse")
## boxplot(matricerecouvrement ~ matricerecouvrement[, 0], data=matricerecouvrement, varwidth = TRUE, ylab="Recouvrement (% par transect)", main=paste("Recouvrement de ", sp, sep=""), las=3)

## require(grDevices) # for colours
## barplot(tN, col=heat.colors(12), log = "y") dégradés pour espèces de la même famille

################################################################################
## Nom    : affichageGraphiques.f
## Objet  : fonction d'affichage des graphiques
## Input  : OUI ou NON
## Output : Graphiques générés
################################################################################
## ! cette fonction ne fait aucun graphique et n'appelle rien : peut être en début de programme (sous le nom affichageInterfaceGraphique)

AffichageGraphiques.f <- function ()
{
    print("fonction AffichageGraphiques.f activée")

    nn <- tktoplevel()
    tkwm.title(nn, "Affichage des graphiques")
    tkgrid(tklabel(nn, text="Voulez-vous afficher les graphiques ?"))
    done <- tclVar(0)
    OK.but <- tkbutton(nn, text="OUI", command=function() {tclvalue(done) <- 1})
    Cancel.but <- tkbutton(nn, text="NON", command=function() {tclvalue(done) <- 2})
    tkgrid(OK.but, Cancel.but)
    tkbind(nn, "<Destroy>", function() {tclvalue(done) <- 2})
    tkfocus(nn)
    tkwait.variable(done)
    doneVal <- as.integer(tclvalue(done))
    assign("doneVal", doneVal, envir=.GlobalEnv)
    tkdestroy(nn)
}
################################################################################
## Nom    : graph1.f()
## Objet  : affichage des graphiques par groupe d'unites d'observation
## Input  : tables "unit" (pour pouvoir utiliser des boxplot sur l'ensemble des
## valeurs et non pas sur la valeur agrégée de la table grp)
## + 1 facteur fact
## Output : graphiques
################################################################################

graph1.f <- function (fact)
{

    print("fonction graph1.f activée")

    AffichageGraphiques.f()

    if (doneVal==1)
    {
        unit[, fact] <- unitobs[, fact][match(unit$unitobs, unitobs$unite_observation)]
        assign("unit", unit, envir=.GlobalEnv)

        if (unique(unitobs$type) != "LIT")
        {
            ## affichage des indices representes sous boxplot dans un message
            tkmessageBox(message=paste("Les indices representes sous boxplot sont :
    - la densite
    - la biomasse
    - la richesse specifique
    - l indice de Simpson
    - l indice de Pielou
    - l indice de Hill
    - l indice de diversite taxonomique"))

            ## test existence champ densite
            if (length(unique(unit$densite))>1)
            {
                X11()
                if (length(typePeche)>1)
                {
                    x11(width=12, height=8, pointsize=12)
                    par(mar=c(8, 15, 4, 2), mgp=c(10, 1, 0))
                    boxplot(unit$densite ~ unit[, fact], data=unit, varwidth = TRUE, ylab="CPUE en nombre", main=paste(typePeche, "- CPUE par", fact), las=1, horizontal = TRUE)
                }else{
                    boxplot(unit$densite ~ unit[, fact], data=unit, varwidth = TRUE, ylab=expression("Densite "(individus/m^2)), main=paste("Densite d'abondance moyenne par", fact))
                }
                bx1 <- as.vector(tapply(unit$densite, unit[, fact], na.rm = T, mean))
                points(bx1, pch=19, col="red")
            }
            ## test existence champ biomasse
            if (length(unique(unit$biomasse))>1)
            {
                X11()
                boxplot(unit$biomasse ~ unit[, fact], data=unit, varwidth = TRUE, ylab=expression("Biomasse "(g/m^2)), main=paste("Biomasse moyenne par", fact))
                bx1 <- as.vector(tapply(unit$biomasse, unit[, fact], na.rm = T, mean))
                points(bx1, pch=19, col="red")
            }

            X11()
            boxplot(unit$richesse_specifique ~ unit[, fact], data=unit, varwidth = TRUE, ylab="Nombre d'especes", main=paste("Richesse specifique moyenne par", fact))
            bx1 <- as.vector(tapply(unit$richesse_specifique, unit[, fact], na.rm = T, mean))
            points(bx1, pch=19, col="red")
            X11()
            boxplot(unit$simpson ~ unit[, fact], data=unit, varwidth = TRUE, ylab="Simpson", main="Indice de Simpson")
            bx1 <- as.vector(tapply(unit$sim, unit[, fact], na.rm = T, mean))
            points(bx1, pch=19, col="red")
            X11()
            boxplot(unit$pielou ~ unit[, fact], data=unit, varwidth = TRUE, ylab="Pielou", main="Indice de Pielou")
            bx1 <- as.vector(tapply(unit$pielou, unit[, fact], na.rm = T, mean))
            points(bx1, pch=19, col="red")
            X11()
            boxplot(unit$hill ~ unit[, fact], data=unit, varwidth = TRUE, ylab="Hill", main="Indice de Hill")
            bx1 <- as.vector(tapply(unit$hill, unit[, fact], na.rm = T, mean))
            points(bx1, pch=19, col="red")
        } # fin cas != LIT

        if (unique(unitobs$type) == "LIT")
        {
            ## affichage des indices representés sous boxplot dans un message
            tkmessageBox(message=paste("Les indices representes sous boxplot sont :
  - la richesse specifique
  - l indice de Simpson
  - l indice de Pielou
  - l indice de Hill
  - l indice de diversite taxonomique"))

            X11()
            boxplot(unit$richesse_specifique ~ unit[, fact], data=unit, varwidth = TRUE, las=3, ylab="Nombre d'especes", main="Richesse specifique")
            bx1 <- as.vector(tapply(unit$richesse_specifique, unit[, fact], na.rm = T, mean))
            points(bx1, pch=19, col="red")
            X11()
            boxplot(unit$simpson ~ unit[, fact], data=unit, varwidth = TRUE, las=3, ylab="Simpson", main="Indice de Simpson")
            bx1 <- as.vector(tapply(unit$sim, unit[, fact], na.rm = T, mean))
            points(bx1, pch=19, col="red")
            X11()
            boxplot(unit$pielou ~ unit[, fact], data=unit, varwidth = TRUE, las=3, ylab="Pielou", main="Indice de Pielou")
            bx1 <- as.vector(tapply(unit$pielou, unit[, fact], na.rm = T, mean))
            points(bx1, pch=19, col="red")
            X11()
            boxplot(unit$hill ~ unit[, fact], data=unit, varwidth = TRUE, las=3, ylab="Hill", main="Indice de Hill")
            bx1 <- as.vector(tapply(unit$hill, unit[, fact], na.rm = T, mean))
            points(bx1, pch=19, col="red")
            X11()
            boxplot(unit$Delta ~ unit[, fact], data=unit, varwidth = TRUE, ylab="Delta", main="Indice de diversite taxonomique")
            bx1 <- as.vector(tapply(unit$Delta, unit[, fact], na.rm = T, mean))
            points(bx1, pch=19, col="red")
            X11()
            boxplot(unit$DeltaEtoile ~ unit[, fact], data=unit, varwidth = TRUE, ylab="Delta*", main="Indice d'originalite taxonomique")
            bx1 <- as.vector(tapply(unit$DeltaEtoile, unit[, fact], na.rm = T, mean))
            points(bx1, pch=19, col="red")
            X11()
            boxplot(unit$LambdaPlus ~ unit[, fact], data=unit, varwidth = TRUE, ylab="Lambda+", main="Indice de variation de l'originalite taxonomique")
            bx1 <- as.vector(tapply(unit$LambdaPlus, unit[, fact], na.rm = T, mean))
            points(bx1, pch=19, col="red")
            X11()
            boxplot(unit$DeltaPlus ~ unit[, fact], data=unit, varwidth = TRUE, ylab="Delta+", main="Indice d'originalite taxonomique sur les presence/absence")
            bx1 <- as.vector(tapply(unit$DeltaPlus, unit[, fact], na.rm = T, mean))
            points(bx1, pch=19, col="red")
            X11()
            boxplot(unit$SDeltaPlus ~ unit[, fact], data=unit, varwidth = TRUE, ylab="S Delta+", main="RS*Delta+")
            bx1 <- as.vector(tapply(unit$SDeltaPlus, unit[, fact], na.rm = T, mean))
            points(bx1, pch=19, col="red")

            ii <- tktoplevel()
            tkwm.title(ii, "Recouvrement par espece")
            scr <- tkscrollbar(ii, repeatinterval=5, command=function(...)tkyview(tl, ...))
            tl <- tklistbox(ii, height=20, width=50, selectmode="single", yscrollcommand=function(...)tkset(scr, ...), background="white")
            tkgrid(tklabel(ii, text="recouvrement de l'espece:"))
            tkgrid(tl, scr)
            tkgrid.configure(scr, rowspan=4, sticky="nsw")
            esp <- sort(especes$code_espece)
            a <- length(esp)
            for (i in (1:a))
            {
                tkinsert(tl, "end", esp[i])
            }

            tkselection.set(tl, 0)

            OnOK <- function ()
            {
                choixespece <- esp[as.numeric(tkcurselection(tl))+1]
                b <- especes$code_espece[especes$code_espece == choixespece]
                tkdestroy(ii)
                print(paste("Pourcentage de recouvrement de", choixespece))
                unitesp[, fact] <- unitobs[, fact][match(unitesp$unite_observation, unitobs$unite_observation)]
                X11()
                boxplot(unitesp$recouvrement[unitesp$code_espece == b] ~ unitesp[unitesp$code_espece == b, fact], data=unitesp, varwidth = TRUE, las=3, ylab="%", main="Pourcentage de recouvrement")
                X11()
                boxplot(unitesp$colonie[unitesp$code_espece == b] ~ unitesp[unitesp$code_espece == b, fact], data=unitesp, varwidth = TRUE, las=3, ylab="Colonies", main="Nombre de colonies")
            }

            OK.but <-tkbutton(ii, text="OK", command=OnOK)
            tkgrid(OK.but)
            tkfocus(ii)
            tkwait.window(ii)
            rm(a)
        }#fin cas LIT
    } #fin doneEval
} #fin graph1.f

################################################################################
## Nom    : graph2.f()
## Objet  : affichage des graphiques par groupe d'unités d'observation
## Input  : tables "unit" (pour pouvoir utiliser des boxplot sur l'ensemble des
##          valeurs et non pas sur la valeur agrégée de la table grp12)
##          + 2 facteurs fact21 et fact22
## Output : graphiques
################################################################################

graph2.f <- function (fact21, fact22)
{

    print("fonction graph2.f activée")
    AffichageGraphiques.f()

    ## si volonté d'afficher les graphiques
    if (doneVal==1)
    {
        unit[, fact21] <- unitobs[, fact21][match(unit$unitobs, unitobs$unite_observation)]
        unit[, fact22] <- unitobs[, fact22][match(unit$unitobs, unitobs$unite_observation)]
        assign("unit", unit, envir=.GlobalEnv)

        ## affichage des indices representes sous boxplot dans un message
        tkmessageBox(message=paste("Les indices representes sous boxplot sont :
  - la taille moyenne
  - la densite
  - la biomasse
  - la richesse specifique
  - l indice de Simpson
  - l indice de Pielou
  - l indice de Hill
  - l indice de diversite taxonomique
  - l indice d originalite taxonomique
  - l indice de variation de l originalite taxonomique
  - l indice d originalite taxonomique sur les presences/absences
  - le RS*Delta+
  "))

        ## test existence champs biomasse et densite
        if (length(unique(unit$biomasse))>1)
        {
            x11(width=9, height=6, pointsize=12)
            par(mar=c(5, 6, 4, 1))
            interaction.plot(grp12[, fact21], grp12[, fact22], grp12$densite, lwd=2, col=cl[seq(550, (550+(4*(length(split(grp12, grp12[, fact22]))-1))), by=4)],
                             type="b", fun=mean, trace.label = fact22, xlab="Annee", ylab=expression("Densite "(individus/m^2)),
                             main=paste("Densite d'abondance moyenne par", fact21, "et", fact22))
            x11(width=11, height=8, pointsize=12)
            par(mar=c(8, 15, 4, 2), mgp=c(5, 1, 0))
            boxplot(unit$densite ~ unit[, fact21] + unit[, fact22], varwidth = TRUE, xlab=expression("Densite "(individus/m^2)), main=paste("Densite d'abondance moyenne par", fact21, "et", fact22), las=1,
                    col=c(rep(cl[seq(400, (400+(4*(length(split(unit, unit[, fact22]))-1))), by=4)], each=length(split(unit, unit[, fact21])))), horizontal = TRUE)
            x1 <- as.vector(tapply(unit$densite, list(unit[, fact22], unit[, fact21]), na.rm = T, mean))
            y1 <- 1:length(x1)
            points(x1, y1, pch=19, col="red")
            x11(width=12, height=8, pointsize=12)
            par(mar=c(8, 15, 4, 2))
            boxplot(unit$biomasse ~ unit[, fact21] + unit[, fact22], data=unit, varwidth = TRUE, ylim = c(0, max(1.58*tapply(unit$biomasse, list(unit[, fact21], unit[, fact22]), IQR), na.rm=TRUE)), xlab=expression("Biomasse "(g/m^2)), main=paste("Biomasse moyenne par", fact21, "et", fact22), las=1,
                    col=c(rep(cl[seq(400, (400+(4*(length(split(unit, unit[, fact22]))-1))), by=4)], each=length(split(unit, unit[, fact21])))), horizontal = TRUE)
            x1 <- as.vector(tapply(unit$biomasse, list(unit[, fact22], unit[, fact21]), na.rm = T, mean))
            y1 <- 1:length(x1)
            points(x1, y1, pch=19, col="red")
        }
        ## richesse specifique totale
        x11(width=9, height=6, pointsize=12)
        par(mar=c(5, 6, 4, 1))
        interaction.plot(grp12[, fact21], grp12[, fact22], grp12$richesse_specifique,
                         lwd=2, col=cl[seq(550, (550+(4*(length(split(grp12, grp12[, fact22]))-1))), by=4)], type="b",
                         fun=mean, trace.label = fact22, xlab="Annee", ylab="Nombre d'especes", main=paste("Richesse specifique totale par", fact21, "et", fact22))
        ## richesse specifique moyenne
        x11(width=12, height=8, pointsize=12)
        par(mar=c(8, 15, 4, 2))
        boxplot(unit$richesse_specifique ~ unit[, fact21] + unit[, fact22], data=unit, varwidth = TRUE, xlab="Nombre d'especes", main=paste("Richesse specifique moyenne par", fact21, "et", fact22), las=1,
                col=c(rep(cl[seq(400, (400+(4*(length(split(unit, unit[, fact22]))-1))), by=4)], each=length(split(unit, unit[, fact21])))), horizontal = TRUE)
        x1 <- as.vector(tapply(unit$richesse_specifique, list(unit[, fact22], unit[, fact21]), na.rm = T, mean))
        y1 <- 1:length(x1)
        points(x1, y1, pch=19, col="red")
        x11(width=12, height=8, pointsize=12)
        par(mar=c(8, 15, 4, 2))
        boxplot(unit$sim ~ unit[, fact21] + unit[, fact22], data=unit, varwidth = TRUE, xlab="Simpson", main="Indice de Simpson", las=1,
                col=c(rep(cl[seq(400, (400+(4*(length(split(unit, unit[, fact22]))-1))), by=4)], each=length(split(unit, unit[, fact21])))), horizontal = TRUE)
        bx1 <- as.vector(tapply(unit$sim, list(unit[, fact21], unit[, fact22]), na.rm = T, mean))
        points(bx1, pch=19, col="red")

        x11(width=12, height=8, pointsize=12)
        par(mar=c(8, 15, 4, 2))
        boxplot(unit$pielou ~ unit[, fact21] + unit[, fact22], data=unit, varwidth = TRUE, xlab="Pielou", main="Indice de Pielou", las=1,
                col=c(rep(cl[seq(400, (400+(4*(length(split(unit, unit[, fact22]))-1))), by=4)], each=length(split(unit, unit[, fact21])))), horizontal = TRUE)
        bx1 <- as.vector(tapply(unit$pielou, list(unit[, fact21], unit[, fact22]), na.rm = T, mean))
        points(bx1, pch=19, col="red")
        x11(width=12, height=8, pointsize=12)
        par(mar=c(8, 15, 4, 2))
        boxplot(unit$hill ~ unit[, fact21] + unit[, fact22], data=unit, varwidth = TRUE, xlab="Hill", main="Indice de Hill", las=1,
                col=c(rep(cl[seq(400, (400+(4*(length(split(unit, unit[, fact22]))-1))), by=4)], each=length(split(unit, unit[, fact21])))), horizontal = TRUE)
        bx1 <- as.vector(tapply(unit$hill, list(unit[, fact21], unit[, fact22]), na.rm = T, mean))
        points(bx1, pch=19, col="red")
        x11(width=12, height=8, pointsize=12)
        par(mar=c(8, 15, 4, 2))
        boxplot(unit$Delta ~ unit[, fact21] + unit[, fact22], data=unit, varwidth = TRUE, xlab="Delta", main="Indice de diversite taxonomique (Delta)", las=1,
                col=c(rep(cl[seq(400, (400+(4*(length(split(unit, unit[, fact22]))-1))), by=4)], each=length(split(unit, unit[, fact21])))), horizontal = TRUE)
        bx1 <- as.vector(tapply(unit$Delta, list(unit[, fact21], unit[, fact22]), na.rm = T, mean))
        points(bx1, pch=19, col="red")
        x11(width=12, height=8, pointsize=12)
        par(mar=c(8, 15, 4, 2))
        boxplot(unit$DeltaEtoile ~ unit[, fact21] + unit[, fact22], data=unit, varwidth = TRUE, xlab="Delta*", main="Indice d'originalite taxonomique (Delta*)", las=1,
                col=c(rep(cl[seq(400, (400+(4*(length(split(unit, unit[, fact22]))-1))), by=4)], each=length(split(unit, unit[, fact21])))), horizontal = TRUE)
        bx1 <- as.vector(tapply(unit$DeltaEtoile, list(unit[, fact21], unit[, fact22]), na.rm = T, mean))
        points(bx1, pch=19, col="red")
        x11(width=12, height=8, pointsize=12)
        par(mar=c(8, 15, 4, 2))
        boxplot(unit$LambdaPlus ~ unit[, fact21] + unit[, fact22], data=unit, varwidth = TRUE, xlab="Lambda+", main="Indice de variation de l'originalite taxonomique (Lambda+)", las=1,
                col=c(rep(cl[seq(400, (400+(4*(length(split(unit, unit[, fact22]))-1))), by=4)], each=length(split(unit, unit[, fact21])))), horizontal = TRUE)
        bx1 <- as.vector(tapply(unit$LambdaPlus, list(unit[, fact21], unit[, fact22]), na.rm = T, mean))
        points(bx1, pch=19, col="red")
        x11(width=12, height=8, pointsize=12)
        par(mar=c(8, 15, 4, 2))
        boxplot(unit$DeltaPlus ~ unit[, fact21] + unit[, fact22], data=unit, varwidth = TRUE, xlab="Delta+", main="Indice d'originalite taxonomique sur les presence/absence (Delta+)", las=1,
                col=c(rep(cl[seq(400, (400+(4*(length(split(unit, unit[, fact22]))-1))), by=4)], each=length(split(unit, unit[, fact21])))), horizontal = TRUE)
        bx1 <- as.vector(tapply(unit$DeltaPlus, list(unit[, fact21], unit[, fact22]), na.rm = T, mean))
        points(bx1, pch=19, col="red")
        x11(width=12, height=8, pointsize=12)
        par(mar=c(8, 15, 4, 2))
        boxplot(unit$SDeltaPlus ~ unit[, fact21] + unit[, fact22], data=unit, varwidth = TRUE, xlab="RS Delta+", main="RS*Delta+", las=1,
                col=c(rep(cl[seq(400, (400+(4*(length(split(unit, unit[, fact22]))-1))), by=4)], each=length(split(unit, unit[, fact21])))), horizontal = TRUE)
        bx1 <- as.vector(tapply(unit$SDeltaPlus, list(unit[, fact21], unit[, fact22]), na.rm = T, mean))
        points(bx1, pch=19, col="red")
    } #fin doneEval
} #fin graph2.f

################################################################################
## Nom    : graph3.f()
## Objet  : affichage des graphiques par groupe d'unités d'observation
## Input  : tables "unit" (pour pouvoir utiliser des boxplot sur l'ensemble des
##          valeurs et non pas sur la valeur agrégée de la table grp13)
## Output : graphiques
################################################################################

graph3.f <- function (fact31, fact32, fact33)
{

    print("fonction graph3.f activée")
    AffichageGraphiques.f()


    ## si volonte d'afficher les graphs
    if (doneVal==1)
    {

        unit[, fact31] <- unitobs[, fact31][match(unit$unitobs, unitobs$unite_observation)]
        unit[, fact32] <- unitobs[, fact32][match(unit$unitobs, unitobs$unite_observation)]
        unit[, fact33] <- unitobs[, fact33][match(unit$unitobs, unitobs$unite_observation)]
        assign("unit", unit, envir=.GlobalEnv)

        ## affichage des indices representes sous boxplot dans un message
        tkmessageBox(message=paste("Les indices representes sous boxplot sont :
    - la densite
    - la biomasse
    - la richesse specifique
    - l indice de Simpson
    - l indice de Pielou
    - l indice de Hill"))

        x11(width=12, height=8, pointsize=12)
        par(mar=c(8, 15, 4, 2))
        boxplot(unit$densite ~ unit[, fact31] + unit[, fact32] + unit[, fact33], data=unit, varwidth = TRUE, xlab=expression("Densite "(individus/m^2)), main=paste("Densite par", fact31, fact32, "et", fact33), las=1,
                col=c(rep(cl[seq(400, (400+(4*(length(split(unit, unit[, fact33]))-1))), by=4)], each=length(split(unit, unit[, fact33])), times=length(split(unit, unit[, fact31])))), horizontal = TRUE)
        x1 <- as.vector(tapply(unit$densite, list(unit[, fact31], unit[, fact32], unit[, fact33]), na.rm = T, mean))
        y1 <- 1:length(x1)
        points(x1, y1, pch=19, col="red")

        ## test existence champs biomasse et densite
        if (length(unique(unit$biomasse))>1)
        {
            x11(width=12, height=8, pointsize=12)
            par(mar=c(8, 15, 4, 2))
            boxplot(unit$biomasse ~ unit[, fact31] + unit[, fact32] + unit[, fact33], data=unit, varwidth = TRUE, xlab=expression("Biomasse "(g/m^2)), main=paste("Biomasse par", fact31, fact32, "et", fact33), las=1,
                    col=c(rep(cl[seq(400, (400+(4*(length(split(unit, unit[, fact33]))-1))), by=4)], each=length(split(unit, unit[, fact33])), times=length(split(unit, unit[, fact31])))), horizontal = TRUE)
            x1 <- as.vector(tapply(unit$biomasse, list(unit[, fact31], unit[, fact32], unit[, fact33]), na.rm = T, mean))
            y1 <- 1:length(x1)
            points(x1, y1, pch=19, col="red")
        } # fin test biomasse
        x11(width=12, height=8, pointsize=12)
        par(mar=c(8, 15, 4, 2))
        boxplot(unit$richesse_specifique ~ unit[, fact31] + unit[, fact32] + unit[, fact33], data=unit, varwidth = TRUE, xlab="Nombre d'especes", main=paste("Richesse specifique par", fact31, fact32, "et", fact33), las=1,
                col=c(rep(cl[seq(400, (400+(4*(length(split(unit, unit[, fact33]))-1))), by=4)], each=length(split(unit, unit[, fact33])), times=length(split(unit, unit[, fact31])))), horizontal = TRUE)
        x1 <- as.vector(tapply(unit$richesse_specifique, list(unit[, fact31], unit[, fact32], unit[, fact33]), na.rm = T, mean))
        y1 <- 1:length(x1)
        points(x1, y1, pch=19, col="red")
        x11(width=12, height=8, pointsize=12)
        par(mar=c(8, 15, 4, 2))
        boxplot(unit$sim ~ unit[, fact31] + unit[, fact32] + unit[, fact33], data=unit, varwidth = TRUE, xlab="Simpson", main="Indice de Simpson", las=1,
                col=c(rep(cl[seq(400, (400+(4*(length(split(unit, unit[, fact33]))-1))), by=4)], each=length(split(unit, unit[, fact33])), times=length(split(unit, unit[, fact31])))), horizontal = TRUE)
        x1 <- as.vector(tapply(unit$sim, list(unit[, fact31], unit[, fact32], unit[, fact33]), na.rm = T, mean))
        y1 <- 1:length(x1)
        points(x1, y1, pch=19, col="red")
        x11(width=12, height=8, pointsize=12)
        par(mar=c(8, 15, 4, 2))
        boxplot(unit$pielou ~ unit[, fact31] + unit[, fact32] + unit[, fact33], data=unit, varwidth = TRUE, xlab="Pielou", main="Indice de Pielou", las=1,
                col=c(rep(cl[seq(400, (400+(4*(length(split(unit, unit[, fact33]))-1))), by=4)], each=length(split(unit, unit[, fact33])), times=length(split(unit, unit[, fact31])))), horizontal = TRUE)
        x1 <- as.vector(tapply(unit$pielou, list(unit[, fact31], unit[, fact32], unit[, fact33]), na.rm = T, mean))
        y1 <- 1:length(x1)
        points(x1, y1, pch=19, col="red")
        x11(width=12, height=8, pointsize=12)
        par(mar=c(8, 15, 4, 2))
        boxplot(unit$hill ~ unit[, fact31] + unit[, fact32] + unit[, fact33], data=unit, varwidth = TRUE, xlab="Hill", main="Indice de Hill", las=1,
                col=c(rep(cl[seq(400, (400+(4*(length(split(unit, unit[, fact33]))-1))), by=4)], each=length(split(unit, unit[, fact33])), times=length(split(unit, unit[, fact31])))), horizontal = TRUE)
        x1 <- as.vector(tapply(unit$hill, list(unit[, fact31], unit[, fact32], unit[, fact33]), na.rm = T, mean))
        y1 <- 1:length(x1)
        points(x1, y1, pch=19, col="red")
        x11(width=12, height=8, pointsize=12)
        par(mar=c(8, 15, 4, 2))
        boxplot(unit$Delta ~ unit[, fact31] + unit[, fact32] + unit[, fact33], data=unit, varwidth = TRUE, xlab="Delta", main="Indice de diversite taxonomique", las=1,
                col=c(rep(cl[seq(400, (400+(4*(length(split(unit, unit[, fact33]))-1))), by=4)], each=length(split(unit, unit[, fact33])), times=length(split(unit, unit[, fact31])))), horizontal = TRUE)
        x1 <- as.vector(tapply(unit$Delta, list(unit[, fact31], unit[, fact32], unit[, fact33]), na.rm = T, mean))
        y1 <- 1:length(x1)
        points(x1, y1, pch=19, col="red")
        x11(width=12, height=8, pointsize=12)
        par(mar=c(8, 15, 4, 2))
        boxplot(unit$DeltaEtoile ~ unit[, fact31] + unit[, fact32] + unit[, fact33], data=unit, varwidth = TRUE, xlab="Delta*", main="Indice d'originalite taxonomique", las=1,
                col=c(rep(cl[seq(400, (400+(4*(length(split(unit, unit[, fact33]))-1))), by=4)], each=length(split(unit, unit[, fact33])), times=length(split(unit, unit[, fact31])))), horizontal = TRUE)
        x1 <- as.vector(tapply(unit$DeltaEtoile, list(unit[, fact31], unit[, fact32], unit[, fact33]), na.rm = T, mean))
        y1 <- 1:length(x1)
        points(x1, y1, pch=19, col="red")
        x11(width=12, height=8, pointsize=12)
        par(mar=c(8, 15, 4, 2))
        boxplot(unit$LambdaPlus ~ unit[, fact31] + unit[, fact32] + unit[, fact33], data=unit, varwidth = TRUE, xlab="Lambda+", main="Indice de variation de l'originalite taxonomique", las=1,
                col=c(rep(cl[seq(400, (400+(4*(length(split(unit, unit[, fact33]))-1))), by=4)], each=length(split(unit, unit[, fact33])), times=length(split(unit, unit[, fact31])))), horizontal = TRUE)
        x1 <- as.vector(tapply(unit$LambdaPlus, list(unit[, fact31], unit[, fact32], unit[, fact33]), na.rm = T, mean))
        y1 <- 1:length(x1)
        points(x1, y1, pch=19, col="red")
        x11(width=12, height=8, pointsize=12)
        par(mar=c(8, 15, 4, 2))
        boxplot(unit$DeltaPlus ~ unit[, fact31] + unit[, fact32] + unit[, fact33], data=unit, varwidth = TRUE, xlab="Delta+", main="Indice d'originalite taxonomique sur les presence/absence", las=1,
                col=c(rep(cl[seq(400, (400+(4*(length(split(unit, unit[, fact33]))-1))), by=4)], each=length(split(unit, unit[, fact33])), times=length(split(unit, unit[, fact31])))), horizontal = TRUE)
        x1 <- as.vector(tapply(unit$DeltaPlus, list(unit[, fact31], unit[, fact32], unit[, fact33]), na.rm = T, mean))
        y1 <- 1:length(x1)
        points(x1, y1, pch=19, col="red")
        x11(width=12, height=8, pointsize=12)
        par(mar=c(8, 15, 4, 2))
        boxplot(unit$SDeltaPlus ~ unit[, fact31] + unit[, fact32] + unit[, fact33], data=unit, varwidth = TRUE, xlab="RS Delta+", main="RS*Delta+", las=1,
                col=c(rep(cl[seq(400, (400+(4*(length(split(unit, unit[, fact33]))-1))), by=4)], each=length(split(unit, unit[, fact33])), times=length(split(unit, unit[, fact31])))), horizontal = TRUE)
        x1 <- as.vector(tapply(unit$SDeltaPlus, list(unit[, fact31], unit[, fact32], unit[, fact33]), na.rm = T, mean))
        y1 <- 1:length(x1)
        points(x1, y1, pch=19, col="red")
    } # fin doneVal
} #fin graph3.f

################################################################################
## Nom    : graphCT1.f()
## Objet  : affichage des graphiques par groupe d'unités d'observation
##          et classes de taille
## Input  : tables "unitobs", "unitesptat" + facteur fact
## Output : graphiques
################################################################################

graphCT1.f <- function (fact)
{

    print("fonction graphCT1.f activée")
    AffichageGraphiques.f()

    if (doneVal==1)
    {
        unitesptat[, fact] <- unitobs[, fact][match(unitesptat$unitobs, unitobs$unite_observation)]
        assign("unitesptat", unitesptat, envir=.GlobalEnv)
        if (unique(unitobs$type) != "LIT")
        {
            ## là on fait des boxplot donc sur l'ensemble des valeurs, on passe donc par la table "unit" et pas "grp"
            ## affichage des indices representes sous boxplot dans un message
            tkmessageBox(message=paste("Les indices representes sous boxplot sont : \n- la densite \n- la biomasse \n- la taille moyenne"))

            ## test existence champs biomasse et densite
            if (NA %in% unique(unit$biomasse) == FALSE) # [!!!] Argh, c'est quoi ça ??? [yr: 30/07/2010]
            {
                X11()
                boxplot(unitesptat$densite ~ unitesptat[, fact], data=unitesptat, varwidth = TRUE, ylab=expression("Densite "(individus/m^2)), main=paste("Densite de", TailleChoisie, "par", fact))
                bx1 <- as.vector(tapply(unitesptat$densite, unitesptat[, fact], na.rm = T, mean))
                points(bx1, pch=19, col="red")

                X11()
                boxplot(unitesptat$biomasse ~ unitesptat[, fact], data=unitesptat, varwidth = TRUE, ylab=expression("Biomasse "(g/m^2)), main=paste("Biomasse de", TailleChoisie, "par", fact))
                bx1 <- as.vector(tapply(unitesptat$biomasse, unitesptat[, fact], na.rm = T, mean))
                points(bx1, pch=19, col="red")
            }

            if (ct == 1)
            {
                X11()
                boxplot(unitesptat$taille_moyenne ~ unitesptat[, fact], data=unitesptat, varwidth = TRUE, ylab="Taille moyenne", main=paste("Taille moyenne de", TailleChoisie, "par", fact))
                bx1 <- as.vector(tapply(unitesptat$taille_moyenne, unitesptat[, fact], na.rm = T, mean))
                points(bx1, pch=19, col="red")
            }
        }
        if (unique(unitobs$type) == "LIT")
        {
            tkmessageBox(message="Ces metriques ne peuvent pas être calculees pour les suivis LIT")
        }
    } # fin DoneVal
} # fin graphCT1.f

################################################################################
## Nom    : graphCT2.f()
## Objet  : affichage des graphiques par groupe d'unités d'observation
##          et classes de taille
## Input  : tables "unitobs", "unitesptat" + facteurs fact21 et fact22
## Output : graphiques
################################################################################

graphCT2.f <- function (fact21, fact22)
{

    print("fonction graphCT2.f activée")
    AffichageGraphiques.f()

    ## si volonte d'afficher les graphs
    if (doneVal==1)
    {
        if (unique(unitobs$type) != "LIT")
        {
            cl <- colors()
            tkmessageBox(message=paste("Les indices representes sous boxplot sont : \n- la densite, \n- la biomasse \n- la taille moyenne"))

            ## test existence champs biomasse et densite
            if (NA %in% unique(unit$biomasse) == FALSE)
            { # [!!!] devrait pouvoir être remplacé par all(!is.na(...)) [yr: 30/07/2010]
                x11(width=12, height=8, pointsize=12)
                par(mar=c(8, 15, 4, 1))
                interaction.plot(grpCT2[, fact21], grpCT2[, fact22], grpCT2$densite, lwd=2, col=cl[seq(550, (550+(4*(length(split(grpCT2, grpCT2[, fact22]))-1))), by=4)],
                                 type="b", fun=mean, trace.label = fact22, xlab="Annee", ylab="densite", main=paste("Densite moyenne de", TailleChoisie, "par", fact21, "et", fact22))

                ## là on fait des boxplot donc sur l'ensemble des valeurs, on passe donc par la table "unitesptat" et pas "grp12"
                x11(width=12, height=8, pointsize=12)
                par(mar=c(8, 15, 4, 2), mgp=c(5, 1, 0))
                boxplot(unitesptat$densite ~ unitesptat[, fact21] + unitesptat[, fact22], data=unitesptat, varwidth = TRUE, xlab=expression("Densite "(individus/m^2)), main=paste("Densite totale de", TailleChoisie, "par", fact21, "et", fact22), las=1,
                        col=c(rep(cl[seq(400, (400+(4*(length(split(unitesptat, unitesptat[, fact22]))-1))), by=4)], each=length(split(unitesptat, unitesptat[, fact21])))), horizontal = TRUE)
                x1 <- as.vector(tapply(unitesptat$densite, list(unitesptat[, fact21], unitesptat[, fact22]), na.rm = T, mean))
                y1 <- 1:length(x1)
                points(x1, y1, pch=19, col="red")

                x11(width=12, height=8, pointsize=12)
                par(mar=c(8, 15, 4, 2))
                boxplot(unitesptat$biomasse ~ unitesptat[, fact21] + unitesptat[, fact22], data=unitesptat, varwidth = TRUE, xlab=expression("Biomasse "(g/m^2)), main=paste("Biomasse de", TailleChoisie, "par", fact21, "et", fact22), las=1,
                        col=c(rep(cl[seq(400, (400+(4*(length(split(unitesptat, unitesptat[, fact22]))-1))), by=4)], each=length(split(unitesptat, unitesptat[, fact21])))), horizontal = TRUE)
                bx1 <- as.vector(tapply(unitesptat$biomasse, list(unitesptat[, fact21], unitesptat[, fact22]), na.rm = T, mean))
                points(bx1, pch=19, col="red")
            }

            x11(width=12, height=8, pointsize=12)
            par(mar=c(5, 6, 4, 1))
            interaction.plot(grpCT2[, fact21], grpCT2[, fact22], grpCT2$richesse_specifique, lwd=2, col=cl[seq(550, (550+(4*(length(split(grpCT2, grpCT2[, fact22]))-1))), by=4)], type="b",
                             fun=mean, trace.label = fact22, xlab="Annee", ylab="Nombre d'especes", main=paste("Richesse specifique de", TailleChoisie, "par", fact21, "et", fact22))

            if (ct == 1)
            {
                x11(width=12, height=8, pointsize=12)
                par(mar=c(8, 15, 4, 2))
                boxplot(unitesptat$taille_moyenne ~ unitesptat[, fact21] + unitesptat[, fact22], data=unitesptat, varwidth = TRUE, xlab="Nombre d'especes", main=paste("Taille moyenne de", TailleChoisie, "par", fact21, "et", fact22), las=1,
                        col=c(rep(cl[seq(400, (400+(4*(length(split(unitesptat, unitesptat[, fact22]))-1))), by=4)], each=length(split(unitesptat, unitesptat[, fact21])))), horizontal = TRUE)
                x1 <- as.vector(tapply(unitesptat$taille_moyenne, list(unitesptat[, fact21], unitesptat[, fact22]), na.rm = T, mean))
                y1 <- 1:length(x1)
                points(x1, y1, pch=19, col="red")
            }
        }

        if (unique(unitobs$type) == "LIT")
        {
            tkmessageBox(message="Ces metriques ne peuvent pas être calculees pour les suivis LIT")
        }
    } # fin doneVal
} # fin graphCT2.f

################################################################################
## Nom    : graphCT3.f()
## Objet  : affichage des graphiques par groupe d'unités d'observation
##          et classes de taille
## Input  : tables "unitobs", "unitesptat" + facteurs fact31, fact32 et fact33
## Output : graphiques
################################################################################

graphCT3.f <- function (fact31, fact32, fact33)
{

    print("fonction graphCT3.f activée")
    AffichageGraphiques.f()

    if (doneVal==1)
    {
        if (unique(unitobs$type) != "LIT")
        {

            ## affichage des indices representes sous boxplot dans un message
            tkmessageBox(message=paste("Les indices representes sous boxplot sont : \n- la densite, \n- la biomasse \n- la taille moyenne"))

            ## là on fait des boxplot donc sur l'ensemble des valeurs, on passe donc par la table "unit" et pas "grpCT3"
            cl <- colors()
            X11()
            par(mar=c(10, 6, 4, 2))
            boxplot(unitesptat$densite ~ unitesptat[, fact31] + unitesptat[, fact32] + unitesptat[, fact33],
                    data=unitesptat, varwidth = TRUE, ylab=expression("Densite "(individus/m^2)),
                    main=paste("Densite de", TailleChoisie, "par", fact31, "et", fact32, "et", fact33), las=3,
                    col=c(rep(cl[seq(400, (400+(4*(length(split(unitesptat, unitesptat[, fact33]))-1))), by=4)],
                    each=length(split(unitesptat, unitesptat[, fact33])), times=length(split(unitesptat, unitesptat[,
                                                                          fact31])))))
            bx1 <- as.vector(tapply(unitesptat$densite,
                                    list(unitesptat[, fact31], unitesptat[, fact32],
                                         unitesptat[, fact33]), na.rm = T, mean))
            points(bx1, pch=19, col="red")
            X11()
            par(mar=c(10, 6, 4, 2))
            boxplot(unitesptat$biomasse ~ unitesptat[, fact31] + unitesptat[, fact32] + unitesptat[, fact33],
                    data=unitesptat, varwidth = TRUE, ylab=expression("Biomasse "(g/m^2)), main=paste("Biomasse de",
                    TailleChoisie, "par", fact31, "et", fact32, "et", fact33), las=3, col=c(rep(cl[seq(400,
                    (400+(4*(length(split(unitesptat, unitesptat[, fact33]))-1))), by=4)], each=length(split(unitesptat,
                    unitesptat[, fact33])), times=length(split(unitesptat, unitesptat[, fact31])))))
            bx1 <- as.vector(tapply(unitesptat$biomasse,
                                    list(unitesptat[, fact31], unitesptat[, fact32],
                                         unitesptat[, fact33]), na.rm = T, mean))
            points(bx1, pch=19, col="red")
            if (ct == 1)
            { # dans le cas où l'on a que les categories de taille et pas les mesures.
                X11()
                par(mar=c(10, 6, 4, 2))
                boxplot(unitesptat$taille_moyenne ~ unitesptat[, fact31] + unitesptat[, fact32] + unitesptat[, fact33],
                        data=unitesptat, varwidth = TRUE, ylab="Taille moyenne", main=paste("Taille moyenne de",
                        TailleChoisie, "par", fact31, "et", fact32, "et", fact33), las=3, col=c(rep(cl[seq(400,
                        (400+(4*(length(split(unitesptat, unitesptat[, fact33]))-1))), by=4)],
                        each=length(split(unitesptat, unitesptat[, fact33])), times=length(split(unitesptat,
                        unitesptat[, fact31])))))
                bx1 <- as.vector(tapply(unitesptat$taille_moyenne,
                                        list(unitesptat[, fact31], unitesptat[, fact32],
                                             unitesptat[, fact33]), na.rm = T, mean))
                points(bx1, pch=19, col="red")
            }
            rm(fact31, fact32, fact33, envir=.GlobalEnv)
        }
        if (unique(unitobs$type) == "LIT")
        {
            tkmessageBox(message="Ces metriques ne peuvent pas être calculees pour les suivis LIT")
        }
    }
} # fin graphCT3.f

################################################################################
## FONCTIONS DE CALCUL DE METRIQUES PAR GROUPE D'UNITE D'OBSERVATION
##                                  SUR UNE ESPECE
##     - choixunfacteurCT.f(), choixdeuxfacteursCT.f(), choixtroisfacteursCT.f() A CHANGER
##     - gra1.f(), gra2.f(), gra3.f()
################################################################################

################################################################################
## Nom    : gra1.f
## Objet  : créer les boxplot sur l'espèce sélectionnée
## Input  : table "listespunit" + espèce sp + 1 facteur fact
## Output : boxplot densite et biomasse
################################################################################

## ! nom de fonction trop générique et peu parlant, valable pour la suite
## ! s'assurer que les requêtages ne soient pas présents dans ce fichier

gra1.f <- function (fact)
{

    print("fonction gra1.f activée")
    ## on restreint la table "listespunit" à l'espèce sélectionnée
    spunit <- subset(listespunit, listespunit$code_espece==sp)
    spunit[, fact] <- unitobs[, fact][match(spunit$unite_observation, unitobs$unite_observation)]

    ## boxplot de la densite
    X11()
    if (length(typePeche)>1)
    {
        boxplot(spunit$densite ~ spunit[, fact], data=spunit, varwidth = TRUE, ylab="CPUE",
                main=paste("CPUE de ", sp, sep=""), las=3)
    }else{
        boxplot(spunit$densite ~ spunit[, fact], data=spunit, varwidth = TRUE,
                ylab=expression("Densite "(individus/m^2)), main=paste("Densite de ", sp, sep=""), las=3)
    }
    ## la biomasse n'est pas calculée sur tous les jeux de données
    if (length(unique(listespunit$biomasse))>1)
    {
        ## on vérifie que la biomasse a été calculée pour l'espèce sélectionnée
        if (length(unique(spunit$biomasse))>1)
        {
            X11()
            boxplot(spunit$biomasse ~ spunit[, fact], data=spunit, varwidth = TRUE, ylab=expression("Biomasse "(g/m^2)),
                    main=paste("Biomasse de ", sp, sep=""), las=3)
        }else{
            tkmessageBox(message="Calcul de biomasse impossible - Coefficients a et b manquants dans le referentiel especes")
        }
    }
} # fin gra1.f

################################################################################
## Nom    : gra2.f
## Objet  : créer les boxplot sur l'espèce sélectionnée
## Input  : table "listespunit" + espèce sp + facteurs fact21 et fact22
## Output : boxplot densite et biomasse
################################################################################

gra2.f <- function (fact21, fact22)
{
    print("fonction gra2.f activée")
    ## on restreint la table "listespunit" à l'espèce sélectionnée
    spunit <- subset(listespunit, listespunit$code_espece==sp)
    spunit[, fact21] <- unitobs[, fact21][match(spunit$unite_observation, unitobs$unite_observation)]
    spunit[, fact22] <- unitobs[, fact22][match(spunit$unite_observation, unitobs$unite_observation)]

    ## boxplot de la densite
    x11(width=12, height=8, pointsize=12)
    par(mar=c(8, 15, 4, 2))
    if (length(typePeche)>1)
    {
        boxplot(spunit$densite ~ spunit[, fact21] + spunit[, fact22], data=spunit, varwidth = TRUE, xlab="CPUE",
                main=paste("CPUE de ", sp, sep=""), las=1, col=c(rep(cl[seq(400, (400+(4*(length(split(spunit, spunit[,
                fact22]))-1))), by=4)], each=length(split(spunit, spunit[, fact21])))), horizontal = TRUE)
    }else{
        boxplot(spunit$densite ~ spunit[, fact21] + spunit[, fact22], data=spunit, varwidth = TRUE,
                xlab=expression("Densite "(individus/m^2)), main=paste("Densite de ", sp, sep=""), las=1,
                col=c(rep(cl[seq(400, (400+(4*(length(split(spunit, spunit[, fact22]))-1))), by=4)],
                each=length(split(spunit, spunit[, fact21])))), horizontal = TRUE)
    }

    ## la biomasse n'est pas calculée sur tous les jeux de données
    if (length(unique(listespunit$biomasse))>1)
    {
        ## on vérifie que la biomasse a été calculée pour l'espèce sélectionnée
        if (length(unique(spunit$biomasse))>1)
        {
            x11(width=12, height=8, pointsize=12)
            par(mar=c(8, 15, 4, 2))
            boxplot(spunit$biomasse ~ spunit[, fact21] + spunit[, fact22], data=spunit, varwidth = TRUE,
                    xlab=expression("Biomasse "(g/m^2)), main=paste("Biomasse de ", sp, sep=""), las=1,
                    col=c(rep(cl[seq(400, (400+(4*(length(split(spunit, spunit[, fact22]))-1))), by=4)],
                    each=length(split(spunit, spunit[, fact21])))), horizontal = TRUE)
        }else{
            tkmessageBox(message="Calcul de biomasse impossible - Coefficients a et b manquants dans le referentiel especes")
        }
    }
} # fin gra2.f

################################################################################
## Nom    : gra3.f
## Objet  : créer les boxplot sur l'espèce sélectionnée
## Input  : table "listespunit" + espèce sp + facteurs fact21, fact22 et fact23
## Output : boxplot densite et biomasse
################################################################################

gra3.f <- function (fact31, fact32, fact33)
{
    print("fonction gra3.f activée")
    ## on restreint la table "listespunit" à l'espèce sélectionnée
    spunit <- subset(listespunit, listespunit$code_espece==sp)
    spunit[, fact31] <- unitobs[, fact31][match(spunit$unite_observation, unitobs$unite_observation)]
    spunit[, fact32] <- unitobs[, fact32][match(spunit$unite_observation, unitobs$unite_observation)]
    spunit[, fact33] <- unitobs[, fact33][match(spunit$unite_observation, unitobs$unite_observation)]

    ## boxplot de la densite ou CPUE
    x11(width=12, height=8, pointsize=12)
    par(mar=c(8, 15, 4, 2))
    if (length(typePeche)>1)
    {
        boxplot(spunit$densite ~ spunit[, fact31] + spunit[, fact32] + spunit[, fact33], data=spunit, varwidth = TRUE,
                xlab="CPUE", main=paste("CPUE de ", sp, sep=""), las=1,
                col=c(rep(cl[seq(400, (400+(4*(length(split(spunit, spunit[, fact33]))-1))), by=4)],
                each=length(split(spunit, spunit[, fact33])), times=length(split(spunit, spunit[, fact31])))),
                horizontal = TRUE)
    }else{
        boxplot(spunit$densite ~ spunit[, fact31] + spunit[, fact32] + spunit[, fact33], data=spunit, varwidth = TRUE,
                xlab=expression("Densite "(individus/m^2)), main=paste("Densite de ", sp, sep=""), las=1,
                col=c(rep(cl[seq(400, (400+(4*(length(split(spunit, spunit[, fact33]))-1))), by=4)],
                each=length(split(spunit, spunit[, fact33])), times=length(split(spunit, spunit[, fact31])))),
                horizontal = TRUE)
    }

    ## la biomasse n'est pas calculée sur tous les jeux de données
    if (length(unique(listespunit$biomasse))>1)
    {
        ## on vérifie que la biomasse a été calculée pour l'espèce sélectionnée
        if (length(unique(spunit$biomasse))>1)
        {
            x11(width=12, height=8, pointsize=12)
            par(mar=c(8, 15, 4, 2))
            boxplot(spunit$biomasse ~ spunit[, fact31] + spunit[, fact32] + spunit[, fact33], data=spunit, varwidth =
                    TRUE, xlab=expression("Biomasse "(g/m^2)), main=paste("Biomasse de ", sp, sep=""), las=1,
                    col=c(rep(cl[seq(400, (400+(4*(length(split(spunit, spunit[, fact33]))-1))), by=4)],
                    each=length(split(spunit, spunit[, fact33])), times=length(split(spunit, spunit[, fact31])))),
                    horizontal = TRUE)
        }else{
            tkmessageBox(message="Calcul de biomasse impossible - Coefficients a et b manquants dans le referentiel especes")
        }
    }
} # fin gra3.f

################################################################################
## Nom     : graphIndicesDiv.f
## Objet   : graphiques d'indices de diversité taxonomique
## Input   : taxdis et div
## Output  : - dendrogramme des relations taxonomiques entre especes,
##           - graphique de l'indice Delta+ en fonction du nombre d'especes
################################################################################

graphIndicesDiv.f <- function(){
    print("fonction graphIndicesDiv.f activée")
    x11(width=15, height=8, pointsize=12)
    plot(hclust(taxdis), labels = NULL, hang = -1, xlab = "Especes", main="Distances taxonomiques entre especes")
    X11()
    plot(div, main="Delta +")
}
