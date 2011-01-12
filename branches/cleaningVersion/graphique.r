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

## [sup] [yr:12/01/2011]:

## UneFamilleDansPDF.f <- function ()
## {
##     print("fonction UneFamilleDansPDF.f activée")

##     ChoixOptionsGraphiques.f()
##     listespunit$Famille <- especes$Famille[match(listespunit$code_espece, especes$code_espece)]
##     FamillesPresentes <- subset(listespunit$Famille, listespunit$pres_abs==1)
##     ChoixFacteurSelect.f(FamillesPresentes, "Famille", "multiple", 1, "selectPDFFamille")

##     print(choixgraph)
##     print(selectPDFFamille)

##     MetriqueUneFamilleDansPDF.f <- function (metrique, MaFamille)
##     {
##         textstatut <- c("HR : Hors Réserve", "PP : Protection Partielle", "RE : En réserve")
##         Nbdecimales <- 2
##         monspunit <- subset(listespunit, listespunit$Famille==MaFamille)
##         spunit <- monspunit
##         print(paste("graphique pour", MaFamille, " et la métrique", metrique, ": ", length(spunit[, metrique]), "observations"))
##         if (choixgraph["maxExclu"] == 1)   # 1
##         {
##             spunit[, metrique][which(spunit[, metrique]>GraphPartMax*max(spunit[, metrique], na.rm=T))] <- NA
##         }

##         if (length(unique(listespunit[, metrique]))>1)
##         {
##             ## on vérifie que la metrique a été calculée pour l'espèce sélectionnée
##             if (length(unique(spunit[, metrique]))>=choixgraph["MinNbObs"])
##             {

##                 ##  #################### graphique par année ####################
##                 if (choixgraph["graphAn"] == 1)   # 2
##                 {
##                     par(mar=c(9, 4, 5, 1), mgp=c(3, 1, 9))
##                     boxplot(spunit[, metrique] ~spunit$an, data=spunit, varwidth = TRUE, ylab=metrique, las=2,
##                             main=paste(metrique, "\n pour la famille : ", unique(spunit$Famille), " selon l'année \n"))

##                     if (choixgraph["NbObsOrange"] == 1)  # 9
##                     {
##                         nbObs <- tapply(spunit[, metrique], spunit$an, length)
##                         axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)), col.ticks="orange",
##                              col.axis = "orange", lty = 2, lwd = 0.5)
##                         legend("topleft", "Nombre d'enregistrement par Boxplot", cex =0.7, col="orange",
##                                text.col="orange", merge=FALSE)
##                     }
##                     Moyenne <- as.vector(tapply(spunit[, metrique], spunit$an, mean))
##                     if (choixgraph["PtMoyenneBleu"] == 1)  # 10
##                     {
##                         points(Moyenne, pch=19, col="blue")
##                     }
##                     if (choixgraph["ChiffreMoyenneBleu"] == 1)  # 11
##                     {
##                         text(Moyenne+(Moyenne/10), col = "blue", cex = 0.8, labels=as.character(round(Moyenne, digits=Nbdecimales)))
##                     }
##                     abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")
##                     if (choixgraph["maxExclu"] == 1)   # 1
##                     {
##                         legend("top", "Enregistrements > 95% du maximum retirés", cex =0.7, col="red", text.col="red", merge=FALSE)
##                     }
##                 }
##                 ##  #################### graphique par AN et PAR BIOTOPE ####################
##                 if (choixgraph["graphAnBiotope"] == 1)   # 3
##                 {
##                     par(mar=c(15, 4, 5, 1), mgp=c(3, 1, 9))
##                     boxplot(spunit[, metrique] ~spunit$biotope+spunit$an, data=spunit,
##                             varwidth = TRUE, ylab=metrique, las=2, col=heat.colors(length(unique(spunit$statut_protection))),
##                             main=paste(metrique, "\n pour la famille : ", unique(spunit$Famille), "\n selon le biotope et l'année"))
##                     if (choixgraph["NbObsOrange"] == 1)  # 9
##                     {
##                         nbObs <- tapply(spunit[, metrique], list(spunit$biotope, spunit$an), length)
##                         axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)), col.ticks="orange",
##                              col.axis = "orange", lty = 2, lwd = 0.5)
##                         legend("topleft", "Nombre d'enregistrement par Boxplot", cex =0.7, col="orange",
##                                text.col="orange", merge=FALSE)
##                     }
##                     Moyenne <- as.vector(tapply(spunit[, metrique], list(spunit$biotope, spunit$an), mean))
##                     if (choixgraph["PtMoyenneBleu"] == 1)  # 10
##                     {
##                         points(Moyenne, pch=19, col="blue")
##                     }
##                     if (choixgraph["ChiffreMoyenneBleu"] == 1)  # 11
##                     {
##                         text(Moyenne+(Moyenne/10), col = "blue", cex = 0.8, labels=as.character(round(Moyenne, digits=Nbdecimales)))
##                     }
##                     abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")
##                     if (choixgraph["maxExclu"] == 1)   # 1
##                     {
##                         legend("top", "Enregistrements > 95% du maximum retirés", cex =0.7, col="red", text.col="red", merge=FALSE)
##                     }
##                 }
##                 ##  #################### graphique par AN et PAR STATUT ####################
##                 if (choixgraph["graphAnStatut"] == 1)   # 4
##                 {

##                     par(mar=c(9, 4, 5, 1), mgp=c(3, 1, 9))
##                     boxplot(spunit[, metrique] ~spunit$statut_protection+spunit$an, data=spunit,
##                             varwidth = TRUE, ylab=metrique, las=2, col=heat.colors(length(unique(spunit$statut_protection))),
##                             main=paste(metrique, " pour la famille : ", unique(spunit$Famille), "\n selon le statut et l'année\n"))
##                     if (length(unique(spunit$statut_protection))==3)
##                     {
##                         legend("topright", textstatut, col=heat.colors(length(unique(spunit$statut_protection))),
##                                pch = 15, cex =0.9, title="Statuts")
##                     }
##                     if (choixgraph["NbObsOrange"] == 1)  # 9
##                     {
##                         nbObs <- tapply(spunit[, metrique], list(spunit$statut_protection, spunit$an), length)
##                         axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)), col.ticks="orange",
##                              col.axis = "orange", lty = 2, lwd = 0.5)
##                         legend("topleft", "Nombre d'enregistrement par Boxplot", cex =0.7, col="orange",
##                                text.col="orange", merge=FALSE)
##                     }
##                     Moyenne <- as.vector(tapply(spunit[, metrique], list(spunit$statut_protection, spunit$an), na.rm = T, mean))
##                     if (choixgraph["PtMoyenneBleu"] == 1)  # 10
##                     {
##                         points(Moyenne, pch=19, col="blue")
##                     }
##                     if (choixgraph["ChiffreMoyenneBleu"] == 1)  # 11
##                     {
##                         text(Moyenne+(Moyenne/10), col = "blue", cex = 0.8, labels=as.character(round(Moyenne, digits=Nbdecimales)))
##                     }
##                     abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")
##                     if (choixgraph["maxExclu"] == 1)   # 1
##                     {
##                         legend("top", "Enregistrements > 95% du maximum retirés", cex =0.7, col="red", text.col="red", merge=FALSE)
##                     }
##                 }
##                 ##  #################### graphique par STATUT, PAR AN et par BIOTOPE  ####################
##                 if (choixgraph["graphAnBiotopeStatut"] == 1)   # 5
##                 {
##                     par(mar=c(9, 4, 5, 1), mgp=c(3, 1, 9))

##                     boxplot(spunit[, metrique] ~spunit$statut_protection+spunit$an+spunit$biotope, data=spunit,
##                             varwidth = TRUE, ylab=metrique, las=2, col=heat.colors(length(unique(spunit$statut_protection))),
##                             main=paste(metrique, " pour la famille : ", unique(spunit$Famille), "\n selon le statut, l'année et le biotope\n"))
##                     if (length(unique(spunit$statut_protection))==3)
##                     {
##                         legend("topright", textstatut, col=heat.colors(length(unique(spunit$statut_protection))),
##                                pch = 15, cex =0.9, title="Statuts")
##                     }
##                     if (choixgraph["NbObsOrange"] == 1)  # 9
##                     {
##                         nbObs <- tapply(spunit[, metrique], list(spunit$statut_protection, spunit$an, spunit$biotope), length)
##                         axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)), col.ticks="orange", col.axis = "orange", lty = 2, lwd = 0.5)
##                         legend("topleft", "Nombre d'enregistrement par Boxplot", cex =0.7, col="orange", text.col="orange", merge=FALSE)
##                     }
##                     Moyenne <- as.vector(tapply(spunit[, metrique], list(spunit$statut_protection, spunit$an, spunit$biotope), na.rm = T, mean))
##                     if (choixgraph["PtMoyenneBleu"] == 1)  # 10
##                     {
##                         points(Moyenne, pch=19, col="blue")
##                     }
##                     if (choixgraph["ChiffreMoyenneBleu"] == 1)  # 11
##                     {
##                         text(Moyenne+(Moyenne/10), col = "blue", cex = 0.8, labels=as.character(round(Moyenne, digits=Nbdecimales)))
##                     }
##                     abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")
##                     if (choixgraph["maxExclu"] == 1)   # 1
##                     {
##                         legend("top", "Enregistrements > 95% du maximum retirés", cex =0.7, col="red", text.col="red", merge=FALSE)
##                     }
##                 }
##                 ##  #################### graphique par BIOTOPE  ####################
##                 if (choixgraph["graphBiotope"] == 1)   # 6
##                 {
##                     par(mar=c(9, 4, 5, 1), mgp=c(3, 1, 9))
##                     boxplot(spunit[, metrique] ~spunit$biotope, data=spunit, varwidth = TRUE, ylab=metrique, las=2,
##                             main=paste(metrique, "\n pour la famille : ", unique(spunit$Famille),
##                             " selon le biotope \n"))

##                     if (choixgraph["NbObsOrange"] == 1)  # 9
##                     {
##                         nbObs <- tapply(spunit[, metrique], spunit$biotope, length)
##                         axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)), col.ticks="orange", col.axis = "orange", lty = 2, lwd = 0.5)
##                         legend("topleft", "Nombre d'enregistrement par Boxplot", cex =0.7, col="orange", text.col="orange", merge=FALSE)
##                     }
##                     Moyenne <- as.vector(tapply(spunit[, metrique], spunit$biotope, na.rm = T, mean))
##                     if (choixgraph["PtMoyenneBleu"] == 1)  # 10
##                     {
##                         points(Moyenne, pch=19, col="blue")
##                     }
##                     if (choixgraph["ChiffreMoyenneBleu"] == 1)  # 11
##                     {
##                         text(Moyenne+(Moyenne/10), col = "blue", cex = 0.8, labels=as.character(round(Moyenne, digits=Nbdecimales)))
##                     }
##                     abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")
##                     if (choixgraph["maxExclu"] == 1)   # 1
##                     {
##                         legend("top", "Enregistrements > 95% du maximum retirés", cex =0.7, col="red", text.col="red", merge=FALSE)
##                     }
##                 }
##                 ##  #################### graphique par STATUT   ####################
##                 if (choixgraph["graphStatut"] == 1)   # 7
##                 {
##                     par(mar=c(9, 4, 5, 1), mgp=c(3, 1, 9))
##                     ## boxplot(spunit[, metrique] ~spunit$Famille, data=spunit, varwidth = TRUE, ylab=metrique, las=2,
##                     ## main=paste(metrique, " \n pour la famille : ", spunit$Famille, "\npour toutes les unités d'obs"))

##                     boxplot(spunit[, metrique] ~spunit$statut_protection, data=spunit, varwidth = TRUE, ylab=metrique,
##                             las=2, main=paste(metrique, "\n pour la famille : ", unique(spunit$Famille), "selon le statut \n"))
##                     if (length(unique(spunit$statut_protection))==3)
##                     {
##                         legend("topright", textstatut, col=heat.colors(length(unique(spunit$statut_protection))),
##                                pch = 15, cex =0.9, title="Statuts")
##                     }
##                     if (choixgraph["NbObsOrange"] == 1)  # 9
##                     {
##                         nbObs <- tapply(spunit[, metrique], spunit$statut_protection, length)
##                         axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)), col.ticks="orange",
##                              col.axis = "orange", lty = 2, lwd = 0.5)
##                         legend("topleft", "Nombre d'enregistrement par Boxplot", cex =0.7, col="orange",
##                                text.col="orange", merge=FALSE)
##                     }
##                     Moyenne <- as.vector(tapply(spunit[, metrique], spunit$statut_protection, na.rm = T, mean))
##                     if (choixgraph["PtMoyenneBleu"] == 1)  # 10
##                     {
##                         points(Moyenne, pch=19, col="blue")
##                     }
##                     if (choixgraph["ChiffreMoyenneBleu"] == 1)  # 11
##                     {
##                         text(Moyenne+(Moyenne/10), col = "blue", cex = 0.8, labels=as.character(round(Moyenne, digits=Nbdecimales)))
##                     }
##                     abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")
##                     if (choixgraph["maxExclu"] == 1)   # 1
##                     {
##                         legend("top", "Enregistrements > 95% du maximum retirés", cex =0.7, col="red", text.col="red", merge=FALSE)
##                     }
##                 }
##                 ##  #################### graphique par STATUT et PAR BIOTOPE ####################
##                 if (choixgraph["graphBiotopeStatut"] == 1)  # 8
##                 {
##                     par(mar=c(9, 4, 5, 1), mgp=c(3, 1, 9))
##                     boxplot(spunit[, metrique] ~spunit$statut_protection+spunit$biotope, data=spunit,
##                             varwidth = TRUE, ylab=metrique, las=2, col=heat.colors(length(unique(spunit$statut_protection))),
##                             main=paste(metrique, " pour la famille : ", unique(spunit$Famille), "\n selon le statut et le biotope\n"))
##                     if (length(unique(spunit$statut_protection))==3)
##                     {
##                         legend("topright", textstatut, col=heat.colors(length(unique(spunit$statut_protection))),
##                                pch = 15, cex =0.9, title="Statuts")
##                     }
##                     if (choixgraph["NbObsOrange"] == 1)  # 9
##                     {
##                         nbObs <- tapply(spunit[, metrique], list(spunit$statut_protection, spunit$biotope), length)
##                         axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)), col.ticks="orange", col.axis = "orange", lty = 2, lwd = 0.5)
##                         legend("topleft", "Nombre d'enregistrement par Boxplot", cex =0.7, col="orange", text.col="orange", merge=FALSE)
##                     }
##                     Moyenne <- as.vector(tapply(spunit[, metrique], list(spunit$statut_protection, spunit$biotope), na.rm = T, mean))
##                     if (choixgraph["PtMoyenneBleu"] == 1)  # 10
##                     {
##                         points(Moyenne, pch=19, col="blue")
##                     }
##                     if (choixgraph["ChiffreMoyenneBleu"] == 1)  # 11
##                     {
##                         text(Moyenne+(Moyenne/10), col = "blue", cex = 0.8, labels=as.character(round(Moyenne, digits=Nbdecimales)))
##                     }
##                     abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")
##                     if (choixgraph["maxExclu"] == 1)   # 1
##                     {
##                         legend("top", "Enregistrements > 95% du maximum retirés", cex =0.7, col="red", text.col="red", merge=FALSE)
##                     }
##                 }
##                 ##  #################### graphique par SITE ####################
##                 if (choixgraph["graphSite"] == 1)  # 13
##                 {
##                     par(mar=c(9, 4, 5, 1), mgp=c(3, 1, 9))
##                     boxplot(spunit[, metrique] ~spunit$site, data=spunit, varwidth = TRUE, ylab=metrique, las=2,
##                             main=paste(metrique, "\n pour la famille : ", unique(spunit$Famille), " selon le site \n"))

##                     if (choixgraph["NbObsOrange"] == 1)  # 9
##                     {
##                         nbObs <- tapply(spunit[, metrique], spunit$site, length)
##                         axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)), col.ticks="orange", col.axis = "orange", lty = 2, lwd = 0.5)
##                         legend("topleft", "Nombre d'enregistrement par Boxplot", cex =0.7, col="orange", text.col="orange", merge=FALSE)
##                     }
##                     Moyenne <- as.vector(tapply(spunit[, metrique], spunit$site, mean))
##                     if (choixgraph["PtMoyenneBleu"] == 1)  # 10
##                     {
##                         points(Moyenne, pch=19, col="blue")
##                     }
##                     if (choixgraph["ChiffreMoyenneBleu"] == 1)  # 11
##                     {
##                         text(Moyenne+(Moyenne/10), col = "blue", cex = 0.8, labels=as.character(round(Moyenne, digits=Nbdecimales)))
##                     }
##                     abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")
##                     if (choixgraph["maxExclu"] == 1)   # 1
##                     {
##                         legend("top", "Enregistrements > 95% du maximum retirés", cex =0.7, col="red", text.col="red", merge=FALSE)
##                     }
##                 }
##                 ##  #################### graphique par AN et PAR SITE ####################
##                 if (choixgraph["graphAnSite"] == 1)  # 14
##                 {
##                     par(mar=c(15, 4, 5, 1), mgp=c(3, 1, 9))
##                     boxplot(spunit[, metrique] ~spunit$site+spunit$an, data=spunit,
##                             varwidth = TRUE, ylab=metrique, las=2, col=heat.colors(length(unique(spunit$site))),
##                             main=paste(metrique, "\n pour la famille : ", unique(spunit$Famille), "\n selon le site et l'année"))
##                     if (choixgraph["NbObsOrange"] == 1)  # 9
##                     {
##                         nbObs <- tapply(spunit[, metrique], list(spunit$site, spunit$an), length)
##                         axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)), col.ticks="orange", col.axis = "orange", lty = 2, lwd = 0.5)
##                         legend("topleft", "Nombre d'enregistrement par Boxplot", cex =0.7, col="orange", text.col="orange", merge=FALSE)
##                     }
##                     Moyenne <- as.vector(tapply(spunit[, metrique], list(spunit$site, spunit$an), mean))
##                     if (choixgraph["PtMoyenneBleu"] == 1)  # 10
##                     {
##                         points(Moyenne, pch=19, col="blue")
##                     }
##                     if (choixgraph["ChiffreMoyenneBleu"] == 1)  # 11
##                     {
##                         text(Moyenne+(Moyenne/10), col = "blue", cex = 0.8, labels=as.character(round(Moyenne, digits=Nbdecimales)))
##                     }
##                     abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")
##                     if (choixgraph["maxExclu"] == 1)   # 1
##                     {
##                         legend("top", "Enregistrements > 95% du maximum retirés", cex =0.7, col="red", text.col="red", merge=FALSE)
##                     }
##                 }
##                 ##  #################### graphique par Caract2 ####################
##                 if (choixgraph["graphCarac"] == 1)  # 15
##                 {
##                     par(mar=c(9, 4, 5, 1), mgp=c(3, 1, 9))
##                     boxplot(spunit[, metrique] ~spunit$caracteristique_2, data=spunit, varwidth = TRUE, ylab=metrique,
##                             las=2, main=paste(metrique, "\n pour la famille : ", unique(spunit$Famille),
##                                    " selon le caracteristique_2 \n"))

##                     if (choixgraph["NbObsOrange"] == 1)  # 9
##                     {
##                         nbObs <- tapply(spunit[, metrique], spunit$caracteristique_2, length)
##                         axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)), col.ticks="orange", col.axis = "orange", lty = 2, lwd = 0.5)
##                         legend("topleft", "Nombre d'enregistrement par Boxplot", cex =0.7, col="orange", text.col="orange", merge=FALSE)
##                     }
##                     Moyenne <- as.vector(tapply(spunit[, metrique], spunit$caracteristique_2, mean))
##                     if (choixgraph["PtMoyenneBleu"] == 1)  # 10
##                     {
##                         points(Moyenne, pch=19, col="blue")
##                     }
##                     if (choixgraph["ChiffreMoyenneBleu"] == 1)  # 11
##                     {
##                         text(Moyenne+(Moyenne/10), col = "blue", cex = 0.8, labels=as.character(round(Moyenne, digits=Nbdecimales)))
##                     }
##                     abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")
##                     if (choixgraph["maxExclu"] == 1)   # 1
##                     {
##                         legend("top", "Enregistrements > 95% du maximum retirés", cex =0.7, col="red", text.col="red", merge=FALSE)
##                     }
##                 }
##                 ##  #################### graphique par AN et PAR Caract2 ####################
##                 if (choixgraph["graphAnCarac"] == 1)  # 16
##                 {
##                     par(mar=c(15, 4, 5, 1), mgp=c(3, 1, 9))
##                     boxplot(spunit[, metrique] ~spunit$caracteristique_2+spunit$an, data=spunit,
##                             varwidth = TRUE, ylab=metrique, las=2, col=heat.colors(length(unique(spunit$caracteristique_2))),
##                             main=paste(metrique, "\n pour la famille : ", unique(spunit$Famille), "\n selon le caracteristique_2 et l'année"))
##                     if (choixgraph["NbObsOrange"] == 1)  # 9
##                     {
##                         nbObs <- tapply(spunit[, metrique], list(spunit$caracteristique_2, spunit$an), length)
##                         axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)), col.ticks="orange", col.axis = "orange", lty = 2, lwd = 0.5)
##                         legend("topleft", "Nombre d'enregistrement par Boxplot", cex =0.7, col="orange", text.col="orange", merge=FALSE)
##                     }
##                     Moyenne <- as.vector(tapply(spunit[, metrique], list(spunit$caracteristique_2, spunit$an), mean))
##                     if (choixgraph["PtMoyenneBleu"] == 1)  # 10
##                     {
##                         points(Moyenne, pch=19, col="blue")
##                     }
##                     if (choixgraph["ChiffreMoyenneBleu"] == 1)  # 11
##                     {
##                         text(Moyenne+(Moyenne/10), col = "blue", cex = 0.8, labels=as.character(round(Moyenne, digits=Nbdecimales)))
##                     }
##                     abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")
##                     if (choixgraph["maxExclu"] == 1)   # 1
##                     {
##                         legend("top", "Enregistrements > 95% du maximum retirés", cex =0.7, col="red", text.col="red", merge=FALSE)
##                     }
##                 }

##             }else{
##                 tkmessageBox(message=paste("Nombres d'enregistrements inssuffisants \n d'après votre choix de nombre minimum d'observations pour", MaFamille))
##             }
##         }
##     }# fin de MetriqueUneFamilleDansPDF.f

##     for (i in 1:length(selectPDFFamille))
##     {
##         nomPDF <- paste(nameWorkspace, "/FichiersSortie/", selectPDFFamille[i], "_", unique(unitobs$type), ".pdf", sep="")
##         pdf(nomPDF, encoding="ISOLatin1", family="URWHelvetica")
##         if (unique(unitobs$type) != "LIT")
##         {
##             MetriqueUneFamilleDansPDF.f("nombre", selectPDFFamille[i])
##             MetriqueUneFamilleDansPDF.f("densite", selectPDFFamille[i])
##             MetriqueUneFamilleDansPDF.f("biomasse", selectPDFFamille[i])
##             MetriqueUneFamilleDansPDF.f("taille_moy", selectPDFFamille[i])
##             MetriqueUneFamilleDansPDF.f("poids_moyen", selectPDFFamille[i])
##         }else{
##             MetriqueUneFamilleDansPDF.f("colonie", selectPDFFamille[i])
##             MetriqueUneFamilleDansPDF.f("nombre", selectPDFFamille[i])
##             MetriqueUneFamilleDansPDF.f("recouvrement", selectPDFFamille[i])
##         }
##         tkmessageBox(message=paste("vos graphiques par an seront enregistrés dans ", nomPDF))
##         dev.off()
##     }
## } # fin de UneFamilleDansPDF.f


##############################################################################################
## [sup] [yr:12/01/2011]:
## UneEspeceDansPDF.f <- function ()
## {
##     print("fonction UneEspeceDansPDF.f activée")

##     ChoixOptionsGraphiques.f()
##     especesPresentes <- subset(unitesp$code_espece, unitesp$pres_abs==1)
##     ChoixFacteurSelect.f(especesPresentes, "code_espece", "multiple", 1, "selectPDFespece")

##     print(choixgraph)
##     print(selectPDFespece)

##     MetriqueUneEspeceDansPDF.f <- function (metrique, MonEspece)
##     {
##         textstatut <- c("HR : Hors Réserve", "PP : Protection Partielle", "RE : En réserve")
##         Nbdecimales <- 2
##         spunit <- subset(listespunit, listespunit$code_espece==MonEspece)
##         if (choixgraph["maxExclu"] == 1)   # 1
##         {
##             spunit[, metrique][which(spunit[, metrique]>GraphPartMax*max(spunit[, metrique], na.rm=T))] <- NA
##         }

##         print(paste("NbObs pour", MonEspece, " : ", length(spunit[, metrique])))
##         if (length(unique(listespunit[, metrique]))>1)
##         {
##             ## on vérifie que la metrique a été calculée pour l'espèce sélectionnée
##             if (length(unique(spunit[, metrique]))>=choixgraph["MinNbObs"])
##             {

##                 ##  #################### graphique par année ####################
##                 if (choixgraph["graphAn"] == 1)   # 2
##                 {
##                     par(mar=c(9, 4, 5, 1), mgp=c(3, 1, 9))
##                     boxplot(spunit[, metrique] ~spunit$an, data=spunit, varwidth = TRUE, ylab=metrique, las=2,
##                             main=paste(metrique, "\n pour l'espèce : ", unique(spunit$code_espece),
##                             " selon l'année \n"))

##                     if (choixgraph["NbObsOrange"] == 1)  # 9
##                     {
##                         nbObs <- tapply(spunit[, metrique], spunit$an, length)
##                         axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)), col.ticks="orange", col.axis = "orange", lty = 2, lwd = 0.5)
##                         legend("topleft", "Nombre d'enregistrement par Boxplot", cex =0.7, col="orange", text.col="orange", merge=FALSE)
##                     }
##                     Moyenne <- as.vector(tapply(spunit[, metrique], spunit$an, na.rm = T, mean))
##                     if (choixgraph["PtMoyenneBleu"] == 1)  # 10
##                     {
##                         points(Moyenne, pch=19, col="blue")
##                     }
##                     if (choixgraph["ChiffreMoyenneBleu"] == 1)  # 11
##                     {
##                         text(Moyenne+(Moyenne/10), col = "blue", cex = 0.8, labels=as.character(round(Moyenne, digits=Nbdecimales)))
##                     }
##                     abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")
##                     if (choixgraph["maxExclu"] == 1)   # 1
##                     {
##                         legend("top", "Enregistrements > 95% du maximum retirés", cex =0.7, col="red", text.col="red", merge=FALSE)
##                     }
##                 }
##                 ##  #################### graphique par AN et PAR BIOTOPE ####################
##                 if (choixgraph["graphAnBiotope"] == 1)   # 3
##                 {
##                     par(mar=c(15, 4, 5, 1), mgp=c(3, 1, 9))
##                     boxplot(spunit[, metrique] ~spunit$biotope+spunit$an, data=spunit,
##                             varwidth = TRUE, ylab=metrique, las=2, col=heat.colors(length(unique(spunit$biotope))),
##                             main=paste(metrique, "\n pour l'espèces : ", unique(spunit$code_espece), "\n selon le biotope et l'année"))
##                     if (choixgraph["NbObsOrange"] == 1)  # 9
##                     {
##                         nbObs <- tapply(spunit[, metrique], list(spunit$biotope, spunit$an), length)
##                         axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)), col.ticks="orange", col.axis = "orange", lty = 2, lwd = 0.5)
##                         legend("topleft", "Nombre d'enregistrement par Boxplot", cex =0.7, col="orange", text.col="orange", merge=FALSE)
##                     }
##                     Moyenne <- as.vector(tapply(spunit[, metrique], list(spunit$biotope, spunit$an), na.rm = T, mean))
##                     if (choixgraph["PtMoyenneBleu"] == 1)  # 10
##                     {
##                         points(Moyenne, pch=19, col="blue")
##                     }
##                     if (choixgraph["ChiffreMoyenneBleu"] == 1)  # 11
##                     {
##                         text(Moyenne+(Moyenne/10), col = "blue", cex = 0.8, labels=as.character(round(Moyenne, digits=Nbdecimales)))
##                     }
##                     abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")
##                     if (choixgraph["maxExclu"] == 1)   # 1
##                     {
##                         legend("top", "Enregistrements > 95% du maximum retirés", cex =0.7, col="red", text.col="red", merge=FALSE)
##                     }
##                 }
##                 ##  #################### graphique par AN et PAR STATUT ####################
##                 if (choixgraph["graphAnStatut"] == 1)   # 4
##                 {
##                     par(mar=c(9, 4, 5, 1), mgp=c(3, 1, 9))
##                     boxplot(spunit[, metrique] ~spunit$statut_protection+spunit$an, data=spunit,
##                             varwidth = TRUE, ylab=metrique, las=2, col=heat.colors(length(unique(spunit$statut_protection))),
##                             main=paste(metrique, " pour l'espèces : ", unique(spunit$code_espece), "\n selon le statut et l'année\n"))
##                     if (length(unique(spunit$statut_protection))==3)
##                     {
##                         legend("topright", textstatut, col=heat.colors(length(unique(spunit$statut_protection))), pch = 15, cex =0.9, title="Statuts")
##                     }
##                     if (choixgraph["NbObsOrange"] == 1)  # 9
##                     {
##                         nbObs <- tapply(spunit[, metrique], list(spunit$statut_protection, spunit$an), length)
##                         axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)), col.ticks="orange", col.axis = "orange", lty = 2, lwd = 0.5)
##                         legend("topleft", "Nombre d'enregistrement par Boxplot", cex =0.7, col="orange", text.col="orange", merge=FALSE)
##                     }
##                     Moyenne <- as.vector(tapply(spunit[, metrique], list(spunit$statut_protection, spunit$an), na.rm = T, mean))
##                     if (choixgraph["PtMoyenneBleu"] == 1)  # 10
##                     {
##                         points(Moyenne, pch=19, col="blue")
##                     }
##                     if (choixgraph["ChiffreMoyenneBleu"] == 1)  # 11
##                     {
##                         text(Moyenne+(Moyenne/10), col = "blue", cex = 0.8, labels=as.character(round(Moyenne, digits=Nbdecimales)))
##                     }
##                     abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")
##                     if (choixgraph["maxExclu"] == 1)   # 1
##                     {
##                         legend("top", "Enregistrements > 95% du maximum retirés", cex =0.7, col="red", text.col="red", merge=FALSE)
##                     }
##                 }
##                 ## #################### graphique par STATUT, PAR AN et par BIOTOPE  ####################
##                 if (choixgraph["graphAnBiotopeStatut"] == 1)   # 5
##                 {
##                     par(mar=c(9, 4, 5, 1), mgp=c(3, 1, 9))

##                     boxplot(spunit[, metrique] ~spunit$statut_protection+spunit$an+spunit$biotope, data=spunit,
##                             varwidth = TRUE, ylab=metrique, las=2, col=heat.colors(length(unique(spunit$statut_protection))),
##                             main=paste(metrique, " pour l'espèces : ", unique(spunit$code_espece), "\n selon le statut, l'année et le biotope\n"))
##                     if (length(unique(spunit$statut_protection))==3)
##                     {
##                         legend("topright", textstatut, col=heat.colors(length(unique(spunit$statut_protection))), pch = 15, cex =0.9, title="Statuts")
##                     }
##                     if (choixgraph["NbObsOrange"] == 1)  # 9
##                     {
##                         nbObs <- tapply(spunit[, metrique], list(spunit$statut_protection, spunit$an, spunit$biotope), length)
##                         axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)), col.ticks="orange", col.axis = "orange", lty = 2, lwd = 0.5)
##                         legend("topleft", "Nombre d'enregistrement par Boxplot", cex =0.7, col="orange", text.col="orange", merge=FALSE)
##                     }
##                     Moyenne <- as.vector(tapply(spunit[, metrique], list(spunit$statut_protection, spunit$an, spunit$biotope), na.rm = T, mean))
##                     if (choixgraph["PtMoyenneBleu"] == 1)  # 10
##                     {
##                         points(Moyenne, pch=19, col="blue")
##                     }
##                     if (choixgraph["ChiffreMoyenneBleu"] == 1)  # 11
##                     {
##                         text(Moyenne+(Moyenne/10), col = "blue", cex = 0.8, labels=as.character(round(Moyenne, digits=Nbdecimales)))
##                     }
##                     abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")
##                     if (choixgraph["maxExclu"] == 1)   # 1
##                     {
##                         legend("top", "Enregistrements > 95% du maximum retirés", cex =0.7, col="red", text.col="red", merge=FALSE)
##                     }
##                 }
##                 ## #################### graphique par BIOTOPE  ####################
##                 if (choixgraph["graphBiotope"] == 1)   # 6
##                 {
##                     par(mar=c(9, 4, 5, 1), mgp=c(3, 1, 9))
##                     boxplot(spunit[, metrique] ~spunit$biotope, data=spunit, varwidth = TRUE, ylab=metrique, las=2,
##                             main=paste(metrique, "\n pour l'espèces : ", unique(spunit$code_espece),
##                             " selon le biotope \n"))

##                     if (choixgraph["NbObsOrange"] == 1)  # 9
##                     {
##                         nbObs <- tapply(spunit[, metrique], spunit$biotope, length)
##                         axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)), col.ticks="orange", col.axis = "orange", lty = 2, lwd = 0.5)
##                         legend("topleft", "Nombre d'enregistrement par Boxplot", cex =0.7, col="orange", text.col="orange", merge=FALSE)
##                     }
##                     Moyenne <- as.vector(tapply(spunit[, metrique], spunit$biotope, na.rm = T, mean))
##                     if (choixgraph["PtMoyenneBleu"] == 1)  # 10
##                     {
##                         points(Moyenne, pch=19, col="blue")
##                     }
##                     if (choixgraph["ChiffreMoyenneBleu"] == 1)  # 11
##                     {
##                         text(Moyenne+(Moyenne/10), col = "blue", cex = 0.8, labels=as.character(round(Moyenne, digits=Nbdecimales)))
##                     }
##                     abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")
##                     if (choixgraph["maxExclu"] == 1)   # 1
##                     {
##                         legend("top", "Enregistrements > 95% du maximum retirés", cex =0.7, col="red", text.col="red", merge=FALSE)
##                     }
##                 }
##                 ## #################### graphique par STATUT   ####################
##                 if (choixgraph["graphStatut"] == 1)   # 7
##                 {
##                     par(mar=c(9, 4, 5, 1), mgp=c(3, 1, 9))
##                     ## boxplot(spunit[, metrique] ~spunit$code_espece, data=spunit, varwidth = TRUE, ylab=metrique,
##                     ## las=2, main=paste(metrique, " \n pour l'espèces : ", spunit$code_espece, "\npour toutes les
##                     ## unités d'obs"))

##                     boxplot(spunit[, metrique] ~spunit$statut_protection, data=spunit, varwidth = TRUE, ylab=metrique,
##                             las=2, main=paste(metrique, "\n pour l'espèces : ", unique(spunit$code_espece),
##                                    "selon le statut \n"))
##                     if (length(unique(spunit$statut_protection))==3)
##                     {
##                         legend("topright", textstatut, col=heat.colors(length(unique(spunit$statut_protection))), pch = 15, cex =0.9, title="Statuts")
##                     }
##                     if (choixgraph["NbObsOrange"] == 1)  # 9
##                     {
##                         nbObs <- tapply(spunit[, metrique], spunit$statut_protection, length)
##                         axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)), col.ticks="orange", col.axis = "orange", lty = 2, lwd = 0.5)
##                         legend("topleft", "Nombre d'enregistrement par Boxplot", cex =0.7, col="orange", text.col="orange", merge=FALSE)
##                     }
##                     Moyenne <- as.vector(tapply(spunit[, metrique], spunit$statut_protection, na.rm = T, mean))
##                     if (choixgraph["PtMoyenneBleu"] == 1)  # 10
##                     {
##                         points(Moyenne, pch=19, col="blue")
##                     }
##                     if (choixgraph["ChiffreMoyenneBleu"] == 1)  # 11
##                     {
##                         text(Moyenne+(Moyenne/10), col = "blue", cex = 0.8, labels=as.character(round(Moyenne, digits=Nbdecimales)))
##                     }
##                     abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")
##                     if (choixgraph["maxExclu"] == 1)   # 1
##                     {
##                         legend("top", "Enregistrements > 95% du maximum retirés", cex =0.7, col="red", text.col="red", merge=FALSE)
##                     }
##                 }
##                 ## #################### graphique par STATUT et PAR BIOTOPE ####################
##                 if (choixgraph["graphBiotopeStatut"] == 1)  # 8
##                 {
##                     par(mar=c(9, 4, 5, 1), mgp=c(3, 1, 9))
##                     boxplot(spunit[, metrique] ~spunit$statut_protection+spunit$biotope, data=spunit,
##                             varwidth = TRUE, ylab=metrique, las=2, col=heat.colors(length(unique(spunit$statut_protection))),
##                             main=paste(metrique, " pour l'espèces : ", unique(spunit$code_espece), "\n selon le statut et le biotope\n"))
##                     if (length(unique(spunit$statut_protection))==3)
##                     {
##                         legend("topright", textstatut, col=heat.colors(length(unique(spunit$statut_protection))), pch = 15, cex =0.9, title="Statuts")
##                     }
##                     if (choixgraph["NbObsOrange"] == 1)  # 9
##                     {
##                         nbObs <- tapply(spunit[, metrique], list(spunit$statut_protection, spunit$biotope), length)
##                         axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)), col.ticks="orange", col.axis = "orange", lty = 2, lwd = 0.5)
##                         legend("topleft", "Nombre d'enregistrement par Boxplot", cex =0.7, col="orange", text.col="orange", merge=FALSE)
##                     }
##                     Moyenne <- as.vector(tapply(spunit[, metrique], list(spunit$statut_protection, spunit$biotope), na.rm = T, mean))
##                     if (choixgraph["PtMoyenneBleu"] == 1)  # 10
##                     {
##                         points(Moyenne, pch=19, col="blue")
##                     }
##                     if (choixgraph["ChiffreMoyenneBleu"] == 1)  # 11
##                     {
##                         text(Moyenne+(Moyenne/10), col = "blue", cex = 0.8, labels=as.character(round(Moyenne, digits=Nbdecimales)))
##                     }
##                     abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")
##                     if (choixgraph["maxExclu"] == 1)   # 1
##                     {
##                         legend("top", "Enregistrements > 95% du maximum retirés", cex =0.7, col="red", text.col="red", merge=FALSE)
##                     }
##                 }
##                 ## #################### graphique par SITE ####################
##                 if (choixgraph["graphSite"] == 1)  # 13
##                 {
##                     par(mar=c(9, 4, 5, 1), mgp=c(3, 1, 9))
##                     boxplot(spunit[, metrique] ~spunit$site, data=spunit, varwidth = TRUE, ylab=metrique, las=2,
##                             main=paste(metrique, "\n pour l'espèce : ", unique(spunit$code_espece),
##                             " selon le site \n"))

##                     if (choixgraph["NbObsOrange"] == 1)  # 9
##                     {
##                         nbObs <- tapply(spunit[, metrique], spunit$site, length)
##                         axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)), col.ticks="orange", col.axis = "orange", lty = 2, lwd = 0.5)
##                         legend("topleft", "Nombre d'enregistrement par Boxplot", cex =0.7, col="orange", text.col="orange", merge=FALSE)
##                     }
##                     Moyenne <- as.vector(tapply(spunit[, metrique], spunit$site, na.rm = T, mean))
##                     if (choixgraph["PtMoyenneBleu"] == 1)  # 10
##                     {
##                         points(Moyenne, pch=19, col="blue")
##                     }
##                     if (choixgraph["ChiffreMoyenneBleu"] == 1)  # 11
##                     {
##                         text(Moyenne+(Moyenne/10), col = "blue", cex = 0.8, labels=as.character(round(Moyenne, digits=Nbdecimales)))
##                     }
##                     abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")
##                     if (choixgraph["maxExclu"] == 1)   # 1
##                     {
##                         legend("top", "Enregistrements > 95% du maximum retirés", cex =0.7, col="red", text.col="red", merge=FALSE)
##                     }
##                 }
##                 ## #################### graphique par AN et PAR SITE ####################
##                 if (choixgraph["graphAnSite"] == 1)  # 14
##                 {
##                     par(mar=c(15, 4, 5, 1), mgp=c(3, 1, 9))
##                     boxplot(spunit[, metrique] ~spunit$site+spunit$an, data=spunit,
##                             varwidth = TRUE, ylab=metrique, las=2, col=heat.colors(length(unique(spunit$site))),
##                             main=paste(metrique, "\n pour l'espèces : ", unique(spunit$code_espece), "\n selon le site et l'année"))
##                     if (choixgraph["NbObsOrange"] == 1)  # 9
##                     {
##                         nbObs <- tapply(spunit[, metrique], list(spunit$biotope, spunit$an), length)
##                         axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)), col.ticks="orange", col.axis = "orange", lty = 2, lwd = 0.5)
##                         legend("topleft", "Nombre d'enregistrement par Boxplot", cex =0.7, col="orange", text.col="orange", merge=FALSE)
##                     }
##                     Moyenne <- as.vector(tapply(spunit[, metrique], list(spunit$site, spunit$an), na.rm = T, mean))
##                     if (choixgraph["PtMoyenneBleu"] == 1)  # 10
##                     {
##                         points(Moyenne, pch=19, col="blue")
##                     }
##                     if (choixgraph["ChiffreMoyenneBleu"] == 1)  # 11
##                     {
##                         text(Moyenne+(Moyenne/10), col = "blue", cex = 0.8, labels=as.character(round(Moyenne, digits=Nbdecimales)))
##                     }
##                     abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")
##                     if (choixgraph["maxExclu"] == 1)   # 1
##                     {
##                         legend("top", "Enregistrements > 95% du maximum retirés", cex =0.7, col="red", text.col="red", merge=FALSE)
##                     }
##                 }
##                 ## #################### graphique par CARAC2 ####################
##                 if (choixgraph["graphCarac"] == 1)  # 15
##                 {
##                     par(mar=c(9, 4, 5, 1), mgp=c(3, 1, 9))
##                     boxplot(spunit[, metrique] ~spunit$caracteristique_2, data=spunit, varwidth = TRUE, ylab=metrique,
##                             las=2, main=paste(metrique, "\n pour l'espèce : ", unique(spunit$code_espece),
##                                    " selon le caracteristique_2 \n"))

##                     if (choixgraph["NbObsOrange"] == 1)  # 9
##                     {
##                         nbObs <- tapply(spunit[, metrique], spunit$caracteristique_2, length)
##                         axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)), col.ticks="orange", col.axis = "orange", lty = 2, lwd = 0.5)
##                         legend("topleft", "Nombre d'enregistrement par Boxplot", cex =0.7, col="orange", text.col="orange", merge=FALSE)
##                     }
##                     Moyenne <- as.vector(tapply(spunit[, metrique], spunit$caracteristique_2, na.rm = T, mean))
##                     if (choixgraph["PtMoyenneBleu"] == 1)  # 10
##                     {
##                         points(Moyenne, pch=19, col="blue")
##                     }
##                     if (choixgraph["ChiffreMoyenneBleu"] == 1)  # 11
##                     {
##                         text(Moyenne+(Moyenne/10), col = "blue", cex = 0.8, labels=as.character(round(Moyenne, digits=Nbdecimales)))
##                     }
##                     abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")
##                     if (choixgraph["maxExclu"] == 1)   # 1
##                     {
##                         legend("top", "Enregistrements > 95% du maximum retirés", cex =0.7, col="red", text.col="red", merge=FALSE)
##                     }
##                 }
##                 ## #################### graphique par AN et PAR CARAC2 ####################
##                 if (choixgraph["graphAnCarac"] == 1)  # 16
##                 {
##                     par(mar=c(15, 4, 5, 1), mgp=c(3, 1, 9))
##                     boxplot(spunit[, metrique] ~spunit$caracteristique_2+spunit$an, data=spunit,
##                             varwidth = TRUE, ylab=metrique, las=2, col=heat.colors(length(unique(spunit$caracteristique_2))),
##                             main=paste(metrique, "\n pour l'espèces : ", unique(spunit$code_espece), "\n selon le caracteristique_2 et l'année"))
##                     if (choixgraph["NbObsOrange"] == 1)  # 9
##                     {
##                         nbObs <- tapply(spunit[, metrique], list(spunit$biotope, spunit$an), length)
##                         axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)), col.ticks="orange", col.axis = "orange", lty = 2, lwd = 0.5)
##                         legend("topleft", "Nombre d'enregistrement par Boxplot", cex =0.7, col="orange", text.col="orange", merge=FALSE)
##                     }
##                     Moyenne <- as.vector(tapply(spunit[, metrique], list(spunit$caracteristique_2, spunit$an), na.rm = T, mean))
##                     if (choixgraph["PtMoyenneBleu"] == 1)  # 10
##                     {
##                         points(Moyenne, pch=19, col="blue")
##                     }
##                     if (choixgraph["ChiffreMoyenneBleu"] == 1)  # 11
##                     {
##                         text(Moyenne+(Moyenne/10), col = "blue", cex = 0.8, labels=as.character(round(Moyenne, digits=Nbdecimales)))
##                     }
##                     abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")
##                     if (choixgraph["maxExclu"] == 1)   # 1
##                     {
##                         legend("top", "Enregistrements > 95% du maximum retirés", cex =0.7, col="red", text.col="red", merge=FALSE)
##                     }
##                 }
##             }else{
##                 tkmessageBox(message=paste("Nombres d'enregistrements inssuffisants \n d'après votre choix de nombre minimum d'observations pour", MonEspece))
##             }
##         }
##     }# fin de MetriqueUneEspeceDansPDF.f

##     for (i in 1:length(selectPDFespece))
##     {
##         nomPDF <- paste(nameWorkspace, "/FichiersSortie/", selectPDFespece[i], "_", unique(unitobs$type), ".pdf", sep="")
##         pdf(nomPDF, encoding="ISOLatin1", family="URWHelvetica")
##         if (unique(unitobs$type) != "LIT")
##         {
##             MetriqueUneEspeceDansPDF.f("nombre", selectPDFespece[i])
##             MetriqueUneEspeceDansPDF.f("densite", selectPDFespece[i])
##             MetriqueUneEspeceDansPDF.f("biomasse", selectPDFespece[i])
##             MetriqueUneEspeceDansPDF.f("taille_moy", selectPDFespece[i])
##             MetriqueUneEspeceDansPDF.f("poids_moyen", selectPDFespece[i])
##         }else{
##             MetriqueUneEspeceDansPDF.f("colonie", selectPDFespece[i])
##             MetriqueUneEspeceDansPDF.f("nombre", selectPDFespece[i])
##             MetriqueUneEspeceDansPDF.f("recouvrement", selectPDFespece[i])
##         }
##         tkmessageBox(message=paste("vos graphiques par an seront enregistrés dans ", nomPDF))
##         dev.off()
##     }
## } # fin de UneEspeceDansPDF.f

## [sup] [yr:12/01/2011]:

## ChaqueEspeceDansPDF.f <- function (metrique)
## {
##     print("fonction ChaqueEspeceDansPDF.f activée")

##     ## récupération des variables et de la liste d'espèce
##     ChoixOptionsGraphiques.f()
##     matable <- "listespunit"
##     choixchamptable.f(matable)
##     metrique <- champtrouve
##     print(metrique)
##     especesPresentes <- unique(subset(unitesp$code_espece, unitesp$pres_abs==1))

##     ## variables pour les graphiques
##     textstatut <- c("HR : Hors Réserve", "PP : Protection Partielle", "RE : En réserve")
##     Nbdecimales <- 2
##     ## choixgraph  contient c(cMaxVal, cTypeAnVal, cTypeAnBiotopeVal, cTypeAnStatutVal, cTypeAnBiotopeStatutVal,
##     ## cTypeBiotopeVal, cTypeStatutVal, cTypeBiotopeStatutVal, cAfficheNbObsVal, cAffichePointMoyenneVal, cAfficheValeurMoyenneVal, NbMinObsPourGraphVal)
##     ## en fonction de ces variables, on actionne les boucles et on affiche des nb obs et des moyennes dans les graphiques

##     ## CAS DES GRAPHIQUE POUR CHAQUE ESPECE PAR AN
##     if (choixgraph["graphAn"] == 1)   # 2
##     {
##         nomPDF <- paste(nameWorkspace, "/FichiersSortie/", metrique, "_ParAn", unique(unitobs$type), "ParEsp", ".pdf", sep="")
##         pdf(nomPDF, encoding="ISOLatin1", family="URWHelvetica")

##         for (i in 1:length(especesPresentes))
##         {
##             spunit <- subset(listespunit, listespunit$code_espece==especesPresentes[i])
##             if (choixgraph["maxExclu"] == 1)   # 1
##             {
##                 spunit[, metrique][which(spunit[, metrique]>GraphPartMax*max(spunit[, metrique], na.rm=T))] <- NA
##             }
##             print(paste("NbObs pour", especesPresentes[i], " : ", length(spunit[, metrique])))
##             if (length(unique(listespunit[, metrique]))>1)
##             {
##                 ## on vérifie que la metrique a été calculée pour l'espèce sélectionnée
##                 if (length(unique(spunit[, metrique]))>=choixgraph["MinNbObs"])
##                 {
##                     par(mar=c(9, 4, 5, 1), mgp=c(3, 1, 9))
##                     boxplot(spunit[, metrique] ~spunit$an, data=spunit, varwidth = TRUE, ylab=metrique, las=2,
##                             main=paste(metrique, "\n pour l'espèces : ", unique(spunit$code_espece),
##                             " selon l'année \n"))

##                     if (choixgraph["NbObsOrange"] == 1)  # 9
##                     {
##                         nbObs <- tapply(spunit[, metrique], spunit$an, length)
##                         axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)), col.ticks="orange", col.axis = "orange", lty = 2, lwd = 0.5)
##                         legend("topleft", "Nombre d'enregistrement par Boxplot", cex =0.7, col="orange", text.col="orange", merge=FALSE)
##                     }
##                     Moyenne <- as.vector(tapply(spunit[, metrique], spunit$an, na.rm = T, mean))
##                     if (choixgraph["PtMoyenneBleu"] == 1)  # 10
##                     {
##                         points(Moyenne, pch=19, col="blue")
##                     }
##                     if (choixgraph["ChiffreMoyenneBleu"] == 1)  # 11
##                     {
##                         text(Moyenne+(Moyenne/10), col = "blue", cex = 0.8, labels=as.character(round(Moyenne, digits=Nbdecimales)))
##                     }
##                     abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")
##                     if (choixgraph["maxExclu"] == 1)   # 1
##                     {
##                         legend("top", "Enregistrements > 95% du maximum retirés", cex =0.7, col="red", text.col="red", merge=FALSE)
##                     }
##                 }
##             }
##         }  #fin du for pour chaque espèce
##         tkmessageBox(message=paste("vos graphiques par an seront enregistrés dans ", nomPDF, " à la racine de PAMPA"))
##         print("boucle POUR CHAQUE ESPECE PAR AN réalisée")
##         ## ferme le PDF
##         dev.off()
##     }#fin de GRAPHIQUE POUR CHAQUE ESPECE PAR AN


##     ## CAS DES GRAPHIQUE POUR CHAQUE ESPECE PAR AN et PAR BIOTOPE
##     if (choixgraph["graphAnBiotope"] == 1)   # 3
##     {
##         nomPDF <- paste(nameWorkspace, "/FichiersSortie/", metrique, "_ParStatut", unique(unitobs$type), "ParEsp", ".pdf", sep="")
##         pdf(nomPDF, encoding="ISOLatin1", family="URWHelvetica")

##         for (i in 1:length(especesPresentes))
##         {
##             spunit <- subset(listespunit, listespunit$code_espece==especesPresentes[i])
##             if (choixgraph["maxExclu"] == 1)   # 1
##             {
##                 ## choix <- 1 #il faut que la variable choix de enlevermax soit à 1 pour que les max soient retirés
##                 spunit[, metrique][which(spunit[, metrique]>GraphPartMax*max(spunit[, metrique], na.rm=T))] <- NA
##             }

##             if (length(unique(listespunit[, metrique]))>1)
##             {
##                 ## on vérifie que la metrique a été calculée pour l'espèce sélectionnée
##                 if (length(unique(spunit[, metrique]))>1)
##                 {
##                     par(mar=c(15, 4, 5, 1), mgp=c(3, 1, 9))
##                     boxplot(spunit[, metrique] ~spunit$biotope+spunit$an, data=spunit,
##                             varwidth = TRUE, ylab=metrique, las=2, col=heat.colors(length(unique(spunit$biotope))),
##                             main=paste(metrique, "\n pour l'espèces : ", unique(spunit$code_espece), "\n selon le biotope, l'année et le statut"))

##                     if (choixgraph["NbObsOrange"] == 1)  # 9
##                     {
##                         nbObs <- tapply(spunit[, metrique], list(spunit$biotope, spunit$an), length)
##                         axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)), col.ticks="orange", col.axis = "orange", lty = 2, lwd = 0.5)
##                         legend("topleft", "Nombre d'enregistrement par Boxplot", cex =0.7, col="orange", text.col="orange", merge=FALSE)
##                     }
##                     Moyenne <- as.vector(tapply(spunit[, metrique], list(spunit$biotope, spunit$an), na.rm = T, mean))
##                     if (choixgraph["PtMoyenneBleu"] == 1)  # 10
##                     {
##                         points(Moyenne, pch=19, col="blue")
##                     }
##                     if (choixgraph["ChiffreMoyenneBleu"] == 1)  # 11
##                     {
##                         text(Moyenne+(Moyenne/10), col = "blue", cex = 0.8, labels=as.character(round(Moyenne, digits=Nbdecimales)))
##                     }
##                     abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")
##                     if (choixgraph["maxExclu"] == 1)   # 1
##                     {
##                         legend("top", "Enregistrements > 95% du maximum retirés", cex =0.7, col="red", text.col="red", merge=FALSE)
##                     }
##                 }
##             }
##         }  #fin du for pour chaque espèce
##         tkmessageBox(message=paste("vos graphiques par statut seront enregistrés dans ", nomPDF, " à la racine de PAMPA"))
##         print("boucle POUR CHAQUE ESPECE PAR BIOTOPE ET  PAR AN réalisée")
##         ## ferme le PDF
##         dev.off()

##     }#fin de GRAPHIQUE POUR CHAQUE ESPECE PAR BIOTOPE ET  PAR AN


##     ## CAS DES GRAPHIQUE POUR CHAQUE ESPECE PAR STATUT ET PAR AN
##     if (choixgraph["graphAnStatut"] == 1)   # 4
##     {
##         nomPDF <- paste(nameWorkspace, "/FichiersSortie/", metrique, "_ParAn_Statut", unique(unitobs$type), "ParEsp", ".pdf", sep="")
##         pdf(nomPDF, encoding="ISOLatin1", family="URWHelvetica")

##         for (i in 1:length(especesPresentes))
##         {
##             spunit <- subset(listespunit, listespunit$code_espece==especesPresentes[i])
##             if (choixgraph["maxExclu"] == 1)   # 1
##             {
##                 ## choix <- 1 #il faut que la variable choix de enlevermax soit à 1 pour que les max soient retirés
##                 spunit[, metrique][which(spunit[, metrique]>GraphPartMax*max(spunit[, metrique], na.rm=T))] <- NA
##             }

##             if (length(unique(listespunit[, metrique]))>1)
##             {
##                 ## on vérifie que la metrique a été calculée pour l'espèce sélectionnée
##                 if (length(unique(spunit[, metrique]))>1)
##                 {
##                     par(mar=c(9, 4, 5, 1), mgp=c(3, 1, 9))

##                     boxplot(spunit[, metrique] ~spunit$statut_protection+spunit$an, data=spunit,
##                             varwidth = TRUE, ylab=metrique, las=2, col=heat.colors(length(unique(spunit$statut_protection))),
##                             main=paste(metrique, " pour l'espèces : ", unique(spunit$code_espece), "\n selon le statut et l'année\n"))
##                     if (length(unique(spunit$statut_protection))==3)
##                     {
##                         legend("topright", textstatut, col=heat.colors(length(unique(spunit$statut_protection))), pch = 15, cex =0.9, title="Statuts")
##                     }
##                     if (choixgraph["NbObsOrange"] == 1)  # 9
##                     {
##                         nbObs <- tapply(spunit[, metrique], list(spunit$statut_protection, spunit$an), length)
##                         axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)), col.ticks="orange", col.axis = "orange", lty = 2, lwd = 0.5)
##                         legend("topleft", "Nombre d'enregistrement par Boxplot", cex =0.7, col="orange", text.col="orange", merge=FALSE)
##                     }
##                     Moyenne <- as.vector(tapply(spunit[, metrique], list(spunit$statut_protection, spunit$an), na.rm = T, mean))
##                     if (choixgraph["PtMoyenneBleu"] == 1)  # 10
##                     {
##                         points(Moyenne, pch=19, col="blue")
##                     }
##                     if (choixgraph["ChiffreMoyenneBleu"] == 1)  # 11
##                     {
##                         text(Moyenne+(Moyenne/10), col = "blue", cex = 0.8, labels=as.character(round(Moyenne, digits=Nbdecimales)))
##                     }
##                     abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")
##                     if (choixgraph["maxExclu"] == 1)   # 1
##                     {
##                         legend("top", "Enregistrements > 95% du maximum retirés", cex =0.7, col="red", text.col="red", merge=FALSE)
##                     }
##                 }
##             }
##         }  #fin du for pour chaque espèce
##         tkmessageBox(message=paste("vos graphiques par statut seront enregistrés dans ", nomPDF, " à la racine de PAMPA"))
##         print("boucle POUR CHAQUE ESPECE PAR STATUT ET  PAR AN réalisée")
##         ## ferme le PDF
##         dev.off()

##     }#fin de GRAPHIQUE POUR CHAQUE ESPECE PAR STATUT ET  PAR AN


##     ## CAS DES GRAPHIQUE POUR CHAQUE ESPECE PAR STATUT, PAR AN ET PAR BIOTOPE
##     if (choixgraph["graphAnBiotopeStatut"] == 1)   # 5
##     {
##         nomPDF <- paste(nameWorkspace, "/FichiersSortie/", metrique, "_ParBiotope_An_Statut", unique(unitobs$type), "ParEsp", ".pdf", sep="")
##         pdf(nomPDF, encoding="ISOLatin1", family="URWHelvetica")

##         for (i in 1:length(especesPresentes))
##         {
##             spunit <- subset(listespunit, listespunit$code_espece==especesPresentes[i])
##             if (choixgraph["maxExclu"] == 1)   # 1
##             {
##                 spunit[, metrique][which(spunit[, metrique]>GraphPartMax*max(spunit[, metrique], na.rm=T))] <- NA
##             }

##             if (length(unique(listespunit[, metrique]))>1)
##             {
##                 ## on vérifie que la metrique a été calculée pour l'espèce sélectionnée
##                 if (length(unique(spunit[, metrique]))>1)
##                 {
##                     par(mar=c(9, 4, 5, 1), mgp=c(3, 1, 9))
##                     boxplot(spunit[, metrique] ~spunit$statut_protection+spunit$an+spunit$biotope, data=spunit,
##                             varwidth = TRUE, ylab=metrique, las=2, col=heat.colors(length(unique(spunit$statut_protection))),
##                             main=paste(metrique, " pour l'espèces : ", unique(spunit$code_espece), "\n selon le statut, l'année et le biotope\n"))
##                     if (length(unique(spunit$statut_protection))==3)
##                     {
##                         legend("topright", textstatut, col=heat.colors(length(unique(spunit$statut_protection))), pch = 15, cex =0.9, title="Statuts")
##                     }
##                     if (choixgraph["NbObsOrange"] == 1)  # 9
##                     {
##                         nbObs <- tapply(spunit[, metrique], list(spunit$statut_protection, spunit$an, spunit$biotope), length)
##                         axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)), col.ticks="orange", col.axis = "orange", lty = 2, lwd = 0.5)
##                         legend("topleft", "Nombre d'enregistrement par Boxplot", cex =0.7, col="orange", text.col="orange", merge=FALSE)
##                     }
##                     Moyenne <- as.vector(tapply(spunit[, metrique], list(spunit$statut_protection, spunit$an, spunit$biotope), na.rm = T, mean))
##                     if (choixgraph["PtMoyenneBleu"] == 1)  # 10
##                     {
##                         points(Moyenne, pch=19, col="blue")
##                     }
##                     if (choixgraph["ChiffreMoyenneBleu"] == 1)  # 11
##                     {
##                         text(Moyenne+(Moyenne/10), col = "blue", cex = 0.8, labels=as.character(round(Moyenne, digits=Nbdecimales)))
##                     }
##                     abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")
##                     if (choixgraph["maxExclu"] == 1)   # 1
##                     {
##                         legend("top", "Enregistrements > 95% du maximum retirés", cex =0.7, col="red", text.col="red", merge=FALSE)
##                     }
##                 }
##             }
##         }  #fin du for pour chaque espèce
##         tkmessageBox(message=paste("vos graphiques par statut seront enregistrés dans ", nomPDF, " à la racine de PAMPA"))
##         print("boucle POUR CHAQUE ESPECE PAR STATUT ET PAR BIOTOPE et PAR AN réalisée")
##         ## ferme le PDF
##         dev.off()

##     }#fin de GRAPHIQUE POUR CHAQUE ESPECE PAR STATUT ET PAR BIOTOPE et PAR AN


##     ## CAS DES GRAPHIQUE POUR CHAQUE ESPECE PAR BIOTOPE
##     if (choixgraph["graphBiotope"] == 1)   # 6
##     {
##         nomPDF <- paste(nameWorkspace, "/FichiersSortie/", metrique, "_ParBiotope", unique(unitobs$type), "ParEsp", ".pdf", sep="")
##         pdf(nomPDF, encoding="ISOLatin1", family="URWHelvetica")

##         for (i in 1:length(especesPresentes))
##         {
##             spunit <- subset(listespunit, listespunit$code_espece==especesPresentes[i])
##             if (choixgraph["maxExclu"] == 1)   # 1
##             {
##                 spunit[, metrique][which(spunit[, metrique]>GraphPartMax*max(spunit[, metrique], na.rm=T))] <- NA
##             }

##             if (length(unique(listespunit[, metrique]))>1)
##             {
##                 ## on vérifie que la metrique a été calculée pour l'espèce sélectionnée
##                 if (length(unique(spunit[, metrique]))>1)
##                 {
##                     par(mar=c(9, 4, 5, 1), mgp=c(3, 1, 9))
##                     boxplot(spunit[, metrique] ~spunit$biotope, data=spunit, varwidth = TRUE,
##                             ylab=metrique, las=2, main=paste(metrique, "\n pour l'espèces : ", unique(spunit$code_espece), " selon le biotope \n"))

##                     if (choixgraph["NbObsOrange"] == 1)  # 9
##                     {
##                         nbObs <- tapply(spunit[, metrique], spunit$biotope, length)
##                         axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)), col.ticks="orange", col.axis = "orange", lty = 2, lwd = 0.5)
##                         legend("topleft", "Nombre d'enregistrement par Boxplot", cex =0.7, col="orange", text.col="orange", merge=FALSE)
##                     }
##                     Moyenne <- as.vector(tapply(spunit[, metrique], spunit$biotope, na.rm = T, mean))
##                     if (choixgraph["PtMoyenneBleu"] == 1)  # 10
##                     {
##                         points(Moyenne, pch=19, col="blue")
##                     }
##                     if (choixgraph["ChiffreMoyenneBleu"] == 1)  # 11
##                     {
##                         text(Moyenne+(Moyenne/10), col = "blue", cex = 0.8, labels=as.character(round(Moyenne, digits=Nbdecimales)))
##                     }
##                     abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")
##                     if (choixgraph["maxExclu"] == 1)   # 1
##                     {
##                         legend("top", "Enregistrements > 95% du maximum retirés", cex =0.7, col="red", text.col="red", merge=FALSE)
##                     }
##                 }
##             }
##         }  #fin du for pour chaque espèce
##         tkmessageBox(message=paste("vos graphiques par an seront enregistrés dans ", nomPDF, " à la racine de PAMPA"))
##         print("boucle POUR CHAQUE ESPECE PAR BIOTOPE réalisée")
##         ## ferme le PDF
##         dev.off()

##     }#fin de GRAPHIQUE POUR CHAQUE ESPECE PAR BIOTOPE

##     ## CAS DES GRAPHIQUE POUR CHAQUE ESPECE PAR STATUT
##     if (choixgraph["graphStatut"] == 1)   # 7
##     {
##         nomPDF <- paste(nameWorkspace, "/FichiersSortie/", metrique, "_ParStatut", unique(unitobs$type), "ParEsp", ".pdf", sep="")
##         pdf(nomPDF, encoding="ISOLatin1", family="URWHelvetica")

##         for (i in 1:length(especesPresentes))
##         {
##             spunit <- subset(listespunit, listespunit$code_espece==especesPresentes[i])
##             if (choixgraph["maxExclu"] == 1)   # 1
##             {
##                 spunit[, metrique][which(spunit[, metrique]>GraphPartMax*max(spunit[, metrique], na.rm=T))] <- NA
##             }

##             if (length(unique(listespunit[, metrique]))>1)
##             {
##                 ## on vérifie que la metrique a été calculée pour l'espèce sélectionnée
##                 if (length(unique(spunit[, metrique]))>1)
##                 {
##                     par(mar=c(9, 4, 5, 1), mgp=c(3, 1, 9))
##                     ## boxplot(spunit[, metrique] ~spunit$code_espece, data=spunit, varwidth = TRUE, ylab=metrique,
##                     ## las=2, main=paste(metrique, " \n pour l'espèces : ", spunit$code_espece, "\npour toutes les
##                     ## unités d'obs"))

##                     boxplot(spunit[, metrique] ~spunit$statut_protection, data=spunit, varwidth = TRUE, ylab=metrique,
##                             las=2, main=paste(metrique, "\n pour l'espèces : ", unique(spunit$code_espece),
##                                    "selon le statut \n"))
##                     if (length(unique(spunit$statut_protection))==3)
##                     {
##                         legend("topright", textstatut, col=heat.colors(length(unique(spunit$statut_protection))), pch = 15, cex =0.9, title="Statuts")
##                     }
##                     if (choixgraph["NbObsOrange"] == 1)  # 9
##                     {
##                         nbObs <- tapply(spunit[, metrique], spunit$statut_protection, length)
##                         axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)), col.ticks="orange", col.axis = "orange", lty = 2, lwd = 0.5)
##                         legend("topleft", "Nombre d'enregistrement par Boxplot", cex =0.7, col="orange", text.col="orange", merge=FALSE)
##                     }
##                     Moyenne <- as.vector(tapply(spunit[, metrique], spunit$statut_protection, na.rm = T, mean))
##                     if (choixgraph["PtMoyenneBleu"] == 1)  # 10
##                     {
##                         points(Moyenne, pch=19, col="blue")
##                     }
##                     if (choixgraph["ChiffreMoyenneBleu"] == 1)  # 11
##                     {
##                         text(Moyenne+(Moyenne/10), col = "blue", cex = 0.8, labels=as.character(round(Moyenne, digits=Nbdecimales)))
##                     }
##                     abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")
##                     if (choixgraph["maxExclu"] == 1)   # 1
##                     {
##                         legend("top", "Enregistrements > 95% du maximum retirés", cex =0.7, col="red", text.col="red", merge=FALSE)
##                     }
##                 }
##             }
##         }  #fin du for pour chaque espèce
##         tkmessageBox(message=paste("vos graphiques par statut seront enregistrés dans ", nomPDF, " à la racine de PAMPA"))
##         print("boucle POUR CHAQUE ESPECE PAR STATUT réalisée")
##         ## ferme le PDF
##         dev.off()

##     }#fin de GRAPHIQUE POUR CHAQUE ESPECE PAR STATUT

##     ## CAS DES GRAPHIQUE POUR CHAQUE ESPECE PAR STATUT ET  PAR BIOTOPE
##     if (choixgraph["graphBiotopeStatut"] == 1)  # 8
##     {
##         nomPDF <- paste(nameWorkspace, "/FichiersSortie/", metrique, "_ParBiotope_Statut", unique(unitobs$type), "ParEsp", ".pdf", sep="")
##         pdf(nomPDF, encoding="ISOLatin1", family="URWHelvetica")

##         for (i in 1:length(especesPresentes))
##         {
##             spunit <- subset(listespunit, listespunit$code_espece==especesPresentes[i])
##             if (choixgraph["maxExclu"] == 1)   # 1
##             {
##                 spunit[, metrique][which(spunit[, metrique]>GraphPartMax*max(spunit[, metrique], na.rm=T))] <- NA
##             }

##             if (length(unique(listespunit[, metrique]))>1)
##             {
##                 ## on vérifie que la metrique a été calculée pour l'espèce sélectionnée
##                 if (length(unique(spunit[, metrique]))>1)
##                 {

##                     par(mar=c(9, 4, 5, 1), mgp=c(3, 1, 9))
##                     boxplot(spunit[, metrique] ~spunit$statut_protection+spunit$biotope, data=spunit,
##                             varwidth = TRUE, ylab=metrique, las=2, col=heat.colors(length(unique(spunit$statut_protection))),
##                             main=paste(metrique, " pour l'espèces : ", unique(spunit$code_espece), "\n selon le statut et le biotope\n"))
##                     if (length(unique(spunit$statut_protection))==3)
##                     {
##                         legend("topright", textstatut, col=heat.colors(length(unique(spunit$statut_protection))), pch = 15, cex =0.9, title="Statuts")
##                     }
##                     if (choixgraph["NbObsOrange"] == 1)  # 9
##                     {
##                         nbObs <- tapply(spunit[, metrique], list(spunit$statut_protection, spunit$biotope), length)
##                         axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)), col.ticks="orange", col.axis = "orange", lty = 2, lwd = 0.5)
##                         legend("topleft", "Nombre d'enregistrement par Boxplot", cex =0.7, col="orange", text.col="orange", merge=FALSE)
##                     }
##                     Moyenne <- as.vector(tapply(spunit[, metrique], list(spunit$statut_protection, spunit$biotope), na.rm = T, mean))
##                     if (choixgraph["PtMoyenneBleu"] == 1)  # 10
##                     {
##                         points(Moyenne, pch=19, col="blue")
##                     }
##                     if (choixgraph["ChiffreMoyenneBleu"] == 1)  # 11
##                     {
##                         text(Moyenne+(Moyenne/10), col = "blue", cex = 0.8, labels=as.character(round(Moyenne, digits=Nbdecimales)))
##                     }
##                     abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")
##                     if (choixgraph["maxExclu"] == 1)   # 1
##                     {
##                         legend("top", "Enregistrements > 95% du maximum retirés", cex =0.7, col="red", text.col="red", merge=FALSE)
##                     }
##                 }
##             }
##         }  #fin du for pour chaque espèce
##         tkmessageBox(message=paste("vos graphiques par statut seront enregistrés dans ", nomPDF, " dans le dossier FichiersSortie"))
##         print("boucle POUR CHAQUE ESPECE PAR STATUT ET  PAR BIOTOPE réalisée")
##         ## ferme le PDF
##         dev.off()
##     }#fin de GRAPHIQUE POUR CHAQUE ESPECE PAR STATUT ET  PAR BIOTOPE

##     ## CAS DES GRAPHIQUE POUR CHAQUE ESPECE PAR SITE
##     if (choixgraph["graphSite"] == 1)  # 13
##     {
##         nomPDF <- paste(nameWorkspace, "/FichiersSortie/", metrique, "_ParSite", unique(unitobs$type), "ParEsp", ".pdf", sep="")
##         pdf(nomPDF, encoding="ISOLatin1", family="URWHelvetica")

##         for (i in 1:length(especesPresentes))
##         {
##             spunit <- subset(listespunit, listespunit$code_espece==especesPresentes[i])
##             if (choixgraph["maxExclu"] == 1)   # 1
##             {
##                 spunit[, metrique][which(spunit[, metrique]>GraphPartMax*max(spunit[, metrique], na.rm=T))] <- NA
##             }
##             print(paste("NbObs pour", especesPresentes[i], " : ", length(spunit[, metrique])))
##             if (length(unique(listespunit[, metrique]))>1)
##             {
##                 ## on vérifie que la metrique a été calculée pour l'espèce sélectionnée
##                 if (length(unique(spunit[, metrique]))>=choixgraph["MinNbObs"])
##                 {
##                     par(mar=c(9, 4, 5, 1), mgp=c(3, 1, 9))
##                     boxplot(spunit[, metrique] ~spunit$site, data=spunit, varwidth = TRUE, ylab=metrique, las=2,
##                             main=paste(metrique, "\n pour l'espèces : ", unique(spunit$code_espece),
##                             " selon l'année \n"))

##                     if (choixgraph["NbObsOrange"] == 1)  # 9
##                     {
##                         nbObs <- tapply(spunit[, metrique], spunit$site, length)
##                         axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)), col.ticks="orange", col.axis = "orange", lty = 2, lwd = 0.5)
##                         legend("topleft", "Nombre d'enregistrement par Boxplot", cex =0.7, col="orange", text.col="orange", merge=FALSE)
##                     }
##                     Moyenne <- as.vector(tapply(spunit[, metrique], spunit$site, na.rm = T, mean))
##                     if (choixgraph["PtMoyenneBleu"] == 1)  # 10
##                     {
##                         points(Moyenne, pch=19, col="blue")
##                     }
##                     if (choixgraph["ChiffreMoyenneBleu"] == 1)  # 11
##                     {
##                         text(Moyenne+(Moyenne/10), col = "blue", cex = 0.8, labels=as.character(round(Moyenne, digits=Nbdecimales)))
##                     }
##                     abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")
##                     if (choixgraph["maxExclu"] == 1)   # 1
##                     {
##                         legend("top", "Enregistrements > 95% du maximum retirés", cex =0.7, col="red", text.col="red", merge=FALSE)
##                     }
##                 }
##             }
##         }  #fin du for pour chaque espèce
##         tkmessageBox(message=paste("vos graphiques par an seront enregistrés dans ", nomPDF, " à la racine de PAMPA"))
##         print("boucle POUR CHAQUE ESPECE PAR SITE réalisée")
##         ## ferme le PDF
##         dev.off()
##     }#fin de GRAPHIQUE POUR CHAQUE ESPECE PAR SITE


##     ## CAS DES GRAPHIQUE POUR CHAQUE ESPECE PAR AN et PAR SITE
##     if (choixgraph["graphAnSite"] == 1)  # 14
##     {
##         nomPDF <- paste(nameWorkspace, "/FichiersSortie/", metrique, "_ParSite", unique(unitobs$type), "ParEsp", ".pdf", sep="")
##         pdf(nomPDF, encoding="ISOLatin1", family="URWHelvetica")

##         for (i in 1:length(especesPresentes))
##         {
##             spunit <- subset(listespunit, listespunit$code_espece==especesPresentes[i])
##             if (choixgraph["maxExclu"] == 1)   # 1
##             {
##                 ## choix <- 1 #il faut que la variable choix de enlevermax soit à 1 pour que les max soient retirés
##                 spunit[, metrique][which(spunit[, metrique]>GraphPartMax*max(spunit[, metrique], na.rm=T))] <- NA
##             }

##             if (length(unique(listespunit[, metrique]))>1)
##             {
##                 ## on vérifie que la metrique a été calculée pour l'espèce sélectionnée
##                 if (length(unique(spunit[, metrique]))>1)
##                 {
##                     par(mar=c(15, 4, 5, 1), mgp=c(3, 1, 9))
##                     boxplot(spunit[, metrique] ~spunit$site+spunit$an, data=spunit,
##                             varwidth = TRUE, ylab=metrique, las=2, col=heat.colors(length(unique(spunit$site))),
##                             main=paste(metrique, "\n pour l'espèces : ", unique(spunit$code_espece), "\n selon le site et l'année"))

##                     if (choixgraph["NbObsOrange"] == 1)  # 9
##                     {
##                         nbObs <- tapply(spunit[, metrique], list(spunit$site, spunit$an), length)
##                         axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)), col.ticks="orange", col.axis = "orange", lty = 2, lwd = 0.5)
##                         legend("topleft", "Nombre d'enregistrement par Boxplot", cex =0.7, col="orange", text.col="orange", merge=FALSE)
##                     }
##                     Moyenne <- as.vector(tapply(spunit[, metrique], list(spunit$site, spunit$an), na.rm = T, mean))
##                     if (choixgraph["PtMoyenneBleu"] == 1)  # 10
##                     {
##                         points(Moyenne, pch=19, col="blue")
##                     }
##                     if (choixgraph["ChiffreMoyenneBleu"] == 1)  # 11
##                     {
##                         text(Moyenne+(Moyenne/10), col = "blue", cex = 0.8, labels=as.character(round(Moyenne, digits=Nbdecimales)))
##                     }
##                     abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")
##                     if (choixgraph["maxExclu"] == 1)   # 1
##                     {
##                         legend("top", "Enregistrements > 95% du maximum retirés", cex =0.7, col="red", text.col="red", merge=FALSE)
##                     }
##                 }
##             }
##         }  #fin du for pour chaque espèce
##         tkmessageBox(message=paste("vos graphiques par statut seront enregistrés dans ", nomPDF, " à la racine de PAMPA"))
##         print("boucle POUR CHAQUE ESPECE PAR SITE ET  PAR AN réalisée")
##         ## ferme le PDF
##         dev.off()

##     }#fin de GRAPHIQUE POUR CHAQUE ESPECE PAR SITE ET  PAR AN

##     ## CAS DES GRAPHIQUE POUR CHAQUE ESPECE PAR CARACT2
##     if (choixgraph["graphCarac"] == 1)  # 15
##     {
##         nomPDF <- paste(nameWorkspace, "/FichiersSortie/", metrique, "_ParCaract2", unique(unitobs$type), "ParEsp", ".pdf", sep="")
##         pdf(nomPDF, encoding="ISOLatin1", family="URWHelvetica")

##         for (i in 1:length(especesPresentes))
##         {
##             spunit <- subset(listespunit, listespunit$code_espece==especesPresentes[i])
##             if (choixgraph["maxExclu"] == 1)   # 1
##             {
##                 spunit[, metrique][which(spunit[, metrique]>GraphPartMax*max(spunit[, metrique], na.rm=T))] <- NA
##             }
##             print(paste("NbObs pour", especesPresentes[i], " : ", length(spunit[, metrique])))
##             if (length(unique(listespunit[, metrique]))>1)
##             {
##                 ## on vérifie que la metrique a été calculée pour l'espèce sélectionnée
##                 if (length(unique(spunit[, metrique]))>=choixgraph["MinNbObs"])
##                 {
##                     par(mar=c(9, 4, 5, 1), mgp=c(3, 1, 9))
##                     boxplot(spunit[, metrique] ~spunit$caracteristique_2, data=spunit, varwidth = TRUE, ylab=metrique,
##                             las=2, main=paste(metrique, "\n pour l'espèces : ", unique(spunit$code_espece),
##                                    " selon l'année \n"))

##                     if (choixgraph["NbObsOrange"] == 1)  # 9
##                     {
##                         nbObs <- tapply(spunit[, metrique], spunit$caracteristique_2, length)
##                         axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)), col.ticks="orange", col.axis = "orange", lty = 2, lwd = 0.5)
##                         legend("topleft", "Nombre d'enregistrement par Boxplot", cex =0.7, col="orange", text.col="orange", merge=FALSE)
##                     }
##                     Moyenne <- as.vector(tapply(spunit[, metrique], spunit$caracteristique_2, na.rm = T, mean))
##                     if (choixgraph["PtMoyenneBleu"] == 1)  # 10
##                     {
##                         points(Moyenne, pch=19, col="blue")
##                     }
##                     if (choixgraph["ChiffreMoyenneBleu"] == 1)  # 11
##                     {
##                         text(Moyenne+(Moyenne/10), col = "blue", cex = 0.8, labels=as.character(round(Moyenne, digits=Nbdecimales)))
##                     }
##                     abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")
##                     if (choixgraph["maxExclu"] == 1)   # 1
##                     {
##                         legend("top", "Enregistrements > 95% du maximum retirés", cex =0.7, col="red", text.col="red", merge=FALSE)
##                     }
##                 }
##             }
##         }  #fin du for pour chaque espèce
##         tkmessageBox(message=paste("vos graphiques par an seront enregistrés dans ", nomPDF, " à la racine de PAMPA"))
##         print("boucle POUR CHAQUE ESPECE PAR caracteristique_2 réalisée")
##         ## ferme le PDF
##         dev.off()
##     }#fin de GRAPHIQUE POUR CHAQUE ESPECE PAR CARACT2


##     ## CAS DES GRAPHIQUE POUR CHAQUE ESPECE PAR AN et PAR CARACT2
##     if (choixgraph["graphAnCarac"] == 1)  # 16
##     {
##         nomPDF <- paste(nameWorkspace, "/FichiersSortie/", metrique, "_ParCaract2", unique(unitobs$type), "ParEsp", ".pdf", sep="")
##         pdf(nomPDF, encoding="ISOLatin1", family="URWHelvetica")

##         for (i in 1:length(especesPresentes))
##         {
##             spunit <- subset(listespunit, listespunit$code_espece==especesPresentes[i])
##             if (choixgraph["maxExclu"] == 1)   # 1
##             {
##                 ## choix <- 1 #il faut que la variable choix de enlevermax soit à 1 pour que les max soient retirés
##                 spunit[, metrique][which(spunit[, metrique]>GraphPartMax*max(spunit[, metrique], na.rm=T))] <- NA
##             }

##             if (length(unique(listespunit[, metrique]))>1)
##             {
##                 ## on vérifie que la metrique a été calculée pour l'espèce sélectionnée
##                 if (length(unique(spunit[, metrique]))>1)
##                 {
##                     par(mar=c(15, 4, 5, 1), mgp=c(3, 1, 9))
##                     boxplot(spunit[, metrique] ~spunit$caracteristique_2+spunit$an, data=spunit,
##                             varwidth = TRUE, ylab=metrique, las=2, col=heat.colors(length(unique(spunit$caracteristique_2))),
##                             main=paste(metrique, "\n pour l'espèces : ", unique(spunit$code_espece), "\n selon caracteristique_2 et l'année"))

##                     if (choixgraph["NbObsOrange"] == 1)  # 9
##                     {
##                         nbObs <- tapply(spunit[, metrique], list(spunit$caracteristique_2, spunit$an), length)
##                         axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)), col.ticks="orange", col.axis = "orange", lty = 2, lwd = 0.5)
##                         legend("topleft", "Nombre d'enregistrement par Boxplot", cex =0.7, col="orange", text.col="orange", merge=FALSE)
##                     }
##                     Moyenne <- as.vector(tapply(spunit[, metrique], list(spunit$caracteristique_2, spunit$an), na.rm = T, mean))
##                     if (choixgraph["PtMoyenneBleu"] == 1)  # 10
##                     {
##                         points(Moyenne, pch=19, col="blue")
##                     }
##                     if (choixgraph["ChiffreMoyenneBleu"] == 1)  # 11
##                     {
##                         text(Moyenne+(Moyenne/10), col = "blue", cex = 0.8, labels=as.character(round(Moyenne, digits=Nbdecimales)))
##                     }
##                     abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")
##                     if (choixgraph["maxExclu"] == 1)   # 1
##                     {
##                         legend("top", "Enregistrements > 95% du maximum retirés", cex =0.7, col="red", text.col="red", merge=FALSE)
##                     }
##                 }
##             }
##         }  #fin du for pour chaque espèce
##         tkmessageBox(message=paste("vos graphiques par statut seront enregistrés dans ", nomPDF, " à la racine de PAMPA"))
##         print("boucle POUR CHAQUE ESPECE PAR caracteristique_2 ET  PAR AN réalisée")
##         ## ferme le PDF
##         dev.off()

##     }#fin de GRAPHIQUE POUR CHAQUE ESPECE PAR CARACT2 ET  PAR AN


##     if (choixgraph["graphAn"] == 1 || choixgraph["graphAnBiotope"] == 1 || choixgraph["graphAnStatut"] == 1 || choixgraph["graphAnBiotopeStatut"] == 1 || choixgraph["graphBiotope"] == 1 ||   # 2   # 3   # 4   # 5   # 6
##         choixgraph["graphStatut"] == 1 || choixgraph["graphBiotopeStatut"] == 1 || choixgraph["graphSite"] == 1 || choixgraph["graphAnSite"] == 1 || choixgraph["graphCarac"] == 1 ||   # 7  # 8  # 13  # 14  # 15
##         choixgraph["graphAnCarac"] == 1)              # Sûrement moyen de faire plus simple (avec une somme ?) [yr: 30/07/2010]  # 16
##     {
##         gestionMSGinfo.f("InfoPDFdansFichierSortie")
##     }else{
##         gestionMSGinfo.f("AucunPDFdansFichierSortie")
##     }
## }

################################################################################
## Nom    : GraphGroup1factUnitobs.f()
## Objet  : graphiques regroupant les observations sur une caractéristique des unités d'observations
## Input  : table "listespunit" et un code champ
## Output : boxplot
################################################################################

## [sup] [yr:12/01/2011]:

## GraphGroup1factUnitobs.f <- function()
## {
##     print("fonction GraphGroup1factUnitobs.f activée")

##     ## ###############################
##     ## pour la table listespunit    #
##     ## ###############################
##     matable <- "listespunit"

##     ## Choix du champ à représenter champobs
##     choixchamptable.f(matable)
##     print(champtrouve)
##     ## Choix du facteur de regroupement
##     choixunfacteurUnitobs.f()
##     print(fact)
##     objtable <- eval(parse(text=matable))
##     objtable[, fact] <- unitobs[, fact][match(objtable$unite_observation, unitobs$unite_observation)]
##     objtable$an <- unitobs$an[match(objtable$unite_observation, unitobs$unite_observation)]
##     objtable$statut <- unitobs$statut[match(objtable$unite_observation, unitobs$unite_observation)]
##     obsSansExtreme <- objtable
##     extremes.f()
##     ## Graphiques sur un champ de obs
##     obsSansExtreme[, champtrouve] <- EnleverMaxExtremes.f(obsSansExtreme[, champtrouve])    #  selon choix retourné par extremes.f()
##     print(head(obsSansExtreme))
##     TauxUnitobsCritereSansNA <- round(100*(length(unique(obsSansExtreme$unite_observation[is.na(obsSansExtreme[, fact])==FALSE]))  # [!!!]
##                                            / length(unique(obsSansExtreme$unite_observation))), digits=2)
##     TauxObsCritereSansNA <- round(100*(sum(table(obsSansExtreme[, fact]))/length(obsSansExtreme[, fact])), digits=2)
##     TauxRenseignement <- paste("Taux de renseignement \ndu champs ", fact, ":\n", TauxObsCritereSansNA,
##                                "% des observations\n", TauxUnitobsCritereSansNA, "% des unites d'observation\n")
##     if (length(unique(obsSansExtreme[, champtrouve]))>0)     #teste si il y a au moins un enregistrement
##     {
##         if (round(sum(table(obsSansExtreme[, fact]))/length(obsSansExtreme[, fact]), digits=2)>=0.5)
##         {
##             if (length(unique(obsSansExtreme[, fact]))>1)  #teste si il y a plus d'une valeur pour le regroupement dans le champs
##             {
##                 ## distribution des enregistrements groupés selon le critère

##                 x11(width=50, height=20, pointsize=10)
##                 par(mar=c(9, 4, 5, 1), mgp=c(3, 1, 9))
##                 boxplot(obsSansExtreme[, champtrouve] ~obsSansExtreme[, fact], data=obsSansExtreme, varwidth = TRUE,
##                         las=3, main=paste("Regroupement des ", champtrouve, " selon \"", fact, "\""))
##                 nbObs <- tapply(obsSansExtreme[, champtrouve], obsSansExtreme[, fact], length)
##                 axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)), col.ticks="orange", col.axis = "orange", lty = 2, lwd = 0.5)
##                 legend("topleft", "Nombre d'enregistrements par Boxplot", cex =0.7, col="orange", text.col="orange", merge=FALSE)
##                 Moyenne <- as.vector(tapply(obsSansExtreme[, champtrouve], obsSansExtreme[, fact], na.rm = T, mean))
##                 points(Moyenne, pch=19, col="blue")
##                 text(Moyenne+(Moyenne/10), col = "blue", cex = 0.8, labels=as.character(round(Moyenne, digits=1)))
##                 abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")
##                 ## boxplot(spunit[, champtrouve] ~spunit$an+spunit$code_espece, las=2, cex.names=0.6, main=paste("Nombre
##                 ## d'individus par espèce et par an pour le site ", si, "\n"))
##                 ## nbObs <- tapply(spunit[, champtrouve], list(spunit$an, spunit$code_espece), length)
##                 if (choix=="1")
##                 {
##                     legend("top", "Enregistrements > 95% du maximum retirés", cex =0.7, col="red", text.col="red", merge=FALSE)
##                 }
##                 legend("topleft", TauxRenseignement, cex =0.8, bty="n", text.col="red", merge=FALSE)

##                 ## distribution des enregistrements groupés selon le critère et le statut du site

##                 textstatut <- c("HR : Hors Réserve", "PP : Protection Partielle", "RE : En réserve")
##                 x11(width=120, height=50, pointsize=10)
##                 par(mar=c(8, 4, 5, 1), mgp=c(3, 1, 9))
##                 boxplot(obsSansExtreme[, champtrouve][obsSansExtreme[, champtrouve]!= 0]
##                         ~obsSansExtreme$statut[obsSansExtreme[, champtrouve]!= 0]+obsSansExtreme[, fact][obsSansExtreme[,
##                                                              champtrouve]!= 0], col=heat.colors(length(unique(obsSansExtreme$statut))), las=2,
##                         main=paste(champtrouve, " par enregistrement\n selon le statut et Regroupement selon \"", fact,
##                         "\"\n\n"))
##                 nbObs <- tapply(obsSansExtreme[, champtrouve][obsSansExtreme[, champtrouve]!= 0],
##                                 list(obsSansExtreme$statut[obsSansExtreme[, champtrouve]!= 0], obsSansExtreme[, fact][obsSansExtreme[,
##                                                                          champtrouve]!= 0]), length)
##                 legend("topleft", "Nombre d'enregistrement par Boxplot", cex =0.7, col="orange", text.col="orange", merge=FALSE)
##                 legend("topright", textstatut, col=heat.colors(length(unique(obsSansExtreme$statut))), pch = 15, cex =0.9, title="Statuts")
##                 axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)), col.ticks="orange", col.axis = "orange", lty = 2, lwd = 0.5)
##                 Moyenne <- as.vector(tapply(obsSansExtreme[, champtrouve][obsSansExtreme[, champtrouve]!= 0],
##                                             list(obsSansExtreme$statut[obsSansExtreme[, champtrouve]!= 0], obsSansExtreme[, fact][obsSansExtreme[,
##                                                                                      champtrouve]!= 0]), na.rm = T, mean))
##                 points(Moyenne, pch=19, col="blue")
##                 abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")
##                 abline(v = 0.5+(1:length(as.vector(unique(obsSansExtreme[, fact]))))*length(as.vector(unique(obsSansExtreme$statut))), col = "red")
##                 if (choix=="1")
##                 {
##                     legend("top", "Enregistrements > 95% du maximum retirés", cex =0.7, col="red", text.col="red", merge=FALSE)
##                 }
##                 legend("topleft", TauxRenseignement, cex =0.8, bty="n", col="red", text.col="red", merge=FALSE)

##                 ## distribution des enregistrements groupés selon le critère et l'année de l'observation

##                 x11(width=120, height=50, pointsize=10)
##                 par(mar=c(8, 4, 5, 1), mgp=c(3, 1, 9))
##                 boxplot(obsSansExtreme[, champtrouve][obsSansExtreme[, champtrouve]!= 0]
##                         ~obsSansExtreme$statut[obsSansExtreme[, champtrouve]!= 0]+obsSansExtreme$an[obsSansExtreme[,
##                                                              champtrouve]!= 0]+obsSansExtreme[, fact][obsSansExtreme[, champtrouve]!= 0],
##                         col=heat.colors(length(unique(obsSansExtreme$statut))), las=2,
##                         main=paste(champtrouve,
##                         " par enregistrement\n selon le statut puis l'année et Regroupement selon \"", fact, "\"\n\n"))
##                 nbObs <- tapply(obsSansExtreme[, champtrouve][obsSansExtreme[, champtrouve]!= 0],
##                                 list(obsSansExtreme$statut[obsSansExtreme[, champtrouve]!= 0],
##                                      obsSansExtreme$an[obsSansExtreme[, champtrouve]!= 0], obsSansExtreme[,
##                                                                      fact][obsSansExtreme[, champtrouve]!= 0]), length)
##                 legend("topleft", "Nombre d'enregistrement par Boxplot", cex =0.7, col="orange", text.col="orange", merge=FALSE)
##                 legend("topright", textstatut, col=heat.colors(length(unique(obsSansExtreme$statut))), pch = 15, cex =0.9, title="Statuts")
##                 axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)), col.ticks="orange", col.axis = "orange", lty = 2, lwd = 0.5, cex =0.7)
##                 Moyenne <- as.vector(tapply(obsSansExtreme[, champtrouve][obsSansExtreme[, champtrouve]!= 0],
##                                             list(obsSansExtreme$statut[obsSansExtreme[, champtrouve]!= 0],
##                                                  obsSansExtreme$an[obsSansExtreme[, champtrouve]!= 0],
##                                                  obsSansExtreme[, fact][obsSansExtreme[, champtrouve]!= 0]),
##                                             na.rm = T, mean))
##                 points(Moyenne, pch=19, col="blue")
##                 abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")
##                 abline(v = 0.5+(1:(length(as.vector(unique(obsSansExtreme$an))) *
##                        length(as.vector(unique(obsSansExtreme[, fact]))))) *
##                        length(as.vector(unique(obsSansExtreme$statut))), col = "red")
##                 if (choix=="1")
##                 {
##                     legend("top", "Enregistrements > 95% du maximum retirés", cex =0.7, col="red", text.col="red", merge=FALSE)
##                 }
##                 legend("topleft", TauxRenseignement, cex =0.8, col="red", bty="n", text.col="red", merge=FALSE)

##                 ## distribution des enregistrements groupés selon le critère et le statut l'année de l'observation

##                 x11(width=120, height=50, pointsize=10)
##                 par(mar=c(8, 4, 5, 1), mgp=c(3, 1, 9))
##                 boxplot(obsSansExtreme[, champtrouve][obsSansExtreme[, champtrouve]!= 0] ~
##                         obsSansExtreme$an[obsSansExtreme[, champtrouve]!= 0] +
##                         obsSansExtreme$statut[obsSansExtreme[, champtrouve]!= 0] +
##                         obsSansExtreme[, fact][obsSansExtreme[, champtrouve]!= 0],
##                         col=heat.colors(length(unique(obsSansExtreme$statut))*length(unique(obsSansExtreme$an))),
##                         las=2,
##                         main=paste(champtrouve, " par enregistrement\n selon l'année puis le statut et Regroupement selon \"", fact, "\"\n\n"))
##                 nbObs <- tapply(obsSansExtreme[, champtrouve][obsSansExtreme[, champtrouve]!= 0],
##                                 list(obsSansExtreme$an[obsSansExtreme[, champtrouve]!= 0],
##                                      obsSansExtreme$statut[obsSansExtreme[, champtrouve]!= 0],
##                                      obsSansExtreme[, fact][obsSansExtreme[, champtrouve]!= 0]),
##                                 length)
##                 legend("topleft", "Nombre d'enregistrement par Boxplot", cex =0.7, col="orange", text.col="orange", merge=FALSE)
##                 legend("topright", textstatut, col=heat.colors(length(unique(obsSansExtreme$statut))), pch = 15, cex =0.9, title="Statuts")
##                 axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)), col.ticks="orange", col.axis = "orange", lty = 2, lwd = 0.5, cex =0.7)
##                 Moyenne <- as.vector(tapply(obsSansExtreme[, champtrouve][obsSansExtreme[, champtrouve]!= 0],
##                                             list(obsSansExtreme$an[obsSansExtreme[, champtrouve]!= 0],
##                                                  obsSansExtreme$statut[obsSansExtreme[, champtrouve]!= 0],
##                                                  obsSansExtreme[, fact][obsSansExtreme[, champtrouve]!= 0]),
##                                             na.rm = TRUE, mean))
##                 points(Moyenne, pch=19, col="blue")
##                 abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")
##                 abline(v = 0.5+(1:(length(as.vector(unique(obsSansExtreme$statut)))*
##                        length(as.vector(unique(obsSansExtreme[, fact])))))*
##                        length(as.vector(unique(obsSansExtreme$an))), col = "red")
##                 if (choix=="1")
##                 {
##                     legend("top", "Enregistrements > 95% du maximum retirés", cex =0.7, col="red", text.col="red", merge=FALSE)
##                 }
##                 legend("topleft", TauxRenseignement, cex =0.8, col="red", bty="n", text.col="red", merge=FALSE)

##                 ## sélection d'espèces

##             }else{             #si pas plus d'une valeur
##                 tkmessageBox(message=paste("Le champ", fact, "contient toujours la même valeur :", unique(obsSansExtreme[, fact]), "\n regroupement inefficace, sélectionnez un autre facteur"), icon="info")
##                 gestionMSGerreur.f("UneSeuleValeurRegroupement")
##             }
##         }else{          #si moins de 50% du critère renseigné
##             tkmessageBox(message=paste("Graphique sans intérêt - plus de la moitié \n des enregistrements du critère de regroupement ", fact, "ne sont pas renseignés (NA)"))
##             gestionMSGerreur.f("CritereMalRenseigne50")
##         }
##     }else{
##         tkmessageBox(message="Graphique impossible - pas d'enregistrements dans votre sélection")
##         gestionMSGerreur.f("ZeroEnregistrement")
##     }
##     choix <- "0"
## }  #fin de GraphGroup1factUnitobs.f

################################################################################
## Nom    : GraphGroup2factUnitobs.f()
## Objet  : graphiques regroupant les observations sur une caractéristique du référentiel espèce
## Input  : table "listespunit" et un code champ
## Output : boxplot
################################################################################

## [sup] [yr:12/01/2011]:

## GraphGroup2factUnitobs.f <- function ()
## {

##     print("fonction GraphGroup2factUnitobs.f activée")

##     ## ###############################
##     ## pour la table listespunit    #
##     ## ###############################
##     matable <- "listespunit"

##     ## Choix du champ à représenter champobs
##     choixchamptable.f(matable)
##     print(champtrouve)
##     ## Choix du facteur de regroupement
##     choixDeuxFacteursUnitobs.f()
##     print(fact21)
##     print(fact22)
##     objtable <- eval(parse(text=matable))
##     objtable[, fact21] <- unitobs[, fact21][match(objtable$unite_observation, unitobs$unite_observation)]
##     objtable[, fact22] <- unitobs[, fact22][match(objtable$unite_observation, unitobs$unite_observation)]
##     objtable$an <- unitobs$an[match(objtable$unite_observation, unitobs$unite_observation)]
##     objtable$statut <- unitobs$statut[match(objtable$unite_observation, unitobs$unite_observation)]
##     obsSansExtreme <- objtable
##     extremes.f()
##     ## Graphiques sur un champ de obs
##     obsSansExtreme[, champtrouve] <- EnleverMaxExtremes.f(obsSansExtreme[, champtrouve])    #  selon choix retourné par extremes.f()
##     print(head(obsSansExtreme))
##     TauxUnitobsCritereSansNA1 <- round(100*(length(unique(obsSansExtreme$unite_observation[is.na(obsSansExtreme[, fact21])==FALSE]))/length(unique(obsSansExtreme$unite_observation))), digits=2)
##     TauxUnitobsCritereSansNA2 <- round(100*(length(unique(obsSansExtreme$unite_observation[is.na(obsSansExtreme[, fact22])==FALSE]))/length(unique(obsSansExtreme$unite_observation))), digits=2)
##     TauxObsCritereSansNA1 <- round(100*(sum(table(obsSansExtreme[, fact21]))/length(obsSansExtreme[, fact21])), digits=2)
##     TauxObsCritereSansNA2 <- round(100*(sum(table(obsSansExtreme[, fact22]))/length(obsSansExtreme[, fact22])), digits=2)
##     TauxRenseignement <- paste("Taux de renseignement \ndu champs ", fact21, ":\n", TauxObsCritereSansNA1,
##                                "% des observations \n", TauxUnitobsCritereSansNA1, "% des espèces\n", "du champs ", fact22, ":\n",
##                                TauxObsCritereSansNA2, "% des observations\n", TauxUnitobsCritereSansNA2, "% des espèces\n")
##     print(TauxRenseignement)
##     if (length(unique(obsSansExtreme[, champtrouve]))>0)     #teste si il y a au moins un enregistrement
##     {
##         if (round(sum(table(list(obsSansExtreme[, fact21], obsSansExtreme[, fact22])))/length(list(obsSansExtreme[, fact21], obsSansExtreme[, fact22])), digits=2)>=0.5)
##         {
##             if (length(unique(list(obsSansExtreme[, fact21], obsSansExtreme[, fact22])))>1)  #teste si il y a plus d'une valeur pour le regroupement dans le champs
##             {
##                 ## distribution des enregistrements groupés selon fact21 , fact22

##                 x11(width=50, height=20, pointsize=10)
##                 par(mar=c(9, 4, 5, 1), mgp=c(3, 1, 9))
##                 boxplot(obsSansExtreme[, champtrouve] ~obsSansExtreme[, fact21]+obsSansExtreme[, fact22], data=obsSansExtreme, varwidth = TRUE, las=3, main=paste("Regroupement des ", champtrouve, " selon \"", fact21, "\"et \"", fact22, "\""))
##                 nbObs <- tapply(obsSansExtreme[, champtrouve], list(obsSansExtreme[, fact21], obsSansExtreme[, fact22]), length)
##                 axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)), col.ticks="orange", col.axis = "orange", lty = 2, lwd = 0.5)
##                 legend("topleft", "Nombre d'enregistrements par Boxplot", cex =0.7, col="orange", text.col="orange", merge=FALSE)
##                 Moyenne <- as.vector(tapply(obsSansExtreme[, champtrouve], list(obsSansExtreme[, fact21], obsSansExtreme[, fact22]), na.rm = T, mean))
##                 points(Moyenne, pch=19, col="blue")
##                 text(Moyenne+(Moyenne/10), col = "blue", cex = 0.8, labels=as.character(round(Moyenne, digits=1)))
##                 abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")
##                 ## boxplot(spunit[, champtrouve] ~spunit$an+spunit$code_espece, las=2, cex.names=0.6, main=paste("Nombre d'individus par espèce et par an pour le site ", si, "\n"))
##                 ## nbObs <- tapply(spunit[, champtrouve], list(spunit$an, spunit$code_espece), length)
##                 if (choix=="1")
##                 {
##                     legend("top", "Enregistrements > 95% du maximum retirés", cex =0.7, col="red", text.col="red", merge=FALSE)
##                 }
##                 legend("topleft", TauxRenseignement, cex =0.8, col="red", bty="n", text.col="red", merge=FALSE)
##                 print(paste("graphique de distribution des enregistrements groupés selon", fact21 , fact22, " réalisé"))

##                 ## distribution des enregistrements groupés selon le statut du site et fact21 , fact22

##                 textstatut <- c("HR : Hors Réserve", "PP : Protection Partielle", "RE : En réserve")
##                 x11(width=120, height=50, pointsize=10)
##                 par(mar=c(8, 4, 5, 1), mgp=c(3, 1, 9))
##                 boxplot(obsSansExtreme[, champtrouve][obsSansExtreme[, champtrouve]!= 0] ~obsSansExtreme$statut[obsSansExtreme[, champtrouve]!= 0]+obsSansExtreme[, fact21][obsSansExtreme[, champtrouve]!= 0]+obsSansExtreme[, fact22][obsSansExtreme[, champtrouve]!= 0], col=heat.colors(length(unique(obsSansExtreme$statut))), las=2, main=paste(champtrouve, " par enregistrement\n selon le statut et Regroupement selon \"", fact21, "\"et \"", fact22, "\"\n"))
##                 nbObs <- tapply(obsSansExtreme[, champtrouve][obsSansExtreme[, champtrouve]!= 0], list(obsSansExtreme$statut[obsSansExtreme[, champtrouve]!= 0], obsSansExtreme[, fact21][obsSansExtreme[, champtrouve]!= 0], obsSansExtreme[, fact22][obsSansExtreme[, champtrouve]!= 0]), length)
##                 legend("topleft", "Nombre d'enregistrement par Boxplot", cex =0.7, col="orange", text.col="orange", merge=FALSE)
##                 legend("topright", textstatut, col=heat.colors(length(unique(obsSansExtreme$statut))), pch = 15, cex =0.9, title="Statuts")
##                 axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)), col.ticks="orange", col.axis = "orange", lty = 2, lwd = 0.5)
##                 Moyenne <- as.vector(tapply(obsSansExtreme[, champtrouve][obsSansExtreme[, champtrouve]!= 0], list(obsSansExtreme$statut[obsSansExtreme[, champtrouve]!= 0], obsSansExtreme[, fact21][obsSansExtreme[, champtrouve]!= 0], obsSansExtreme[, fact22][obsSansExtreme[, champtrouve]!= 0]), na.rm = T, mean))
##                 points(Moyenne, pch=19, col="blue")
##                 if (length(Moyenne)>150)
##                 {
##                     legend("center", "plus de 150 possibilités de boxplot\nFaites une sélection préalable sur votre jeux de données", cex =1, col="blue", text.col="blue", merge=FALSE)
##                     print("plus de 150 possibilités de boxplot\nFaites une sélection préalable dans votre jeu de données\n...")
##                 }else{
##                     abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")
##                     abline(v = 0.5+(1:length(as.vector(unique(list(obsSansExtreme[, fact21], obsSansExtreme[, fact22])))))*length(as.vector(unique(obsSansExtreme$statut))), col = "red")
##                 }
##                 if (choix=="1")
##                 {
##                     legend("top", "Enregistrements > 95% du maximum retirés", cex =0.7, col="red", text.col="red", merge=FALSE)
##                 }
##                 legend("topleft", TauxRenseignement, cex =0.8, col="red", bty="n", text.col="red", merge=FALSE)
##                 print(paste("graphique de distribution des enregistrements groupés selon le statut du site et", fact21 , fact22, " réalisé"))

##                 ## distribution des enregistrements groupés selon l'année de l'observation  et fact21 , fact22

##                 x11(width=120, height=50, pointsize=10)
##                 par(mar=c(8, 4, 5, 1), mgp=c(3, 1, 9))
##                 boxplot(obsSansExtreme[, champtrouve][obsSansExtreme[, champtrouve]!= 0] ~obsSansExtreme$statut[obsSansExtreme[, champtrouve]!= 0]+obsSansExtreme$an[obsSansExtreme[, champtrouve]!= 0]+obsSansExtreme[, fact21][obsSansExtreme[, champtrouve]!= 0]+obsSansExtreme[, fact22][obsSansExtreme[, champtrouve]!= 0], col=heat.colors(length(unique(obsSansExtreme$statut))), las=2, main=paste(champtrouve, " par enregistrement\n selon le statut puis l'année et Regroupement selon \"", fact21, "\"et \"", fact22, "\"\n"))
##                 nbObs <- tapply(obsSansExtreme[, champtrouve][obsSansExtreme[, champtrouve]!= 0], list(obsSansExtreme$statut[obsSansExtreme[, champtrouve]!= 0], obsSansExtreme$an[obsSansExtreme[, champtrouve]!= 0], obsSansExtreme[, fact21][obsSansExtreme[, champtrouve]!= 0], obsSansExtreme[, fact22][obsSansExtreme[, champtrouve]!= 0]), length)
##                 legend("topleft", "Nombre d'enregistrement par Boxplot", cex =0.7, col="orange", text.col="orange", merge=FALSE)
##                 legend("topright", textstatut, col=heat.colors(length(unique(obsSansExtreme$statut))), pch = 15, cex =0.9, title="Statuts")
##                 axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)), col.ticks="orange", col.axis = "orange", lty = 2, lwd = 0.5, cex =0.7)
##                 Moyenne <- as.vector(tapply(obsSansExtreme[, champtrouve][obsSansExtreme[, champtrouve]!= 0], list(obsSansExtreme$statut[obsSansExtreme[, champtrouve]!= 0], obsSansExtreme$an[obsSansExtreme[, champtrouve]!= 0], obsSansExtreme[, fact21][obsSansExtreme[, champtrouve]!= 0], obsSansExtreme[, fact22][obsSansExtreme[, champtrouve]!= 0]), na.rm = T, mean))
##                 points(Moyenne, pch=19, col="blue")
##                 if (length(Moyenne)>150)
##                 {
##                     legend("center", "plus de 150 possibilités de boxplot\nFaites une sélection préalable sur votre jeux de données", cex =1, col="blue", text.col="blue", merge=FALSE)
##                     print("plus de 150 possibilités de boxplot\nFaites une sélection préalable dans votre jeu de données\n...")
##                 }else{
##                     abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")
##                     abline(v = 0.5+(1:(length(as.vector(unique(obsSansExtreme$an)))*length(as.vector(unique(list(obsSansExtreme[, fact21][obsSansExtreme[, champtrouve]!= 0], obsSansExtreme[, fact22][obsSansExtreme[, champtrouve]!= 0]))))))*length(as.vector(unique(obsSansExtreme$statut))), col = "red")
##                 }
##                 if (choix=="1")
##                 {
##                     legend("top", "Enregistrements > 95% du maximum retirés", cex =0.7, col="red", text.col="red", merge=FALSE)
##                 }
##                 legend("topleft", TauxRenseignement, cex =0.8, col="red", bty="n", text.col="red", merge=FALSE)
##                 print(paste("graphique de distribution des enregistrements groupés selon l'année de l'observation  et", fact21 , fact22, " réalisé"))

##                 ## distribution des enregistrements groupés selon le statut, l'année de l'observation  et fact21 , fact22

##                 x11(width=120, height=50, pointsize=10)
##                 par(mar=c(8, 4, 5, 1), mgp=c(3, 1, 9))
##                 boxplot(obsSansExtreme[, champtrouve][obsSansExtreme[, champtrouve]!= 0] ~obsSansExtreme$an[obsSansExtreme[, champtrouve]!= 0]+obsSansExtreme$statut[obsSansExtreme[, champtrouve]!= 0]+obsSansExtreme[, fact21][obsSansExtreme[, champtrouve]!= 0]+obsSansExtreme[, fact22][obsSansExtreme[, champtrouve]!= 0], col=heat.colors(length(unique(obsSansExtreme$statut))*length(unique(obsSansExtreme$an))), las=2, main=paste(champtrouve, " par enregistrement\n selon l'année puis le statut et Regroupement selon \"", fact21, "\"et \"", fact22, "\"\n"))
##                 nbObs <- tapply(obsSansExtreme[, champtrouve][obsSansExtreme[, champtrouve]!= 0], list(obsSansExtreme$an[obsSansExtreme[, champtrouve]!= 0], obsSansExtreme$statut[obsSansExtreme[, champtrouve]!= 0], obsSansExtreme[, fact21][obsSansExtreme[, champtrouve]!= 0], obsSansExtreme[, fact22][obsSansExtreme[, champtrouve]!= 0]), length)
##                 legend("topleft", "Nombre d'enregistrement par Boxplot", cex =0.7, col="orange", text.col="orange", merge=FALSE)
##                 legend("topright", textstatut, col=heat.colors(length(unique(obsSansExtreme$statut))), pch = 15, cex =0.9, title="Statuts")
##                 axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)), col.ticks="orange", col.axis = "orange", lty = 2, lwd = 0.5, cex =0.7)
##                 Moyenne <- as.vector(tapply(obsSansExtreme[, champtrouve][obsSansExtreme[, champtrouve]!= 0], list(obsSansExtreme$an[obsSansExtreme[, champtrouve]!= 0], obsSansExtreme$statut[obsSansExtreme[, champtrouve]!= 0], obsSansExtreme[, fact21][obsSansExtreme[, champtrouve]!= 0], obsSansExtreme[, fact22][obsSansExtreme[, champtrouve]!= 0]), na.rm = T, mean))
##                 points(Moyenne, pch=19, col="blue")
##                 if (length(Moyenne)>150)
##                 {
##                     legend("center", "plus de 150 possibilités de boxplot\nFaites une sélection préalable sur votre jeux de données", cex =1, col="blue", text.col="blue", merge=FALSE)
##                     print("plus de 150 possibilités de boxplot\nFaites une sélection préalable dans votre jeu de données\n...")
##                 }else{
##                     abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")
##                     abline(v = 0.5+(1:(length(as.vector(unique(obsSansExtreme$statut)))*length(as.vector(unique(list(obsSansExtreme[, fact21][obsSansExtreme[, champtrouve]!= 0], obsSansExtreme[, fact22][obsSansExtreme[, champtrouve]!= 0]))))))*length(as.vector(unique(obsSansExtreme$an))), col = "red")
##                 }
##                 if (choix=="1")
##                 {
##                     legend("top", "Enregistrements > 95% du maximum retirés", cex =0.7, col="red", text.col="red", merge=FALSE)
##                 }
##                 legend("topleft", TauxRenseignement, cex =0.8, col="red", bty="n", text.col="red", merge=FALSE)
##                 print(paste("graphique de distribution des enregistrements le statut, l'année de l'observation  et", fact21 , fact22, " réalisé"))

##                 ## évolution de champtrouvé trouve en fonction des deux facteurs
##                 x11(width=12, height=8, pointsize=12)
##                 par(mar=c(5, 6, 4, 1))
##                 interaction.plot(obsSansExtreme[, fact21], obsSansExtreme[, fact22], obsSansExtreme[, champtrouve],
##                                  lwd=2, col=cl[seq(550, (550+(4*(length(split(obsSansExtreme, obsSansExtreme[, fact21]))-1))), by=4)],
##                                  type="b", fun=mean, trace.label = fact21, xlab=fact22, ylab=champtrouve,
##                                  main=paste("Evolution des valeurs de ", champtrouve, " par", fact21))
##             }else{             #si pas plus d'une valeur
##                 tkmessageBox(message=paste("Les champs", fact21, "et", fact22, "contiennent toujours la même valeur :", unique(obsSansExtreme[, fact21][, fact22]), "\n regroupement inefficace, sélectionnez un autre facteur"), icon="info")
##                 gestionMSGerreur.f("UneSeuleValeurRegroupement")
##             }
##         }else{          #si moins de 50% du critère renseigné
##             tkmessageBox(message=paste("Graphique sans intérêt - plus de la moitié \n des enregistrements des critères de regroupement ", fact21, "et", fact22, "ne sont pas renseignés (NA)"))
##             gestionMSGerreur.f("CritereMalRenseigne50")
##         }
##     }else{
##         tkmessageBox(message="Graphique impossible - pas d'enregistrements dans votre sélection")
##         gestionMSGerreur.f("ZeroEnregistrement")
##     }
##     choix <- "0"
## }         #fin de GraphGroup2factUnitobs.f

################################################################################
## Nom    : GraphGroup1factEsp.f()
## Objet  : graphiques regroupant les observations sur une caractéristique du référentiel espèce
## Input  : table "listespunit" et un code champ
## Output : boxplot
################################################################################

## [sup] [yr:12/01/2011]:

## GraphGroup1factEsp.f <- function ()
## {

##     print("fonction GraphGroup1factEsp.f activée")
##     ## ###############################
##     ## pour la table obs    #
##     ## ###############################
##     ## matable="obs"

##     ## ###############################
##     ## pour la table listespunit    #
##     ## ###############################
##     matable <- "listespunit"

##     ## Choix du champ à représenter champobs
##     choixchamptable.f(matable)
##     print(champtrouve)
##     ## Choix du facteur de regroupement
##     critereespref.f()
##     print(factesp)
##     objtable <- eval(parse(text=matable))
##     objtable[, factesp] <- especes[, factesp][match(objtable$code_espece, especes$code_espece)]
##     objtable$an <- unitobs$an[match(objtable$unite_observation, unitobs$unite_observation)]
##     objtable$statut <- unitobs$statut[match(objtable$unite_observation, unitobs$unite_observation)]
##     obsSansExtreme <- objtable
##     extremes.f()
##     ## Graphiques sur un champ de obs
##     obsSansExtreme[, champtrouve] <- EnleverMaxExtremes.f(obsSansExtreme[, champtrouve])    #  selon choix retourné par extremes.f()
##     print(head(obsSansExtreme))
##     TauxEspCritereSansNA <- round(100*(length(unique(obsSansExtreme$code_espece[is.na(obsSansExtreme[, factesp])==FALSE]))/length(unique(obsSansExtreme$code_espece))), digits=2)
##     TauxObsCritereSansNA <- round(100*(sum(table(obsSansExtreme[, factesp]))/length(obsSansExtreme[, factesp])), digits=2)
##     TauxRenseignement <- paste("Taux de renseignement \ndu champs ", factesp, ":\n", TauxObsCritereSansNA, "% des observations\n", TauxEspCritereSansNA, "% des espèces\n")
##     if (length(unique(obsSansExtreme[, champtrouve]))>0)     #teste si il y a au moins un enregistrement
##     {
##         if (round(sum(table(obsSansExtreme[, factesp]))/length(obsSansExtreme[, factesp]), digits=2)>=0.5)
##         {
##             if (length(unique(obsSansExtreme[, factesp]))>1)  #teste si il y a plus d'une valeur pour le regroupement dans le champs
##             {
##                 ## distribution des enregistrements groupés selon le critère

##                 x11(width=50, height=20, pointsize=10)
##                 par(mar=c(9, 4, 5, 1), mgp=c(3, 1, 9))
##                 boxplot(obsSansExtreme[, champtrouve] ~obsSansExtreme[, factesp], data=obsSansExtreme, varwidth = TRUE, las=3, main=paste("Regroupement des ", champtrouve, " selon \"", factesp, "\""))
##                 nbObs <- tapply(obsSansExtreme[, champtrouve], obsSansExtreme[, factesp], length)
##                 axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)), col.ticks="orange", col.axis = "orange", lty = 2, lwd = 0.5)
##                 legend("topleft", "Nombre d'enregistrements par Boxplot", cex =0.7, col="orange", text.col="orange", merge=FALSE)
##                 Moyenne <- as.vector(tapply(obsSansExtreme[, champtrouve], obsSansExtreme[, factesp], na.rm = T, mean))
##                 points(Moyenne, pch=19, col="blue")
##                 text(Moyenne+(Moyenne/10), col = "blue", cex = 0.8, labels=as.character(round(Moyenne, digits=1)))
##                 abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")
##                 ## boxplot(spunit[, champtrouve] ~spunit$an+spunit$code_espece, las=2, cex.names=0.6, main=paste("Nombre d'individus par espèce et par an pour le site ", si, "\n"))
##                 ## nbObs <- tapply(spunit[, champtrouve], list(spunit$an, spunit$code_espece), length)
##                 if (choix=="1")
##                 {
##                     legend("top", "Enregistrements > 95% du maximum retirés", cex =0.7, col="red", text.col="red", merge=FALSE)
##                 }
##                 legend("topleft", TauxRenseignement, cex =0.8, bty="n", text.col="red", merge=FALSE)

##                 ## distribution des enregistrements groupés selon le critère et le statut du site

##                 textstatut <- c("HR : Hors Réserve", "PP : Protection Partielle", "RE : En réserve")
##                 x11(width=120, height=50, pointsize=10)
##                 par(mar=c(8, 4, 5, 1), mgp=c(3, 1, 9))
##                 boxplot(obsSansExtreme[, champtrouve][obsSansExtreme[, champtrouve]!= 0] ~obsSansExtreme$statut[obsSansExtreme[, champtrouve]!= 0]+obsSansExtreme[, factesp][obsSansExtreme[, champtrouve]!= 0], col=heat.colors(length(unique(obsSansExtreme$statut))), las=2, main=paste(champtrouve, " par enregistrement\n selon le statut et Regroupement selon \"", factesp, "\"\n\n"))
##                 nbObs <- tapply(obsSansExtreme[, champtrouve][obsSansExtreme[, champtrouve]!= 0], list(obsSansExtreme$statut[obsSansExtreme[, champtrouve]!= 0], obsSansExtreme[, factesp][obsSansExtreme[, champtrouve]!= 0]), length)
##                 legend("topleft", "Nombre d'enregistrement par Boxplot", cex =0.7, col="orange", text.col="orange", merge=FALSE)
##                 legend("topright", textstatut, col=heat.colors(length(unique(obsSansExtreme$statut))), pch = 15, cex =0.9, title="Statuts")
##                 axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)), col.ticks="orange", col.axis = "orange", lty = 2, lwd = 0.5)
##                 Moyenne <- as.vector(tapply(obsSansExtreme[, champtrouve][obsSansExtreme[, champtrouve]!= 0], list(obsSansExtreme$statut[obsSansExtreme[, champtrouve]!= 0], obsSansExtreme[, factesp][obsSansExtreme[, champtrouve]!= 0]), na.rm = T, mean))
##                 points(Moyenne, pch=19, col="blue")
##                 abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")
##                 abline(v = 0.5+(1:length(as.vector(unique(obsSansExtreme[, factesp]))))*length(as.vector(unique(obsSansExtreme$statut))), col = "red")
##                 if (choix=="1")
##                 {
##                     legend("top", "Enregistrements > 95% du maximum retirés", cex =0.7, col="red", text.col="red", merge=FALSE)
##                 }
##                 legend("topleft", TauxRenseignement, cex =0.8, bty="n", col="red", text.col="red", merge=FALSE)

##                 ## distribution des enregistrements groupés selon le critère et l'année de l'observation

##                 x11(width=120, height=50, pointsize=10)
##                 par(mar=c(8, 4, 5, 1), mgp=c(3, 1, 9))
##                 boxplot(obsSansExtreme[, champtrouve][obsSansExtreme[, champtrouve]!= 0] ~obsSansExtreme$statut[obsSansExtreme[, champtrouve]!= 0]+obsSansExtreme$an[obsSansExtreme[, champtrouve]!= 0]+obsSansExtreme[, factesp][obsSansExtreme[, champtrouve]!= 0], col=heat.colors(length(unique(obsSansExtreme$statut))), las=2, main=paste(champtrouve, " par enregistrement\n selon le statut puis l'année et Regroupement selon \"", factesp, "\"\n\n"))
##                 nbObs <- tapply(obsSansExtreme[, champtrouve][obsSansExtreme[, champtrouve]!= 0], list(obsSansExtreme$statut[obsSansExtreme[, champtrouve]!= 0], obsSansExtreme$an[obsSansExtreme[, champtrouve]!= 0], obsSansExtreme[, factesp][obsSansExtreme[, champtrouve]!= 0]), length)
##                 legend("topleft", "Nombre d'enregistrement par Boxplot", cex =0.7, col="orange", text.col="orange", merge=FALSE)
##                 legend("topright", textstatut, col=heat.colors(length(unique(obsSansExtreme$statut))), pch = 15, cex =0.9, title="Statuts")
##                 axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)), col.ticks="orange", col.axis = "orange", lty = 2, lwd = 0.5, cex =0.7)
##                 Moyenne <- as.vector(tapply(obsSansExtreme[, champtrouve][obsSansExtreme[, champtrouve]!= 0], list(obsSansExtreme$statut[obsSansExtreme[, champtrouve]!= 0], obsSansExtreme$an[obsSansExtreme[, champtrouve]!= 0], obsSansExtreme[, factesp][obsSansExtreme[, champtrouve]!= 0]), na.rm = T, mean))
##                 points(Moyenne, pch=19, col="blue")
##                 abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")
##                 abline(v = 0.5+(1:(length(as.vector(unique(obsSansExtreme$an)))*length(as.vector(unique(obsSansExtreme[, factesp])))))*length(as.vector(unique(obsSansExtreme$statut))), col = "red")
##                 if (choix=="1")
##                 {
##                     legend("top", "Enregistrements > 95% du maximum retirés", cex =0.7, col="red", text.col="red", merge=FALSE)
##                 }
##                 legend("topleft", TauxRenseignement, cex =0.8, col="red", bty="n", text.col="red", merge=FALSE)

##                 ## distribution des enregistrements groupés selon le critère et le statut l'année de l'observation

##                 x11(width=120, height=50, pointsize=10)
##                 par(mar=c(8, 4, 5, 1), mgp=c(3, 1, 9))
##                 boxplot(obsSansExtreme[, champtrouve][obsSansExtreme[, champtrouve]!= 0] ~obsSansExtreme$an[obsSansExtreme[, champtrouve]!= 0]+obsSansExtreme$statut[obsSansExtreme[, champtrouve]!= 0]+obsSansExtreme[, factesp][obsSansExtreme[, champtrouve]!= 0], col=heat.colors(length(unique(obsSansExtreme$statut))*length(unique(obsSansExtreme$an))), las=2, main=paste(champtrouve, " par enregistrement\n selon l'année puis le statut et Regroupement selon \"", factesp, "\"\n\n"))
##                 nbObs <- tapply(obsSansExtreme[, champtrouve][obsSansExtreme[, champtrouve]!= 0], list(obsSansExtreme$an[obsSansExtreme[, champtrouve]!= 0], obsSansExtreme$statut[obsSansExtreme[, champtrouve]!= 0], obsSansExtreme[, factesp][obsSansExtreme[, champtrouve]!= 0]), length)
##                 legend("topleft", "Nombre d'enregistrement par Boxplot", cex =0.7, col="orange", text.col="orange", merge=FALSE)
##                 legend("topright", textstatut, col=heat.colors(length(unique(obsSansExtreme$statut))), pch = 15, cex =0.9, title="Statuts")
##                 axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)), col.ticks="orange", col.axis = "orange", lty = 2, lwd = 0.5, cex =0.7)
##                 Moyenne <- as.vector(tapply(obsSansExtreme[, champtrouve][obsSansExtreme[, champtrouve]!= 0], list(obsSansExtreme$an[obsSansExtreme[, champtrouve]!= 0], obsSansExtreme$statut[obsSansExtreme[, champtrouve]!= 0], obsSansExtreme[, factesp][obsSansExtreme[, champtrouve]!= 0]), na.rm = T, mean))
##                 points(Moyenne, pch=19, col="blue")
##                 abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")
##                 abline(v = 0.5+(1:(length(as.vector(unique(obsSansExtreme$statut)))*length(as.vector(unique(obsSansExtreme[, factesp])))))*length(as.vector(unique(obsSansExtreme$an))), col = "red")
##                 if (choix=="1")
##                 {
##                     legend("top", "Enregistrements > 95% du maximum retirés", cex =0.7, col="red", text.col="red", merge=FALSE)
##                 }
##                 legend("topleft", TauxRenseignement, cex =0.8, col="red", bty="n", text.col="red", merge=FALSE)

##                 interaction.plot(obsSansExtreme$an, obsSansExtreme[, factesp], obsSansExtreme[, champtrouve], lwd=2, col=cl[seq(550, (550+(4*(length(split(obsSansExtreme, obsSansExtreme$an))-1))), by=4)], type="b",
##                                  fun=mean, trace.label = factesp, xlab="Annee", ylab=champtrouve, main=paste("Evolution des valeurs de ", champtrouve, " par ", factesp))

##             }else{             #si pas plus d'une valeur
##                 tkmessageBox(message=paste("Le champ", factesp, "contient toujours la même valeur :", unique(obsSansExtreme[, factesp]), "\n regroupement inefficace, sélectionnez un autre facteur"), icon="info")
##                 gestionMSGerreur.f("UneSeuleValeurRegroupement")
##             }
##         }else{          #si moins de 50% du critère renseigné
##             tkmessageBox(message=paste("Graphique sans intérêt - plus de la moitié \n des enregistrements du critère de regroupement ", factesp, "ne sont pas renseignés (NA)"))
##             gestionMSGerreur.f("CritereMalRenseigne50")
##         }
##     }else{
##         tkmessageBox(message="Graphique impossible - pas d'enregistrements dans votre sélection")
##         gestionMSGerreur.f("ZeroEnregistrement")
##     }
##     choix <- "0"
## }
################################################################################
## Nom    : GraphGroup2factEsp.f()
## Objet  : graphiques regroupant les observations sur une caractéristique du référentiel espèce
## Input  : table "listespunit" et un code champ
## Output : boxplot
################################################################################

## [sup] [yr:12/01/2011]:

## GraphGroup2factEsp.f <- function ()
## {

##     print("fonction GraphGroup2factEsp.f activée")

##     ## ###############################
##     ## pour la table obs    #
##     ## ###############################
##     ## matable="obs"

##     ## ###############################
##     ## pour la table listespunit    #
##     ## ###############################
##     matable <- "listespunit"

##     ## ###############################
##     ## pour la table listespunit    #
##     ## ###############################
##     ## matable="unitobs"

##     ## Choix du champ à représenter champobs
##     choixchamptable.f(matable)
##     print(champtrouve)
##     ## Choix du facteur de regroupement
##     critereespref.f()
##     critere2espref.f()


##     print(factesp)
##     print(factesp2)

##     objtable <- eval(parse(text=matable))
##     objtable[, factesp] <- especes[, factesp][match(objtable$code_espece, especes$code_espece)]
##     objtable[, factesp2] <- especes[, factesp2][match(objtable$code_espece, especes$code_espece)]
##     objtable$an <- unitobs$an[match(objtable$unite_observation, unitobs$unite_observation)]
##     objtable$statut <- unitobs$statut[match(objtable$unite_observation, unitobs$unite_observation)]
##     obsSansExtreme <- objtable
##     extremes.f()
##     ## Graphiques sur un champ de obs
##     obsSansExtreme[, champtrouve] <- EnleverMaxExtremes.f(obsSansExtreme[, champtrouve])    #  selon choix retourné par extremes.f()
##     print(head(obsSansExtreme))
##     TauxEspCritereSansNA1 <- round(100*(length(unique(obsSansExtreme$code_espece[is.na(obsSansExtreme[, factesp])==FALSE]))/length(unique(obsSansExtreme$code_espece))), digits=2)
##     TauxEspCritereSansNA2 <- round(100*(length(unique(obsSansExtreme$code_espece[is.na(obsSansExtreme[, factesp2])==FALSE]))/length(unique(obsSansExtreme$code_espece))), digits=2)
##     TauxObsCritereSansNA1 <- round(100*(sum(table(obsSansExtreme[, factesp]))/length(obsSansExtreme[, factesp])), digits=2)
##     TauxObsCritereSansNA2 <- round(100*(sum(table(obsSansExtreme[, factesp2]))/length(obsSansExtreme[, factesp2])), digits=2)
##     TauxRenseignement <- paste("Taux de renseignement \ndu champs ", factesp, ":\n", TauxObsCritereSansNA1, "% des observations \n", TauxEspCritereSansNA1, "% des espèces\n", "du champs ", factesp2, ":\n", TauxObsCritereSansNA2, "% des observations\n", TauxEspCritereSansNA2, "% des espèces\n")
##     print(TauxRenseignement)
##     if (length(unique(obsSansExtreme[, champtrouve]))>0)     #teste si il y a au moins un enregistrement
##     {
##         if (round(sum(table(list(obsSansExtreme[, factesp], obsSansExtreme[, factesp2])))/length(list(obsSansExtreme[, factesp], obsSansExtreme[, factesp2])), digits=2)>=0.5)
##         {
##             if (length(unique(list(obsSansExtreme[, factesp], obsSansExtreme[, factesp2])))>1)  #teste si il y a plus d'une valeur pour le regroupement dans le champs
##             {
##                 ## distribution des enregistrements groupés selon factesp , factesp2

##                 x11(width=80, height=50, pointsize=10)
##                 par(mar=c(9, 4, 5, 1), mgp=c(3, 1, 9))
##                 boxplot(obsSansExtreme[, champtrouve] ~obsSansExtreme[, factesp]+obsSansExtreme[, factesp2], data=obsSansExtreme, varwidth = TRUE, las=3, main=paste("Regroupement des ", champtrouve, " selon \"", factesp, "\"et \"", factesp2, "\""))
##                 nbObs <- tapply(obsSansExtreme[, champtrouve], list(obsSansExtreme[, factesp], obsSansExtreme[, factesp2]), length)
##                 axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)), col.ticks="orange", col.axis = "orange", lty = 2, lwd = 0.5)
##                 legend("topleft", "Nombre d'enregistrements par Boxplot", cex =0.7, col="orange", text.col="orange", merge=FALSE)
##                 Moyenne <- as.vector(tapply(obsSansExtreme[, champtrouve], list(obsSansExtreme[, factesp], obsSansExtreme[, factesp2]), na.rm = T, mean))
##                 points(Moyenne, pch=19, col="blue")
##                 text(Moyenne+(Moyenne/10), col = "blue", cex = 0.8, labels=as.character(round(Moyenne, digits=1)))
##                 abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")
##                 ## boxplot(spunit[, champtrouve] ~spunit$an+spunit$code_espece, las=2, cex.names=0.6, main=paste("Nombre d'individus par espèce et par an pour le site ", si, "\n"))
##                 ## nbObs <- tapply(spunit[, champtrouve], list(spunit$an, spunit$code_espece), length)
##                 if (choix=="1")
##                 {
##                     legend("top", "Enregistrements > 95% du maximum retirés", cex =0.7, col="red", text.col="red", merge=FALSE)
##                 }
##                 legend("topleft", TauxRenseignement, cex =0.8, col="red", bty="n", text.col="red", merge=FALSE)
##                 print(paste("graphique de distribution des enregistrements groupés selon", factesp , factesp2, " réalisé"))

##                 ## distribution des enregistrements groupés selon le statut du site et factesp , factesp2

##                 textstatut <- c("HR : Hors Réserve", "PP : Protection Partielle", "RE : En réserve")
##                 x11(width=120, height=50, pointsize=10)
##                 par(mar=c(8, 4, 5, 1), mgp=c(3, 1, 9))
##                 boxplot(obsSansExtreme[, champtrouve][obsSansExtreme[, champtrouve]!= 0] ~obsSansExtreme$statut[obsSansExtreme[, champtrouve]!= 0]+obsSansExtreme[, factesp][obsSansExtreme[, champtrouve]!= 0]+obsSansExtreme[, factesp2][obsSansExtreme[, champtrouve]!= 0], col=heat.colors(length(unique(obsSansExtreme$statut))), las=2, main=paste(champtrouve, " par enregistrement\n selon le statut et Regroupement selon \"", factesp, "\"et \"", factesp2, "\"\n"))
##                 nbObs <- tapply(obsSansExtreme[, champtrouve][obsSansExtreme[, champtrouve]!= 0], list(obsSansExtreme$statut[obsSansExtreme[, champtrouve]!= 0], obsSansExtreme[, factesp][obsSansExtreme[, champtrouve]!= 0], obsSansExtreme[, factesp2][obsSansExtreme[, champtrouve]!= 0]), length)
##                 legend("topleft", "Nombre d'enregistrement par Boxplot", cex =0.7, col="orange", text.col="orange", merge=FALSE)
##                 legend("topright", textstatut, col=heat.colors(length(unique(obsSansExtreme$statut))), pch = 15, cex =0.9, title="Statuts")
##                 axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)), col.ticks="orange", col.axis = "orange", lty = 2, lwd = 0.5)
##                 Moyenne <- as.vector(tapply(obsSansExtreme[, champtrouve][obsSansExtreme[, champtrouve]!= 0], list(obsSansExtreme$statut[obsSansExtreme[, champtrouve]!= 0], obsSansExtreme[, factesp][obsSansExtreme[, champtrouve]!= 0], obsSansExtreme[, factesp2][obsSansExtreme[, champtrouve]!= 0]), na.rm = T, mean))
##                 points(Moyenne, pch=19, col="blue")
##                 if (length(Moyenne)>150)
##                 {
##                     legend("center", "plus de 150 possibilités de boxplot\nFaites une sélection préalable sur votre jeux de données", cex =1, col="blue", text.col="blue", merge=FALSE)
##                     print("plus de 150 possibilités de boxplot\nFaites une sélection préalable dans votre jeu de données\n...")
##                 }else{
##                     abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")
##                     abline(v = 0.5+(1:length(as.vector(unique(list(obsSansExtreme[, factesp], obsSansExtreme[, factesp2])))))*length(as.vector(unique(obsSansExtreme$statut))), col = "red")
##                 }
##                 if (choix=="1")
##                 {
##                     legend("top", "Enregistrements > 95% du maximum retirés", cex =0.7, col="red", text.col="red", merge=FALSE)
##                 }
##                 legend("topleft", TauxRenseignement, cex =0.8, col="red", bty="n", text.col="red", merge=FALSE)
##                 print(paste("graphique de distribution des enregistrements groupés selon le statut du site et", factesp , factesp2, " réalisé"))

##                 ## distribution des enregistrements groupés selon l'année de l'observation  et factesp , factesp2

##                 x11(width=120, height=50, pointsize=10)
##                 par(mar=c(8, 4, 5, 1), mgp=c(3, 1, 9))
##                 boxplot(obsSansExtreme[, champtrouve][obsSansExtreme[, champtrouve]!= 0] ~obsSansExtreme$statut[obsSansExtreme[, champtrouve]!= 0]+obsSansExtreme$an[obsSansExtreme[, champtrouve]!= 0]+obsSansExtreme[, factesp][obsSansExtreme[, champtrouve]!= 0]+obsSansExtreme[, factesp2][obsSansExtreme[, champtrouve]!= 0], col=heat.colors(length(unique(obsSansExtreme$statut))), las=2, main=paste(champtrouve, " par enregistrement\n selon le statut puis l'année et Regroupement selon \"", factesp, "\"et \"", factesp2, "\"\n"))
##                 nbObs <- tapply(obsSansExtreme[, champtrouve][obsSansExtreme[, champtrouve]!= 0], list(obsSansExtreme$statut[obsSansExtreme[, champtrouve]!= 0], obsSansExtreme$an[obsSansExtreme[, champtrouve]!= 0], obsSansExtreme[, factesp][obsSansExtreme[, champtrouve]!= 0], obsSansExtreme[, factesp2][obsSansExtreme[, champtrouve]!= 0]), length)
##                 legend("topleft", "Nombre d'enregistrement par Boxplot", cex =0.7, col="orange", text.col="orange", merge=FALSE)
##                 legend("topright", textstatut, col=heat.colors(length(unique(obsSansExtreme$statut))), pch = 15, cex =0.9, title="Statuts")
##                 axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)), col.ticks="orange", col.axis = "orange", lty = 2, lwd = 0.5, cex =0.7)
##                 Moyenne <- as.vector(tapply(obsSansExtreme[, champtrouve][obsSansExtreme[, champtrouve]!= 0], list(obsSansExtreme$statut[obsSansExtreme[, champtrouve]!= 0], obsSansExtreme$an[obsSansExtreme[, champtrouve]!= 0], obsSansExtreme[, factesp][obsSansExtreme[, champtrouve]!= 0], obsSansExtreme[, factesp2][obsSansExtreme[, champtrouve]!= 0]), na.rm = T, mean))
##                 points(Moyenne, pch=19, col="blue")
##                 if (length(Moyenne)>150)
##                 {
##                     legend("center", "plus de 150 possibilités de boxplot\nFaites une sélection préalable sur votre jeux de données", cex =1, col="blue", text.col="blue", merge=FALSE)
##                     print("plus de 150 possibilités de boxplot\nFaites une sélection préalable dans votre jeu de données\n...")
##                 }else{
##                     abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")
##                     abline(v = 0.5+(1:(length(as.vector(unique(obsSansExtreme$an)))*length(as.vector(unique(list(obsSansExtreme[, factesp][obsSansExtreme[, champtrouve]!= 0], obsSansExtreme[, factesp2][obsSansExtreme[, champtrouve]!= 0]))))))*length(as.vector(unique(obsSansExtreme$statut))), col = "red")
##                 }
##                 if (choix=="1")
##                 {
##                     legend("top", "Enregistrements > 95% du maximum retirés", cex =0.7, col="red", text.col="red", merge=FALSE)
##                 }
##                 legend("topleft", TauxRenseignement, cex =0.8, col="red", bty="n", text.col="red", merge=FALSE)
##                 print(paste("graphique de distribution des enregistrements groupés selon l'année de l'observation  et", factesp , factesp2, " réalisé"))

##                 ## distribution des enregistrements groupés selon le statut, l'année de l'observation  et factesp , factesp2

##                 x11(width=120, height=50, pointsize=10)
##                 par(mar=c(8, 4, 5, 1), mgp=c(3, 1, 9))
##                 boxplot(obsSansExtreme[, champtrouve][obsSansExtreme[, champtrouve]!= 0] ~obsSansExtreme$an[obsSansExtreme[, champtrouve]!= 0]+obsSansExtreme$statut[obsSansExtreme[, champtrouve]!= 0]+obsSansExtreme[, factesp][obsSansExtreme[, champtrouve]!= 0]+obsSansExtreme[, factesp2][obsSansExtreme[, champtrouve]!= 0], col=heat.colors(length(unique(obsSansExtreme$statut))*length(unique(obsSansExtreme$an))), las=2, main=paste(champtrouve, " par enregistrement\n selon l'année puis le statut et Regroupement selon \"", factesp, "\"et \"", factesp2, "\"\n"))
##                 nbObs <- tapply(obsSansExtreme[, champtrouve][obsSansExtreme[, champtrouve]!= 0], list(obsSansExtreme$an[obsSansExtreme[, champtrouve]!= 0], obsSansExtreme$statut[obsSansExtreme[, champtrouve]!= 0], obsSansExtreme[, factesp][obsSansExtreme[, champtrouve]!= 0], obsSansExtreme[, factesp2][obsSansExtreme[, champtrouve]!= 0]), length)
##                 legend("topleft", "Nombre d'enregistrement par Boxplot", cex =0.7, col="orange", text.col="orange", merge=FALSE)
##                 legend("topright", textstatut, col=heat.colors(length(unique(obsSansExtreme$statut))), pch = 15, cex =0.9, title="Statuts")
##                 axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)), col.ticks="orange", col.axis = "orange", lty = 2, lwd = 0.5, cex =0.7)
##                 Moyenne <- as.vector(tapply(obsSansExtreme[, champtrouve][obsSansExtreme[, champtrouve]!= 0], list(obsSansExtreme$an[obsSansExtreme[, champtrouve]!= 0], obsSansExtreme$statut[obsSansExtreme[, champtrouve]!= 0], obsSansExtreme[, factesp][obsSansExtreme[, champtrouve]!= 0], obsSansExtreme[, factesp2][obsSansExtreme[, champtrouve]!= 0]), na.rm = T, mean))
##                 points(Moyenne, pch=19, col="blue")
##                 if (length(Moyenne)>150)
##                 {
##                     legend("center", "plus de 150 possibilités de boxplot\nFaites une sélection préalable sur votre jeux de données", cex =1, col="blue", text.col="blue", merge=FALSE)
##                     print("plus de 150 possibilités de boxplot\nFaites une sélection préalable dans votre jeu de données\n...")
##                 }else{
##                     abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")
##                     abline(v = 0.5+(1:(length(as.vector(unique(obsSansExtreme$statut)))*length(as.vector(unique(list(obsSansExtreme[, factesp][obsSansExtreme[, champtrouve]!= 0], obsSansExtreme[, factesp2][obsSansExtreme[, champtrouve]!= 0]))))))*length(as.vector(unique(obsSansExtreme$an))), col = "red")
##                 }
##                 if (choix=="1")
##                 {
##                     legend("top", "Enregistrements > 95% du maximum retirés", cex =0.7, col="red", text.col="red", merge=FALSE)
##                 }
##                 legend("topleft", TauxRenseignement, cex =0.8, col="red", bty="n", text.col="red", merge=FALSE)
##                 print(paste("graphique de distribution des enregistrements le statut, l'année de l'observation  et", factesp , ", ", factesp2, " réalisé"))

##                 interaction.plot(obsSansExtreme$an, obsSansExtreme[, factesp], obsSansExtreme[, champtrouve], lwd=2, col=cl[seq(550, (550+(4*(length(split(obsSansExtreme, obsSansExtreme$an))-1))), by=4)], type="b",
##                                  fun=mean, trace.label = factesp, xlab="Annee", ylab=champtrouve, main=paste("Evolution des valeurs de ", champtrouve, " par ", factesp))

##                 interaction.plot(obsSansExtreme$an, obsSansExtreme[, factesp2], obsSansExtreme[, champtrouve], lwd=2, col=cl[seq(550, (550+(4*(length(split(obsSansExtreme, obsSansExtreme$an))-1))), by=4)], type="b",
##                                  fun=mean, trace.label = factesp2, xlab="Annee", ylab=champtrouve, main=paste("Evolution des valeurs de ", champtrouve, " par ", factesp2))

##             }else{             #si pas plus d'une valeur
##                 tkmessageBox(message=paste("Les champs", factesp, "et", factesp2, "contiennent toujours la même valeur :", unique(obsSansExtreme[, factesp][, factesp2]), "\n regroupement inefficace, sélectionnez un autre facteur"), icon="info")
##                 gestionMSGerreur.f("UneSeuleValeurRegroupement")
##             }
##         }else{          #si moins de 50% du critère renseigné
##             tkmessageBox(message=paste("Graphique sans intérêt - plus de la moitié \n des enregistrements des critères de regroupement ", factesp, "et", factesp2, "ne sont pas renseignés (NA)"))
##             gestionMSGerreur.f("CritereMalRenseigne50")
##         }
##     }else{
##         tkmessageBox(message="Graphique impossible - pas d'enregistrements dans votre sélection")
##         gestionMSGerreur.f("ZeroEnregistrement")
##     }
##     choix <- "0"
## }

################################################################################
## Nom    : GraphGroup2factEspUnitobs.f()
## Objet  : graphiques regroupant les observations sur une caractéristique du référentiel espèce
## Input  : table "listespunit" et un code champ
## Output : boxplot
################################################################################

## [sup] [yr:12/01/2011]:

## GraphGroup2factEspUnitobs.f <- function ()
## {

##     print("fonction GraphGroup2factEspUnitobs.f activée")

##     ## ###############################
##     ## pour la table listespunit    #
##     ## ###############################
##     matable <- "listespunit"

##     ## Choix du champ à représenter champobs
##     choixchamptable.f(matable)
##     print(champtrouve)
##     ## Choix du facteur de regroupement
##     critereespref.f()
##     print(factesp)
##     choixunfacteurUnitobs.f()
##     print(fact)

##     objtable <- eval(parse(text=matable))
##     objtable[, fact] <- unitobs[, fact][match(objtable$unite_observation, unitobs$unite_observation)]
##     objtable$an <- unitobs$an[match(objtable$unite_observation, unitobs$unite_observation)]
##     objtable$statut <- unitobs$statut[match(objtable$unite_observation, unitobs$unite_observation)]
##     objtable[, factesp] <- especes[, factesp][match(objtable$code_espece, especes$code_espece)]
##     obsSansExtreme <- objtable
##     extremes.f()
##     ## Graphiques sur un champ de obs
##     obsSansExtreme[, champtrouve] <- EnleverMaxExtremes.f(obsSansExtreme[, champtrouve])    #  selon choix retourné par extremes.f()
##     print(head(obsSansExtreme))

##     TauxUnitobsCritereSansNA <- round(100*(length(unique(obsSansExtreme$unite_observation[is.na(obsSansExtreme[, fact])==FALSE]))/length(unique(obsSansExtreme$unite_observation))), digits=2)
##     TauxObsCritereSansNA <- round(100*(sum(table(obsSansExtreme[, fact]))/length(obsSansExtreme[, fact])), digits=2)
##     TauxEspCritereSansNA1 <- round(100*(length(unique(obsSansExtreme$code_espece[is.na(obsSansExtreme[, factesp])==FALSE]))/length(unique(obsSansExtreme$code_espece))), digits=2)
##     TauxObsCritereSansNA1 <- round(100*(sum(table(obsSansExtreme[, factesp]))/length(obsSansExtreme[, factesp])), digits=2)

##     TauxRenseignement <- paste("Taux de renseignement \ndu champs ", factesp, ":\n", TauxObsCritereSansNA1, "% des observations \n", TauxEspCritereSansNA1, "% des espèces\n", "du champs ", fact, ":\n", TauxObsCritereSansNA, "% des observations\n", TauxUnitobsCritereSansNA, "% des unités d'observations\n")
##     print(TauxRenseignement)
##     if (length(unique(obsSansExtreme[, champtrouve]))>0)     #teste si il y a au moins un enregistrement
##     {
##         if (round(sum(table(list(obsSansExtreme[, factesp], obsSansExtreme[, fact])))/length(list(obsSansExtreme[, factesp], obsSansExtreme[, fact])), digits=2)>=0.5)
##         {
##             if (length(unique(list(obsSansExtreme[, factesp], obsSansExtreme[, fact])))>1)  #teste si il y a plus d'une valeur pour le regroupement dans le champs
##             {
##                 ## distribution des enregistrements groupés selon factesp , fact

##                 x11(width=50, height=50, pointsize=10)
##                 par(mar=c(9, 4, 5, 1), mgp=c(3, 1, 9))
##                 boxplot(obsSansExtreme[, champtrouve] ~obsSansExtreme[, factesp]+obsSansExtreme[, fact], data=obsSansExtreme, varwidth = TRUE, las=3, main=paste("Regroupement des ", champtrouve, " selon \"", factesp, "\"et \"", fact, "\""))
##                 nbObs <- tapply(obsSansExtreme[, champtrouve], list(obsSansExtreme[, factesp], obsSansExtreme[, fact]), length)
##                 axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)), col.ticks="orange", col.axis = "orange", lty = 2, lwd = 0.5)
##                 legend("topleft", "Nombre d'enregistrements par Boxplot", cex =0.7, col="orange", text.col="orange", merge=FALSE)
##                 Moyenne <- as.vector(tapply(obsSansExtreme[, champtrouve], list(obsSansExtreme[, factesp], obsSansExtreme[, fact]), na.rm = T, mean))
##                 points(Moyenne, pch=19, col="blue")
##                 text(Moyenne+(Moyenne/10), col = "blue", cex = 0.8, labels=as.character(round(Moyenne, digits=1)))
##                 abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")
##                 ## boxplot(spunit[, champtrouve] ~spunit$an+spunit$code_espece, las=2, cex.names=0.6, main=paste("Nombre d'individus par espèce et par an pour le site ", si, "\n"))
##                 ## nbObs <- tapply(spunit[, champtrouve], list(spunit$an, spunit$code_espece), length)
##                 if (choix=="1")
##                 {
##                     legend("top", "Enregistrements > 95% du maximum retirés", cex =0.7, col="red", text.col="red", merge=FALSE)
##                 }
##                 legend("topleft", TauxRenseignement, cex =0.8, col="red", bty="n", text.col="red", merge=FALSE)
##                 print(paste("graphique de distribution des enregistrements groupés selon", factesp , fact, " réalisé"))

##                 ## distribution des enregistrements groupés selon le statut du site et factesp , fact

##                 textstatut <- c("HR : Hors Réserve", "PP : Protection Partielle", "RE : En réserve")
##                 x11(width=120, height=50, pointsize=10)
##                 par(mar=c(8, 4, 5, 1), mgp=c(3, 1, 9))
##                 boxplot(obsSansExtreme[, champtrouve][obsSansExtreme[, champtrouve]!= 0] ~obsSansExtreme$statut[obsSansExtreme[, champtrouve]!= 0]+obsSansExtreme[, factesp][obsSansExtreme[, champtrouve]!= 0]+obsSansExtreme[, fact][obsSansExtreme[, champtrouve]!= 0], col=heat.colors(length(unique(obsSansExtreme$statut))), las=2, main=paste(champtrouve, " par enregistrement\n selon le statut et Regroupement selon \"", factesp, "\"et \"", fact, "\"\n"))
##                 nbObs <- tapply(obsSansExtreme[, champtrouve][obsSansExtreme[, champtrouve]!= 0], list(obsSansExtreme$statut[obsSansExtreme[, champtrouve]!= 0], obsSansExtreme[, factesp][obsSansExtreme[, champtrouve]!= 0], obsSansExtreme[, fact][obsSansExtreme[, champtrouve]!= 0]), length)
##                 legend("topleft", "Nombre d'enregistrement par Boxplot", cex =0.7, col="orange", text.col="orange", merge=FALSE)
##                 legend("topright", textstatut, col=heat.colors(length(unique(obsSansExtreme$statut))), pch = 15, cex =0.9, title="Statuts")
##                 axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)), col.ticks="orange", col.axis = "orange", lty = 2, lwd = 0.5)
##                 Moyenne <- as.vector(tapply(obsSansExtreme[, champtrouve][obsSansExtreme[, champtrouve]!= 0], list(obsSansExtreme$statut[obsSansExtreme[, champtrouve]!= 0], obsSansExtreme[, factesp][obsSansExtreme[, champtrouve]!= 0], obsSansExtreme[, fact][obsSansExtreme[, champtrouve]!= 0]), na.rm = T, mean))
##                 points(Moyenne, pch=19, col="blue")
##                 if (length(Moyenne)>150)
##                 {
##                     legend("center", "plus de 150 possibilités de boxplot\nFaites une sélection préalable sur votre jeux de données", cex =1, col="blue", text.col="blue", merge=FALSE)
##                     print("plus de 150 possibilités de boxplot\nFaites une sélection préalable dans votre jeu de données\n...")
##                 }else{
##                     abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")
##                     abline(v = 0.5+(1:length(as.vector(unique(list(obsSansExtreme[, factesp], obsSansExtreme[, fact])))))*length(as.vector(unique(obsSansExtreme$statut))), col = "red")
##                 }
##                 if (choix=="1")
##                 {
##                     legend("top", "Enregistrements > 95% du maximum retirés", cex =0.7, col="red", text.col="red", merge=FALSE)
##                 }
##                 legend("topleft", TauxRenseignement, cex =0.8, col="red", bty="n", text.col="red", merge=FALSE)
##                 print(paste("graphique de distribution des enregistrements groupés selon le statut du site et", factesp , fact, " réalisé"))

##                 ## distribution des enregistrements groupés selon l'année de l'observation  et factesp , fact

##                 x11(width=120, height=50, pointsize=10)
##                 par(mar=c(8, 4, 5, 1), mgp=c(3, 1, 9))
##                 boxplot(obsSansExtreme[, champtrouve][obsSansExtreme[, champtrouve]!= 0] ~obsSansExtreme$statut[obsSansExtreme[, champtrouve]!= 0]+obsSansExtreme$an[obsSansExtreme[, champtrouve]!= 0]+obsSansExtreme[, factesp][obsSansExtreme[, champtrouve]!= 0]+obsSansExtreme[, fact][obsSansExtreme[, champtrouve]!= 0], col=heat.colors(length(unique(obsSansExtreme$statut))), las=2, main=paste(champtrouve, " par enregistrement\n selon le statut puis l'année et Regroupement selon \"", factesp, "\"et \"", fact, "\"\n"))
##                 nbObs <- tapply(obsSansExtreme[, champtrouve][obsSansExtreme[, champtrouve]!= 0], list(obsSansExtreme$statut[obsSansExtreme[, champtrouve]!= 0], obsSansExtreme$an[obsSansExtreme[, champtrouve]!= 0], obsSansExtreme[, factesp][obsSansExtreme[, champtrouve]!= 0], obsSansExtreme[, fact][obsSansExtreme[, champtrouve]!= 0]), length)
##                 legend("topleft", "Nombre d'enregistrement par Boxplot", cex =0.7, col="orange", text.col="orange", merge=FALSE)
##                 legend("topright", textstatut, col=heat.colors(length(unique(obsSansExtreme$statut))), pch = 15, cex =0.9, title="Statuts")
##                 axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)), col.ticks="orange", col.axis = "orange", lty = 2, lwd = 0.5, cex =0.7)
##                 Moyenne <- as.vector(tapply(obsSansExtreme[, champtrouve][obsSansExtreme[, champtrouve]!= 0], list(obsSansExtreme$statut[obsSansExtreme[, champtrouve]!= 0], obsSansExtreme$an[obsSansExtreme[, champtrouve]!= 0], obsSansExtreme[, factesp][obsSansExtreme[, champtrouve]!= 0], obsSansExtreme[, fact][obsSansExtreme[, champtrouve]!= 0]), na.rm = T, mean))
##                 points(Moyenne, pch=19, col="blue")
##                 if (length(Moyenne)>150)
##                 {
##                     legend("center", "plus de 150 possibilités de boxplot\nFaites une sélection préalable sur votre jeux de données", cex =1, col="blue", text.col="blue", merge=FALSE)
##                     print("plus de 150 possibilités de boxplot\nFaites une sélection préalable dans votre jeu de données\n...")
##                 }else{
##                     abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")
##                     abline(v = 0.5+(1:(length(as.vector(unique(obsSansExtreme$an)))*length(as.vector(unique(list(obsSansExtreme[, factesp][obsSansExtreme[, champtrouve]!= 0], obsSansExtreme[, fact][obsSansExtreme[, champtrouve]!= 0]))))))*length(as.vector(unique(obsSansExtreme$statut))), col = "red")
##                 }
##                 if (choix=="1")
##                 {
##                     legend("top", "Enregistrements > 95% du maximum retirés", cex =0.7, col="red", text.col="red", merge=FALSE)
##                 }
##                 legend("topleft", TauxRenseignement, cex =0.8, col="red", bty="n", text.col="red", merge=FALSE)
##                 print(paste("graphique de distribution des enregistrements groupés selon l'année de l'observation  et", factesp , fact, " réalisé"))

##                 ## distribution des enregistrements groupés selon le statut, l'année de l'observation  et factesp , fact

##                 x11(width=120, height=50, pointsize=10)
##                 par(mar=c(8, 4, 5, 1), mgp=c(3, 1, 9))
##                 boxplot(obsSansExtreme[, champtrouve][obsSansExtreme[, champtrouve]!= 0] ~obsSansExtreme$an[obsSansExtreme[, champtrouve]!= 0]+obsSansExtreme$statut[obsSansExtreme[, champtrouve]!= 0]+obsSansExtreme[, factesp][obsSansExtreme[, champtrouve]!= 0]+obsSansExtreme[, fact][obsSansExtreme[, champtrouve]!= 0], col=heat.colors(length(unique(obsSansExtreme$statut))*length(unique(obsSansExtreme$an))), las=2, main=paste(champtrouve, " par enregistrement\n selon l'année puis le statut et Regroupement selon \"", factesp, "\"et \"", fact, "\"\n"))
##                 nbObs <- tapply(obsSansExtreme[, champtrouve][obsSansExtreme[, champtrouve]!= 0], list(obsSansExtreme$an[obsSansExtreme[, champtrouve]!= 0], obsSansExtreme$statut[obsSansExtreme[, champtrouve]!= 0], obsSansExtreme[, factesp][obsSansExtreme[, champtrouve]!= 0], obsSansExtreme[, fact][obsSansExtreme[, champtrouve]!= 0]), length)
##                 legend("topleft", "Nombre d'enregistrement par Boxplot", cex =0.7, col="orange", text.col="orange", merge=FALSE)
##                 legend("topright", textstatut, col=heat.colors(length(unique(obsSansExtreme$statut))), pch = 15, cex =0.9, title="Statuts")
##                 axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)), col.ticks="orange", col.axis = "orange", lty = 2, lwd = 0.5, cex =0.7)
##                 Moyenne <- as.vector(tapply(obsSansExtreme[, champtrouve][obsSansExtreme[, champtrouve]!= 0], list(obsSansExtreme$an[obsSansExtreme[, champtrouve]!= 0], obsSansExtreme$statut[obsSansExtreme[, champtrouve]!= 0], obsSansExtreme[, factesp][obsSansExtreme[, champtrouve]!= 0], obsSansExtreme[, fact][obsSansExtreme[, champtrouve]!= 0]), na.rm = T, mean))
##                 points(Moyenne, pch=19, col="blue")
##                 if (length(Moyenne)>150)
##                 {
##                     legend("center", "plus de 150 possibilités de boxplot\nFaites une sélection préalable sur votre jeux de données", cex =1, col="blue", text.col="blue", merge=FALSE)
##                     print("plus de 150 possibilités de boxplot\nFaites une sélection préalable dans votre jeu de données\n...")
##                 }else{
##                     abline(v = 0.5+(1:length(Moyenne)) , col = "lightgray", lty = "dotted")
##                     abline(v = 0.5+(1:(length(as.vector(unique(obsSansExtreme$statut)))*length(as.vector(unique(list(obsSansExtreme[, factesp][obsSansExtreme[, champtrouve]!= 0], obsSansExtreme[, fact][obsSansExtreme[, champtrouve]!= 0]))))))*length(as.vector(unique(obsSansExtreme$an))), col = "red")
##                 }
##                 if (choix=="1")
##                 {
##                     legend("top", "Enregistrements > 95% du maximum retirés", cex =0.7, col="red", text.col="red", merge=FALSE)
##                 }
##                 legend("topleft", TauxRenseignement, cex =0.8, col="red", bty="n", text.col="red", merge=FALSE)
##                 print(paste("graphique de distribution des enregistrements le statut, l'année de l'observation  et", factesp, ", ", fact, " réalisé"))

##                 interaction.plot(obsSansExtreme[, fact], obsSansExtreme[, factesp], obsSansExtreme[, champtrouve], lwd=2, col=cl[seq(550, (550+(4*(length(split(obsSansExtreme, obsSansExtreme[, fact]))-1))), by=4)], type="b",
##                                  fun=mean, trace.label = fact, xlab=factesp, ylab=champtrouve, main=paste("Interaction des valeurs de ", champtrouve, " par", fact))

##             }else{             #si pas plus d'une valeur
##                 tkmessageBox(message=paste("Les champs", factesp, "et", fact, "contiennent toujours la même valeur :", unique(obsSansExtreme[, factesp][, fact]), "\n regroupement inefficace, sélectionnez un autre facteur"), icon="info")
##                 gestionMSGerreur.f("UneSeuleValeurRegroupement")
##             }
##         }else{          #si moins de 50% du critère renseigné
##             tkmessageBox(message=paste("Graphique sans intérêt - plus de la moitié \n des enregistrements des critères de regroupement ", factesp, "et", fact, "ne sont pas renseignés (NA)"))
##             gestionMSGerreur.f("CritereMalRenseigne50")
##         }
##     }else{
##         tkmessageBox(message="Graphique impossible - pas d'enregistrements dans votre sélection")
##         gestionMSGerreur.f("ZeroEnregistrement")
##     }
##     choix <- "0"
## }




################################################################################
## Nom    : graphNbreParEsp.f()
## Objet  : graphiques par unité d'observation sur une espèce sélectionnée
## Input  : table "listespunit" et un code espèce sp
## Output : boxplot densité et biomasse
################################################################################
## !!!!!!!!!!!!!!!!!!!    spunit$densite <- EnleverMaxExtremes.f(spunit$densite) a décliner sur deux facteurs

## [sup] [yr:12/01/2011]:

## graphNbreParEsp.f <- function ()
## {
##     print("fonction graphNbreParEsp activée")
##     ChoixUneEspece.f()
##     ## on restreint la table "listespunit" à l'espèce sélectionnée
##     extremes.f()
##     spunit <- subset(listespunit, listespunit$code_espece==sp)
##     X11()
##     boxplot(spunit$densite ~ spunit$code_espece, data=spunit, varwidth = TRUE, ylab=expression("Densite "(individus/m^2)), main=paste("Densite de ", sp, sep=""), las=3)
##     ## la biomasse n'est pas calculée sur tous les jeux de données
##     if (length(unique(listespunit$biomasse))>1)
##     {
##         ## on vérifie que la biomasse a été calculée pour l'espèce sélectionnée
##         if (length(unique(spunit$biomasse))>1)
##         {
##             X11()
##             boxplot(spunit$biomasse ~ spunit$code_espece, col=heat.colors(length(unique(spunit$code_espece))), data=spunit, varwidth = TRUE, ylab=expression("Biomasse "(g/m^2)), main=paste("Biomasse de ", sp, sep=""), las=3)
##         }else{
##             tkmessageBox(message="Calcul de biomasse impossible - Coefficients a et b manquants dans le referentiel especes")
##         }
##     }
##     choix <- "0"
## }

## [sup] [yr:12/01/2011]:

## GraphRecouvrementPourUneEspece.f <- function ()
## {
##     print("fonction GraphRecouvrementPourUneEspece activée")
##     ## il faudrait mettre des couleurs #FONCTION OK mais peu générique
##     ChoixUneEspece.f()
##     extremes.f()
##     print(paste("espece considérée :", sp))
##     ## on restreint la table "listespunit" à l'espèce sélectionnée
##     spunit <- subset(listespunit, listespunit$code_espece==sp)
##     spunit$recouvrement <- EnleverMaxExtremes.f(spunit$recouvrement)
##     if (length(unique(spunit$recouvrement))>0)
##     {
##         boxplot(spunit$recouvrement, main=paste("Recouvrement\n pour l'espèce", sp, "\npour toutes les unités d'obs la concernant"))
##         nbObs <- tapply(spunit$recouvrement, length)
##         axis(3, as.vector(nbObs), at=1:length(as.vector(nbObs)))
##     }else{
##         tkmessageBox(message="Graphique impossible - pas d'enregistrements dans votre sélection")
##         gestionMSGerreur.f("ZeroEnregistrement")
##     }
##     choix <- "0"
## }




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

## [sup] [yr:12/01/2011]:

## GraphiqueRecouvrement.f <- function (matricerecouvrement, typegraph)
## {
##     matricerecouvrement <- CalculRecouvrement.f()
##     extremes.f()

##     typegraph <- "tout"
##     if (typegraph=="tout")
##     {
##         x11(width=50, height=30, pointsize=10)
##         boxplot(as.data.frame(matricerecouvrement), col=row(matricerecouvrement), las=2, cex.names=0.8, main = "Recouvrement (en %) par espèce")
##         par(las=1)# all axis labels horizontal
##         for (i in 1:length(unique(unitobs$statut_protection)))
##         {
##             unitstatut <- unitobs$unite_observation[which(unitobs$statut_protection==unique(unitobs$statut_protection)[i])]
##             unitselect <- matricerecouvrement[match(unitstatut, rownames(matricerecouvrement)), ]
##             x11(width=50, height=30, pointsize=10)
##             boxplot(as.data.frame(matricerecouvrement), col=row(matricerecouvrement), main = paste("Recouvrement (en %) par espèce pour le statut", unique(unitobs$statut_protection)[i]), horizontal = TRUE)
##             legend("right", "histogramme de part de recouvrement toutes espèces confondues")
##             x11(width=50, height=30, pointsize=10)
##             boxplot(matricerecouvrement, col=row(matricerecouvrement), cex.lab=1.2, las=2, xlab="Espèces présentes", ylab="part de recouvrement", main=paste("Recouvrement (en %) par espèce pour les observations du statut", unique(unitobs$statut_protection)[i]))
##         }
##     }
##     typegraph <- "paran"
##     if (typegraph=="paran")
##     {
##         par(las=1)# all axis labels horizontal
##         for (i in 1:length(unique(unitobs$an)))
##         {
##             unitan <- unitobs$unite_observation[which(unitobs$an==unique(unitobs$an)[i])]
##             unitselectan <- matricerecouvrement[match(unitan, rownames(matricerecouvrement)), ]
##             x11(width=50, height=30, pointsize=10)
##             boxplot(as.data.frame(unitselectan), col=row(unitselectan), main = paste("Recouvrement (en %) par espèce pour l'année ", unique(unitobs$an)[i]), horizontal = TRUE)
##             col2 <- rgb(1, 0, 1, 0.5)
##             legend("right", "histogramme de part de recouvrement toutes espèces confondues")
##             x11(width=50, height=30, pointsize=10)
##             boxplot(unitselectan, col=row(unitselectan), cex.lab=1.2, las=2, xlab="Espèces présentes", ylab="part de recouvrement", main=paste("Recouvrement (en %) par espèce pour les observations de l'année", unique(unitobs$an)[i]))
##         }
##     }

##     typegraph <- "parfamille"
##     if (typegraph=="parfamille")
##     {
##         ChoixUneFamille.f()
##         ListeEspFamilleSelectionnee <- especes$code_espece[which(especes$Famille==fa)]
##         ## on met dans une liste les espèces de la famille sélectionnée
##         if (sum(match(ListeEspFamilleSelectionnee, colnames(matricerecouvrement)), na.rm=T)!=0)
##         {
##             unitselect <- matricerecouvrement[match(ListeEspFamilleSelectionnee, colnames(matricerecouvrement)), ]
##             x11(width=50, height=30, pointsize=10)
##             par(las=1)# all axis labels horizontal
##             unitselect2 <- unitselect[, which(apply(unitselect, 2, sum, na.rm=T)!=0)]
##             boxplot(as.data.frame(unitselect2), col=row(unitselect2), main = paste("Recouvrement (en %) par espèce pour la famille", fa), horizontal = TRUE)
##             boxplot(as.data.frame(unitselect2), col=row(unitselect2), main = paste("Recouvrement (en %) par espèce pour la famille", fa), horizontal = TRUE)
##             legend("right", paste("Part de recouvrement pour les espèces de la famille", fa))
##             x11(width=50, height=30, pointsize=10)
##             boxplot(unitselect[, which( apply(unitselect, 2, sum, na.rm=T)!=0)], col=row(unitselect[, which( apply(unitselect, 2, sum, na.rm=T)!=0)]), cex.lab=1.2, las=2, xlab="Espèces présentes", ylab="Part de recouvrement", main=paste("Recouvrement (en %) par espèce pour les observations de la famille", fa))
##         }
##     }
##     choix <- "0"
## }

PartRecouvrementTot.f <- function (matricerecouvrement, typegraph)
{
    print("fonction PartRecouvrementTot activée")
    extremes.f()

    col2 <- rgb(0, 1, 1, 0.5)
    hist(as.matrix(matricerecouvrement), col = col2)
    choix <- "0"
}

PartRecouvrementEsp.f <- function (matricerecouvrement, typegraph) # [inc]
{
    print("fonction PartRecouvrementEsp activée")}




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

## [sup] [yr:12/01/2011]:

## graphCT1.f <- function (fact)
## {

##     print("fonction graphCT1.f activée")
##     AffichageGraphiques.f()

##     if (doneVal==1)
##     {
##         unitesptat[, fact] <- unitobs[, fact][match(unitesptat$unitobs, unitobs$unite_observation)]
##         assign("unitesptat", unitesptat, envir=.GlobalEnv)
##         if (unique(unitobs$type) != "LIT")
##         {
##             ## là on fait des boxplot donc sur l'ensemble des valeurs, on passe donc par la table "unit" et pas "grp"
##             ## affichage des indices representes sous boxplot dans un message
##             tkmessageBox(message=paste("Les indices representes sous boxplot sont : \n- la densite \n- la biomasse \n- la taille moyenne"))

##             ## test existence champs biomasse et densite
##             if (NA %in% unique(unit$biomasse) == FALSE) # [!!!] Argh, c'est quoi ça ??? [yr: 30/07/2010]
##             {
##                 X11()
##                 boxplot(unitesptat$densite ~ unitesptat[, fact], data=unitesptat, varwidth = TRUE, ylab=expression("Densite "(individus/m^2)), main=paste("Densite de", TailleChoisie, "par", fact))
##                 bx1 <- as.vector(tapply(unitesptat$densite, unitesptat[, fact], na.rm = T, mean))
##                 points(bx1, pch=19, col="red")

##                 X11()
##                 boxplot(unitesptat$biomasse ~ unitesptat[, fact], data=unitesptat, varwidth = TRUE, ylab=expression("Biomasse "(g/m^2)), main=paste("Biomasse de", TailleChoisie, "par", fact))
##                 bx1 <- as.vector(tapply(unitesptat$biomasse, unitesptat[, fact], na.rm = T, mean))
##                 points(bx1, pch=19, col="red")
##             }

##             if (ct == 1)
##             {
##                 X11()
##                 boxplot(unitesptat$taille_moyenne ~ unitesptat[, fact], data=unitesptat, varwidth = TRUE, ylab="Taille moyenne", main=paste("Taille moyenne de", TailleChoisie, "par", fact))
##                 bx1 <- as.vector(tapply(unitesptat$taille_moyenne, unitesptat[, fact], na.rm = T, mean))
##                 points(bx1, pch=19, col="red")
##             }
##         }
##         if (unique(unitobs$type) == "LIT")
##         {
##             tkmessageBox(message="Ces metriques ne peuvent pas être calculees pour les suivis LIT")
##         }
##     } # fin DoneVal
## } # fin graphCT1.f

################################################################################
## Nom    : graphCT2.f()
## Objet  : affichage des graphiques par groupe d'unités d'observation
##          et classes de taille
## Input  : tables "unitobs", "unitesptat" + facteurs fact21 et fact22
## Output : graphiques
################################################################################

## [sup] [yr:12/01/2011]:

## graphCT2.f <- function (fact21, fact22)
## {

##     print("fonction graphCT2.f activée")
##     AffichageGraphiques.f()

##     ## si volonte d'afficher les graphs
##     if (doneVal==1)
##     {
##         if (unique(unitobs$type) != "LIT")
##         {
##             cl <- colors()
##             tkmessageBox(message=paste("Les indices representes sous boxplot sont : \n- la densite, \n- la biomasse \n- la taille moyenne"))

##             ## test existence champs biomasse et densite
##             if (NA %in% unique(unit$biomasse) == FALSE)
##             { # [!!!] devrait pouvoir être remplacé par all(!is.na(...)) [yr: 30/07/2010]
##                 x11(width=12, height=8, pointsize=12)
##                 par(mar=c(8, 15, 4, 1))
##                 interaction.plot(grpCT2[, fact21], grpCT2[, fact22], grpCT2$densite, lwd=2, col=cl[seq(550, (550+(4*(length(split(grpCT2, grpCT2[, fact22]))-1))), by=4)],
##                                  type="b", fun=mean, trace.label = fact22, xlab="Annee", ylab="densite", main=paste("Densite moyenne de", TailleChoisie, "par", fact21, "et", fact22))

##                 ## là on fait des boxplot donc sur l'ensemble des valeurs, on passe donc par la table "unitesptat" et pas "grp12"
##                 x11(width=12, height=8, pointsize=12)
##                 par(mar=c(8, 15, 4, 2), mgp=c(5, 1, 0))
##                 boxplot(unitesptat$densite ~ unitesptat[, fact21] + unitesptat[, fact22], data=unitesptat, varwidth = TRUE, xlab=expression("Densite "(individus/m^2)), main=paste("Densite totale de", TailleChoisie, "par", fact21, "et", fact22), las=1,
##                         col=c(rep(cl[seq(400, (400+(4*(length(split(unitesptat, unitesptat[, fact22]))-1))), by=4)], each=length(split(unitesptat, unitesptat[, fact21])))), horizontal = TRUE)
##                 x1 <- as.vector(tapply(unitesptat$densite, list(unitesptat[, fact21], unitesptat[, fact22]), na.rm = T, mean))
##                 y1 <- 1:length(x1)
##                 points(x1, y1, pch=19, col="red")

##                 x11(width=12, height=8, pointsize=12)
##                 par(mar=c(8, 15, 4, 2))
##                 boxplot(unitesptat$biomasse ~ unitesptat[, fact21] + unitesptat[, fact22], data=unitesptat, varwidth = TRUE, xlab=expression("Biomasse "(g/m^2)), main=paste("Biomasse de", TailleChoisie, "par", fact21, "et", fact22), las=1,
##                         col=c(rep(cl[seq(400, (400+(4*(length(split(unitesptat, unitesptat[, fact22]))-1))), by=4)], each=length(split(unitesptat, unitesptat[, fact21])))), horizontal = TRUE)
##                 bx1 <- as.vector(tapply(unitesptat$biomasse, list(unitesptat[, fact21], unitesptat[, fact22]), na.rm = T, mean))
##                 points(bx1, pch=19, col="red")
##             }

##             x11(width=12, height=8, pointsize=12)
##             par(mar=c(5, 6, 4, 1))
##             interaction.plot(grpCT2[, fact21], grpCT2[, fact22], grpCT2$richesse_specifique, lwd=2, col=cl[seq(550, (550+(4*(length(split(grpCT2, grpCT2[, fact22]))-1))), by=4)], type="b",
##                              fun=mean, trace.label = fact22, xlab="Annee", ylab="Nombre d'especes", main=paste("Richesse specifique de", TailleChoisie, "par", fact21, "et", fact22))

##             if (ct == 1)
##             {
##                 x11(width=12, height=8, pointsize=12)
##                 par(mar=c(8, 15, 4, 2))
##                 boxplot(unitesptat$taille_moyenne ~ unitesptat[, fact21] + unitesptat[, fact22], data=unitesptat, varwidth = TRUE, xlab="Nombre d'especes", main=paste("Taille moyenne de", TailleChoisie, "par", fact21, "et", fact22), las=1,
##                         col=c(rep(cl[seq(400, (400+(4*(length(split(unitesptat, unitesptat[, fact22]))-1))), by=4)], each=length(split(unitesptat, unitesptat[, fact21])))), horizontal = TRUE)
##                 x1 <- as.vector(tapply(unitesptat$taille_moyenne, list(unitesptat[, fact21], unitesptat[, fact22]), na.rm = T, mean))
##                 y1 <- 1:length(x1)
##                 points(x1, y1, pch=19, col="red")
##             }
##         }

##         if (unique(unitobs$type) == "LIT")
##         {
##             tkmessageBox(message="Ces metriques ne peuvent pas être calculees pour les suivis LIT")
##         }
##     } # fin doneVal
## } # fin graphCT2.f

################################################################################
## Nom    : graphCT3.f()
## Objet  : affichage des graphiques par groupe d'unités d'observation
##          et classes de taille
## Input  : tables "unitobs", "unitesptat" + facteurs fact31, fact32 et fact33
## Output : graphiques
################################################################################

## [sup] [yr:12/01/2011]:

## graphCT3.f <- function (fact31, fact32, fact33)
## {

##     print("fonction graphCT3.f activée")
##     AffichageGraphiques.f()

##     if (doneVal==1)
##     {
##         if (unique(unitobs$type) != "LIT")
##         {

##             ## affichage des indices representes sous boxplot dans un message
##             tkmessageBox(message=paste("Les indices representes sous boxplot sont : \n- la densite, \n- la biomasse \n- la taille moyenne"))

##             ## là on fait des boxplot donc sur l'ensemble des valeurs, on passe donc par la table "unit" et pas "grpCT3"
##             cl <- colors()
##             X11()
##             par(mar=c(10, 6, 4, 2))
##             boxplot(unitesptat$densite ~ unitesptat[, fact31] + unitesptat[, fact32] + unitesptat[, fact33],
##                     data=unitesptat, varwidth = TRUE, ylab=expression("Densite "(individus/m^2)),
##                     main=paste("Densite de", TailleChoisie, "par", fact31, "et", fact32, "et", fact33), las=3,
##                     col=c(rep(cl[seq(400, (400+(4*(length(split(unitesptat, unitesptat[, fact33]))-1))), by=4)],
##                     each=length(split(unitesptat, unitesptat[, fact33])), times=length(split(unitesptat, unitesptat[,
##                                                                           fact31])))))
##             bx1 <- as.vector(tapply(unitesptat$densite,
##                                     list(unitesptat[, fact31], unitesptat[, fact32],
##                                          unitesptat[, fact33]), na.rm = T, mean))
##             points(bx1, pch=19, col="red")
##             X11()
##             par(mar=c(10, 6, 4, 2))
##             boxplot(unitesptat$biomasse ~ unitesptat[, fact31] + unitesptat[, fact32] + unitesptat[, fact33],
##                     data=unitesptat, varwidth = TRUE, ylab=expression("Biomasse "(g/m^2)), main=paste("Biomasse de",
##                     TailleChoisie, "par", fact31, "et", fact32, "et", fact33), las=3, col=c(rep(cl[seq(400,
##                     (400+(4*(length(split(unitesptat, unitesptat[, fact33]))-1))), by=4)], each=length(split(unitesptat,
##                     unitesptat[, fact33])), times=length(split(unitesptat, unitesptat[, fact31])))))
##             bx1 <- as.vector(tapply(unitesptat$biomasse,
##                                     list(unitesptat[, fact31], unitesptat[, fact32],
##                                          unitesptat[, fact33]), na.rm = T, mean))
##             points(bx1, pch=19, col="red")
##             if (ct == 1)
##             { # dans le cas où l'on a que les categories de taille et pas les mesures.
##                 X11()
##                 par(mar=c(10, 6, 4, 2))
##                 boxplot(unitesptat$taille_moyenne ~ unitesptat[, fact31] + unitesptat[, fact32] + unitesptat[, fact33],
##                         data=unitesptat, varwidth = TRUE, ylab="Taille moyenne", main=paste("Taille moyenne de",
##                         TailleChoisie, "par", fact31, "et", fact32, "et", fact33), las=3, col=c(rep(cl[seq(400,
##                         (400+(4*(length(split(unitesptat, unitesptat[, fact33]))-1))), by=4)],
##                         each=length(split(unitesptat, unitesptat[, fact33])), times=length(split(unitesptat,
##                         unitesptat[, fact31])))))
##                 bx1 <- as.vector(tapply(unitesptat$taille_moyenne,
##                                         list(unitesptat[, fact31], unitesptat[, fact32],
##                                              unitesptat[, fact33]), na.rm = T, mean))
##                 points(bx1, pch=19, col="red")
##             }
##             rm(fact31, fact32, fact33, envir=.GlobalEnv)
##         }
##         if (unique(unitobs$type) == "LIT")
##         {
##             tkmessageBox(message="Ces metriques ne peuvent pas être calculees pour les suivis LIT")
##         }
##     }
## } # fin graphCT3.f

################################################################################
## FONCTIONS DE CALCUL DE METRIQUES PAR GROUPE D'UNITE D'OBSERVATION
##                                  SUR UNE ESPECE
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

## [sup] [yr:12/01/2011]:

## graphIndicesDiv.f <- function(){
##     print("fonction graphIndicesDiv.f activée")
##     x11(width=15, height=8, pointsize=12)
##     plot(hclust(taxdis), labels = NULL, hang = -1, xlab = "Especes", main="Distances taxonomiques entre especes")
##     X11()
##     plot(div, main="Delta +")
## }
