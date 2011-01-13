## choix d'extraire les valeurs extrêmes des graph
## [sup] [yr: 13/01/2011]:
## extremes.f <- function ()
## {
##     print("fonction extremes.f activée")
##     choixExtremes <- tktoplevel()
##     tkwm.title(choixExtremes, "Extrêmes")
##     cb <- tkcheckbutton(choixExtremes)
##     cbValue <- tclVar("0")
##     tkconfigure(cb, variable=cbValue)
##     tkgrid(tklabel(choixExtremes, text="Extraire les valeurs extrêmes maximales de la représentation graphique (supérieures à 95% de la valeur max)"), cb)

##     OnOK <- function()
##     {
##         cbVal <- as.character(tclvalue(cbValue))
##         tkdestroy(choixExtremes)
##         if (cbVal=="1")
##         {
##             choix <- 1
##             tkmessageBox(message="Les valeurs extrêmes maximales sont retirées de la représentation graphique", icon="info")
##         }
##         if (cbVal=="0")
##         {
##             choix <- 0
##             tkmessageBox(message="Les valeurs extrêmes maximales sont conservées dans la représentation graphique", icon="info")
##         }
##         assign("choix", choix, envir=.GlobalEnv)
##     }
##     ## GraphPartMax peut être modifié par un slide puis retourné

##     OK.but <- tkbutton(choixExtremes, text="OK", command=OnOK)
##     tkgrid(OK.but)
##     tkfocus(choixExtremes)
##     tkwait.window(choixExtremes)
## }

## [sup] [yr: 13/01/2011]:

## ChoixOptionsGraphiques.f <- function ()
## {
##     print("fonction ChoixOptionsGraphiques.f activée")

##     choixGraphiques <- tktoplevel()
##     tkwm.title(choixGraphiques, "Paramètres de sortie graphique")

##     cMax <- tkcheckbutton(choixGraphiques)
##     cTypeAn <- tkcheckbutton(choixGraphiques)
##     cTypeAnBiotope <- tkcheckbutton(choixGraphiques)
##     cTypeAnStatut <- tkcheckbutton(choixGraphiques)
##     cTypeAnBiotopeStatut <- tkcheckbutton(choixGraphiques)
##     cTypeBiotope <- tkcheckbutton(choixGraphiques)
##     cTypeStatut <- tkcheckbutton(choixGraphiques)
##     cTypeBiotopeStatut <- tkcheckbutton(choixGraphiques)
##     cTypeSite <- tkcheckbutton(choixGraphiques)
##     cTypeAnSite <- tkcheckbutton(choixGraphiques)
##     cTypeCarac <- tkcheckbutton(choixGraphiques)
##     cTypeAnCarac <- tkcheckbutton(choixGraphiques)
##     cAfficheNbObs <- tkcheckbutton(choixGraphiques)
##     cAffichePointMoyenne <- tkcheckbutton(choixGraphiques)
##     cAfficheValeurMoyenne <- tkcheckbutton(choixGraphiques)

##     cMaxValue <- tclVar("0")
##     cTypeAnValue <- tclVar("0")
##     cTypeAnBiotopeValue <- tclVar("0")
##     cTypeAnStatutValue <- tclVar("0")
##     cTypeAnBiotopeStatutValue <- tclVar("0")
##     cTypeBiotopeValue <- tclVar("0")
##     cTypeStatutValue <- tclVar("0")
##     cTypeBiotopeStatutValue <- tclVar("0")
##     cTypeSiteValue <- tclVar("0")
##     cTypeAnSiteValue <- tclVar("0")
##     cTypeCaracValue <- tclVar("0")
##     cTypeAnCaracValue <- tclVar("0")
##     cAfficheNbObsValue <- tclVar("0")
##     cAffichePointMoyenneValue <- tclVar("0")
##     cAfficheValeurMoyenneValue <- tclVar("0")
##     NbMinObsPourGraphValue <- tclVar("1")

##     entry.NbMinObsPourGraph <-tkentry(choixGraphiques, width="3", textvariable=NbMinObsPourGraphValue)

##     tkconfigure(cMax, variable=cMaxValue)
##     tkconfigure(cTypeAn, variable=cTypeAnValue)
##     tkconfigure(cTypeAnBiotope, variable=cTypeAnBiotopeValue)
##     tkconfigure(cTypeAnStatut, variable=cTypeAnStatutValue)
##     tkconfigure(cTypeAnBiotopeStatut, variable=cTypeAnBiotopeStatutValue)
##     tkconfigure(cTypeBiotope, variable=cTypeBiotopeValue)
##     tkconfigure(cTypeStatut, variable=cTypeStatutValue)
##     tkconfigure(cTypeBiotopeStatut, variable=cTypeBiotopeStatutValue)
##     tkconfigure(cTypeSite, variable=cTypeSiteValue)
##     tkconfigure(cTypeAnSite, variable=cTypeAnSiteValue)
##     tkconfigure(cTypeCarac, variable=cTypeCaracValue)
##     tkconfigure(cTypeAnCarac, variable=cTypeAnCaracValue)
##     tkconfigure(cAfficheNbObs, variable=cAfficheNbObsValue)
##     tkconfigure(cAffichePointMoyenne, variable=cAffichePointMoyenneValue)
##     tkconfigure(cAfficheValeurMoyenne, variable=cAfficheValeurMoyenneValue)

##     tkgrid(tklabel(choixGraphiques, text="Extraire les valeurs extrêmes maximales \nde la représentation graphique \n(supérieures à 95% de la valeur max)"), cMax, sticky="e")
##     tkgrid(tklabel(choixGraphiques, text="\n"))
##     tkgrid(tklabel(choixGraphiques, text="LES CHOIX DES FACTEURS NE CONTENANT\n QU'UNE VALEUR SONT DESACTIVES"))
##     tkgrid(tklabel(choixGraphiques, text="Graphiques par année"), cTypeAn, sticky="e")
##     tkgrid(tklabel(choixGraphiques, text="Graphiques par année puis biotope"), cTypeAnBiotope, sticky="e")
##     tkgrid(tklabel(choixGraphiques, text="Graphiques par année puis statut"), cTypeAnStatut, sticky="e")
##     tkgrid(tklabel(choixGraphiques, text="Graphiques par année puis biotope puis statut"), cTypeAnBiotopeStatut, sticky="e")
##     tkgrid(tklabel(choixGraphiques, text="Graphiques par biotope"), cTypeBiotope, sticky="e")
##     tkgrid(tklabel(choixGraphiques, text="Graphiques par statut"), cTypeStatut, sticky="e")
##     tkgrid(tklabel(choixGraphiques, text="Graphiques par biotope puis statut"), cTypeBiotopeStatut, sticky="e")
##     tkgrid(tklabel(choixGraphiques, text="Graphiques par site"), cTypeSite, sticky="e")
##     tkgrid(tklabel(choixGraphiques, text="Graphiques par année puis site"), cTypeAnSite, sticky="e")
##     tkgrid(tklabel(choixGraphiques, text="Graphiques par caractéristique (2)"), cTypeCarac, sticky="e")
##     tkgrid(tklabel(choixGraphiques, text="Graphiques par année puis  caractéristique (2)"), cTypeAnCarac, sticky="e")
##     tkgrid(tklabel(choixGraphiques, text="\n"))
##     tkgrid(tklabel(choixGraphiques, text="Afficher les nombres d'enregistrement par boxplot (orange)"), cAfficheNbObs, sticky="e")
##     tkgrid(tklabel(choixGraphiques, text="Afficher les moyennes sur les boxplot (point bleu)"), cAffichePointMoyenne, sticky="e")
##     tkgrid(tklabel(choixGraphiques, text="Afficher les valeurs des moyennes sur les boxplot (en bleu)"), cAfficheValeurMoyenne, sticky="e")
##     tkgrid(tklabel(choixGraphiques, text="\n"))
##     tkgrid(tklabel(choixGraphiques, text="Supprimer les graphiques ayant moins de"))
##     tkgrid(entry.NbMinObsPourGraph)
##     tkgrid(tklabel(choixGraphiques, text=" Observations pour l'espèce.\n"))

##     if (length(unique(unitobs$caracteristique_2))<=1) # on vérifie que le regroupement puisse avoir lieu sur au moins deux valeurs
##     {
##         tkconfigure(cTypeCarac, state="disabled")
##         tkconfigure(cTypeAnCarac, state="disabled")
##     }
##     if (length(unique(unitobs$an))<=1) # on vérifie que le regroupement puisse avoir lieu sur au moins deux valeurs
##     {
##         tkconfigure(cTypeAn, state="disabled")
##         tkconfigure(cTypeAnBiotope, state="disabled")
##         tkconfigure(cTypeAnStatut, state="disabled")
##         tkconfigure(cTypeAnBiotopeStatut, state="disabled")
##         tkconfigure(cTypeAnSite, state="disabled")
##         tkconfigure(cTypeAnCarac, state="disabled")
##     }
##     if (length(unique(unitobs$site))<=1) # on vérifie que le regroupement puisse avoir lieu sur au moins deux valeurs
##     {
##         tkconfigure(cTypeSite, state="disabled")
##         tkconfigure(cTypeAnSite, state="disabled")
##     }

##     OnOK <- function()
##     {
##         cMaxVal <- as.character(tclvalue(cMaxValue))
##         cTypeAnVal <- as.character(tclvalue(cTypeAnValue))
##         cTypeAnBiotopeVal <- as.character(tclvalue(cTypeAnBiotopeValue))
##         cTypeAnStatutVal <- as.character(tclvalue(cTypeAnStatutValue))
##         cTypeAnBiotopeStatutVal <- as.character(tclvalue(cTypeAnBiotopeStatutValue))
##         cTypeBiotopeVal <- as.character(tclvalue(cTypeBiotopeValue))
##         cTypeStatutVal <- as.character(tclvalue(cTypeStatutValue))
##         cTypeBiotopeStatutVal <- as.character(tclvalue(cTypeBiotopeStatutValue))
##         cTypeSiteVal <- as.character(tclvalue(cTypeSiteValue))
##         cTypeAnSiteVal <- as.character(tclvalue(cTypeAnSiteValue))
##         cTypeCaracVal <- as.character(tclvalue(cTypeCaracValue))
##         cTypeAnCaracVal <- as.character(tclvalue(cTypeAnCaracValue))
##         cAfficheNbObsVal <- as.character(tclvalue(cAfficheNbObsValue))
##         cAffichePointMoyenneVal <- as.character(tclvalue(cAffichePointMoyenneValue))
##         cAfficheValeurMoyenneVal <- as.character(tclvalue(cAfficheValeurMoyenneValue))
##         NbMinObsPourGraphVal <- tclvalue(NbMinObsPourGraphValue)
##         tkdestroy(choixGraphiques)

##         ## Nommer les valeurs dans ce qui suit pour rendre l'utilisation plus claire [yr: 23/07/2010]
##         choixgraph <- c("maxExclu" = cMaxVal, # 1
##                         "graphAn" = cTypeAnVal,
##                         "graphAnBiotope" = cTypeAnBiotopeVal, # 3
##                         "graphAnStatut" = cTypeAnStatutVal,
##                         "graphAnBiotopeStatut" = cTypeAnBiotopeStatutVal, # 5
##                         "graphBiotope" = cTypeBiotopeVal,
##                         "graphStatut" = cTypeStatutVal, #7
##                         "graphBiotopeStatut" = cTypeBiotopeStatutVal,
##                         "NbObsOrange" = cAfficheNbObsVal,
##                         "PtMoyenneBleu" = cAffichePointMoyenneVal, # 10
##                         "ChiffreMoyenneBleu" = cAfficheValeurMoyenneVal,
##                         "MinNbObs" = NbMinObsPourGraphVal, # 12
##                         "graphSite" = cTypeSiteVal,
##                         "graphAnSite" = cTypeAnSiteVal,
##                         "graphCarac" = cTypeCaracVal,  # 15
##                         "graphAnCarac" = cTypeAnCaracVal,
##                         "GraphEnPDF"=cEnregistreEnPDFVal, # 17
##                         "separateurGroupe" = cSeparateurRegroupementVal,
##                         "plusieursGraphs"=cPlusieursGraphsParPageVal # 19
##                         )

##         tkmessageBox(message="Executions lancées", icon="info")
##         assign("choixgraph", choixgraph, envir=.GlobalEnv)
##         print(choixgraph)
##     }
##     OK.but <- tkbutton(choixGraphiques, text="OK", command=OnOK)
##     tkgrid(OK.but)
##     tkfocus(choixGraphiques)
##     tkwait.window(choixGraphiques)
## }

## [sup] [yr: 13/01/2011]:

## EnleverMaxExtremes.f <- function (tabExtremes)
## {
##     print("fonction EnleverMaxExtremes.f activée")

##     if (choix == 1)
##     {
##         print(max(tabExtremes, na.rm=T))
##         if (mode(tabExtremes) =="list")
##         {
##             tabExtremes[tabExtremes>GraphPartMax*max(tabExtremes, na.rm=T)] <- NA
##         }else{
##             tabExtremes[which(tabExtremes>GraphPartMax*max(tabExtremes, na.rm=T))] <- NA
##         }
##     }else{
##         print("pas d'extrêmes enlevés")
##         tabExtremes <- tabExtremes
##     }
##     return(tabExtremes)
## }



################################################################################
## Nom    : GraphiqueRecouvrement.f()
## Objet  : graphiques recouvrement diagramme moustache sur chaque espèce
## Input  : matrice "matricerecouvrement"
## Output : boxplot recouvrement
################################################################################

## [sup] [yr: 13/01/2011]:

## PartRecouvrementUnitobs.f <- function (matricerecouvrement, typegraph)
## {
##     print("fonction PartRecouvrementUnitobs activée")
##     extremes.f()
##     ## nbColMax=30
##     PartRecouvrementParUnitobs <- tapply(unitesp$nombre, list(unitesp$code_espece, unitesp$unite_observation), sum, na.rm=TRUE)

##     if (length(PartRecouvrementParUnitobs[PartRecouvrementParUnitobs!=0])>0) # si il y a des recouvrements de renseignés, on fait les graphiques
##     {
##         ReturnVal <- gestionMSGchoix("Définiser le nombre de colones pour le graphique", paste("Votre graphique contient plus de", nbColMax, " colonnes, voulez vous le diviser ?"), nbColMax)
##         if (ReturnVal=="ID_CANCEL")
##         {
##             x11(width=120, height=50, pointsize=10)
##             par(mar=c(7, 6, 2, 8), mgp=c(4.5, 0.5, 0))
##             barplot(PartRecouvrementParUnitobs, col = row(as.matrix(PartRecouvrementParUnitobs)), cex.lab=1.2, las=2, legend.text=TRUE, args.legend = list(x = "topright", title = "Espèces"), main="recouvrements par unite d'observation")
##             return()
##         }
##         tkmessageBox(title="Nombre de colonne défini", message=paste("Votre nombre de colonne est défini à ", ReturnVal, ".", sep=""))
##         if (dim(PartRecouvrementParUnitobs)[1]<(nbColMax+1))
##         {
##             barplot(PartRecouvrementParUnitobs, col = row(PartRecouvrementParUnitobs)[, 0], legend.text=TRUE, main="recouvrements par unite d'observation")
##         }else{  # condition de partage du graphique en plusieurs parties

##             barplot(PartRecouvrementParUnitobs, col = row(as.matrix(PartRecouvrementParUnitobs)), legend.text=TRUE, main="recouvrements par unite d'observation")
##             nbgraphe <- as.integer(dim(PartRecouvrementParUnitobs)[2]/nbColMax+1)
##             gestionMSGinfo.f("MSGnbgraphe", nbgraphe)
##             for (i in 0:(nbgraphe-1))
##             {
##                 debutcol <- i*nbColMax+1
##                 fincol <- (i+1)*nbColMax
##                 X11()
##                 if (i<(nbgraphe-1))
##                 {
##                     barplot(PartRecouvrementParUnitobs[, debutcol:fincol], col = row(as.matrix(PartRecouvrementParUnitobs)), legend.text=TRUE, main=paste("recouvrements par unite d'observation ", debutcol, ":", fincol))
##                 }else{
##                     barplot(PartRecouvrementParUnitobs[, debutcol:dim(PartRecouvrementParUnitobs)[2]], col = row(as.matrix(PartRecouvrementParUnitobs)), legend.text=TRUE, main=paste("recouvrements par unite d'observation ", debutcol, ":", dim(PartRecouvrementParUnitobs)))}
##             }# fin du for
##         }
##     }else{
##         tkmessageBox(message="Graphique impossible - pas d'enregistrements dans votre sélection")
##         gestionMSGerreur.f("ZeroEnregistrement")
##     }
##     choix <- "0"
## }

## [sup] [yr: 13/01/2011]:

## PartRecouvrementTot.f <- function (matricerecouvrement, typegraph)
## {
##     print("fonction PartRecouvrementTot activée")
##     extremes.f()

##     col2 <- rgb(0, 1, 1, 0.5)
##     hist(as.matrix(matricerecouvrement), col = col2)
##     choix <- "0"
## }

## [sup] [yr: 13/01/2011]:

## PartRecouvrementEsp.f <- function (matricerecouvrement, typegraph) # [inc]
## {
##     print("fonction PartRecouvrementEsp activée")}




################################################################################
## Nom    : affichageGraphiques.f
## Objet  : fonction d'affichage des graphiques
## Input  : OUI ou NON
## Output : Graphiques générés
################################################################################
## ! cette fonction ne fait aucun graphique et n'appelle rien : peut être en début de programme (sous le nom
## affichageInterfaceGraphique)

## [sup] [yr: 13/01/2011]:

## AffichageGraphiques.f <- function ()
## {
##     print("fonction AffichageGraphiques.f activée")

##     nn <- tktoplevel()
##     tkwm.title(nn, "Affichage des graphiques")
##     tkgrid(tklabel(nn, text="Voulez-vous afficher les graphiques ?"))
##     done <- tclVar(0)
##     OK.but <- tkbutton(nn, text="OUI", command=function() {tclvalue(done) <- 1})
##     Cancel.but <- tkbutton(nn, text="NON", command=function() {tclvalue(done) <- 2})
##     tkgrid(OK.but, Cancel.but)
##     tkbind(nn, "<Destroy>", function() {tclvalue(done) <- 2})
##     tkfocus(nn)
##     tkwait.variable(done)
##     doneVal <- as.integer(tclvalue(done))
##     assign("doneVal", doneVal, envir=.GlobalEnv)
##     tkdestroy(nn)
## }
################################################################################
## Nom    : graph1.f()
## Objet  : affichage des graphiques par groupe d'unites d'observation
## Input  : tables "unit" (pour pouvoir utiliser des boxplot sur l'ensemble des
## valeurs et non pas sur la valeur agrégée de la table grp)
## + 1 facteur fact
## Output : graphiques
################################################################################

## [sup] [yr: 13/01/2011]:

## graph1.f <- function (fact)
## {

##     print("fonction graph1.f activée")

##     AffichageGraphiques.f()

##     if (doneVal==1)
##     {
##         unit[, fact] <- unitobs[, fact][match(unit$unitobs, unitobs$unite_observation)]
##         assign("unit", unit, envir=.GlobalEnv)

##         if (unique(unitobs$type) != "LIT")
##         {
##             ## affichage des indices representes sous boxplot dans un message
##             tkmessageBox(message=paste("Les indices representes sous boxplot sont :
##     - la densite
##     - la biomasse
##     - la richesse specifique
##     - l indice de Simpson
##     - l indice de Pielou
##     - l indice de Hill
##     - l indice de diversite taxonomique"))

##             ## test existence champ densite
##             if (length(unique(unit$densite))>1)
##             {
##                 X11()
##                 if (length(typePeche)>1)
##                 {
##                     x11(width=12, height=8, pointsize=12)
##                     par(mar=c(8, 15, 4, 2), mgp=c(10, 1, 0))
##                     boxplot(unit$densite ~ unit[, fact], data=unit, varwidth = TRUE, ylab="CPUE en nombre", main=paste(typePeche, "- CPUE par", fact), las=1, horizontal = TRUE)
##                 }else{
##                     boxplot(unit$densite ~ unit[, fact], data=unit, varwidth = TRUE, ylab=expression("Densite "(individus/m^2)), main=paste("Densite d'abondance moyenne par", fact))
##                 }
##                 bx1 <- as.vector(tapply(unit$densite, unit[, fact], na.rm = T, mean))
##                 points(bx1, pch=19, col="red")
##             }
##             ## test existence champ biomasse
##             if (length(unique(unit$biomasse))>1)
##             {
##                 X11()
##                 boxplot(unit$biomasse ~ unit[, fact], data=unit, varwidth = TRUE, ylab=expression("Biomasse "(g/m^2)), main=paste("Biomasse moyenne par", fact))
##                 bx1 <- as.vector(tapply(unit$biomasse, unit[, fact], na.rm = T, mean))
##                 points(bx1, pch=19, col="red")
##             }

##             X11()
##             boxplot(unit$richesse_specifique ~ unit[, fact], data=unit, varwidth = TRUE, ylab="Nombre d'especes", main=paste("Richesse specifique moyenne par", fact))
##             bx1 <- as.vector(tapply(unit$richesse_specifique, unit[, fact], na.rm = T, mean))
##             points(bx1, pch=19, col="red")
##             X11()
##             boxplot(unit$simpson ~ unit[, fact], data=unit, varwidth = TRUE, ylab="Simpson", main="Indice de Simpson")
##             bx1 <- as.vector(tapply(unit$sim, unit[, fact], na.rm = T, mean))
##             points(bx1, pch=19, col="red")
##             X11()
##             boxplot(unit$pielou ~ unit[, fact], data=unit, varwidth = TRUE, ylab="Pielou", main="Indice de Pielou")
##             bx1 <- as.vector(tapply(unit$pielou, unit[, fact], na.rm = T, mean))
##             points(bx1, pch=19, col="red")
##             X11()
##             boxplot(unit$hill ~ unit[, fact], data=unit, varwidth = TRUE, ylab="Hill", main="Indice de Hill")
##             bx1 <- as.vector(tapply(unit$hill, unit[, fact], na.rm = T, mean))
##             points(bx1, pch=19, col="red")
##         } # fin cas != LIT

##         if (unique(unitobs$type) == "LIT")
##         {
##             ## affichage des indices representés sous boxplot dans un message
##             tkmessageBox(message=paste("Les indices representes sous boxplot sont :
##   - la richesse specifique
##   - l indice de Simpson
##   - l indice de Pielou
##   - l indice de Hill
##   - l indice de diversite taxonomique"))

##             X11()
##             boxplot(unit$richesse_specifique ~ unit[, fact], data=unit, varwidth = TRUE, las=3, ylab="Nombre d'especes", main="Richesse specifique")
##             bx1 <- as.vector(tapply(unit$richesse_specifique, unit[, fact], na.rm = T, mean))
##             points(bx1, pch=19, col="red")
##             X11()
##             boxplot(unit$simpson ~ unit[, fact], data=unit, varwidth = TRUE, las=3, ylab="Simpson", main="Indice de Simpson")
##             bx1 <- as.vector(tapply(unit$sim, unit[, fact], na.rm = T, mean))
##             points(bx1, pch=19, col="red")
##             X11()
##             boxplot(unit$pielou ~ unit[, fact], data=unit, varwidth = TRUE, las=3, ylab="Pielou", main="Indice de Pielou")
##             bx1 <- as.vector(tapply(unit$pielou, unit[, fact], na.rm = T, mean))
##             points(bx1, pch=19, col="red")
##             X11()
##             boxplot(unit$hill ~ unit[, fact], data=unit, varwidth = TRUE, las=3, ylab="Hill", main="Indice de Hill")
##             bx1 <- as.vector(tapply(unit$hill, unit[, fact], na.rm = T, mean))
##             points(bx1, pch=19, col="red")
##             X11()
##             boxplot(unit$Delta ~ unit[, fact], data=unit, varwidth = TRUE, ylab="Delta", main="Indice de diversite taxonomique")
##             bx1 <- as.vector(tapply(unit$Delta, unit[, fact], na.rm = T, mean))
##             points(bx1, pch=19, col="red")
##             X11()
##             boxplot(unit$DeltaEtoile ~ unit[, fact], data=unit, varwidth = TRUE, ylab="Delta*", main="Indice d'originalite taxonomique")
##             bx1 <- as.vector(tapply(unit$DeltaEtoile, unit[, fact], na.rm = T, mean))
##             points(bx1, pch=19, col="red")
##             X11()
##             boxplot(unit$LambdaPlus ~ unit[, fact], data=unit, varwidth = TRUE, ylab="Lambda+", main="Indice de variation de l'originalite taxonomique")
##             bx1 <- as.vector(tapply(unit$LambdaPlus, unit[, fact], na.rm = T, mean))
##             points(bx1, pch=19, col="red")
##             X11()
##             boxplot(unit$DeltaPlus ~ unit[, fact], data=unit, varwidth = TRUE, ylab="Delta+", main="Indice d'originalite taxonomique sur les presence/absence")
##             bx1 <- as.vector(tapply(unit$DeltaPlus, unit[, fact], na.rm = T, mean))
##             points(bx1, pch=19, col="red")
##             X11()
##             boxplot(unit$SDeltaPlus ~ unit[, fact], data=unit, varwidth = TRUE, ylab="S Delta+", main="RS*Delta+")
##             bx1 <- as.vector(tapply(unit$SDeltaPlus, unit[, fact], na.rm = T, mean))
##             points(bx1, pch=19, col="red")

##             ii <- tktoplevel()
##             tkwm.title(ii, "Recouvrement par espece")
##             scr <- tkscrollbar(ii, repeatinterval=5, command=function(...)tkyview(tl, ...))
##             tl <- tklistbox(ii, height=20, width=50, selectmode="single", yscrollcommand=function(...)tkset(scr, ...), background="white")
##             tkgrid(tklabel(ii, text="recouvrement de l'espece:"))
##             tkgrid(tl, scr)
##             tkgrid.configure(scr, rowspan=4, sticky="nsw")
##             esp <- sort(especes$code_espece)
##             a <- length(esp)
##             for (i in (1:a))
##             {
##                 tkinsert(tl, "end", esp[i])
##             }

##             tkselection.set(tl, 0)

##             OnOK <- function ()
##             {
##                 choixespece <- esp[as.numeric(tkcurselection(tl))+1]
##                 b <- especes$code_espece[especes$code_espece == choixespece]
##                 tkdestroy(ii)
##                 print(paste("Pourcentage de recouvrement de", choixespece))
##                 unitesp[, fact] <- unitobs[, fact][match(unitesp$unite_observation, unitobs$unite_observation)]
##                 X11()
##                 boxplot(unitesp$recouvrement[unitesp$code_espece == b] ~ unitesp[unitesp$code_espece == b, fact], data=unitesp, varwidth = TRUE, las=3, ylab="%", main="Pourcentage de recouvrement")
##                 X11()
##                 boxplot(unitesp$colonie[unitesp$code_espece == b] ~ unitesp[unitesp$code_espece == b, fact], data=unitesp, varwidth = TRUE, las=3, ylab="Colonies", main="Nombre de colonies")
##             }

##             OK.but <-tkbutton(ii, text="OK", command=OnOK)
##             tkgrid(OK.but)
##             tkfocus(ii)
##             tkwait.window(ii)
##             rm(a)
##         }#fin cas LIT
##     } #fin doneEval
## } #fin graph1.f

################################################################################
## Nom    : graph2.f()
## Objet  : affichage des graphiques par groupe d'unités d'observation
## Input  : tables "unit" (pour pouvoir utiliser des boxplot sur l'ensemble des
##          valeurs et non pas sur la valeur agrégée de la table grp12)
##          + 2 facteurs fact21 et fact22
## Output : graphiques
################################################################################

## [sup] [yr: 13/01/2011]:

## graph2.f <- function (fact21, fact22)
## {

##     print("fonction graph2.f activée")
##     AffichageGraphiques.f()

##     ## si volonté d'afficher les graphiques
##     if (doneVal==1)
##     {
##         unit[, fact21] <- unitobs[, fact21][match(unit$unitobs, unitobs$unite_observation)]
##         unit[, fact22] <- unitobs[, fact22][match(unit$unitobs, unitobs$unite_observation)]
##         assign("unit", unit, envir=.GlobalEnv)

##         ## affichage des indices representes sous boxplot dans un message
##         tkmessageBox(message=paste("Les indices representes sous boxplot sont :
##   - la taille moyenne
##   - la densite
##   - la biomasse
##   - la richesse specifique
##   - l indice de Simpson
##   - l indice de Pielou
##   - l indice de Hill
##   - l indice de diversite taxonomique
##   - l indice d originalite taxonomique
##   - l indice de variation de l originalite taxonomique
##   - l indice d originalite taxonomique sur les presences/absences
##   - le RS*Delta+
##   "))

##         ## test existence champs biomasse et densite
##         if (length(unique(unit$biomasse))>1)
##         {
##             x11(width=9, height=6, pointsize=12)
##             par(mar=c(5, 6, 4, 1))
##             interaction.plot(grp12[, fact21], grp12[, fact22], grp12$densite, lwd=2, col=cl[seq(550, (550+(4*(length(split(grp12, grp12[, fact22]))-1))), by=4)],
##                              type="b", fun=mean, trace.label = fact22, xlab="Annee", ylab=expression("Densite "(individus/m^2)),
##                              main=paste("Densite d'abondance moyenne par", fact21, "et", fact22))
##             x11(width=11, height=8, pointsize=12)
##             par(mar=c(8, 15, 4, 2), mgp=c(5, 1, 0))
##             boxplot(unit$densite ~ unit[, fact21] + unit[, fact22], varwidth = TRUE, xlab=expression("Densite "(individus/m^2)), main=paste("Densite d'abondance moyenne par", fact21, "et", fact22), las=1,
##                     col=c(rep(cl[seq(400, (400+(4*(length(split(unit, unit[, fact22]))-1))), by=4)], each=length(split(unit, unit[, fact21])))), horizontal = TRUE)
##             x1 <- as.vector(tapply(unit$densite, list(unit[, fact22], unit[, fact21]), na.rm = T, mean))
##             y1 <- 1:length(x1)
##             points(x1, y1, pch=19, col="red")
##             x11(width=12, height=8, pointsize=12)
##             par(mar=c(8, 15, 4, 2))
##             boxplot(unit$biomasse ~ unit[, fact21] + unit[, fact22], data=unit, varwidth = TRUE, ylim = c(0, max(1.58*tapply(unit$biomasse, list(unit[, fact21], unit[, fact22]), IQR), na.rm=TRUE)), xlab=expression("Biomasse "(g/m^2)), main=paste("Biomasse moyenne par", fact21, "et", fact22), las=1,
##                     col=c(rep(cl[seq(400, (400+(4*(length(split(unit, unit[, fact22]))-1))), by=4)], each=length(split(unit, unit[, fact21])))), horizontal = TRUE)
##             x1 <- as.vector(tapply(unit$biomasse, list(unit[, fact22], unit[, fact21]), na.rm = T, mean))
##             y1 <- 1:length(x1)
##             points(x1, y1, pch=19, col="red")
##         }
##         ## richesse specifique totale
##         x11(width=9, height=6, pointsize=12)
##         par(mar=c(5, 6, 4, 1))
##         interaction.plot(grp12[, fact21], grp12[, fact22], grp12$richesse_specifique,
##                          lwd=2, col=cl[seq(550, (550+(4*(length(split(grp12, grp12[, fact22]))-1))), by=4)], type="b",
##                          fun=mean, trace.label = fact22, xlab="Annee", ylab="Nombre d'especes", main=paste("Richesse specifique totale par", fact21, "et", fact22))
##         ## richesse specifique moyenne
##         x11(width=12, height=8, pointsize=12)
##         par(mar=c(8, 15, 4, 2))
##         boxplot(unit$richesse_specifique ~ unit[, fact21] + unit[, fact22], data=unit, varwidth = TRUE, xlab="Nombre d'especes", main=paste("Richesse specifique moyenne par", fact21, "et", fact22), las=1,
##                 col=c(rep(cl[seq(400, (400+(4*(length(split(unit, unit[, fact22]))-1))), by=4)], each=length(split(unit, unit[, fact21])))), horizontal = TRUE)
##         x1 <- as.vector(tapply(unit$richesse_specifique, list(unit[, fact22], unit[, fact21]), na.rm = T, mean))
##         y1 <- 1:length(x1)
##         points(x1, y1, pch=19, col="red")
##         x11(width=12, height=8, pointsize=12)
##         par(mar=c(8, 15, 4, 2))
##         boxplot(unit$sim ~ unit[, fact21] + unit[, fact22], data=unit, varwidth = TRUE, xlab="Simpson", main="Indice de Simpson", las=1,
##                 col=c(rep(cl[seq(400, (400+(4*(length(split(unit, unit[, fact22]))-1))), by=4)], each=length(split(unit, unit[, fact21])))), horizontal = TRUE)
##         bx1 <- as.vector(tapply(unit$sim, list(unit[, fact21], unit[, fact22]), na.rm = T, mean))
##         points(bx1, pch=19, col="red")

##         x11(width=12, height=8, pointsize=12)
##         par(mar=c(8, 15, 4, 2))
##         boxplot(unit$pielou ~ unit[, fact21] + unit[, fact22], data=unit, varwidth = TRUE, xlab="Pielou", main="Indice de Pielou", las=1,
##                 col=c(rep(cl[seq(400, (400+(4*(length(split(unit, unit[, fact22]))-1))), by=4)], each=length(split(unit, unit[, fact21])))), horizontal = TRUE)
##         bx1 <- as.vector(tapply(unit$pielou, list(unit[, fact21], unit[, fact22]), na.rm = T, mean))
##         points(bx1, pch=19, col="red")
##         x11(width=12, height=8, pointsize=12)
##         par(mar=c(8, 15, 4, 2))
##         boxplot(unit$hill ~ unit[, fact21] + unit[, fact22], data=unit, varwidth = TRUE, xlab="Hill", main="Indice de Hill", las=1,
##                 col=c(rep(cl[seq(400, (400+(4*(length(split(unit, unit[, fact22]))-1))), by=4)], each=length(split(unit, unit[, fact21])))), horizontal = TRUE)
##         bx1 <- as.vector(tapply(unit$hill, list(unit[, fact21], unit[, fact22]), na.rm = T, mean))
##         points(bx1, pch=19, col="red")
##         x11(width=12, height=8, pointsize=12)
##         par(mar=c(8, 15, 4, 2))
##         boxplot(unit$Delta ~ unit[, fact21] + unit[, fact22], data=unit, varwidth = TRUE, xlab="Delta", main="Indice de diversite taxonomique (Delta)", las=1,
##                 col=c(rep(cl[seq(400, (400+(4*(length(split(unit, unit[, fact22]))-1))), by=4)], each=length(split(unit, unit[, fact21])))), horizontal = TRUE)
##         bx1 <- as.vector(tapply(unit$Delta, list(unit[, fact21], unit[, fact22]), na.rm = T, mean))
##         points(bx1, pch=19, col="red")
##         x11(width=12, height=8, pointsize=12)
##         par(mar=c(8, 15, 4, 2))
##         boxplot(unit$DeltaEtoile ~ unit[, fact21] + unit[, fact22], data=unit, varwidth = TRUE, xlab="Delta*", main="Indice d'originalite taxonomique (Delta*)", las=1,
##                 col=c(rep(cl[seq(400, (400+(4*(length(split(unit, unit[, fact22]))-1))), by=4)], each=length(split(unit, unit[, fact21])))), horizontal = TRUE)
##         bx1 <- as.vector(tapply(unit$DeltaEtoile, list(unit[, fact21], unit[, fact22]), na.rm = T, mean))
##         points(bx1, pch=19, col="red")
##         x11(width=12, height=8, pointsize=12)
##         par(mar=c(8, 15, 4, 2))
##         boxplot(unit$LambdaPlus ~ unit[, fact21] + unit[, fact22], data=unit, varwidth = TRUE, xlab="Lambda+", main="Indice de variation de l'originalite taxonomique (Lambda+)", las=1,
##                 col=c(rep(cl[seq(400, (400+(4*(length(split(unit, unit[, fact22]))-1))), by=4)], each=length(split(unit, unit[, fact21])))), horizontal = TRUE)
##         bx1 <- as.vector(tapply(unit$LambdaPlus, list(unit[, fact21], unit[, fact22]), na.rm = T, mean))
##         points(bx1, pch=19, col="red")
##         x11(width=12, height=8, pointsize=12)
##         par(mar=c(8, 15, 4, 2))
##         boxplot(unit$DeltaPlus ~ unit[, fact21] + unit[, fact22], data=unit, varwidth = TRUE, xlab="Delta+", main="Indice d'originalite taxonomique sur les presence/absence (Delta+)", las=1,
##                 col=c(rep(cl[seq(400, (400+(4*(length(split(unit, unit[, fact22]))-1))), by=4)], each=length(split(unit, unit[, fact21])))), horizontal = TRUE)
##         bx1 <- as.vector(tapply(unit$DeltaPlus, list(unit[, fact21], unit[, fact22]), na.rm = T, mean))
##         points(bx1, pch=19, col="red")
##         x11(width=12, height=8, pointsize=12)
##         par(mar=c(8, 15, 4, 2))
##         boxplot(unit$SDeltaPlus ~ unit[, fact21] + unit[, fact22], data=unit, varwidth = TRUE, xlab="RS Delta+", main="RS*Delta+", las=1,
##                 col=c(rep(cl[seq(400, (400+(4*(length(split(unit, unit[, fact22]))-1))), by=4)], each=length(split(unit, unit[, fact21])))), horizontal = TRUE)
##         bx1 <- as.vector(tapply(unit$SDeltaPlus, list(unit[, fact21], unit[, fact22]), na.rm = T, mean))
##         points(bx1, pch=19, col="red")
##     } #fin doneEval
## } #fin graph2.f

################################################################################
## Nom    : graph3.f()
## Objet  : affichage des graphiques par groupe d'unités d'observation
## Input  : tables "unit" (pour pouvoir utiliser des boxplot sur l'ensemble des
##          valeurs et non pas sur la valeur agrégée de la table grp13)
## Output : graphiques
################################################################################

## [sup] [yr: 13/01/2011]:

## graph3.f <- function (fact31, fact32, fact33)
## {

##     print("fonction graph3.f activée")
##     AffichageGraphiques.f()


##     ## si volonte d'afficher les graphs
##     if (doneVal==1)
##     {

##         unit[, fact31] <- unitobs[, fact31][match(unit$unitobs, unitobs$unite_observation)]
##         unit[, fact32] <- unitobs[, fact32][match(unit$unitobs, unitobs$unite_observation)]
##         unit[, fact33] <- unitobs[, fact33][match(unit$unitobs, unitobs$unite_observation)]
##         assign("unit", unit, envir=.GlobalEnv)

##         ## affichage des indices representes sous boxplot dans un message
##         tkmessageBox(message=paste("Les indices representes sous boxplot sont :
##     - la densite
##     - la biomasse
##     - la richesse specifique
##     - l indice de Simpson
##     - l indice de Pielou
##     - l indice de Hill"))

##         x11(width=12, height=8, pointsize=12)
##         par(mar=c(8, 15, 4, 2))
##         boxplot(unit$densite ~ unit[, fact31] + unit[, fact32] + unit[, fact33], data=unit, varwidth = TRUE, xlab=expression("Densite "(individus/m^2)), main=paste("Densite par", fact31, fact32, "et", fact33), las=1,
##                 col=c(rep(cl[seq(400, (400+(4*(length(split(unit, unit[, fact33]))-1))), by=4)], each=length(split(unit, unit[, fact33])), times=length(split(unit, unit[, fact31])))), horizontal = TRUE)
##         x1 <- as.vector(tapply(unit$densite, list(unit[, fact31], unit[, fact32], unit[, fact33]), na.rm = T, mean))
##         y1 <- 1:length(x1)
##         points(x1, y1, pch=19, col="red")

##         ## test existence champs biomasse et densite
##         if (length(unique(unit$biomasse))>1)
##         {
##             x11(width=12, height=8, pointsize=12)
##             par(mar=c(8, 15, 4, 2))
##             boxplot(unit$biomasse ~ unit[, fact31] + unit[, fact32] + unit[, fact33], data=unit, varwidth = TRUE, xlab=expression("Biomasse "(g/m^2)), main=paste("Biomasse par", fact31, fact32, "et", fact33), las=1,
##                     col=c(rep(cl[seq(400, (400+(4*(length(split(unit, unit[, fact33]))-1))), by=4)], each=length(split(unit, unit[, fact33])), times=length(split(unit, unit[, fact31])))), horizontal = TRUE)
##             x1 <- as.vector(tapply(unit$biomasse, list(unit[, fact31], unit[, fact32], unit[, fact33]), na.rm = T, mean))
##             y1 <- 1:length(x1)
##             points(x1, y1, pch=19, col="red")
##         } # fin test biomasse
##         x11(width=12, height=8, pointsize=12)
##         par(mar=c(8, 15, 4, 2))
##         boxplot(unit$richesse_specifique ~ unit[, fact31] + unit[, fact32] + unit[, fact33], data=unit, varwidth = TRUE, xlab="Nombre d'especes", main=paste("Richesse specifique par", fact31, fact32, "et", fact33), las=1,
##                 col=c(rep(cl[seq(400, (400+(4*(length(split(unit, unit[, fact33]))-1))), by=4)], each=length(split(unit, unit[, fact33])), times=length(split(unit, unit[, fact31])))), horizontal = TRUE)
##         x1 <- as.vector(tapply(unit$richesse_specifique, list(unit[, fact31], unit[, fact32], unit[, fact33]), na.rm = T, mean))
##         y1 <- 1:length(x1)
##         points(x1, y1, pch=19, col="red")
##         x11(width=12, height=8, pointsize=12)
##         par(mar=c(8, 15, 4, 2))
##         boxplot(unit$sim ~ unit[, fact31] + unit[, fact32] + unit[, fact33], data=unit, varwidth = TRUE, xlab="Simpson", main="Indice de Simpson", las=1,
##                 col=c(rep(cl[seq(400, (400+(4*(length(split(unit, unit[, fact33]))-1))), by=4)], each=length(split(unit, unit[, fact33])), times=length(split(unit, unit[, fact31])))), horizontal = TRUE)
##         x1 <- as.vector(tapply(unit$sim, list(unit[, fact31], unit[, fact32], unit[, fact33]), na.rm = T, mean))
##         y1 <- 1:length(x1)
##         points(x1, y1, pch=19, col="red")
##         x11(width=12, height=8, pointsize=12)
##         par(mar=c(8, 15, 4, 2))
##         boxplot(unit$pielou ~ unit[, fact31] + unit[, fact32] + unit[, fact33], data=unit, varwidth = TRUE, xlab="Pielou", main="Indice de Pielou", las=1,
##                 col=c(rep(cl[seq(400, (400+(4*(length(split(unit, unit[, fact33]))-1))), by=4)], each=length(split(unit, unit[, fact33])), times=length(split(unit, unit[, fact31])))), horizontal = TRUE)
##         x1 <- as.vector(tapply(unit$pielou, list(unit[, fact31], unit[, fact32], unit[, fact33]), na.rm = T, mean))
##         y1 <- 1:length(x1)
##         points(x1, y1, pch=19, col="red")
##         x11(width=12, height=8, pointsize=12)
##         par(mar=c(8, 15, 4, 2))
##         boxplot(unit$hill ~ unit[, fact31] + unit[, fact32] + unit[, fact33], data=unit, varwidth = TRUE, xlab="Hill", main="Indice de Hill", las=1,
##                 col=c(rep(cl[seq(400, (400+(4*(length(split(unit, unit[, fact33]))-1))), by=4)], each=length(split(unit, unit[, fact33])), times=length(split(unit, unit[, fact31])))), horizontal = TRUE)
##         x1 <- as.vector(tapply(unit$hill, list(unit[, fact31], unit[, fact32], unit[, fact33]), na.rm = T, mean))
##         y1 <- 1:length(x1)
##         points(x1, y1, pch=19, col="red")
##         x11(width=12, height=8, pointsize=12)
##         par(mar=c(8, 15, 4, 2))
##         boxplot(unit$Delta ~ unit[, fact31] + unit[, fact32] + unit[, fact33], data=unit, varwidth = TRUE, xlab="Delta", main="Indice de diversite taxonomique", las=1,
##                 col=c(rep(cl[seq(400, (400+(4*(length(split(unit, unit[, fact33]))-1))), by=4)], each=length(split(unit, unit[, fact33])), times=length(split(unit, unit[, fact31])))), horizontal = TRUE)
##         x1 <- as.vector(tapply(unit$Delta, list(unit[, fact31], unit[, fact32], unit[, fact33]), na.rm = T, mean))
##         y1 <- 1:length(x1)
##         points(x1, y1, pch=19, col="red")
##         x11(width=12, height=8, pointsize=12)
##         par(mar=c(8, 15, 4, 2))
##         boxplot(unit$DeltaEtoile ~ unit[, fact31] + unit[, fact32] + unit[, fact33], data=unit, varwidth = TRUE, xlab="Delta*", main="Indice d'originalite taxonomique", las=1,
##                 col=c(rep(cl[seq(400, (400+(4*(length(split(unit, unit[, fact33]))-1))), by=4)], each=length(split(unit, unit[, fact33])), times=length(split(unit, unit[, fact31])))), horizontal = TRUE)
##         x1 <- as.vector(tapply(unit$DeltaEtoile, list(unit[, fact31], unit[, fact32], unit[, fact33]), na.rm = T, mean))
##         y1 <- 1:length(x1)
##         points(x1, y1, pch=19, col="red")
##         x11(width=12, height=8, pointsize=12)
##         par(mar=c(8, 15, 4, 2))
##         boxplot(unit$LambdaPlus ~ unit[, fact31] + unit[, fact32] + unit[, fact33], data=unit, varwidth = TRUE, xlab="Lambda+", main="Indice de variation de l'originalite taxonomique", las=1,
##                 col=c(rep(cl[seq(400, (400+(4*(length(split(unit, unit[, fact33]))-1))), by=4)], each=length(split(unit, unit[, fact33])), times=length(split(unit, unit[, fact31])))), horizontal = TRUE)
##         x1 <- as.vector(tapply(unit$LambdaPlus, list(unit[, fact31], unit[, fact32], unit[, fact33]), na.rm = T, mean))
##         y1 <- 1:length(x1)
##         points(x1, y1, pch=19, col="red")
##         x11(width=12, height=8, pointsize=12)
##         par(mar=c(8, 15, 4, 2))
##         boxplot(unit$DeltaPlus ~ unit[, fact31] + unit[, fact32] + unit[, fact33], data=unit, varwidth = TRUE, xlab="Delta+", main="Indice d'originalite taxonomique sur les presence/absence", las=1,
##                 col=c(rep(cl[seq(400, (400+(4*(length(split(unit, unit[, fact33]))-1))), by=4)], each=length(split(unit, unit[, fact33])), times=length(split(unit, unit[, fact31])))), horizontal = TRUE)
##         x1 <- as.vector(tapply(unit$DeltaPlus, list(unit[, fact31], unit[, fact32], unit[, fact33]), na.rm = T, mean))
##         y1 <- 1:length(x1)
##         points(x1, y1, pch=19, col="red")
##         x11(width=12, height=8, pointsize=12)
##         par(mar=c(8, 15, 4, 2))
##         boxplot(unit$SDeltaPlus ~ unit[, fact31] + unit[, fact32] + unit[, fact33], data=unit, varwidth = TRUE, xlab="RS Delta+", main="RS*Delta+", las=1,
##                 col=c(rep(cl[seq(400, (400+(4*(length(split(unit, unit[, fact33]))-1))), by=4)], each=length(split(unit, unit[, fact33])), times=length(split(unit, unit[, fact31])))), horizontal = TRUE)
##         x1 <- as.vector(tapply(unit$SDeltaPlus, list(unit[, fact31], unit[, fact32], unit[, fact33]), na.rm = T, mean))
##         y1 <- 1:length(x1)
##         points(x1, y1, pch=19, col="red")
##     } # fin doneVal
## } #fin graph3.f


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

## [sup] [yr: 13/01/2011]:

## gra1.f <- function (fact)
## {

##     print("fonction gra1.f activée")
##     ## on restreint la table "listespunit" à l'espèce sélectionnée
##     spunit <- subset(listespunit, listespunit$code_espece==sp)
##     spunit[, fact] <- unitobs[, fact][match(spunit$unite_observation, unitobs$unite_observation)]

##     ## boxplot de la densite
##     X11()
##     if (length(typePeche)>1)
##     {
##         boxplot(spunit$densite ~ spunit[, fact], data=spunit, varwidth = TRUE, ylab="CPUE",
##                 main=paste("CPUE de ", sp, sep=""), las=3)
##     }else{
##         boxplot(spunit$densite ~ spunit[, fact], data=spunit, varwidth = TRUE,
##                 ylab=expression("Densite "(individus/m^2)), main=paste("Densite de ", sp, sep=""), las=3)
##     }
##     ## la biomasse n'est pas calculée sur tous les jeux de données
##     if (length(unique(listespunit$biomasse))>1)
##     {
##         ## on vérifie que la biomasse a été calculée pour l'espèce sélectionnée
##         if (length(unique(spunit$biomasse))>1)
##         {
##             X11()
##             boxplot(spunit$biomasse ~ spunit[, fact], data=spunit, varwidth = TRUE, ylab=expression("Biomasse "(g/m^2)),
##                     main=paste("Biomasse de ", sp, sep=""), las=3)
##         }else{
##             tkmessageBox(message="Calcul de biomasse impossible - Coefficients a et b manquants dans le referentiel especes")
##         }
##     }
## } # fin gra1.f

################################################################################
## Nom    : gra2.f
## Objet  : créer les boxplot sur l'espèce sélectionnée
## Input  : table "listespunit" + espèce sp + facteurs fact21 et fact22
## Output : boxplot densite et biomasse
################################################################################

## [sup] [yr: 13/01/2011]:

## gra2.f <- function (fact21, fact22)
## {
##     print("fonction gra2.f activée")
##     ## on restreint la table "listespunit" à l'espèce sélectionnée
##     spunit <- subset(listespunit, listespunit$code_espece==sp)
##     spunit[, fact21] <- unitobs[, fact21][match(spunit$unite_observation, unitobs$unite_observation)]
##     spunit[, fact22] <- unitobs[, fact22][match(spunit$unite_observation, unitobs$unite_observation)]

##     ## boxplot de la densite
##     x11(width=12, height=8, pointsize=12)
##     par(mar=c(8, 15, 4, 2))
##     if (length(typePeche)>1)
##     {
##         boxplot(spunit$densite ~ spunit[, fact21] + spunit[, fact22], data=spunit, varwidth = TRUE, xlab="CPUE",
##                 main=paste("CPUE de ", sp, sep=""), las=1, col=c(rep(cl[seq(400, (400+(4*(length(split(spunit, spunit[,
##                 fact22]))-1))), by=4)], each=length(split(spunit, spunit[, fact21])))), horizontal = TRUE)
##     }else{
##         boxplot(spunit$densite ~ spunit[, fact21] + spunit[, fact22], data=spunit, varwidth = TRUE,
##                 xlab=expression("Densite "(individus/m^2)), main=paste("Densite de ", sp, sep=""), las=1,
##                 col=c(rep(cl[seq(400, (400+(4*(length(split(spunit, spunit[, fact22]))-1))), by=4)],
##                 each=length(split(spunit, spunit[, fact21])))), horizontal = TRUE)
##     }

##     ## la biomasse n'est pas calculée sur tous les jeux de données
##     if (length(unique(listespunit$biomasse))>1)
##     {
##         ## on vérifie que la biomasse a été calculée pour l'espèce sélectionnée
##         if (length(unique(spunit$biomasse))>1)
##         {
##             x11(width=12, height=8, pointsize=12)
##             par(mar=c(8, 15, 4, 2))
##             boxplot(spunit$biomasse ~ spunit[, fact21] + spunit[, fact22], data=spunit, varwidth = TRUE,
##                     xlab=expression("Biomasse "(g/m^2)), main=paste("Biomasse de ", sp, sep=""), las=1,
##                     col=c(rep(cl[seq(400, (400+(4*(length(split(spunit, spunit[, fact22]))-1))), by=4)],
##                     each=length(split(spunit, spunit[, fact21])))), horizontal = TRUE)
##         }else{
##             tkmessageBox(message="Calcul de biomasse impossible - Coefficients a et b manquants dans le referentiel especes")
##         }
##     }
## } # fin gra2.f

################################################################################
## Nom    : gra3.f
## Objet  : créer les boxplot sur l'espèce sélectionnée
## Input  : table "listespunit" + espèce sp + facteurs fact21, fact22 et fact23
## Output : boxplot densite et biomasse
################################################################################

## [sup] [yr: 13/01/2011]:

## gra3.f <- function (fact31, fact32, fact33)
## {
##     print("fonction gra3.f activée")
##     ## on restreint la table "listespunit" à l'espèce sélectionnée
##     spunit <- subset(listespunit, listespunit$code_espece==sp)
##     spunit[, fact31] <- unitobs[, fact31][match(spunit$unite_observation, unitobs$unite_observation)]
##     spunit[, fact32] <- unitobs[, fact32][match(spunit$unite_observation, unitobs$unite_observation)]
##     spunit[, fact33] <- unitobs[, fact33][match(spunit$unite_observation, unitobs$unite_observation)]

##     ## boxplot de la densite ou CPUE
##     x11(width=12, height=8, pointsize=12)
##     par(mar=c(8, 15, 4, 2))
##     if (length(typePeche)>1)
##     {
##         boxplot(spunit$densite ~ spunit[, fact31] + spunit[, fact32] + spunit[, fact33], data=spunit, varwidth = TRUE,
##                 xlab="CPUE", main=paste("CPUE de ", sp, sep=""), las=1,
##                 col=c(rep(cl[seq(400, (400+(4*(length(split(spunit, spunit[, fact33]))-1))), by=4)],
##                 each=length(split(spunit, spunit[, fact33])), times=length(split(spunit, spunit[, fact31])))),
##                 horizontal = TRUE)
##     }else{
##         boxplot(spunit$densite ~ spunit[, fact31] + spunit[, fact32] + spunit[, fact33], data=spunit, varwidth = TRUE,
##                 xlab=expression("Densite "(individus/m^2)), main=paste("Densite de ", sp, sep=""), las=1,
##                 col=c(rep(cl[seq(400, (400+(4*(length(split(spunit, spunit[, fact33]))-1))), by=4)],
##                 each=length(split(spunit, spunit[, fact33])), times=length(split(spunit, spunit[, fact31])))),
##                 horizontal = TRUE)
##     }

##     ## la biomasse n'est pas calculée sur tous les jeux de données
##     if (length(unique(listespunit$biomasse))>1)
##     {
##         ## on vérifie que la biomasse a été calculée pour l'espèce sélectionnée
##         if (length(unique(spunit$biomasse))>1)
##         {
##             x11(width=12, height=8, pointsize=12)
##             par(mar=c(8, 15, 4, 2))
##             boxplot(spunit$biomasse ~ spunit[, fact31] + spunit[, fact32] + spunit[, fact33], data=spunit, varwidth =
##                     TRUE, xlab=expression("Biomasse "(g/m^2)), main=paste("Biomasse de ", sp, sep=""), las=1,
##                     col=c(rep(cl[seq(400, (400+(4*(length(split(spunit, spunit[, fact33]))-1))), by=4)],
##                     each=length(split(spunit, spunit[, fact33])), times=length(split(spunit, spunit[, fact31])))),
##                     horizontal = TRUE)
##         }else{
##             tkmessageBox(message="Calcul de biomasse impossible - Coefficients a et b manquants dans le referentiel especes")
##         }
##     }
## } # fin gra3.f
