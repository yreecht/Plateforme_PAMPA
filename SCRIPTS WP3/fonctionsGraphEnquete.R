################################################################################
# Nom               : fonctionsGraphEnquete.R
# Type              : Programme
# Objet             : Fonctions de calculs des proportions de réponses et de  
#                     lancement des graphiques pour les questions d'enquêtes
# Input             : data importées en txt
# Output            : tableaux et graphs
# Auteur            : Elodie Gamp
# R version         : 2.11.1
# Date de création  : novembre 2011
# Sources
################################################################################

########################################################################################################################
BarplotParticulier.f <- function (tableau, nomTable, facteur, metrique, periode)
{
    ## Purpose: Fonction de calcul des tableux descriptifs et graphiques 
    ##          correspondants pour les questions à choix multiples
    ## ----------------------------------------------------------------------
    ## Arguments: tableau : table de données (data.frame)
    ##            nomTable ; nom de la table de données (character)
    ##            facteur : nom du facteur de séparation (character)
    ##            metrique : nom de la métrique (character)
    ##            periode : choix de séparer ou non les périodes d'échantillonnage
    ## ----------------------------------------------------------------------
    ## Author: Elodie Gamp, Date: 16 novembre 2011

    periodeEchant <- unique(tableau[,periode])
    ## recupération du nomp des champs
    casParticulier <- casParticulier.f()

    champMultiple <- casParticulier[grep(metrique, casParticulier)]

    liste1 <- sapply(seq(1:length(champMultiple)),FUN=function(x){unique(tableau[,champMultiple[x]])})
    liste <- levels(as.factor(liste1))
### calcul de la proportion de réponse par modalité (plusieurs réponses donc >100%)
    TabMulti <- t(sapply(seq(1:length(champMultiple)),
                         FUN=function(x)
                     {
                         table(tableau[,champMultiple[x]])[match(liste, names(table(tableau[ , champMultiple[1]])))]
                     }))   # nb de réponses

    rownames(TabMulti) <- champMultiple
    TabPropMulti <- round(apply(TabMulti,2,sum, na.rm=T)*100/lengthnna.f(tableau[,champMultiple[1]]),digits=2)   # proportions
    TauxReponse <- c(round(lengthnna.f(tableau[,champMultiple[1]])*100/nrow(tableau),digits=2),NA,NA)
    ICinfRslt <- round((TabPropMulti/100+qnorm(0.025)*sqrt(TabPropMulti/100*(1-TabPropMulti/100)/nrow(tableau)))*100,digits=2)
    ICsupRslt <- round((TabPropMulti/100+qnorm(0.975)*sqrt(TabPropMulti/100*(1-TabPropMulti/100)/nrow(tableau)))*100,digits=2)
    TableauResultat <- cbind(rbind(ICinfRslt,TabPropMulti,ICsupRslt),TauxReponse)
    sink(paste("C:/PAMPA/resultats script R/metriquesWP3/opinion/", metrique, "selon", nomTable, facteur, "en", periodeEchant, ".txt"))
    print(TableauResultat)
    sink()

    ## graphique
    x11(width=50,height=30,pointsize=6)
    par(oma=c(0, 0, 3, 0))    # agrandissement de la marge externe supérieure (pour titre général)
    graph <- barplot(TabPropMulti, beside=T, ylim=c(0,max(ICsupRslt,na.rm=T)),
                     main = paste("En", periodeEchant, metrique, "pour l'activité",
                     nomTable),  #, "selon le facteur", facteur),
                     cex.main = 3,cex.axis=2.5, cex.names=2.5) #legend.text=rownames(TabRslt$proportionReponse),args.legend=list(x="topleft")
    arrows(graph, ICinfRslt, graph, ICsupRslt, code = 3, col = "red", angle = 90, length = .1)
    savePlot(filename=paste("C:/PAMPA/resultats script R/metriquesWP3/opinion/barplotProp",metrique,"selon",nomTable,facteur, "en", periodeEchant), type =c("png"))
}


########################################################################################################################
###   FONCTIONS CALCUL TABLEAUX DESCRIPTIFS QUALI ET GRAPHS CORRESPONDANTS   ###

    ## Purpose: Fonction de calcul des tableux descriptifs et graphiques 
    ##          correspondants pour les questions qualitatives
    ## ----------------------------------------------------------------------
    ## Arguments: tableau : table de données (data.frame)
    ##            nomTable ; nom de la table de données (character)
    ##            facteur : nom du facteur de séparation (character)
    ##            metrique : nom de la métrique (character)
    ##            periodEchant : période d'échantillonnage considérée
    ## ----------------------------------------------------------------------
    ## Author: Elodie Gamp, Date: 16 novembre 2011


### Tableau descriptif qualitatif  (pour barplotProp et pie)
TabDescripQuali.f <- function(tableau, nomTable, facteur, metrique, periodeEchant) {
    nbRep <- table(tableau[,metrique],tableau[,facteur])
    total <- apply(nbRep,2,sum)
    propRslt <- round(prop.table(nbRep,2)*100,digits=2)    # proportion de chaque réponse
    tauxReponse <- round(total*100/as.numeric(table(tableau[,facteur])),digits=2)    # taux de réponse
    
    # calcul des IC bootstrap
    estimIC <- BootstrapOpinionQuali.f (tableau = tableau, 
                                        facteur = facteur, 
                                        metrique = metrique)
    ICinfRslt <- round(estimIC[,,"ICInf"] ,digits=2)
    ICinfRslt[which(ICinfRslt < 0)] <- 0
    ICsupRslt <- round(estimIC[,,"ICSup"] ,digits=2)
    ICsupRslt[which(ICsupRslt > 100)] <- 100
    TableauResultat <- list("tauxReponse"=tauxReponse,"proportionReponse"=propRslt,"ICinf"=ICinfRslt,"ICsup"=ICsupRslt)
    sink(paste("C:/PAMPA/resultats script R/metriquesWP3/opinion/",metrique,"selon",nomTable,facteur,"en", periodeEchant, ".txt"))
                                        #  print(tauxReponse)
    print(TableauResultat)
    sink()
    return (TableauResultat)
}

  # graphiques qualitatif : barplot
BarplotPropEnquetes.f <- function(tableau, nomTable, facteur, metrique, periode) {
    periodeEchant <- unique(tableau[,periode])
    TabRslt <- TabDescripQuali.f(tableau, nomTable, facteur, metrique, periodeEchant)
    OptGraph <- OptionGraph.f(TabRslt$proportionReponse, graph="barplot")    #lay (répartition des graphs) et hauteur (hauteur des portions de fenêtre graphique)
                                        #  couleur <- color.choice (nrow(TabRslt$proportionReponse), list(rownames(TabRslt$proportionReponse)))
    couleur <- grey.colors(nrow(TabRslt$proportionReponse))
    x11(width=50,height=30,pointsize=6)
    layout(OptGraph[[1]], height=OptGraph[[2]])
    par(oma=c(0, 0, 5, 0))    # agrandissement de la marge externe supérieure (pour titre général)
    graph <- barplot(TabRslt$proportionReponse, beside=T, ylim=c(0,max(TabRslt$ICsup,na.rm=T)), 
                      col=couleur, main = paste("En", periodeEchant, metrique, "selon le facteur", facteur),
                      cex.main = 3,cex.axis=2.5, cex.names=2.5) #legend.text=rownames(TabRslt$proportionReponse),args.legend=list(x="topleft")
    arrows(graph, TabRslt$ICinf, graph, TabRslt$ICsup, code = 3, col = "red", angle = 90, length = .1)
    ## Affichage de la légence 1) graphique qui n'affiche rien
    plot(1:2, 1:2, type="n", xaxt="n", yaxt="n", xlab="", ylab="", bty="n")
### 2) légende dans le graph vide
    legend("center",
           title="Légende des couleurs",
           legend=rownames(TabRslt$proportionReponse), col=couleur, pch=15, ncol=2, xpd=NA, cex = 3)
    savePlot(filename=paste("C:/PAMPA/resultats script R/metriquesWP3/opinion/barplotProp",metrique,"selon",nomTable,facteur, "en", periodeEchant), type =c("png"))
}


  # graphiques qualitatif : camembert
CamembertEnquetes.f <- function(tableau, nomTable, facteur, metrique, periode)
{
    periodeEchant <- unique(tableau[,periode])
    TabRslt <- TabDescripQuali.f(tableau, nomTable, facteur, metrique, periodeEchant)
    graphNb <- TabRslt$proportionReponse[,apply(TabRslt$proportionReponse,2,sum,na.rm=T)!=0,drop=F]    # enlève les colonnes sans réponse
                                        #  couleur <- color.choice (nrow(graphNb), list(rownames(graphNb)))
    couleur <- grey.colors(nrow(graphNb))
    OptGraph <- OptionGraph.f(graphNb, graph="camembert")    #lay (répartition des graphs) et hauteur (hauteur des portions de fenêtre graphique)
    x11(width=50,height=30,pointsize=6)
    layout(OptGraph[[1]], height=OptGraph[[2]])
    par(oma=c(0, 0, 9, 0))    # agrandissement de la marge externe supérieure (pour titre général)
    graphNb <- TabRslt$proportionReponse[,apply(TabRslt$proportionReponse,2,sum,na.rm=T)!=0,drop=F]    # enlève les colonnes sans réponse
    graph <- sapply(seq(1:ncol(graphNb)),FUN=function(x) {pie(graphNb[,x],main=colnames(graphNb)[x],
                                         labels=paste(graphNb[,x],"%"),
                                         ,cex=3.5,cex.main = 3.5, col=couleur)})
    mtext(paste("En", periodeEchant, metrique, "selon le facteur", facteur),line=2, outer=TRUE, cex = 3.5)
    ## Affichage de la légence 1) graphique qui n'affiche rien
    plot(1:2, 1:2, type="n", xaxt="n", yaxt="n", xlab="", ylab="", bty="n")
### 2) légende dans le graph vide
    legend("center",
           title="Légende des couleurs",
           legend=rownames(graphNb), col=couleur, pch=15, ncol=2, xpd=NA, cex = 3.5)
    savePlot(filename=paste("C:/PAMPA/resultats script R/metriquesWP3/opinion/camembert",metrique,"selon",nomTable,facteur, "en", periodeEchant), type =c("png"))
}

########################################################################################################################
###   FONCTIONS CALCUL TABLEAUX DESCRIPTIFS QUANTI ET GRAPHS CORRESPONDANTS  ###

    ## Purpose: Fonction de calcul des tableux descriptifs et graphiques 
    ##          correspondants pour les questions quantitatives
    ## ----------------------------------------------------------------------
    ## Arguments: tableau : table de données (data.frame)
    ##            nomTable ; nom de la table de données (character)
    ##            facteur : nom du facteur de séparation (character)
    ##            metrique : nom de la métrique (character)
    ##            periodEchant : période d'échantillonnage considérée
    ## ----------------------------------------------------------------------
    ## Author: Elodie Gamp, Date: 16 novembre 2011


### Tableau descriptif quantitatif  (pour barplotMoy et boxplot)
TabDescripQuanti.f <- function(tableau, nomTable, facteur, metrique, periodeEchant) {

    nbTot <-  as.numeric(table(tableau[,facteur]))
    Mini <- tapply(tableau[,metrique],tableau[,facteur],min, na.rm=T)
    Maxi <- tapply(tableau[,metrique],tableau[,facteur],max, na.rm=T)
    Mediane <- tapply(tableau[,metrique],tableau[,facteur],median, na.rm=T)
    Moyenne <- tapply(tableau[,metrique],tableau[,facteur],mean, na.rm=T)
    EcartType <- tapply(tableau[,metrique],tableau[,facteur],sd, na.rm=T)
    Variance <- tapply(tableau[,metrique],tableau[,facteur],var, na.rm=T)
    TauxReponse <- tapply(tableau[,metrique],tableau[,facteur],lengthnna.f)*100/nbTot
    # calcul des IC bootstrap
    estimIC <- BootstrapOpinionQuanti.f (tableau = tableau, 
                                         facteur = facteur, 
                                         metrique = metrique)
    ICinf <- round(estimIC[,,"ICInf"] ,digits=2)
    ICinf[which(ICinfRslt < 0)] <- 0
    ICsup <- round(estimIC[,,"ICSup"] ,digits=2)
    TableauResultat <- round(rbind(Mini, Maxi, Mediane, Moyenne, EcartType, Variance, TauxReponse, ICinf, ICsup), digits=2)
    sink(paste("C:/PAMPA/resultats script R/metriquesWP3/opinion/",metrique,"selon",nomTable,facteur,"en", periodeEchant, ".txt"))
                                        #  print(tauxReponse)
    print(TableauResultat)
    sink()
    return (TableauResultat)
}


  ## graphiques quantitatifs : boxplot
BoxplotEnquetes.f <- function (tableau, nomTable, facteur, metrique, periode){

    periodeEchant <- unique(tableau[,periode])
    TabRslt <- TabDescripQuanti.f(tableau, nomTable, facteur, metrique, periodeEchant)
    OptGraph <- OptionGraph.f(TabRslt$proportionReponse, graph="boxplot")    #lay (répartition des graphs) et hauteur (hauteur des portions de fenêtre graphique)
    x11(width=50,height=30,pointsize=6)
    layout(OptGraph[[1]], height=OptGraph[[2]])
    par(oma=c(0, 0, 4, 0))
    graph <- boxplot(tableau[,metrique]~tableau[,facteur], 
                      main=paste("En", periodeEchant, metrique, "selon le facteur", facteur),
                      cex.main = 3,cex.axis=2.5, cex.names=2.5)
    text(1:ncol(TabRslt),max(TabRslt["Maxi",],na.rm=T),labels=round(TabRslt["Moyenne",],digits=1),col="red",cex=2)
    ## Affichage de la légence 1) graphique qui n'affiche rien
    plot(1:2, 1:2, type="n", xaxt="n", yaxt="n", xlab="", ylab="", bty="n")
### 2) légende dans le graph vide
    legend("center",
           title="Précisions",
           legend=paste("La valeur des moyennes pour chaque valeur de",facteur,"sont rajoutées en haut du graphique"), cex = 2)
    savePlot(filename=paste("C:/PAMPA/resultats script R/metriquesWP3/opinion/boxplot",metrique,"selon",nomTable,facteur, "en", periodeEchant), type =c("png"))
}


  ## graphiques quantitatifs : barplot
BarplotMoyenneEnquetes.f <- function (tableau, nomTable, facteur, metrique, periode){
    periodeEchant <- unique(tableau[,periode])
    TabRslt <- TabDescripQuanti.f(tableau, nomTable, facteur, metrique, periodeEchant)
    OptGraph <- OptionGraph.f(TabRslt$proportionReponse, graph="barplot")    #lay (répartition des graphs) et hauteur (hauteur des portions de fenêtre graphique)
    x11(width=50,height=30,pointsize=6)
    layout(OptGraph[[1]], height=OptGraph[[2]])
    par(oma=c(0, 0, 4, 0))
    graph <- barplot(TabRslt["Moyenne",], 
                     main=paste("En", periodeEchant, metrique, "selon le facteur", facteur),
                     cex.main = 3,cex.axis=2.5, cex.names=2.5,ylim=c(0,max(TabRslt["ICsup",],na.rm=T)))
    arrows(graph, TabRslt["ICinf",],graph, TabRslt["ICsup",], code = 3, col = "red", angle = 90, length = .1)
    ## Affichage de la légence 1) graphique qui n'affiche rien
    plot(1:2, 1:2, type="n", xaxt="n", yaxt="n", xlab="", ylab="", bty="n")
### 2) légende dans le graph vide
    legend("center",
           title="Précisions",
           legend=paste("Les intervalles de confiance sont rajoutés pour chaque valeur de ",facteur), cex = 2)
    savePlot(filename=paste("C:/PAMPA/resultats script R/metriquesWP3/opinion/barplotMoy",metrique,"selon",nomTable,facteur, "en", periodeEchant), type =c("png"))
}


########################################################################################################################
OptionGraph.f <- function (proportionReponse, graph) 
{       

    ## Purpose: définition de la fenêtre graphique (disposition des graphs  
    ##          et de la légende)
    ## ----------------------------------------------------------------------
    ## Arguments: proportionReponse : tableau de résultats à la question (quali ou quanti)
    ##            graph : type de graph choisi
    ## ----------------------------------------------------------------------
    ## Author: Elodie Gamp, Date: 16 novembre 2011

    if (graph =="barplot" | graph =="boxplot"){
        lay = matrix(c(1,2),ncol=1)     # création de la matrice de découpage de la fenêtre  1 = graph, 2 = légende
        hauteur = c(10,3)
    } else {
        nbColTab <- ncol(proportionReponse)  # nb de graph à prévoir
        if (nbColTab == 2) {
            lay = matrix(c(1, 2, 3, 3), ncol=2, byrow=TRUE)
            hauteur = c(10,3)
        } else {
            div = nbColTab%/%2          # 2 = nb de lignes sur lesquels se répartissent les graphs
            modulo = nbColTab%%2        # est-ce qu'il reste des graphs en plus ?
            nbColGraph = div+modulo     # définit le nombre de colonnes de la zone graphique
            if (modulo == 1) {
                                        #            nbLigGraph = 2      # modulo = 1 donc il reste une place pour la légende
                lay = matrix (seq (1 : (nbColTab+1)), ncol=nbColGraph,byrow=TRUE)  # pas nécessaire de définir le nombre de ligne car définit par la matrice)
                hauteur = c(10,10)
            } else {
                                        #            nbLigGraph = 3      # modulo = 0, il n'y a plus de place pour la légende donc je rajoute une ligne
                lay = matrix (c(seq (1 : nbColTab),rep(nbColTab+1,nbColGraph)), ncol=nbColGraph,byrow=TRUE) # pas nécessaire de définir le nombre de ligne car définit par la matrice)
                hauteur = c(10,10,5)
            }
        }
    }
    return(list(lay,hauteur))
}


########################################################################################################################

