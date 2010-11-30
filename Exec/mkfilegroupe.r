
################################################################################
## FONCTIONS DE CALCUL DE METRIQUES PAR GROUPE D'ESPECE
## - grpespcalc.f()
## - grpesp.f()
################################################################################

interpSecteurs.f <- function(sectUnitobs)
{
    ## Purpose: Interpolation des valeurs pour les secteurs non valides
    ##          (rotations valides seulement) en trois étapes :
    ##            1) interpolation sur le même secteur d'après les autres
    ##               rotations.
    ##            2) interpolation sur les secteurs adjacents de la même
    ##               rotation.
    ##            3) moyenne sur la roatation pour les secteurs qui ne
    ##               peuvent être interpolés avec les deux premières étapes.
    ## ----------------------------------------------------------------------
    ## Arguments: sectUnitobs : matrice des secteurs de l'unité
    ##                          d'observation, avec les secteurs en colonnes
    ##                          et les rotations en lignes.
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date:  9 nov. 2010, 14:52


    ## On ne travaille que sur les rotations valides (i.e. ayant au moins un secteur valide) :
    idx <- apply(sectUnitobs, 1, function(x)!all(is.na(x)))

    tmp <- sectUnitobs[idx, , drop=FALSE]

    ## Étape 1 : interpolation sur le même secteur dans les autres rotations :
    tmp <- matrix(apply(tmp, 2, function(x)
                    {
                        x[which(is.na(x))] <- mean(x, na.rm=TRUE)
                        return(x)
                    }), ncol=ncol(tmp))

    ## Étape 2 : interpolation sur les secteurs adjacents de la même rotation :
    if (any(is.na(tmp)))
    {
        tmp <- t(apply(tmp,
                       1,
                       function(x)
                   {
                       xi <- which(is.na(x))
                       x[xi] <- sapply(xi,
                                       function(i, x){mean(x[i + c(0, 2)], na.rm=TRUE)},
                                       x=c(tail(x, 1), x, head(x, 1)))
                       return(x)
                   }))

        if (any(is.na(tmp)))
        {
            ## Étape 3 : moyenne de la rotation sinon :
            tmp <- t(apply(tmp, 1,
                           function(x)
                       {
                           x[which(is.na(x))] <- mean(x, na.rm=TRUE)
                           return(x)
                       }))
        }else{}
    }else{}



    ## Récupération dans le tableau d'origine :
    sectUnitobs[idx, ] <- tmp
    ## sectUnitobs <- as.array(sectUnitobs)

    if (any(dim(sectUnitobs) != c(3, 6))) print(sectUnitobs)


    return(sectUnitobs)
}

statRotation.f <- function(facteurs)
{
    ## Purpose: Calcul des statistiques des abondances (max, sd) par rotation
    ## ----------------------------------------------------------------------
    ## Arguments: facteurs : vecteur des noms de facteurs d'agrégation
    ##                       (résolution à laquelle on travaille).
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date:  9 nov. 2010, 09:25

    ## Identification des secteurs et rotations valides :
    if (is.element("unite_observation", facteurs))
    {
        ## Secteurs valides (les vides doivent tout de même être renseignés) :
        secteurs <- tapply(obs$secteur,
                           as.list(obs[ , c("unite_observation", "rotation", "secteur"), drop=FALSE]),
                           function(x)length(x) > 0)

        ## Les secteurs non renseignés apparaissent en NA et on veut FALSE :
        secteurs[is.na(secteurs)] <- FALSE

        ## Rotations valides :
        rotations <- apply(secteurs, c(1, 2), any) # Au moins un secteur renseigné.

    }else{
        stop("\n\tL'unité d'observation doit faire partie des facteurs d'agrégation !!\n")
    }

    ## ###########################################################
    ## Nombres par rotation avec le niveau d'agrégation souhaité :
    ## Ajouter les interpolations [!!!][SVR]
    nombresRS <- tapply(obs$nombre,
                        as.list(obs[ , c(facteurs, "rotation", "secteur"), drop=FALSE]),
                        sum, na.rm = TRUE)

    tmp <- apply(nombresRS,
                 which(is.element(names(dimnames(nombresRS)), facteurs)),
                 interpSecteurs.f)

    ## On récupère les dimensions d'origine (résultats de la fonction dans 1 vecteur ; attention, plus le même ordre) :
    dim(tmp) <- c(tail(dim(nombresRS), 2), head(dim(nombresRS), -2))

    ## On renomme les dimensions avec leur nom d'origine en tenant compte du nouvel ordre :
    dimnames(tmp) <- dimnames(nombresRS)[c(tail(seq_along(dimnames(nombresRS)), 2),
                                           head(seq_along(dimnames(nombresRS)), -2))]

    ## On restocke le tout dans nombresRS (attentions, toujours travailler sur les noms de dimensions)
    nombresRS <- tmp


    ## Les NAs sont considérés comme des vrais zéros lorsque la rotation est valide :
    nombresRS <- sweep(nombresRS,
                       match(names(dimnames(secteurs)), names(dimnames(nombresRS)), nomatch=NULL),
                       secteurs,        # Tableau des secteurs valides (booléens).
                       function(x, y)
                   {
                       x[is.na(x) & y] <- 0 # Lorsque NA et secteur valide => 0.
                       return(x)
                   })


    nombresR <- apply(nombresRS,
                      which(is.element(names(dimnames(nombresRS)), c(facteurs, "rotation"))),
                      function(x){ifelse(all(is.na(x)), NA, sum(x))})



    ## ##################################################
    ## Statistiques :

    ## Moyennes :
    nombresMean <- apply(nombresR, which(is.element(names(dimnames(nombresR)), facteurs)),
                         function(x,...){ifelse(all(is.na(x)), NA, mean(x,...))}, na.rm=TRUE)

    ## Maxima :
    nombresMax <- apply(nombresR, which(is.element(names(dimnames(nombresR)), facteurs)),
                        function(x,...){ifelse(all(is.na(x)), NA, max(x,...))}, na.rm=TRUE)

    ## Déviation standard :
    nombresSD <- apply(nombresR, which(is.element(names(dimnames(nombresR)), facteurs)),
                       function(x,...){ifelse(all(is.na(x)), NA, sd(x,...))}, na.rm=TRUE)

    ## Nombre de rotations valides :
    nombresRotations <- apply(rotations, 1, sum, na.rm=TRUE)

    ## Retour des résultats sous forme de liste :
    return(list(nombresMean=nombresMean, nombresMax=nombresMax, nombresSD=nombresSD,
                nombresRotations=nombresRotations))
}


################################################################################
## Nom    : grpespcalc.f
## Objet  : calcul des métriques par groupe d'espèce (biomasse, abondance, etc.)
## Input  : x facteur de groupement des espèces (mobilité, famille, etc.)
## Output : table grpesp
################################################################################

grpespcalc.f <- function(x)
{
    print("fonction grpespcalc.f activée")
    if (exists("grpesp", envir=.GlobalEnv, frame, mode="any", inherits=TRUE))
    {
        rm(grpesp)
    }
    ## somme des abondances
    obs[, x] <- especes[, x][match(obs$code_espece, especes$code_espece)]
    grpespT <- tapply(obs$nombre, list(obs$unite_observation, obs[, x]), sum, na.rm = TRUE)
    grpespT[is.na(grpespT)] <- 0
    grpesp <- as.data.frame(matrix(NA, dim(grpespT)[1]*dim(grpespT)[2], 3))
    colnames(grpesp) = c("unitobs", x, "nombre")
    grpesp$nombre <- as.vector(grpespT)
    ## on divise les abondances par 3 dans le cas de la vidéo (3 rotations)
    if (unique(unitobs$type) == "SVR")
    {
        grpesp$nombre <- grpesp$nombre / 3
    }
    grpesp$unitobs <- rep(dimnames(grpespT)[[1]], dim(grpespT)[2])
    grpesp[, x] <- rep(dimnames(grpespT)[[2]], each = dim(grpespT)[1], 1)

    if (!is.benthos.f())                # unique(unitobs$type) != "LIT"
    {
        ## somme des biomasses
        ## calcul des biomasses à partir des relations taille-poids :

        ## obs$biomasse <- rep(0, nrow(obs)) # [!!!] il ne faut pas initialiser des variables, ou bien juste avec des NAs
        ##                                 # [yr: 13/08/2010]
        if (siteEtudie == "BA" || siteEtudie == "CB" || siteEtudie == "CR")
        {
            obs$biomasse <- obs$nombre * (especes$Coeff.a.Med[match(obs$code_espece, especes$code_espece)] *
                                          obs$taille^especes$Coeff.b.Med[match(obs$code_espece, especes$code_espece)])
        }else{
            if (siteEtudie == "MAY" || siteEtudie == "RUN")
            {
                obs$biomasse <- obs$nombre * (especes$Coeff.a.MAY[match(obs$code_espece, especes$code_espece)] *
                                              obs$taille^especes$Coeff.b.MAY[match(obs$code_espece, especes$code_espece)])
            }else{
                ## cas NC et STM
                obs$biomasse <- obs$nombre * (especes$Coeff.a.NC[match(obs$code_espece, especes$code_espece)] *
                                              obs$taille^especes$Coeff.b.NC[match(obs$code_espece, especes$code_espece)])
            }
        }
        if (siteEtudie == "BO" )
        {
            obs$biomasse <- obs$nombre *
                (especes$poids.moyen.moyens[match(obs$code_espece,
                                                  especes$code_espece)][especes$poids.moyen.moyens!=-999]) # [!!!] -999
                                                    # ou NAs ?  [yr: 13/08/2010]
        }
        ## obs$biomasse[is.na(obs$biomasse)] <- 0
                                        # [!!!] Arghh ! Mais quelle horreur... de remplacer des données
                                        # manquantes par des zéros  [yr: 13/08/2010]

        grpespT.b <- tapply(obs$biomasse, list(obs$unite_observation, obs[, x]),
                            function(x){if (all(is.na(x))) {return(NA)}else{return(sum(x, na.rm=TRUE))}}) # Pour éviter
                                        # des sommes à zéro là où seulement des NAs. ## sum)    # la selection
                                        # pour les especes à considerer dans les calculs de biomasses est appliquee.
        grpesp.b <- as.data.frame(matrix(NA, dim(grpespT.b)[1]*dim(grpespT.b)[2], 3))  # Attention, il se pourrait que
                                        # grpest.b et grpesp n'aient pas les même dimensions, dans ce cas là ça ne
                                        # marchera pas. Penser à une modification du script.
        colnames(grpesp.b) = c("unitobs", "Grp_esp", "biomasse")
        grpesp.b$biomasse <- as.vector(grpespT.b)
        grpesp$biomasse <- grpesp.b$biomasse / (unitobs$DimObs1[match(grpesp$unitobs, unitobs$unite_observation)]*
                                                unitobs$DimObs2[match(grpesp$unitobs, unitobs$unite_observation)])
    }

    ## calcul densites
    if (unique(unitobs$type) != "SVR")
    {
        grpespT2 <- tapply(obs$nombre, list(obs$unite_observation, obs[, x]), sum, na.rm = TRUE)

        grpesp.d <- as.data.frame(matrix(NA, dim(grpespT2)[1]*dim(grpespT2)[2], 3)) # Attention, il se pourrait que
                                        # grpest.d et grpesp n'est pas les même dimensions, dans ce cas là ça ne
                                        # marchera pas. Penser à une modification du script.
        colnames(grpesp.d) = c("unitobs", "grp_esp", "densite")
        grpesp.d$densite <- as.vector(grpespT2)
        grpesp$densite <- grpesp.d$densite / ((unitobs$DimObs1[match(grpesp$unitobs, unitobs$unite_observation)])*
                                              (unitobs$DimObs2[match(grpesp$unitobs, unitobs$unite_observation)]))
    }else{
        ## on ne retient que les individus vus dans un rayon de 5m
        grpesp$densite <- grpesp$nombre / (pi * 25)
    }

    grpesp$pres_abs[grpesp$densite!=0] <- 1   # il peut donc y avoir des espece comptees dans les P/A et pas dans les
                                        # densites...
    grpesp$pres_abs[grpesp$densite==0] <- 0

    ## calcul Richesse Specifique
    unitesp[, x] <- especes[, x][match(unitesp$code_espece, especes$code_espece)]
    grpespT3 <- tapply(unitesp$pres_abs, list(unitesp$unite_observation, unitesp[, x]), sum, na.rm = TRUE)
    grpesp.RS <- as.data.frame(matrix(NA, dim(grpespT3)[1]*dim(grpespT3)[2], 3))
    colnames(grpesp.RS) = c("unitobs", x, "RS")
    grpesp.RS$RS <- as.vector(grpespT3)
    grpesp.RS$unitobs <- rep(dimnames(grpespT3)[[1]], dim(grpespT3)[2])
    grpesp.RS[, x] <- rep(dimnames(grpespT3)[[2]], each = dim(grpespT3)[1], 1)
    grpesp <- merge(grpesp, grpesp.RS, by = intersect(names(grpesp), names(grpesp.RS)))

    ## ajout des champs "an", "site", "biotope" et "statut de protection"
    grpesp$an <- unitobs$an[match(grpesp$unitobs, unitobs$unite_observation)]
    grpesp$site <- unitobs$site[match(grpesp$unitobs, unitobs$unite_observation)]
    grpesp$biotope <- unitobs$biotope[match(grpesp$unitobs, unitobs$unite_observation)]
    grpesp$statut_protection <- unitobs$statut_protection[match(grpesp$unitobs, unitobs$unite_observation)]

    ## graphiques de densité ou CPUE
    if (length(unique(grpesp$densite))> 1)
    {
        if (length(typePeche)>1)
        {
            x11(width=12, height=8, pointsize=12)
            par(mar=c(8, 15, 4, 2), mgp=c(10, 1, 0))
            boxplot(grpesp$densite ~ grpesp[, x], data=grpesp, varwidth = TRUE, ylab="CPUE en nombre",
                    main=paste(typePeche, "- CPUE par", x), las=1, horizontal = TRUE)
        }else{
            x11(width=11, height=6, pointsize=12)
            par(mar=c(8, 15, 4, 2), mgp=c(10, 1, 0))
            boxplot(grpesp$densite ~ grpesp[, x], data=grpesp, varwidth = TRUE, ylab="Densite (individus/m²)",
                    main=paste("Densite d'abondance par", x), las=1, horizontal = TRUE)
            ## par site
            x11(width=11, height=6, pointsize=12)
            par(mar=c(8, 15, 4, 2), mgp=c(12, 1, 0))
            boxplot(grpesp$densite ~ grpesp[, x]*grpesp$site, data=grpesp, varwidth = TRUE,
                    ylab="Densite (individus/m²)", main=paste("Densite d'abondance par", x, "et site"), las=1,
                    horizontal = TRUE)
            ## site et biotope
            if (length(unique(grpesp$biotope))> 1)
            {
                x11(width=14, height=8, pointsize=12)
                par(mar=c(10, 32, 4, 1), mgp=c(31, 1, 0))
                boxplot(grpesp$densite ~ grpesp[, x]*grpesp$site*grpesp$biotope, data=grpesp, varwidth = TRUE,
                        ylab="Densite (individus/m²)", main=paste("Densite d'abondance par", x, "et site*biotope"),
                        las=1, horizontal = TRUE)
            }
            ## par site et an
            x11(width=18, height=8, pointsize=12)
            par(mar=c(3, 25, 4, 2), mgp=c(22, 1, 0))
            boxplot(grpesp$densite ~ grpesp[, x]*grpesp$site*grpesp$an, data=grpesp, varwidth = TRUE,
                    ylab="Densite (individus/m²)", main=paste("Densite d'abondance par", x, "et site et an"), las=1,
                    horizontal = TRUE)
            ## par an et statut_protection
            x11(width=18, height=8, pointsize=12)
            par(mar=c(3, 25, 4, 2), mgp=c(22, 1, 0))
            boxplot(grpesp$densite ~ grpesp[, x]*grpesp$an*grpesp$statut_protection, data=grpesp, varwidth = TRUE,
                    ylab="Densite (individus/m²)", main=paste("Densite d'abondance par", x, "et an*statut_protection"),
                    las=1, horizontal = TRUE)
            ## évolution de la densité d'abondance
            x11(width=12, height=8, pointsize=12)
            par(mar=c(5, 6, 4, 1))
            interaction.plot(grpesp$an, grpesp[, x], grpesp$densite, lwd=2,
                             col=cl[seq(550, (550+(4*(length(split(grpesp, grpesp$an))-1))), by=4)], type="b", fun=mean,
                             trace.label = x, xlab="Annee", ylab="Densite",
                             main=paste("Evolution de la densite d'abondance par", x))
        }
    }
    ## richesse_specifique
    x11(width=11, height=6, pointsize=12)
    par(mar=c(8, 15, 4, 2), mgp=c(10, 1, 0))
    boxplot(grpesp$RS ~ grpesp[, x], data=grpesp, varwidth = TRUE, ylab="Richesse specifique",
            main=paste("Richesse specifique par", x), las=1, horizontal = TRUE)

    ## test l'existence du champ biomasse
    if (length(unique(grpesp$biomasse))> 1)
    {
        ## biomasse 1 facteur
        x11(width=11, height=6, pointsize=12)
        par(mar=c(8, 15, 4, 2), mgp=c(10, 1, 0))
        boxplot(grpesp$biom ~ grpesp[, x], data=grpesp, varwidth = TRUE, ylab="Biomasse (g/m²)",
                main=paste("Biomasse par", x), las=1, horizontal = TRUE)
        ## biomasse 1 facteur et site
        x11(width=11, height=6, pointsize=12)
        par(mar=c(8, 15, 4, 2), mgp=c(12, 1, 0))
        boxplot(grpesp$biomasse ~ grpesp[, x]*grpesp$site, data=grpesp, varwidth = TRUE, ylab="Biomasse (g/m²)",
                main=paste("Biomasse par", x, "et site"), las=1, horizontal = TRUE)
        ## site et biotope
        if (length(unique(grpesp$biotope))> 1)
        {
            x11(width=14, height=8, pointsize=12)
            par(mar=c(10, 32, 4, 1), mgp=c(31, 1, 0))
            boxplot(grpesp$biomasse ~ grpesp[, x]*grpesp$site*grpesp$biotope, data=grpesp, varwidth = TRUE,
                    ylab="Biomasse (g/m²)", main=paste("Biomasse par", x, "et site*biotope"), las=1, horizontal = TRUE)
        }
        ## site et an
        x11(width=14, height=8, pointsize=12)
        par(mar=c(8, 25, 4, 1), mgp=c(22, 1, 0))
        boxplot(grpesp$biomasse ~ grpesp[, x]*grpesp$site*grpesp$an, data=grpesp, varwidth = TRUE,
                ylab="Biomasse (g/m²)", main=paste("Biomasse par", x, "et site*an"), las=1, horizontal = TRUE)
        ## an et statut protection
        x11(width=14, height=8, pointsize=12)
        par(mar=c(8, 25, 4, 1), mgp=c(22, 1, 0))
        boxplot(grpesp$biomasse ~ grpesp[, x]*grpesp$an*grpesp$statut_protection, data=grpesp, varwidth = TRUE,
                ylab="Biomasse (g/m²)", main=paste("Biomasse par", x, "et an*statut_protection"), las=1,
                horizontal = TRUE)
    }

    write.csv(grpesp, file=paste(nameWorkspace, "/FichiersSortie/grpesp", x, ".csv", sep=""), row.names = FALSE)
    assign("grpesp", grpesp, envir=.GlobalEnv)
} ##  fin grpespcalc.f

################################################################################
## FONCTIONS DE CALCUL DE METRIQUES PAR GROUPE D'UNITE D'OBSERVATION
##     - unfacteur.f(), deuxfacteurs.f(), troisfacteurs.f()
##     - grp1.f(), grp2.f(), grp3.f()
##     - graph1.f(), graph2.f(), graph3.f()
##     - occurrence.f(), occurrence2.f(), occurrence3.f()
################################################################################

################################################################################
## Nom    : occurrence.f()
## Objet  : choix d'une espèce parmi la liste des espèces présentes
##          calcul de la fréquence d'occurrence de l'espèce dans la table "grp"
## Input  : tables "grp" + facteur fact
## Output : fréquence d'occurrence de l'espèce sélectionnée selon les facteurs
##          de regroupement
################################################################################
## !Fonction à mieux documenter


occurrence.f <- function(fact)
{
    print("fonction occurrence.f activée")
    ## Demande d'affichage du pourcentage d'occurrence d'une espece
    nn <- tktoplevel()
    tkwm.title(nn, "Pourcentage d'occurrence")
    tkgrid(tklabel(nn, text="Voulez-vous calculer le pourcentage d'occurrence d'une espece?"))
    done <- tclVar(0)
    OK.but <- tkbutton(nn, text="OUI", command=function() {tclvalue(done) <- 1})
    Cancel.but <- tkbutton(nn, text="NON", command=function() {tclvalue(done) <- 2})
    tkgrid(OK.but, Cancel.but)
    tkbind(nn, "<Destroy>", function() {tclvalue(done) <- 2})
    tkfocus(nn)
    tkwait.variable(done)
    doneVal <- as.integer(tclvalue(done))
    tkdestroy(nn)

    if (doneVal==1)
    {
        tt <- tktoplevel()
        tkwm.title(tt, "Pourcentage d'occurrence par espece")
        scr <- tkscrollbar(tt, repeatinterval=5, command=function(...){tkyview(tl, ...)})
        tl <- tklistbox(tt, height=20, width=50, selectmode="single",
                        yscrollcommand=function(...){tkset(scr, ...)}, background="white")
        tkgrid(tklabel(tt, text="Especes presentes"))
        tkgrid(tl, scr)
        tkgrid.configure(scr, rowspan=4, sticky="nsw")

        ## on ne propose uniquement les especes presentes dans le jeu de donnees
        ## ATTENTION PRENDRE EN COMPTE LES ESPECES EXCLUES
        especesPresentes <- subset(unitesp, unitesp$pres_abs==1)$code_espece
        esp <- sort(unique(especesPresentes))
        a <- length(esp)
        for (i in (1:a))
        {
            tkinsert(tl, "end", esp[i])
        }
        tkselection.set(tl, 0)

        OnOK <- function()
        {
            choixespece <- esp[as.numeric(tkcurselection(tl))+1]
            b <- especes$code_espece[especes$code_espece == choixespece]
            tkdestroy(tt)
            print(paste("Pourcentage d'occurrence de", choixespece))
            print(grp[, b])
            assign("b", b, envir=.GlobalEnv)
        }
        OK.but <-tkbutton(tt, text="OK", command=OnOK)
        tkgrid(OK.but)
        tkfocus(tt)
        tkwait.window(tt)
        rm(a)
        rm(fact, envir=.GlobalEnv)
    } ##  fin doneVal
} ## fin occurrence.f

################################################################################
## Nom    : choixDeuxFacteursUnitobs.f()
## Objet  : choix du 2 facteurs de groupement des unités d'observations
## Input  : tables "unit" et "unitobs"
## Output : table "unit" + les 2 facteurs de la table unitobs choisis
################################################################################

choixDeuxFacteursUnitobs.f <- function()
{

    print("fonction choixDeuxFacteursUnitobs.f activée")

    ## selection du premier facteur
    aa <- tktoplevel()
    tkwm.title(aa, "Selection du PREMIER facteur de groupement des unites d'observation")
    scr <- tkscrollbar(aa, repeatinterval=5, command=function(...){tkyview(tl, ...)})
    tl <- tklistbox(aa, height=20, selectmode="single",
                    yscrollcommand=function(...){tkset(scr, ...)}, background="white")
    tkgrid(tklabel(aa, text="Liste des facteurs de groupement"))
    tkgrid(tl, scr)
    tkgrid.configure(scr, rowspan=4, sticky="nsw")
    facts <- sort(names(unitobs))
    a <- length(facts)
    for (i in (1:a))
    {
        tkinsert(tl, "end", facts[i])
    }
    tkselection.set(tl, 0)

    OnOK <- function()
    {
        fact21 <- facts[as.numeric(tkcurselection(tl))+1]
        assign("fact21", fact21, envir=.GlobalEnv)
        tkdestroy(aa)
        assign("unit", unit, envir=.GlobalEnv)
    }
    OK.but <-tkbutton(aa, text="OK", command=OnOK)
    tkgrid(OK.but)
    tkfocus(aa)
    tkwait.window(aa)
    rm(a)

    ## selection du deuxieme facteur ##
    aa <- tktoplevel()
    tkwm.title(aa, "Selection du SECOND facteur de groupement des unites d'observation")
    scr <- tkscrollbar(aa, repeatinterval=5, command=function(...){tkyview(tl, ...)})
    tl <- tklistbox(aa, height=20, selectmode="single",
                    yscrollcommand=function(...){tkset(scr, ...)}, background="white")
    tkgrid(tklabel(aa, text="Liste des facteurs de groupement"))
    tkgrid(tl, scr)
    tkgrid.configure(scr, rowspan=4, sticky="nsw")
    facts <- sort(names(unitobs))
    a <- length(facts)
    for (i in (1:a))
    {
        tkinsert(tl, "end", facts[i])
    }
    tkselection.set(tl, 0)

    OnOK <- function()
    {
        fact22 <- facts[as.numeric(tkcurselection(tl))+1]
        assign("fact22", fact22, envir=.GlobalEnv)
        tkdestroy(aa)
        assign("unit", unit, envir=.GlobalEnv)
    }
    OK.but <-tkbutton(aa, text="OK", command=OnOK)
    tkgrid(OK.but)
    tkfocus(aa)
    tkwait.window(aa)
    rm(a)
} ##  fin choixDeuxFacteursUnitobs.f

################################################################################
## Nom    : occurrence2.f()
## Objet  : choix d'une espèce parmi la liste des espèces présentes
##          calcul de la fréquence d'occurrence de l'espèce dans la table "grp12"
## Input  : tables "grp12" + facteur fact21 et fact22
## Output : fréquence d'occurrence de l'espèce sélectionnée selon les facteurs
##          de regroupement
################################################################################

occurrence2.f <- function(fact21, fact22)
{
    ## Demande d'affichage du pourcentage d'occurrence d'une espece
    nn <- tktoplevel()
    tkwm.title(nn, "Pourcentage d'occurrence")
    tkgrid(tklabel(nn, text="Voulez-vous calculer le pourcentage d'occurrence d'une espece?"))
    done <- tclVar(0)
    OK.but <- tkbutton(nn, text="OUI", command=function() {tclvalue(done) <- 1})
    Cancel.but <- tkbutton(nn, text="NON", command=function() {tclvalue(done) <- 2})
    tkgrid(OK.but, Cancel.but)
    tkbind(nn, "<Destroy>", function() {tclvalue(done) <- 2})
    tkfocus(nn)
    tkwait.variable(done)
    doneVal <- as.integer(tclvalue(done))
    tkdestroy(nn)

    if (doneVal==1)
    {
        tt <- tktoplevel()
        tkwm.title(tt, "Pourcentage d'occurrence par espece")
        scr <- tkscrollbar(tt, repeatinterval=5, command=function(...){tkyview(tl, ...)})
        tl <- tklistbox(tt, height=20, width=50, selectmode="single",
                        yscrollcommand=function(...){tkset(scr, ...)}, background="white")
        tkgrid(tklabel(tt, text="Especes presentes:"))
        tkgrid(tl, scr)
        tkgrid.configure(scr, rowspan=4, sticky="nsw")

        ## on ne propose uniquement les especes presentes dans le jeu de donnees
        ## ATTENTION PRENDRE EN COMPTE LES ESPECES EXCLUES
        especesPresentes <- subset(unitesp, unitesp$pres_abs==1)$code_espece
        esp <- sort(unique(especesPresentes))
        a <- length(esp)
        for (i in (1:a))
        {
            tkinsert(tl, "end", esp[i])
        }
        tkselection.set(tl, 0)

        OnOK <- function()
        {
            choixespece <- esp[as.numeric(tkcurselection(tl))+1]
            b <- especes$code_espece[especes$code_espece == choixespece]
            tkdestroy(tt)
            print(paste("Pourcentage d'occurrence de", choixespece))
            print(cbind(grp12[, fact21], grp12[, fact22], grp12[, b]))
            assign("b", b, envir=.GlobalEnv)
        }
        OK.but <-tkbutton(tt, text="OK", command=OnOK)
        tkgrid(OK.but)
        tkfocus(tt)
        tkwait.window(tt)
        rm(a)

        rm(fact21, fact22, envir=.GlobalEnv)
        rm(b, envir=.GlobalEnv)
    }## fin doneVal
} ##  fin occurrence2.f

################################################################################
## Nom    : choixTroisFacteurs.f()
## Objet  : choix du facteur de groupement des unités d'observations
## Input  : tables "unit" et "unitobs"
## Output : table "unit" + les 3 facteurs de la table unitobs choisis
################################################################################
## ! code dans un fichier "choix"

choixtroisfacteurs.f <- function()
{

    ## sélection du premier facteur
    aa <- tktoplevel()
    tkwm.title(aa, "Selection du PREMIER facteur de groupement des unites d'observation")
    scr <- tkscrollbar(aa, repeatinterval=5, command=function(...){tkyview(tl, ...)})
    tl <- tklistbox(aa, height=20, selectmode="single",
                    yscrollcommand=function(...){tkset(scr, ...)}, background="white")
    tkgrid(tklabel(aa, text="Liste des facteurs de groupement"))
    tkgrid(tl, scr)
    tkgrid.configure(scr, rowspan=4, sticky="nsw")
    facts <- sort(names(unitobs))
    a <- length(facts)
    for (i in (1:a))
    {
        tkinsert(tl, "end", facts[i])
    }
    tkselection.set(tl, 0)

    OnOK <- function(){
        fact31 <- facts[as.numeric(tkcurselection(tl))+1]
        assign("fact31", fact31, envir=.GlobalEnv)
        tkdestroy(aa)
    }
    OK.but <-tkbutton(aa, text="OK", command=OnOK)
    tkgrid(OK.but)
    tkfocus(aa)
    tkwait.window(aa)
    rm(a)

    ## sélection du deuxieme facteur
    aa <- tktoplevel()
    tkwm.title(aa, "Selection du SECOND facteur de groupement des unites d'observation")
    scr <- tkscrollbar(aa, repeatinterval=5, command=function(...){tkyview(tl, ...)})
    tl <- tklistbox(aa, height=20, selectmode="single",
                    yscrollcommand=function(...){tkset(scr, ...)}, background="white")
    tkgrid(tklabel(aa, text="Liste des facteurs de groupement"))
    tkgrid(tl, scr)
    tkgrid.configure(scr, rowspan=4, sticky="nsw")
    facts <- sort(names(unitobs))
    a <- length(facts)
    for (i in (1:a))
    {
        tkinsert(tl, "end", facts[i])
    }
    tkselection.set(tl, 0)

    OnOK <- function()
    {
        fact32 <- facts[as.numeric(tkcurselection(tl))+1]
        assign("fact32", fact32, envir=.GlobalEnv)
        tkdestroy(aa)
    }
    OK.but <-tkbutton(aa, text="   OK   ", command=OnOK)
    tkgrid(OK.but)
    tkfocus(aa)
    tkwait.window(aa)
    rm(a)

    ## sélection du troisieme facteur
    aa <- tktoplevel()
    tkwm.title(aa, "Selection du TROISIEME facteur de groupement des unites d'observation")
    scr <- tkscrollbar(aa, repeatinterval=5, command=function(...){tkyview(tl, ...)})
    tl <- tklistbox(aa, height=20, selectmode="single",
                    yscrollcommand=function(...){tkset(scr, ...)}, background="white")
    tkgrid(tklabel(aa, text="Liste des facteurs de groupement"))
    tkgrid(tl, scr)
    tkgrid.configure(scr, rowspan=4, sticky="nsw")
    facts <- sort(names(unitobs))
    a <- length(facts)
    for (i in (1:a))
    {
        tkinsert(tl, "end", facts[i])
    }
    tkselection.set(tl, 0)

    OnOK <- function()
    {
        fact33 <- facts[as.numeric(tkcurselection(tl))+1]
        assign("fact33", fact33, envir=.GlobalEnv)
        tkdestroy(aa)
        unit[, fact31] <- unitobs[, fact31][match(unit$unitobs, unitobs$unite_observation)]
        unit[, fact32] <- unitobs[, fact32][match(unit$unitobs, unitobs$unite_observation)]
        unit[, fact33] <- unitobs[, fact33][match(unit$unitobs, unitobs$unite_observation)]
        assign("unit", unit, envir=.GlobalEnv)
    }
    OK.but <-tkbutton(aa, text="OK", command=OnOK)
    tkgrid(OK.but)
    tkfocus(aa)
    tkwait.window(aa)
    rm(a)
} ##  fin choixtroisfacteurs.f

################################################################################
## Nom    : occurrence3.f()
## Objet  : choix d'une espèce parmi la liste des espèces présentes
##          calcul de la fréquence d'occurrence de l'espèce dans la table "grp13"
## Input  : tables "grp13" + facteur fact31, fact32 et fact33
## Output : fréquence d'occurrence de l'espèce sélectionnée selon les facteurs
##          de regroupement
################################################################################

occurrence3.f <- function(fact31, fact32, fact33)
{
    ## Demande d'affichage du pourcentage d'occurrence d'une espece
    nn <- tktoplevel()
    tkwm.title(nn, "Pourcentage d'occurrence")
    tkgrid(tklabel(nn, text="Voulez-vous calculer le pourcentage d'occurrence d'une espece?"))
    done <- tclVar(0)
    OK.but <- tkbutton(nn, text="OUI", command=function() {tclvalue(done) <- 1})
    Cancel.but <- tkbutton(nn, text="NON", command=function() {tclvalue(done) <- 2})
    tkgrid(OK.but, Cancel.but)
    tkbind(nn, "<Destroy>", function() {tclvalue(done) <- 2})
    tkfocus(nn)
    tkwait.variable(done)
    doneVal <- as.integer(tclvalue(done))
    tkdestroy(nn)

    if (doneVal==1)
    {
        tt <- tktoplevel()
        tkwm.title(tt, "Pourcentage d'occurrence par espece")
        scr <- tkscrollbar(tt, repeatinterval=5, command=function(...){tkyview(tl, ...)})
        tl <- tklistbox(tt, height=20, width=50, selectmode="single",
                        yscrollcommand=function(...){tkset(scr, ...)}, background="white")
        tkgrid(tklabel(tt, text="Occurrence de l'espece:"))
        tkgrid(tl, scr)
        tkgrid.configure(scr, rowspan=4, sticky="nsw")
        ## on ne propose uniquement les especes presentes dans le jeu de donnees
        ## ATTENTION PRENDRE EN COMPTE LES ESPECES EXCLUES
        especesPresentes <- subset(unitesp, unitesp$pres_abs==1)$code_espece
        esp <- sort(unique(especesPresentes))
        a <- length(esp)
        for (i in (1:a))
        {
            tkinsert(tl, "end", esp[i])
        }
        tkselection.set(tl, 0)

        OnOK <- function(){
            choixespece <- esp[as.numeric(tkcurselection(tl))+1]
            b <- especes$code_espece[especes$code_espece == choixespece]
            tkdestroy(tt)
            print(paste("Pourcentage d'occurrence de", choixespece))
            print(cbind(grp13[, fact31], grp13[, fact32], grp13[, fact33], grp13[, b]))
            assign("b", b, envir=.GlobalEnv)
        }
        OK.but <-tkbutton(tt, text="   OK   ", command=OnOK)
        tkgrid(OK.but)
        tkfocus(tt)
        tkwait.window(tt)
        rm(a)

        rm(fact31, fact32, fact33, envir=.GlobalEnv)
        rm(b, envir=.GlobalEnv)
    }
} ## fin occurrence3.f


################################################################################
## FONCTIONS DE CALCUL DE METRIQUES PAR GROUPE D'UNITE D'OBSERVATION
##                                  ET CLASSES DE TAILLE
##     - unfacteurCT.f(), deuxfacteursCT.f(), troisfacteursCT.f()
##     - choixunfacteurCT.f(), choixdeuxfacteursCT.f(), choixtroisfacteursCT.f()
##     - choixCT.f()
##     - grpCT1.f(), grpCT2.f(), grpCT3.f()
##     - graphCT1.f(), graphCT2.f(), graphCT3.f()
################################################################################

################################################################################
## Nom    : choixCT.f()
## Objet  : choix de la classe de taille du groupement des unités d'observations
## Input  : tables "unitespta"
## Output : tailleChoisie et "unitesptat"
################################################################################

## On doit pouvoir pas mal factoriser (e.g. une seule fonction "openTaille.f" avec un argument) +
## voir si possibilité de passer des arguments aux commandes [yreecht: 22/07/2010]
choixCT.f <- function(fact)
{

    print("fonction choixCT.f activée")

    ## sélection des gros
    opengros.f <- function ()
    {
        tkdestroy(nn)
        unitesptaG <- subset(unitespta, unitespta$classe_taille=="G")
        unitesptat <- unitesptaG
        TailleChoisie <- "gros"
        assign("TailleChoisie", TailleChoisie, envir=.GlobalEnv)
        assign("unitesptat", unitesptat, envir=.GlobalEnv)
        assign("unitesptaG", unitesptaG, envir=.GlobalEnv)
    }
    ## sélection des moyens
    openmoy.f <- function ()
    {
        tkdestroy(nn)
        unitesptaM <- subset(unitespta, unitespta$classe_taille=="M")
        unitesptat <- unitesptaM
        TailleChoisie <- "moyens"
        assign("TailleChoisie", TailleChoisie, envir=.GlobalEnv)
        assign("unitesptat", unitesptat, envir=.GlobalEnv)
        assign("unitesptaM", unitesptaM, envir=.GlobalEnv)
    }
    ## sélection des petits
    openpetit.f <- function ()
    {
        tkdestroy(nn)
        unitesptaP <- subset(unitespta, unitespta$classe_taille=="P")
        unitesptat <- unitesptaP
        TailleChoisie <- "petits"
        assign("TailleChoisie", TailleChoisie, envir=.GlobalEnv)
        assign("unitesptat", unitesptat, envir=.GlobalEnv)
        assign("unitesptaP", unitesptaP, envir=.GlobalEnv)
    }
    ## sélection de toutes les espèces
    openttes.f <- function ()
    {
        tkdestroy(nn)
        unitesptat <- unitespta
        assign("unitesptat", unitesptat, envir=.GlobalEnv)
    }

    nn <- tktoplevel()
    tkwm.title(nn, "Choix des categories de taille")
    tkgrid(tklabel(nn, text="Sur quelle categorie de taille voulez-vous regrouper les especes ?"))
    grosses.but <- tkbutton(nn, text="Grand", command=opengros.f)
    moyennes.but <- tkbutton(nn, text="Moyens", command=openmoy.f)
    petites.but <- tkbutton(nn, text="Petits", command=openpetit.f)
    toutes.but <- tkbutton(nn, text="Toutes", command=openttes.f)
    tkgrid(grosses.but)
    tkgrid(moyennes.but)
    tkgrid(petites.but)
    tkgrid(toutes.but)
    tkfocus(nn)
    tkwait.window(nn)
    tkdestroy(nn)
} ##  fin choixCT.f

################################################################################
## Nom    : choixUnFacteurCT.f()
## Objet  : choix du facteur de groupement des unités d'observations
## Input  : tables "unitespta" et "unitobs"
## Output : table "unitespta" + le facteur de la table unitobs choisi
################################################################################

choixunfacteurCT.f <- function ()
{

    print("fonction choixunfacteurCT.f activée")

    aa <- tktoplevel()
    tkwm.title(aa, "Selection du facteur de groupement des unites d'observation")
    scr <- tkscrollbar(aa, repeatinterval=5, command=function(...)tkyview(tl, ...))
    tl <- tklistbox(aa, height=20, selectmode="single",
                    yscrollcommand=function(...)tkset(scr, ...), background="white")
    tkgrid(tklabel(aa, text="Liste des facteurs de groupement"))
    tkgrid(tl, scr)
    tkgrid.configure(scr, rowspan=4, sticky="nsw")
    facts <- sort(names(unitobs))
    a <- length(facts)
    for (i in (1:a))
    {
        tkinsert(tl, "end", facts[i])
    }
    tkselection.set(tl, 0)

    OnOK <- function ()
    {
        fact <- facts[as.numeric(tkcurselection(tl))+1]
        assign("fact", fact, envir=.GlobalEnv)
        tkdestroy(aa)
        unitespta[, fact] <- unitobs[, fact][match(unitespta$unitobs, unitobs$unite_observation)]
        assign("unitespta", unitespta, envir=.GlobalEnv)
    }
    OK.but <-tkbutton(aa, text="OK", command=OnOK)
    tkgrid(OK.but)
    tkfocus(aa)
    tkwait.window(aa)
    rm(a)
} ##  fin choixunfacteurCT.f

################################################################################
## Nom    : choixdeuxFacteursCT.f()
## Objet  : choix des facteurs de groupement des unités d'observations
## Input  : tables "unitespta" et "unitobs"
## Output : table "unitespta" + les facteur fact21 et fact 22
##          de la table unitobs choisis
################################################################################

choixdeuxfacteursCT.f <- function ()
{

    print("fonction choixdeuxfacteursCT.f activée")
    ## selection du premier facteur
    aa <- tktoplevel()
    tkwm.title(aa, "Selection du PREMIER facteur de groupement des unites d'observation")
    scr <- tkscrollbar(aa, repeatinterval=5, command=function(...)tkyview(tl, ...))
    tl <- tklistbox(aa, height=20, selectmode="single",
                    yscrollcommand=function(...)tkset(scr, ...), background="white")
    tkgrid(tklabel(aa, text="Liste des facteurs de groupement"))
    tkgrid(tl, scr)
    tkgrid.configure(scr, rowspan=4, sticky="nsw")
    facts <- sort(names(unitobs))
    a <- length(facts)
    for (i in (1:a))
    {
        tkinsert(tl, "end", facts[i])
    }
    tkselection.set(tl, 0)

    OnOK <- function(){
        fact21 <- facts[as.numeric(tkcurselection(tl))+1]
        assign("fact21", fact21, envir=.GlobalEnv)
        tkdestroy(aa)
    }
    OK.but <-tkbutton(aa, text="OK", command=OnOK)
    tkgrid(OK.but)
    tkfocus(aa)
    tkwait.window(aa)
    rm(a)

    ## selection du deuxieme facteur ##
    aa <- tktoplevel()
    tkwm.title(aa, "Selection du SECOND facteur de groupement des unites d'observation")
    scr <- tkscrollbar(aa, repeatinterval=5, command=function(...)tkyview(tl, ...))
    tl <- tklistbox(aa, height=20, selectmode="single",
                    yscrollcommand=function(...)tkset(scr, ...), background="white")
    tkgrid(tklabel(aa, text="Liste des facteurs de groupement"))
    tkgrid(tl, scr)
    tkgrid.configure(scr, rowspan=4, sticky="nsw")
    facts <- sort(names(unitobs))
    a <- length(facts)
    for (i in (1:a))
    {
        tkinsert(tl, "end", facts[i])
    }
    tkselection.set(tl, 0)

    OnOK <- function ()
    {
        fact22 <- facts[as.numeric(tkcurselection(tl))+1]
        assign("fact22", fact22, envir=.GlobalEnv)
        tkdestroy(aa)
        unitespta[, fact21] <- unitobs[, fact21][match(unitespta$unitobs, unitobs$unite_observation)]
        unitespta[, fact22] <- unitobs[, fact22][match(unitespta$unitobs, unitobs$unite_observation)]
        assign("unitespta", unitespta, envir=.GlobalEnv)
    }
    OK.but <-tkbutton(aa, text="OK", command=OnOK)
    tkgrid(OK.but)
    tkfocus(aa)
    tkwait.window(aa)
    rm(a)
} ##  fin choixdeuxfacteursCT.f



################################################################################
## Nom    : choixtroisFacteurssCT.f()
## Objet  : choix des facteurs de groupement des unités d'observations
## Input  : tables "unitespta" et "unitobs"
## Output : table "unitespta" + les facteurs fact31, fact32 et fact33
##          de la table unitobs choisis
################################################################################

## ! pourquoi la table unitespa est elle en argument?
## ! place en table requête ou en choix (a créer)

choixtroisfacteursCT.f <- function ()
{

    print("fonction choixtroisfacteursCT.f activée")
    ## sélection du premier facteur  ##
    aa <- tktoplevel()
    tkwm.title(aa, "Selection du PREMIER facteur de groupement des unites d'observation")
    scr <- tkscrollbar(aa, repeatinterval=5, command=function(...)tkyview(tl, ...))
    tl <- tklistbox(aa, height=20, selectmode="single",
                    yscrollcommand=function(...)tkset(scr, ...), background="white")
    tkgrid(tklabel(aa, text="Liste des facteurs de groupement"))
    tkgrid(tl, scr)
    tkgrid.configure(scr, rowspan=4, sticky="nsw")
    facts <- sort(names(unitobs))
    a <- length(facts)
    for (i in (1:a))
    {
        tkinsert(tl, "end", facts[i])
    }
    tkselection.set(tl, 0)

    OnOK <- function ()
    {
        fact31 <- facts[as.numeric(tkcurselection(tl))+1]
        assign("fact31", fact31, envir=.GlobalEnv)
        tkdestroy(aa)
    }
    OK.but <-tkbutton(aa, text="OK", command=OnOK)
    tkgrid(OK.but)
    tkfocus(aa)
    tkwait.window(aa)
    rm(a)

    ## sélection du deuxieme facteur  ##
    aa <- tktoplevel()
    tkwm.title(aa, "Selection du SECOND facteur de groupement des unites d'observation")
    scr <- tkscrollbar(aa, repeatinterval=5, command=function(...)tkyview(tl, ...))
    tl <- tklistbox(aa, height=20, selectmode="single",
                    yscrollcommand=function(...)tkset(scr, ...), background="white")
    tkgrid(tklabel(aa, text="Liste des facteurs de groupement"))
    tkgrid(tl, scr)
    tkgrid.configure(scr, rowspan=4, sticky="nsw")
    facts <- sort(names(unitobs))
    a <- length(facts)
    for (i in (1:a))
    {
        tkinsert(tl, "end", facts[i])
    }
    tkselection.set(tl, 0)

    OnOK <- function(){
        fact32 <- facts[as.numeric(tkcurselection(tl))+1]
        assign("fact32", fact32, envir=.GlobalEnv)
        tkdestroy(aa)
    }
    OK.but <-tkbutton(aa, text="OK", command=OnOK)
    tkgrid(OK.but)
    tkfocus(aa)
    tkwait.window(aa)
    rm(a)

    ## selection du troisieme facteur ##
    aa <- tktoplevel()
    tkwm.title(aa, "Selection du TROISIEME facteur de groupement des unites d'observation")
    scr <- tkscrollbar(aa, repeatinterval=5, command=function(...)tkyview(tl, ...))
    tl <- tklistbox(aa, height=20, selectmode="single",
                    yscrollcommand=function(...)tkset(scr, ...), background="white")
    tkgrid(tklabel(aa, text="Liste des facteurs de groupement"))
    tkgrid(tl, scr)
    tkgrid.configure(scr, rowspan=4, sticky="nsw")
    facts <- sort(names(unitobs))
    a <- length(facts)
    for (i in (1:a))
    {
        tkinsert(tl, "end", facts[i])
    }
    tkselection.set(tl, 0)

    OnOK <- function(){
        fact33 <- facts[as.numeric(tkcurselection(tl))+1]
        assign("fact33", fact33, envir=.GlobalEnv)
        tkdestroy(aa)
        unitespta[, fact31] <- unitobs[, fact31][match(unitespta$unitobs, unitobs$unite_observation)]
        unitespta[, fact32] <- unitobs[, fact32][match(unitespta$unitobs, unitobs$unite_observation)]
        unitespta[, fact33] <- unitobs[, fact33][match(unitespta$unitobs, unitobs$unite_observation)]
        assign("unitesptat", unitesptat, envir=.GlobalEnv)
    }
    OK.but <-tkbutton(aa, text="OK", command=OnOK)
    tkgrid(OK.but)
    tkfocus(aa)
    tkwait.window(aa)
    rm(a)
} # fin choixtroisfacteursCT.f





################################################################################
## FONCTIONS DE CALCUL DE METRIQUES PAR GROUPE D'UNITE D'OBSERVATION
##                               ET PAR GROUPE D'ESPECES
##     - grpunitobsGrpEspece.f()
################################################################################

grpunitobsGrpEspece.f <- function(){

    print("fonction grpunitobsGrpEspece.f activée")
    aa <- tktoplevel()
    tkwm.title(aa, "Selection du facteur de groupement des especes")
    scr <- tkscrollbar(aa, repeatinterval=5, command=function(...)tkyview(tl, ...))
    tl <- tklistbox(aa, height=20, selectmode="single",
                    yscrollcommand=function(...)tkset(scr, ...), background="white")
    tkgrid(tklabel(aa, text="Liste des facteurs de groupement"))
    tkgrid(tl, scr)
    tkgrid.configure(scr, rowspan=4, sticky="nsw")
    facts <- sort(names(especes))
    a <- length(facts)
    for (i in (1:a))
    {
        tkinsert(tl, "end", facts[i])
    }
    tkselection.set(tl, 0)

    OnOK <- function(){
        factesp <- facts[as.numeric(tkcurselection(tl))+1]
        assign("factesp", factesp, envir=.GlobalEnv)
        tkdestroy(aa)
        tkmessageBox(message="Non programme")
    }
    OK.but <-tkbutton(aa, text="OK", command=OnOK)
    tkgrid(OK.but)
    tkfocus(aa)
    tkwait.window(aa)
    rm(a)
}  # fin grpunitobsGrpEspece.f()


################################################################################
## CREATION DES TABLES DE BASE
##      Calcul par unité d'observation / classe de taille / espèce : unitespta.f
##                  " " " par rotation : unitesptar.f
##      Calcul par unité d'observation par espèce : unitesp.f
##                  " " " par rotation : unitespr.f
##      Calcul par unité d'observation toutes espèces confondues : unit.f
##                  " " " par rotation : unitr.f
################################################################################


################################################################################
## Nom     : unitespta.f
## Objet   : calcul des métriques par unité d'observation / espèce et classe
##            de taille
## Input   : tables "obs" et "unitobs"
## Output  : table "unitespta"
################################################################################

unitespta.f <- function(){
    print("fonction unitespta.f activée")

    ## creation des classes de tailles si champ classe taille contient uniquement des NA [!!!] uniquement [???]
    if (any(is.na(obs$classe_taille)))  ## (NA %in% unique(obs$classe_taille)==TRUE) # [!!!]
    {
        classeTaille.f()
    }else{
        ## si le champ taille contient uniquement des valeurs a NA
        if (all(is.na(obs$taille))) # (length(unique(obs$taille))==1 & NA %in% unique(obs$taille)==TRUE)
                                   # # [!!!] remplacer par all(is.na()) ??
                                        # !!Non!! + inconsistence avec la première clause (il ne devrait pas y avoir de
                                        # NAs ici) !!Non!!
                                        # [yr: 13/08/2010]
        {
            ct <- 2
        }else{
            ct <- 1
        }
    }
    assign("ct", ct, envir=.GlobalEnv)  # à quoi ça sert au final [???]

    if (ct == 1 || !all(is.na(obs$classe_taille)))
    {
        ## ##################################################
        ## Calcul de la biomasse dans la table obs

        biomasse.f()

        ## #########################################################################################################
        ## Creation de la table par unite d'observation, par espece et par classe de taille et par rotation si SVR :

        ## Nombre d'individus :
        if (unique(unitobs$type) == "SVR")
        {
            statRotations <- statRotation.f(facteurs=c("unite_observation", "code_espece", "classe_taille"))

            ## Moyenne pour les vidéos rotatives (habituellement 3 rotation) :
            unitesptaT <- statRotations[["nombresMean"]]
        }else{
            ## Somme des nombres d'individus :
            unitesptaT <- tapply(obs$nombre,
                                 as.list(obs[ , c("unite_observation", "code_espece", "classe_taille")]),
                                 sum, na.rm = TRUE) # [!!!] nombres à zéro [???] [yr: 17/08/2010]

            ## Absences considérée comme "vrais zéros" :
            unitesptaT[is.na(unitesptaT)] <- 0
        }


        unitespta <- as.data.frame(as.table(unitesptaT), responseName="nombre")
        unitespta$unitobs <- unitespta$unite_observation # Pour compatibilité uniquement !!!

        unitespta$classe_taille[unitespta$classe_taille == ""] <- NA

        ## Si les nombres sont des entiers, leur redonner la bonne classe :
        if (isTRUE(all.equal(unitespta$nombre, as.integer(unitespta$nombre))))
        {
            unitespta$nombre <- as.integer(unitespta$nombre)
        }else{}

        ## Stats sur les nombres pour les (généralement 3) rotations :
        if (unique(unitobs$type) == "SVR")
        {
            unitespta$nombreMax <- as.vector(statRotations[["nombresMax"]])
            unitespta$nombreSD <- as.vector(statRotations[["nombresSD"]])
        }else{}

        ## ######################################################
        ## tailles moyennes ponderees si champ taille renseigne :
        if (!all(is.na(obs$taille)))        # (length(unique(obs$taille))>1)
        {
            unitespta$taille_moy <- as.vector(tapply(seq(length.out=nrow(obs)),
                                                     as.list(obs[ , c("unite_observation",
                                                                      "code_espece", "classe_taille")]),
                                                     function(ii)
                                                 {
                                                     weighted.mean(obs$taille[ii], obs$nombre[ii])
                                                 }))
        }

        ## ######################################################################
        ## sommes des biomasses par espece par unitobs et par classes de taille :
        if (!all(is.na(obs$biomasse)))      # (length(unique(obs$biomasse))>1)
        {
            ## ##################################################
            ## biomasse :
            unitespta$biomasse <- as.vector(tapply(obs$biomasse,
                                                   as.list(obs[ , c("unite_observation",
                                                                    "code_espece", "classe_taille")]),
                                                   function(x)
                                               {
                                                   if (all(is.na(x))) {return(NA)}else{return(sum(x, na.rm=TRUE))}
                                               }))

            ## C'est bête que la biomasse soit calculée comme ça... il faut faire des corrections
            ## pour les vidéos rotatives :
            if (unique(unitobs$type) == "SVR")
            {
                unitespta$biomasse <- unitespta$biomasse * unitespta$nombre /
                    as.vector(tapply(obs$nombre,
                                     as.list(obs[ , c("unite_observation",
                                                      "code_espece", "classe_taille")]),
                                     function(x)
                                 {
                                     if (all(is.na(x))) {return(NA)}else{return(sum(x, na.rm=TRUE))}
                                 }))

                ## Biomasse max
                unitespta$biomasseMax <- unitespta$biomasse * unitespta$nombreMax /
                    as.vector(tapply(obs$nombre,
                                     as.list(obs[ , c("unite_observation",
                                                      "code_espece", "classe_taille")]),
                                     function(x)
                                 {
                                     if (all(is.na(x))) {return(NA)}else{return(sum(x, na.rm=TRUE))}
                                 }))

            }else{}


            ## Certains NAs correspondent à des vrai zéros :

            ## Especes pour lesquelles aucune biomasse n'est calculée.
            espSansBiom <- tapply(unitespta$biomasse, unitespta$code_espece,
                                  function(x)all(is.na(x) | x == 0))
            espSansBiom <- names(espSansBiom)[espSansBiom]

            ## Ajout des vrais zéros :
            unitespta$biomasse[is.na(unitespta$biomasse) &
                               unitespta$nombre == 0 &
                               !is.element(unitespta$code_espece, espSansBiom)] <- 0


            if (unique(unitobs$type) == "SVR")
            {
                ## On divise par la surface du cercle contenant l'observation la plus lointaine :
                unitespta$biomasse <- unitespta$biomasse /
                    (pi * (as.vector(tapply(obs$dmin,
                                            as.list(obs[ , c("unite_observation", "code_espece")]),
                                            max, na.rm=TRUE)))^2) # Recyclé 3X.
            }else{
                ## on divise la biomasse par dimObs1*dimObs2
                unitespta$biomasse <- as.numeric(unitespta$biomasse) /
                    (unitobs$DimObs1[match(unitespta$unite_observation, unitobs$unite_observation)] *
                     unitobs$DimObs2[match(unitespta$unite_observation, unitobs$unite_observation)])
            }


        }else{
            ## alerte que les calculs de biomasse sont impossibles
            tkmessageBox(message=paste("Calcul de biomasse impossible - ",
                         "Les tailles ne sont pas renseignées dans les observations", sep=""))
        }

        ## poids

        ## ##################################################
        ## poids
        unitespta$poids <- as.vector(tapply(obs$poids,
                                            as.list(obs[ , c("unite_observation",
                                                             "code_espece", "classe_taille")]),
                                            function(x)
                                        {
                                            ifelse(all(is.na(x)),
                                                   as.numeric(NA),
                                                   sum(x, na.rm=TRUE))
                                        }))

        ## Certains NAs correspondent à des vrai zéros :
        if (!all(is.na(obs$poids)))
        {
            unitespta$poids[is.na(unitespta$poids) & unitespta$nombre == 0] <- 0
        }

        ## ##################################################
        ## poids moyen
        unitespta$poids_moyen <- apply(unitespta[ , c("nombre", "poids")], 1,
                                       function(x)
                                   {
                                       return(as.vector(ifelse(is.na(x[2]) || x[1] == 0,
                                                               as.numeric(NA),
                                                               x[2]/x[1])))
                                   })

        ## Presence - absence
        unitespta$pres_abs[unitespta$nombre > 0] <- as.integer(1) # pour avoir la richesse spécifique en 'integer'.1
        unitespta$pres_abs[unitespta$nombre == 0] <- as.integer(0) # pour avoir la richesse spécifique en 'integer'.0

        ## calcul densites (pour les pêches, ce calcul correspond au CPUE en nombre par espece)
        ## [densité]
        if (unique(unitobs$type) != "SVR")
        {
            unitespta$densite <- unitespta$nombre /
                (unitobs$DimObs1[match(unitespta$unite_observation, unitobs$unite_observation)] *
                 unitobs$DimObs2[match(unitespta$unite_observation, unitobs$unite_observation)])
        }else{
            ## Densité :
            unitespta$densite <- unitespta$nombre /
                (pi * (as.vector(tapply(obs$dmin,
                                        as.list(obs[ , c("unite_observation", "code_espece")]),
                                        max, na.rm=TRUE)))^2)

            ## Vrais zéros :
            unitespta$densite[unitespta$nombre == 0 & !is.na(unitespta$nombre)] <- 0

            ## Densité max :
            unitespta$densiteMax <- unitespta$nombreMax /
                (pi * (as.vector(tapply(obs$dmin,
                                        as.list(obs[ , c("unite_observation", "code_espece")]),
                                        max, na.rm=TRUE)))^2)

            ## Vrais zéros :
            unitespta$densiteMax[unitespta$nombreMax == 0 & !is.na(unitespta$nombreMax)] <- 0
        }

        ## ajout des champs "an", "site", "statut_protection", "biotope", "latitude", "longitude" :
        unitespta <- cbind(unitespta,
                           unitobs[match(unitespta$unite_observation, unitobs$unite_observation),
                                   c("an", "site", "statut_protection", "biotope", "latitude", "longitude")])

        ## ##################################################
        ## Proportion d'abondance par classe de taille :
        abondance <- with(unitespta, tapply(densite, list(unite_observation, code_espece, classe_taille),
                                            function(x){x})) # -> tableau à 3D.

        ## Sommes d'abondances pour chaque unitobs pour chaque espèce :
        sommesCT <- apply(abondance, c(1, 2), sum, na.rm=TRUE)

        ## Calcul des proportions d'abondance -> tableau 3D :
        propAbondance <- sweep(abondance, c(1, 2), sommesCT, FUN="/")
        names(dimnames(propAbondance)) <- c("unite_observation", "code_espece", "classe_taille")

        ## Mise au format colonne + % :
        unitespta$prop.abondance.CL <- 100 * as.data.frame(as.table(propAbondance),
                                                           responseName="prop.abondance.CL",
                                                           stringsAsFactors=FALSE)$prop.abondance.CL

        ## ##################################################
        ## Proportion de biomasse par classe de taille :
        if (!is.null(unitespta$biomasse))
        {
            biomasses <- with(unitespta, tapply(biomasse,
                                                list(unite_observation, code_espece, classe_taille),
                                                function(x){x})) # -> tableau à 3D.

            ## Sommes de biomasses pour chaque unitobs pour chaque espèce :
            sommesCT <- apply(biomasses, c(1, 2), sum, na.rm=TRUE)

            ## Calcul des proportions de biomasse -> tableau 3D :
            propBiomasse <- sweep(biomasses, c(1, 2), sommesCT, FUN="/")
            names(dimnames(propBiomasse)) <- c("unite_observation", "code_espece", "classe_taille")

            ## Mise au format colonne + % :
            unitespta$prop.biomasse.CL <- 100 * as.data.frame(as.table(propBiomasse),
                                                              responseName="prop.biomasse.CL",
                                                              stringsAsFactors=FALSE)$prop.biomasse.CL
        }else{}

        ## #################################################
        ## on renomme densite et biomasse en CPUE
        ## pour les jeux de données pêche
        if (is.peche.f())                   # length(typePeche)>1
        {
            unitespta$CPUE <- unitespta$densite
            unitespta$densite <- NULL
            unitespta$CPUEbiomasse <- unitespta$biomasse # Fonctionne même si biomasse n'existe pas.
            unitespta$biomasse <- NULL
        }

        assign("unitespta", unitespta, envir=.GlobalEnv)
        write.csv(unitespta[ , colnames(unitespta) != "unite_observation"],
                  file=paste(nameWorkspace, "/FichiersSortie/UnitobsEspeceClassetailleMetriques.csv", sep=""),
                  row.names = FALSE)
        print(paste("La table par unite d'observation / espece / classe de taille",
                    " a ete creee: UnitobsEspeceClassetailleMetriques.csv", sep=""))
    }else{
        message("Métriques par classe de taille incalculables")
        assign("unitespta",
               data.frame("unite_observation"=NULL, "code_espece"=NULL, "nombre"=NULL,
                          "poids"=NULL, "poids_moyen"=NULL, "densite"=NULL,
                          "pres_abs"=NULL, "site"=NULL, "biotope"=NULL,
                          "an"=NULL, "statut_protection"=NULL),
               envir=.GlobalEnv)
    }
} #fin unitespta.f()

################################################################################
## Nom     : unitesptar.f
## Objet   : calcul des métriques par unité d'observation / rotation / espèce /
##           classe de taille
## Input   : tables "obs" et "unitobs"
## Output  : table "unitesptar"
################################################################################

## calcul par rotation
unitesptar.f <- function ()
{
    print("fonction unitesptar.f activée")
    ## creation des classes de tailles si champ classe taille contient uniquement des NA
    if (NA %in% unique(obs$classe_taille)==TRUE)
    {
        classeTaille.f()
    }else{
        ## si le champ taille contient uniquement des valeurs a NA
        if (length(unique(obs$taille))==1 & NA %in% unique(obs$taille)==TRUE) # [!!!] remplacer par all(is.na()) ?  [yr: 13/08/2010]
        {
            ct <- 2
        }else{
            ct <- 1
        }
    }
    assign("ct", ct, envir=.GlobalEnv)

    ## Calcul de la biomasse dans la table obs
    biomasse.f()

    ## Creation de la table par unite d'observation, par espece et par classe de taille (et par rotation si SVR)

    ## Somme des individus
    unitesptaTR <- tapply(obs$nombre, list(obs$unite_observation, obs$rotation, obs$code_espece, obs$classe_taille),
                          sum, na.rm = TRUE) # Ordre fonction - arguments ! [!!!] [yr: 30/07/2010]
    unitesptaTR[is.na(unitesptaTR)] <- 0
    unitesptaR <- as.data.frame(matrix(as.numeric(NA),
                                       dim(unitesptaTR)[1] * dim(unitesptaTR)[2] *
                                       dim(unitesptaTR)[3] * dim(unitesptaTR)[4], 5))
    colnames(unitesptaR) = c("unitobs", "rotation", "code_espece", "classe_taille", "nombre")
    unitesptaR$nombre <- as.vector(unitesptaTR, "integer") # 'numeric' changé pour avoir des entiers.
    unitesptaR$unitobs <- rep(dimnames(unitesptaTR)[[1]],
                              times = dim(unitesptaTR)[2] * dim(unitesptaTR)[3] * dim(unitesptaTR)[4])
    unitesptaR$rotation <- rep(dimnames(unitesptaTR)[[2]], each = dim(unitesptaTR)[1], times = dim(unitesptaTR)[3])
    unitesptaR$code_espece <- rep(dimnames(unitesptaTR)[[3]], each = dim(unitesptaTR)[1]*dim(unitesptaTR)[2])
    unitesptaR$classe_taille <- rep(dimnames(unitesptaTR)[[4]],
                                    each = dim(unitesptaTR)[1]*dim(unitesptaTR)[2]*dim(unitesptaTR)[3])
    unitesptaR$classe_taille[unitespta$classe_taille == ""] <- NA

    ## sommes des biomasses par espece par unitobs et par classes de taille
    unitesptaTR.b <- tapply(obs$biomasse,
                            list(obs$unite_observation, obs$rotation, obs$code_espece, obs$classe_taille),
                            function(x){if (all(is.na(x))) {return(NA)}else{return(sum(x, na.rm=TRUE))}}) # Pour éviter
                                        # des sommes à zéro là où seulement des NAs. ##sum) # [bio]
    unitesptaR.b <- as.data.frame(matrix(as.numeric(NA),
                                         dim(unitesptaTR.b)[1] * dim(unitesptaTR.b)[2] *
                                         dim(unitesptaTR.b)[3] * dim(unitesptaTR)[4], 5))
    colnames(unitesptaR.b) = c("unitobs", "rotation", "code_espece", "classe_taille", "biomasse")
    unitesptaR.b$biomasse <- as.vector(unitesptaTR.b, "numeric")
    unitesptaR.b$unitobs <- rep(dimnames(unitesptaTR.b)[[1]],
                                times = dim(unitesptaTR.b)[2] * dim(unitesptaTR.b)[3] * dim(unitesptaTR.b)[4])
    unitesptaR.b$rotation <- rep(dimnames(unitesptaTR.b)[[2]],
                                 each = dim(unitesptaTR.b)[1], times = dim(unitesptaTR.b)[3])
    unitesptaR.b$code_espece <- rep(dimnames(unitesptaTR.b)[[3]], each = dim(unitesptaTR.b)[1]*dim(unitesptaTR.b)[2])
    unitesptaR.b$classe_taille <- rep(dimnames(unitesptaTR.b)[[4]],
                                      each = dim(unitesptaTR.b)[1]*dim(unitesptaTR.b)[2]*dim(unitesptaTR.b)[3])
    unitesptaR.b$an <- unitobs$an[match(unitesptaR.b$unitobs, unitobs$unite_observation)]
    unitesptaR$biomasse <- unitesptaR.b$biomasse

    ## Presence - absence
    unitesptaR$pres_abs[unitesptaR$nombre != 0] <- as.integer(1) # pour avoir la richesse spécifique en 'integer'.1
    unitesptaR$pres_abs[unitesptaR$nombre == 0] <- as.integer(0) # pour avoir la richesse spécifique en 'integer'.0

    ## calcul densites
    rayonsR.t <- tapply(obs$dmin, list(obs$unite_observation, obs$rotation), max)
    rayonsR <- as.data.frame(matrix(as.numeric(NA), dim(rayonsR.t)[1]*dim(rayonsR.t)[2], 3))
    colnames(rayonsR) = c("unitobs", "rotation", "rayonMax")
    rayonsR$rayonMax <- as.vector(rayonsR.t, "numeric")
    rayonsR$unitobs <- rep(dimnames(rayonsR.t)[[1]], dim(rayonsR.t)[2])
    rayonsR$rotation <- rep(dimnames(rayonsR.t)[[2]], each = dim(rayonsR.t)[1], 1)
    unitesptaR$densite <- unitesptaR$nombre / (pi * rayonsR$rayonMax[match(unitesptaR$unitobs, rayonsR$unitobs)])

    assign("unitesptaR", unitesptaR, envir=.GlobalEnv)
    write.csv(unitesptaR,
              file=paste(NomDossierTravail, "UnitobsEspeceClassetailleRotationMetriques.csv", sep=""),
              row.names = FALSE)
    print(paste("La table par unite d'observation / rotation / espece / classe de taille",
                " a ete creee: UnitobsEspeceClassetailleRotationMetriques.csv", sep=""))
} #fin unitesptar.f()

################################################################################
## Nom     : unitesp.f
## Objet   : calcul des métriques par unité d'observation / espèce
## Input   : tables "obs" et "unitobs"
## Output  : table "unitesp" et "listespunit"
################################################################################

unitesp.f <- function(){


    print("fonction unitesp.f activée")

    ## ##################################################
    ## somme des abondances


    ## Si video rotative, on divise par le nombre de rotation
    if (unique(unitobs$type) == "SVR")
    {
        statRotations <- statRotation.f(facteurs=c("unite_observation", "code_espece"))

        ## Moyenne pour les vidéos rotatives (habituellement 3 rotation) :
        unitespT <- statRotations[["nombresMean"]]
    }else{
        unitespT <- tapply(obs$nombre,
                           as.list(obs[ , c("unite_observation", "code_espece")]),
                           sum, na.rm = TRUE)

        unitespT[is.na(unitespT)] <- 0      # Les NAs correspondent à des vrais zéros.
    }


    unitesp <- as.data.frame(as.table(unitespT), responseName="nombre")

    if (isTRUE(all.equal(unitesp$nombre, as.integer(unitesp$nombre))))
    {
        unitesp$nombre <- as.integer(unitesp$nombre)
    }else{}

    ## Si video rotative, stat sur les rotations :
    if (unique(unitobs$type) == "SVR")
    {
        unitesp$nombreMax <- as.vector(statRotations[["nombresMax"]])
        unitesp$nombreSD <- as.vector(statRotations[["nombresSD"]])
    }else{}

    ## Euh... ce serait pas hyper dangeureux ce qui suit [!!!] [???] [yr: 01/10/2010]
    ## unitespT[is.na(unitespT)] <- as.integer(0) # Pour conserver des entiers

    if (!is.benthos.f())                               # unique(unitobs$type) != "LIT"
    {

        ## ##################################################
        ## tailles moyennes ponderees
        if (!all(is.na(obs$taille)))
        {
            unitesp$taille_moy <- as.vector(tapply(seq(length.out=nrow(obs)),
                                                   list(obs$unite_observation, obs$code_espece),
                                                   function(ii)
                                               {
                                                   weighted.mean(obs$taille[ii], obs$nombre[ii])
                                               }))
        }else{}

        ## ##################################################
        ## biomasses
        biomasse.f()

        if (!all(is.na(obs$biomasse)))
        {
            unitesp$biomasse <- as.vector(tapply(obs$biomasse,
                                                 list(obs$unite_observation, obs$code_espece),
                                                 function(x)
                                             {
                                                 if (all(is.na(x))) {return(NA)}else{return(sum(x, na.rm=TRUE))}
                                             }))##  /
            ## (unitobs$DimObs1[match(unitesp$unite_observation, unitobs$unite_observation)] *
            ##  unitobs$DimObs2[match(unitesp$unite_observation, unitobs$unite_observation)])



            ## C'est bête que la biomasse soit calculée comme ça... il faut faire des corrections pour les vidéos rotatives :
            if (unique(unitobs$type) == "SVR")
            {
                unitesp$biomasse <- unitesp$biomasse * unitesp$nombre /
                    as.vector(tapply(obs$nombre,
                                     as.list(obs[ , c("unite_observation",
                                                      "code_espece")]),
                                     function(x)
                                 {
                                     if (all(is.na(x))) {return(NA)}else{return(sum(x, na.rm=TRUE))}
                                 }))

                ## Biomasse max
                unitesp$biomasseMax <- unitesp$biomasse * unitesp$nombreMax /
                    as.vector(tapply(obs$nombre,
                                     as.list(obs[ , c("unite_observation",
                                                      "code_espece")]),
                                     function(x)
                                 {
                                     if (all(is.na(x))) {return(NA)}else{return(sum(x, na.rm=TRUE))}
                                 }))

            }else{}


            ## Certains NAs correspondent à des vrai zéros :

            ## Especes pour lesquelles aucune biomasse n'est calculée.
            espSansBiom <- tapply(unitesp$biomasse, unitesp$code_espece,
                                  function(x)all(is.na(x) | x == 0))
            espSansBiom <- names(espSansBiom)[espSansBiom]

            ## Ajout des vrais zéros :
            unitesp$biomasse[is.na(unitesp$biomasse) &
                             unitesp$nombre == 0 &
                             !is.element(unitesp$code_espece, espSansBiom)] <- 0

            if (unique(unitobs$type) == "SVR")
            {
                ## On divise par la surface du cercle contenant l'observation la plus lointaine :
                unitesp$biomasse <- unitesp$biomasse /
                    (pi * (as.vector(tapply(obs$dmin,
                                            as.list(obs[ , c("unite_observation", "code_espece")]),
                                            max, na.rm=TRUE)))^2)
            }else{
                ## on divise la biomasse par dimObs1*dimObs2
                unitesp$biomasse <- as.numeric(unitesp$biomasse) /
                    (unitobs$DimObs1[match(unitesp$unite_observation, unitobs$unite_observation)] *
                     unitobs$DimObs2[match(unitesp$unite_observation, unitobs$unite_observation)])
            }
        }else{}

        ## ##################################################
        ## poids
        unitesp$poids <- as.vector(tapply(obs$poids,
                                          list(obs$unite_observation, obs$code_espece),
                                          function(x) {if (all(is.na(x))) {return(NA)}else{return(sum(x, na.rm=TRUE))}}))

        ## Certains NAs correspondent à des vrai zéros :
        if (!all(is.na(obs$poids)))
        {
            unitesp$poids[is.na(unitesp$poids) & unitesp$nombre == 0] <- 0
        }

        ## ##################################################
        ## poids moyen
        unitesp$poids_moyen <- apply(unitesp[ , c("nombre", "poids")], 1,
                                     function(x)
                                 {
                                     return(as.vector(ifelse(is.na(x[2]) || x[1] == 0,
                                                             as.numeric(NA),
                                                             x[2]/x[1])))
                                 })

        ## ##################################################
        ## calcul densites (pour les pêches, ce calcul correspond aux captures par unite d'effort)
        if (unique(unitobs$type) != "SVR")
        {
            unitesp$densite <- unitesp$nombre /
                (unitobs$DimObs1[match(unitesp$unite_observation, unitobs$unite_observation)] *
                 unitobs$DimObs2[match(unitesp$unite_observation, unitobs$unite_observation)])
        }else{
            unitesp$densite <- unitesp$nombre /
                (pi * (as.vector(tapply(obs$dmin,
                                        list(obs$unite_observation, obs$code_espece),
                                        max, na.rm=TRUE)))^2)

            ## Densité max :
            unitesp$densiteMax <- unitesp$nombreMax /
                (pi * (as.vector(tapply(obs$dmin,
                                        as.list(obs[ , c("unite_observation", "code_espece")]),
                                        max, na.rm=TRUE)))^2)

            ## Vrais zéros :
            unitesp$densiteMax[unitesp$nombreMax == 0 & !is.na(unitesp$nombreMax)] <- 0

        }

        ## Ajout des vrais zéros :
        unitesp$densite[unitesp$nombre == 0 & !is.na(unitesp$nombre)] <- 0


    }else{ # cas LIT

        ## Pourcentage de recouvrement de chaque espece/categorie pour les couvertures biotiques et abiotiques
        s <- tapply(unitesp$nombre, unitesp$unite_observation, sum, na.rm=TRUE)
        unitesp$recouvrement <- as.vector(100 * unitesp$nombre /
                                          s[match(unitesp$unite_observation, rownames(s))]) ## [!!!] ajout 100 * [???]
        rm(s)

        ## Nombre de colonies
        ## Vérifier si pas de risque que des longueurs de transitions == 0 => besoin de mettre 0 à count dans ces cas là
        ## [!!!]
        obs$count <- 1                  # [!!!] somme des obs$nombre > 0 [???]
        e <- tapply(obs$count, list(obs$unite_observation, obs$code_espece), sum, na.rm=TRUE)
        ## Vérif faite :
        ## sapply(1:2, function(x)all(expand.grid(row.names(e), colnames(e))[!is.na(e), ][, x] ==
        ##                            listespunit[ , c("unite_observation", "code_espece")][, x]))
        unitesp$colonie <- as.vector(e)
        unitesp$colonie[is.na(unitesp$colonie)] <- 0 # [???]
        unitesp$taille.moy.colonies <- apply(unitesp[ , c("nombre", "colonie")], 1,
                                             function(x){ifelse(x[2] == 0, NA, x[1] / x[2])})

        rm(e)
    }

    ## Creation de l'info Presence/Absence :
    unitesp$pres_abs[unitesp$nombre > 0] <- as.integer(1) # pour avoir la richesse spécifique en 'integer'.1
    unitesp$pres_abs[unitesp$nombre == 0] <- as.integer(0) # pour avoir la richesse spécifique en 'integer'.0

    unitesp <- cbind(unitesp,
                     unitobs[match(unitesp$unite_observation, unitobs$unite_observation),
                             c("site", "biotope", "an", "statut_protection")])

    ## on renomme densite en CPUE pour les jeux de données pêche
    if (is.peche.f())                   # length(typePeche)>1
    {
        unitesp$CPUE <- unitesp$densite
        unitesp$densite <- NULL
        unitesp$CPUEbiomasse <- unitesp$biomasse
        unitesp$biomasse <- NULL
    }

    ## Ecriture du fichier des unités d'observations par espèce en sortie
    assign("unitesp", unitesp, envir=.GlobalEnv)
    print("La table par unite d'observation / espece a ete creee : UnitobsEspeceMetriques.csv")
    write.csv(unitesp, file=paste(NomDossierTravail, "UnitobsEspeceMetriques.csv", sep=""), row.names = FALSE)

    ## table avec la liste des especes presentes dans chaque transect
    listespunit <- unitesp## [unitesp$pres_abs != 0, ]
    listespunit <- listespunit[order(listespunit$code_espece), ]
    assign("listespunit", listespunit, envir=.GlobalEnv)
    print("La liste des especes presentes dans chaque transect a ete creee : ListeEspecesUnitobs.csv")

    write.csv(listespunit, file=paste(NomDossierTravail, "ListeEspecesUnitobs.csv", sep=""), row.names = FALSE)
} # fin unitesp.f()


################################################################################
## Nom     : unitespr.f
## Objet   : calcul des métriques par unité d'observation / rotation /espèce
## Input   : tables "obs" et "unitobs"
## Output  : table "unitespr" et "listesprunit"
################################################################################

unitespr.f <- function(){

    print("fonction unitespr.f activée")
    ## biomasses
    biomasse.f()

    ## somme des abondances
    unitespTR <- tapply(obs$nombre, list(obs$unite_observation, obs$rotation, obs$code_espece), sum, na.rm = TRUE)
    unitespTR[is.na(unitespTR)] <- 0

    ## reaffectation dans un data.frame
    unitespr <- as.data.frame(matrix(NA, dim(unitespTR)[1]*dim(unitespTR)[2]*dim(unitespTR)[3], 4))
    colnames(unitespr) = c("unitobs", "rotation", "code_espece", "nombre")
    unitespr$nombre <- as.vector(unitespTR, "integer") # 'numeric' changé pour avoir des entiers
    unitespr$unitobs <- rep(dimnames(unitespTR)[[1]], times = dim(unitespTR)[2] * dim(unitespTR)[3])
    unitespr$rotation <- rep(dimnames(unitespTR)[[2]], each = dim(unitespTR)[1], times = dim(unitespTR)[3])
    unitespr$code_espece <- rep(dimnames(unitespTR)[[3]], each = dim(unitespTR)[1]*dim(unitespTR)[2])

    ## biomasse

    ## somme des biomasses par espece par unitobs
    unitespTR.b <- tapply(obs$biomasse, list(obs$unite_observation, obs$rotation, obs$code_espece),
                          function(x){if (all(is.na(x))) {return(NA)}else{return(sum(x, na.rm=TRUE))}}) # Pour éviter
                                        # des sommes à zéro là où seulement des NAs. ## na.rm = TRUE, sum)
    unitespR.b <- as.data.frame(matrix(NA, dim(unitespTR.b)[1]*dim(unitespTR.b)[2]*dim(unitespTR.b)[3], 4))
    colnames(unitespR.b) = c("unitobs", "rotation", "code_espece", "biomasse")
    unitespR.b$biomasse <- as.vector(unitespTR.b)
    unitespR.b$unitobs <- rep(dimnames(unitespTR.b)[[1]], times = dim(unitespTR.b)[2] * dim(unitespTR.b)[3])
    unitespR.b$rotation <- rep(dimnames(unitespTR.b)[[2]], each = dim(unitespTR.b)[1], times = dim(unitespTR.b)[3])
    unitespR.b$code_espece <- rep(dimnames(unitespTR.b)[[3]], each = dim(unitespTR.b)[1]*dim(unitespTR.b)[2])
    unitespr$biomasse <- unitespR.b$biomasse

    rayons.t <- tapply(obs$dmin, list(obs$unite_observation), max)
    rayons <- as.data.frame(matrix(as.numeric(NA), dim(rayons.t)[1], 2))
    colnames(rayons) = c("unitobs", "rayonMax")
    rayons$rayonMax <- as.vector(rayons.t, "numeric")
    rayons$unitobs <- rep(dimnames(rayons.t)[[1]])
    unitespr$densite <- unitespr$nombre / (pi * rayons$rayonMax[match(unitespr$unitobs, rayons$unitobs)]^2)

    ## Creation de l'info Presence/Absence
    unitespr$pres_abs[unitespr$nombre != 0] <- as.integer(1) # pour avoir la richesse spécifique en 'integer'.1
    unitespr$pres_abs[unitespr$nombre == 0] <- as.integer(0) # pour avoir la richesse spécifique en 'integer'.0

    ## Ecriture du fichier en sortie
    assign("unitespr", unitespr, envir=.GlobalEnv)
    print("La table par unite d'observation / rotation / espece a ete creee : UnitobsEspeceRotationMetriques.csv")
    write.csv(unitespr,
              file=paste(nameWorkspace, "/FichiersSortie/UnitobsEspeceRotationMetriques.csv", sep=""),
              row.names = FALSE)

    ## table avec la liste des especes presentes dans chaque transect
    listesprunit <- unitespr[unitespr$pres_abs != 0, ]
    listesprunit <- listesprunit[order(listesprunit$code_espece), ]
    assign("listesprunit", listesprunit, envir=.GlobalEnv)
    print("La liste des especes presentes dans chaque transect par rotation a ete creee : ListeEspecesRotationUnitobs.csv")
    write.csv(listesprunit,
              file=paste(nameWorkspace, "/FichiersSortie/ListeEspecesRotationUnitobs.csv", sep=""),
              row.names = FALSE)
} # fin unitespr.f()

################################################################################
## Nom     : unit.f
## Objet   : calcul des métriques par unité d'observation toutes espèces confondues
## Input   : tables "obs" et "unitobs"
## Output  : table "unit" et carte de la CPUE pour les données de pêche NC
################################################################################

unit.f <- function(){

    print("fonction unit.f activée")

    ## somme des abondances
    ## uniti <- tapply(obs$nombre, obs$unite_observation, sum, na.rm = TRUE)
    ## unit <- as.data.frame(matrix(NA, dim(uniti)[1], 2))
    ## unit$nombre <- as.integer(uniti)    # 'as.numeric' changé pour avoir des entiers.
    ## unit$unitobs <- rownames(uniti)
    ## rm(uniti)

    unit <- as.data.frame(as.table(tapply(obs$nombre, obs$unite_observation, sum, na.rm = TRUE))
                          , responseName="nombre")
    colnames(unit)[1] = c("unitobs")

    unit$nombre[is.na(unit$nombre)] <- 0      # Les NAs correspondent à des vrais zéros.

    if (!is.benthos.f())                # unique(unitobs$type) != "LIT"
    {
        ## somme des biomasses
        biomasse.f()

        ## biomasse par unite d'observation
        unit.b <- tapply(obs$biomasse, obs$unite_observation,
                         function(x){if (all(is.na(x))) {return(NA)}else{return(sum(x, na.rm=TRUE))}}) # Pour éviter
                                        # des sommes à zéro là où seulement des NAs. ## sum)
        unit$biomasse <- unit.b[match(unit$unitobs, rownames(unit.b))]



        if (unique(unitobs$type) != "SVR")
        {
            ## calcul biomasse
            ## unit$nombre[is.na(unit$nombre)] <- 0 # as.integer() pour conserver des entiers [???]
            unit$biomasse <- as.numeric(unit$biomasse) /
                (unitobs$DimObs1[match(unit$unitobs, unitobs$unite_observation)] *
                 unitobs$DimObs2[match(unit$unitobs, unitobs$unite_observation)])
            ## unit$biomasse[is.na(unit$biomasse)] <- 0 # [!!!] encore une fois, quelle horreur  [yr: 13/08/2010]

            ## calcul densite
            unit.d <- tapply(obs$nombre, obs$unite_observation, sum, na.rm=TRUE)
            unit$densite <- unit.d[match(unit$unitobs, rownames(unit.d))]
            unit$densite <- as.numeric(unit$densite) /
                (unitobs$DimObs1[match(unit$unitobs, unitobs$unite_observation)] *
                 unitobs$DimObs2[match(unit$unitobs, unitobs$unite_observation)])
            unit$densite[is.na(unit$densite)] <- 0 # [!!!] vérifier si c'est correct [yr: 17/08/2010]
        }else{
            ## calcul densite d'abondance
            unit$nombre <- unit$nombre / 3
            unit$nombre[is.na(unit$nombre)] <- 0 # as.integer() pour conserver des entiers ?
            unit$densite <- unit$nombre / (pi * 25)          # [!!!][???] Pourquoi la distance d'observation est fixe
                                                             # [!!!][???] dans ce cas-ci (5m) alors qu'elle est
            unit$biomasse <- unit$biomasse / (pi * 25)       # [!!!][???] "dynamique" dans unitesp.f() ?
        }

        ## Certains NAs correspondent à des vrai zéros :
        if (!all(is.na(obs$biomasse)))  # Si les biomasses ne sont pas calculables, inutile de mettre les zéros !
        {
            ## Ajout des vrais zéros :
            unit$biomasse[is.na(unit$biomasse) & unit$nombre == 0] <- 0
        }

        ## Ajout des vrais zéros de densité :
        unitesp$densite[unitesp$nombre == 0 & !is.na(unitesp$nombre)] <- 0

        ## ##################################################
        ## calcul richesse specifique
        unit$richesse_specifique <- as.integer(tapply(unitesp$pres_abs,
                                                      unitesp$unite_observation, sum, na.rm=TRUE)) # changé pour avoir
                                                        # des entiers.
        unit$richesse_specifique[is.na(unit$richesse_specifique)] <- as.integer(0) # pour conserver des entiers  # [!!!] vérifier si c'est correct
                                        # [yr: 17/08/2010]

        ## calcul de l'indice de Simpson
        ## le calcul se fait sur les $nombre il n'y a donc aucune espece exclue pour le calcul de ces metriques
        unitespT <- tapply(obs$nombre, list(obs$unite_observation, obs$code_espece), sum, na.rm = TRUE)
        unitespT[is.na(unitespT)] <- 0 # as.integer() pour conserver des entiers ?
        ot <- apply(unitespT, 1, sum, na.rm = TRUE)
        a <- sweep(unitespT, 1, ot, FUN="/")
        sim <- a^2
        sim[is.nan(sim)] <- 0
        sim <- apply(sim, 1, sum, na.rm = TRUE)
        sim <- as.data.frame(sim)
        sim$sim <- as.numeric(sim$sim)
        unit$simpson <- NA                                          # inutile
        unit$simpson <- sim$sim[match(unit$unitobs, rownames(sim))] # inutile
        unit$simpson[is.na(unit$simpson)] <- 0
        unit$l.simpson <- 1 - unit$simpson
        rm(sim)

        ## calcul de l'indice de Shannon
        sha <- a*log(a) # en base e
        sha[is.nan(sha)] <- 0
        sha <- apply(sha, 1, na.rm = TRUE, sum)
        sha <- as.data.frame(sha)
        sha$sha <- as.numeric(sha$sha)
        unit$shannon <- NA
        unit$shannon <- -sha$sha[match(unit$unitobs, rownames(sha))]
        unit$shannon[is.na(unit$shannon)] <- 0
        rm(a, sha)

        ## calcul de l'indice de Pielou
        unit$pielou <- unit$shannon / log(unit$richesse_specifique)
        unit$pielou[is.na(unit$pielou)] <- 0

        ## calcul de l'indice de Hill
        unit$hill <- (1-unit$simpson) / exp(unit$shannon)
        unit$hill[is.na(unit$hill)] <- 0

        ## suppression de l'indice de shannon (non pertinent)
        unit$shannon <- NULL

        ## browser()

        ## richesse specifique relative  ## Remplacer par un "switch" ou même une construction plus
                                        # générique (e.g. construction et évaluation d'une expression dépendant du site
                                        # étudié) [yreecht: 22/07/2010] OK [yr: 08/10/2010]

        ## Phylum(s) présent(s) dans le jeux de données :
        phylums <- as.character(unique(na.omit(especes$Phylum[match(obs$code_espece, especes$code_espece)])))

        ## RS relative par rapp. au nombre d'espèces du site :
        unit$RS.relative.site <- (unit$richesse_specifique /
                                  nrow(subset(especes,
                                              eval(parse(text=paste("Obs", siteEtudie, sep=""))) == "oui"))) * 100

        ## RS relative par rapp. au nombre d'espèces du site et du(des) phylum(s) concerné(s) (jeu de données) :
        unit$RS.relative.site.phylum <- (unit$richesse_specifique /
                                         nrow(subset(especes,
                                                     eval(parse(text=paste("Obs", siteEtudie, sep=""))) == "oui" &
                                                     is.element(Phylum, phylums)))) * 100

        ## RS relative par rapp. au nombre d'espèces des données :
        unit$RS.relative.donnees <- (unit$richesse_specifique /
                                     nrow(subset(especes,
                                                 is.element(code_espece, obs$code_espece)))) * 100

        ## ## RS relative par rapp. au nombre d'espèces des données :
        ## Inutile : "RS.relative.donnees" est par définition limitée au phylums présents

        ## RS relative par rapp. au nombre d'espèces au niveau régional (OM ou méditerrannée) :
        unit$RS.relative.region <- (unit$richesse_specifique /
                                            nrow(especes)) * 100

        ## RS relative par rapp. au nombre d'espèces au niveau régional (OM ou méditerrannée) et
        ## du(des) phylum(s) concerné(s) (jeu de données) :
        unit$RS.relative.region.phylum <- (unit$richesse_specifique /
                                            nrow(subset(especes, is.element(Phylum, phylums)))) * 100

    }

    ## ajout des champs "an", "site", "statut_protection", "biotope", "latitude", "longitude"
    unit <- cbind(unit,
                  unitobs[match(unit$unitobs, unitobs$unite_observation),
                          c("an", "site", "statut_protection", "biotope", "latitude", "longitude")])

    assign("unit", unit, envir=.GlobalEnv)

    ## calculs des indices de diversite taxonomique
    indicesDiv.f()

    ## le jeu de donnees doit comporter au moins 2 genres et 2 unite d'observations sinon taxa2dist ne fonctionne pas
    if (length(unique(sp.taxon$genre))>2)
    {
        ## formation d'un nouveau tableau avec les valeurs des differents indices
        ## pour les unites d'observation ayant les indices calcules
        unit$Delta <- ind_div$Delta[match(unit$unitobs, rownames(ind_div))]
        unit$DeltaEtoile <- ind_div$DeltaEtoile[match(unit$unitobs, rownames(ind_div))]
        unit$LambdaPlus <- ind_div$LambdaPlus[match(unit$unitobs, rownames(ind_div))]
        unit$DeltaPlus <- ind_div$DeltaPlus[match(unit$unitobs, rownames(ind_div))]
        unit$SDeltaPlus <- ind_div$SDeltaPlus[match(unit$unitobs, rownames(ind_div))]
        div_expect <- c(div[[8]], div[[9]], div[[10]])

        ## affichage des valeurs attendues
        print(paste("La valeur theorique de Delta est :" , div_expect[1]))
        print(paste("La valeur theorique de Delta* est :" , div_expect[2]))
        print(paste("La valeur theorique de Delta+ est :" , div_expect[3]))
    }

    ## on renomme densite en CPUE pour les jeux de données pêche
    if (is.peche.f())                   # length(typePeche)>1
    {
        unit$CPUE <- unit$densite
        unit$densite <- NULL
        unit$CPUEbiomasse <- unit$biomasse
        unit$biomasse <- NULL
    }

    ## message de creation de la table unit
    print("La table metriques par unite d'observation a ete creee : UnitobsMetriques.csv")
    write.csv(unit, file=paste(NomDossierTravail, "UnitobsMetriques.csv", sep=""), row.names = FALSE)
    ## carte de la CPUE pour les données de pêche NC
    if (is.peche.f() & (siteEtudie == "NC")) # (length(typePeche)>1)
    {
        x11(width=50, height=30, pointsize=10)
        MapNC <- read.shape("./shapefiles/NewCaledonia_v7.shp", dbf.data = TRUE, verbose=TRUE, repair=FALSE)
        plot(MapNC, xlim=c(166, 167), ylim=c(-23, -22), fg="lightyellow", xaxs="i", yaxs="i", axes=TRUE)
        unit$latitude <- as.vector(unit$latitude , "numeric")
        unit$longitude <- as.vector(unit$longitude , "numeric")
        unitSymbols <- subset(unit, longitude>0)
        symbols(unitSymbols$longitude, unitSymbols$latitude, unitSymbols$densite, add=TRUE, # Nommer les arguments [yr: 30/07/2010]
                fg=colours()[seq(10, (nrow(unitSymbols)*10), by=10)], lwd=3) #
        title(main=paste("CPUE", typePeche))
    }
    if (is.benthos.f())                 # unique(unitobs$type) == "LIT"
    {
        unit$richesse_specifique <- as.integer(tapply(unitesp$pres_abs, unitesp$unite_observation,
                                                      sum, na.rm=TRUE)) # changé pour avoir des entiers.
        unit$richesse_specifique[is.na(unit$richesse_specifique)] <- as.integer(0) # pour conserver des entiers.
    }
    assign("unit", unit, envir=.GlobalEnv)

} # fin unit.f()

################################################################################
## Nom     : unitr.f
## Objet   : calcul des métriques par unité d'observation et rotation
##           toutes espèces confondues
## Input   : tables "obs" et "unitobs"
## Output  : table "unitr"
################################################################################

unitr.f <- function(){

    print("fonction unitr.f activée")
    ## somme des abondances
    unitir <- tapply(obs$nombre, list(obs$unite_observation, obs$rotation), sum, na.rm = TRUE)
    unitir[is.na(unitir)] <- 0
    unitr <- as.data.frame(matrix(NA, dim(unitir)[1]*dim(unitir)[2], 3))
    colnames(unitr) = c("unitobs", "rotation", "nombre")
    unitr$nombre <- as.vector(unitir, "integer") # 'numeric' changé pour avoir des entiers.
    unitr$unitobs <- rep(dimnames(unitir)[[1]], dim(unitir)[2])
    unitr$rotation <- rep(dimnames(unitir)[[2]], each = dim(unitir)[1], 1)

    ## somme des biomasses
    biomasse.f()

    ## biomasse par unite d'observation
    unitrT.b <- tapply(obs$biomasse, list(obs$unite_observation, obs$rotation),
                       function(x){if (all(is.na(x))) {return(NA)}else{return(sum(x, na.rm=TRUE))}}) # Pour éviter
                                        # des sommes à zéro là où seulement des NAs. ## sum)

    unitr.b <- as.data.frame(matrix(NA, dim(unitrT.b)[1]*dim(unitrT.b)[2], 3))
    colnames(unitr.b) = c("unitobs", "rotation", "biomasse")
    unitr.b$biomasse <- as.vector(unitrT.b, "numeric")
    unitr.b$unitobs <- rep(dimnames(unitrT.b)[[1]], dim(unitrT.b)[2])
    unitr.b$rotation <- rep(dimnames(unitrT.b)[[2]], each = dim(unitrT.b)[1], 1)
    unitr$biomasse <- unitr.b$biomasse
    unitr$rotation <- unitr.b$rotation

    ## ajout des champs "an" et "site"
    unitr$an <- unitobs$an[match(unitr$unitobs, unitobs$unite_observation)]
    unitr$site <- unitobs$site[match(unitr$unitobs, unitobs$unite_observation)]

    ## calcul richesse specifique
    unitrT.RS <- tapply(unitespr$pres_abs, list(unitespr$unitobs, unitespr$rotation), na.rm = TRUE, sum)
    unitr.RS <- as.data.frame(matrix(NA, dim(unitrT.RS)[1]*dim(unitrT.RS)[2], 3))
    colnames(unitr.RS) = c("unitobs", "rotation", "RS")
    unitr.RS$RS <- as.vector(unitrT.RS, "integer") # 'numeric' changé pour avoir des entiers.
    unitr.RS$unitobs <- rep(dimnames(unitrT.RS)[[1]], dim(unitrT.RS)[2])
    unitr.RS$rotation <- rep(dimnames(unitrT.RS)[[2]], each = dim(unitrT.RS)[1], 1)
    unitr$richesse_specifique <- unitr.RS$RS

    ## calcul des indices de Simpson
    ## le calcul se fait sur les $nombre il n'y a donc aucune espece exclue pour le calcul de ces metriques
    unitrT <- tapply(obs$nombre, list(obs$unite_observation, obs$rotation, obs$code_espece), sum, na.rm = TRUE)
    unitrT[is.na(unitrT)] <- as.integer(0) # pour conserver des entiers

    ot <- apply(unitrT, 1, na.rm = TRUE, sum)
    a <- sweep(unitrT, 1, ot, FUN="/")
    sim <- a^2
    sim[is.nan(sim)] <- 0
    sim <- apply(sim, 1, na.rm = TRUE, sum)
    sim <- as.data.frame(sim)
    sim$sim <- as.numeric(sim$sim)
    unitr$simpson <- NA
    unitr$simpson <- sim$sim[match(unitr$unitobs, rownames(sim))]
    unitr$l.simpson <- 1 - unitr$simpson
    rm(sim)

    ## calcul de l'indice de Shannon
    sha <- a*log(a) # en base e
    sha[is.nan(sha)] <- 0
    sha <- apply(sha, 1, na.rm = TRUE, sum)
    sha <- as.data.frame(sha)
    sha$sha <- as.numeric(sha$sha)
    unitr$shannon <- NA
    unitr$shannon <- -sha$sha[match(unitr$unitobs, rownames(sha))]
    rm(a, sha)

    ## calcul de l'indice de Pielou
    unitr$pielou <- unitr$shannon / log(unitr$richesse_specifique)

    ## calcul de l'indice de Hill
    unitr$hill <- (1-unitr$simpson) / exp(unitr$shannon)

    ## suppression indice de shannon
    unitr$shannon <- NULL

    ## deplacement de unit dans l'environnement global
    assign("unitr", unitr, envir=.GlobalEnv)

    ## message de creation de la table
    print("La table metriques par unite d'observation a ete creee : UnitobsRotationMetriques.csv")
    write.csv(unitr,
              file=paste(nameWorkspace, "/FichiersSortie/UnitobsRotationMetriques.csv", sep=""),
              row.names = FALSE)
} # fin unitr.f()


################################################################################
## Nom    : creationTablesBase.f()
## Objet  : exécution des fonctions unit.f, unitesp.f et unitespta.f
##          dans le cas de la vidéo rotative, unitr.f, unitespr.f et unitesptar.f
################################################################################

creationTablesBase.f <- function(){
    print("fonction creationTablesBase.f activée")


    ## ATTENTION A L'ORDRE D'APPEL DES FONCTIONS!!
    if (!is.benthos.f())                 # unique(unitobs$type) != "LIT"
    {  #car pas de classes de tailles avec les recouvrements
        unitespta.f()
        if (Jeuxdonnescoupe==0)
        {
            ## SAUVunitespta <- unitespta  # stockage inutile [yr: 10/08/2010]
            assign("SAUVunitespta", unitespta, envir=.GlobalEnv)
        }
    }
    unitesp.f()
    unit.f()
    ## Sauvegarde des calculs pour restauration sans rechargement
    if (Jeuxdonnescoupe==0)
    {
        ## SAUVobs <- obs                  # ########################################
        ## SAUVunitobs <- unitobs          #
        ## SAUVcontingence <- contingence  # stockages inutiles [yr: 10/08/2010]
        ## SAUVunitesp <- unitesp          #
        ## SAUVunit <- unit                # ########################################
        assign("SAUVobs", obs, envir=.GlobalEnv)
        assign("SAUVunitobs", unitobs, envir=.GlobalEnv)
        assign("SAUVcontingence", contingence, envir=.GlobalEnv)
        assign("SAUVunitesp", unitesp, envir=.GlobalEnv)
        assign("SAUVunit", unit, envir=.GlobalEnv)
        assign("SAUVTablePresAbs", TablePresAbs, envir=.GlobalEnv)
        assign("SAUVlistespunit", listespunit, envir=.GlobalEnv)
    }

    ## si SVR calcul des metriques par rotation
    if (unique(unitobs$type) == "SVR")
    {
        unitesptar.f()
        unitespr.f()
        unitr.f()
        if (Jeuxdonnescoupe==0)
        {
            ## SAUVunitesptar <- unitesptaR # ########################################
            ## SAUVunitespr <- unitespr     # stockages inutiles [yr: 10/08/2010]
            ## SAUVunitr <- unitr           # ########################################
            assign("SAUVunitesptar", unitesptaR, envir=.GlobalEnv)
            assign("SAUVunitespr", unitespr, envir=.GlobalEnv)
            assign("SAUVunitr", unitr, envir=.GlobalEnv)
        }
    }
    if (Jeuxdonnescoupe==1)
    {
        tkmessageBox(message="Les métriques par unités d'observations ont été recalculées sur le jeu de données sélectionnés")
        gestionMSGinfo.f("CalculSelectionFait")
    }
    if (Jeuxdonnescoupe==0)
    {
        tkmessageBox(message="Les métriques par unités d'observations ont été calculées sur l'ensemble du jeu de données importé")
        gestionMSGinfo.f("CalculTotalFait")
    }
}

################################################################################
## Nom    : creationTablesCalcul.f()
## Objet  : Génération d'une table globale basée sur obs a partir de listespunit
## rajoute des champs sélectionnés dans unitosb et especes
##          dans le cas de la vidéo
##          dans le cas du benthos
##          dans les cas autre que benthos
################################################################################

creationTablesCalcul.f <- function(){

    print("fonction creationTablesCalcul.f activée")
    if (!is.benthos.f())                # unique(unitobs$type) != "LIT"
    {  #car pas de classes de tailles avec les recouvrements

    }
    ## si SVR calcul des metriques par rotation
    if (unique(unitobs$type) == "SVR")
    {

    }

    TableMetrique <- listespunit

    ## Simplification OK [yreecht: 08/10/2010] :
    TableMetrique <- cbind(TableMetrique,
                           ## Colonnes d'unitobs :
                           unitobs[match(TableMetrique$unite_observation, unitobs$unite_observation),
                                   c("station", "caracteristique_1", "caracteristique_2", "fraction_echantillonnee",
                                     "jour", "mois", "heure", "nebulosite", "direction_vent", "force_vent", "etat_mer",
                                     "courant", "maree", "phase_lunaire", "latitude", "longitude", "avant_apres",
                                     "biotope_2", "habitat1", "habitat2", "habitat3", "visibilite", "prof_min",
                                     "prof_max", "DimObs1", "DimObs2", "nb_plong", "plongeur")],
                           ## Colonnes du référentiel espèces :
                           especes[match(TableMetrique$code_espece, especes$code_espece),
                                   c("Genre", "Famille", "mobilite", "nocturne", "cryptique", "taillemax", "regim.alim",
                                     ## Interêts types de pêches :
                                     grep(paste("^interet\\.[[:alpha:]]+", siteEtudie, "$", sep=""), # Colonnes
                                          colnames(especes), value=TRUE))])                          # site-spécifiques.


    if (is.benthos.f())                 # unique(unitobs$type) == "LIT"
    {
        TableMetrique$Cath_benthique <- especes$Cath_benthique[match(TableMetrique$code_espece, especes$code_espece)]
    }
    if (unique(unitobs$type) == "UVC")
    {
        ## Si Video     "poids.moyen.petits" "poids.moyen.moyens" "poids.moyen.gros"   "taille_max_petits"  "taille_max_moyens"     "L50"
    }
    ## print(names(TableMetrique))    # Pas utile sauf en développement.

    ## On peut rendre plus lisible ce qui suit... [yreecht: 22/07/2010] OK [yreecht: 08/10/2010]
    TableBiodiv <- unit
    names(TableBiodiv)[1] <- "unite_observation"

    TableBiodiv <- cbind(TableBiodiv,
                         ## Colonnes d'unitobs :
                         unitobs[match(TableBiodiv$unite_observation, unitobs$unite_observation),
                                 c("station", "caracteristique_1", "caracteristique_2", "fraction_echantillonnee",
                                   "jour", "mois", "heure", "nebulosite", "direction_vent", "force_vent", "etat_mer",
                                   "courant", "maree", "phase_lunaire", "avant_apres", "biotope_2", "habitat1",
                                   "habitat2", "habitat3", "visibilite", "prof_min", "prof_max", "DimObs1", "DimObs2",
                                   "nb_plong", "plongeur")])


    assign("TableBiodiv", TableBiodiv, envir=.GlobalEnv)
    assign("TableMetrique", TableMetrique, envir=.GlobalEnv)
    print("tableau TableMetrique réalisé")
    ## print(names(TableMetrique))    # Pas utile sauf en développement.

    if (Jeuxdonnescoupe==0)
    {
        assign("SAUVTableBiodiv", TableBiodiv, envir=.GlobalEnv)
        assign("SAUVTableMetrique", TableMetrique, envir=.GlobalEnv)
    }else{}
}

########################################################################################################################
calcPresAbs.f <- function()
{
    ## Purpose: Créer une table "TablePresAbs" à partir de la table de
    ##          contingence
    ## ----------------------------------------------------------------------
    ## Arguments: aucun.
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 13 oct. 2010, 15:18

    if (nrow(contingence) > 0 && ncol(contingence) > 0) # Contingence ne doit pas être vide.
    {
        ## Pour avoir les bons noms de colonnes dans ce qui suit :
        names(dimnames(contingence)) <- c("unite_observation", "code_espece")

        ## On utilise la méthode "as.data.frame" pour la classe "table"
        ## (fonctionne comme un reshape() en direction="long") :
        TablePresAbs <- as.data.frame(as.table(contingence), responseName="pres_abs", stringsAsFactors=FALSE)

        ## Seules les présences absences nous intéressent :
        TablePresAbs$pres_abs[TablePresAbs$pres_abs > 0] <- 1

        assign(x="TablePresAbs", value=TablePresAbs, envir=.GlobalEnv)
    }else{
        assign(x="TablePresAbs", value=NULL, envir=.GlobalEnv)
    }

    statutPresAbs.f()
}
