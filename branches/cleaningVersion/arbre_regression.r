## ################################################################################
## Nom     : arbre1.f
## Objet   : régression d'une métrique de la table "unit" en fonction d'un facteur
##           sélectionné dans la table "unitobs"
## Input   : tables "unit" et "unitobs"
## Output  : arbre de régression à un facteur
## ################################################################################

arbre1.f <- function ()
{
    affichageMetriques.f()

    bb <- tktoplevel(width = 80)
    tkwm.title(bb, "Sélection du facteur explicatif")
    scr <- tkscrollbar(bb, repeatinterval=5, command=function(...)tkyview(tl, ...))
    tl <- tklistbox(bb, height=20, width=30, selectmode="single",
                    yscrollcommand=function(...)tkset(scr, ...), background="white")
    tkgrid(tklabel(bb, text="Liste des facteurs"))
    tkgrid(tl, scr)
    tkgrid.configure(scr, rowspan=4, sticky="nsw")
    facteurs <- sort(names(unitobs))

    ## création de la liste des facteurs de + 2 modalites
    listeFacteursOK <-"pas de facteur"
    a <- length(facteurs)
    j <- 1

    for (i in (1:a))
    {
        if (length(unique(na.exclude(unitobs[, facteurs[i]])))>1)
        {
            listeFacteursOK[j] <- facteurs[i]
            j <- j+1
        }
    }

    b <- length(listeFacteursOK)

    for (i in (1:b))
    {
        tkinsert(tl, "end", listeFacteursOK[i])
    }

    tkselection.set(tl, 0)

    OnOK <- function()
    {
        fact1arbre <- listeFacteursOK[as.numeric(tkcurselection(tl))+1]
        assign("fact1arbre", fact1arbre, envir=.GlobalEnv)
        tkdestroy(bb)

        unit[, fact1arbre[1]] <- unitobs[match(unit$unitobs, unitobs$unite_observation), fact1arbre[1]]
        unitbis <- unit[!is.na(unit[, fact1arbre]), ]

        tkmessageBox(message=paste("Le facteur choisi est renseigné pour ", nrow(unitbis), " données.", sep=""))

        x11(width=50, height=30, pointsize=10)

        arbre1 <- mvpart(data.matrix(unitbis[, me]) ~ data.matrix(unitbis[, fact1arbre]), unitbis, xv="1se")

        if (length(arbre1$cptable[, "CP"])<2)
        {
            graphics.off()
            tkmessageBox(message="impossible de réaliser un arbre de régression avec ce facteur")
        }else{
            mtext(line=2, paste("Arbre de régression (MVPART) pour la métrique: ", me,
                                "\n et le facteur de variabilité: ", fact1arbre[1]))
            summary(arbre1)
            arbre1rpart <- rpart(data.matrix(unitbis[, me]) ~ unitbis[, fact1arbre[1]], unitbis)

            x11(width=50, height=30, pointsize=10)

            summary(arbre1rpart)
            print(arbre1rpart)

            plot(arbre1rpart, branch=0.2, compress=TRUE, margin=0.1,
                 main=paste("Arbre de régression (RPART) pour la métrique: ", me,
                            "\n et le facteur de variabilité: ", fact1arbre[1]))

            text(arbre1rpart, use.n=TRUE, pretty=0, all=TRUE)

            capture.output(print(arbre1),
                           print(summary(arbre1)),
                           file=paste(nameWorkspace, "/FichiersSortie/arbre de régression-MVPART_",
                                      me, fact1arbre, ".txt", sep=""))

            capture.output(print(arbre1rpart),
                           print(summary(arbre1rpart)),
                           file=paste(nameWorkspace, "/FichiersSortie/arbre de régression-RPART_",
                                      me, fact1arbre[1], ".txt", sep=""))
        }
    }

    OK.but <-tkbutton(bb, text="OK", command=OnOK)
    tkgrid(OK.but)
    tkfocus(bb)
    tkwait.window(bb)
}

## ################################################################################
## Nom     : arbre2.f
## Objet   : régression d'une métrique de la table "unit" en fonction de 2
##           facteurs sélectionnés dans la table "unitobs"
## Input   : tables "unit" et "unitobs"
## Output  : arbre de régression à deux facteurs
## ################################################################################

arbre2.f <- function ()
{
    ## Selection de la métrique
    affichageMetriques.f()

    ## Selection du premier facteur explicatif
    bb <- tktoplevel(width = 80)
    tkwm.title(bb, "Selection du PREMIER facteur explicatif")
    scr <- tkscrollbar(bb, repeatinterval=5, command=function(...)tkyview(tl, ...))
    tl <- tklistbox(bb, height=20, width=30, selectmode="single",
                    yscrollcommand=function(...)tkset(scr, ...), background="white")
    tkgrid(tklabel(bb, text="Selection du PREMIER facteur explicatif"))
    tkgrid(tl, scr)
    tkgrid.configure(scr, rowspan=4, sticky="nsw")
    fac1 <- sort(names(unitobs))

    ## creation de la liste des facteurs de + 2 modalites
    listeFacteursOK <-"pas de facteur"
    a <- length(fac1)
    j <- 1

    for (i in (1:a))
    {
        if (length(unique(na.exclude(unitobs[, fac1[i]])))>1)
        {
            listeFacteursOK[j] <- fac1[i]
            j <- j+1
        }
    }

    b <- length(listeFacteursOK)

    for (i in (1:b))
    {
        tkinsert(tl, "end", listeFacteursOK[i])
    }

    tkselection.set(tl, 0)

    OnOK <- function()
    {
        fa1 <- listeFacteursOK[as.numeric(tkcurselection(tl))+1]
        assign("fa1", fa1, envir=.GlobalEnv)
        tkdestroy(bb)
    }

    OK.but <-tkbutton(bb, text="OK", command=OnOK)
    tkgrid(OK.but)
    tkfocus(bb)
    tkwait.window(bb)

    ## sélection du deuxième facteur explicatif
    bb <- tktoplevel(width = 80)
    tkwm.title(bb, "Selection du SECOND facteur explicatif")
    scr <- tkscrollbar(bb, repeatinterval=5, command=function(...)tkyview(tl, ...))
    tl <- tklistbox(bb, height=20, width=30, selectmode="single",
                    yscrollcommand=function(...)tkset(scr, ...), background="white")
    tkgrid(tklabel(bb, text="Selection du SECOND facteur explicatif"))
    tkgrid(tl, scr)
    tkgrid.configure(scr, rowspan=4, sticky="nsw")
    fac2 <- sort(names(unitobs))

    ## creation de la liste des facteurs de + 2 modalites
    listeFacteursOK <-"pas de facteur"
    a <- length(fac2)
    j <- 1

    for (i in (1:a))
    {
        if (length(unique(na.exclude(unitobs[, fac2[i]])))>1)
        {
            listeFacteursOK[j] <- fac2[i]
            j <- j+1
        }
    }

    b <- length(listeFacteursOK)

    for (i in (1:b))
    {
        tkinsert(tl, "end", listeFacteursOK[i])
    }

    tkselection.set(tl, 0)

    OnOK <- function()
    {
        fa2 <- listeFacteursOK[as.numeric(tkcurselection(tl))+1]
        fact1arbre <- c(fa1, fa2)
        assign("fact1arbre", fact1arbre, envir=.GlobalEnv)
        tkdestroy(bb)

        unit[, fact1arbre[1]] <- as.factor(unitobs[, fact1arbre[1]][match(unit$unitobs, unitobs$unite_observation)])
        unit[, fact1arbre[2]] <- as.factor(unitobs[, fact1arbre[2]][match(unit$unitobs, unitobs$unite_observation)])
        unitbis <- unit[!is.na(unit[, fact1arbre[1]]), ]
        unitbis <- unitbis[!is.na(unitbis[, fact1arbre[2]]), ]

        tkmessageBox(message=paste("Les facteurs choisis sont renseignés pour ", nrow(unitbis), " données.", sep=""))

        x11(width=50, height=30, pointsize=10)

        arbre2 <- mvpart(data.matrix(unitbis[, me]) ~ unitbis[, fact1arbre[1]] +
                         unitbis[, fact1arbre[2]], unitbis, xv="1se")  #pca=TRUE

        if (length(arbre2$cptable[, "CP"])<2)
        {
            graphics.off()
            tkmessageBox(message="impossible de réaliser un arbre de régression avec ces facteurs")
        }else{
            mtext(line=2, paste("Arbre de régression (MVPART) pour la metrique: ", me,
                                "\n et les facteurs de variabilité: ", fact1arbre[1], "et", fact1arbre[2]))

            summary(arbre2)
            print(arbre2)

            arbre2rpart <- rpart(data.matrix(unitbis[, me]) ~ unitbis[, fact1arbre[1]] +
                                 unitbis[, fact1arbre[2]], unitbis)

            x11(width=50, height=30, pointsize=10)
            summary(arbre2rpart)
            print(arbre2rpart)
            plot(arbre2rpart, branch=0.2, compress=TRUE, margin=0.1,
                 main=paste("Arbre de régression (RPART) pour la metrique: ", me,
                            "\n et les facteurs de variabilité: ", fact1arbre[1], "et", fact1arbre[2]))

            text(arbre2rpart, use.n=TRUE, pretty=0, all=TRUE)

            ## plotcp(arbre2rpart)
            capture.output(print(arbre2), print(summary(arbre2)),
                           file=paste(nameWorkspace, "/FichiersSortie/arbre de régression-MVPART_",
                                      me, fact1arbre[1], fact1arbre[2], ".txt", sep=""))

            capture.output(print(arbre2rpart), print(summary(arbre2rpart)),
                           file=paste(nameWorkspace, "/FichiersSortie/arbre de régression-RPART_",
                                      me, fact1arbre[1], fact1arbre[2], ".txt", sep=""))
        }
    }
    OK.but <-tkbutton(bb, text="OK", command=OnOK)
    tkgrid(OK.but)
    tkfocus(bb)
    tkwait.window(bb)
}

## ################################################################################
## Nom     : arbre3.f
## Objet   : régression d'une métrique de la table "unit" en fonction de 3
##           facteurs sélectionnés dans la table "unitobs"
## Input   : tables "unit" et "unitobs"
## Output  : arbre de régression à trois facteurs
## ################################################################################

arbre3.f <- function ()
{
    affichageMetriques.f()

    ## Selection du premier facteur explicatif
    bb <- tktoplevel(width = 80)
    tkwm.title(bb, "Selection du PREMIER facteur explicatif")
    scr <- tkscrollbar(bb, repeatinterval=5, command=function(...)tkyview(tl, ...))
    tl <- tklistbox(bb, height=20, width=30, selectmode="single",
                    yscrollcommand=function(...)tkset(scr, ...), background="white")
    tkgrid(tklabel(bb, text="Selection du PREMIER facteur explicatif"))
    tkgrid(tl, scr)
    tkgrid.configure(scr, rowspan=4, sticky="nsw")
    fac1 <- sort(names(unitobs))

    ## creation de la liste des facteurs de + 2 modalites
    listeFacteursOK <-"pas de facteur"
    a <- length(fac1)
    j <- 1

    for (i in (1:a))
    {
        if (length(unique(na.exclude(unitobs[, fac1[i]])))>1)
        {
            listeFacteursOK[j] <- fac1[i]
            j <- j+1
        }
    }

    b <- length(listeFacteursOK)

    for (i in (1:b))
    {
        tkinsert(tl, "end", listeFacteursOK[i])
    }

    tkselection.set(tl, 0)

    OnOK <- function()
    {
        fa1 <- listeFacteursOK[as.numeric(tkcurselection(tl))+1]
        assign("fa1", fa1, envir=.GlobalEnv)
        tkdestroy(bb)
    }

    OK.but <-tkbutton(bb, text="OK", command=OnOK)
    tkgrid(OK.but)
    tkfocus(bb)
    tkwait.window(bb)

    ## sélection du deuxième facteur explicatif
    bb <- tktoplevel(width = 80)
    tkwm.title(bb, "Selection du SECOND facteur explicatif")
    scr <- tkscrollbar(bb, repeatinterval=5, command=function(...)tkyview(tl, ...))
    tl <- tklistbox(bb, height=20, width=30, selectmode="single",
                    yscrollcommand=function(...)tkset(scr, ...), background="white")
    tkgrid(tklabel(bb, text="Selection du SECOND facteur explicatif"))
    tkgrid(tl, scr)
    tkgrid.configure(scr, rowspan=4, sticky="nsw")
    fac2 <- sort(names(unitobs))

    ## creation de la liste des facteurs de + 2 modalites
    listeFacteursOK <-"pas de facteur"
    a <- length(fac2)
    j <- 1
    for (i in (1:a))
    {
        if (length(unique(na.exclude(unitobs[, fac2[i]])))>1)
        {
            listeFacteursOK[j] <- fac2[i]
            j <- j+1
        }
    }
    b <- length(listeFacteursOK)
    for (i in (1:b))
    {
        tkinsert(tl, "end", listeFacteursOK[i])
    }
    tkselection.set(tl, 0)
    OnOK <- function()
    {
        fa2 <- listeFacteursOK[as.numeric(tkcurselection(tl))+1]
        assign("fa2", fa2, envir=.GlobalEnv)
        tkdestroy(bb)
    }
    OK.but <-tkbutton(bb, text="OK", command=OnOK)
    tkgrid(OK.but)
    tkfocus(bb)
    tkwait.window(bb)

    ## sélection du troisième facteur explicatif
    bb <- tktoplevel(width = 80)
    tkwm.title(bb, "Selection du TROISIEME facteur explicatif")
    scr <- tkscrollbar(bb, repeatinterval=5, command=function(...)tkyview(tl, ...))
    tl <- tklistbox(bb, height=20, width=30, selectmode="single",
                    yscrollcommand=function(...)tkset(scr, ...), background="white")
    tkgrid(tklabel(bb, text="Selection du TROISIEME facteur explicatif"))
    tkgrid(tl, scr)
    tkgrid.configure(scr, rowspan=4, sticky="nsw")
    fac3 <- sort(names(unitobs))

    ## creation de la liste des facteurs de + 2 modalites
    listeFacteursOK <-"pas de facteur"
    a <- length(fac3)
    j <- 1
    for (i in (1:a))
    {
        if (length(unique(na.exclude(unitobs[, fac3[i]])))>1)
        {
            listeFacteursOK[j] <- fac3[i]
            j <- j+1
        }
    }
    b <- length(listeFacteursOK)
    for (i in (1:b))
    {
        tkinsert(tl, "end", listeFacteursOK[i])
    }
    tkselection.set(tl, 0)
    OnOK <- function()
    {
        fa3 <- listeFacteursOK[as.numeric(tkcurselection(tl))+1]
        fact1arbre <- c(fa1, fa2, fa3)
        assign("fact1arbre", fact1arbre, envir=.GlobalEnv)
        tkdestroy(bb)
        unit[, fact1arbre[1]] <- as.factor(unitobs[, fact1arbre[1]][match(unit$unitobs, unitobs$unite_observation)])
        unit[, fact1arbre[2]] <- as.factor(unitobs[, fact1arbre[2]][match(unit$unitobs, unitobs$unite_observation)])
        unit[, fact1arbre[3]] <- as.factor(unitobs[, fact1arbre[3]][match(unit$unitobs, unitobs$unite_observation)])
        unitbis <- unit[!is.na(unit[, fact1arbre[1]]), ]
        unitbis <- unitbis[!is.na(unitbis[, fact1arbre[2]]), ]
        unitbis <- unitbis[!is.na(unitbis[, fact1arbre[3]]), ]

        tkmessageBox(message=paste("Les facteurs choisis sont renseignés pour ", nrow(unitbis), " données.", sep=""))

        x11(width=50, height=30, pointsize=10)

        arbre3 <- mvpart(data.matrix(unitbis[, me]) ~ unitbis[, fact1arbre[1]] +
                         unitbis[, fact1arbre[2]]+unitbis[, fact1arbre[3]], unitbis, xv="1se")

        if (length(arbre3$cptable[, "CP"])<2)
        {
            graphics.off()
            tkmessageBox(message="impossible de réaliser un arbre de régression avec ces facteurs")
        }else{
            mtext(line=2, paste("Arbre de régression (MVPART) pour la métrique: ", me,
                                "\n et les facteurs de variabilité: ",
                                fact1arbre[1], fact1arbre[2], "et", fact1arbre[3]))

            summary(arbre3)

            arbre3rpart <- rpart(data.matrix(unitbis[, me]) ~ unitbis[, fact1arbre[1]] +
                                 unitbis[, fact1arbre[2]]+unitbis[, fact1arbre[3]], unitbis)

            x11(width=50, height=30, pointsize=10)

            summary(arbre3rpart)
            print(arbre3rpart)

            plot(arbre3rpart, branch=0.2, compress=TRUE, margin=0.1,
                 main=paste("Arbre de régression (RPART) pour la métrique: ", me,
                            "\n et les facteurs de variabilité: ", fact1arbre[1], fact1arbre[2], "et", fact1arbre[3]))

            text(arbre3rpart, use.n=TRUE, pretty=0, all=TRUE)
            ## plotcp(arbre2rpart)

            capture.output(print(arbre3), print(summary(arbre3)),
                           file=paste(nameWorkspace, "/FichiersSortie/arbre de régression-MVPART_",
                                      me, fact1arbre[1], fact1arbre[2], fact1arbre[3], ".txt", sep=""))

            capture.output(print(arbre3rpart), print(summary(arbre3rpart)),
                           file=paste(nameWorkspace, "/FichiersSortie/arbre de régression-RPART_",
                                      me, fact1arbre[1], fact1arbre[2], fact1arbre[3], ".txt", sep=""))
        }
    }

    OK.but <-tkbutton(bb, text="OK", command=OnOK)
    tkgrid(OK.but)
    tkfocus(bb)
    tkwait.window(bb)
}

## ################################################################################
