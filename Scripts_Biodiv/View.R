#-*- coding: latin-1 -*-
# Time-stamp: <2019-02-03 21:15:20 yreecht>

## Plateforme PAMPA de calcul d'indicateurs de ressources & biodiversité
##   Copyright (C) 2008-2018 Ifremer - Tous droits réservés.
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

########################################################################################################################
Voirentableau <- function(Montclarray, title="", height=-1, width=-1, nrow=-1, ncol=-1, infos=title,
                          image)
{
    tclRequire("Tktable")

    tb <- tktoplevel()
    tkwm.title(tb, title)

    tkbind(tb, "<Destroy>",
           function()
       {
           ## winRaise.f(W.main)
       })

    resultDir <- get("filePathes", envir = .dataEnv)["results"]

    ## Fonctions activées par les boutons de la fenêtre
    FermerWintb <- function()
    {
        tkdestroy(tb)
    }

    EnregistrerWintb <- function()
    {
        FichierCSV <- paste(resultDir, mltext("table.file.prefix"), title, ".csv", sep="")
        message(FichierCSV)
        write.csv(dataframetb, file=FichierCSV, row.names = FALSE)

        add.logFrame.f(msgID="TableSavedCSV", env = .baseEnv, file=FichierCSV)
    }

    ## Déclaration des objets bouton
    Fermer.but <- tkbutton(tb, text = mltext("filetest.B.close"), command=FermerWintb)
    Enregistrer.but <- tkbutton(tb, text = mltext("filetest.B.save.csv"), command=EnregistrerWintb)

    ## ICI CONTINUER LA MISE EN FORME
    dataframetb <- as.data.frame(matrix("", nrow = nrow, ncol = ncol),
                                 stringsAsFactors = FALSE)


    for (nbRow in (0:(nrow - 1)))          # Vectoriser ??? [yr: 27/07/2010]
    {
        for (nbCol in (1:ncol))
        {

            if (nbRow == 0)
            {
                colnames(dataframetb)[nbCol] <- tclvalue(Montclarray[[nbRow, nbCol - 1]])
            }else{
                 dataframetb[nbRow, nbCol] <- tclvalue(Montclarray[[nbRow, nbCol - 1]])
             }
        }
    }

    ## Éléments graphiques :
    frameOverwintb <- tkframe(tb)
    imgAsLabelwintb <- tklabel(frameOverwintb, image=image, bg="white")

    xscr <-tkscrollbar(tb, orient="horizontal", command=function(...)tkxview(tabletb,...))
    yscrtb <- tkscrollbar(tb, repeatinterval=3, command=function(...)tkyview(tabletb,...))

    tabletb <- tkwidget(tb, "table", rows=nrow, cols=ncol, titlerows=1, titlecols=0,
                        height=height+1, width=width+1, colwidth=23,
                        xscrollcommand=function(...) tkset(xscr,...),
                        yscrollcommand=function(...) tkset(yscrtb,...))

    ## ## Ne fonctionne pas [!!!] :
    ## tkbind(tabletb, "<MouseWheel>", function(...) tkyview(tabletb,...))
    ## tkbind(yscrtb, "<MouseWheel>", function(...) tkyview(tabletb,...))

    ## Placement des éléments :
    tkgrid(frameOverwintb, sticky="ew", columnspan=4)
    tkgrid(imgAsLabelwintb,
           tklabel(frameOverwintb, text=""),
           LB.titre <- tklabel(frameOverwintb, text=infos, relief="groove",
                               borderwidth=2, bg="yellow", justify="left"),
           padx=5, pady=5, sticky="nw")

    tkgrid.configure(LB.titre, sticky="new")

    tkgrid(tklabel(tb, text=paste("\n***", mltext("table.spreadsheet.copy"))))

    tkgrid(Enregistrer.but, Fermer.but, pady=5, padx=5)

    tkgrid(tabletb, yscrtb, columnspan=3, sticky="nsew")
    tkgrid.configure(yscrtb, sticky="nse")
    tkgrid(xscr, sticky="new", columnspan=2)

    tkconfigure(tabletb, variable=Montclarray, background="white", selectmode="extended", rowseparator="\"\n\"",
                colseparator="\"\t\"", titlerows=1,
                maxwidth=550)

    tkgrid.configure(tabletb, columnspan=2, sticky="w")

    tcl("update")
    ColAutoWidth.f(tabletb)
    RowAutoEight.f(tabletb)

    tcl("update")
    winSmartPlace.f(tb, xoffset=-30)

    return (tabletb)
}

########################################################################################################################
VoirPlanEchantillonnage.f <- function(dataEnv)
{
    runLog.f(msg=c(mltext("logmsg.print.sampling.design")))

    resultsDir <- get("filePathes", envir=dataEnv)["results"]

    myRarrayPE <- read.csv(paste(resultsDir,
                                 "PlanEchantillonnage_basique", ## mltext("sampling.file.prefix"),
                                 ifelse(getOption("P.selection"),
                                        "_selection", ## mltext("sampling.file.sufix"),
                                        ""),
                                 ".csv", sep=""),
                           row.names=1)

    tclarrayPE <- tclArray()
    ## tclarrayPE[[0, ]] <- c("Année", "observation.type", "Fréquence")

    tclarrayPE[[0, 0]] <- paste(mltext("sampling.arr.0.0.1"),
                                mltext("sampling.arr.0.0.2"), sep="")

    ## Remplissage du tableau tcl :
    for (i in (1:nrow(myRarrayPE)))
    {
        tclarrayPE[[i, 0]] <- row.names(myRarrayPE)[i]

        for (j in (1:ncol(myRarrayPE)))
        {
            if (i == 1)
            {
                tclarrayPE[[0, j]] <- colnames(myRarrayPE)[j]
            }else{}

            tclarrayPE[[i, j]] <- as.character(myRarrayPE[i, j])
        }
    }

    pe <- tktoplevel()
    tkwm.title(pe, mltext("sampling.title"))
    tablePlanEch <- tkwidget(pe, "table", variable=tclarrayPE, rows=dim(myRarrayPE)[1]+1, cols=3,
                             titlerows=1, titlecol=1,
                             selectmode="extended", colwidth=30, background="white")


    tkpack(tablePlanEch)
    tcl("update")

    RowAutoEight.f(tablePlanEch)
    ColAutoWidth.f(tablePlanEch)

    as.numeric(unlist(strsplit(tclvalue(tcl(.Tk.ID(tablePlanEch),
                                                         "index", "end")),
                               ",")))

    TK.table <- tablePlanEch

    tcl("update")
    winSmartPlace.f(pe)
}

########################################################################################################################
VoirInformationsDonneesEspeces.f <- function(dataEnv, image)
{

    unitSp <- get("unitSp", envir=dataEnv)
    refesp <- get("refesp", envir=dataEnv)
    unitobs <- get("unitobs", envir=dataEnv)
    nombres <- ifelse(is.benthos.f(), mltext("unit.colony"), getOption("P.nbName"))

    Nbesp <- length(unique(unitSp[ , "species.code"]))

    tclarrayID <- tclArray()
    tclarrayID[[0, 0]] <- mltext("infoSpecies.arr.0.0")
    tclarrayID[[0, 1]] <- paste(mltext("info.arr.Nb"),
                                ifelse(is.benthos.f(),
                                       mltext("info.arr.col"),
                                       mltext("info.arr.ind")),
                                mltext("infoSpecies.arr.0.1"), sep="")
    tclarrayID[[0, 2]] <- paste(mltext("info.arr.Nb"),
                                ifelse(is.benthos.f(),
                                       mltext("info.arr.col"),
                                       mltext("info.arr.ind")),
                                mltext("infoSpecies.arr.0.2"), sep="")
    tclarrayID[[0, 3]] <- mltext("infoSpecies.arr.0.3")

    mini <- tapply(unitSp[ , nombres], unitSp[ , "species.code"], min, na.rm=TRUE)
    maxi <- tapply(unitSp[ , nombres], unitSp[ , "species.code"], max, na.rm=TRUE)

    nbunitobs <- nrow(unique(unitobs))
    pacha <- unitSp[, c("observation.unit", "species.code", nombres, "pres.abs"), drop=FALSE]

    for (i in (1:Nbesp))
    {
        tclarrayID[[i, 0]] <- unique(as.character(unitSp[ , "species.code"]))[i]
        tclarrayID[[i, 1]] <- round(mini[i], 2)
        tclarrayID[[i, 2]] <- round(maxi[i], 2)
        tclarrayID[[i, 3]] <-
            paste(round(length(pacha[(pacha[ , "pres.abs"] == 1 &
                                      pacha[ , "species.code"] == unique(unitSp[ , "species.code"])[i]),
                                     "observation.unit"]) /
                        length(pacha[pacha[ , "species.code"] == unique(unitSp[ , "species.code"])[i],
                                     "observation.unit"]) * 100,
                        digits=2), "%")
    }

    ## Pour informations sur le nombre d'espèces :
    tmp <- unique(cbind(pacha[ , "species.code"],
                        refesp[match(pacha[ , "species.code"], refesp[ , "species.code"]),
                               c("phylum", "species")]))

    tableInfodonnees <- Voirentableau(tclarrayID,
                                      title=mltext("infoSpecies.title"),
                                      infos=paste(mltext("infoSpecies.info.title"),
                                                  mltext("infoSpecies.info.cat"), nrow(tmp),
                                                  mltext("infoSpecies.info.tax"),
                                                  sum(!is.na(tmp[ , "phylum"])),
                                                  mltext("infoSpecies.info.sp"),
                                                  sum(!is.na(tmp[ , "species"]) &
                                                      ! (tmp[ , "species"] %in% c("sp.", "spp.", "sp", "spp"))),
                                                  mltext("infoSpecies.info.subset"), sep=""),
                                      height=Nbesp, width=4, nrow=Nbesp + 1, ncol=4,
                                      image=image)
}#fin VoirInformationsDonneesEspeces

########################################################################################################################
VoirInformationsDonneesUnitobs.f <- function(dataEnv, image)
{
    obs <- get("obs", envir=dataEnv)
    unitobs <- get("unitobs", envir=dataEnv)
    unitSp <- get("unitSp", envir=dataEnv)
    nombres <- ifelse(is.benthos.f(), mltext("unit.colony"), getOption("P.nbName"))

    Nbunitobs <- nlevels(obs[ , "observation.unit"]) ## length(unique(unitobs[ , "observation.unit"]))

    tclarrayID <- tclArray()

    tclarrayID[[0, 0]] <- mltext("infoUnitobs.arr.0.0")    #
    tclarrayID[[0, 1]] <- mltext("infoUnitobs.arr.0.1")                   #
    tclarrayID[[0, 2]] <- mltext("infoUnitobs.arr.0.2")                #
    tclarrayID[[0, 3]] <- mltext("infoUnitobs.arr.0.3")              #
    tclarrayID[[0, 4]] <- paste(mltext("info.arr.Nb"),
                                ifelse(is.benthos.f(),
                                       mltext("info.arr.col"),
                                       mltext("info.arr.ind")),
                                mltext("infoUnitobs.arr.0.4"), sep="")    #

    pacha <- unitSp[ , c("observation.unit", "species.code", nombres, "pres.abs"), drop=FALSE]

    ## mini <- tapply(unitSp[ , nombres], unitSp[ , "observation.unit"], min, na.rm=TRUE) # [!!!]

    maxi <- round(tapply(unitSp[ , nombres], unitSp[ , "observation.unit"], max, na.rm=TRUE), 2) # [!!!]

    for (i in (1:Nbunitobs))
    {
        tclarrayID[[i, 0]] <- levels(obs[ , "observation.unit"])[i]

        tclarrayID[[i, 1]] <- as.character(unitobs[(unitobs[ , "observation.unit"] ==
                                                    levels(obs[ , "observation.unit"])[i]),
                                                   "site"])

        tclarrayID[[i, 2]] <- as.character(unitobs[(unitobs[ , "observation.unit"] ==
                                                    levels(obs[ , "observation.unit"])[i]),
                                                   "biotop"])
        tclarrayID[[i, 3]] <-                                                                        #
            length(pacha[(pacha[ , "pres.abs"]==1 &
                          pacha[ , "observation.unit"] == levels(obs[ , "observation.unit"])[i]),
                         "species.code"])

        tclarrayID[[i, 4]] <- maxi[i]
    }

    tableInfodonnees <- Voirentableau(tclarrayID,
                                      title=mltext("infoUnitobs.title"),
                                      infos=paste(mltext("infoUnitobs.info.title"),
                                                  mltext("infoUnitobs.info.subset"),
                                                  sep=""),
                                      height=Nbunitobs, width=5, nrow=Nbunitobs + 1, ncol=5,
                                      image=image)
}


