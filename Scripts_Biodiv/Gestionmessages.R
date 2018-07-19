#-*- coding: latin-1 -*-
# Time-stamp: <2018-07-19 18:18:06 yreecht>

## Plateforme PAMPA de calcul d'indicateurs de ressources & biodiversité
##   Copyright (C) 2008-2013 Ifremer - Tous droits réservés.
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

## le fichier gestionmessages.r a besoin du fichier Config.R pour être executé
## les passages à la ligne se font à la fin des messages
## on retourne encore à la ligne avant une erreur

gestionMSGerreur.f <- function (nameerror, variable, env=.GlobalEnv)
{
    runLog.f(msg=c("Envoie d'un message d'erreur dans l'interface :"))

    ## Message :
    MSG <-
        switch(nameerror,
               "recouvrementsansLIT"={
                   paste(mltext("error.recouvrementsansLIT.1"),
                         mltext("error.recouvrementsansLIT.2"), sep="")
               },
               "noWorkspace"={
                   mltext("error.noWorkspace")
               },
               "nbChampUnitobs"={
                   paste(mltext("error.nbChampUnitobs.1"),
                         mltext("error.nbChampUnitobs.2"), sep="")
               },
               ## "nbChampUnitobs"={
               ##     paste("Votre fichier 'Unites d'observation' ne comporte pas le bon nombre de champs!",
               ##           " Il devrait en contenir 35. Corrigez le et recommencez l'importation.\n", sep="")
               ## },
               "nbChampEsp"={
                   paste(mltext("error.nbChampEsp.1"),
                         mltext("error.nbChampEsp.2"), sep="")
               },
               "nbChampObs"={
                   paste(mltext("error.nbChampObs.1"),
                         mltext("error.nbChampObs.2"), sep="")
               },
               "UnitobsDsObsUnitobs"={
                   paste(mltext("error.UnitobsDsObsUnitobs.1"),
                         mltext("error.UnitobsDsObsUnitobs.1"), sep="")
               },
               "CaractereInterditDsObs"={
                   mltext("error.CaractereInterditDsObs")
               },
               "CaractereInterditDsUnitObs"={
                   mltext("error.CaractereInterditDsUnitObs")
               },
               "ZeroEnregistrement"={
                   mltext("error.ZeroEnregistrement")
               },
               "UneSeuleValeurRegroupement"={
                   mltext("error.UneSeuleValeurRegroupement")
               },
               "CritereMalRenseigne50"={
                   paste(mltext("error.CritereMalRenseigne50.1"),
                         mltext("error.CritereMalRenseigne50.2"), sep="")
               },
               "CaractereInterdit"={
                   paste(mltext("error.CaractereInterdit.1"), variable,
                         mltext("error.CaractereInterdit.2"),
                         mltext("error.CaractereInterdit.3"), sep="")
               },
               ## Message par défaut :
               mltext("error.noMsg"))

    ## gestionMSGerreur.f(nbChampUnitobs)
    ## langue = EN

    tkinsert(get("helpframe", envir=env), "end", paste(mltext("error.prefix"), MSG, sep=""))
    tkyview.moveto(get("helpframe", envir=env), 1)
}

gestionMSGaide.f <- function (namemsg, env=.GlobalEnv)
{
    runLog.f(msg=c(mltext("logmsg.helpmsg")))

    ## Message :
    MSG <-
        switch(namemsg,
               "ZeroEnregistrement"={
                   paste(mltext("helpmsg.ZeroEnregistrement.1"),
                         mltext("helpmsg.ZeroEnregistrement.2"), sep="")
               },
               "etapeImport"={
                   paste(mltext("helpmsg.etapeImport.1"),
                         mltext("helpmsg.etapeImport.2"),
                         mltext("helpmsg.etapeImport.3"),
                         sep="")
               },
               "SelectionOuTraitement"={
                   paste(mltext("helpmsg.SelectionOuTraitement.1"),
                         mltext("helpmsg.SelectionOuTraitement.2"),
                         "\n", sep="")
               },
               ## "startsansficher"={
               ##     paste("Si les fichiers par défauts paramétrés dans 'Config.R'- ", fileNameUnitobs, " - ",
               ##           fileNameObs, " - ",
               ##           fileNameRefesp, " ne sont pas les bons, Veuillez les modifier\n", sep="")
               ## },
               "etapeselected"={
                   paste(mltext("helpmsg.etapeselected.1"),
                         mltext("helpmsg.etapeselected.2"),
                         mltext("helpmsg.etapeselected.3"),
                         mltext("helpmsg.etapeselected.4"),
                         sep="")
               },
               "message à définir")

    tkinsert(get("helpframe", envir=env), "end", paste("\n", MSG, "", sep=""))
    tkyview.moveto(get("helpframe", envir=env), 1)
}

########################################################################################################################
add.logFrame.f <- function(msgID, env=dataEnv,...)
{
    ## Purpose: Ajout de messages dans le cadre d'info sur les chargements
    ##          et sélections.
    ## ----------------------------------------------------------------------
    ## Arguments: msgID : identifiant du type de message.
    ##            env : environnement où est définit le cadre d'information
    ##                  (interface).
    ##            ... : arguments supplémentaires (dont l'existence est
    ##                  testée en fonction du type de message choisi.)
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date:  9 nov. 2011, 16:01

    ## On récupère les arguments supplémentaires sous une forme facilement utilisable (list) :
    argsSup <- list(...)

    ## Traitement des différents cas de message :
    msg <- switch(msgID,
                  "dataLoadingNew"={
                      if (any(!is.element("filePathes",
                                          names(argsSup))))
                      {
                          stop("Wrong argument(s)!")
                      }else{
                          paste("",
                                paste(rep("=", 100), collapse=""),
                                paste(mltext("logFmsg.loading"),
                                      format(Sys.time(), "%d/%m/%Y\t%H:%M:%S"),
                                      ")", sep=""),

                                paste(mltext("logFmsg.obsFile"), argsSup$filePathes["obs"]),
                                paste(mltext("logFmsg.unitobsFile"), argsSup$filePathes["unitobs"]),
                                paste(mltext("logFmsg.refespFile"), argsSup$filePathes["refesp"]),
                                ifelse(is.na(argsSup$filePathes["refspa"]), "",
                                       paste(mltext("logFmsg.refspaFile"), argsSup$filePathes["refspa"])),
                                paste(mltext("logFmsg.exportPath"), argsSup$filePathes["results"]),
                                "\n", sep="")
                      }
                  },
                  "restauration"={
                      paste("",
                            paste(rep("-", 80), collapse=""),
                            paste(mltext("logFmsg.restoreData"),
                                  format(Sys.time(), "%d/%m/%Y\t%H:%M:%S"),
                                  ")", sep=""),
                            "\n", sep="\n")
                  },
                  "selection"={
                      if (any(!is.element(c("facteur", "selection", "results", "referentiel", "has.SzCl"),
                                          names(argsSup))))
                      {
                          stop("Wrong argument(s)!")
                      }else{
                          paste("\n",   # [mlo]
                                paste(rep("-", 100), collapse=""),
                                paste("\nSélection des observations selon un critère",
                                      ifelse(argsSup$referentiel == "especes",
                                             " du référentiel espèces",
                                             " des unités d'observation"),
                                      " (",
                                      format(Sys.time(), "%d/%m/%Y\t%H:%M:%S"),
                                      ")", sep=""),
                                paste("\n\n   Facteur :", argsSup$facteur),
                                paste("\n   Modalités :",
                                      paste(argsSup$selection, collapse=", ")),
                                paste("\n\nFichiers exportés dans ", argsSup$results, " :", sep=""),
                                ifelse(isTRUE(argsSup$has.SzCl),
                                       paste("\n   - métriques par unité d'observation / esp. / cl. de taille :",
                                             "UnitobsEspeceClassetailleMetriques_selection.csv"),
                                       ""),
                                paste("\n   - métriques par unité d'observation / esp. :",
                                      "UnitobsEspeceMetriques_selection.csv"),
                                paste("\n   - métriques par unité d'observation :",
                                      "UnitobsMetriques_selection.csv"),
                                paste("\n   - plan d'échantillonnage basique (année - statut de protection) :",
                                      "PlanEchantillonnage_basique_selection.csv"),
                                "\n\n", sep="")
                      }
                  },
                  "fichiers"={
                      if (any(!is.element(c("results", "has.SzCl"),
                                          names(argsSup))))
                      {
                          stop("Arguments incorrects !")
                      }else{
                          paste("\n",
                                paste(rep("-", 100), collapse=""),
                                paste("\nFichiers exportés dans ", argsSup$results, sep=""),
                                paste("\n   (avant ", format(Sys.time(), "%d/%m/%Y\t%H:%M:%S"), ") :", sep=""),
                                ifelse(isTRUE(argsSup$has.SzCl),
                                       paste("\n   - métriques par unité d'observation / esp. / cl. de taille :",
                                             "UnitobsEspeceClassetailleMetriques.csv"),
                                       ""),
                                paste("\n   - métriques par unité d'observation / esp. :",
                                      "UnitobsEspeceMetriques.csv"),
                                paste("\n   - métriques par unité d'observation :",
                                      "UnitobsMetriques.csv"),
                                paste("\n   - plan d'échantillonnage basique (année - statut de protection) :",
                                      "PlanEchantillonnage_basique.csv"),
                                "\n\n", sep="")
                      }
                  },
                  "InfoRefSpeEnregistre"={
                      if (any(!is.element(c("file"),
                                          names(argsSup))))
                      {
                          stop("Arguments incorrects !")
                      }else{
                          paste("",
                                paste(rep("-", 100), collapse=""),
                                "Enregistrement des informations sur le référentiel espèce dans le fichier :",
                                paste("   ", argsSup$file, format(Sys.time(), "  (%d/%m/%Y\t%H:%M:%S)"), sep=""),
                                "\n", sep="\n")
                      }
                  },
                  "")

    ## Ajout du message :
    tkinsert(get("txt.w", envir=env), "end", msg)
    tkyview.moveto(get("txt.w", envir=env), 1)
}




