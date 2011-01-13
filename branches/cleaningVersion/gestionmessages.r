## le fichier gestionmessages.r a besoin du fichier config.r pour être executé
## les passages à la ligne se font à la fin des messages
## on retourne encore à la ligne avant une erreur

gestionMSGerreur.f <- function (nameerror, variable)
{

    print("fonction gestionMSGerreur.f activée")

    ## langue = FR
    ## [!!!]: utiliser un switch à la place !!
    if (nameerror=="recouvrementsansLIT")
    {
        MGS <- "Le champs obs$type n'est pas 'LIT', vous ne pouvez pas calculer un % de recouvrement avec des espèces non benthiques\n"
    }
    if (nameerror=="noWorkspace")
    {
        MGS <- "Aucun espace de travail n'est choisi ou opérationnel\n"
    }
    if (nameerror=="nbChampUnitobs")
    {
        MGS <- "Votre fichier 'Unites d'observation' ne comporte pas le bon nombre de champs! Il devrait en contenir 35. Corrigez le et recommencez l'importation.\n"
    }
    if (nameerror=="nbChampEsp")
    {
        MGS <- "Votre fichier 'référentiel espèces' ne comporte pas le bon nombre de champs! Il devrait en contenir 124. Corrigez le et recommencez l'importation.\n"
    }
    if (nameerror=="nbChampObs")
    {
        MGS <- "Votre fichier 'observations' ne comporte pas le bon nombre de champs! Il devrait en contenir 11 . Corrigez le et recommencez l'importation.\n"
    }
    if (nameerror=="UnitobsDsObsUnitobs")
    {
        MGS <- "Votre fichier 'observations' ne comporte pas les mêmes unitobs que votre fichier 'Unites d'observation'. Corrigez les et recommencez l'importation.\n"
    }
    if (nameerror=="CaractereInterditDsObs")
    {
        MGS <- "Votre fichier observations contient des "&". Corrigez les et recommencez l'importation.\n"
    }
    if (nameerror=="CaractereInterditDsUnitObs")
    {
        MGS <- "Votre fichier unités d'observations contient des . Corrigez les et recommencez l'importation.\n"
    }
    if (nameerror=="ZeroEnregistrement")
    {
        MGS <- "Graphique impossible - pas d'enregistrements dans votre sélection.\n"
    }
    if (nameerror=="UneSeuleValeurRegroupement")
    {
        MGS <- "Graphique sans intérêt - votre critère de regroupement n'a qu'une seule valeur.\n"
    }
    if (nameerror=="CritereMalRenseigne50")
    {
        MGS <- "Graphique sans intérêt - le critère de regroupement sélectionné est renseigné pour moins de 50% des observations.\n"
    }
    if (nameerror=="CaractereInterdit")
    {
        MGS <- paste("Votre table", variable,
                     " contient des caractères non déconseillés par le référentiel des données ('espaces',',',';' ",
                     "\n-> vous devez corriger le problème pour ne pas rencontrer d'erreurs.\n")
    }
                                        # "non déconseillés" ? [yreecht: 21/07/2010]


    ## gestionMSGerreur.f(nbChampUnitobs)
    ## langue = EN

    tkinsert(helpframe, "end", paste("\nERROR : ", MGS, sep=""))
    tkyview.moveto(helpframe, 1)
}

## [sup] [yr: 13/01/2011]:

## gestionMSGmenus.f <- function (namemenu)
## {
## }

gestionMSGaide.f <- function (namemsg)
{

    print("fonction gestionMSGaide.f activée")

    MGS <- "message à renseigner"

    if (namemsg=="ZeroEnregistrement")
    {
        MGS <- "! votre sélection ne contient plus d'enregistrement, veuillez recharger les données (CTRL + A)"
    }
    if (namemsg=="etapeImport")
    {
        MGS <- "1 Choisissez c:/PAMPA comme dossier d'importation et de travail \n ou importez vos fichiers un à un"
    }
    if (namemsg=="SelectionOuTraitement")
    {
        MGS <- "2 Vous pouvez restreindre votre sélection de données \n ou commencer les traitements standards"
    }
    if (namemsg=="startsansficher")
    {
        MGS <- paste("Si les fichiers par défauts paramétrés dans 'config.r'- ", fileName1, " - ", fileName2, " - ",
                     fileName3, " ne sont pas les bons, Veuillez les modifier", sep="")
    }
    if (namemsg=="etapeselected")
    {
        MGS <- "3 Vous pouvez retrouver l'ensemble des observations en les rechargeant (CTRL+A)"
    }

    tkinsert(helpframe, "end", paste("ETAPE : ", MGS, "\n", sep=""))
    tkyview.moveto(helpframe, 1)
}

gestionMSGinfo.f <- function (namemsg, parametrenum,...)
{
    print("fonction gestionMSGinfo.f activée")

    MGS <- "message à renseigner"

    if (namemsg=="plusieursAMP")
    {
        MGS <- "Votre fichier unités d'observations fait référence à plusieurs AMP! Corrigez les et recommencez l'importation.\n"
    }
    if (namemsg=="start")
    {
        MGS <- "Bienvenue sur l'interface TclTK de PAMPA\n"
    }
    if (namemsg=="config")
    {
        MGS <- paste("Les fichiers par défauts paramétrés dans 'config.r' sont :
    - ", fileName1, "
    - ", fileName2, "
    - ", fileName3, "\n", sep="")
    }

    if (namemsg=="BasetxtCreate")
    {
        MGS <- "Les fichiers .csv:
         - Contingence
         - PlanEchantillonnage
         - Metriques par unite d observation (UnitobsMetriques.csv)
         - Metriques par unite d observation pour les especes presentes (ListeEspecesUnitobs.csv)
         - Metriques par unite d observation / espece (UnitobsEspeceMetriques.csv)
         - Metriques par unite d observation / espece / classe de taille (UnitobsEspeceClassetailleMetriques.csv )
         ont ete crees\n"
    }
    if (namemsg=="MSGnbgraphe")
    {
        MGS <- paste("Le nombre de cathégories de votre graphique étant trop important, celui ci a été découpé en ",
                     parametrenum, " parties\n")
    }
    if (namemsg=="Familleselectionne")
    {
        MGS <- paste("Vous avez réduit les obsertations à la famille ", fa, " et ", parametrenum, " enregistrements\n")
    }
    if (namemsg=="Phylumselectionne")
    {
        MGS <- paste("Vous avez réduit les obsertations au phylum ", phy, " et ", parametrenum, " enregistrements\n")
    }
    if (namemsg=="Ordreselectionne")
    {
        MGS <- paste("Vous avez réduit les obsertations à l'ordre ", ord, " et ", parametrenum, " enregistrements\n")
    }
    if (namemsg=="Classeselectionne")
    {
        MGS <- paste("Vous avez réduit les obsertations à la classe ", cla, " et ", parametrenum, " enregistrements\n")
    }
    if (namemsg=="Biotopeselectionne")
    {
        MGS <- paste("Vous avez réduit les obsertations biotope ", biotopechoisi, " et ", parametrenum,
                     " enregistrements\n")
    }
    if (namemsg=="Statutselectionne")
    {
        MGS <- paste("Vous avez réduit  les obsertations au statut ", statut, " et ", parametrenum,
                     " enregistrements\n")
    }
    if (namemsg=="CatBenthselectionne")
    {
        MGS <- paste("Vous avez réduit  les obsertations à la categorie benthique ", selectcb, " et ", parametrenum,
                     " enregistrements\n", sep="")
    }
    if (namemsg=="InfoRefSpeEnregistre")
    {
        MGS <- paste("Votre fichier d'information sur le référentiel espèce a été enregistré",
                     " au format CSV\n dans le dossier de travail sous le nom", parametrenum, ".\n")
    }
    if (namemsg=="InfoPDFdansFichierSortie")
    {
        MGS <- paste("Vos Fichier regroupant tous les graphes par espèce",
                     " pour une métrique sont enregistrés dans FichiersSortie.\n", sep="")
    }
    if (namemsg=="AucunPDFdansFichierSortie")
    {
        MGS <- paste("Aucun type de graphes par espèce pour une métrique n'est sélectionné.\n")
    }
    if (namemsg=="Jeuxdedonnerestore")
    {
        MGS <- paste("Votre jeux de données a été restauré ainsi que les tables de métriques originales",
                     " \nAttention, pour restaurer les CSV initiaux, vous devez réimporter les données\n ",
                     parametrenum, " dans la table d'observation.\n", sep="")
    }
    if (namemsg=="CalculSelectionFait")
    {
        MGS <- paste("Les métriques par unités d'observations ont été recalculées sur le jeu de données sélectionnés")
    }
    if (namemsg=="CalculTotalFait")
    {
        MGS <- paste("Les métriques par unités d'observations ont été calculées sur l'ensemble",
                     " du jeu de données importé", sep="")
    }
    tkinsert(txt.w, "end", paste("INFO : ", MGS, sep=""))
    ## tkset(scr, 0.999, 1)     # pour activer l'acensseur : activate, cget, configure, delta, fraction, get, identify,
    ## or set.
    tkyview.moveto(txt.w, 1) # bbox, cget, compare, configure, count, debug, delete, dlineinfo, dump, edit, get, image,
                             # index, insert, mark, peer, replace, scan, search, see, tag, window, xview, or yview.
}

## [sup] [yr: 13/01/2011]:

## gestionMSGchoix <- function(title, question, valeurdef, Largeurchamp=5, returnValOnCancel="ID_CANCEL")
## {
##     print("fonction gestionMSGchoix.f activée")
##     dlg <- tktoplevel()
##     tkwm.deiconify(dlg)
##     tkgrab.set(dlg)
##     tkfocus(dlg)
##     tkwm.title(dlg, title)
##     MSGchoixVarTcl <- tclVar(paste(valeurdef))
##     MSGchoixWidget <- tkentry(dlg, width=paste(Largeurchamp), textvariable=MSGchoixVarTcl)
##     tkgrid(tklabel(dlg, text="       "))
##     tkgrid(tklabel(dlg, text=question), MSGchoixWidget)
##     tkgrid(tklabel(dlg, text="       "))
##     ReturnVal <- returnValOnCancel
##     onOK <- function()
##     {
##         ReturnVal <<- tclvalue(MSGchoixVarTcl)
##         tkgrab.release(dlg)
##         tkdestroy(dlg)
##         tkfocus(tm)
##     }
##     onCancel <- function()
##     {
##         ReturnVal <<- returnValOnCancel
##         tkgrab.release(dlg)
##         tkdestroy(dlg)
##         tkfocus(tm)
##     }
##     OK.but     <-tkbutton(dlg, text="   OK   ", command=onOK)
##     Cancel.but <-tkbutton(dlg, text=" Cancel ", command=onCancel)
##     tkgrid(OK.but, Cancel.but)
##     tkgrid(tklabel(dlg, text="    "))

##     tkfocus(dlg)
##     tkbind(dlg, "<Destroy>", function() {tkgrab.release(dlg);tkfocus(tm)})
##     tkbind(MSGchoixWidget, "<Return>", onOK)
##     tkwait.window(dlg)

##     return(ReturnVal)
## }

## [sup] [yr: 13/01/2011]:

## aide.f <- function()
## {

##     require(tcltk) || stop("Package tcltk is not available.") # Add path to BWidgets
##     addTclPath(".")
##     version.BWidget <<- tclvalue(tclRequire("BWidget"))

##     print("fonction aide.f activée")
##     tmaide <- tktoplevel()
##     tkwm.title(tmaide, "aide de l'utilisateur de l'interface PAMPA")
##     tn <- tkwidget(tmaide, "ttk::notebook")
##     tkgrid(tn, sticky="news")

##     tbn <- tclvalue(tkadd(tn, label="1"))
##     tkgrid(tbw <- .Tk.newwin(tbn))
##     tkgrid(fr <- tkframe(tbw))
##     tkgrid(lb <- tklabel(fr, text=paste("This is tab", "1")))
##     ID <- paste(tn$ID, evalq(num.subwin <- num.subwin+1, tn$env), sep=".")
##     win <- .Tk.newwin(ID)
##     assign(ID, tbw, envir = tn$env)
##     assign("parent", tn, envir = tbw$env)

##     tbn <- tclvalue(tkadd(tn, label="2"))
##     tkgrid(tbw <- .Tk.newwin(tbn))
##     tkgrid(fr <- tkframe(tbw))
##     tkgrid(lb <- tklabel(fr, text=paste("This is tab", "2")))

##     ## list(tbw, fr, lb) # return all three in case you need them later
## }

## tcl(tn, "raise", "text2")

## gestion des doubles et triples regroupement impossibles pour chaque champ
