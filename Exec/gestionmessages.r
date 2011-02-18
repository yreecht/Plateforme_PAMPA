## le fichier gestionmessages.r a besoin du fichier config.r pour être executé
## les passages à la ligne se font à la fin des messages
## on retourne encore à la ligne avant une erreur

gestionMSGerreur.f <- function (nameerror, variable)
{
    runLog.f(msg=c("Envoie d'un message d'erreur dans l'interface :"))

    ## Message :
    MSG <-
        switch(nameerror,
               "recouvrementsansLIT"={
                   paste("Le champs obs$type n'est pas 'LIT', vous ne pouvez pas calculer",
                         " un % de recouvrement avec des espèces non benthiques\n", sep="")
               },
               "noWorkspace"={
                   "Aucun espace de travail n'est choisi ou opérationnel\n"
               },
               "nbChampUnitobs"={
                   paste("Votre fichier 'Unites d'observation' ne comporte pas le bon nombre de champs!",
                         " Il devrait en contenir 35. Corrigez le et recommencez l'importation.\n", sep="")
               },
               "nbChampUnitobs"={
                   paste("Votre fichier 'Unites d'observation' ne comporte pas le bon nombre de champs!",
                         " Il devrait en contenir 35. Corrigez le et recommencez l'importation.\n", sep="")
               },
               "nbChampEsp"={
                   paste("Votre fichier 'référentiel espèces' ne comporte pas le bon nombre de champs!",
                         " Il devrait en contenir 124. Corrigez le et recommencez l'importation.\n", sep="")
               },
               "nbChampObs"={
                   paste("Votre fichier 'observations' ne comporte pas le bon nombre de champs!",
                         " Il devrait en contenir 11 . Corrigez le et recommencez l'importation.\n", sep="")
               },
               "UnitobsDsObsUnitobs"={
                   paste("Votre fichier 'observations' ne comporte pas les mêmes unitobs que votre fichier",
                         " 'Unites d'observation'. Corrigez les et recommencez l'importation.\n", sep="")
               },
               "CaractereInterditDsObs"={
                   "Votre fichier observations contient des "&". Corrigez les et recommencez l'importation.\n"
               },
               "CaractereInterditDsUnitObs"={
                   "Votre fichier unités d'observations contient des . Corrigez les et recommencez l'importation.\n"
               },
               "ZeroEnregistrement"={
                   "Graphique impossible - pas d'enregistrements dans votre sélection.\n"
               },
               "UneSeuleValeurRegroupement"={
                   "Graphique sans intérêt - votre critère de regroupement n'a qu'une seule valeur.\n"
               },
               "CritereMalRenseigne50"={
                   paste("Graphique sans intérêt - le critère de regroupement sélectionné",
                         " est renseigné pour moins de 50% des observations.\n", sep="")
               },
               "CaractereInterdit"={
                   paste("Votre table ", variable,
                         " contient des caractères déconseillés par le référentiel des données ('espaces',',',';' ",
                         "\n-> vous devez corriger le problème pour ne pas rencontrer d'erreurs.\n", sep="")
               },
               ## Message par défaut :
               "message à renseigner")

    ## gestionMSGerreur.f(nbChampUnitobs)
    ## langue = EN

    tkinsert(helpframe, "end", paste("\nERROR : ", MSG, sep=""))
    tkyview.moveto(helpframe, 1)
}

gestionMSGaide.f <- function (namemsg)
{
    runLog.f(msg=c("Envoie d'un message d'aide dans l'interface :"))

    ## Message :
    MSG <-
        switch(namemsg,
               "ZeroEnregistrement"={
                   "! votre sélection ne contient plus d'enregistrement, veuillez recharger les données (CTRL + A)"
               },
               "etapeImport"={
                   "1 Choisissez c:/PAMPA comme dossier d'importation et de travail \n ou importez vos fichiers un à un"
               },
               "SelectionOuTraitement"={
                   "2 Vous pouvez restreindre votre sélection de données \n ou commencer les traitements standards"
               },
               "startsansficher"={
                   paste("Si les fichiers par défauts paramétrés dans 'config.r'- ", fileName1, " - ", fileName2, " - ",
                         fileName3, " ne sont pas les bons, Veuillez les modifier", sep="")
               },
               "etapeselected"={
                   "3 Vous pouvez retrouver l'ensemble des observations en les rechargeant (CTRL+A)"
               },
               "message à définir")

    tkinsert(helpframe, "end", paste("ETAPE : ", MSG, "\n", sep=""))
    tkyview.moveto(helpframe, 1)
}

gestionMSGinfo.f <- function (namemsg, parametrenum,...)
{
    runLog.f(msg=c("Envoie d'un message d'information dans l'interface :"))

    ## Message :
    MSG <-
        switch(namemsg,
               "plusieursAMP"={
                   paste("Votre fichier unités d'observations fait référence à plusieurs AMP!",
                         " Corrigez les et recommencez l'importation.\n", sep="")
               },
               "start"={
                   "Bienvenue sur l'interface TclTK de PAMPA\n"
               },
               "config"={
                   paste("Les fichiers par défauts paramétrés dans 'config.r' sont :",
                         "\n    - ", fileName1,
                         "\n    - ", fileName2,
                         "\n    - ", fileName3, "\n", sep="")
               },
               "BasetxtCreate"={
                   paste("Les fichiers .csv:",
                         "\n         - Contingence",
                         "\n         - PlanEchantillonnage",
                         "\n         - Metriques par unite d observation (UnitobsMetriques.csv)",
                         "\n         - Metriques par unite d observation pour les especes presentes",
                         " (ListeEspecesUnitobs.csv)",
                         "\n         - Metriques par unite d observation / espece (UnitobsEspeceMetriques.csv)",
                         "\n         - Metriques par unite d observation / espece / classe de taille",
                         " (UnitobsEspeceClassetailleMetriques.csv )",
                         "\nont ete crees\n", sep="")
               },
               "MSGnbgraphe"={
                   paste("Le nombre de cathégories de votre graphique étant trop important, celui ci a été découpé en ",
                         parametrenum, " parties\n")
               },
               "Familleselectionne"={
                   paste("Vous avez réduit les obsertations à la famille ", fa,
                         " et ", parametrenum, " enregistrements\n")
               },
               "Phylumselectionne"={
                   paste("Vous avez réduit les obsertations au phylum ", phy,
                         " et ", parametrenum, " enregistrements\n")
               },
               "Ordreselectionne"={
                   paste("Vous avez réduit les obsertations à l'ordre ", ord,
                         " et ", parametrenum, " enregistrements\n")
               },
               "Classeselectionne"={
                   paste("Vous avez réduit les obsertations à la classe ", cla,
                         " et ", parametrenum, " enregistrements\n")
               },
               "Biotopeselectionne"={
                   paste("Vous avez réduit les obsertations biotope ", biotopechoisi, " et ", parametrenum,
                         " enregistrements\n")
               },
               "Statutselectionne"={
                   paste("Vous avez réduit  les obsertations au statut ", statut, " et ", parametrenum,
                         " enregistrements\n")
               },
               "CatBenthselectionne"={
                   paste("Vous avez réduit  les obsertations à la categorie benthique ", selectcb,
                         " et ", parametrenum, " enregistrements\n", sep="")
               },
               "InfoRefSpeEnregistre"={
                   paste("Votre fichier d'information sur le référentiel espèce a été enregistré",
                         " au format CSV\n dans le dossier de travail sous le nom", parametrenum, ".\n")
               },
               "InfoPDFdansFichierSortie"={
                   paste("Vos Fichier regroupant tous les graphes par espèce",
                         " pour une métrique sont enregistrés dans FichiersSortie.\n", sep="")
               },
               "AucunPDFdansFichierSortie"={
                   paste("Aucun type de graphes par espèce pour une métrique n'est sélectionné.\n")
               },
               "Jeuxdedonnerestore"={
                   paste("Votre jeux de données a été restauré ainsi que les tables de métriques originales",
                         " \nAttention, pour restaurer les CSV initiaux, vous devez réimporter les données\n ",
                         parametrenum, " dans la table d'observation.\n", sep="")
               },
               "CalculSelectionFait"={
                   paste("Les métriques par unités d'observations ont",
                         " été recalculées sur le jeu de données sélectionnés", sep="")
               },
               "CalculTotalFait"={
                   paste("Les métriques par unités d'observations ont été calculées sur l'ensemble",
                         " du jeu de données importé", sep="")
               },
               "message à définir")

    tkinsert(txt.w, "end", paste("INFO : ", MSG, sep=""))
    ## tkset(scr, 0.999, 1)     # pour activer l'acensseur : activate, cget, configure, delta, fraction, get, identify,
    ## or set.
    tkyview.moveto(txt.w, 1) # bbox, cget, compare, configure, count, debug, delete, dlineinfo, dump, edit, get, image,
                             # index, insert, mark, peer, replace, scan, search, see, tag, window, xview, or yview.
}


