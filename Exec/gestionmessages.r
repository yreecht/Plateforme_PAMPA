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
                   paste("Le champ obs$type n'est pas 'LIT', vous ne pouvez pas calculer",
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
                   "Votre fichier observations contient des \"&\". Corrigez les et recommencez l'importation.\n"
               },
               "CaractereInterditDsUnitObs"={
                   "Votre fichier unités d'observations contient des \".\" Corrigez les et recommencez l'importation.\n"
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

    tkinsert(helpframe, "end", paste("\nERREUR : ", MSG, sep=""))
    tkyview.moveto(helpframe, 1)
}

gestionMSGaide.f <- function (namemsg)
{
    runLog.f(msg=c("Envoie d'un message d'aide dans l'interface :"))

    ## Message :
    MSG <-
        switch(namemsg,
               "ZeroEnregistrement"={
                   paste("! votre sélection ne contient plus d'enregistrement",
                         ", veuillez restaurer ou recharger les données (CTRL + a) !\n", sep="")
               },
               "etapeImport"={
                   paste("1 : Chargez les \"Dossiers et fichiers par défaut\" (CTRL + a)\n",
                         "\t ou choisissez les dossier/fichiers un à un (CTRL + n).\n",
                         "\t (également accessibles par le menu \"Données\")", sep="")
               },
               "SelectionOuTraitement"={
                   paste("2 : Vous pouvez restreindre votre sélection de données\n",
                         "\t (menu \"Sélection et recalcul\")\n",
                         "\t ou commencer les traitements standards.", sep="")
               },
               "startsansficher"={
                   paste("Si les fichiers par défauts paramétrés dans 'config.r'- ", fileName1, " - ", fileName2, " - ",
                         fileName3, " ne sont pas les bons, Veuillez les modifier\n", sep="")
               },
               "etapeselected"={
                   paste("3 : Vous pouvez retrouver l'ensemble des observations en",
                         "\n\t* les restaurant : menu \"Sélection et recalcul\" ou bouton en bas à gauche.",
                         "\n\t* les rechargeant (CTRL + a)",
                         sep="")
               },
               "message à définir")

    tkinsert(helpframe, "end", paste("\nETAPE ", MSG, "", sep=""))
    tkyview.moveto(helpframe, 1)
}

gestionMSGinfo.f <- function (namemsg, parametrenum,...)
{
    runLog.f(msg=c("Envoie d'un message d'information dans l'interface :"))

    ## Message :
    MSG <-
        switch(namemsg,
               "plusieursAMP"={
                   paste("Votre fichier d'unités d'observation fait référence à plusieurs AMP!",
                         " Corrigez le et recommencez l'importation.\n", sep="")
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
                         "\n         - Métriques par unité d'observation (UnitobsMetriques.csv)",
                         "\n         - Métriques par unité d'observation pour les espèces présentes",
                         " (ListeEspecesUnitobs.csv)",
                         "\n         - Métriques par unité d'observation / espèce (UnitobsEspeceMetriques.csv)",
                         "\n         - Métriques par unité d'observation / espèce / classe de taille",
                         " (UnitobsEspeceClassetailleMetriques.csv)",
                         "\nont été créés\n", sep="")
               },
               "MSGnbgraphe"={
                   paste("Le nombre de catégories de votre graphique étant trop important, celui ci a été découpé en ",
                         parametrenum, " parties\n")
               },
               "Familleselectionne"={
                   paste("Vous avez réduit les observations à la famille ", fa,
                         " et ", parametrenum, " enregistrements\n")
               },
               "Phylumselectionne"={
                   paste("Vous avez réduit les observations au phylum ", phy,
                         " et ", parametrenum, " enregistrements\n")
               },
               "Ordreselectionne"={
                   paste("Vous avez réduit les observations à l'ordre ", ord,
                         " et ", parametrenum, " enregistrements\n")
               },
               "Classeselectionne"={
                   paste("Vous avez réduit les observations à la classe ", cla,
                         " et ", parametrenum, " enregistrements\n")
               },
               "Biotopeselectionne"={
                   paste("Vous avez réduit les observations biotope ", biotopechoisi, " et ", parametrenum,
                         " enregistrements\n")
               },
               "Statutselectionne"={
                   paste("Vous avez réduit  les observations au statut ", statut, " et ", parametrenum,
                         " enregistrements\n")
               },
               "InfoRefSpeEnregistre"={
                   paste("Votre fichier d'information sur le référentiel espèce a été enregistré",
                         " au format CSV\n dans le dossier de travail sous le nom", parametrenum, ".\n")
               },
               "InfoPDFdansFichierSortie"={
                   paste("Vos Fichiers regroupant tous les graphiques par espèce",
                         " pour une métrique sont enregistrés dans FichiersSortie.\n", sep="")
               },
               "AucunPDFdansFichierSortie"={
                   paste("Aucun type de graphes par espèce pour une métrique n'est sélectionné.\n")
               },
               "Jeuxdedonnerestore"={
                   paste("Votre jeu de données a été restauré ainsi que les tables de métriques originales",
                         " \nAttention, pour restaurer les CSV initiaux, vous devez réimporter les données\n ",
                         parametrenum, " dans la table d'observation.\n", sep="")
               },
               "CalculSelectionFait"={
                   paste("Les métriques par unités d'observations ont",
                         " été recalculées sur le jeu de données sélectionné", sep="")
               },
               "CalculTotalFait"={
                   paste("Les métriques par unités d'observations ont été calculées sur l'ensemble",
                         " du jeu de données importé", sep="")
               },
               "message à définir")

    tkinsert(txt.w, "end", paste("\nINFO : ", MSG, sep=""))
    ## tkset(scr, 0.999, 1)     # pour activer l'acensseur : activate, cget, configure, delta, fraction, get, identify,
    ## or set.
    tkyview.moveto(txt.w, 1) # bbox, cget, compare, configure, count, debug, delete, dlineinfo, dump, edit, get, image,
                             # index, insert, mark, peer, replace, scan, search, see, tag, window, xview, or yview.
}


