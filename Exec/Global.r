################################################################################
## Nom                  : Global.r
## Objet                : Programme de calcul des métriques biodiversité et ressources
## Input                : TXT
## Output               : CSV
## R version            : 2.10.1
## Date de création     : Février 2008
## Date de modification : Avril 2010
################################################################################

## Réglage de l'encodage des caractères :
options(encoding="latin1")

## Platform-specific treatment:
if (.Platform$OS.type == "windows")
{
    setwd("C:/PAMPA/")
}else{
    setwd("/media/ifremer/PAMPA/Scripts/latest/")
}
basePath <- getwd()

## Répertoire de travail par défaut (si pas configuré par ailleurs) :
nameWorkspace <- basePath

## ! suggestions concernant la réorganisation :

## ! -> un fichier global (executable du programme, appelle chaque fonction)
## ! -> ajouter un fichier "test des champs"
## ! -> rajouter un message signalant que l'import des données a réussi pour chaque fichier
## ! -> un fichier stats de vérification des données
## ! -> un fichier avec les différents modes de calcul des classes de taille (cf WP2/CalculClassesDeTaille.doc)
## ! -> Un fichier calcul metrique base, un autre pour les métriques avancées, avec pour chaque, les noms de variable = contenu menu
## ! -> un fichier analyses ou un fichier par type d'analyse
## ! -> un fichier pour les groupements par classe et préparation de calculs (tri, création d'objets pour les calculs)
## ! -> un fichier data_temp pour ne pas écraser les fichiers chargés à la base
## ! -> un fichier interface
## ! -> un fichier affichage graphique,

## !amélioration des commentaires : en entête de chaque fichier, un plan du contenu
## !améliorer les messages d'alerte, en mettre dés qu'il y a des saisies et après contrôle de saisie
## !création automatique des dossiers et appel de l'ensemble des fichiers une fois l'espace de travail choisi

## ! difficulté possible : appel de fonctions graphiques dans les fonctions de calcul pour extraire les fonctions graph...
## ! les tests de variables "par valeur" (classetaille, nom d'amp...) ne doivent pas être dans les fonctions génériques

## ######################### Chargement des librairies ############################
## ! conseillé : que la version de R et les packages soient fournis avec toute mise à jour


## !Messages d'avis :
## !1: le package 'vegan' a été compilé sous R version 2.7.2 et l'aide ne fonctionnera pas correctement
## !Veuillez le réinstaller, s'il-vous-plait
## !2: le package 'maptools' a été compilé sous R version 2.7.2 et l'aide ne fonctionnera pas correctement
## !Veuillez le réinstaller, s'il-vous-plait
## !3: le package 'sp' a été compilé sous R version 2.7.2 et l'aide ne fonctionnera pas correctement
## !Veuillez le réinstaller, s'il-vous-plait

                                                           # Mise en forme du code :
                                                           # -----------------------
source("./Exec/load_packages.R", encoding="latin1")
source("./Exec/config.r", encoding="latin1")               # faite
source("./Exec/gestionmessages.r", encoding="latin1")      # faite
source("./Exec/mkfilegroupe.r", encoding="latin1")         # faite

source("./Exec/graphique.r", encoding="latin1")            # faite [sll]
source("./Exec/graphique_benthos.r", encoding="latin1")
source("./Exec/calcul_simple.r", encoding="latin1")        # faite [sll]
source("./Exec/arbre_regression.r", encoding="latin1")     # fai.. [sll]
source("./Exec/anova.r", encoding="latin1")


source("./Exec/requetes.r", encoding="latin1")
source("./Exec/modifinterface.r", encoding="latin1")
source("./Exec/command.r", encoding="latin1")              # faite


source("./Exec/testfichier.r", encoding="latin1")
source("./Exec/view.r", encoding="latin1")                 # faite
source("./Exec/import.r", encoding="latin1")
source("./Exec/importdefaut.r", encoding="latin1")         # faite [sll]

source("./Exec/interface_fonctions.R", encoding="latin1")
source("./Exec/interface.r", encoding="latin1")

##################################################
## Nouvelle interface de sélection des variables :
source("./Exec/selection_variables_fonctions.R", encoding="latin1")
source("./Exec/selection_variables_interface.R", encoding="latin1")

##################################################
## Nouveaux boxplots :
source("./Exec/boxplot_generique_calc.R", encoding="latin1")

##################################################
## Analyses statistiques :
source("./Exec/modeles_lineaires_interface.R", encoding="latin1")
source("./Exec/modeles_lineaires_generique.R", encoding="latin1")


tkfocus(tm)

#################### Tags de développement ####################
## [!!!] : construction dangereuse, capilo-tractée ou erreur possible.
## [imb] : fonctions imbriquées dans d'autres fonctions (à corriger)
## [sll] : sans longueur des lignes (mise en forme du code pas terminée)
## [inc] : expression/fonction incomplète.
## [OK]  : problème corrigé.
## [???] : comprend pas !
