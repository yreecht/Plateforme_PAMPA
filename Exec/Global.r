################################################################################
## Nom                  : Global.r
## Objet                : Programme de calcul des métriques biodiversité et ressources
## Input                : TXT
## Output               : CSV
## R version            : 2.10.1
## Date de création     : Février 2008
## Date de modification : Janvier 2011
################################################################################

## ** Version **
options(versionPAMPA = "1.0-alpha-7")

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

## ! -> ajouter un fichier "test des champs"
## ! -> un fichier stats de vérification des données
## ! -> un fichier avec les différents modes de calcul des classes de taille (cf WP2/CalculClassesDeTaille.doc)

## !amélioration des commentaires : en entête de chaque fichier, un plan du contenu
## !améliorer les messages d'alerte, en mettre dés qu'il y a des saisies et après contrôle de saisie
## !création automatique des dossiers et appel de l'ensemble des fichiers une fois l'espace de travail choisi

## ! difficulté possible : appel de fonctions graphiques dans les fonctions de calcul pour extraire les fonctions graph...
## ! les tests de variables "par valeur" (classetaille, nom d'amp...) ne doivent pas être dans les fonctions génériques

## ######################### Chargement des librairies ############################
## ! conseillé : que la version de R et les packages soient fournis avec toute mise à jour


## !Messages d'avis :

                                                           # Mise en forme du code :
                                                           # -----------------------
source("./Exec/load_packages.R", encoding="latin1")        # OK
source("./Exec/fonctions_base.R", encoding="latin1")       # OK
source("./Exec/config.r", encoding="latin1")               # faite
source("./Exec/gestionmessages.r", encoding="latin1")      # faite
source("./Exec/nombres_SVR.R", encoding="latin1")          # OK
source("./Exec/mkfilegroupe.r", encoding="latin1")         # faite

source("./Exec/calcul_simple.r", encoding="latin1")        # faite
source("./Exec/arbre_regression.r", encoding="latin1")     # faite


source("./Exec/requetes.r", encoding="latin1")             # faite
source("./Exec/modifinterface.r", encoding="latin1")       # faite
source("./Exec/command.r", encoding="latin1")              # faite


source("./Exec/testfichier.r", encoding="latin1")          # faite
source("./Exec/view.r", encoding="latin1")                 # faite
source("./Exec/import.r", encoding="latin1")               # faite
source("./Exec/importdefaut.r", encoding="latin1")         # faite

##################################################
## Nouvelle interface de sélection des variables :
source("./Exec/selection_variables_fonctions.R", encoding="latin1")        # OK
source("./Exec/selection_variables_interface.R", encoding="latin1")        # OK

##################################################
## Nouveaux boxplots :
source("./Exec/fonctions_graphiques.R", encoding="latin1")                 # OK
source("./Exec/boxplots_esp_generiques.R", encoding="latin1")              # OK
source("./Exec/boxplots_unitobs_generiques.R", encoding="latin1")          # OK

##################################################
## Analyses statistiques :
source("./Exec/modeles_lineaires_interface.R", encoding="latin1")          # OK
source("./Exec/modeles_lineaires_esp_generiques.R", encoding="latin1")     # OK
source("./Exec/modeles_lineaires_unitobs_generiques.R", encoding="latin1") # OK

##################################################
## Barplots sur les fréquences d'occurrence :
source("./Exec/barplots_occurrence.R", encoding="latin1")                  # OK
source("./Exec/barplots_occurrence_unitobs.R", encoding="latin1")          # OK

## On lance l'interface :
source("./Exec/interface_fonctions.R", encoding="latin1")  # OK
source("./Exec/interface.r", encoding="latin1")            # faite

tkfocus(tm)

#################### Tags de développement ####################
## [!!!] : construction dangereuse, capilo-tractée ou erreur possible.
## [imb] : fonctions imbriquées dans d'autres fonctions (à corriger)
## [sll] : sans longueur des lignes (mise en forme du code pas terminée)
## [inc] : expression/fonction incomplète.
## [OK]  : problème corrigé.
## [???] : comprend pas !
## [sup] : supprimé.
## [dep] : déplacé (menu).
