#-*- coding: latin-1 -*-
# Time-stamp: <2013-02-04 14:43:59 Yves>

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

### File: Main.R
### Created: <2012-02-24 20:36:47 Yves>
###
### Author: Yves Reecht
###
####################################################################################################
### Description:
###
### Objet            : Programme de calcul des métriques "ressources & biodiversité".
### Date de création : Février 2008
###
####################################################################################################

## ** Version **
options(versionPAMPA = "2.7-tortues")

## Platform-specific treatment:
## Identification du dossier parent (d'installation) :
fileCall <- sub("source\\([[:blank:]]*(file[[:blank:]]*=[[:blank:]]*)?(\"|\')([^\"\']*)(\"|\')[[:blank:]]*(,.*\\)|\\))",
                "\\3",
                paste(deparse(tryCatch(sys.call(-2),
                                       error=function(e) {NULL})),
                      collapse=""))

## Réglage du dossier de travail de R :
if(basename(fileCall) == "Main.R")
{
    setwd(paste(dirname(fileCall), "/../", sep=""))
}else{
    ## message("Dossier non-trouvé")
    if (.Platform$OS.type == "windows")
    {
        setwd("C:/PAMPA/")
    }else{}                             # Rien !
}

## Récupéré dans une variable globale (beurk !) :
basePath <- getwd()

## Répertoire de travail par défaut (si pas configuré par ailleurs) :
nameWorkspace <- basePath

########################################################################################################################
## Chargement des fonctions de la plateforme pour :
                                                                                       # Mise en forme du code :
## ...les fonctions communes de base :                                                 # -----------------------
source("./Scripts_Biodiv/Load_packages.R", encoding="latin1")                          # OK
source("./Scripts_Biodiv/Fonctions_base.R", encoding="latin1")                         # OK

## ...la création de l'interface :
source("./Scripts_Biodiv/Interface_fonctions.R", encoding="latin1")                    # OK
source("./Scripts_Biodiv/Interface_principale.R", encoding="latin1")                   # OK

## anciennes fonctions annexes de visualisation des données (corrigées) :
source("./Scripts_Biodiv/Gestionmessages.R", encoding="latin1")                        # faite
source("./Scripts_Biodiv/Testfichier.R", encoding="latin1")                            # faite
source("./Scripts_Biodiv/View.R", encoding="latin1")                                   # faite

## ...le chargement des données :
source("./Scripts_Biodiv/Chargement_fichiers.R", encoding="latin1")                    # OK
source("./Scripts_Biodiv/Chargement_manuel_fichiers.R", encoding="latin1")             # OK
source("./Scripts_Biodiv/Calcul_poids.R", encoding="latin1")                           # OK
source("./Scripts_Biodiv/Lien_unitobs-refspa.R", encoding="latin1")                    # OK
source("./Scripts_Biodiv/Chargement_shapefile.R", encoding="latin1")                   # OK

## ...les calculs de tables de métriques :
source("./Scripts_Biodiv/Agregations_generiques.R", encoding="latin1")                 # OK
source("./Scripts_Biodiv/Calcul_tables_metriques.R", encoding="latin1")                # OK
source("./Scripts_Biodiv/Calcul_tables_metriques_LIT.R", encoding="latin1")            # OK
source("./Scripts_Biodiv/Calcul_tables_metriques_SVR.R", encoding="latin1")            # OK

## ...la sélection des données :
source("./Scripts_Biodiv/Selection_donnees.R", encoding="latin1")                      # OK

## ...options graphiques et générales :
source("./Scripts_Biodiv/Options.R", encoding="latin1")                                # OK

##################################################
## Analyses et graphiques :

## ...l'interface de sélection des variables :
source("./Scripts_Biodiv/Selection_variables_fonctions.R", encoding="latin1")          # OK
source("./Scripts_Biodiv/Selection_variables_interface.R", encoding="latin1")          # OK

## ...la création de boxplots (...) :
source("./Scripts_Biodiv/Fonctions_graphiques.R", encoding="latin1")                   # OK
source("./Scripts_Biodiv/Boxplots_esp_generiques.R", encoding="latin1")                # OK
source("./Scripts_Biodiv/Boxplots_unitobs_generiques.R", encoding="latin1")            # OK
## ...dont démonstartions sur des cartes :
source("./Scripts_Biodiv/Demo_cartes.R", encoding="latin1")                            # OK

## ...les analyses statistiques :
source("./Scripts_Biodiv/Modeles_lineaires_interface.R", encoding="latin1")            # OK
source("./Scripts_Biodiv/Modeles_lineaires_esp_generiques.R", encoding="latin1")       # OK
source("./Scripts_Biodiv/Modeles_lineaires_unitobs_generiques.R", encoding="latin1")   # OK
source("./Scripts_Biodiv/Arbres_regression_unitobs_generiques.R", encoding="latin1")   # OK
source("./Scripts_Biodiv/Arbres_regression_esp_generiques.R", encoding="latin1")       # OK

## ...les barplots sur les fréquences d'occurrence :
source("./Scripts_Biodiv/Barplots_occurrence.R", encoding="latin1")                    # OK
source("./Scripts_Biodiv/Barplots_occurrence_unitobs.R", encoding="latin1")            # OK

## ...barplots génériques :
source("./Scripts_Biodiv/Barplots_esp_generiques.R", encoding="latin1")                # OK
source("./Scripts_Biodiv/Barplots_unitobs_generiques.R", encoding="latin1")            # OK

########################################################################################################################
## Configuration :
source("./Scripts_Biodiv/Initialisation.R", encoding="latin1")

## Initialisation des options graphiques (nouveau système) :
if (is.null(getOption("GraphPAMPA")))   # uniquement si pas déjà initialisées (cas de lancement multiple)
{
    initialiseOptions.f()
}

## On lance l'interface :
mainInterface.create.f()

#################### Tags de développement ####################
## [!!!] : construction dangereuse, capilo-tractée ou erreur possible.
## [imb] : fonctions imbriquées dans d'autres fonctions (à corriger)
## [sll] : sans longueur des lignes (mise en forme du code pas terminée)
## [inc] : expression/fonction incomplète.
## [OK]  : problème corrigé.
## [???] : comprend pas !
## [sup] : supprimé.
## [dep] : déplacé (menu).


### Local Variables:
### ispell-local-dictionary: "english"
### fill-column: 120
### End:
