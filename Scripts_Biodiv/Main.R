#-*- coding: latin-1 -*-
# Time-stamp: <2020-06-05 12:26:36 a23579>

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
options(versionPAMPA = "3.1-alpha1",
        defaultLang = "en")

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
    if (grepl("[\\/]Scripts_Biodiv$", normalizePath(getwd())))
    {
        setwd("..")
    }
    ## message("Dossier non-trouvé")
    if (.Platform$OS.type == "windows")
    {
        ## setwd("C:/PAMPA/")   # source of problems if anything else
    }else{} # Rien !
}

## Récupéré dans une variable globale (beurk !) :
basePath <- getwd()

## Initialisation of user specific paths:
source(file.path(basePath, "Scripts_Biodiv/Initialisation_user.R"), encoding = "latin1")

## Load configuration:
.Config <- parse(file.path(ConfigDir, "Config.R"), encoding = "latin1")

## Répertoire de travail:
if (length(idxWD <- grep("^[[:blank:]]*nameWorkspace[[:blank:]]*(<-|=)", .Config)))
{
    eval(.Config[[idxWD]])
}else{
    ## ...par défaut (si pas configuré par ailleurs) :
    nameWorkspace <- PAMPAhome
}

## #############################################################################################################
## Chargement des fonctions de la plateforme pour :
                                                                                       # Code formating
                                                                                       # [ML status]:
## ...les fonctions communes de base :                                                 # -----------------------
source("./Scripts_Biodiv/Functions_Multilingual.R", encoding="latin1")                 # [mli]
source("./Scripts_Biodiv/Functions_base.R", encoding="latin1")                         # OK [mld]


########################################################################################################################
## Configuration :
source("./Scripts_Biodiv/Initialisation.R", encoding="latin1")

## Load user-defined options - Run 1 (language option):
if (is.null(getOption("PAMPAdummy")))   # uniquement si pas déjà initialisées (cas de lancement multiple)
{
    ## Index of
    idxOpt <- grep("^[[:blank:]]*options[[:blank:]]*\\(", .Config)

    ## Evaluate the options a first time
    ## (some - like the language options -
    ##  are persistent during the initialization):
    eval(.Config[idxOpt])

    if (is.null(getOption("P.GUIlang")))
    {
        options("P.GUIlang" = getOption("defaultLang"))
    }
}

## Loading and - if needed - installing packages:
source("./Scripts_Biodiv/Load_packages.R", encoding="latin1")                          # OK [mld]


## ...la création de l'interface :
source("./Scripts_Biodiv/Interface_functions.R", encoding="latin1")                    # OK [mld]
source("./Scripts_Biodiv/Interface_main.R", encoding="latin1")                         # OK [mld]

## anciennes fonctions annexes de visualisation des données (corrigées) :
source("./Scripts_Biodiv/Messages_management.R", encoding="latin1")                    # done [mld]
source("./Scripts_Biodiv/Test_files.R", encoding="latin1")                             # done [mld]
source("./Scripts_Biodiv/View.R", encoding="latin1")                                   # done [mld]

## ...le chargement des données :
source("./Scripts_Biodiv/Load_files.R", encoding="latin1")                             # OK [mld]
source("./Scripts_Biodiv/Load_files_manually.R", encoding="latin1")                    # OK [mld]
source("./Scripts_Biodiv/Weight_calculation.R", encoding="latin1")                     # OK [mld]
source("./Scripts_Biodiv/Link_unitobs-refspa.R", encoding="latin1")                    # OK [mld]
source("./Scripts_Biodiv/Load_shapefile.R", encoding="latin1")                         # OK [mld]
source("./Scripts_Biodiv/Load_OBSIND.R", encoding="latin1")                            # OK [mli]

## ...les calculs de tables de métriques :
source("./Scripts_Biodiv/Generic_aggregations.R", encoding="latin1")                   # OK [mld]
source("./Scripts_Biodiv/Calculate_metrics_tables.R", encoding="latin1")               # OK [mld]
source("./Scripts_Biodiv/Calculate_metrics_tables_LIT.R", encoding="latin1")           # OK [mli]
source("./Scripts_Biodiv/Calculate_metrics_tables_SVR.R", encoding="latin1")           # OK [mld]
source("./Scripts_Biodiv/Turtle_tracks.R", encoding="latin1")                          # OK [mld]

## ...la sélection des données :
source("./Scripts_Biodiv/Data_subsets.R", encoding="latin1")                           # OK [mld]

## ...options graphiques et générales :
source("./Scripts_Biodiv/Options.R", encoding="latin1")                                # OK [mld]

##################################################
## Analyses et graphiques :

## ...l'interface de sélection des variables :
source("./Scripts_Biodiv/Variables_selection_functions.R", encoding="latin1")          # OK [mld]
source("./Scripts_Biodiv/Variables_selection_interface.R", encoding="latin1")          # OK [mld]

## ...la création de boxplots (...) :
source("./Scripts_Biodiv/Functions_graphics.R", encoding="latin1")                     # OK [mld]
source("./Scripts_Biodiv/Boxplots_generic_sp.R", encoding="latin1")                    # OK [mld]
source("./Scripts_Biodiv/Boxplots_generic_unitobs.R", encoding="latin1")               # OK [mld]
## ...dont cartes :
source("./Scripts_Biodiv/Maps_graphics.R", encoding="latin1")                          # [mld]
source("./Scripts_Biodiv/Maps_variables.R", encoding="latin1")                         # [mld]

## ...les analyses statistiques :
source("./Scripts_Biodiv/Linear_models_interface.R", encoding="latin1")                # OK [mld]
source("./Scripts_Biodiv/Linear_models_generic_sp.R", encoding="latin1")               # OK [mld]
source("./Scripts_Biodiv/Linear_models_generic_unitobs.R", encoding="latin1")          # OK [mld]
source("./Scripts_Biodiv/MRT_generic_unitobs.R", encoding="latin1")                    # OK [mld]
source("./Scripts_Biodiv/MRT_generic_sp.R", encoding="latin1")                         # OK [mld]

## ...les barplots sur les fréquences d'occurrence :
source("./Scripts_Biodiv/Barplots_occurrence.R", encoding="latin1")                    # OK [mld]
source("./Scripts_Biodiv/Barplots_occurrence_unitobs.R", encoding="latin1")            # OK [mld]

## ...barplots génériques :
source("./Scripts_Biodiv/Barplots_generic_sp.R", encoding="latin1")                    # OK [mld]
source("./Scripts_Biodiv/Barplots_generic_unitobs.R", encoding="latin1")               # OK [mld]

## Initialization of options (new ~ persistent system) - Run 2 (all other options):
if (is.null(getOption("PAMPAdummy")))   # uniquement si pas déjà initialisées (cas de lancement multiple)
{
    ## Index of
    idxOpt <- grep("^[[:blank:]]*options[[:blank:]]*\\(", .Config)

    ## Evaluate the options a first time
    ## (some - like the language options -
    ##  are persistent during the initialization):
    eval(.Config[idxOpt])

    initialiseOptions.f()

    ## Override the non-persistent options after initialization:
    eval(.Config[idxOpt])
}

## options("P.GUIlang" = "fr")
## options(error = recover)
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

## Translation tags:
## [mli]: irrelevant
## [mlo]: ongoing
## [mld]: Done


### Local Variables:
### ispell-local-dictionary: "english"
### fill-column: 120
### End:
