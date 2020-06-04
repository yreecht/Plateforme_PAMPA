#-*- coding: latin-1 -*-
# Time-stamp: <2020-06-04 17:27:30 a23579>

## Plateforme PAMPA de calcul d'indicateurs de ressources & biodiversité
##   Copyright (C) 2008-2020 Ifremer - Tous droits réservés.
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

### File: Initialisation_user.R
### Created: <2020-06-04 17:24:16 a23579>
###
### Created: 04/06/2020	15:55:39
### Author: Yves Reecht
###
####################################################################################################
### Description:
###
###
####################################################################################################


PAMPAhome <- ifelse(Sys.getenv("PAMPA_HOME") == "",
                    normalizePath(file.path(Sys.getenv("HOME"),
                                            "PAMPA", fsep = "/"),
                                  winslash = "/", mustWork = FALSE),
                    normalizePath(Sys.getenv("PAMPA_HOME"),
                                  winslash = "/", mustWork = FALSE))

ConfigDir <- file.path(PAMPAhome, "config")


## Create directory and copy files if does not exist yet:
if (! dir.exists(ConfigDir))
{
    dir.create(path = , recursive = TRUE)

    file.copy(from = file.path(basePath, "Scripts_Biodiv/Config.R"),
              to = file.path(ConfigDir, "Config.R"))

    ignoredDirs <- c(".git", "branches", "Donnees_Usages", "Exec",
                     "img", "Img", "logs", "Resultats_Usages",
                     "Scripts_Biodiv", "Scripts_communs", "Scripts_Usages", "Travail_Images")

    copiedDirs <- basename(list.dirs(basePath, recursive = FALSE))
    copiedDirs <- copiedDirs[! copiedDirs %in% ignoredDirs]

    file.copy(from = file.path(basePath, copiedDirs),
              to = paste0(PAMPAhome, "/"),
              recursive = TRUE, copy.date = TRUE)
}



### Local Variables:
### ispell-local-dictionary: "english"
### fill-column: 100
### End:
