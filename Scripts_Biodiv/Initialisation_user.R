#-*- coding: latin-1 -*-
# Time-stamp: <2020-06-05 15:30:20 a23579>

## Plateforme PAMPA de calcul d'indicateurs de ressources & biodiversit�
##   Copyright (C) 2008-2020 Ifremer - Tous droits r�serv�s.
##
##   Ce programme est un logiciel libre ; vous pouvez le redistribuer ou le
##   modifier suivant les termes de la "GNU General Public License" telle que
##   publi�e par la Free Software Foundation : soit la version 2 de cette
##   licence, soit (� votre gr�) toute version ult�rieure.
##
##   Ce programme est distribu� dans l'espoir qu'il vous sera utile, mais SANS
##   AUCUNE GARANTIE : sans m�me la garantie implicite de COMMERCIALISABILIT�
##   ni d'AD�QUATION � UN OBJECTIF PARTICULIER. Consultez la Licence G�n�rale
##   Publique GNU pour plus de d�tails.
##
##   Vous devriez avoir re�u une copie de la Licence G�n�rale Publique GNU avec
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
    ## ##################################################
    ## Apply for new user or new PAMPA_HOME directory
    ## (transition).

    dir.create(path = ConfigDir, recursive = TRUE)

    ## Copy pre-existing or template config file to the new home (transition):
    file.copy(from = file.path(basePath, "Scripts_Biodiv/Config.R"),
              to = file.path(ConfigDir, "Config.R"))

    ignoredDirs <- c(".git", "branches", "Donnees_Usages", "Exec",
                     "img", "Img", "logs", "Resultats_Usages",
                     "Scripts_Biodiv", "Scripts_communs", "Scripts_Usages", "Travail_Images")

    copiedDirs <- basename(list.dirs(basePath, recursive = FALSE))
    copiedDirs <- copiedDirs[! copiedDirs %in% ignoredDirs]

    ## Copy existing directories (alledgedly WDs) to the new home (transition):
    file.copy(from = file.path(basePath, copiedDirs),
              to = paste0(PAMPAhome, "/"),
              recursive = TRUE, copy.date = TRUE)

    ## Create the WD template structure (as named in the default Config.R)
    dir.create(file.path(PAMPAhome, "Dummy_WD/Data"), recursive = TRUE)
}



### Local Variables:
### ispell-local-dictionary: "english"
### fill-column: 100
### End:
