#-*- coding: latin-1 -*-
# Time-stamp: <2018-12-09 16:23:55 yreecht>

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

### File: Initialisation.R
### Created: <2012-01-15 20:35:45 yves>
###
### Author: Yves Reecht
###
####################################################################################################
### Description:
###
### Script destiné à recevoir les initialisations précédemment faites dans Config.R
####################################################################################################

## Variables de fichiers requises:
options(P.requiredVar=c(unitobs="fileNameUnitobs",
                        obs="fileNameObs",
                        refesp="fileNameRefesp",
                        ws="nameWorkspace"))

## Options du référentiel spatial :
options(P.linkUnitobs="site",
        P.linkRefspa="CODE.SITE",
        P.shapefileEncoding="latin1",
        P.landField="HABITAT1",         # Champs du référentiel spatial permettant d'identifier la terre...
        P.landMods=c("terre", "ilot"),  # ...modalités de ce champs correspondant à la terre.
        P.landCols=c(terre="chocolate3",  mer="powderblue"), # couleurs terre/mer.
        P.pinSubplot=c(2.0, 1.8))      # dimensions (en pouces/inches) des sous-graphiques pour représentation sur des
                                        # cartes.
        ## P.landCols=c(terre="saddlebrown", mer="steelblue"))

## Option de noms de champs :
options(P.MPAfield="cas.etude")

## ##################### Initialisation des (rares) variables globales ####################

#### Logo :
.fileimage <- "./Scripts_Biodiv/img/pampa2.GIF"
assign(".fileimage", .fileimage, envir=.GlobalEnv)

#### Image de lien de tables :
.fileimageLink <- "./Scripts_Biodiv/img/tableLink.GIF"
assign(".fileimageLink", .fileimageLink, envir=.GlobalEnv)

## ####################################################################################################
## Load translation table:
.translations <- read.csv("./Scripts_Biodiv/Translations.csv", stringsAsFactor = FALSE, row.names = 1)
colnames(.translations) <- tolower(colnames(.translations))
assign(".translations", .translations, envir=.GlobalEnv)



########################################################################################################################
## Ajouts pour les graphs génériques [yr: 13/08/2010] :

## Noms d'usage des variables des principales tables de données
## (référentiels compris) :
init.GraphLang.f <- function()
{
    ## Purpose: Initialisation de la langue utilisée pour les noms de
    ## variables sur les graphiques.
    ## ----------------------------------------------------------------------
    ## Arguments: aucun (basé sur les options).
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 17 nov. 2011, 10:22

    assign("varNames",
           read.csv(paste(basePath, "/Scripts_Biodiv/VariableNames_",
                           tolower(getOption("P.lang")), ".csv",
                           sep=""),
                     header=TRUE, row.names=1, stringsAsFactors=FALSE,
                     fileEncoding="latin1", quote = "\""),
           envir=.GlobalEnv)
}

## Remplacer "/Scripts_Biodiv/VariableNames_fr.csv" par "/Scripts_Biodiv/VariableNames_en.csv" pour des axes et noms de variables en
## anglais.
## Affecte uniquement les sorties !








### Local Variables:
### ispell-local-dictionary: "english"
### fill-column: 120
### End:
