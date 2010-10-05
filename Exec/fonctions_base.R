#-*- coding: latin-1 -*-

### File: fonctions_base.R
### Time-stamp: <2010-10-05 14:42:34 yreecht>
###
### Author: Yves Reecht
###
####################################################################################################
### Description:
###
### Fonctions de bases de la plateforme (également utilisé pour définir des fonctions de base
### récentes de R lorsqu'elles n'existent pas, par ex si on travaille sur une version ancienne de R)
####################################################################################################

########################################################################################################################
if (!exists("grepl"))
{
    grepl <- function(pattern, x,...)
    {
        ## Purpose: Émulation des fonctions de 'grepl' (en moins efficace
        ##          probablement) si la fonction n'existe pas.
        ## ----------------------------------------------------------------------
        ## Arguments: pattern : le motif à rechercher.
        ##            x : le vecteur dans lequel chercher le motif.
        ##            ... : arguments supplémentaires pour 'grep'
        ## ----------------------------------------------------------------------
        ## Author: Yves Reecht, Date:  5 oct. 2010, 14:36

        return(sapply(x,
                      function(x2)
                  {
                      ## On teste pour chaque élément s'il contient le motif :
                      as.logical(length(grep(as.character(pattern), x2,...)))
                  }))
    }
}else{}                                 # Sinon rien à faire








### Local Variables:
### ispell-local-dictionary: "english"
### fill-column: 120
### End:
