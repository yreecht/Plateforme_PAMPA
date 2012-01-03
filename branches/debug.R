#-*- coding: latin-1 -*-

### File: debug.R
### Time-stamp: <2011-12-22 10:54:04 yreecht>
###
### Author: Yves Reecht
###
####################################################################################################
### Description:
###
###
####################################################################################################


init.debug.f <- function(loadMain=FALSE)
{
    ## Purpose:
    ## ----------------------------------------------------------------------
    ## Arguments:
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 12 déc. 2011, 13:53

    if (loadMain) source("./Scripts_Biodiv/Global.r", encoding="latin1")

    source("y:/PAMPA/Scripts/packPAMPA-WP2/Exec/branches/chargement/fonctions_base.R", encoding="latin1")
    source("y:/PAMPA/Scripts/packPAMPA-WP2/Exec/branches/chargement/gestionmessages.r", encoding="latin1")
    source("y:/PAMPA/Scripts/packPAMPA-WP2/Exec/branches/chargement/Interface_principale.R", encoding="latin1")
    source("y:/PAMPA/Scripts/packPAMPA-WP2/Exec/branches/chargement/Chargement_fichiers.R", encoding="latin1")
    source("y:/PAMPA/Scripts/packPAMPA-WP2/Exec/branches/chargement/Initialisation.R", encoding="latin1")
    source("y:/PAMPA/Scripts/packPAMPA-WP2/Exec/branches/chargement/Chargement_manuel_fichiers.R", encoding="latin1")
    source("y:/PAMPA/Scripts/packPAMPA-WP2/Exec/branches/chargement/Calcul_poids.R", encoding="latin1")
    source("y:/PAMPA/Scripts/packPAMPA-WP2/Exec/branches/chargement/Calcul_tables_metriques.R", encoding="latin1")
    source("y:/PAMPA/Scripts/packPAMPA-WP2/Exec/branches/chargement/Agregations_generiques.R", encoding="latin1")
    source("y:/PAMPA/Scripts/packPAMPA-WP2/Exec/branches/chargement/selection_variables_fonctions.R", encoding="latin1")
    source("y:/PAMPA/Scripts/packPAMPA-WP2/Exec/branches/chargement/nombres_SVR.R", encoding="latin1")
    source("y:/PAMPA/Scripts/packPAMPA-WP2/Exec/branches/chargement/Calcul_tables_metriques_SVR.R", encoding="latin1")
}

debug.in.env.f <- function()
{
    ## Purpose: Démarrer une nouvelle fonction (avec son environnement
    ##          propre) et donner la main à l'utilisateur pour évaluer des
    ##          expressions dans cet environnement. Permet de tester la
    ##          portée de variables non-globales.
    ## ----------------------------------------------------------------------
    ## Arguments: aucun.
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 12 déc. 2011, 13:51

    browser()
}


traceBrowse.f <- function(what, skip=0L,...)
{
    ## Purpose: initie le tracking d'une fonction avec un browser au début.
    ## ----------------------------------------------------------------------
    ## Arguments: ceux de trace().
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 12 déc. 2011, 11:56


    tmp <- as.list(sys.call(0))

    if (is.function(what))
    {
        if (any(grepl("what", names(tmp))))
        {
            whattmp <- tmp$what
        }else{
            whattmp <- tmp[[2]]
        }
    }else{
        whattmp <- as.character(what)
    }

    ## browser()
    eval(substitute(trace(what=whattmp,
                          tracer=quote(browser(skipCalls=skip)),
                          ...),
                    list(whattmp=whattmp, skip=skip)))
}

traceBrowse.once.f <- function(what, skip=0L,...)
{
    ## Purpose: initie le tracking d'une fonction avec un browser au début et
    ##          stoppe le tracking à la fin.
    ## ----------------------------------------------------------------------
    ## Arguments: ceux de trace().
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 13 déc. 2011, 10:25

    tmp <- as.list(sys.call(0))

    if (is.function(what))
    {
        if (any(grepl("what", names(tmp))))
        {
            whattmp <- tmp$what
        }else{
            whattmp <- tmp[[2]]
        }
    }else{
        whattmp <- as.character(what)
    }

    ## browser()
    eval(substitute(trace(what=whattmp,
                          tracer=quote(browser(skipCalls=skip)),
                          exit=quote(untrace(what=whattmp)),
                          ...),
                    list(whattmp=whattmp, skip=skip)))
}










### Local Variables:
### ispell-local-dictionary: "english"
### fill-column: 120
### End:
