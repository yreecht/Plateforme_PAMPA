#-*- coding: latin-1 -*-

### File: debug.R
### Time-stamp: <2012-01-11 19:06:31 yreecht>
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
    source("y:/PAMPA/Scripts/packPAMPA-WP2/Exec/branches/chargement/interface_fonctions.R", encoding="latin1")
    source("y:/PAMPA/Scripts/packPAMPA-WP2/Exec/branches/chargement/Chargement_fichiers.R", encoding="latin1")
    source("y:/PAMPA/Scripts/packPAMPA-WP2/Exec/branches/chargement/Initialisation.R", encoding="latin1")
    source("y:/PAMPA/Scripts/packPAMPA-WP2/Exec/branches/chargement/Chargement_manuel_fichiers.R", encoding="latin1")
    source("y:/PAMPA/Scripts/packPAMPA-WP2/Exec/branches/chargement/Calcul_poids.R", encoding="latin1")
    source("y:/PAMPA/Scripts/packPAMPA-WP2/Exec/branches/chargement/Calcul_tables_metriques.R", encoding="latin1")
    source("y:/PAMPA/Scripts/packPAMPA-WP2/Exec/branches/chargement/Agregations_generiques.R", encoding="latin1")
    source("y:/PAMPA/Scripts/packPAMPA-WP2/Exec/branches/chargement/selection_variables_fonctions.R", encoding="latin1")
    source("y:/PAMPA/Scripts/packPAMPA-WP2/Exec/branches/chargement/selection_variables_interface.R", encoding="latin1")
    source("y:/PAMPA/Scripts/packPAMPA-WP2/Exec/branches/chargement/nombres_SVR.R", encoding="latin1")
    source("y:/PAMPA/Scripts/packPAMPA-WP2/Exec/branches/chargement/Calcul_tables_metriques_SVR.R", encoding="latin1")
    source("y:/PAMPA/Scripts/packPAMPA-WP2/Exec/branches/chargement/Calcul_tables_metriques_LIT.R", encoding="latin1")
    source("y:/PAMPA/Scripts/packPAMPA-WP2/Exec/branches/chargement/selection_donnees.R", encoding="latin1")
    source("y:/PAMPA/Scripts/packPAMPA-WP2/Exec/branches/chargement/view.r", encoding="latin1")
    source("y:/PAMPA/Scripts/packPAMPA-WP2/Exec/branches/chargement/modeles_lineaires_unitobs_generiques.R", encoding="latin1")
    source("y:/PAMPA/Scripts/packPAMPA-WP2/Exec/branches/chargement/boxplots_unitobs_generiques.R", encoding="latin1")
    source("y:/PAMPA/Scripts/packPAMPA-WP2/Exec/branches/chargement/boxplots_esp_generiques.R", encoding="latin1")
    source("y:/PAMPA/Scripts/packPAMPA-WP2/Exec/branches/chargement/arbres_regression_unitobs_generiques.R", encoding="latin1")
    source("y:/PAMPA/Scripts/packPAMPA-WP2/Exec/branches/chargement/arbres_regression_esp_generiques.R", encoding="latin1")
    source("y:/PAMPA/Scripts/packPAMPA-WP2/Exec/branches/chargement/modeles_lineaires_esp_generiques.R", encoding="latin1")
    source("y:/PAMPA/Scripts/packPAMPA-WP2/Exec/branches/chargement/barplots_occurrence_unitobs.R", encoding="latin1")
    source("y:/PAMPA/Scripts/packPAMPA-WP2/Exec/branches/chargement/barplots_occurrence.R", encoding="latin1")
    source("y:/PAMPA/Scripts/packPAMPA-WP2/Exec/branches/chargement/load_packages.R", encoding="latin1")
    source("y:/PAMPA/Scripts/packPAMPA-WP2/Exec/branches/chargement/fonctions_graphiques.R", encoding="latin1")
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




findGlobalsNotFUN <- function(fun, inverse=FALSE)
{
    ## Purpose: trouver les objets (non-fonctions) globeaux
    ##          utilisés par une fonction.
    ## ----------------------------------------------------------------------
    ## Arguments: fun : une fonction.
    ##            inverse : inversion (logical) : renvoie uniquement les
    ##                      fonctions si TRUE.
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date:  4 janv. 2012, 12:18

    require(codetools)

    obj <- findGlobals(fun=fun, merge=FALSE)

    if (inverse)
    {
        return(obj$functions)
    }else{
        return(obj$variables)
    }
}


## block <- function()
## checkUsage




### Local Variables:
### ispell-local-dictionary: "english"
### fill-column: 120
### End:
