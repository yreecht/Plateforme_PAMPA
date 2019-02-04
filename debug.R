#-*- coding: latin-1 -*-

### File: debug.R
### Time-stamp: <2019-02-04 01:01:34 yreecht>
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


    if (.Platform$OS.type == "windows")
    {
        setwd("C:/PAMPA/")
        ## devDir <- "y:/Ifremer/PAMPA/Scripts/packPAMPA-WP2/Exec/"
        devDir <- "y:/Other_projects/Ifremer/PAMPA/PAMPA_Platform_dev/"
    }else{
        devDir <- "/media/ifremer/PAMPA/PAMPA/Scripts/packPAMPA-WP2/Exec/"
    }

    if (loadMain) source("./Scripts_Biodiv/Main.R", encoding="latin1")

    ## source(paste(devDir, "Scripts_Biodiv/Generic_aggregations.R", sep=""), encoding="latin1")
    ## source(paste(devDir, "Scripts_Biodiv/MRT_generic_sp.R", sep=""), encoding="latin1")
    ## source(paste(devDir, "Scripts_Biodiv/MRT_generic_unitobs.R", sep=""), encoding="latin1")
    ## source(paste(devDir, "Scripts_Biodiv/Barplots_occurrence.R", sep=""), encoding="latin1")
    ## source(paste(devDir, "Scripts_Biodiv/Barplots_occurrence_unitobs.R", sep=""), encoding="latin1")
    ## source(paste(devDir, "Scripts_Biodiv/Boxplots_generic_sp.R", sep=""), encoding="latin1")
    ## source(paste(devDir, "Scripts_Biodiv/Boxplots_generic_unitobs.R", sep=""), encoding="latin1")
    ## source(paste(devDir, "Scripts_Biodiv/Weight_calculation.R", sep=""), encoding="latin1")
    ## source(paste(devDir, "Scripts_Biodiv/Calculate_metrics_tables.R", sep=""), encoding="latin1")
    ## source(paste(devDir, "Scripts_Biodiv/Calculate_metrics_tables_LIT.R", sep=""), encoding="latin1")
    ## source(paste(devDir, "Scripts_Biodiv/Calculate_metrics_tables_SVR.R", sep=""), encoding="latin1")
    ## source(paste(devDir, "Scripts_Biodiv/Load_files.R", sep=""), encoding="latin1")
    ## source(paste(devDir, "Scripts_Biodiv/Load_files_manually.R", sep=""), encoding="latin1")
    ## source(paste(devDir, "Scripts_Biodiv/Functions_base.R", sep=""), encoding="latin1")
    ## source(paste(devDir, "Scripts_Biodiv/Functions_graphics.R", sep=""), encoding="latin1")
    ## source(paste(devDir, "Scripts_Biodiv/Messages_management.R", sep=""), encoding="latin1")
    ## source(paste(devDir, "Scripts_Biodiv/Maps_graphics.R", sep=""), encoding="latin1")
    ## source(paste(devDir, "Scripts_Biodiv/Initialisation.R", sep=""), encoding="latin1")
    ## source(paste(devDir, "Scripts_Biodiv/Interface_functions.R", sep=""), encoding="latin1")
    ## source(paste(devDir, "Scripts_Biodiv/Interface_main.R", sep=""), encoding="latin1")
    ## source(paste(devDir, "Scripts_Biodiv/Load_packages.R", sep=""), encoding="latin1")
    ## source(paste(devDir, "Scripts_Biodiv/Linear_models_generic_sp.R", sep=""), encoding="latin1")
    ## source(paste(devDir, "Scripts_Biodiv/Linear_models_generic_unitobs.R", sep=""), encoding="latin1")
    ## source(paste(devDir, "Scripts_Biodiv/Nombres_SVR.R", sep=""), encoding="latin1")
    ## source(paste(devDir, "Scripts_Biodiv/Data_subsets.R", sep=""), encoding="latin1")
    ## source(paste(devDir, "Scripts_Biodiv/Variables_selection_functions.R", sep=""), encoding="latin1")
    ## source(paste(devDir, "Scripts_Biodiv/Variables_selection_interface.R", sep=""), encoding="latin1")
    ## source(paste(devDir, "Scripts_Biodiv/Maps_variables.R", sep=""), encoding="latin1")
    ## source(paste(devDir, "Scripts_Biodiv/View.R", sep=""), encoding="latin1")
    ## source(paste(devDir, "Scripts_Biodiv/Generic_aggregations.R", sep=""), encoding="latin1")
    ## source(paste(devDir, "Scripts_Biodiv/Test_files.R", sep=""), encoding="latin1")
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
### fill-column: 140
### End:
