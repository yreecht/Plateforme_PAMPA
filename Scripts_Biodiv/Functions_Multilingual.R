#-*- coding: latin-9 -*-

### File: Functions_Multilingual.R
### Time-stamp: <2018-11-27 11:45:57 yreecht>
###
### Created: 09/07/2018	15:37:32
### Author: Yves Reecht
###
####################################################################################################
### Description:
###
### Functions for internationalization of the GUI
####################################################################################################

mltext <- function(msgid)
{
    ## Purpose: Fetching translation of GUI text in the current language
    ##          (option). Uses English if language/msg not defined
    ## ----------------------------------------------------------------------
    ## Arguments: msgid: unique identifier of the text message to fetch
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date:  9 Jul 2018, 15:38

    transl <- get(".translations", envir = .GlobalEnv)

    lang <- ifelse(getOption("P.GUIlang") %in% colnames(transl),
                   getOption("P.GUIlang"),
                   "en")

    msg <- sapply(msgid,
                  function(i)
                  {
                      res <- if (! i %in% row.names(transl))
                             {
                                 " !! No Text !! "
                             }else{
                                 ifelse(test = nchar(transl[i, lang]),
                                        yes = gsub("\\\\n", "\n",
                                                   gsub("\\\\t", "\t",
                                                        transl[i, lang])),
                                        no = ifelse(nchar(transl[i, "en"]),
                                                    transl[i, "en"],
                                                    " !! No Text !! "))
                             }
                      return(res)
                  })

    return(unname(msg))
}








### Local Variables:
### ispell-local-dictionary: "english"
### fill-column: 120
### End:
