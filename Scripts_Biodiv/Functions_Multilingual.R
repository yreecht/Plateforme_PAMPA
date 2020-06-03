#-*- coding: latin-9 -*-

### File: Functions_Multilingual.R
### Time-stamp: <2020-06-03 14:43:24 a23579>
###
### Created: 09/07/2018	15:37:32
### Author: Yves Reecht
###
####################################################################################################
### Description:
###
### Functions for internationalization of the GUI
####################################################################################################

mltext <- function(msgid, language = tolower(getOption("P.GUIlang")))
{
    ## Purpose: Fetching translation of GUI text in the current language
    ##          (option). Uses English if language/msg not defined
    ## ----------------------------------------------------------------------
    ## Arguments: msgid: unique identifier of the text message to fetch
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date:  9 Jul 2018, 15:38

    transl <- get(".translations", envir = .GlobalEnv)

    lang <- ifelse(isTRUE(language %in% colnames(transl)),
                   language,
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
                                        ## If not defined in the desired language, tests if mandatory:
                                        no = ifelse(test = (nchar(transl[i, "en"]) &&
                                                            transl[i, "mandatory"]),
                                                    ## English used if missing and mandatory, and defined in English:
                                                    yes = gsub("\\\\n", "\n",
                                                               gsub("\\\\t", "\t",
                                                                    transl[i, "en"])),
                                                    no = ifelse(transl[i, "mandatory"],
                                                                ## ...generic text if mandatory
                                                                ##    AND not defined in English:
                                                                " !! No Text !! ",
                                                                ""))) # Kept empty if not mandatory!
                             }
                      return(res)
                  })

    return(unname(msg))
}

aliases <- function(fieldID, language = tolower(getOption("P.GUIlang")), reverse = FALSE)
{
    ## Purpose:
    ## ----------------------------------------------------------------------
    ## Arguments:
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 12 Dec 2018, 23:19

    aliases <- get(".aliases", envir = .GlobalEnv)

    lang <- ifelse(language %in% colnames(aliases),
                   language,
                   "en")

    als <- sapply(fieldID,
                  function(i)
                  {
                      res <- if (! i %in% row.names(aliases))
                             {
                                 i
                             }else{
                                 ifelse(test = nchar(aliases[i, lang]),
                                        yes = aliases[i, lang],
                                        no = ifelse(test = nchar(aliases[i, "en"]),
                                                    yes = aliases[i, "en"],
                                                    no = i))
                             }
                      return(res)
                  })

    if (isTRUE(reverse))
    {
        alsTmp <- als
        als <- names(alsTmp)
        names(als) <- alsTmp
    }

    return(als)
}







### Local Variables:
### ispell-local-dictionary: "english"
### fill-column: 120
### End:
