#-*- coding: latin-1 -*-

### File: modeles_lineaires_interface.R
### Time-stamp: <2010-09-06 13:09:56 yreecht>
###
### Author: Yves Reecht
###
####################################################################################################
### Description:
###
###
####################################################################################################

########################################################################################################################
## Fonctions pour ajouter des barres de défilement dans une fenêtre principale :

.RDEnv <- new.env(hash=TRUE)                   # Private environment

rdenv <- function() {
    return(.RDEnv)
}


scrollframeCreate.f <- function(parent,env=rdenv(),...)
{
    stopifnot(is.tkwin(parent))

    frame <- tkframe(parent,
                     class="ScrollFrame",...)
    xscroll <- tkscrollbar(frame,
                           repeatinterval=5,
                           orient="horizontal",
                           command=function(...) tkxview(vport, ...))
    yscroll <- tkscrollbar(frame,
                           repeatinterval=5,
                           orient="vertical",
                           command=function(...) tkyview(vport, ...))
    vport <- tkcanvas(frame)
    tkconfigure(vport, xscrollcommand=function(...) tkset(xscroll, ...))
    tkconfigure(vport, yscrollcommand=function(...) tkset(yscroll, ...))

    pady <- paste("0", tclvalue(tkwinfo("reqheight", xscroll)))
    tkpack(yscroll, side="right", fill="y", pady=pady)
    tkpack(xscroll, side="bottom", fill="x")
    tkpack(vport, side="left", fill="both", expand=TRUE)

    int.frame <- tkframe(vport,
                         borderwidth=4,
                         relief="groove")
    tkcreate(vport, "window", "0 0", anchor="nw", window=int.frame$ID)
    tkbind(int.frame, "<Configure>", function() scrollframeResize(int.frame))

    ## Save this so items can be put in it
    assign("interior.frame", int.frame, envir=env)

    return(frame)
}


scrollframeResize <- function(iframe) {
    stopifnot(tclvalue(tkwinfo("class", iframe)) == "Frame")

    w <- tkwinfo("width", iframe)
    h <- tkwinfo("height", iframe)

    vport <- tkwinfo("parent", iframe)
    stopifnot(tclvalue(tkwinfo("class", vport)) == "Canvas")
    bbox <- tkbbox(vport, "all")

    tkconfigure(vport,
                width=w,
                height=h,
                scrollregion=bbox,
                xscrollincrement="0.1i",
                yscrollincrement="0.1i")
}


scrollframeInterior <- function(env=rdenv()) {
    return(get("interior.frame", envir=env))
}








### Local Variables:
### ispell-local-dictionary: "english"
### fill-column: 120
### End:
