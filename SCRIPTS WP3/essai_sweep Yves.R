#-*- coding: latin-1 -*-

### File: essai_sweep.R
### Time-stamp: <2011-07-27 12:34:44 yreecht>
###
### Author: Yves Reecht
###
####################################################################################################
### Description:
###
###
####################################################################################################


setwd("y:/PAMPA/Scripts/bac_a_sable/Elo/")

load("./donneesComptage.rdata")

dim(FreqJour)
dim(nbSortie)

dimnames(FreqJour)
dimnames(nbSortie)

FreqJour[ , , 1:2, ]

## (1) avec sweep :
PropJour <- sweep(x=FreqJour,
                  MARGIN=c(1, 2, 4),
                  STATS=nbSortie,
                  FUN="/")

## dim(PropJour)

## PropJour[ , , 1:2, ]

Moyennes <- apply(PropJour, c(1, 2, 4),
                  FUN=function(x,...)   # sum, mais renvoie NA si que des NAs.
             {
                 if (all(is.na(x)))
                 {
                     return(NA)
                 }else{
                     return(sum(x,...))
                 }
             },
                  na.rm=TRUE)

## (2) Solution alternative (sans sweep) :
TotJour <- apply(FreqJour, c(1, 2, 4),
                 sum,
                 na.rm=TRUE)

Moyennes <- TotJour / nbSortie

## (3) Plus compact :

Moyennes <- apply(FreqJour, c(1, 2, 4),
                 sum,
                 na.rm=TRUE) /
    apply(FreqJour, c(1, 2, 4),         # équivalent à mais sans la classe "table".
          function(x) sum(!is.na(x)))


### Local Variables:
### ispell-local-dictionary: "english"
### fill-column: 120
### End:
