# prise en compte de la surface
diviseur <- refSpatial$surface[match(freqtot$zone,refSpatial$codeZone)]
freqtot$nb_pers=freqtot$nb_pers/diviseur
freqtot$nb_bat=freqtot$nb_bat/diviseur
freqtot$nb_persAct=freqtot$nb_persAct/diviseur
freqtot$nb_batAct=freqtot$nb_batAct/diviseur

# prise en compte du linéaire côtier
diviseurKM <- refSpatial$lineaireCotier[match(freqtot$zone,refSpatial$codeZone)]
freqtot$nb_pers=freqtot$nb_pers/diviseurKM
freqtot$nb_bat=freqtot$nb_bat/diviseurKM
freqtot$nb_persAct=freqtot$nb_persAct/diviseurKM
freqtot$nb_batAct=freqtot$nb_batAct/diviseurKM

freqtot$nb_pers[which(freqtot$nb_pers=="Inf")]=0
freqtot$nb_bat[which(freqtot$nb_bat=="Inf")]=0
freqtot$nb_persAct[which(freqtot$nb_persAct=="Inf")]=0
freqtot$nb_batAct[which(freqtot$nb_batAct=="Inf")]=0


# calcul du nombre de sorties dans AMP et hors AMP
peche$partAMP2 <- 0.125
peche$partAMP2[which(peche$partAMP=="26a50%")] <- 0.385
peche$partAMP2[which(peche$partAMP=="51a75%")] <- 0.635
peche$partAMP2[which(peche$partAMP=="76a100%")] <- 0.885

peche$sortieAMP <- peche$nb_sortie_tot*peche$partAMP2
peche$sortieNonAMP <- peche$nb_sortie_tot * (1-peche$partAMP2)
ratioAMP <- peche$sortieAMP/peche$sortieNonAMP
hist(ratioAMP)