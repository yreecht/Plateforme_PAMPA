################################################################################
# Nom               : ChoixUtilisateur.r
# Type              : Programme
# Objet             : Ce programme permet à l'utilisateur de choisir certains
#                    facteurs pour calculer les métriques (familles, espèces, pris en compte des surfaces, etc...)
# Input             : TXT
# Output            : Data
# Auteur            : Elodie GAMP
# R version         : 2.8.1
# Date de création  : Juillet 2010
# Sources           :
################################################################################

# choix de pooler toutes les années pour calculer les métriques
ChoixPoolerAnnees.f = function () {
  if (nrow(freqtot)!=0) {
    freqtot$periodEchant<-" "
    assign("freqtotModif",freqtot,envir=.GlobalEnv)
    assign("freqtotRefModif",freqtot,envir=.GlobalEnv)
    assign("freqtot",freqtot,envir=.GlobalEnv)
    assign("freqtotRef",freqtot,envir=.GlobalEnv)
  }
  if (nrow(tousQuest)!=0) {
    tousQuest$periodEchant<-" "
    assign("tousQuestModif",tousQuest,envir=.GlobalEnv)
    assign("tousQuest",tousQuest,envir=.GlobalEnv)
  }
  if (nrow(peche)!=0) {
    peche$periodEchant<-" "
    assign("pecheModif",peche,envir=.GlobalEnv)
    assign("peche",peche,envir=.GlobalEnv)
    pecheQ$periodEchant<-" "
    assign("pecheQModif",pecheQ,envir=.GlobalEnv)
    assign("pecheQ",pecheQ,envir=.GlobalEnv)    
  }
  if (nrow(captures)!=0) {
    captures$periodEchant<-" "
    assign("capturesModif",captures,envir=.GlobalEnv)
    assign("captures",captures,envir=.GlobalEnv)
  }
  if (nrow(captures2)!=0) {
    captures2$periodEchant<-" "
    assign("captures2Modif",captures2,envir=.GlobalEnv)
    assign("captures2",captures2,envir=.GlobalEnv)
  }
  if (nrow(capturesAn)!=0) {
    capturesAn$periodEchant<-" "    
    assign("capturesAnModif",capturesAn,envir=.GlobalEnv)
    assign("capturesAn",capturesAn,envir=.GlobalEnv)
  }
  if (nrow(plaisance)!=0) {
    plaisance$periodEchant<-" "    
    assign("plaisanceModif",plaisance,envir=.GlobalEnv)
    assign("plaisance",plaisance,envir=.GlobalEnv)
  }
  if (nrow(plongee)!=0) {
    plongee$periodEchant<-" "    
    assign("plongeeModif",plongee,envir=.GlobalEnv)
    assign("plongee",plongee,envir=.GlobalEnv)    
  }
  if (nrow(excursion)!=0) {
    excursion$periodEchant<-" "    
    assign("excursionModif",excursion,envir=.GlobalEnv)
    assign("excursion",excursion,envir=.GlobalEnv)
  }
    critereSelection<-"toutes années confondues"
    assign("critereSelection",critereSelection,envir=.GlobalEnv)
    gestionMSGaide.f("Poolage")
    tkmessageBox(message=paste("Vous avez choisi de calculer les métriques toutes années confondues.",sep=""))

  ### calcul des limites des barplot
  limActTot<-c(0,length(unique(tousQuest$activite)))
  assign("limActTot",limActTot,envir=.GlobalEnv)  
  limActDet<-c(0,length(unique(tousQuest$activitePec)))
  assign("limActDet",limActDet,envir=.GlobalEnv)
}


# choix des périodes échantillonnées à étudier
### ATTENTION: les périodes échantillonnées peuvent ne pas être les mêmes selon les usages
### (fréquentation, pêche, plaisance, plongée, SSM) 
### donc refaire une liste avec toutes les périodes rencontrées
ChoixPeriodEchant.f = function () {
  tt<-tktoplevel()
  tkwm.title(tt,"Selection")
  scr <- tkscrollbar(tt, repeatinterval=5, command=function(...)tkyview(tl,...))
  tl<-tklistbox(tt,height=10,width=30,selectmode="multiple",background="white")
  tkgrid(tklabel(tt,text="Choix des périodes d'étude à considérer"))
  tkgrid(tl,scr)
  tkgrid.configure(scr,rowspan=5,sticky="nsw")
  periodEchant <- sort(unique(c(as.character(freqtot$periodEchant),as.character(tousQuest$periodEchant))))
  for (i in (1:length(unique(periodEchant)))) {
    tkinsert(tl,"end",periodEchant[i])
    }
  tkselection.set(tl,0)  
  OnOK <- function()
    {
   	periodesChoisies <- periodEchant[as.numeric(tkcurselection(tl))+1]
    assign("periodesChoisies",periodesChoisies,envir=.GlobalEnv)
    tkdestroy(tt)
    freqtot<-FREQTOT[1,]
 	  for (i in 1:length(periodesChoisies)){
 	    freqtot<-rbind(freqtot,subset(FREQTOT,FREQTOT$periodEchant==periodesChoisies[i]))
 	    }
    freqtot<-freqtot[-1,]
    assign("freqtotModif",freqtot,envir=.GlobalEnv)
    assign("freqtotRefModif",freqtot,envir=.GlobalEnv)
    assign("freqtot",freqtot,envir=.GlobalEnv)
    assign("freqtotRef",freqtot,envir=.GlobalEnv)
    tousQuest<-TOUSQUEST[1,]
 	  for (i in 1:length(periodesChoisies)){
 	    tousQuest<-rbind(tousQuest,subset(TOUSQUEST,TOUSQUEST$periodEchant==periodesChoisies[i]))
 	    }
    tousQuest<-tousQuest[-1,]
    assign("tousQuestModif",tousQuest,envir=.GlobalEnv)
    assign("tousQuest",tousQuest,envir=.GlobalEnv)
    peche<-PECHE[1,]
 	  for (i in 1:length(periodesChoisies)){
 	    peche<-rbind(peche,subset(PECHE,PECHE$periodEchant==periodesChoisies[i]))
 	    }
    peche<-peche[-1,]
    assign("pecheModif",peche,envir=.GlobalEnv)
    assign("peche",peche,envir=.GlobalEnv)
    pecheQ<-PECHEQ[1,]
 	  for (i in 1:length(periodesChoisies)){
 	    pecheQ<-rbind(pecheQ,subset(PECHEQ,PECHEQ$periodEchant==periodesChoisies[i]))
 	    }
    pecheQ<-pecheQ[-1,]
    assign("pecheQModif",pecheQ,envir=.GlobalEnv)
    assign("pecheQ",pecheQ,envir=.GlobalEnv)
    captures<-CAPTURES[1,]
 	  for (i in 1:length(periodesChoisies)){
 	    captures<-rbind(captures,subset(CAPTURES,CAPTURES$periodEchant==periodesChoisies[i]))
 	    }
    captures<-captures[-1,]
    assign("capturesModif",captures,envir=.GlobalEnv)
    assign("captures",captures,envir=.GlobalEnv)
    captures2<-CAPTURES2[1,]
 	  for (i in 1:length(periodesChoisies)){
 	    captures2<-rbind(captures2,subset(CAPTURES2,CAPTURES2$periodEchant==periodesChoisies[i]))
 	    }
    captures2<-captures2[-1,]
    assign("captures2Modif",captures2,envir=.GlobalEnv)
    assign("captures2",captures2,envir=.GlobalEnv)
    capturesAn<-CAPTURESAN[1,]
 	  for (i in 1:length(periodesChoisies)){
 	    capturesAn<-rbind(capturesAn,subset(CAPTURESAN,CAPTURESAN$periodEchant==periodesChoisies[i]))
 	    }
    capturesAn<-capturesAn[-1,]
    assign("capturesAnModif",capturesAn,envir=.GlobalEnv)
    assign("capturesAn",capturesAn,envir=.GlobalEnv)
    plaisance<-PLAISANCE[1,]
 	  for (i in 1:length(periodesChoisies)){
 	    plaisance<-rbind(plaisance,subset(PLAISANCE,PLAISANCE$periodEchant==periodesChoisies[i]))
 	    }
    plaisance<-plaisance[-1,]
    assign("plaisanceModif",plaisance,envir=.GlobalEnv)
    assign("plaisance",plaisance,envir=.GlobalEnv)
    plongee<-PLONGEE[1,]
 	  for (i in 1:length(periodesChoisies)){
 	    plongee<-rbind(plongee,subset(PLONGEE,PLONGEE$periodEchant==periodesChoisies[i]))
 	    }
    plongee<-plongee[-1,]
    assign("plongeeModif",plongee,envir=.GlobalEnv)
    assign("plongee",plongee,envir=.GlobalEnv)    
    excursion<-EXCURSION[1,]
 	  for (i in 1:length(periodesChoisies)){
 	    excursion<-rbind(excursion,subset(EXCURSION,EXCURSION$periodEchant==periodesChoisies[i]))
 	    }
    excursion<-excursion[-1,]
    assign("excursionModif",excursion,envir=.GlobalEnv)
    assign("excursion",excursion,envir=.GlobalEnv)
    MiseajourTableau.f(tclarray)
    critereSelection<-"période d'échantillonnage"
    assign("critereSelection",critereSelection,envir=.GlobalEnv)
    gestionMSGaide.f("Selection")
    tkmessageBox(message=paste("Vous avez choisi les périodes d'échantillonnage suivantes : ",list(periodesChoisies),".",sep=""))
    }
  OK.but <-tkbutton(tt,text="   OK   ",command=OnOK)
  tkgrid(OK.but)
  tkfocus(tt)
  tkwait.window(tt)
  ### calcul des limites des barplot
  limActTot<-c(0,length(unique(tousQuest$activite)))
  assign("limActTot",limActTot,envir=.GlobalEnv)  
  limActDet<-c(0,length(unique(tousQuest$activitePec)))
  assign("limActDet",limActDet,envir=.GlobalEnv)
}


# choix des familles à étudier plus spécifiquement
ChoixFamilles.f = function () {
  tt<-tktoplevel()
  tkwm.title(tt,"Selection")
  scr <- tkscrollbar(tt, repeatinterval=5, command=function(...)tkyview(tl,...))
  tl<-tklistbox(tt,height=30,width=30,selectmode="multiple",background="white")
  tkgrid(tklabel(tt,text="Choix des familles à étudier"))
  tkgrid(tl,scr)
  tkgrid.configure(scr,rowspan=5,sticky="nsw")
  familles <- sort(as.character(unique(captures$famille)))
  for (i in (1:length(unique(captures$famille)))) {
    tkinsert(tl,"end",familles[i])
    }
  tkselection.set(tl,0)  
  OnOK <- function()
    {
   	famillesChoisies <- familles[as.numeric(tkcurselection(tl))+1]
    assign("famillesChoisies",famillesChoisies,envir=.GlobalEnv)
    tkdestroy(tt)
    msg <- paste("Vous avez choisi les familles suivantes : ",list(famillesChoisies),".",sep="")
    tkmessageBox(message=msg)
    }
  OK.but <-tkbutton(tt,text="   OK   ",command=OnOK)
  tkgrid(OK.but)
  tkfocus(tt)
  tkwait.window(tt)
}

# choix des espèces à étudier plus spécifiquement
ChoixEspeces.f = function () {
  tt<-tktoplevel()
  tkwm.title(tt,"Selection")
  scr <- tkscrollbar(tt, repeatinterval=5, command=function(...)tkyview(tl,...))
  tl<-tklistbox(tt,height=30,width=30,selectmode="multiple",background="white")
  tkgrid(tklabel(tt,text="Choix des espèces à étudier"))
  tkgrid(tl,scr)
  tkgrid.configure(scr,rowspan=5,sticky="nsw")
  especes <- sort(as.character(unique(captures$identifiant)))
  for (i in (1:length(unique(captures$identifiant))))  {
    tkinsert(tl,"end",especes[i])
    }
  tkselection.set(tl,0)  
  OnOK <- function()
    {
 	  especesChoisies <- especes[as.numeric(tkcurselection(tl))+1]
    assign("especesChoisies",especesChoisies,envir=.GlobalEnv)
    tkdestroy(tt)
    msg <- paste("Vous avez choisi les espèces suivantes : ",list(especesChoisies),".",sep="")
    tkmessageBox(message=msg)
    }
  OK.but <-tkbutton(tt,text="   OK   ",command=OnOK)
  tkgrid(OK.but)
  tkfocus(tt)
  tkwait.window(tt)
}

# choix de l'année en cours
ChoixAnnee.f = function () {
  choixAnnee<-tktoplevel()
  tkwm.title(choixAnnee,"Année")
  annee <- tclVar(2011)
  entry.annee <-tkentry(choixAnnee,width="20",textvariable=annee)
  tkgrid(tklabel(choixAnnee,text="Entrer l'année en cours."))
  tkgrid(entry.annee)
  OnOK <- function()
  {
  	anneeVal <- tclvalue(annee)
  	tkdestroy(choixAnnee)
  	tkmessageBox(message=paste("Nous sommes en ",anneeVal))
    assign("annee",as.numeric(anneeVal),envir=.GlobalEnv)
  }
  OK.but <-tkbutton(choixAnnee,text="   OK   ",command=OnOK)
  tkbind(entry.annee, "annee",OnOK)
  tkgrid(OK.but)
  tkfocus(choixAnnee)
  tkwait.window(choixAnnee)
}


# choix de la valeur seuil pour la fréquentation  (percentile pour les sorties sup à ce seuil)
ChoixSeuilFreq.f = function () {
  choixSeuilFreq<-tktoplevel()
  tkwm.title(choixSeuilFreq,"Seuil Fréquentation")
  SeuilFreq <- tclVar(0.8)
  entry.SeuilFreq <-tkentry(choixSeuilFreq,width="20",textvariable=SeuilFreq)
  tkgrid(tklabel(choixSeuilFreq,text="Entrer le seuil souhaité pour la fréquentation"))
  tkgrid(entry.SeuilFreq)
  OnOK <- function()
  {
  	SeuilFreqVal <- tclvalue(SeuilFreq)
  	tkdestroy(choixSeuilFreq)
  	tkmessageBox(message=paste("Votre seuil de fréquentation est de ",as.numeric(SeuilFreqVal)*100,"%"))
    assign("SeuilFreq",as.numeric(SeuilFreqVal),envir=.GlobalEnv)
  }
  OK.but <-tkbutton(choixSeuilFreq,text="   OK   ",command=OnOK)
  tkbind(entry.SeuilFreq, "SeuilFreq",OnOK)
  tkgrid(OK.but)
  tkfocus(choixSeuilFreq)
  tkwait.window(choixSeuilFreq)
}

# choix de séparer les individus selon leur situation géographique
ChoixResident.f = function () {
  tt<-tktoplevel()
  tkwm.title(tt,"Selection de la catégorie d'individus")
  scr <- tkscrollbar(tt, repeatinterval=5, command=function(...)tkyview(tl,...))
  tl<-tklistbox(tt,height=10,width=30,selectmode="single",background="white")
  tkgrid(tklabel(tt,text="Choix de séparer les individus \n selon leur situation géographique \n"))
  tkgrid(tl,scr)
  tkgrid.configure(scr,rowspan=5,sticky="nsw")
  niveaux <- c("résidents uniquement","non-résidents uniquement","résidents et non-résidents confondus")
  for (i in (1:length(niveaux)))  {
    tkinsert(tl,"end",niveaux[i])
    }
  tkselection.set(tl,0)  
  OnOK <- function()
    {
 	  niveauResid <- niveaux[as.numeric(tkcurselection(tl))+1]
    assign("niveauResid",niveauResid,envir=.GlobalEnv)
    tkdestroy(tt)
    }
  OK.but <-tkbutton(tt,text="   OK   ",command=OnOK)
  tkgrid(OK.but)
  tkfocus(tt)
  tkwait.window(tt)
  if (niveauResid=="résidents uniquement") {
    tousQuest2<-subset(tousQuest,tousQuest$resident=="oui")    
    peche2<-subset(peche,peche$resident=="oui")
    pecheQ2<-subset(pecheQ,pecheQ$resident=="oui")
    plaisance2<-subset(plaisance,plaisance$resident=="oui")
    plongee2<-subset(plongee,plongee$resident=="oui")
    excursion2<-subset(excursion,excursion$resident=="oui")
    titreResid<-"pour les résidents"      
    }  
  if (niveauResid=="non-résidents uniquement") {
    tousQuest2<-subset(tousQuest,tousQuest$resident=="non")    
    peche2<-subset(peche,peche$resident=="non")
    pecheQ2<-subset(pecheQ,pecheQ$resident=="non")
    plaisance2<-subset(plaisance,plaisance$resident=="non")
    plongee2<-subset(plongee,plongee$resident=="non")
    excursion2<-subset(excursion,excursion$resident=="non")
    titreResid<-"pour les non-résidents"      
    }   
  if (niveauResid=="résidents et non-résidents confondus") {
    tousQuest2<-TOUSQUEST    
    peche2<-PECHE
    pecheQ2<-PECHEQ
    plaisance2<-PLAISANCE
    plongee2<-PLONGEE
    excursion2<-EXCURSION
    titreResid<-"résidents et non-résidents confondus"      
  }      
  assign("tousQuestModif",tousQuest2,envir=.GlobalEnv)  
  assign("pecheModif",peche2,envir=.GlobalEnv)  
  assign("pecheQModif",pecheQ2,envir=.GlobalEnv)  
  assign("plaisanceModif",plaisance2,envir=.GlobalEnv)  
  assign("plongeeModif",plongee2,envir=.GlobalEnv)  
  assign("excursionModif",excursion2,envir=.GlobalEnv)
  assign("tousQuest",tousQuest2,envir=.GlobalEnv)  
  assign("peche",peche2,envir=.GlobalEnv)  
  assign("plaisance",plaisance2,envir=.GlobalEnv)  
  assign("plongee",plongee2,envir=.GlobalEnv)  
  assign("excursion",excursion2,envir=.GlobalEnv)  
  
  assign("titreResid",titreResid,envir=.GlobalEnv)
  MiseajourTableau.f(tclarray)
  critereSelection<-niveauResid
  assign("critereSelection",critereSelection,envir=.GlobalEnv)
  gestionMSGaide.f("Selection")
  tkmessageBox(message=paste("Vous avez choisi de calculer les métriques pour les : ",niveauResid,".",sep=""))  
}

# choix d'étudier un seul type d'usagers
ChoixUsagers.f = function () {
  tt<-tktoplevel()
  tkwm.title(tt,"Choix des usagers")
  scr <- tkscrollbar(tt, repeatinterval=5, command=function(...)tkyview(tl,...))
  tl<-tklistbox(tt,height=10,width=30,selectmode="multiple",background="white")
  tkgrid(tklabel(tt,text="Choix des usagers à étudier \n ATTENTION première ligne sélectionnée par défaut \n"))
  tkgrid(tl,scr)
  tkgrid.configure(scr,rowspan=5,sticky="nsw")
  usagers <- as.character(unique(tousQuest$activite))
  for (i in (1:length(usagers)))  {
    tkinsert(tl,"end",usagers[i])
    }
  tkselection.set(tl,0)  
  OnOK <- function()
    {
 	  usagersChoisis <- usagers[as.numeric(tkcurselection(tl))+1]
 	  tousQuest2<-tousQuest[1,]
 	  for (i in 1:length(usagersChoisis)){
 	    tousQuest2<-rbind(tousQuest2,subset(tousQuest,tousQuest$activite==usagersChoisis[i]))
 	    }
    tousQuest2<-tousQuest2[-1,]
    tousQuest2$activite<-tousQuest2$activitePec
    assign("tousQuestModif",tousQuest2,envir=.GlobalEnv)
    assign("tousQuest",tousQuest2,envir=.GlobalEnv)
    assign("usagersChoisis",usagersChoisis,envir=.GlobalEnv)
    MiseajourTableau.f(tclarray)
    critereSelection<-"certains usagers"
    assign("critereSelection",critereSelection,envir=.GlobalEnv)
    gestionMSGaide.f("Selection")  
    tkdestroy(tt)
    msg <- paste("Vous avez choisi de calculer les métriques pour les : ",list(usagersChoisis),".",sep="")
    tkmessageBox(message=msg)
    }
  OK.but <-tkbutton(tt,text="   OK   ",command=OnOK)
  tkgrid(OK.but)
  tkfocus(tt)
  tkwait.window(tt)
  ### calcul des limites des barplot
  limActTot<-c(0,length(unique(tousQuest$activite)))
  assign("limActTot",limActTot,envir=.GlobalEnv)  
  limActDet<-c(0,length(unique(tousQuest$activitePec)))
  assign("limActDet",limActDet,envir=.GlobalEnv)
}  


# choix de calcul par surface ou non
ChoixSurface.f = function () {
  tt<-tktoplevel()
  tkwm.title(tt,"Prise en compte de la surface")
  scr <- tkscrollbar(tt, repeatinterval=5, command=function(...)tkyview(tl,...))
  tl<-tklistbox(tt,height=10,width=30,selectmode="single",background="white")
  tkgrid(tklabel(tt,text="Choix de l'unité de surface"))
  tkgrid(tl,scr)
  tkgrid.configure(scr,rowspan=5,sticky="nsw")
  niveaux <- c("par unité de surface (km²)","par linéaire cotier (km)","sans unité de surface","par dispositif de mouillages")
  for (i in (1:length(niveaux)))  {
    tkinsert(tl,"end",niveaux[i])
    }
  tkselection.set(tl,0)  
  OnOK <- function()
    {
 	  niveauSurface <- niveaux[as.numeric(tkcurselection(tl))+1]
    assign("niveauSurface",niveauSurface,envir=.GlobalEnv)
    tkdestroy(tt)
    msg <- paste("Vous avez choisi de calculer les métriques : ",niveauSurface,".",sep="")
    tkmessageBox(message=msg)
    }
  OK.but <-tkbutton(tt,text="   OK   ",command=OnOK)
  tkgrid(OK.but)
  tkfocus(tt)
  tkwait.window(tt)
  if (niveauSurface=="par unité de surface (km²)") {
    surfaceZo<-refSpatial$surface       # pour la surface des zones
    surfaceZ<-tapply(refSpatial$surface,refSpatial$statutPAMPA,sum,na.rm=T)  # pour la surface des statuts PAMPA
    surfaceGr<-tapply(refSpatial$surface,refSpatial$groupe,sum,na.rm=T)   # pour la surface des groupes de zones 
    surfaceTot<-tapply(refSpatial$surface,refSpatial$AMP,sum,na.rm=T)   # pour la surface totale de l'AMP 
    titreSurface<-"par km²"      
    }  
  if (niveauSurface=="par linéaire cotier (km)") {
    surfaceZo<-refSpatial$lineaireCotier  # pour le linéaire cotier des zones
    surfaceZ<-tapply(refSpatial$lineaireCotier,refSpatial$statutPAMPA,sum,na.rm=T)  # pour le linéaire côtier des statuts PAMPA
    surfaceGr<-tapply(refSpatial$lineaireCotier,refSpatial$groupe,sum,na.rm=T)  # pour le linéaire côtier des groupes de zone
    surfaceTot<-tapply(refSpatial$lineaireCotier,refSpatial$AMP,sum,na.rm=T)   # pour la surface totale de l'AMP 
    titreSurface<-"par kilomètre"      
    }   
  if (niveauSurface=="sans unité de surface") {
 	  surfaceZo<-rep(1,nrow(refSpatial))
   	surfaceGr<-rep(1,length(unique(refSpatial$groupe)))
   	surfaceZ<-rep(1,length(unique(refSpatial$statutPAMPA)))
   	surfaceTot<-rep(1,length(unique(refSpatial$AMP)))
    titreSurface<-""      
  } 
    if (niveauSurface=="par dispositif de mouillages") {
    surfaceZo<-refSpatial$nbCM  # pour le linéaire cotier des zones
    surfaceZ<-tapply(refSpatial$nbCM,refSpatial$statutPAMPA,sum,na.rm=T)  # pour le linéaire côtier des statuts PAMPA
    surfaceGr<-tapply(refSpatial$nbCM,refSpatial$groupe,sum,na.rm=T)  # pour le linéaire côtier des groupes de zone
    surfaceTot<-tapply(refSpatial$nbCM,refSpatial$AMP,sum,na.rm=T)   # pour la surface totale de l'AMP 
    titreSurface<-"par mouillage"      
  }  
  names(surfaceZo)<-unique(refSpatial$codeZone)
  names(surfaceGr)<-unique(refSpatial$groupe) 
  names(surfaceZ)<-unique(refSpatial$statutPAMPA)
  names(surfaceTot)<-unique(refSpatial$AMP)      
  assign("surfaceZo",surfaceZo,envir=.GlobalEnv)
  assign("surfaceZ",surfaceZ,envir=.GlobalEnv)
  assign("surfaceGr",surfaceGr,envir=.GlobalEnv)
  assign("surfaceTot",surfaceTot,envir=.GlobalEnv)
  assign("titreSurface",titreSurface,envir=.GlobalEnv)  
}


# choix du niveau temporel désiré (JS/JW, types jours détaillés, mois, trimestre, année)
ChoixTemporel.f = function () {
  tt<-tktoplevel()
  tkwm.title(tt,"Selection du niveau temporel")
  scr <- tkscrollbar(tt, repeatinterval=5, command=function(...)tkyview(tl,...))
  tl<-tklistbox(tt,height=10,width=30,selectmode="single",background="white")
  tkgrid(tklabel(tt,text="Choix du niveau temporel à considérer \n"))
  tkgrid(tl,scr)
  tkgrid.configure(scr,rowspan=5,sticky="nsw")
  niveaux <- c("type de jours simplifié (JS_JW)","type de jours détaillé","mois","trimestre","saison")
  for (i in (1:length(niveaux)))  {
    tkinsert(tl,"end",niveaux[i])
    }
  tkselection.set(tl,0)  
  OnOK <- function()
    {
 	  niveauTempChoisi <- niveaux[as.numeric(tkcurselection(tl))+1]
    assign("niveauTempChoisi",niveauTempChoisi,envir=.GlobalEnv)
    tkdestroy(tt)
    msg <- paste("Vous avez choisi de calculer les métriques par : ",niveauTempChoisi,".",sep="")
    tkmessageBox(message=msg)
    }
  OK.but <-tkbutton(tt,text="   OK   ",command=OnOK)
  tkgrid(OK.but)
  tkfocus(tt)
  tkwait.window(tt)
  if (niveauTempChoisi=="type de jours détaillé") {
    niveauTemporel<-"typeJ"    # la colonne utilisée pour le calcul des métriques sera "typeJ"
    }  
  if (niveauTempChoisi=="type de jours simplifié (JS_JW)") {
    niveauTemporel<-"typeJsimp"    
    }   
  if (niveauTempChoisi=="mois") {
    niveauTemporel<-"moisAn"    
  }
  if (niveauTempChoisi=="trimestre") {
    niveauTemporel<-"trimestre"    
  }  
  if (niveauTempChoisi=="saison") {
    niveauTemporel<-"saison"   
  }   
  assign("niveauTemporel",niveauTemporel,envir=.GlobalEnv)  
}


# choix du niveau spatial désiré (zones, groupes, zonage PAMPA, choix de certaines zones)
ChoixSpatial.f = function () {
  tt<-tktoplevel()
  tkwm.title(tt,"Selection du niveau spatial")
  scr <- tkscrollbar(tt, repeatinterval=5, command=function(...)tkyview(tl,...))
  tl<-tklistbox(tt,height=10,width=30,selectmode="single",background="white")
  tkgrid(tklabel(tt,text="Choix du niveau spatial à considérer \n"))
  tkgrid(tl,scr)
  tkgrid.configure(scr,rowspan=5,sticky="nsw")
  niveaux <- c("zones","zones au choix","groupe de zones","zonage PAMPA","AMP totale") #,"site","station","codeSIH")
  for (i in (1:length(niveaux)))  {
    tkinsert(tl,"end",niveaux[i])
    }
  tkselection.set(tl,0)  
  OnOK <- function()
    {
 	  niveauChoisi <- niveaux[as.numeric(tkcurselection(tl))+1]
    assign("niveauChoisi",niveauChoisi,envir=.GlobalEnv)
    tkdestroy(tt)
    msg <- paste("Vous avez choisi de calculer les métriques par : ",niveauChoisi,".",sep="")
    tkmessageBox(message=msg)
    }
  OK.but <-tkbutton(tt,text="   OK   ",command=OnOK)
  tkgrid(OK.but)
  tkfocus(tt)
  tkwait.window(tt)
  if (niveauChoisi=="zones") {
    niveauSpatial<-"zone"    # la colonne utilisée pour le calcul des métriques sera "zone"
    surface<-surfaceZo        # la surface utilisée est celle des zones
    }  
  if (niveauChoisi=="zones au choix") {
    ChoixZonesdInteret.f()   # restriction de freqtot aux zones choisies
    niveauSpatial<-"zone"
    surface<-surfaceZo        
    }   
  if (niveauChoisi=="groupe de zones") {
    niveauSpatial<-"groupe"
    surface<-surfaceGr        
  }  
  if (niveauChoisi=="zonage PAMPA") {
    niveauSpatial<-"statut"      
    surface<-surfaceZ        
  } 
  if (niveauChoisi=="AMP totale") {
    niveauSpatial<-"amp"      
    surface<-surfaceTot        
  }    
  assign("niveauSpatial",niveauSpatial,envir=.GlobalEnv)  
  assign("surface",surface,envir=.GlobalEnv)  
}

# choix des zones à étudier plus spécifiquement
ChoixZonesdInteret.f = function () {
  tt<-tktoplevel()
  tkwm.title(tt,"Selection des zones d'intérêt")
  scr <- tkscrollbar(tt, repeatinterval=5, command=function(...)tkyview(tl,...))
  tl<-tklistbox(tt,height=30,width=30,selectmode="multiple",background="white")
  tkgrid(tklabel(tt,text="Choix des zones à représenter \n \n ATTENTION, par défaut la première ligne est sélectionnée \n"))
  tkgrid(tl,scr)
  tkgrid.configure(scr,rowspan=5,sticky="nsw")
  zonesDispo <- sort(as.character(unique(refSpatial$codeZone)))
  for (i in (1:length(unique(zonesDispo))))  {
    tkinsert(tl,"end",zonesDispo[i])
    }
  tkselection.set(tl,0)  
  OnOK <- function()
    {
 	  zonesChoisies <- zonesDispo[as.numeric(tkcurselection(tl))+1]
 	  freqtot<-FREQTOT[1,]
 	  for (i in 1:length(zonesChoisies)){
 	    freqtot<-rbind(freqtot,subset(FREQTOT,FREQTOT$zone==zonesChoisies[i]))
 	    }
    freqtot<-freqtot[-1,]
    assign("freqtot",freqtot,envir=.GlobalEnv)
    assign("freqtotRef",freqtot,envir=.GlobalEnv)
 	  capturesCh<-captures[1,]
 	  for (i in 1:length(zonesChoisies)){
 	    capturesCh<-rbind(capturesCh,subset(captures,captures$zone==zonesChoisies[i]))
 	    }
    capturesCh<-capturesCh[-1,]
    assign("captures",capturesCh,envir=.GlobalEnv)
 	  capturesC<-captures2[1,]
 	  for (i in 1:length(zonesChoisies)){
 	    capturesC<-rbind(capturesC,subset(captures2,captures2$zone==zonesChoisies[i]))
 	    }
    capturesC<-capturesC[-1,]
    assign("captures2",capturesC,envir=.GlobalEnv)
    tkdestroy(tt)
    msg <- paste("Vous avez choisi les zones suivantes : ",list(zonesChoisies),".",sep="")
    tkmessageBox(message=msg)
    }
  OK.but <-tkbutton(tt,text="   OK   ",command=OnOK)
  tkgrid(OK.but)
  tkfocus(tt)
  tkwait.window(tt)
}


# choix des activités à étudier plus spécifiquement
ChoixActdInteret.f = function () {
  tt<-tktoplevel()
  tkwm.title(tt,"Selection des activités d'intérêt")
  scr <- tkscrollbar(tt, repeatinterval=5, command=function(...)tkyview(tl,...))
  tl<-tklistbox(tt,height=30,width=30,selectmode="multiple",background="white")
  tkgrid(tklabel(tt,text="Choix des activités à représenter \n \n ATTENTION, par défaut la première ligne est sélectionnée \n"))
  tkgrid(tl,scr)
  tkgrid.configure(scr,rowspan=5,sticky="nsw")
  actDispo <- sort(as.character(unique(unique(freqtot$act1),unique(freqtot$act2))))
  for (i in (1:length(actDispo)))  {
    tkinsert(tl,"end",actDispo[i])
    }
  tkselection.set(tl,0)  
  OnOK <- function()
    {
 	  actChoisies <- actDispo[as.numeric(tkcurselection(tl))+1]
    assign("actChoisies",actChoisies,envir=.GlobalEnv)
    assign("freqtotRef",freqtot,envir=.GlobalEnv)
 	  freqtotTrans<-freqtot[1,]
 	  for (i in 1:length(actChoisies)){
 	    freqtotTrans<-rbind(freqtotTrans,subset(freqtot,freqtot$act1==actChoisies[i]),subset(freqtot,freqtot$act2==actChoisies[i]))
    }
    freqtotTrans<-freqtotTrans[-1,]
    assign("freqtot",freqtotTrans,envir=.GlobalEnv)
    tkdestroy(tt)
    msg <- paste("Vous avez choisi les activités suivantes : ",list(actChoisies),".",sep="")
    tkmessageBox(message=msg)
    }
  OK.but <-tkbutton(tt,text="   OK   ",command=OnOK)
  tkgrid(OK.but)
  tkfocus(tt)
  tkwait.window(tt)
}


# choix du facteur météo à étudier
ChoixMeteo.f = function () {
  tt<-tktoplevel()
  tkwm.title(tt,"Selection du facteur météo")
  scr <- tkscrollbar(tt, repeatinterval=5, command=function(...)tkyview(tl,...))
  tl<-tklistbox(tt,height=10,width=30,selectmode="single",background="white")
  tkgrid(tklabel(tt,text="Choix du facteur météo \n"))
  tkgrid(tl,scr)
  tkgrid.configure(scr,rowspan=5,sticky="nsw")
  niveaux <- c("la meteo","la nébulosité","la direction du vent","la force du vent","l état de la mer","la phase lunaire")
  for (i in (1:length(niveaux)))  {
    tkinsert(tl,"end",niveaux[i])
    }
  tkselection.set(tl,0)  
  OnOK <- function()
    {
 	  meteoChoisie <- niveaux[as.numeric(tkcurselection(tl))+1]
    assign("meteoChoisie",meteoChoisie,envir=.GlobalEnv)
    tkdestroy(tt)
    msg <- paste("Vous avez choisi de calculer les métriques selon : ",meteoChoisie,".",sep="")
    tkmessageBox(message=msg)
    }
  OK.but <-tkbutton(tt,text="   OK   ",command=OnOK)
  tkgrid(OK.but)
  tkfocus(tt)
  tkwait.window(tt)
  if (meteoChoisie=="la meteo") {
    meteo<-"meteo"    # la colonne utilisée pour le calcul des métriques sera "typeJ"
    }  
  if (meteoChoisie=="la nébulosité") {
    meteo<-"nebu"    
    }   
  if (meteoChoisie=="la direction du vent") {
    meteo<-"dir_vent"    
  }  
  if (meteoChoisie=="la force du vent") {
    meteo<-"force_vent"   
  } 
  if (meteoChoisie=="l état de la mer") {
    meteo<-"mer"    
  }  
  if (meteoChoisie=="la phase lunaire") {
    meteo<-"lune"   
  }   
  assign("meteo",meteo,envir=.GlobalEnv)  
}


# Choix de l'engin d'intérêt
ChoixEngin.f = function () {
  tt<-tktoplevel()
  tkwm.title(tt,"Selection de l'engin")
  scr <- tkscrollbar(tt, repeatinterval=5, command=function(...)tkyview(tl,...))
  tl<-tklistbox(tt,height=10,width=30,selectmode="single",background="white")
  tkgrid(tklabel(tt,text="Choix de l'engin à considérer \n"))
  tkgrid(tl,scr)
  tkgrid.configure(scr,rowspan=5,sticky="nsw")
  niveaux <- c("la chasse sous-marine","la ligne","le filet","le casier","la pêche à pied")
  for (i in (1:length(niveaux)))  {
    tkinsert(tl,"end",niveaux[i])
    }
  tkselection.set(tl,0)  
  OnOK <- function()
    {
 	  enginChoisi <- niveaux[as.numeric(tkcurselection(tl))+1]
    assign("enginChoisi",enginChoisi,envir=.GlobalEnv)
    tkdestroy(tt)
    msg <- paste("Vous avez choisi de calculer les métriques pour : ",enginChoisi,".",sep="")
    tkmessageBox(message=msg)
    }
  OK.but <-tkbutton(tt,text="   OK   ",command=OnOK)
  tkgrid(OK.but)
  tkfocus(tt)
  tkwait.window(tt)
  if (enginChoisi=="la chasse sous-marine") {
    especesInteret<-rbind(subset(Interet,Interet$interetCHA=="TR"),subset(Interet,Interet$interetCHA=="AR"))
    listeEspece<-especesInteret
    capturesEngin<-subset(captures,captures$engin=="fusil_harpon")
    capturesEngin2<-subset(captures2,captures2$engin=="fusil_harpon")
    }  
  if (enginChoisi=="la ligne") {
    especesInteret<-rbind(subset(Interet,Interet$interetLG=="TR"),subset(Interet,Interet$interetLG=="AR"))
    listeEspece<-especesInteret
    capturesEngin<-subset(captures,captures$engin=="ligne")  
    capturesEngin2<-subset(captures2,captures2$engin=="ligne")  
    }   
  if (enginChoisi=="le filet") {
    especesInteret<-rbind(subset(Interet,Interet$interetFIL=="TR"),subset(Interet,Interet$interetFIL=="AR"))
    listeEspece<-especesInteret
    capturesEngin<-subset(captures,captures$engin=="filet")
    capturesEngin2<-subset(captures2,captures2$engin=="filet")
  }  
  if (enginChoisi=="le casier") {
    especesInteret<-rbind(subset(Interet,Interet$interetCAS=="TR"),subset(Interet,Interet$interetCAS=="AR"))
    listeEspece<-especesInteret
    capturesEngin<-subset(captures,captures$engin=="casier")
    capturesEngin2<-subset(captures2,captures2$engin=="casier")
  } 
  if (enginChoisi=="la pêche à pied") {
    especesInteret<-rbind(subset(Interet,Interet$interetPP=="TR"),subset(Interet,Interet$interetPP=="AR"))
    listeEspece<-especesInteret
    capturesEngin<-subset(captures,captures$engin=="main")
    capturesEngin2<-subset(captures2,captures2$engin=="main")
  }    
  assign("listeEspece",listeEspece,envir=.GlobalEnv)  
  assign("capturesEngin",capturesEngin,envir=.GlobalEnv)
  assign("capturesEngin2",capturesEngin2,envir=.GlobalEnv)  
  assign("especesInteret",especesInteret,envir=.GlobalEnv)   
}

### fonction pour choisir l'activité de pêche souhaitée pour le calcul des captures et rendements du jour
ChoixActivitePeche.f = function (){
  tt<-tktoplevel()
  tkwm.title(tt,"Selection de l'activité de pêche d'intérêt")
  scr <- tkscrollbar(tt, repeatinterval=5, command=function(...)tkyview(tl,...))
  tl<-tklistbox(tt,height=30,width=30,selectmode="single",background="white")
  tkgrid(tklabel(tt,text="Choix de l'activité de pêche à représenter \n \n ATTENTION, par défaut la première ligne est sélectionnée \n"))
  tkgrid(tl,scr)
  tkgrid.configure(scr,rowspan=5,sticky="nsw")
  actDispo <- sort(as.character(unique(captures$act_peche)))
  for (i in (1:length(actDispo)))  {
    tkinsert(tl,"end",actDispo[i])
    }
  tkselection.set(tl,0)  
  OnOK <- function()
    {
 	  actPecChoisie <- actDispo[as.numeric(tkcurselection(tl))+1]
    assign("actPecChoisie",actPecChoisie,envir=.GlobalEnv)
 	  capturesAct<-subset(captures,captures$act_peche==actPecChoisie)
    assign("captures",capturesAct,envir=.GlobalEnv)
 	  capturesAct2<-subset(captures2,captures2$act_peche==actPecChoisie)
    assign("captures2",capturesAct2,envir=.GlobalEnv)
    tkdestroy(tt)
    msg <- paste("Vous avez choisi l'activité de pêche : ",actPecChoisie,".",sep="")
    tkmessageBox(message=msg)
    }
  OK.but <-tkbutton(tt,text="   OK   ",command=OnOK)
  tkgrid(OK.but)
  tkfocus(tt)
  tkwait.window(tt)
}

### fonction pour choisir l'activité de pêche souhaitée pour le calcul des captures et rendements du jour
ChoixTechniquePeche.f = function (){
  tt<-tktoplevel()
  tkwm.title(tt,"Selection de la technique de pêche d'intérêt")
  scr <- tkscrollbar(tt, repeatinterval=5, command=function(...)tkyview(tl,...))
  tl<-tklistbox(tt,height=30,width=30,selectmode="single",background="white")
  tkgrid(tklabel(tt,text="Choix de la technique de pêche à représenter \n \n ATTENTION, par défaut la première ligne est sélectionnée \n"))
  tkgrid(tl,scr)
  tkgrid.configure(scr,rowspan=5,sticky="nsw")
  techDispo <- sort(as.character(unique(captures$code_engin)))
  for (i in (1:length(techDispo)))  {
    tkinsert(tl,"end",techDispo[i])
    }
  tkselection.set(tl,0)  
  OnOK <- function()
    {
 	  techPecChoisie <- techDispo[as.numeric(tkcurselection(tl))+1]
    assign("techPecChoisie",techPecChoisie,envir=.GlobalEnv)
 	  capturesTech<-subset(captures,captures$code_engin==techPecChoisie)
    assign("captures",capturesTech,envir=.GlobalEnv)
 	  capturesTech2<-subset(captures2,captures2$code_engin==techPecChoisie)
    assign("captures2",capturesTech2,envir=.GlobalEnv)
    tkdestroy(tt)
    msg <- paste("Vous avez choisi la technique de pêche : ",techPecChoisie,".",sep="")
    tkmessageBox(message=msg)
    }
  OK.but <-tkbutton(tt,text="   OK   ",command=OnOK)
  tkgrid(OK.but)
  tkfocus(tt)
  tkwait.window(tt)
}


### fonction de restitution des jeux de données initiaux
RestitutionDonnees.f = function () {
  freqtot<-FREQTOT
  peche<-PECHE
  pecheQ<-PECHEQ
  captures<-CAPTURES
  captures2<-CAPTURES2
  capturesAn<-CAPTURESAN
  plaisance<-PLAISANCE
  plongee<-PLONGEE
  excursion<-EXCURSION
  tousQuest<-TOUSQUEST
  assign("freqtot",freqtot,envir=.GlobalEnv)
  assign("freqtotRef",freqtot,envir=.GlobalEnv)
  assign("peche",peche,envir=.GlobalEnv)
  assign("pecheQ",pecheQ,envir=.GlobalEnv)
  assign("captures",captures,envir=.GlobalEnv)
  assign("captures2",captures2,envir=.GlobalEnv)
  assign("capturesAn",capturesAn,envir=.GlobalEnv)
  assign("plaisance",plaisance,envir=.GlobalEnv)
  assign("plongee",plongee,envir=.GlobalEnv)
  assign("excursion",excursion,envir=.GlobalEnv)
  assign("tousQuest",tousQuest,envir=.GlobalEnv)
  assign("freqtotModif",freqtot,envir=.GlobalEnv)
  assign("freqtotRefModif",freqtot,envir=.GlobalEnv)
  assign("pecheModif",peche,envir=.GlobalEnv)
  assign("pecheQModif",pecheQ,envir=.GlobalEnv)
  assign("capturesModif",captures,envir=.GlobalEnv)
  assign("captures2Modif",captures2,envir=.GlobalEnv)
  assign("capturesAnModif",capturesAn,envir=.GlobalEnv)
  assign("plaisanceModif",plaisance,envir=.GlobalEnv)
  assign("plongeeModif",plongee,envir=.GlobalEnv)
  assign("excursionModif",excursion,envir=.GlobalEnv)
  assign("tousQuestModif",tousQuest,envir=.GlobalEnv)

  MiseajourTableau.f(tclarray)
  gestionMSGaide.f("restitution")  
  ### calcul des limites des barplot
  limActTot<-c(0,length(unique(tousQuest$activite)))
  assign("limActTot",limActTot,envir=.GlobalEnv)  
  limActDet<-c(0,length(unique(tousQuest$activitePec)))
  assign("limActDet",limActDet,envir=.GlobalEnv)
}


################################################################################
