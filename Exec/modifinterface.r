ajoutMenuBenthos.f = function(TabRecouvrementEspUnit)
{
    ## tkadd(habitat,"command",label="Hist % de recouvrement Total",command=PartRecouvrementTot.f(TabRecouvrementEspUnit))
    ## tkadd(habitat,"command",label="Hist % de recouvrement par espèce",command=PartRecouvrementEsp.f(TabRecouvrementEspUnit))
    ## tkadd(habitat,"command",label="Hist % de recouvrement par unitobs",command=PartRecouvrementUnitobs.f(TabRecouvrementEspUnit))
}

ajoutMenuExport.f = function()
{
    tkadd(habitat,"command",label="Calcul % de recouvrement",command=test())
}

ModifierMenuApresImport.f = function()
{

    print("fonction ModifierMenuApresImport activée")
    ## tkinvoke(statut,state="normal")
    tkentryconfigure(topMenu,1,state="normal")
    tkentryconfigure(topMenu,2,state="normal")
    tkentryconfigure(topMenu,5,state="normal")
    tkentryconfigure(topMenu,6,state="normal")
    tkentryconfigure(pampainfos,2,state="normal")
    tkentryconfigure(pampainfos,3,state="normal")
    tkentryconfigure(pampainfos,4,state="normal")
    if (unique(unitobs$type) == "LIT")
    {
        ## tkadd(selection,"checkbutton",label="Par Catégories Benthiques",variable=SelectBenth,onvalue=1,offvalue=0,command = SelectionUneCatBenth.f)
        tkentryconfigure(topMenu,4,state="normal")   # traitement benthos actif si c'est du LIT
        tkentryconfigure(selection,3,state="normal") # traitement benthos actif si c'est du LIT
    }else{
        tkentryconfigure(topMenu,3,state="normal") #traitement standart actif si ce n'est pas du benthos
    }
    NbEsp<-length(unique(obs$code_espece))
    tkconfigure(ResumerSituationEspecesSelectionnees,text=paste("-> Nombre d'espèces concernées : ",NbEsp))
    Nbunitobs<-length(unique(obs$unite_observation))
    tkconfigure(ResumerSituationUnitobsSelectionnees,text=paste("-> Nombre  d'unités d'observations concernées : ",Nbunitobs))
}

MiseajourTableau.f = function(tclarray)
{
    ## ############# Mise à jour des valeurs dans le tableau #############
    tclarray[[1,1]] <- sub(paste(nameWorkspace,"/Data/",sep=""),'',fileName1)
    tclarray[[1,2]] <- dim(unitobs)[1]
    tclarray[[1,3]] <- dim(unitobs)[2]
    tclarray[[2,1]] <- sub(paste(nameWorkspace,"/Data/",sep=""),'',fileName2)
    tclarray[[2,2]] <- dim(obs)[1]
    tclarray[[2,3]] <- dim(obs)[2]
    tclarray[[3,1]] <- sub(paste(nameWorkspace,"/Data/",sep=""),'',fileName3)
    tclarray[[3,2]] <- dim(especes)[1]
    tclarray[[3,3]] <- dim(especes)[2]
}

ModifierInterfaceApresSelection.f = function(Critere,Valeur)
{
    print("fonction ModifierInterfaceApresSelection activée")

    tkinsert(table1,"cols","end",1)
    tclarray[[0,4]] <- "Sélection"
    tclarray[[2,4]] <- dim(obs)[1]
    tkconfigure(MonCritere,text=Critere)
    tkconfigure(MesEnregistrements,text=Valeur)
    NbEsp<-length(unique(obs$code_espece))
    tkconfigure(ResumerSituationEspecesSelectionnees, text = paste("-> Nombre d'espèces concernées : ",
                                                      NbEsp))
    Nbunitobs<-length(unique(obs$unite_observation))
    tkconfigure(ResumerSituationUnitobsSelectionnees,text=paste("-> Nombre d'unités d'observations dans le fichier d'observations : ",Nbunitobs))
    if (Jeuxdonnescoupe==1)
    {
        tkconfigure(button.DataRestore, state="normal")
    }
    ## tkconfigure(frameLower,text=paste(length(obs$code_espece[obs$code_espece==fa])," enregistrements concernés",sep=""))

}

ModifierInterfaceApresRestore.f = function(Critere="Aucun",Valeur="NA")
{
    print("fonction ModifierInterfaceApresRestore.f activée")


    tkdelete(table1,"cols","end",1)

    tclarray[[0,4]] <- "Sélection"
    tclarray[[2,4]] <- dim(obs)[1]
    tkconfigure(MonCritere,text=Critere)
    tkconfigure(MesEnregistrements,text=Valeur)
    NbEsp<-length(unique(obs$code_espece))
    tkconfigure(ResumerSituationEspecesSelectionnees,text=paste("-> Nombre d'espèces concernées : ",NbEsp))
    Nbunitobs<-length(unique(obs$unite_observation))
    tkconfigure(ResumerSituationUnitobsSelectionnees,text=paste("-> Nombre d'unités d'observations dans le fichier d'observations : ",Nbunitobs))
    tkconfigure(button.DataRestore, state="disabled")
}
