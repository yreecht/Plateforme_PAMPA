
################################################################################
## CREATION DES TABLES DE BASE
##      Calcul par unit� d'observation par esp�ce : unitesp.f
##      Calcul par unit� d'observation toutes esp�ces confondues : unit.f
################################################################################


################################################################################
## Nom     : unitespta.f
## Objet   : calcul des m�triques par unit� d'observation / esp�ce et classe
##            de taille
## Input   : tables "obs" et "unitobs"
## Output  : table "unitespta"
################################################################################

unitespta.f <- function(){
    print("fonction unitespta.f activ�e")

    ## creation des classes de tailles si champ classe taille contient uniquement des NA [!!!] uniquement [???]
    if (any(is.na(obs$classe_taille)))  ## (NA %in% unique(obs$classe_taille)==TRUE) # [!!!]
    {
        classeTaille.f()
    }else{
        ## si le champ taille contient uniquement des valeurs a NA
        if (all(is.na(obs$taille))) # (length(unique(obs$taille))==1 & NA %in% unique(obs$taille)==TRUE)
                                   # # [!!!] remplacer par all(is.na()) ??
                                        # !!Non!! + inconsistence avec la premi�re clause (il ne devrait pas y avoir de
                                        # NAs ici) !!Non!!
                                        # [yr: 13/08/2010]
        {
            ct <- 2
        }else{
            ct <- 1
        }
    }
    assign("ct", ct, envir=.GlobalEnv)  # � quoi �a sert au final [???]

    if (ct == 1 || !all(is.na(obs$classe_taille)))
    {
        ## #########################################################################################################
        ## Creation de la table par unite d'observation, par espece et par classe de taille et par rotation si SVR :

        ## Nombre d'individus :
        if (unique(unitobs$type) == "SVR")
        {
            switch(getOption("PAMPA.SVR.interp"),
                   "extended"={
                       statRotations <- statRotation.extended.f(facteurs=c("unite_observation", "code_espece",
                                                                           "classe_taille"))
                   },
                   "basic"={
                       statRotations <- statRotation.basic.f(facteurs=c("unite_observation", "code_espece",
                                                             "classe_taille"))
                   },
                   stop(paste("Y'a un truc qui cloche dans les options d'interpolations : ",
                              "\n\tcontactez le support technique !", sep=""))
                   )

            ## Moyenne pour les vid�os rotatives (habituellement 3 rotation) :
            unitesptaT <- statRotations[["nombresMean"]]
        }else{
            ## Somme des nombres d'individus :
            unitesptaT <- tapply(obs$nombre,
                                 as.list(obs[ , c("unite_observation", "code_espece", "classe_taille")]),
                                 sum, na.rm = TRUE) # [!!!] nombres � z�ro [???] [yr: 17/08/2010]

            ## Absences consid�r�e comme "vrais z�ros" :
            unitesptaT[is.na(unitesptaT)] <- 0
        }


        unitespta <- as.data.frame(as.table(unitesptaT), responseName="nombre")
        unitespta$unitobs <- unitespta$unite_observation # Pour compatibilit� uniquement !!!

        unitespta$classe_taille[unitespta$classe_taille == ""] <- NA

        ## Si les nombres sont des entiers, leur redonner la bonne classe :
        if (isTRUE(all.equal(unitespta$nombre, as.integer(unitespta$nombre))))
        {
            unitespta$nombre <- as.integer(unitespta$nombre)
        }else{}

        ## Stats sur les nombres pour les (g�n�ralement 3) rotations :
        if (unique(unitobs$type) == "SVR")
        {
            unitespta$nombreMax <- as.vector(statRotations[["nombresMax"]])
            unitespta$nombreSD <- as.vector(statRotations[["nombresSD"]])
        }else{}

        ## ######################################################
        ## tailles moyennes ponderees si champ taille renseigne :
        if (!all(is.na(obs$taille)))        # (length(unique(obs$taille))>1)
        {
            unitespta$taille_moy <- as.vector(tapply(seq(length.out=nrow(obs)),
                                                     as.list(obs[ , c("unite_observation",
                                                                      "code_espece", "classe_taille")]),
                                                     function(ii)
                                                 {
                                                     weighted.mean(obs$taille[ii], obs$nombre[ii])
                                                 }))
        }

        ## ######################################################################
        ## sommes des biomasses par espece par unitobs et par classes de taille :
        if (!all(is.na(obs$poids)))      # (length(unique(obs$biomasse))>1)
        {
            ## ##################################################
            ## biomasse :
            unitespta$biomasse <- as.vector(tapply(obs$poids,
                                                   as.list(obs[ , c("unite_observation",
                                                                    "code_espece", "classe_taille")]),
                                                   function(x)
                                               {
                                                   if (all(is.na(x))) {return(NA)}else{return(sum(x, na.rm=TRUE))}
                                               }))

            ## C'est b�te que la biomasse soit calcul�e comme �a... il faut faire des corrections
            ## pour les vid�os rotatives :
            if (unique(unitobs$type) == "SVR")
            {
                unitespta$biomasse <- unitespta$biomasse * unitespta$nombre /
                    as.vector(tapply(obs$nombre,
                                     as.list(obs[ , c("unite_observation",
                                                      "code_espece", "classe_taille")]),
                                     function(x)
                                 {
                                     if (all(is.na(x))) {return(NA)}else{return(sum(x, na.rm=TRUE))}
                                 }))

                ## Biomasse max
                unitespta$biomasseMax <- unitespta$biomasse * unitespta$nombreMax /
                    as.vector(tapply(obs$nombre,
                                     as.list(obs[ , c("unite_observation",
                                                      "code_espece", "classe_taille")]),
                                     function(x)
                                 {
                                     if (all(is.na(x))) {return(NA)}else{return(sum(x, na.rm=TRUE))}
                                 }))

            }else{}


            ## Certains NAs correspondent � des vrai z�ros :

            ## Especes pour lesquelles aucune biomasse n'est calcul�e.
            espSansBiom <- tapply(unitespta$biomasse, unitespta$code_espece,
                                  function(x)all(is.na(x) | x == 0))
            espSansBiom <- names(espSansBiom)[espSansBiom]

            ## Ajout des vrais z�ros :
            unitespta$biomasse[is.na(unitespta$biomasse) &
                               unitespta$nombre == 0 &
                               !is.element(unitespta$code_espece, espSansBiom)] <- 0


            if (unique(unitobs$type) == "SVR")
            {
                ## On divise par la surface du cercle contenant l'observation la plus lointaine :
                unitespta$biomasse <- unitespta$biomasse /
                    (pi * (as.vector(tapply(obs$dmin,
                                            as.list(obs[ , c("unite_observation", "code_espece")]),
                                            max, na.rm=TRUE)))^2) # Recycl� 3X.
            }else{
                ## on divise la biomasse par dimObs1*dimObs2
                unitespta$biomasse <- as.numeric(unitespta$biomasse) /
                    (unitobs$DimObs1[match(unitespta$unite_observation, unitobs$unite_observation)] *
                     unitobs$DimObs2[match(unitespta$unite_observation, unitobs$unite_observation)])
            }


        }else{
            ## alerte que les calculs de biomasse sont impossibles
            tkmessageBox(message=paste("Calcul de biomasse impossible - ",
                         "Les tailles ne sont pas renseign�es dans les observations", sep=""))
        }

        ## poids

        ## ##################################################
        ## poids
        unitespta$poids <- as.vector(tapply(obs$poids,
                                            as.list(obs[ , c("unite_observation",
                                                             "code_espece", "classe_taille")]),
                                            function(x)
                                        {
                                            ifelse(all(is.na(x)),
                                                   as.numeric(NA),
                                                   sum(x, na.rm=TRUE))
                                        }))

        ## Certains NAs correspondent � des vrai z�ros :
        if (!all(is.na(obs$poids)))
        {
            unitespta$poids[is.na(unitespta$poids) & unitespta$nombre == 0] <- 0
        }

        ## ##################################################
        ## poids moyen
        unitespta$poids_moyen <- apply(unitespta[ , c("nombre", "poids")], 1,
                                       function(x)
                                   {
                                       return(as.vector(ifelse(is.na(x[2]) || x[1] == 0,
                                                               as.numeric(NA),
                                                               x[2]/x[1])))
                                   })

        ## Presence - absence
        unitespta$pres_abs[unitespta$nombre > 0] <- as.integer(1) # pour avoir la richesse sp�cifique en 'integer'.1
        unitespta$pres_abs[unitespta$nombre == 0] <- as.integer(0) # pour avoir la richesse sp�cifique en 'integer'.0

        ## calcul densites (pour les p�ches, ce calcul correspond au CPUE en nombre par espece)
        ## [densit�]
        if (unique(unitobs$type) != "SVR")
        {
            unitespta$densite <- unitespta$nombre /
                (unitobs$DimObs1[match(unitespta$unite_observation, unitobs$unite_observation)] *
                 unitobs$DimObs2[match(unitespta$unite_observation, unitobs$unite_observation)])
        }else{
            ## Densit� :
            unitespta$densite <- unitespta$nombre /
                (pi * (as.vector(tapply(obs$dmin,
                                        as.list(obs[ , c("unite_observation", "code_espece")]),
                                        max, na.rm=TRUE)))^2)

            ## Vrais z�ros :
            unitespta$densite[unitespta$nombre == 0 & !is.na(unitespta$nombre)] <- 0

            ## Densit� max :
            unitespta$densiteMax <- unitespta$nombreMax /
                (pi * (as.vector(tapply(obs$dmin,
                                        as.list(obs[ , c("unite_observation", "code_espece")]),
                                        max, na.rm=TRUE)))^2)

            ## Vrais z�ros :
            unitespta$densiteMax[unitespta$nombreMax == 0 & !is.na(unitespta$nombreMax)] <- 0
        }

        ## ajout des champs "an", "site", "statut_protection", "biotope", "latitude", "longitude" :
        unitespta <- cbind(unitespta,
                           unitobs[match(unitespta$unite_observation, unitobs$unite_observation),
                                   c("an", "site", "statut_protection", "biotope", "latitude", "longitude")])

        ## ##################################################
        ## Proportion d'abondance par classe de taille :
        abondance <- with(unitespta, tapply(densite, list(unite_observation, code_espece, classe_taille),
                                            function(x){x})) # -> tableau � 3D.

        ## Sommes d'abondances pour chaque unitobs pour chaque esp�ce :
        sommesCT <- apply(abondance, c(1, 2), sum, na.rm=TRUE)

        ## Calcul des proportions d'abondance -> tableau 3D :
        propAbondance <- sweep(abondance, c(1, 2), sommesCT, FUN="/")
        names(dimnames(propAbondance)) <- c("unite_observation", "code_espece", "classe_taille")

        ## Mise au format colonne + % :
        unitespta$prop.abondance.CL <- 100 * as.data.frame(as.table(propAbondance),
                                                           responseName="prop.abondance.CL",
                                                           stringsAsFactors=FALSE)$prop.abondance.CL

        ## ##################################################
        ## Proportion de biomasse par classe de taille :
        if (!is.null(unitespta$biomasse))
        {
            biomasses <- with(unitespta, tapply(biomasse,
                                                list(unite_observation, code_espece, classe_taille),
                                                function(x){x})) # -> tableau � 3D.

            ## Sommes de biomasses pour chaque unitobs pour chaque esp�ce :
            sommesCT <- apply(biomasses, c(1, 2), sum, na.rm=TRUE)

            ## Calcul des proportions de biomasse -> tableau 3D :
            propBiomasse <- sweep(biomasses, c(1, 2), sommesCT, FUN="/")
            names(dimnames(propBiomasse)) <- c("unite_observation", "code_espece", "classe_taille")

            ## Mise au format colonne + % :
            unitespta$prop.biomasse.CL <- 100 * as.data.frame(as.table(propBiomasse),
                                                              responseName="prop.biomasse.CL",
                                                              stringsAsFactors=FALSE)$prop.biomasse.CL
        }else{}

        ## #################################################
        ## on renomme densite et biomasse en CPUE
        ## pour les jeux de donn�es p�che
        if (is.peche.f())                   # length(typePeche)>1
        {
            unitespta$CPUE <- unitespta$densite
            unitespta$densite <- NULL
            unitespta$CPUEbiomasse <- unitespta$biomasse # Fonctionne m�me si biomasse n'existe pas.
            unitespta$biomasse <- NULL
        }

        assign("unitespta", unitespta, envir=.GlobalEnv)
        write.csv(unitespta[ , colnames(unitespta) != "unite_observation"],
                  file=paste(nameWorkspace, "/FichiersSortie/UnitobsEspeceClassetailleMetriques.csv", sep=""),
                  row.names = FALSE)
        print(paste("La table par unite d'observation / espece / classe de taille",
                    " a ete creee: UnitobsEspeceClassetailleMetriques.csv", sep=""))
    }else{
        message("M�triques par classe de taille incalculables")
        assign("unitespta",
               data.frame("unite_observation"=NULL, "code_espece"=NULL, "nombre"=NULL,
                          "poids"=NULL, "poids_moyen"=NULL, "densite"=NULL,
                          "pres_abs"=NULL, "site"=NULL, "biotope"=NULL,
                          "an"=NULL, "statut_protection"=NULL),
               envir=.GlobalEnv)
    }
} #fin unitespta.f()


################################################################################
## Nom     : unitesp.f
## Objet   : calcul des m�triques par unit� d'observation / esp�ce
## Input   : tables "obs" et "unitobs"
## Output  : table "unitesp" et "listespunit"
################################################################################

unitesp.f <- function(){


    print("fonction unitesp.f activ�e")

    ## ##################################################
    ## somme des abondances


    ## Si video rotative, on divise par le nombre de rotation
    if (unique(unitobs$type) == "SVR")
    {
        switch(getOption("PAMPA.SVR.interp"),
                   "extended"={
                       statRotations <- statRotation.extended.f(facteurs=c("unite_observation", "code_espece"))
                   },
                   "basic"={
                       statRotations <- statRotation.basic.f(facteurs=c("unite_observation", "code_espece"))
                   },
                   stop(paste("Y'a un truc qui cloche dans les options d'interpolations : ",
                              "\n\tcontactez le support technique !", sep=""))
                   )

        ## Moyenne pour les vid�os rotatives (habituellement 3 rotation) :
        unitespT <- statRotations[["nombresMean"]]
    }else{
        unitespT <- tapply(obs$nombre,
                           as.list(obs[ , c("unite_observation", "code_espece")]),
                           sum, na.rm = TRUE)

        unitespT[is.na(unitespT)] <- 0      # Les NAs correspondent � des vrais z�ros.
    }


    unitesp <- as.data.frame(as.table(unitespT), responseName="nombre")

    if (isTRUE(all.equal(unitesp$nombre, as.integer(unitesp$nombre))))
    {
        unitesp$nombre <- as.integer(unitesp$nombre)
    }else{}

    ## Si video rotative, stat sur les rotations :
    if (unique(unitobs$type) == "SVR")
    {
        unitesp$nombreMax <- as.vector(statRotations[["nombresMax"]])
        unitesp$nombreSD <- as.vector(statRotations[["nombresSD"]])
    }else{}

    if (!is.benthos.f())                               # unique(unitobs$type) != "LIT"
    {

        ## ##################################################
        ## tailles moyennes ponderees
        if (!all(is.na(obs$taille)))
        {
            unitesp$taille_moy <- as.vector(tapply(seq(length.out=nrow(obs)),
                                                   list(obs$unite_observation, obs$code_espece),
                                                   function(ii)
                                               {
                                                   weighted.mean(obs$taille[ii], obs$nombre[ii])
                                               }))
        }else{}

        if (!all(is.na(obs$poids)))
        {
            unitesp$biomasse <- as.vector(tapply(obs$poids,
                                                 list(obs$unite_observation, obs$code_espece),
                                                 function(x)
                                             {
                                                 if (all(is.na(x))) {return(NA)}else{return(sum(x, na.rm=TRUE))}
                                             }))##  /
            ## (unitobs$DimObs1[match(unitesp$unite_observation, unitobs$unite_observation)] *
            ##  unitobs$DimObs2[match(unitesp$unite_observation, unitobs$unite_observation)])



            ## C'est b�te que la biomasse soit calcul�e comme �a... il faut faire des corrections pour les vid�os rotatives :
            if (unique(unitobs$type) == "SVR")
            {
                unitesp$biomasse <- unitesp$biomasse * unitesp$nombre /
                    as.vector(tapply(obs$nombre,
                                     as.list(obs[ , c("unite_observation",
                                                      "code_espece")]),
                                     function(x)
                                 {
                                     if (all(is.na(x))) {return(NA)}else{return(sum(x, na.rm=TRUE))}
                                 }))

                ## Biomasse max
                unitesp$biomasseMax <- unitesp$biomasse * unitesp$nombreMax /
                    as.vector(tapply(obs$nombre,
                                     as.list(obs[ , c("unite_observation",
                                                      "code_espece")]),
                                     function(x)
                                 {
                                     if (all(is.na(x))) {return(NA)}else{return(sum(x, na.rm=TRUE))}
                                 }))

            }else{}


            ## Certains NAs correspondent � des vrai z�ros :

            ## Especes pour lesquelles aucune biomasse n'est calcul�e.
            espSansBiom <- tapply(unitesp$biomasse, unitesp$code_espece,
                                  function(x)all(is.na(x) | x == 0))
            espSansBiom <- names(espSansBiom)[espSansBiom]

            ## Ajout des vrais z�ros :
            unitesp$biomasse[is.na(unitesp$biomasse) &
                             unitesp$nombre == 0 &
                             !is.element(unitesp$code_espece, espSansBiom)] <- 0

            if (unique(unitobs$type) == "SVR")
            {
                ## On divise par la surface du cercle contenant l'observation la plus lointaine :
                unitesp$biomasse <- unitesp$biomasse /
                    (pi * (as.vector(tapply(obs$dmin,
                                            as.list(obs[ , c("unite_observation", "code_espece")]),
                                            max, na.rm=TRUE)))^2)
            }else{
                ## on divise la biomasse par dimObs1*dimObs2
                unitesp$biomasse <- as.numeric(unitesp$biomasse) /
                    (unitobs$DimObs1[match(unitesp$unite_observation, unitobs$unite_observation)] *
                     unitobs$DimObs2[match(unitesp$unite_observation, unitobs$unite_observation)])
            }
        }else{}

        ## ##################################################
        ## poids
        unitesp$poids <- as.vector(tapply(obs$poids,
                                          list(obs$unite_observation, obs$code_espece),
                                          function(x) {if (all(is.na(x))) {return(NA)}else{return(sum(x, na.rm=TRUE))}}))

        ## Certains NAs correspondent � des vrai z�ros :
        if (!all(is.na(obs$poids)))
        {
            unitesp$poids[is.na(unitesp$poids) & unitesp$nombre == 0] <- 0
        }

        ## ##################################################
        ## poids moyen
        unitesp$poids_moyen <- apply(unitesp[ , c("nombre", "poids")], 1,
                                     function(x)
                                 {
                                     return(as.vector(ifelse(is.na(x[2]) || x[1] == 0,
                                                             as.numeric(NA),
                                                             x[2]/x[1])))
                                 })

        ## ##################################################
        ## calcul densites (pour les p�ches, ce calcul correspond aux captures par unite d'effort)
        if (unique(unitobs$type) != "SVR")
        {
            unitesp$densite <- unitesp$nombre /
                (unitobs$DimObs1[match(unitesp$unite_observation, unitobs$unite_observation)] *
                 unitobs$DimObs2[match(unitesp$unite_observation, unitobs$unite_observation)])
        }else{
            unitesp$densite <- unitesp$nombre /
                (pi * (as.vector(tapply(obs$dmin,
                                        list(obs$unite_observation, obs$code_espece),
                                        max, na.rm=TRUE)))^2)

            ## Densit� max :
            unitesp$densiteMax <- unitesp$nombreMax /
                (pi * (as.vector(tapply(obs$dmin,
                                        as.list(obs[ , c("unite_observation", "code_espece")]),
                                        max, na.rm=TRUE)))^2)

            ## Vrais z�ros :
            unitesp$densiteMax[unitesp$nombreMax == 0 & !is.na(unitesp$nombreMax)] <- 0

        }

        ## Ajout des vrais z�ros :
        unitesp$densite[unitesp$nombre == 0 & !is.na(unitesp$nombre)] <- 0


    }else{ # cas LIT

        ## Pourcentage de recouvrement de chaque espece/categorie pour les couvertures biotiques et abiotiques
        s <- tapply(unitesp$nombre, unitesp$unite_observation, sum, na.rm=TRUE)
        unitesp$recouvrement <- as.vector(100 * unitesp$nombre /
                                          s[match(unitesp$unite_observation, rownames(s))]) ## [!!!] ajout 100 * [???]
        rm(s)

        ## Nombre de colonies
        ## V�rifier si pas de risque que des longueurs de transitions == 0 => besoin de mettre 0 � count dans ces cas l�
        ## [!!!]
        obs$count <- 1                  # [!!!] somme des obs$nombre > 0 [???]

        e <- tapply(obs$count, list(obs$unite_observation, obs$code_espece), sum, na.rm=TRUE)

        unitesp$colonie <- as.vector(e)
        unitesp$colonie[is.na(unitesp$colonie)] <- 0 # [???]
        unitesp$taille.moy.colonies <- apply(unitesp[ , c("nombre", "colonie")], 1,
                                             function(x){ifelse(x[2] == 0, NA, x[1] / x[2])})

        rm(e)
    }

    ## Creation de l'info Presence/Absence :
    unitesp$pres_abs[unitesp$nombre > 0] <- as.integer(1) # pour avoir la richesse sp�cifique en 'integer'.1
    unitesp$pres_abs[unitesp$nombre == 0] <- as.integer(0) # pour avoir la richesse sp�cifique en 'integer'.0

    unitesp <- cbind(unitesp,
                     unitobs[match(unitesp$unite_observation, unitobs$unite_observation),
                             c("site", "biotope", "an", "statut_protection")])

    ## on renomme densite en CPUE pour les jeux de donn�es p�che
    if (is.peche.f())                   # length(typePeche)>1
    {
        unitesp$CPUE <- unitesp$densite
        unitesp$densite <- NULL
        unitesp$CPUEbiomasse <- unitesp$biomasse
        unitesp$biomasse <- NULL
    }

    ## Ecriture du fichier des unit�s d'observations par esp�ce en sortie
    assign("unitesp", unitesp, envir=.GlobalEnv)
    print("La table par unite d'observation / espece a ete creee : UnitobsEspeceMetriques.csv")
    write.csv(unitesp, file=paste(NomDossierTravail, "UnitobsEspeceMetriques.csv", sep=""), row.names = FALSE)

    ## table avec la liste des especes presentes dans chaque transect
    listespunit <- unitesp## [unitesp$pres_abs != 0, ]
    listespunit <- listespunit[order(listespunit$code_espece), ]
    assign("listespunit", listespunit, envir=.GlobalEnv)
    print("La liste des especes presentes dans chaque transect a ete creee : ListeEspecesUnitobs.csv")

    write.csv(listespunit, file=paste(NomDossierTravail, "ListeEspecesUnitobs.csv", sep=""), row.names = FALSE)
} # fin unitesp.f()


################################################################################
## Nom     : unit.f
## Objet   : calcul des m�triques par unit� d'observation toutes esp�ces confondues
## Input   : tables "obs" et "unitobs"
## Output  : table "unit" et carte de la CPUE pour les donn�es de p�che NC
################################################################################

unit.f <- function(){

    print("fonction unit.f activ�e")

    unit <- as.data.frame(as.table(tapply(obs$nombre, obs$unite_observation, sum, na.rm = TRUE))
                          , responseName="nombre")
    colnames(unit)[1] = c("unitobs")

    unit$nombre[is.na(unit$nombre)] <- 0      # Les NAs correspondent � des vrais z�ros.

    if (!is.benthos.f())                # unique(unitobs$type) != "LIT"
    {

        ## biomasse par unite d'observation
        unit.b <- tapply(obs$poids, obs$unite_observation,
                         function(x){if (all(is.na(x))) {return(NA)}else{return(sum(x, na.rm=TRUE))}}) # Pour �viter
                                        # des sommes � z�ro l� o� seulement des NAs. ## sum)
        unit$biomasse <- unit.b[match(unit$unitobs, rownames(unit.b))]



        if (unique(unitobs$type) != "SVR")
        {
            ## calcul biomasse
            ## unit$nombre[is.na(unit$nombre)] <- 0 # as.integer() pour conserver des entiers [???]
            unit$biomasse <- as.numeric(unit$biomasse) /
                (unitobs$DimObs1[match(unit$unitobs, unitobs$unite_observation)] *
                 unitobs$DimObs2[match(unit$unitobs, unitobs$unite_observation)])
            ## unit$biomasse[is.na(unit$biomasse)] <- 0 # [!!!] encore une fois, quelle horreur  [yr: 13/08/2010]

            ## calcul densite
            unit.d <- tapply(obs$nombre, obs$unite_observation, sum, na.rm=TRUE)
            unit$densite <- unit.d[match(unit$unitobs, rownames(unit.d))]
            unit$densite <- as.numeric(unit$densite) /
                (unitobs$DimObs1[match(unit$unitobs, unitobs$unite_observation)] *
                 unitobs$DimObs2[match(unit$unitobs, unitobs$unite_observation)])
            unit$densite[is.na(unit$densite)] <- 0 # [!!!] v�rifier si c'est correct [yr: 17/08/2010]
        }else{
            ## calcul densite d'abondance
            unit$nombre <- unit$nombre / 3
            unit$nombre[is.na(unit$nombre)] <- 0 # as.integer() pour conserver des entiers ?
            unit$densite <- unit$nombre / (pi * 25)          # [!!!][???] Pourquoi la distance d'observation est fixe
                                                             # [!!!][???] dans ce cas-ci (5m) alors qu'elle est
            unit$biomasse <- unit$biomasse / (pi * 25)       # [!!!][???] "dynamique" dans unitesp.f() ?
        }

        ## Certains NAs correspondent � des vrai z�ros :
        if (!all(is.na(obs$poids)))  # Si les biomasses ne sont pas calculables, inutile de mettre les z�ros !
        {
            ## Ajout des vrais z�ros :
            unit$biomasse[is.na(unit$biomasse) & unit$nombre == 0] <- 0
        }

        ## Ajout des vrais z�ros de densit� :
        unitesp$densite[unitesp$nombre == 0 & !is.na(unitesp$nombre)] <- 0

        ## ##################################################
        ## calcul richesse specifique
        unit$richesse_specifique <- as.integer(tapply(unitesp$pres_abs,
                                                      unitesp$unite_observation, sum, na.rm=TRUE)) # chang� pour avoir
                                                        # des entiers.
        unit$richesse_specifique[is.na(unit$richesse_specifique)] <- as.integer(0) # pour conserver des entiers  # [!!!] v�rifier si c'est correct
                                        # [yr: 17/08/2010]

        ## calcul de l'indice de Simpson
        ## le calcul se fait sur les $nombre il n'y a donc aucune espece exclue pour le calcul de ces metriques
        unitespT <- tapply(obs$nombre, list(obs$unite_observation, obs$code_espece), sum, na.rm = TRUE)
        unitespT[is.na(unitespT)] <- 0 # as.integer() pour conserver des entiers ?
        ot <- apply(unitespT, 1, sum, na.rm = TRUE)
        a <- sweep(unitespT, 1, ot, FUN="/")
        sim <- a^2
        sim[is.nan(sim)] <- 0
        sim <- apply(sim, 1, sum, na.rm = TRUE)
        sim <- as.data.frame(sim)
        sim$sim <- as.numeric(sim$sim)
        unit$simpson <- NA                                          # inutile
        unit$simpson <- sim$sim[match(unit$unitobs, rownames(sim))] # inutile
        unit$simpson[is.na(unit$simpson)] <- 0
        unit$l.simpson <- 1 - unit$simpson
        rm(sim)

        ## calcul de l'indice de Shannon
        sha <- a*log(a) # en base e
        sha[is.nan(sha)] <- 0
        sha <- apply(sha, 1, na.rm = TRUE, sum)
        sha <- as.data.frame(sha)
        sha$sha <- as.numeric(sha$sha)
        unit$shannon <- NA
        unit$shannon <- -sha$sha[match(unit$unitobs, rownames(sha))]
        unit$shannon[is.na(unit$shannon)] <- 0
        rm(a, sha)

        ## calcul de l'indice de Pielou
        unit$pielou <- unit$shannon / log(unit$richesse_specifique)
        unit$pielou[is.na(unit$pielou)] <- 0

        ## calcul de l'indice de Hill
        unit$hill <- (1-unit$simpson) / exp(unit$shannon)
        unit$hill[is.na(unit$hill)] <- 0

        ## suppression de l'indice de shannon (non pertinent)
        unit$shannon <- NULL

        ## richesse specifique relative :  ## Remplacer par un "switch" ou m�me une construction plus
                                        # g�n�rique (e.g. construction et �valuation d'une expression d�pendant du site
                                        # �tudi�) [yreecht: 22/07/2010] OK [yr: 08/10/2010]

        ## Phylum(s) pr�sent(s) dans le jeux de donn�es :
        phylums <- as.character(unique(na.omit(especes$Phylum[match(obs$code_espece, especes$code_espece)])))

        ## RS relative par rapp. au nombre d'esp�ces du site :
        unit$RS.relative.site <- (unit$richesse_specifique /
                                  nrow(subset(especes,
                                              eval(parse(text=paste("Obs", siteEtudie, sep=""))) == "oui"))) * 100

        ## RS relative par rapp. au nombre d'esp�ces du site et du(des) phylum(s) concern�(s) (jeu de donn�es) :
        unit$RS.relative.site.phylum <- (unit$richesse_specifique /
                                         nrow(subset(especes,
                                                     eval(parse(text=paste("Obs", siteEtudie, sep=""))) == "oui" &
                                                     is.element(Phylum, phylums)))) * 100

        ## RS relative par rapp. au nombre d'esp�ces des donn�es :
        unit$RS.relative.donnees <- (unit$richesse_specifique /
                                     nrow(subset(especes,
                                                 is.element(code_espece, obs$code_espece)))) * 100

        ## ## RS relative par rapp. au nombre d'esp�ces des donn�es :
        ## Inutile : "RS.relative.donnees" est par d�finition limit�e au phylums pr�sents

        ## RS relative par rapp. au nombre d'esp�ces au niveau r�gional (OM ou m�diterrann�e) :
        unit$RS.relative.region <- (unit$richesse_specifique /
                                            nrow(especes)) * 100

        ## RS relative par rapp. au nombre d'esp�ces au niveau r�gional (OM ou m�diterrann�e) et
        ## du(des) phylum(s) concern�(s) (jeu de donn�es) :
        unit$RS.relative.region.phylum <- (unit$richesse_specifique /
                                            nrow(subset(especes, is.element(Phylum, phylums)))) * 100

    }

    ## ajout des champs "an", "site", "statut_protection", "biotope", "latitude", "longitude"
    unit <- cbind(unit,
                  unitobs[match(unit$unitobs, unitobs$unite_observation),
                          c("an", "site", "statut_protection", "biotope", "latitude", "longitude")])

    assign("unit", unit, envir=.GlobalEnv)

    ## calculs des indices de diversite taxonomique
    indicesDiv.f()

    ## le jeu de donnees doit comporter au moins 2 genres et 2 unite d'observations sinon taxa2dist ne fonctionne pas
    if (length(unique(sp.taxon$genre))>2)
    {
        ## formation d'un nouveau tableau avec les valeurs des differents indices
        ## pour les unites d'observation ayant les indices calcules
        unit$Delta <- ind_div$Delta[match(unit$unitobs, rownames(ind_div))]
        unit$DeltaEtoile <- ind_div$DeltaEtoile[match(unit$unitobs, rownames(ind_div))]
        unit$LambdaPlus <- ind_div$LambdaPlus[match(unit$unitobs, rownames(ind_div))]
        unit$DeltaPlus <- ind_div$DeltaPlus[match(unit$unitobs, rownames(ind_div))]
        unit$SDeltaPlus <- ind_div$SDeltaPlus[match(unit$unitobs, rownames(ind_div))]
        div_expect <- c(div[[8]], div[[9]], div[[10]])

        ## affichage des valeurs attendues
        print(paste("La valeur theorique de Delta est :" , div_expect[1]))
        print(paste("La valeur theorique de Delta* est :" , div_expect[2]))
        print(paste("La valeur theorique de Delta+ est :" , div_expect[3]))
    }

    ## on renomme densite en CPUE pour les jeux de donn�es p�che
    if (is.peche.f())                   # length(typePeche)>1
    {
        unit$CPUE <- unit$densite
        unit$densite <- NULL
        unit$CPUEbiomasse <- unit$biomasse
        unit$biomasse <- NULL
    }

    ## message de creation de la table unit
    print("La table metriques par unite d'observation a ete creee : UnitobsMetriques.csv")
    write.csv(unit, file=paste(NomDossierTravail, "UnitobsMetriques.csv", sep=""), row.names = FALSE)
    ## carte de la CPUE pour les donn�es de p�che NC
    if (FALSE) # (is.peche.f() & (siteEtudie == "NC")) # (length(typePeche)>1)
    {
        x11(width=50, height=30, pointsize=10)
        MapNC <- read.shape("./shapefiles/NewCaledonia_v7.shp", dbf.data = TRUE, verbose=TRUE, repair=FALSE)
        plot(MapNC, xlim=c(166, 167), ylim=c(-23, -22), fg="lightyellow", xaxs="i", yaxs="i", axes=TRUE)
        unit$latitude <- as.vector(unit$latitude , "numeric")
        unit$longitude <- as.vector(unit$longitude , "numeric")
        unitSymbols <- subset(unit, longitude>0)
        symbols(unitSymbols$longitude, unitSymbols$latitude, unitSymbols$densite, add=TRUE, # Nommer les arguments [yr: 30/07/2010]
                fg=colours()[seq(10, (nrow(unitSymbols)*10), by=10)], lwd=3) #
        ## title(main=paste("CPUE", typePeche))
    }
    if (is.benthos.f())                 # unique(unitobs$type) == "LIT"
    {
        unit$richesse_specifique <- as.integer(tapply(unitesp$pres_abs, unitesp$unite_observation,
                                                      sum, na.rm=TRUE)) # chang� pour avoir des entiers.
        unit$richesse_specifique[is.na(unit$richesse_specifique)] <- as.integer(0) # pour conserver des entiers.
    }
    assign("unit", unit, envir=.GlobalEnv)

} # fin unit.f()


################################################################################
## Nom    : creationTablesBase.f()
## Objet  : ex�cution des fonctions unit.f, unitesp.f et unitespta.f
################################################################################

creationTablesBase.f <- function(){
    print("fonction creationTablesBase.f activ�e")

    ## ATTENTION A L'ORDRE D'APPEL DES FONCTIONS!!
    if (!is.benthos.f())                 # unique(unitobs$type) != "LIT"
    {  #car pas de classes de tailles avec les recouvrements

        ## Calculs des poids non observ�s :
        obs <- calcPoids.f(obs)
        assign("obs", obs, envir=.GlobalEnv)

        unitespta.f()
        if (Jeuxdonnescoupe==0)
        {
            ## SAUVunitespta <- unitespta  # stockage inutile [yr: 10/08/2010]
            assign("SAUVunitespta", unitespta, envir=.GlobalEnv)
        }
    }
    unitesp.f()
    unit.f()

    ## Sauvegarde des calculs pour restauration sans rechargement
    if (Jeuxdonnescoupe==0)
    {
        assign("SAUVobs", obs, envir=.GlobalEnv)
        assign("SAUVunitobs", unitobs, envir=.GlobalEnv)
        assign("SAUVcontingence", contingence, envir=.GlobalEnv)
        assign("SAUVunitesp", unitesp, envir=.GlobalEnv)
        assign("SAUVunit", unit, envir=.GlobalEnv)
        assign("SAUVlistespunit", listespunit, envir=.GlobalEnv)
    }

    ## Infos :
    if (Jeuxdonnescoupe==1)
    {
        tkmessageBox(message="Les m�triques par unit�s d'observations ont �t� recalcul�es sur le jeu de donn�es s�lectionn�s")
        gestionMSGinfo.f("CalculSelectionFait")
    }
    if (Jeuxdonnescoupe==0)
    {
        tkmessageBox(message="Les m�triques par unit�s d'observations ont �t� calcul�es sur l'ensemble du jeu de donn�es import�")
        gestionMSGinfo.f("CalculTotalFait")
    }
}

################################################################################
## Nom    : creationTablesCalcul.f()
## Objet  : G�n�ration d'une table globale bas�e sur obs a partir de listespunit
## rajoute des champs s�lectionn�s dans unitosb et especes
##          dans le cas de la vid�o
##          dans le cas du benthos
##          dans les cas autre que benthos
################################################################################

creationTablesCalcul.f <- function(){

    print("fonction creationTablesCalcul.f activ�e")
    if (!is.benthos.f())                # unique(unitobs$type) != "LIT"
    {  #car pas de classes de tailles avec les recouvrements

    }
    ## si SVR calcul des metriques par rotation
    if (unique(unitobs$type) == "SVR")
    {

    }

    TableMetrique <- listespunit

    ## Simplification OK [yreecht: 08/10/2010] :
    TableMetrique <- cbind(TableMetrique,
                           ## Colonnes d'unitobs :
                           unitobs[match(TableMetrique$unite_observation, unitobs$unite_observation),
                                   c("station", "caracteristique_1", "caracteristique_2", "fraction_echantillonnee",
                                     "jour", "mois", "heure", "nebulosite", "direction_vent", "force_vent", "etat_mer",
                                     "courant", "maree", "phase_lunaire", "latitude", "longitude", "avant_apres",
                                     "biotope_2", "habitat1", "habitat2", "habitat3", "visibilite", "prof_min",
                                     "prof_max", "DimObs1", "DimObs2", "nb_plong", "plongeur")],
                           ## Colonnes du r�f�rentiel esp�ces :
                           especes[match(TableMetrique$code_espece, especes$code_espece),
                                   c("Genre", "Famille", "mobilite", "nocturne", "cryptique", "taillemax", "regim.alim",
                                     ## Inter�ts types de p�ches :
                                     grep(paste("^interet\\.[[:alpha:]]+", siteEtudie, "$", sep=""), # Colonnes
                                          colnames(especes), value=TRUE))])                          # site-sp�cifiques.


    if (is.benthos.f())                 # unique(unitobs$type) == "LIT"
    {
        TableMetrique$Cath_benthique <- especes$Cath_benthique[match(TableMetrique$code_espece, especes$code_espece)]
    }
    if (unique(unitobs$type) == "SVR")  # suppr: "UVC" �_� [???]
    {
        ## Si Video     "poids.moyen.petits" "poids.moyen.moyens" "poids.moyen.gros"   "taille_max_petits"  "taille_max_moyens"     "L50"
    }

    ## On peut rendre plus lisible ce qui suit... [yreecht: 22/07/2010] OK [yreecht: 08/10/2010]
    TableBiodiv <- unit
    names(TableBiodiv)[1] <- "unite_observation"

    TableBiodiv <- cbind(TableBiodiv,
                         ## Colonnes d'unitobs :
                         unitobs[match(TableBiodiv$unite_observation, unitobs$unite_observation),
                                 c("station", "caracteristique_1", "caracteristique_2", "fraction_echantillonnee",
                                   "jour", "mois", "heure", "nebulosite", "direction_vent", "force_vent", "etat_mer",
                                   "courant", "maree", "phase_lunaire", "avant_apres", "biotope_2", "habitat1",
                                   "habitat2", "habitat3", "visibilite", "prof_min", "prof_max", "DimObs1", "DimObs2",
                                   "nb_plong", "plongeur")])


    assign("TableBiodiv", TableBiodiv, envir=.GlobalEnv)
    assign("TableMetrique", TableMetrique, envir=.GlobalEnv)
    print("tableau TableMetrique r�alis�")

    if (Jeuxdonnescoupe==0)
    {
        assign("SAUVTableBiodiv", TableBiodiv, envir=.GlobalEnv)
        assign("SAUVTableMetrique", TableMetrique, envir=.GlobalEnv)
    }else{}
}