## GUI language/Langues d'interface:
options(P.GUIlang="en")                 # en/fr implemented so far


## #########################################################################################################
## Path to automatically loaded files/ Chemin des fichiers utilisés par le chargement automatique.
##
## Example below: replicate with your own file names/paths (without the comment signs, ##) /
## Exemple : faites de même avec vos jeux de donnée (sans les commentaires - "## " - en début de ligne.) :

## #### <Study case> / <Cas d'étude> :
## fileNameUnitobs <- "<unitobs_file.txt>"
## fileNameObs <- "<observations_file.txt>"
## fileNameRefesp <- "<global_species_reference_table.txt>"
## fileNameRefespLocal <- "<local_species_reference_table.txt>"
## fileNameRefspa <- "<spatial_reference_(table.txt|shapefile.shp)>"
## nameWorkspace <- "~/PAMPA/Dummy_WD"



## #########################################################################################################
## EN: You can also tune the default graphics options:
##
## FR: Vous pouvez également définir des options par défaut personnalisées (donne également accès aux
##     options "cachées") :

## options(P.colPalette="gray", P.pointMoyenneCol = "black", P.sepGroupesCol = "#6f6f6f",
##         P.valMoyenneCol = "black", P.NbObsCol = "black",
##         P.valMoyenne = FALSE, P.pointMoyenne = TRUE,
##         P.legendeCouleurs = FALSE, P.NbObs = FALSE,
##         P.graphPaper=TRUE)

## EN: If you change the language for the outputs, a further step is required:
## FR: Dans le cas ou vous changeriez la langue des sorties une étape supplémentaire est nécesaire :

## options(P.lang="en")
## init.GraphLang.f()          # Load the appropriate variable names file /
                               # Pour charger le bon fichier de noms de variables.


     #############################################
     ###    Paste your configuration below /   ###
     ### Collez votre configuration ci-dessous ###
     #############################################

