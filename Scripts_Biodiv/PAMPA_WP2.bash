#!/bin/bash

pwd | egrep -q "Scripts_Biodiv$" && cd ..

echo `pwd`

export R_PROFILE="Scripts_Biodiv/Rprofile.site"

## (R --no-restore --no-save)

## R --file=./Scripts_Biodiv/Main_Unix.R --interactive --no-restore --no-save

## R R_PROFILE=./Scripts_Biodiv/Rprofile.site --interactive --no-restore --no-save

exec R --interactive --no-restore --no-save
## source("./Scripts_Biodiv/Main.R", encoding="latin1")

## (R --no-restore --no-save < ./Scripts_Biodiv/Main_Unix.R)

## (Rscript ./Scripts_Biodiv/Main_Unix.R --no-restore --no-save --no-site-file  --interactive) # R_PROFILE=./Scripts_Biodiv/Rprofile.site)

## wait

## no-restore --no-save
