#!/bin/bash

pwd | egrep -q "Scripts_Biodiv$" && cd ..

echo `pwd`

(R --no-restore --no-save < ./Scripts_Biodiv/Main_Unix.R)

## (Rscript ./Scripts_Biodiv/Main_Unix.R --no-restore --no-save --no-site-file  --interactive) # R_PROFILE=./Scripts_Biodiv/Rprofile.site)

wait

## no-restore --no-save
