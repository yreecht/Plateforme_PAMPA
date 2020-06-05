#!/bin/bash

## Set the right path:
MY_PATH="`dirname \"$0\"`"              # relative
MY_PATH="`( cd \"$MY_PATH\" && pwd )`"  # absolutized and normalized
if [ -z "$MY_PATH" ]
then
  # error; for some reason, the path is not accessible
  # to the script (e.g. permissions re-evaled after suid)
  # exit 1  # fail
  echo "Path issue"
else
  cd "$MY_PATH"
fi

## echo "$MY_PATH"

## Go to parent directory of the script.
pwd | egrep -q "Scripts_Biodiv$" && cd ..

## echo `pwd`

## R_PROFILE which loads automatically the platform:
export R_PROFILE="Scripts_Biodiv/Rprofile.site"

## Launch R:
exec R --interactive --no-restore --no-save

echo ""
