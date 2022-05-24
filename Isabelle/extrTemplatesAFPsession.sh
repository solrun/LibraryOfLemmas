#!/bin/bash

#mkdir -p GeneratedThys/AFP/$1
#mkdir -p Output/AFP/$1

cd ../afp-2022-04-22/thys/$1

for f in $(find . -name '*.thy'); do
    thy_name=$(basename $f .thy)
    echo "Processing theory file: $thy_name"
    dir_name=$(dirname $f)
    cd ../../../Isabelle
    if [ "$dir_name" == "." ]
    then
        # ./extrTemplatesAFPthy.sh "$thy_name" "$session_name"
        echo "no subdirectory"
    else
        # ./extrTemplatesAFPthy.sh "$thy_name" "$session_name${$dir_name:1}"
        echo "$1${dir_name:1}"
    fi
    cd ../afp-2022-04-22/thys/$1;
done
