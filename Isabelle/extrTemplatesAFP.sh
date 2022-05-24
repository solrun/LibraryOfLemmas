#!/bin/bash

start_time=$SECONDS
cd ../afp-2022-04-22/thys

for session_name in *; do
    if [ -d "$session_name" ]
    then
        session_start=$SECONDS
        echo "Extracting lemmas from session: $session_name"
        if [ -d "../../Isabelle/Output/AFP/$session_name" ]
        then
            echo "Session has already been processed."
        else
            mkdir -p ../../Isabelle/GeneratedThys/AFP/$session_name
            mkdir -p ../../Isabelle/Output/AFP/$session_name
            cd $session_name
            for f in $(find . -name '*.thy'); do
                thy_start=$SECONDS
                thy_name=$(basename $f .thy)
                echo "Processing theory file: $thy_name"
                dir_name=$(dirname $f)
                cd ../../../Isabelle
                if [ "$dir_name" == "." ]
                then
                    ./extrTemplatesAFPthy.sh "$thy_name" "$session_name"
                else
                    mkdir -p GeneratedThys/AFP/$session_name${dir_name:1}
                    mkdir -p Output/AFP/$session_name${dir_name:1}
                    ./extrTemplatesAFPthy.sh "$thy_name" "$session_name${dir_name:1}"
                fi
                elapsed=$(( SECONDS - thy_start ))
                echo "Time spent on theory: $elapsed s"
                cd ../afp-2022-04-22/thys/$session_name;
            done
            cd ..
        fi
        session_elapsed=$(( SECONDS - session_start ))
        echo "Time spent on session: $session_elapsed s"
        echo ""
    fi
done
