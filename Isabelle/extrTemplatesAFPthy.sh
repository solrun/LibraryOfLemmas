#!/bin/bash

# Generate Isabelle theory file that finds equational lemmas
echo 'theory '$1'Templates imports Main "~/RI/templateAnalysis/Isabelle/ExtrEqs" "~/RI/templateAnalysis/afp-2022-04-22/thys/'$2'/'$1'" begin ML\<open> val templateEqs = template_eqs "'$1'*" @{context} \<close> end' > GeneratedThys/AFP/$2/$1Templates.thy

# Run theory and write output to file
isabelle process -m ASCII -o quick_and_dirty=true -o skip_proofs=true -T GeneratedThys/AFP/$2/$1Templates > Output/AFP/$2/$1out.txt

# Extract templates from output
sed -n '/templateEqs/,/]:/p' Output/AFP/$2/$1out.txt | sed '$ s/.$//' | tail -n +2 > Output/AFP/$2/$1templateEqs.txt
