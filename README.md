# LibraryOfLemmas
LOL: a Library of Lemmas

This is a collection of lemmas extracted from Isabelle's [Archive of Formal Proofs](https://www.isa-afp.org/index.html).

The data is in [data/AFP/](data/AFP/).

For each equational lemma we found in the AFP, we we output a triplet containing 
the lemma name (including the name of the file it is defined in), 
a string representation of the lemma statement, 
and a template representation of the lemma. 
The template representation shows the
lemma statement’s term structure with function and variable names abstracted
away but using integer labels, along with the constructor template_hole for functions and template_var for variables,
to keep track of function symbols and variables that occur more than once in the lemma statement. 

The template datatype is defined in [Templates.thy](Isabelle/Templates.thy).

A parser for the data format is defined in [Parse.hs](templateAnalysis/src/Template/Parse.hs).

