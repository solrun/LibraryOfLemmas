(*For each theory file x.thy being considered, 
generate a new (Isabelle) theory file 
that imports x as well as our evaluation method and calls evaluation method on the lemmas output by the model
Evaluation method (also defined in Isabelle) 
parses (reads) generated lemmas, 
does counterexample checking and proof attempts, 
outputs categorization for each lemma 
The generated theory code is run via Isabelle batch processing mode, we can catch the output and process it as we like 

*)
theory LemmaChecker
  imports Main
begin
(*
Evaluation method (also defined in Isabelle) 
parses (reads) generated lemmas, 
does counterexample checking and proof attempts, 
outputs categorization for each lemma 
*)
ML\<open>
(* TODO: more informative error handling *)
fun check_syntax lemma_str ctxt = 
  let val conj = Syntax.read_prop ctxt lemma_str
  in true
  handle _ => false
end

fun check_correct lemma_conj ctxt = 

fun check_lemma lemmastring ctxt =
  let
    val syntax_correct = check_syntax lemmastring ctxt
    val conj = case syntax_correct of false => NONE
                | true => SOME (Syntax.read_prop ctxt lemmastring)
    val check_correct  = case syntax_correct of false => false
                          | true => counterexample_check conj ctxt
    val easy_proof     = case check_correct of false => false
                          | true => try_easy_proof lemmastring ctxt
  in (syntax_correct, check_correct, easy_proof)
  end
\<close>
end
