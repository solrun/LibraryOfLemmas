theory ExtrEqs imports Main Templates
begin
declare [[ML_print_depth=10000]]

ML\<open>

val eqpattern = Find_Theorems.Pattern(Proof_Context.read_term_pattern 
                @{context} "Trueprop(_ = _)");
val badeqpattern = Find_Theorems.Pattern(Proof_Context.read_term_pattern 
                   @{context} "Pure.eq");
val undefpattern = Find_Theorems.Pattern(Proof_Context.read_term_pattern 
                   @{context} "Trueprop(_ = undefined)");
(*Try to avoid some utility theorems, associated with various tools *)
val badnames =  ["*nitpick*","*Nitpick*", "*full_exhaustive*", "*_def*", "Enum.finite*",
                 "*Quickcheck*", "ATP*","*Nunchaku*","*Code*", "*BNF*","*SMT*","*.*.*"]
val unwanted_names : (bool * Term.term Find_Theorems.criterion) list = 
  map (Library.pair false) (map (fn s  => Find_Theorems.Name s) badnames);
fun has_vars t =
  case t of
    Var _           => true
  | Abs (_,_,body)  => has_vars body
  | f$t2            => has_vars f orelse has_vars t2
  | _               => false



fun get_all_eqs namefilter ctxt =
  let  
    val (_, eqs) = Find_Theorems.find_theorems ctxt NONE (SOME 16000) true 
                   ([(true,eqpattern),(false,badeqpattern),(false,undefpattern)]@[(true,Find_Theorems.Name (namefilter))]@unwanted_names)
    in map_filter (fn (factref,thm) => 
                        if  Thm.prop_of thm |> has_vars (* Remove plain definitions *)
                        then SOME (Facts.ref_name factref, thm)
                        else NONE) eqs
    end;


(* Remove things that are non-equalities and meta-level equalities *)
fun drop_unwanted thm = 
  let 
    fun is_not (Const ("HOL.Not", _)) = true
  | is_not _ = false;

    fun try_dest_Trueprop ((Const ("HOL.Trueprop", _))$t) = t
      | try_dest_Trueprop t = t

    fun not_metaeq ((Const ("Pure.eq", _))) = false
      | not_metaeq _ = true

    val t = (try_dest_Trueprop o Thm.concl_of) thm
      
  in
    ((not o is_not o Term.head_of) t) andalso not_metaeq (Term.head_of t)
  end;

(* do we want to compile more data about these theorems ?*)
fun template_eqs namefilter ctxt = 
    List.map (fn (s,thm) => (s,thm,thm2template thm))
       (#1 ((List.partition (fn (_,thm) => Thm.no_prems thm))
       (rev (get_all_eqs namefilter ctxt))
     ));

\<close>

end