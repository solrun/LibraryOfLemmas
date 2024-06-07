theory ExtrEqs 
  imports Main "HOL-Library.Tree" "HOL-Library.Stream" (* Templates *)
begin
declare [[ML_print_depth=10000]]

ML\<open>

val eqpattern = Find_Theorems.Pattern(Proof_Context.read_term_pattern 
                @{context} "Trueprop(_ = _)");
val badeqpattern = Find_Theorems.Pattern(Proof_Context.read_term_pattern 
                   @{context} "Pure.eq");
val undefpattern = Find_Theorems.Pattern(Proof_Context.read_term_pattern 
                   @{context} "Trueprop(_ = undefined)");
val thmpattern = Find_Theorems.Pattern(Proof_Context.read_term_pattern
                 @{context} "Trueprop(_)");
(*Try to avoid some utility theorems, associated with various tools - have another look at whether more things should be added to the list *)
(*Filter out theorems with more than one "." to avoid imports
and stick with those defined in the current theory file*)
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

fun get_all_thms namefilter ctxt =
  let val (_,thms) = Find_Theorems.find_theorems ctxt NONE (SOME 16000) true
                    ([(true,thmpattern),(false,badeqpattern),(false,undefpattern)]@[(true,Find_Theorems.Name (namefilter))]@unwanted_names)
    in map_filter (fn (factref,thm) => 
                        if  Thm.prop_of thm |> has_vars (* Remove plain definitions *)
                        then SOME (Facts.ref_name factref, thm)
                        else NONE) thms
    end;

fun get_thms_withname namefilter ctxt =
  let val (_,thms) = Find_Theorems.find_theorems ctxt NONE (SOME 16000) true
                    [(true,Find_Theorems.Name (namefilter))]
    in thms
    end;
fun get_noneq_thms namefilter ctxt =
  let val (_,thms) = Find_Theorems.find_theorems ctxt NONE (SOME 16000) true
                    ([(true,thmpattern),(false,eqpattern),(false,badeqpattern),(false,undefpattern)]@[(true,Find_Theorems.Name (namefilter))]@unwanted_names)
    in map_filter (fn (factref,thm) => 
                        if  Thm.prop_of thm |> has_vars (* Remove plain definitions *)
                        then SOME (Facts.ref_name factref, thm)
                        else NONE) thms
    end;
fun get_tydefs t = Term.add_vars (Thm.full_prop_of t) [];
(* Remove things that are non-equalities and meta-level equalities *)
(*
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
*)
(* do we want to compile more data about these theorems ?*)
(*
fun template_eqs namefilter ctxt =
    List.map (fn (s,thm) => (s,thm,thm2template thm))
       (#1 ((List.partition (fn (_,thm) => Thm.no_prems thm))
       (rev (get_all_eqs namefilter ctxt))
     ));

fun template_thms namefilter ctxt = 
    List.map (fn (s,thm) => (s,thm,thm2template thm))
       (rev (get_all_thms namefilter ctxt));
*)
(* Trying some methods out *)

val t = @{thm Tree.mirror_mirror};

(*full_prop_of lists names and types of all components
prop_of is kind of the same? *)
val fp = Thm.full_prop_of t;

(*
Term.add_vars
gives vars in a structure (indexname * typ) list

Term.add_consts lists out constants in a (string * typ) list
the string part is the full name like "Orderings.ord_class.less_eq"
*)
Term.add_vars fp [];
Term.add_consts fp [];
get_tydefs t;
(* All thms that contain the given string in their name *)
get_thms_withname "mirror" @{context};
(*val ths = template_thms "Tree" @{context}*)
\<close>

thm Tree.acomplete_def
thm Tree.mirror.simps
end