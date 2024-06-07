theory Templates imports Main "HOL-Library.Tree"
begin
ML\<open>
(* Term type for templates *)
(* Do something with lambdas? Or add more info to t_empty? *)
datatype template_term = template_var  of int 
                       | template_hole of int 
                       | template_app  of template_term * template_term
                       | t_empty
datatype sign = equals | greater_equals | less_equals | less_than | greater_than

datatype template = template_equation of template_term * template_term
                  | template_implication of template list * template
                  | template_bimplication of template * template
                  | template_predicate of template_term
                  | template_negation of template
                  | template_inequation of sign * template_term * template_term
                  | template_dunno
(* Find all variables and constants *)
fun vfs t = (Term.add_vars t [], Term.add_consts t [])

fun indexOf [] _ _ = raise Empty
 |  indexOf (a::xs) x n = if (a = x) then n else indexOf xs x (n+1);
fun has_eq (Const ("HOL.eq",_)) = true
 |  has_eq (f$t)                = has_eq f orelse has_eq t
 |  has_eq _                    = false
(* Template representation of a term *)
(* FIXME: Do something with remaining cases *)
fun term2template vars funs t = case t of
                        f $ u            => template_app ((term2template vars funs f)
                                                         ,(term2template vars funs u))  
                      | Const (f,t)  => template_hole (indexOf funs (f,t) 0)
                      | Var v  => template_var (indexOf vars v 0) 
                      | _                 => t_empty

(* Template representation of a lemma *)
fun lemma2predicate c l = template_predicate (term2template 
                    (Term.add_vars c []) (Term.add_consts c []) l);
(* FIXME: what if we have more than 3 equality signs? *)
fun makeTemplatewithContext c t = case t of
                    Const _ => lemma2predicate c t
                  | Var _   => lemma2predicate c t
                  | Free _  => lemma2predicate c t 
                  | (Const ("HOL.Trueprop",_)) $ t => makeTemplatewithContext c t
                  | (Const ("HOL.Not",_)) $ t0      => template_negation (makeTemplatewithContext c t0)
                  | (Const ("HOL.eq",_)) $ t1 $ t2 =>
                    if has_eq t2 then
                       if has_eq t1 then
                          template_bimplication 
                          (makeTemplatewithContext c t1
                          ,makeTemplatewithContext c t2)
                       else template_implication ([lemma2predicate c t1],makeTemplatewithContext c t2)
                    else
                    template_equation 
                    (term2template (Term.add_vars c []) (Term.add_consts c []) t1
                    ,term2template (Term.add_vars c []) (Term.add_consts c []) t2)
                  | (Const ("Orderings.ord_class.less_eq",_)) $ t1 $ t2 =>
                    template_inequation (less_equals, term2template (Term.add_vars c []) (Term.add_consts c []) t1
                    ,term2template (Term.add_vars c []) (Term.add_consts c []) t2)
                  | (Const ("Orderings.ord_class.less",_)) $ t1 $ t2 =>
                    template_inequation (less_than, term2template (Term.add_vars c []) (Term.add_consts c []) t1
                    ,term2template (Term.add_vars c []) (Term.add_consts c []) t2)
                  | (Const ("Orderings.ord_class.greater_eq",_)) $ t1 $ t2 =>
                    template_inequation (greater_equals, term2template (Term.add_vars c []) (Term.add_consts c []) t1
                    ,term2template (Term.add_vars c []) (Term.add_consts c []) t2)
                  | (Const ("Orderings.ord_class.greater",_)) $ t1 $ t2 =>
                    template_inequation (greater_than, term2template (Term.add_vars c []) (Term.add_consts c []) t1
                    ,term2template (Term.add_vars c []) (Term.add_consts c []) t2)
                  | (Const ("Pure.imp",_)) $ p1 $ p2 => 
                    template_implication ([makeTemplatewithContext c p1], 
                     makeTemplatewithContext c p2)
                  | _ => lemma2predicate c t
fun lemma2template l = makeTemplatewithContext l l
fun thm2template t = if (Thm.no_prems t) then lemma2template (Thm.concl_of t)
                     else template_implication (
                          map (makeTemplatewithContext (Thm.prop_of t)) (Thm.prems_of t), 
                          makeTemplatewithContext (Thm.prop_of t) (Thm.concl_of t));

fun tterm2string t = case t of 
                 template_var k => "X" ^ (Int.toString k)
               | template_hole k => "?F" ^ (Int.toString k)
               | template_app (t1,t2) => 
                 (tterm2string t1) ^ "(" ^ (tterm2string t2) ^ ")"
               | t_empty => "Ö"
fun template2string t = case t of
                        template_equation (t1,t2) => 
                        (tterm2string t1) ^ " = " ^ (tterm2string t2)
                      | template_implication (ts,t) => 
                        (String.concatWith "&" (map template2string ts)) ^ " => " ^ (template2string t)
                      | template_bimplication (t1,t2) =>
                        (template2string t1) ^ " <=> " ^ (template2string t2)
                      | template_predicate t => tterm2string t
                      | _                    => ""
val t = @{thm Tree.height_le_size_tree};
val th = Thm.concl_of t;

\<close>

end