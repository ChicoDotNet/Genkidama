type expr=Lit of int|Var of string|Add of expr*expr;; let rec eval env=function Lit n->n|Var x->List.assoc x env|Add(a,b)->eval env a+eval env b;; let ()=assert(eval[("x",4)](Add(Var"x",Lit 3))=7)
