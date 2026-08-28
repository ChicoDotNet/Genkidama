module InterpreterExample
type Expr=Lit of int|Add of Expr*Expr|Mul of Expr*Expr
let rec eval=function Lit v->v|Add(l,r)->eval l+eval r|Mul(l,r)->eval l*eval r
let run ()=eval(Add(Lit 7,Mul(Lit 3,Lit 4)))=19
