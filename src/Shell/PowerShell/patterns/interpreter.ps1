Set-StrictMode -Version Latest
# Interpreter: evaluate a tiny expression tree against a context.
$env=@{x=4}; $expr=@('add',@('var','x'),@('lit',3)); function Eval($n){if($n[0]-eq'lit'){return $n[1]};if($n[0]-eq'var'){return $env[$n[1]]};(Eval $n[1])+(Eval $n[2])}; if((Eval $expr)-ne7){throw 'Interpreter failed'}
