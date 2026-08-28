# Interpreter: evaluate a tiny AST against a context.
env<-list(x=4); eval_expr<-function(n) switch(n[[1]],lit=n[[2]],var=env[[n[[2]]]],add=eval_expr(n[[2]])+eval_expr(n[[3]])); stopifnot(eval_expr(list('add',list('var','x'),list('lit',3)))==7)
