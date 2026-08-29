def env=[x:4];def expr=['add',['var','x'],['lit',3]];def evalExpr;evalExpr={n->n[0]=='lit'?n[1]:n[0]=='var'?env[n[1]]:evalExpr(n[1])+evalExpr(n[2])};assert evalExpr(expr)==7
