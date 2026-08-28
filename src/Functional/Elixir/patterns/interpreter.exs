eval=fn {:add,{:var,:x},{:lit,n}},env->env.x+n end; unless eval.({:add,{:var,:x},{:lit,3}},%{x:4})==7,do: raise "Interpreter"
