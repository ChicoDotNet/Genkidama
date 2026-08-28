type InterpreterExpr={kind:'lit',v:number}|{kind:'add'|'mul',l:InterpreterExpr,r:InterpreterExpr};
function evalInterpreterExpr(e:InterpreterExpr):number{return e.kind==='lit'?e.v:e.kind==='add'?evalInterpreterExpr(e.l)+evalInterpreterExpr(e.r):evalInterpreterExpr(e.l)*evalInterpreterExpr(e.r)}
function interpreterPattern(){return evalInterpreterExpr({kind:'add',l:{kind:'lit',v:7},r:{kind:'mul',l:{kind:'lit',v:3},r:{kind:'lit',v:4}}})===19}
