# Template Method: fixed algorithm surrounds a variable hook.
render<-function(body)paste0('<',body(),'>'); stopifnot(render(function()'sales')=='<sales>')
