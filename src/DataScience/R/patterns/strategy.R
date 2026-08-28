# Strategy: interchangeable algorithms share one context.
choose<-function(v,strategy)strategy(v); stopifnot(choose(c(3,1,2),min)==1,choose(c(3,1,2),max)==3)
