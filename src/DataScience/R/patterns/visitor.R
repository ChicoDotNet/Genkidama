# Visitor: operation is supplied separately from element data.
node<-list(value=5); visitor<-function(n)n$value*2; stopifnot(visitor(node)==10)
