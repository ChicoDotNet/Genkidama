# Iterator: traversal stays separate from collection storage.
items<-c(3,2,1); seen<-c(); for(x in items) seen<-c(seen,x); stopifnot(identical(seen,items))
