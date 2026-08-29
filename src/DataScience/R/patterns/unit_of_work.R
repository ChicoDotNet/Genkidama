# Unit of Work: collect pending changes then commit and clear.
pending<-list(list(id=1)); database<-list(); database<-c(database,pending); pending<-list(); stopifnot(database[[1]]$id==1,length(pending)==0)
