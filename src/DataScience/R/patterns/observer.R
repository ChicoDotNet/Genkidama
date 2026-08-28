# Observer: notify registered subscribers.
seen<-c(); subscribers<-list(function(e)seen<<-c(seen,e)); lapply(subscribers,function(f)f('changed')); stopifnot(seen[[1]]=='changed')
