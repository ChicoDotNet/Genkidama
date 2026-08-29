# Message Bus: bus decouples producer from consumers.
seen<-c(); bus<-list(paid=list(function(v)seen<<-c(seen,v))); lapply(bus$paid,function(f)f(42)); stopifnot(seen[[1]]==42)
