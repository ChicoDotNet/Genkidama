# Publish-Subscribe: publishers address topics, not consumers.
received<-c(); topics<-list(news=list(function(v)received<<-c(received,v))); lapply(topics$news,function(f)f('v1')); stopifnot(received[[1]]=='v1')
