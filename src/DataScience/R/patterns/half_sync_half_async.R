# Half-Sync/Half-Async: queue intake before synchronous processing.
incoming<-c('a','b'); completed<-toupper(incoming); stopifnot(identical(completed,c('A','B')))
