# Peer-to-Peer: peers send directly without central server role.
peers<-list(a=c(),b=c()); send<-function(source,target,msg)peers[[target]]<<-c(peers[[target]],paste(source,msg,sep=':')); send('a','b','hello'); stopifnot(peers$b[[1]]=='a:hello')
