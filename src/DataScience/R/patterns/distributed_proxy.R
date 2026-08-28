# Distributed Proxy: local proxy hides remote lookup boundary.
remote<-function(id)list(id=id,name='Ada'); proxy<-function(id)remote(id)$name; stopifnot(proxy(7)=='Ada')
