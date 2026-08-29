# Client-Server: client communicates through server contract.
server<-function(request)list(echo=request); client<-function(v)server(v)$echo; stopifnot(client('ping')=='ping')
