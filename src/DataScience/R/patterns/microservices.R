# Microservices: narrow service contracts compose independently.
inventory<-function(sku)list(sku=sku,available=TRUE); order<-function(sku)inventory(sku)$available; stopifnot(order('A-1'))
