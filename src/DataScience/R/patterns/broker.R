# Broker: route requests to registered handlers.
handlers<-list(price=function(sku)9); request<-function(topic,payload)handlers[[topic]](payload); stopifnot(request('price','A')==9)
