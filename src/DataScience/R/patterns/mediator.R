# Mediator: colleagues communicate through a coordinator.
events<-c(); mediator<-function(sender,msg) events<<-c(events,paste(sender,msg,sep=':')); mediator('checkout','paid'); stopifnot(events[[1]]=='checkout:paid')
