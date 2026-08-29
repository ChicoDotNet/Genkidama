# Service Locator: resolve collaborators from a registry.
services<-list(clock=function()'12:00'); stopifnot(services$clock()=='12:00')
