# Dependency Injection: collaborator is supplied from outside.
greet<-function(clock)paste0('hello@',clock()); stopifnot(greet(function()'noon')=='hello@noon')
