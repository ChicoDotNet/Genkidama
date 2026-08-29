# Command: queue executable requests.
balance<-100; commands<-list(function(){balance<<-balance+50},function(){balance<<-balance-20}); lapply(commands,function(f)f()); stopifnot(balance==130)
