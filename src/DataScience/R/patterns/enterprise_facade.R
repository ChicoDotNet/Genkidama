# Facade: one operation coordinates subsystem calls.
stock<-function()TRUE; charge<-function()'paid'; checkout<-function()if(stock())charge() else 'sold_out'; stopifnot(checkout()=='paid')
