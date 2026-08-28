# Active Object: requests queue before execution.
state<-c(); mailbox<-list(function()state<<-c(state,'done')); mailbox[[1]](); stopifnot(state[[1]]=='done')
