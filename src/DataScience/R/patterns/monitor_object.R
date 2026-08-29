# Monitor Object: mutations pass through one guarded operation.
counter<-0; synchronized_increment<-function()counter<<-counter+1; synchronized_increment(); stopifnot(counter==1)
