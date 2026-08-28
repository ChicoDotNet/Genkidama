# State: transition behavior depends on current state.
state<-'closed'; toggle<-function(){state<<-if(state=='closed')'open' else 'closed'}; toggle(); stopifnot(state=='open')
