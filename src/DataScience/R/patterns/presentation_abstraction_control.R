# PAC: control coordinates abstraction and presentation.
abstraction<-list(value=1); control<-function(d)abstraction$value<<-abstraction$value+d; presentation<-function()as.character(abstraction$value); control(2); stopifnot(presentation()=='3')
