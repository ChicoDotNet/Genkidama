# Lazy Initialization: create once on first demand.
calls<-0; value<-NULL; get_value<-function(){if(is.null(value)){calls<<-calls+1;value<<-new.env()};value}; a<-get_value();b<-get_value();stopifnot(identical(a,b),calls==1)
