# MVP: presenter pushes model state into passive view.
model<-list(name='Ada'); view<-list(); presenter<-function()view$text<<-toupper(model$name); presenter(); stopifnot(view$text=='ADA')
