# MVC: controller changes model; view projects it.
model<-list(count=0); controller<-function()model$count<<-model$count+1; view<-function()paste0('count=',model$count); controller(); stopifnot(view()=='count=1')
