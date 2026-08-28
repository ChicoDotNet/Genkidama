# MVVM: view-model projects model into display state.
model<-list(first='Ada',last='Lovelace'); view_model<-function()paste(model$first,model$last); stopifnot(view_model()=='Ada Lovelace')
