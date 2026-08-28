# Repository: collection-like boundary hides storage.
data<-list('1'=list(name='Ada')); get_user<-function(id)data[[as.character(id)]]; stopifnot(get_user(1)$name=='Ada')
