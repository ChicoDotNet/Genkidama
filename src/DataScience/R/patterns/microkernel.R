# Microkernel: minimal core delegates to plugins.
plugins<-list(upper=function(x)toupper(x)); stopifnot(plugins$upper('plugin')=='PLUGIN')
