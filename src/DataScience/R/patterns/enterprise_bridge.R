# Bridge: abstraction delegates to replaceable implementation.
sender<-function(text)paste0('sms:',text); notify<-function(text)sender(text); stopifnot(notify('ok')=='sms:ok')
