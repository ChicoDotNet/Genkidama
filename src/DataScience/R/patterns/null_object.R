# Null Object: no-op collaborator preserves normal contract.
null_logger<-function(message)invisible(NULL); service<-function(logger){logger('run');'ok'}; stopifnot(service(null_logger)=='ok')
