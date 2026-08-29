# Active Record: record-shaped value owns persistence operation.
table<-list(); save<-function(record)table[[as.character(record$id)]]<<-list(name=record$name); save(list(id=1,name='Ada')); stopifnot(table[['1']]$name=='Ada')
