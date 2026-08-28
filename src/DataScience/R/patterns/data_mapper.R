# Data Mapper: translate storage row to domain value.
row<-list(name='Ada'); mapper<-function(r)structure(list(name=r$name),class='User'); user<-mapper(row); stopifnot(user$name=='Ada')
