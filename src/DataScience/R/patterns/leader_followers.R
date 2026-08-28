# Leader/Followers: workers rotate event acceptance.
workers<-c('leader','follower'); events<-c('one','two'); handled<-paste(workers,events,sep=':'); stopifnot(identical(handled,c('leader:one','follower:two')))
