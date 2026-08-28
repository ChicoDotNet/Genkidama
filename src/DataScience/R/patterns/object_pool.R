# Object Pool: acquire/release reuses an expensive object.
item<-new.env();item$id<-1;pool<-list(item);borrowed<-pool[[1]];pool<-list(borrowed);stopifnot(identical(item,pool[[1]]))
