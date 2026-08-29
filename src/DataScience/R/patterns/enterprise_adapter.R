# Adapter: translate amount contract to legacy cents.
legacy<-function(cents)cents; adapter<-function(amount)legacy(as.integer(round(amount*100))); stopifnot(adapter(12.34)==1234)
