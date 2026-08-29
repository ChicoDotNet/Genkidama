# Document-View: multiple views share one document.
document<-list(title='One'); a<-function()document$title; b<-function()toupper(document$title); stopifnot(a()=='One',b()=='ONE')
