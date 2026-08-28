function visitor(); node.value=5; visit=@(n)n.value*2; assert(visit(node)==10); end
