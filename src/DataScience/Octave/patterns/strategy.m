function strategy(); choose=@(xs,s)s(xs); assert(choose([3 1 2],@min)==1 && choose([3 1 2],@max)==3); end
