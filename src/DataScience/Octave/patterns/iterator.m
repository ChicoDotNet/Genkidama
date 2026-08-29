function iterator(); items=[3 2 1]; seen=[]; for x=items; seen(end+1)=x; end; assert(isequal(seen,items)); end
