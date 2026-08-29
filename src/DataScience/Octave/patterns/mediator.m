function mediator(); route=@(s,m)[s ':' m]; assert(strcmp(route('checkout','paid'),'checkout:paid')); end
