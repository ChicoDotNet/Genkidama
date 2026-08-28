function dependency_injection(); greet=@(clock)['hello@' clock()]; assert(strcmp(greet(@()'noon'),'hello@noon')); end
