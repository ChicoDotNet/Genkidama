function observer(); notify=@(subscriber,event)subscriber(event); subscriber=@(e)['seen:' e]; assert(strcmp(notify(subscriber,'changed'),'seen:changed')); end
