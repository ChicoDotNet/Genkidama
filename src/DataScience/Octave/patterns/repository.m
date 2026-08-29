function repository(); data=containers.Map(1,{'Ada'}); get=@(id)data(id); assert(strcmp(get(1),'Ada')); end
