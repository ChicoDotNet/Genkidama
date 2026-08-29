function distributed_proxy(); remote=@(id)struct('id',id,'name','Ada'); proxy=@(id)remote(id).name; assert(strcmp(proxy(7),'Ada')); end
