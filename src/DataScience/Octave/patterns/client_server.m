function client_server(); server=@(req)struct('echo',req); client=@(v)server(v).echo; assert(strcmp(client('ping'),'ping')); end
