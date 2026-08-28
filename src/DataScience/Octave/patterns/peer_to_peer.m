function peer_to_peer(); send=@(src,msg)[src ':' msg]; assert(strcmp(send('a','hello'),'a:hello')); end
