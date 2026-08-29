def server={r->[echo:r]};def client={v->server(v).echo};assert client('ping')=='ping'
