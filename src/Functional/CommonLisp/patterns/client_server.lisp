(flet((server(r)r)(client(v)v))(assert(string=(server(client "ping"))"ping")))
