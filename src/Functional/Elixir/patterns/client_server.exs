server=fn r->%{echo:r} end; client=fn v->server.(v).echo end; unless client.("ping")=="ping",do: raise "ClientServer"
