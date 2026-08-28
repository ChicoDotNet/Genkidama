send=fn source,target,msg->{target,{source,msg}} end; unless send.(:a,:b,"hello")=={:b,{:a,"hello"}},do: raise "Peer"
