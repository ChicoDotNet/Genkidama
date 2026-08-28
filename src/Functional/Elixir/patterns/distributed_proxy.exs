remote=fn id->%{id:id,name:"Ada"} end; proxy=fn id->remote.(id).name end; unless proxy.(7)=="Ada",do: raise "Proxy"
