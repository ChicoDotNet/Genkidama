incoming=["a","b"]; completed=Enum.map(incoming,&String.upcase/1); unless completed==["A","B"],do: raise "HalfSync"
