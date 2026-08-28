commit=fn pending,db->{db++pending,[]} end; unless commit.([%{id:1}],[])=={[%{id:1}],[]},do: raise "UoW"
