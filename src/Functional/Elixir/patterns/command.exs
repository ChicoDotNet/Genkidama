commands=[fn x->x+50 end,fn x->x-20 end]; unless Enum.reduce(commands,100,fn f,x->f.(x) end)==130,do: raise "Command"
