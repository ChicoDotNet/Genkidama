choose=fn xs,f->f.(xs) end; unless choose.([3,1,2],&Enum.min/1)==1 and choose.([3,1,2],&Enum.max/1)==3,do: raise "Strategy"
