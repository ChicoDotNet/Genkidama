function enterprise_adapter(); legacy=@(c)c; adapter=@(a)legacy(round(a*100)); assert(adapter(12.34)==1234); end
