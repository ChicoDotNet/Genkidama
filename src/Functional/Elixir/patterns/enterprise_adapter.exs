legacy=fn cents->cents end; adapter=fn amount->legacy.(round(amount*100)) end; unless adapter.(12.34)==1234,do: raise "Adapter"
