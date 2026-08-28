(flet((legacy(c)c)(adapter(a)(round(* a 100))))(assert(=1234(legacy(adapter 12.34)))))
