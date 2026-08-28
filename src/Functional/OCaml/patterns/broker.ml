let handlers=[("price",fun _->9)];;let request topic payload=(List.assoc topic handlers)payload;;let ()=assert(request"price""A"=9)
