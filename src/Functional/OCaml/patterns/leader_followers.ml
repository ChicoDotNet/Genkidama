let ()=let workers=["leader";"follower"]and events=["one";"two"]in assert(List.map2(fun w e->w^":"^e)workers events=["leader:one";"follower:two"])
