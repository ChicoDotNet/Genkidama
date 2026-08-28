(assert(equal(mapcar(lambda(w e)(format nil "~A:~A" w e))'("leader" "follower")'("one" "two"))'("leader:one" "follower:two")))
