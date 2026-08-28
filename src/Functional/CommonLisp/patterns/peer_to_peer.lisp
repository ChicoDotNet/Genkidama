(flet((send(s m)(format nil "~A:~A" s m)))(assert(string=(send "a" "hello")"a:hello")))
