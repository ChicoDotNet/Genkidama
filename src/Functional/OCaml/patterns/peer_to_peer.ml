let send source message=source^":"^message;;let ()=assert(send"a""hello"="a:hello")
