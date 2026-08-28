let ()=let model=ref 0 in let controller()=incr model in let view()="count="^string_of_int!model in controller();assert(view()="count=1")
