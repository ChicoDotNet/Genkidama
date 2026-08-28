object CommandExample{fun run():Boolean{val commands=listOf<(Int)->Int>({it+50},{it-20});val balance=commands.fold(100){x,c->c(x)};return balance==130&&commands[1](150)==130}}
