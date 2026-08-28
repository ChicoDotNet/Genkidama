object MicrokernelExample{fun run():Boolean{val plugins=mapOf<String,(Int)->Int>("double" to {it*2},"square" to {it*it});return plugins.getValue("double")(4)==8&&plugins.getValue("square")(4)==16}}
