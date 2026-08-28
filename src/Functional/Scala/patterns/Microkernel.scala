object MicrokernelExample { def run:Boolean={val p=Map[String,Int=>Int]("double"->((x:Int)=>x*2),"square"->((x:Int)=>x*x));p("double")(4)==8&&p("square")(4)==16} }
