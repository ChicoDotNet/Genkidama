object CommandExample { def run:Boolean={val cs=List[Int=>Int](_+50,_-20);val b=cs.foldLeft(100)((x,f)=>f(x));b==130&&cs(1)(150)==130} }
