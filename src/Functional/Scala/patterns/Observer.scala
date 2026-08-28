object ObserverExample { def run:Boolean=List[Int=>String](i=>s"audit:$i",i=>s"dashboard:$i").map(_(42)).mkString(">")=="audit:42>dashboard:42" }
