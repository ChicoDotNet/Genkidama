object PublishSubscribeExample { def run:Boolean=List[Int=>String](i=>s"warehouse:$i",i=>s"analytics:$i").map(_(51)).mkString(">")=="warehouse:51>analytics:51" }
