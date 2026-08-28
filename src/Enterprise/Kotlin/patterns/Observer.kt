object ObserverExample{fun run():Boolean{val obs=listOf<(Int)->String>({"audit:$it"},{"dashboard:$it"});return obs.map{it(42)}.joinToString(">") == "audit:42>dashboard:42"}}
