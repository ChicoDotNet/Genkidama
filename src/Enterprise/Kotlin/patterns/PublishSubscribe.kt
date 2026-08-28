object PublishSubscribeExample{fun run():Boolean{val s=listOf<(Int)->String>({"warehouse:$it"},{"analytics:$it"});return s.map{it(51)}.joinToString(">") == "warehouse:51>analytics:51"}}
