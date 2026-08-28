object UnitOfWorkExample{fun run():Boolean{val store=mutableListOf<Int>();val pending=mutableListOf(2,3);store+=pending;pending.clear();return store==listOf(2,3)&&pending.isEmpty()}}
