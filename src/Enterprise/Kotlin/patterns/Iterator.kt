object IteratorExample{fun run():Boolean{val values=listOf(10,20,30);val it=values.iterator();val seen=mutableListOf<Int>();while(it.hasNext())seen+=it.next();return seen==values&&!it.hasNext()}}
