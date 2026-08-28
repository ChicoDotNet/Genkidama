object IteratorExample { def run:Boolean={val it=List(10,20,30).iterator;val seen=it.toList;seen==List(10,20,30) && !it.hasNext} }
