import scala.collection.mutable
object UnitOfWorkExample { def run:Boolean={val store=mutable.ArrayBuffer[Int]();val pending=mutable.ArrayBuffer(2,3);store++=pending;pending.clear();store.toList==List(2,3)&&pending.isEmpty} }
