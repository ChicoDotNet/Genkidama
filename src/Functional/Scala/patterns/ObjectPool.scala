import scala.collection.mutable
object ObjectPoolExample { def run:Boolean={val pool=mutable.Stack(1,2);val x=pool.pop();pool.push(x);pool.size==2&&pool.contains(x)} }
