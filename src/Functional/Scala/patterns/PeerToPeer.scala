import scala.collection.mutable
object PeerToPeerExample { def run:Boolean={val b=mutable.ArrayBuffer[String]();val c=mutable.ArrayBuffer[String]();def send(f:String,t:String,d:String,i:mutable.ArrayBuffer[String])=i+=s"$f>$t:$d";send("peer-a","peer-b","block-42",b);send("peer-a","peer-c","block-42",c);(b++c).mkString(">")=="peer-a>peer-b:block-42>peer-a>peer-c:block-42"} }
