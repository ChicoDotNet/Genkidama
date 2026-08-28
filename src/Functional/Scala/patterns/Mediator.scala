import scala.collection.mutable
object MediatorExample { def run:Boolean={val e=mutable.ArrayBuffer[String]();def notify(s:String,x:String):Unit={if(s=="button"&&x=="click")e+="panel.refresh";if(s=="panel"&&x=="loaded")e+="button.enable"};notify("button","click");notify("panel","loaded");e.mkString(">")=="panel.refresh>button.enable"} }
