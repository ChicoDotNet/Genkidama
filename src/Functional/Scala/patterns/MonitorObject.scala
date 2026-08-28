object MonitorObjectExample { class C{private var v=0;def add(x:Int):Unit=this.synchronized{v+=x};def get:Int=this.synchronized{v}};def run:Boolean={val c=new C;c.add(2);c.add(3);c.get==5} }
