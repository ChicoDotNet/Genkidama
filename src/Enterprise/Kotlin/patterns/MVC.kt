object MvcExample{fun run():Boolean{data class Model(var count:Int);val m=Model(0);val render={"count=${m.count}"};val before=render();m.count++;return before=="count=0"&&render()=="count=1"}}
