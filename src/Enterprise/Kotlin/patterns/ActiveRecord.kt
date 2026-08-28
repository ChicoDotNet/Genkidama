object ActiveRecordExample{fun run():Boolean{data class P(val id:Int,val name:String);val table=mutableMapOf<Int,P>();val p=P(7,"Ada");table[p.id]=p;return table[7]?.name=="Ada"}}
