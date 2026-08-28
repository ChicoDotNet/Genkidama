object RepositoryExample{fun run():Boolean{data class P(val id:Int,val name:String);val rows=listOf(P(1,"Ada"),P(2,"Grace"));return rows.find{it.id==2}?.name=="Grace"}}
