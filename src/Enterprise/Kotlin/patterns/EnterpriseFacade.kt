object EnterpriseFacadeExample{fun run():Boolean{val crm={id:Int->"crm:create:$id"};val billing={id:Int->"billing:open:$id"};return "${crm(77)}>${billing(77)}"=="crm:create:77>billing:open:77"}}
