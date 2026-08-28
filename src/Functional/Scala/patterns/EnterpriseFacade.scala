object EnterpriseFacadeExample { def run:Boolean={def crm(i:Int)=s"crm:create:$i";def billing(i:Int)=s"billing:open:$i";s"${crm(77)}>${billing(77)}"=="crm:create:77>billing:open:77"} }
