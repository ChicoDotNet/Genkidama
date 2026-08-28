object MvvmExample{fun run():Boolean{data class Vm(var amount:Int);val vm=Vm(10);val text={"$${vm.amount}.00"};val before=text();vm.amount+=5;return before=="$10.00"&&text()=="$15.00"}}
