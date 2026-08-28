object MvvmExample { def run:Boolean={var a=10;def text=s"$$$a.00";val before=text;a+=5;before=="$10.00"&&text=="$15.00"} }
