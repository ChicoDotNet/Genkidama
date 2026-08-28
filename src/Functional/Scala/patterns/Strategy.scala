object StrategyExample { def run:Boolean={def price(v:Int,s:Int=>Int)=s(v);price(100,identity)==100&&price(100,_*80/100)==80} }
