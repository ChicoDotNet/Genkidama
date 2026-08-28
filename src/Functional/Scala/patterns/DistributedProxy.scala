object DistributedProxyExample { def run:Boolean={def remote(s:String)=if(s=="sku-1")7 else 0;def proxy(s:String)=remote(s);proxy("sku-1")==7} }
