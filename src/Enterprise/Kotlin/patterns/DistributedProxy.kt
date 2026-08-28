object DistributedProxyExample{fun run():Boolean{val remote={sku:String->if(sku=="sku-1")7 else 0};val proxy={sku:String->remote(sku)};return proxy("sku-1")==7}}
