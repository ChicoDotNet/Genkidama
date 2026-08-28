object ClientServerExample { def run:Boolean={def server(k:String)=if(k=="sku-1")(200,"stock=7")else(404,"missing");server("sku-1")==((200,"stock=7"))} }
