function broker(); handlers.price=@(~)9; request=@(topic,payload)handlers.(topic)(payload); assert(request('price','A')==9); end
