def handlers=[price:{sku->9}];def request={topic,payload->handlers[topic](payload)};assert request('price','A')==9
