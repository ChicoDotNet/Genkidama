function microservices(); inventory=@(sku)struct('sku',sku,'available',true); order=@(sku)inventory(sku).available; assert(order('A-1')); end
