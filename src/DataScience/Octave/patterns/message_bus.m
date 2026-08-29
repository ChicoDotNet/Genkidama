function message_bus(); consumer=@(v)v+1; bus.paid={consumer}; assert(bus.paid{1}(42)==43); end
