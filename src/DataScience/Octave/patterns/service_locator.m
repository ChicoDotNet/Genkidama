function service_locator(); services.clock=@()'12:00'; assert(strcmp(services.clock(),'12:00')); end
