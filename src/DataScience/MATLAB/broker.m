function result = broker
%BROKER Route client requests to registered services through an intermediary.
services = struct();
services.inventory = @inventoryService;
services.customer = @customerService;
result = struct( ...
    "inventory", brokerCall(services, "inventory", "sku-1"), ...
    "customer", brokerCall(services, "customer", "17"));
end

function response = brokerCall(services, serviceName, payload)
service = services.(char(serviceName));
response = service(payload);
end

function response = inventoryService(payload)
response = "inventory:" + payload + "=7";
end

function response = customerService(payload)
response = "customer:" + payload + "=active";
end
