function result = distributed_proxy
%DISTRIBUTED_PROXY Hide remote invocation details behind a local proxy.
proxy = @(key) remoteCall(@inventoryService, struct("operation", "get", "key", key));
result = proxy("sku-1");
end

function response = remoteCall(service, request)
wireRequest = request;
wireResponse = service(wireRequest);
response = wireResponse;
end

function response = inventoryService(request)
if request.operation == "get" && request.key == "sku-1"
    response = struct("stock", 7, "source", "remote");
else
    response = struct("stock", 0, "source", "remote");
end
end
