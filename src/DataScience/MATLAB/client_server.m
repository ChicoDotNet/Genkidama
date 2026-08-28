function result = client_server
%CLIENT_SERVER Separate request initiation from centralized request handling.
request = struct("operation", "lookup", "key", "sku-1");
response = client(@server, request);
result = response;
end

function response = client(serverFunction, request)
response = serverFunction(request);
end

function response = server(request)
if request.operation == "lookup" && request.key == "sku-1"
    response = struct("status", 200, "body", "stock=7");
else
    response = struct("status", 404, "body", "not-found");
end
end
