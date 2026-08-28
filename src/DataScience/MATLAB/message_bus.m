function result = message_bus
%MESSAGE_BUS Deliver one message to independent handlers through a bus.
handlers = {@auditHandler, @billingHandler};
deliveries = strings(1, numel(handlers));
message = struct("type", "order-created", "id", 42);

for index = 1:numel(handlers)
    deliveries(index) = handlers{index}(message);
end

result = struct("deliveries", strjoin(deliveries, ">"));
end

function text = auditHandler(message)
text = "audit:" + message.type + ":" + string(message.id);
end

function text = billingHandler(message)
text = "billing:" + message.type + ":" + string(message.id);
end
