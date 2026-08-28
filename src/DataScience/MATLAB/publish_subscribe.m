function result = publish_subscribe
%PUBLISH_SUBSCRIBE Route a publication to subscribers of a topic.
subscriptions = struct();
subscriptions.orders = {@warehouseSubscriber, @analyticsSubscriber};
message = struct("topic", "orders", "id", 51);
subscribers = subscriptions.(char(message.topic));
received = strings(1, numel(subscribers));

for index = 1:numel(subscribers)
    received(index) = subscribers{index}(message);
end

result = struct("received", strjoin(received, ">"));
end

function text = warehouseSubscriber(message)
text = "warehouse:" + string(message.id);
end

function text = analyticsSubscriber(message)
text = "analytics:" + string(message.id);
end
