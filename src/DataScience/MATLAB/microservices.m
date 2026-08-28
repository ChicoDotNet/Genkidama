function result = microservices
%MICROSERVICES Coordinate independently owned services through explicit contracts.
inventory = struct("sku1", 7);
order = struct("id", 1001, "status", "new");

reservation = inventoryService(inventory, struct("sku", "sku1", "quantity", 2));
order = orderService(order, struct("type", "inventory-reserved", "accepted", reservation.accepted));

result = struct("reserved", reservation.accepted, "remaining", reservation.remaining, "status", order.status);
end

function response = inventoryService(state, request)
available = state.(request.sku);
accepted = available >= request.quantity;
remaining = available - request.quantity * double(accepted);
response = struct("accepted", accepted, "remaining", remaining);
end

function order = orderService(order, event)
if event.type == "inventory-reserved" && event.accepted
    order.status = "confirmed";
end
end
