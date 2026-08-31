function result = mediator
%MEDIATOR Coordinate colleagues through one mediator-owned routing table.
events = strings(1, 0);
routes = struct("inventory", @inventoryReceive, "payment", @paymentReceive);

payment("paid");
inventory("reserved");

rejected = false;
try
    mediate("payment", "unknown", "ignored");
catch exception
    rejected = strcmp(exception.identifier, "Mediator:UnknownColleague");
end

result = struct( ...
    "events", strjoin(events, ">"), ...
    "rejectedUnknown", rejected);

    function payment(message)
        mediate("payment", "inventory", message);
    end

    function inventory(message)
        mediate("inventory", "payment", message);
    end

    function mediate(sender, recipient, message)
        if ~isfield(routes, recipient)
            error("Mediator:UnknownColleague", "unknown colleague: %s", recipient);
        end
        receiver = routes.(recipient);
        receiver(sender, message);
    end

    function inventoryReceive(sender, message)
        events(end + 1) = "inventory<-" + sender + ":" + message;
    end

    function paymentReceive(sender, message)
        events(end + 1) = "payment<-" + sender + ":" + message;
    end
end
