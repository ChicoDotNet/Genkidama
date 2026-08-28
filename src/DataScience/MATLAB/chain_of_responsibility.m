function chain_of_responsibility
%CHAIN_OF_RESPONSIBILITY Route one refund through ordered handlers.
handlers = [
    struct("name", "faq", "limit", 50)
    struct("name", "billing", "limit", 500)
    struct("name", "escalation", "limit", inf)
];
amount = 250;
visited = strings(0, 1);
handled = "none";
result = "rejected";

for index = 1:numel(handlers)
    handler = handlers(index);
    visited(end + 1, 1) = handler.name;
    if amount <= handler.limit
        handled = handler.name;
        result = "refund(" + string(amount) + ")";
        break;
    end
end

fprintf('visited=%s;handled=%s;result=%s\n', strjoin(visited, ">"), handled, result);
end
