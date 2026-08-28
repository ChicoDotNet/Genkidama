function result = active_object
%ACTIVE_OBJECT Decouple invocation from execution with a command queue and scheduler.
servant = struct("value", 0);
queue = {
    struct("operation", "add", "amount", 3)
    struct("operation", "multiply", "amount", 4)
};

before = servant.value;
[servant, trace] = runScheduler(servant, queue);
result = struct("before", before, "after", servant.value, "trace", trace);
end

function [servant, trace] = runScheduler(servant, queue)
steps = strings(1, numel(queue));
for index = 1:numel(queue)
    request = queue{index};
    switch request.operation
        case "add"
            servant.value = servant.value + request.amount;
        case "multiply"
            servant.value = servant.value * request.amount;
        otherwise
            error("Unknown operation");
    end
    steps(index) = request.operation;
end
trace = strjoin(steps, ">");
end
