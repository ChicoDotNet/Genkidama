function result = monitor_object
%MONITOR_OBJECT Encapsulate shared state behind serialized monitor operations.
monitor = struct("value", 0, "locked", false, "maxCritical", 0);
operations = {
    struct("name", "increment", "amount", 2)
    struct("name", "increment", "amount", 3)
    struct("name", "read", "amount", 0)
};

[monitor, observations] = runMonitor(monitor, operations);
result = struct( ...
    "value", monitor.value, ...
    "observations", observations, ...
    "locked", monitor.locked, ...
    "maxCritical", monitor.maxCritical);
end

function [monitor, observations] = runMonitor(monitor, operations)
observations = strings(1, 0);
for index = 1:numel(operations)
    [monitor, observation] = invokeMonitor(monitor, operations{index});
    if observation ~= ""
        observations(end + 1) = observation; %#ok<AGROW>
    end
end
end

function [monitor, observation] = invokeMonitor(monitor, operation)
if monitor.locked
    error("Monitor re-entry would violate mutual exclusion");
end

monitor.locked = true;
monitor.maxCritical = max(monitor.maxCritical, 1);
observation = "";

switch operation.name
    case "increment"
        monitor.value = monitor.value + operation.amount;
    case "read"
        observation = string(monitor.value);
    otherwise
        monitor.locked = false;
        error("Unknown monitor operation");
end

monitor.locked = false;
end
