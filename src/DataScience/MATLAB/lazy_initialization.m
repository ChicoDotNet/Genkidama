function result = lazy_initialization
%LAZY_INITIALIZATION Create an expensive value only on first demand.
holder = struct("initialized", false, "value", "", "creationCount", 0);
[holder, first] = getValue(holder);
[holder, second] = getValue(holder);
result = struct("first", first, "second", second, "creationCount", holder.creationCount);
end

function [holder, value] = getValue(holder)
if ~holder.initialized
    holder.value = "resource-ready";
    holder.initialized = true;
    holder.creationCount = holder.creationCount + 1;
end
value = holder.value;
end
