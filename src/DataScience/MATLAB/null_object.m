function result = null_object
%NULL_OBJECT Replace absence with a behavior-compatible no-op object/function.
realLogger = @(message) "logged:" + message;
nullLogger = @(message) "";
realResult = processItem("item-1", realLogger);
nullResult = processItem("item-1", nullLogger);
result = struct("real", realResult, "null", nullResult);
end

function output = processItem(item, logger)
output = logger("processed:" + item);
end
