function result = microkernel
%MICROKERNEL Keep a small core and extend behavior through registered plugins.
kernel = struct();
kernel.plugins.double = @(value) value * 2;
kernel.plugins.square = @(value) value * value;
result = struct( ...
    "double", invokePlugin(kernel, "double", 4), ...
    "square", invokePlugin(kernel, "square", 4));
end

function value = invokePlugin(kernel, name, input)
if ~isfield(kernel.plugins, char(name))
    error("Unknown plugin: %s", name);
end
plugin = kernel.plugins.(char(name));
value = plugin(input);
end
