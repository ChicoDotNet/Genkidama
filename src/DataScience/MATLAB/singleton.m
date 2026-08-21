function singleton()
    first = registry_instance();
    second = registry_instance();
    first.increment();

    fprintf('same=%s\n', lower(string(first == second)));
    fprintf('count=%d\n', second.count());
end

function value = registry_instance()
    persistent shared
    if isempty(shared) || ~isvalid(shared)
        shared = RegistryState();
    end
    value = shared;
end

classdef RegistryState < handle
    properties (Access = private)
        CountValue = 0
    end

    methods
        function increment(self)
            self.CountValue = self.CountValue + 1;
        end

        function value = count(self)
            value = self.CountValue;
        end
    end
end
