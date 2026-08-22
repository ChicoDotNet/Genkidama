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
