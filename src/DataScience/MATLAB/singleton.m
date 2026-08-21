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
