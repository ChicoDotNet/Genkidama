backendCreated = 0;
fetches = 0;
cache = containers.Map('KeyType', 'double', 'ValueType', 'char');

[first, backendCreated, fetches, cache] = proxyGet(42, backendCreated, fetches, cache);
[second, backendCreated, fetches, cache] = proxyGet(42, backendCreated, fetches, cache);

assert(backendCreated == 1, 'Proxy must lazily create exactly one backend.');
assert(fetches == 1, 'Second read must be served from cache.');
fprintf('backend=%d;fetches=%d;first=%s;second=%s\n', backendCreated, fetches, first, second);

function [value, backendCreated, fetches, cache] = proxyGet(id, backendCreated, fetches, cache)
    if isKey(cache, id)
        value = cache(id);
        return;
    end

    if backendCreated == 0
        backendCreated = 1;
    end

    fetches = fetches + 1;
    value = sprintf('doc(%d)', id);
    cache(id) = value;
end
