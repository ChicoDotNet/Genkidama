function result = object_pool
%OBJECT_POOL Reuse bounded expensive resources instead of recreating them.
pool = struct("available", [1 2], "inUse", []);
[pool, first] = acquire(pool);
[pool, second] = acquire(pool);
pool = release(pool, first);
[pool, reused] = acquire(pool);
result = struct("first", first, "second", second, "reused", reused, "available", pool.available);
end

function [pool, resource] = acquire(pool)
if isempty(pool.available)
    error("Pool exhausted");
end
resource = pool.available(1);
pool.available(1) = [];
pool.inUse(end + 1) = resource;
end

function pool = release(pool, resource)
pool.inUse(pool.inUse == resource) = [];
pool.available = [resource pool.available];
end
