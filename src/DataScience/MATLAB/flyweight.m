pool = containers.Map('KeyType', 'char', 'ValueType', 'double');
nextId = 0;

[red1, pool, nextId] = getStyle(pool, nextId, 'Inter', 12, 'red');
[red2, pool, nextId] = getStyle(pool, nextId, 'Inter', 12, 'red');
[blue, pool, nextId] = getStyle(pool, nextId, 'Inter', 12, 'blue');

assert(blue ~= red1, 'Distinct intrinsic state must not alias.');
shared = string(red1 == red2);
fprintf('styles=%d;shared=%s;text=ABC\n', pool.Count, lower(shared));

function [id, pool, nextId] = getStyle(pool, nextId, font, fontSize, color)
    key = sprintf('%s|%d|%s', font, fontSize, color);
    if isKey(pool, key)
        id = pool(key);
        return;
    end
    nextId = nextId + 1;
    pool(key) = nextId;
    id = nextId;
end
