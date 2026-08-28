function result = unit_of_work
%UNIT_OF_WORK Track changes and commit them as one explicit unit.
store = struct("a", 10, "b", 20);
unit = {
    struct("key", "a", "delta", 5)
    struct("key", "b", "delta", -3)
};
before = [store.a store.b];
store = commitUnit(store, unit);
after = [store.a store.b];
result = struct("before", before, "after", after, "committed", true);
end

function store = commitUnit(store, unit)
staged = store;
for index = 1:numel(unit)
    change = unit{index};
    staged.(change.key) = staged.(change.key) + change.delta;
end
store = staged;
end
