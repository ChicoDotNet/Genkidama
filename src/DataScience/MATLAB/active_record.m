function result = active_record
%ACTIVE_RECORD Couple row data with persistence-style operations.
store = struct();
record = struct("id", 7, "name", "Ada");
store = saveRecord(store, record);
found = findRecord(store, 7);
result = found;
end

function store = saveRecord(store, record)
key = char("id" + string(record.id));
store.(key) = record;
end

function record = findRecord(store, id)
key = char("id" + string(id));
record = store.(key);
end
