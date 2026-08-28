function result = data_mapper
%DATA_MAPPER Keep domain objects free of persistence mechanics.
domain = struct("id", 8, "name", "Grace");
row = toRow(domain);
loaded = fromRow(row);
result = struct("rowKey", row.key, "domainName", loaded.name);
end

function row = toRow(domain)
row = struct("key", "person:" + string(domain.id), "payload", domain.name);
end

function domain = fromRow(row)
parts = split(row.key, ":");
domain = struct("id", str2double(parts(2)), "name", row.payload);
end
