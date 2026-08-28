save = fn record, table ->
  Map.put(table, record.id, %{name: record.name})
end

record = %{id: 1, name: "Ada"}
saved = save.(record, %{})

unless saved[1].name == "Ada" do
  raise "ActiveRecord"
end
