commit = fn pending, database -> {database ++ pending, []} end
pending = [%{id: 1}]

unless commit.(pending, []) == {[%{id: 1}], []} do
  raise "UoW"
end
