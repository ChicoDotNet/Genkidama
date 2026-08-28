save=fn record,table->Map.put(table,record.id,%{name:record.name}) end; unless save.(%{id:1,name:"Ada"},%{})[1].name=="Ada",do: raise "ActiveRecord"
