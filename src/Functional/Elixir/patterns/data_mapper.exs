mapper=fn row->%{user_name:row.name} end; unless mapper.(%{name:"Ada"}).user_name=="Ada",do: raise "Mapper"
