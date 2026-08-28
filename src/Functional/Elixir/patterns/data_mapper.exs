mapper = fn row -> %{user_name: row.name} end
mapped = mapper.(%{name: "Ada"})

unless mapped.user_name == "Ada" do
  raise "Mapper"
end
