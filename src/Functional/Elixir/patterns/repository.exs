data = %{1 => %{name: "Ada"}}
get = fn id -> data[id] end

unless get.(1).name == "Ada" do
  raise "Repository"
end
