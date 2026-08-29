controller = fn model ->
  Map.update!(model, :count, &(&1 + 1))
end

view = fn model -> "count=#{model.count}" end
updated = controller.(%{count: 0})

unless view.(updated) == "count=1" do
  raise "MVC"
end
