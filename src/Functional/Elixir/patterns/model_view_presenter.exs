presenter = fn model, view ->
  Map.put(view, :text, String.upcase(model.name))
end

presented = presenter.(%{name: "Ada"}, %{})

unless presented.text == "ADA" do
  raise "MVP"
end
