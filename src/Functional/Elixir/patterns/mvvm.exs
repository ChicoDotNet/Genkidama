view_model = fn model -> "#{model.first} #{model.last}" end
model = %{first: "Ada", last: "Lovelace"}

unless view_model.(model) == "Ada Lovelace" do
  raise "MVVM"
end
