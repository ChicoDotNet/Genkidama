control = fn abstraction, delta ->
  Map.update!(abstraction, :value, &(&1 + delta))
end

presentation = fn abstraction -> to_string(abstraction.value) end
updated = control.(%{value: 1}, 2)

unless presentation.(updated) == "3" do
  raise "PAC"
end
