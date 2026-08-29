guarded_increment = fn state ->
  Map.update!(state, :value, &(&1 + 1))
end

updated = guarded_increment.(%{value: 0})

unless updated.value == 1 do
  raise "Monitor"
end
