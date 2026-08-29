visit = fn %{value: value} -> value * 2 end

unless visit.(%{value: 5}) == 10 do
  raise "Visitor"
end
