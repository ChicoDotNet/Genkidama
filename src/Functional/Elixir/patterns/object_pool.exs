item = %{id: 1}
[borrowed | rest] = [item]
pool = [borrowed | rest]

unless hd(pool) == item do
  raise "Pool"
end
