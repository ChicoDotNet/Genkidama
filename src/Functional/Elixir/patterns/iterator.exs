values = [3, 2, 1]
visited = Enum.map(values, & &1)

unless visited == values do
  raise "Iterator"
end
