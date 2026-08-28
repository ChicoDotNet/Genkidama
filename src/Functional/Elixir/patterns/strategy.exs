choose = fn values, strategy -> strategy.(values) end
values = [3, 1, 2]

unless choose.(values, &Enum.min/1) == 1 and choose.(values, &Enum.max/1) == 3 do
  raise "Strategy"
end
