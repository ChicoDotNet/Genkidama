get = fn
  nil -> {7, 1}
  value -> {value, 0}
end

{value, first_initializations} = get.(nil)
{_value, second_initializations} = get.(value)

unless first_initializations + second_initializations == 1 do
  raise "Lazy"
end
