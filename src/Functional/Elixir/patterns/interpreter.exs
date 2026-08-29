evaluate = fn {:add, {:var, :x}, {:lit, number}}, environment ->
  environment.x + number
end

expression = {:add, {:var, :x}, {:lit, 3}}

unless evaluate.(expression, %{x: 4}) == 7 do
  raise "Interpreter"
end
