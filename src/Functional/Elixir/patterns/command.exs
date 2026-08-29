commands = [
  fn value -> value + 50 end,
  fn value -> value - 20 end
]

result = Enum.reduce(commands, 100, fn command, value -> command.(value) end)

unless result == 130 do
  raise "Command"
end
