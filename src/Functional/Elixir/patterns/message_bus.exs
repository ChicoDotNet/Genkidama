bus = %{paid: [fn value -> {:seen, value} end]}
seen = Enum.map(bus.paid, fn handler -> handler.(42) end)

unless seen == [{:seen, 42}] do
  raise "MessageBus"
end
