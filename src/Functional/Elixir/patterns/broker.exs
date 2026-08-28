handlers = %{price: fn _payload -> 9 end}

request = fn topic, payload ->
  handlers[topic].(payload)
end

unless request.(:price, "A") == 9 do
  raise "Broker"
end
