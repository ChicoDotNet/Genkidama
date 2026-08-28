send_direct = fn source, target, message ->
  {target, {source, message}}
end

unless send_direct.(:a, :b, "hello") == {:b, {:a, "hello"}} do
  raise "Peer"
end
