greet = fn clock -> "hello@#{clock.()}" end
clock = fn -> "noon" end

unless greet.(clock) == "hello@noon" do
  raise "DI"
end
