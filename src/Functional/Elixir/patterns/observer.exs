subscriber = fn event -> {:seen, event} end
seen = Enum.map([subscriber], fn notify -> notify.(:changed) end)

unless seen == [{:seen, :changed}] do
  raise "Observer"
end
