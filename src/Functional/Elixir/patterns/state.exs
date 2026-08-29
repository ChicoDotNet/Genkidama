toggle = fn
  :closed -> :open
  :open -> :closed
end

unless toggle.(:closed) == :open do
  raise "State"
end
