sender = fn message -> "sms:#{message}" end
notify = fn message -> sender.(message) end

unless notify.("ok") == "sms:ok" do
  raise "Bridge"
end
