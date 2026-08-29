mailbox = [fn -> :done end]
task = hd(mailbox)

unless task.() == :done do
  raise "ActiveObject"
end
