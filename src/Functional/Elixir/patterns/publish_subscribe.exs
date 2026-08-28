topics = %{news: [fn value -> {:received, value} end]}
received = Enum.map(topics.news, fn subscriber -> subscriber.("v1") end)

unless received == [{:received, "v1"}] do
  raise "PubSub"
end
