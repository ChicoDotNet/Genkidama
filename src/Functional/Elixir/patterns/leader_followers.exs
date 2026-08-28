roles = [:leader, :follower]
requests = [:one, :two]
handled = Enum.zip(roles, requests)

unless handled == [leader: :one, follower: :two] do
  raise "LeaderFollowers"
end
