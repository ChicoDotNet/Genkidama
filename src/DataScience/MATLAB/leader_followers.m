function result = leader_followers
%LEADER_FOLLOWERS Rotate leadership through a worker pool as events arrive.
workers = ["worker-1", "worker-2", "worker-3"];
events = ["event-a", "event-b", "event-c"];
handled = strings(1, numel(events));

leaderIndex = 1;
for index = 1:numel(events)
    handled(index) = workers(leaderIndex) + ":" + events(index);
    leaderIndex = mod(leaderIndex, numel(workers)) + 1;
end

result = struct("handled", strjoin(handled, ">"), "nextLeader", workers(leaderIndex));
end
