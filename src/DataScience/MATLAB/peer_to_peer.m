function result = peer_to_peer
%PEER_TO_PEER Let peers both provide and consume information without a central server.
peers = ["peer-a", "peer-b", "peer-c"];
message = "block-42";
deliveries = strings(1, numel(peers) - 1);

for index = 2:numel(peers)
    deliveries(index - 1) = peers(1) + ">" + peers(index) + ":" + message;
end

result = struct("origin", peers(1), "deliveries", strjoin(deliveries, ">"));
end
