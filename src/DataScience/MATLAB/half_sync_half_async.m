function result = half_sync_half_async
%HALF_SYNC_HALF_ASYNC Queue asynchronous arrivals for synchronous processing.
asyncArrivals = ["job-1", "job-2", "job-3"];
queue = asyncArrivals;
processed = strings(1, numel(queue));

for index = 1:numel(queue)
    processed(index) = syncWorker(queue(index));
end

result = struct("queued", strjoin(queue, ">"), "processed", strjoin(processed, ">"));
end

function output = syncWorker(job)
output = "done:" + job;
end
