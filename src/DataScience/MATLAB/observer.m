function result = observer
%OBSERVER Notify independent subscribers when subject state changes.
observers = {@auditObserver, @dashboardObserver};
notifications = strings(1, numel(observers));
newValue = 42;

for index = 1:numel(observers)
    notifications(index) = observers{index}(newValue);
end

result = struct("value", newValue, "notifications", strjoin(notifications, ">"));
end

function message = auditObserver(value)
message = "audit:" + string(value);
end

function message = dashboardObserver(value)
message = "dashboard:" + string(value);
end
