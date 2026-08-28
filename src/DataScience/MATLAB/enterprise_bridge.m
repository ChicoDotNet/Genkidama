function result = enterprise_bridge
%ENTERPRISE_BRIDGE Vary message abstraction and transport independently.
alert = sendMessage(@formatAlert, @sendKafka, "disk");
reminder = sendMessage(@formatReminder, @sendQueue, "backup");
result = struct("alert", alert, "reminder", reminder);
end

function output = sendMessage(formatter, transport, payload)
output = transport(formatter(payload));
end

function text = formatAlert(payload)
text = "ALERT:" + payload;
end

function text = formatReminder(payload)
text = "REMINDER:" + payload;
end

function output = sendKafka(message)
output = "kafka>" + message;
end

function output = sendQueue(message)
output = "queue>" + message;
end
