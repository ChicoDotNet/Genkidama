function result = state
%STATE Change behavior when the context transitions between states.
current = "locked";
sequence = current;
[current, firstAction] = handleEvent(current, "coin");
sequence = sequence + ">" + current;
[current, secondAction] = handleEvent(current, "push");
sequence = sequence + ">" + current;
result = struct("sequence", sequence, "actions", firstAction + ">" + secondAction);
end

function [next, action] = handleEvent(current, event)
if current == "locked" && event == "coin"
    next = "unlocked";
    action = "unlock";
elseif current == "unlocked" && event == "push"
    next = "locked";
    action = "lock";
else
    next = current;
    action = "ignored";
end
end
