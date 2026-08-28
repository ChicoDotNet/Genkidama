function result = memento
%MEMENTO Capture and restore originator state without exposing mutation details.
originator = struct("text", "draft");
snapshot = saveState(originator);
originator.text = "published";
changed = originator.text;
originator = restoreState(originator, snapshot);
result = struct("changed", changed, "restored", originator.text);
end

function snapshot = saveState(originator)
snapshot = struct("text", originator.text);
end

function originator = restoreState(originator, snapshot)
originator.text = snapshot.text;
end
