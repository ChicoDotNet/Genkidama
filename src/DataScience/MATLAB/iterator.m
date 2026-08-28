function result = iterator
%ITERATOR Traverse a collection through an explicit iteration protocol.
cursor = struct("items", [10 20 30], "index", 1);
visited = zeros(1, 0);

while hasNext(cursor)
    [value, cursor] = nextValue(cursor);
    visited(end + 1) = value; %#ok<AGROW>
end

result = struct("visited", visited, "finished", ~hasNext(cursor));
end

function answer = hasNext(cursor)
answer = cursor.index <= numel(cursor.items);
end

function [value, cursor] = nextValue(cursor)
if ~hasNext(cursor)
    error("Iterator exhausted");
end
value = cursor.items(cursor.index);
cursor.index = cursor.index + 1;
end
