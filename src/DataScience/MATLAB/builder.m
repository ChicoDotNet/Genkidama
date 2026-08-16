function builder
%BUILDER Build one report recipe into text and HTML representations.
textBuilder = createBuilder("text");
htmlBuilder = createBuilder("html");
fprintf('%s\n', buildAvailabilityReport(textBuilder));
fprintf('---\n');
fprintf('%s\n', buildAvailabilityReport(htmlBuilder));
end

function target = createBuilder(format)
target = struct('format', format, 'parts', strings(0, 1));
end

function target = reset(target)
target.parts = strings(0, 1);
end

function target = addTitle(target, title)
if target.format == "text"
    target.parts(end + 1, 1) = "# " + title;
else
    target.parts(end + 1, 1) = "<h1>" + title + "</h1>";
end
end

function target = addSection(target, heading, body)
if target.format == "text"
    target.parts(end + 1, 1) = "## " + heading;
    target.parts(end + 1, 1) = body;
else
    target.parts(end + 1, 1) = "<h2>" + heading + "</h2><p>" + body + "</p>";
end
end

function report = buildAvailabilityReport(target)
target = reset(target);
target = addTitle(target, "Service status");
target = addSection(target, "Availability", "99.95%");
report = strjoin(target.parts, newline);
end
