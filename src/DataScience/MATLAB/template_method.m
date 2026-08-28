function result = template_method
%TEMPLATE_METHOD Keep an algorithm skeleton fixed while varying one step.
csvReport = buildReport(@readCsv, @normalize);
jsonReport = buildReport(@readJson, @aggregate);
result = struct("csv", csvReport, "json", jsonReport);
end

function trace = buildReport(readStep, transformStep)
data = readStep();
data = transformStep(data);
trace = data + ">publish";
end

function value = readCsv
value = "read-csv";
end

function value = readJson
value = "read-json";
end

function value = normalize(value)
value = value + ">normalize";
end

function value = aggregate(value)
value = value + ">aggregate";
end
