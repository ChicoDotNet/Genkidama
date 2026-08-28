function result = service_locator
%SERVICE_LOCATOR Resolve dependencies from a shared registry at runtime.
locator = struct();
locator.email = @(address) "email>" + address;
locator.audit = @(event) "audit>" + event;

email = resolve(locator, "email");
audit = resolve(locator, "audit");
result = struct("email", email("a@example.test"), "audit", audit("created"));
end

function service = resolve(locator, name)
if ~isfield(locator, char(name))
    error("Unknown service: %s", name);
end
service = locator.(char(name));
end
