function prototype
    original = struct('name', "orders", 'features', {{"metrics"}});
    canary = clone_profile(original);
    canary.name = "orders-canary";
    canary.features{end + 1} = "tracing";

    fprintf('original=%s\n', describe(original));
    fprintf('clone=%s\n', describe(canary));
end

function copy = clone_profile(profile)
    copy = struct('name', profile.name, 'features', {profile.features});
end

function text = describe(profile)
    text = profile.name + ": " + strjoin(string(profile.features), ",");
end
