function result = dependency_injection
%DEPENDENCY_INJECTION Supply dependencies from outside the consumer.
productionMailer = @(name) "smtp:" + name;
testMailer = @(name) "fake:" + name;
result = struct( ...
    "production", welcome("Ada", productionMailer), ...
    "test", welcome("Ada", testMailer));
end

function output = welcome(name, mailer)
output = mailer(name);
end
