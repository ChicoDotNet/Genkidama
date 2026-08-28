function result = enterprise_facade
%ENTERPRISE_FACADE Offer one operation over multiple integration subsystems.
result = onboardCustomer(77);
end

function result = onboardCustomer(customerId)
crm = createCrmCustomer(customerId);
billing = openBillingAccount(customerId);
result = struct("customerId", customerId, "trace", crm + ">" + billing);
end

function trace = createCrmCustomer(customerId)
trace = "crm:create:" + string(customerId);
end

function trace = openBillingAccount(customerId)
trace = "billing:open:" + string(customerId);
end
