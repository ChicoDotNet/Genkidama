function result = enterprise_adapter
%ENTERPRISE_ADAPTER Translate an external legacy contract into the local model.
legacy = struct("customer_id", 17, "amount_cents", 1250);
canonical = adaptLegacyInvoice(legacy);
result = canonical;
end

function invoice = adaptLegacyInvoice(legacy)
invoice = struct( ...
    "customerId", legacy.customer_id, ...
    "amount", legacy.amount_cents / 100);
end
