function result = strategy
%STRATEGY Swap pricing algorithms behind the same calling contract.
amount = 100;
regular = price(amount, @regularPrice);
vip = price(amount, @vipPrice);
result = struct("regular", regular, "vip", vip);
end

function total = price(amount, strategyFunction)
total = strategyFunction(amount);
end

function total = regularPrice(amount)
total = amount;
end

function total = vipPrice(amount)
total = amount * 0.8;
end
