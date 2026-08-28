function result = mvvm
%MVVM Expose model state and commands through a view-model boundary.
model = struct("amount", 10);
viewModel = makeViewModel(model);
before = viewModel.text;
model = viewModel.increment(model, 5);
viewModel = makeViewModel(model);
after = viewModel.text;
result = struct("before", before, "after", after, "amount", model.amount);
end

function vm = makeViewModel(model)
vm = struct( ...
    "text", "$" + string(model.amount) + ".00", ...
    "increment", @incrementAmount);
end

function model = incrementAmount(model, delta)
model.amount = model.amount + delta;
end
