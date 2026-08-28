function result = mvc
%MVC Separate model state, controller input handling and view rendering.
model = struct("count", 0);
before = renderView(model);
model = controller(model, "increment");
after = renderView(model);
result = struct("before", before, "after", after, "count", model.count);
end

function model = controller(model, action)
if action == "increment"
    model.count = model.count + 1;
end
end

function text = renderView(model)
text = "count=" + string(model.count);
end
