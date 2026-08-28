function result = model_view_presenter
%MODEL_VIEW_PRESENTER Let a presenter mediate a passive view and model.
model = struct("count", 0);
view = struct("text", "");
[model, view] = presenter(model, view, "increment");
result = struct("count", model.count, "viewText", view.text);
end

function [model, view] = presenter(model, view, action)
if action == "increment"
    model.count = model.count + 1;
end
view.text = "count=" + string(model.count);
end
