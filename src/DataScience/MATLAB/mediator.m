function result = mediator
%MEDIATOR Coordinate colleagues through one mediator function.
afterButton = mediate("button", "clicked");
afterPanel = mediate("panel", "changed");
result = struct("buttonEvent", afterButton, "panelEvent", afterPanel);
end

function action = mediate(source, event)
if source == "button" && event == "clicked"
    action = "panel.refresh";
elseif source == "panel" && event == "changed"
    action = "button.enable";
else
    action = "ignore";
end
end
