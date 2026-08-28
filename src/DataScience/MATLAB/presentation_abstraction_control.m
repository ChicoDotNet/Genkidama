function result = presentation_abstraction_control
%PRESENTATION_ABSTRACTION_CONTROL Coordinate hierarchical agents through controllers.
child = makeAgent("child", 40);
child = control(child, "increment", 2);
root = makeAgent("root", child.abstraction.value);
result = struct("childView", present(child), "rootView", present(root));
end

function agent = makeAgent(name, value)
agent = struct("name", name, "abstraction", struct("value", value));
end

function agent = control(agent, action, amount)
if action == "increment"
    agent.abstraction.value = agent.abstraction.value + amount;
end
end

function view = present(agent)
view = agent.name + ":view=" + string(agent.abstraction.value);
end
