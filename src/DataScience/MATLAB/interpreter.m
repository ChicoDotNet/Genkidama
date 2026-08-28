function result = interpreter
%INTERPRETER Evaluate a tiny arithmetic language represented as an AST.
expression = binary("add", numberNode(7), binary("multiply", numberNode(3), numberNode(4)));
result = struct("value", evaluate(expression), "expression", "7+(3*4)");
end

function node = numberNode(value)
node = struct("kind", "number", "value", value, "left", [], "right", []);
end

function node = binary(kind, left, right)
node = struct("kind", kind, "value", 0, "left", left, "right", right);
end

function value = evaluate(node)
switch node.kind
    case "number"
        value = node.value;
    case "add"
        value = evaluate(node.left) + evaluate(node.right);
    case "multiply"
        value = evaluate(node.left) * evaluate(node.right);
    otherwise
        error("Unknown expression kind: %s", node.kind);
end
end
