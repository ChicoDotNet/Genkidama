function result = visitor
%VISITOR Add operations over heterogeneous shapes without changing shape data.
shapes = {
    struct("kind", "circle", "a", 2, "b", 0)
    struct("kind", "rectangle", "a", 3, "b", 4)
};
areaVisitor = struct("circle", @circleArea, "rectangle", @rectangleArea);
labelVisitor = struct("circle", @circleLabel, "rectangle", @rectangleLabel);

areas = zeros(1, numel(shapes));
labels = strings(1, numel(shapes));
for index = 1:numel(shapes)
    areas(index) = accept(shapes{index}, areaVisitor);
    labels(index) = accept(shapes{index}, labelVisitor);
end

result = struct("areaTotal", sum(areas), "labels", strjoin(labels, ">"));
end

function value = accept(shape, visitor)
operation = visitor.(char(shape.kind));
value = operation(shape);
end

function value = circleArea(shape)
value = pi * shape.a * shape.a;
end

function value = rectangleArea(shape)
value = shape.a * shape.b;
end

function value = circleLabel(~)
value = "circle";
end

function value = rectangleLabel(~)
value = "rectangle";
end
