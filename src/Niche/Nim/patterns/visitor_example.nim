import std/[math, sequtils]
type
  ShapeKind = enum skCircle, skRect
  Shape = object
    case kind: ShapeKind
    of skCircle: radius: float
    of skRect: width, height: float
proc area(shape: Shape): float =
  case shape.kind
  of skCircle: PI * shape.radius * shape.radius
  of skRect: shape.width * shape.height
proc run*(): bool =
  let shapes = @[Shape(kind: skCircle, radius: 2.0), Shape(kind: skRect, width: 3.0, height: 4.0)]
  abs(shapes.mapIt(area(it)).foldl(a + b) - (4.0 * PI + 12.0)) < 1.0e-9
