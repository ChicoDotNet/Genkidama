// SPDX-License-Identifier: MIT
pragma solidity >=0.8.0 <0.9.0;

library VisitorPattern {
    enum Kind { Circle, Rectangle }
    struct Shape { Kind kind; uint256 a; uint256 b; }
    function area(Shape memory shape) private pure returns (uint256) {
        return shape.kind == Kind.Circle ? 314 * shape.a * shape.a / 100 : shape.a * shape.b;
    }
    function run() internal pure returns (bool) {
        Shape memory circle = Shape(Kind.Circle, 2, 0);
        Shape memory rectangle = Shape(Kind.Rectangle, 3, 4);
        return area(circle) == 12 && area(rectangle) == 12;
    }
}
