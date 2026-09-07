"""Interpreter pattern: evaluate a tiny expression language with Python data and recursion.

Grammar (AST form):
    Expr := ("lit", int) | ("var", name) | ("add", Expr, Expr)
"""
from __future__ import annotations

from typing import TypeAlias

Expr: TypeAlias = tuple


def interpret(node: Expr, context: dict[str, int]) -> int:
    kind, *args = node
    if kind == "lit":
        return int(args[0])
    if kind == "var":
        return context[str(args[0])]
    if kind == "add":
        return interpret(args[0], context) + interpret(args[1], context)
    raise ValueError(f"unsupported expression: {kind}")


def main() -> None:
    expression: Expr = ("add", ("var", "x"), ("lit", 3))
    value = interpret(expression, {"x": 4})
    assert value == 7
    print("interpreter=7")


if __name__ == "__main__":
    main()
