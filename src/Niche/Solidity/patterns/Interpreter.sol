// SPDX-License-Identifier: MIT
pragma solidity >=0.8.0 <0.9.0;

library InterpreterPattern {
    enum Kind { Literal, Add, Multiply }
    struct Token { Kind kind; uint256 value; }
    function run() internal pure returns (bool) {
        Token[5] memory program;
        program[0] = Token(Kind.Literal, 7);
        program[1] = Token(Kind.Literal, 3);
        program[2] = Token(Kind.Literal, 4);
        program[3] = Token(Kind.Multiply, 0);
        program[4] = Token(Kind.Add, 0);
        uint256[5] memory stack;
        uint256 top;
        for (uint256 i; i < program.length; i++) {
            Token memory token = program[i];
            if (token.kind == Kind.Literal) stack[top++] = token.value;
            else {
                uint256 right = stack[--top];
                uint256 left = stack[--top];
                stack[top++] = token.kind == Kind.Add ? left + right : left * right;
            }
        }
        return top == 1 && stack[0] == 19;
    }
}
