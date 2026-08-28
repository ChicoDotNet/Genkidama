// SPDX-License-Identifier: MIT
pragma solidity >=0.8.0 <0.9.0;

library CommandPattern {
    enum Kind { Deposit, Withdraw }
    struct Command { Kind kind; uint256 amount; }
    function execute(uint256 balance, Command memory command) private pure returns (uint256) {
        return command.kind == Kind.Deposit ? balance + command.amount : balance - command.amount;
    }
    function run() internal pure returns (bool) {
        Command[2] memory queue;
        queue[0] = Command(Kind.Deposit, 50);
        queue[1] = Command(Kind.Withdraw, 20);
        uint256 balance = 100;
        balance = execute(balance, queue[0]);
        balance = execute(balance, queue[1]);
        return balance == 130 && execute(150, queue[1]) == 130;
    }
}
