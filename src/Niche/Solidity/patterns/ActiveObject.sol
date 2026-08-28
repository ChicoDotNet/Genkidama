// SPDX-License-Identifier: MIT
pragma solidity >=0.8.0 <0.9.0;

library ActiveObjectPattern {
    enum Operation { AddThree, MultiplyFour }
    function execute(uint256 value, Operation operation) private pure returns (uint256) {
        return operation == Operation.AddThree ? value + 3 : value * 4;
    }
    function run() internal pure returns (bool) {
        Operation[2] memory queue = [Operation.AddThree, Operation.MultiplyFour];
        uint256 value;
        for (uint256 i; i < queue.length; i++) value = execute(value, queue[i]);
        return value == 12;
    }
}
