// SPDX-License-Identifier: MIT
pragma solidity >=0.8.0 <0.9.0;

library UnitOfWorkPattern {
    function run() internal pure returns (bool) {
        uint256[2] memory pending = [uint256(2), 3];
        uint256[2] memory store;
        for (uint256 i; i < pending.length; i++) { store[i] = pending[i]; pending[i] = 0; }
        return store[0] == 2 && store[1] == 3 && pending[0] == 0 && pending[1] == 0;
    }
}
