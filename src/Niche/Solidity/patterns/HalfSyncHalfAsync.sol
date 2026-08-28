// SPDX-License-Identifier: MIT
pragma solidity >=0.8.0 <0.9.0;

library HalfSyncHalfAsyncPattern {
    function syncHandle(uint256 job) private pure returns (uint256) { return 100 + job; }
    function run() internal pure returns (bool) {
        uint256[3] memory asyncQueue = [uint256(1), 2, 3];
        uint256[3] memory completed;
        for (uint256 i; i < asyncQueue.length; i++) completed[i] = syncHandle(asyncQueue[i]);
        return completed[0] == 101 && completed[1] == 102 && completed[2] == 103;
    }
}
