// SPDX-License-Identifier: MIT
pragma solidity >=0.8.0 <0.9.0;

library LeaderFollowersPattern {
    function run() internal pure returns (bool) {
        uint256[3] memory workers = [uint256(1), 2, 3];
        uint256[3] memory events = [uint256(10), 20, 30];
        uint256[3] memory handledBy;
        for (uint256 i; i < events.length; i++) handledBy[i] = workers[i % workers.length];
        return handledBy[0] == 1 && handledBy[1] == 2 && handledBy[2] == 3 && workers[events.length % workers.length] == 1;
    }
}
