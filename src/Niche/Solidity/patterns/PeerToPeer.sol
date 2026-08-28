// SPDX-License-Identifier: MIT
pragma solidity >=0.8.0 <0.9.0;

library PeerToPeerPattern {
    struct Message { uint256 from; uint256 to; uint256 blockId; }
    function send(uint256 from, uint256 to, uint256 blockId) private pure returns (Message memory) { return Message(from, to, blockId); }
    function run() internal pure returns (bool) {
        Message memory first = send(1, 2, 42);
        Message memory second = send(1, 3, 42);
        return first.to == 2 && second.to == 3 && first.blockId == second.blockId;
    }
}
