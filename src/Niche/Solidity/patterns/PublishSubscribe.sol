// SPDX-License-Identifier: MIT
pragma solidity >=0.8.0 <0.9.0;

library PublishSubscribePattern {
    function publish(uint256 eventId) private pure returns (uint256 warehouse, uint256 analytics) {
        return (100 + eventId, 200 + eventId);
    }
    function run() internal pure returns (bool) {
        (uint256 warehouse, uint256 analytics) = publish(51);
        return warehouse == 151 && analytics == 251;
    }
}
