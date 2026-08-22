// SPDX-License-Identifier: MIT
pragma solidity ^0.8.30;

contract Singleton {
    uint256 private count;

    function instance() external view returns (address) {
        return address(this);
    }

    function increment() external {
        count += 1;
    }

    function currentCount() external view returns (uint256) {
        return count;
    }
}
