// SPDX-License-Identifier: MIT
pragma solidity >=0.8.0 <0.9.0;

library MicroservicesPattern {
    function reserve(uint256 stock, uint256 quantity) private pure returns (bool ok, uint256 remaining) {
        if (quantity > stock) return (false, stock);
        return (true, stock - quantity);
    }
    function placeOrder(uint256 stock, uint256 quantity) private pure returns (bool, uint256) { return reserve(stock, quantity); }
    function run() internal pure returns (bool) {
        (bool confirmed, uint256 remaining) = placeOrder(7, 2);
        return confirmed && remaining == 5;
    }
}
