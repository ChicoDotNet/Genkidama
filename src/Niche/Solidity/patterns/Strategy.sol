// SPDX-License-Identifier: MIT
pragma solidity >=0.8.0 <0.9.0;

library StrategyPattern {
    enum Pricing { Regular, Discount20 }
    function price(uint256 amount, Pricing strategy) private pure returns (uint256) {
        return strategy == Pricing.Regular ? amount : amount * 80 / 100;
    }
    function run() internal pure returns (bool) {
        return price(100, Pricing.Regular) == 100 && price(100, Pricing.Discount20) == 80;
    }
}
