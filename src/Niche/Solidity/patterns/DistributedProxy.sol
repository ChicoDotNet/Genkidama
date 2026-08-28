// SPDX-License-Identifier: MIT
pragma solidity >=0.8.0 <0.9.0;

library DistributedProxyPattern {
    function remote(uint256 sku) private pure returns (uint256) { return sku == 1 ? 7 : 0; }
    function proxy(uint256 sku) private pure returns (uint256) { return remote(sku); }
    function run() internal pure returns (bool) { return proxy(1) == 7 && proxy(999) == 0; }
}
