// SPDX-License-Identifier: MIT
pragma solidity >=0.8.0 <0.9.0;

library DependencyInjectionPattern {
    function clock() private pure returns (uint256) { return 1000; }
    function service(uint256 injectedTime) private pure returns (uint256) { return 5000 + injectedTime; }
    function run() internal pure returns (bool) { return service(clock()) == 6000 && service(2000) == 7000; }
}
