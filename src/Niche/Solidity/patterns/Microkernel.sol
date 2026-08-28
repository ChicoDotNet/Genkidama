// SPDX-License-Identifier: MIT
pragma solidity >=0.8.0 <0.9.0;

library MicrokernelPattern {
    enum Plugin { Double, Square }
    function invoke(Plugin plugin, uint256 value) private pure returns (uint256) {
        return plugin == Plugin.Double ? value * 2 : value * value;
    }
    function run() internal pure returns (bool) {
        Plugin[2] memory registry = [Plugin.Double, Plugin.Square];
        return invoke(registry[0], 4) == 8 && invoke(registry[1], 4) == 16;
    }
}
