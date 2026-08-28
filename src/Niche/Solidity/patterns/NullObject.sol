// SPDX-License-Identifier: MIT
pragma solidity >=0.8.0 <0.9.0;

library NullObjectPattern {
    enum Logger { Null, Real }
    function log(Logger logger, uint256 messageId) private pure returns (uint256) { return logger == Logger.Null ? 0 : 1000 + messageId; }
    function run() internal pure returns (bool) { return log(Logger.Null, 7) == 0 && log(Logger.Real, 7) == 1007; }
}
