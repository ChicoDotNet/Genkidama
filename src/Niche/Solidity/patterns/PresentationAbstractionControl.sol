// SPDX-License-Identifier: MIT
pragma solidity >=0.8.0 <0.9.0;

library PresentationAbstractionControlPattern {
    struct Agent { uint256 abstraction; uint256 control; }
    function present(Agent memory agent) private pure returns (uint256) { return agent.abstraction + agent.control; }
    function run() internal pure returns (bool) {
        Agent memory child = Agent(40, 2);
        Agent memory root = Agent(present(child), 0);
        return present(child) == 42 && present(root) == 42;
    }
}
