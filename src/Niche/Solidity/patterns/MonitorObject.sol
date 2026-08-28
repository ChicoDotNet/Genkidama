// SPDX-License-Identifier: MIT
pragma solidity >=0.8.0 <0.9.0;

library MonitorObjectPattern {
    struct Monitor { uint256 value; bool entered; }
    function add(Monitor memory monitor, uint256 amount) private pure returns (Monitor memory) {
        require(!monitor.entered);
        monitor.entered = true;
        monitor.value += amount;
        monitor.entered = false;
        return monitor;
    }
    function run() internal pure returns (bool) {
        Monitor memory monitor;
        monitor = add(monitor, 2);
        monitor = add(monitor, 3);
        return monitor.value == 5 && !monitor.entered;
    }
}
