// SPDX-License-Identifier: MIT
pragma solidity >=0.8.0 <0.9.0;

library BrokerPattern {
    enum Service { Inventory, Customer }
    function route(Service service, uint256 key) private pure returns (uint256) {
        if (service == Service.Inventory && key == 1) return 7;
        if (service == Service.Customer && key == 17) return 1;
        return 0;
    }
    function run() internal pure returns (bool) {
        return route(Service.Inventory, 1) == 7 && route(Service.Customer, 17) == 1;
    }
}
