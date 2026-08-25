// SPDX-License-Identifier: MIT
pragma solidity ^0.8.20;

library AuthServiceFacadeExample {
    function authenticate(string memory user) internal pure returns (string memory) {
        return string.concat("auth(", user, ")");
    }
}

library InventoryServiceFacadeExample {
    function reserve(string memory sku) internal pure returns (string memory) {
        return string.concat("reserve(", sku, ")");
    }
}

library BillingServiceFacadeExample {
    function charge(uint256 cents) internal pure returns (string memory) {
        return string.concat("charge(", _toString(cents), ")");
    }

    function _toString(uint256 value) private pure returns (string memory) {
        if (value == 0) return "0";
        uint256 digits;
        uint256 current = value;
        while (current != 0) {
            digits++;
            current /= 10;
        }
        bytes memory buffer = new bytes(digits);
        while (value != 0) {
            digits -= 1;
            buffer[digits] = bytes1(uint8(48 + value % 10));
            value /= 10;
        }
        return string(buffer);
    }
}

contract CheckoutFacade {
    function checkout(string memory user, string memory sku, uint256 cents)
        external
        pure
        returns (string memory)
    {
        return string.concat(
            "checkout=",
            AuthServiceFacadeExample.authenticate(user),
            ">",
            InventoryServiceFacadeExample.reserve(sku),
            ">",
            BillingServiceFacadeExample.charge(cents)
        );
    }
}
