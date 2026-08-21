// SPDX-License-Identifier: MIT
pragma solidity ^0.8.30;

contract Prototype {
    struct ServiceProfile {
        string name;
        string[] features;
    }

    function cloneAndCustomize()
        external
        pure
        returns (string memory originalName, uint256 originalFeatures, string memory cloneName, uint256 cloneFeatures)
    {
        ServiceProfile memory original;
        original.name = "orders";
        original.features = new string[](1);
        original.features[0] = "metrics";

        ServiceProfile memory clone;
        clone.name = original.name;
        clone.features = new string[](2);
        clone.features[0] = original.features[0];
        clone.name = "orders-canary";
        clone.features[1] = "tracing";

        return (original.name, original.features.length, clone.name, clone.features.length);
    }
}
