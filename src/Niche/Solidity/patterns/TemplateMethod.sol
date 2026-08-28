// SPDX-License-Identifier: MIT
pragma solidity >=0.8.0 <0.9.0;

library TemplateMethodPattern {
    enum Transform { Normalize, Validate }
    function pipeline(bytes32 source, Transform transform) private pure returns (bytes32, Transform, bytes32) {
        return (source, transform, bytes32("publish"));
    }
    function run() internal pure returns (bool) {
        (bytes32 read, Transform transform, bytes32 publish) = pipeline(bytes32("read-csv"), Transform.Normalize);
        return read == bytes32("read-csv") && transform == Transform.Normalize && publish == bytes32("publish");
    }
}
