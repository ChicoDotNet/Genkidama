// SPDX-License-Identifier: MIT
pragma solidity ^0.8.30;

contract DecoratorExample {
    function plain() public pure returns (string memory) {
        return "alert";
    }

    function audit(string memory inner) public pure returns (string memory) {
        return string.concat("audit(", inner, ")");
    }

    function encrypt(string memory inner) public pure returns (string memory) {
        return string.concat("enc(", inner, ")");
    }

    function outputs() external pure returns (string memory, string memory, string memory, string memory) {
        string memory base = plain();
        return (base, audit(base), encrypt(base), audit(encrypt(base)));
    }
}
