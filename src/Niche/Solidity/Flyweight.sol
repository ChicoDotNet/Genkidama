// SPDX-License-Identifier: MIT
pragma solidity ^0.8.24;

contract FlyweightPool {
    mapping(bytes32 => uint256) private styleIds;
    uint256 public styleCount;

    function getStyle(string memory font, uint256 size, string memory color)
        public
        returns (uint256 id)
    {
        bytes32 key = keccak256(abi.encode(font, size, color));
        id = styleIds[key];
        if (id == 0) {
            styleCount += 1;
            id = styleCount;
            styleIds[key] = id;
        }
    }

    function probe()
        external
        returns (uint256 styles, bool shared, string memory text)
    {
        uint256 red1 = getStyle("Inter", 12, "red");
        uint256 red2 = getStyle("Inter", 12, "red");
        uint256 blue = getStyle("Inter", 12, "blue");
        require(blue != red1, "distinct intrinsic state must not alias");
        return (styleCount, red1 == red2, "ABC");
    }
}
