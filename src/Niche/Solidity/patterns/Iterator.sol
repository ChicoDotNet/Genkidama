// SPDX-License-Identifier: MIT
pragma solidity >=0.8.0 <0.9.0;

library IteratorPattern {
    struct Iterator { uint256[3] values; uint256 cursor; }
    function next(Iterator memory iterator) private pure returns (uint256 value, Iterator memory advanced) {
        advanced = iterator;
        value = advanced.values[advanced.cursor++];
    }
    function run() internal pure returns (bool) {
        Iterator memory iterator;
        iterator.values = [uint256(10), 20, 30];
        uint256 sum;
        for (uint256 i; i < 3; i++) {
            uint256 value;
            (value, iterator) = next(iterator);
            sum += value;
        }
        return iterator.cursor == 3 && sum == 60;
    }
}
