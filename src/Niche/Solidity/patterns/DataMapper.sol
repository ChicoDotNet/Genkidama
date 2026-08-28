// SPDX-License-Identifier: MIT
pragma solidity >=0.8.0 <0.9.0;

library DataMapperPattern {
    struct Person { uint256 id; uint256 nameId; }
    struct Row { uint256 key; uint256 nameId; }
    function toRow(Person memory person) private pure returns (Row memory) { return Row(1000 + person.id, person.nameId); }
    function fromRow(Row memory row) private pure returns (Person memory) { return Person(row.key - 1000, row.nameId); }
    function run() internal pure returns (bool) {
        Person memory person = Person(8, 2);
        Row memory row = toRow(person);
        Person memory restored = fromRow(row);
        return row.key == 1008 && restored.id == person.id && restored.nameId == person.nameId;
    }
}
