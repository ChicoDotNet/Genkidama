// SPDX-License-Identifier: MIT
pragma solidity >=0.8.0 <0.9.0;

library ActiveRecordPattern {
    struct Record { uint256 id; uint256 nameId; }
    struct Row { uint256 key; uint256 nameId; }
    function save(Record memory record) private pure returns (Row memory) { return Row(record.id, record.nameId); }
    function run() internal pure returns (bool) {
        Record memory record = Record(7, 1);
        Row memory row = save(record);
        return row.key == 7 && row.nameId == 1;
    }
}
