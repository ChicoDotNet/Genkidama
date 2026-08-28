// SPDX-License-Identifier: MIT
pragma solidity >=0.8.0 <0.9.0;

library RepositoryPattern {
    struct Row { uint256 id; uint256 nameId; }
    function find(Row[2] memory rows, uint256 id) private pure returns (Row memory) {
        for (uint256 i; i < rows.length; i++) if (rows[i].id == id) return rows[i];
        return Row(0, 0);
    }
    function run() internal pure returns (bool) {
        Row[2] memory rows;
        rows[0] = Row(1, 10);
        rows[1] = Row(2, 20);
        Row memory row = find(rows, 2);
        return row.id == 2 && row.nameId == 20;
    }
}
