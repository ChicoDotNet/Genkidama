// SPDX-License-Identifier: MIT
pragma solidity >=0.8.0 <0.9.0;

library ObjectPoolPattern {
    struct Pool { uint256[2] values; uint256 size; }
    function borrow(Pool memory pool) private pure returns (uint256 item, Pool memory changed) {
        changed = pool;
        item = changed.values[--changed.size];
    }
    function release(Pool memory pool, uint256 item) private pure returns (Pool memory) { pool.values[pool.size++] = item; return pool; }
    function run() internal pure returns (bool) {
        Pool memory pool;
        pool.values = [uint256(1), 2];
        pool.size = 2;
        uint256 borrowed;
        (borrowed, pool) = borrow(pool);
        pool = release(pool, borrowed);
        return borrowed == 2 && pool.size == 2 && pool.values[1] == 2;
    }
}
