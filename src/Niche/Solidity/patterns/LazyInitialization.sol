// SPDX-License-Identifier: MIT
pragma solidity >=0.8.0 <0.9.0;

library LazyInitializationPattern {
    struct LazyValue { bool ready; uint256 builds; uint256 value; }
    function get(LazyValue memory state) private pure returns (LazyValue memory) {
        if (!state.ready) { state.ready = true; state.builds++; state.value = 42; }
        return state;
    }
    function run() internal pure returns (bool) {
        LazyValue memory state;
        state = get(state);
        state = get(state);
        return state.ready && state.value == 42 && state.builds == 1;
    }
}
