// SPDX-License-Identifier: MIT
pragma solidity >=0.8.0 <0.9.0;

library MementoPattern {
    enum State { Draft, Published }
    function run() internal pure returns (bool) {
        State state = State.Draft;
        State snapshot = state;
        state = State.Published;
        bool changed = state == State.Published;
        state = snapshot;
        return changed && state == State.Draft;
    }
}
