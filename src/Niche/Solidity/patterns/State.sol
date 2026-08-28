// SPDX-License-Identifier: MIT
pragma solidity >=0.8.0 <0.9.0;

library StatePattern {
    enum Gate { Locked, Unlocked }
    enum Action { Lock, Unlock }
    function transition(Gate state, Action action) private pure returns (Gate) {
        if (state == Gate.Locked && action == Action.Unlock) return Gate.Unlocked;
        if (state == Gate.Unlocked && action == Action.Lock) return Gate.Locked;
        return state;
    }
    function run() internal pure returns (bool) {
        Gate state = transition(Gate.Locked, Action.Unlock);
        state = transition(state, Action.Lock);
        return state == Gate.Locked;
    }
}
