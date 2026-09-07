enum GateState {
    LOCKED,
    UNLOCKED
}

final Map<GateState, Map<String, GateState>> transitions = [
    (GateState.LOCKED): [coin: GateState.UNLOCKED],
    (GateState.UNLOCKED): [push: GateState.LOCKED]
]

def transition = { GateState state, String action ->
    transitions[state][action] ?: state
}

def state = GateState.LOCKED
assert state == GateState.LOCKED

state = transition(state, 'push')
assert state == GateState.LOCKED

state = transition(state, 'coin')
assert state == GateState.UNLOCKED

state = transition(state, 'coin')
assert state == GateState.UNLOCKED

state = transition(state, 'push')
assert state == GateState.LOCKED

println 'groovy-state: passed'
