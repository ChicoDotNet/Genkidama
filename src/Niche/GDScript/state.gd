extends SceneTree

enum GateState { LOCKED, UNLOCKED }
enum GateAction { COIN, PUSH }

func transition(current_state: GateState, action: GateAction) -> GateState:
    match [current_state, action]:
        [GateState.LOCKED, GateAction.COIN]:
            return GateState.UNLOCKED
        [GateState.UNLOCKED, GateAction.PUSH]:
            return GateState.LOCKED
        _:
            return current_state

func _initialize() -> void:
    var state := GateState.LOCKED
    assert(state == GateState.LOCKED, "gate must start locked")

    state = transition(state, GateAction.PUSH)
    assert(state == GateState.LOCKED, "push while locked must preserve state")

    state = transition(state, GateAction.COIN)
    assert(state == GateState.UNLOCKED, "coin while locked must unlock")

    state = transition(state, GateAction.COIN)
    assert(state == GateState.UNLOCKED, "duplicate coin must preserve unlocked state")

    state = transition(state, GateAction.PUSH)
    assert(state == GateState.LOCKED, "push while unlocked must lock")

    print("gdscript-state: passed")
    quit()
