LOCKED = 0
UNLOCKED = 1


def transition(state, event):
    if state == LOCKED:
        return UNLOCKED if event == "coin" else LOCKED
    if state == UNLOCKED:
        return LOCKED if event == "push" else UNLOCKED
    raise ValueError("unknown gate state")


def verify_state_canonical():
    state = LOCKED
    assert state == LOCKED

    state = transition(state, "push")
    assert state == LOCKED

    state = transition(state, "coin")
    assert state == UNLOCKED

    state = transition(state, "coin")
    assert state == UNLOCKED

    state = transition(state, "push")
    assert state == LOCKED

    try:
        transition(99, "coin")
        raise AssertionError("unknown state must fail")
    except ValueError:
        pass

    print("MicroPython State: passed")


verify_state_canonical()
