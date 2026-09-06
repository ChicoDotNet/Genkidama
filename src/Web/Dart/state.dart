enum GateState { locked, unlocked }

enum GateAction { insertCoin, push }

({GateState state, String result}) transition(
  GateState state,
  GateAction action,
) => switch ((state, action)) {
  (GateState.locked, GateAction.insertCoin) => (
    state: GateState.unlocked,
    result: 'unlocked',
  ),
  (GateState.unlocked, GateAction.push) => (
    state: GateState.locked,
    result: 'passed',
  ),
  (GateState.locked, GateAction.push) => (
    state: GateState.locked,
    result: 'blocked',
  ),
  (GateState.unlocked, GateAction.insertCoin) => (
    state: GateState.unlocked,
    result: 'already-unlocked',
  ),
};

void check(bool condition, String message) {
  if (!condition) throw StateError(message);
}

void main() {
  var state = GateState.locked;
  check(state == GateState.locked, 'turnstile must start locked');

  var outcome = transition(state, GateAction.push);
  check(outcome.state == GateState.locked, 'invalid push must preserve locked');
  check(outcome.result == 'blocked', 'locked push must be blocked');

  outcome = transition(outcome.state, GateAction.insertCoin);
  check(outcome.state == GateState.unlocked, 'coin must unlock turnstile');
  check(outcome.result == 'unlocked', 'unlock result must be observable');

  outcome = transition(outcome.state, GateAction.insertCoin);
  check(
    outcome.state == GateState.unlocked,
    'duplicate coin must preserve unlocked',
  );
  check(
    outcome.result == 'already-unlocked',
    'duplicate coin behavior must depend on current state',
  );

  outcome = transition(outcome.state, GateAction.push);
  check(outcome.state == GateState.locked, 'valid push must return to locked');
  check(outcome.result == 'passed', 'unlocked push must pass');

  print('dart-state: passed');
}
