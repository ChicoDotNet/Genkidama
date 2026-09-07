enum GateState
  Locked
  Unlocked
end

enum GateAction
  Unlock
  Lock
end

def transition(state : GateState, action : GateAction) : GateState
  case {state, action}
  when {GateState::Locked, GateAction::Unlock}
    GateState::Unlocked
  when {GateState::Unlocked, GateAction::Lock}
    GateState::Locked
  else
    state
  end
end

def must(value : Bool)
  raise "state assertion failed" unless value
end

state = GateState::Locked
must(state == GateState::Locked)

state = transition(state, GateAction::Lock)
must(state == GateState::Locked)

state = transition(state, GateAction::Unlock)
must(state == GateState::Unlocked)

state = transition(state, GateAction::Unlock)
must(state == GateState::Unlocked)

state = transition(state, GateAction::Lock)
must(state == GateState::Locked)

puts "crystal-state: passed"
