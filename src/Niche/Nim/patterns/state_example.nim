proc run*(): bool =
  proc transition(state, action: string): string =
    if state == "locked" and action == "unlock": "unlocked"
    elif state == "unlocked" and action == "lock": "locked"
    else: state
  transition(transition("locked", "unlock"), "lock") == "locked"
