enum StateExample {
    enum Gate { case locked, unlocked }

    static func transition(_ state: Gate, _ action: String) -> Gate {
        if state == .locked && action == "unlock" { return .unlocked }
        if state == .unlocked && action == "lock" { return .locked }
        return state
    }

    static func run() -> Bool {
        transition(transition(.locked, "unlock"), "lock") == .locked
    }
}
