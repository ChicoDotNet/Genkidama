enum MementoExample {
    static func run() -> Bool {
        var state = "draft"
        let snapshot = state
        state = "published"
        guard state == "published" else { return false }
        state = snapshot
        return state == "draft"
    }
}
