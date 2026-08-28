enum DocumentViewExample {
    static func run() -> Bool {
        let document = (title: "Final", words: 120)
        let editor = { "editor:\(document.title):\(document.words)" }
        let summary = { "summary:\(document.title)" }
        return editor() == "editor:Final:120" && summary() == "summary:Final"
    }
}
