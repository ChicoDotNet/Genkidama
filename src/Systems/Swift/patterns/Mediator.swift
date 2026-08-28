enum MediatorExample {
    static func run() -> Bool {
        var events: [String] = []
        func notify(_ sender: String, _ event: String) {
            if sender == "button" && event == "click" { events.append("panel.refresh") }
            if sender == "panel" && event == "loaded" { events.append("button.enable") }
        }
        notify("button", "click")
        notify("panel", "loaded")
        return events.joined(separator: ">") == "panel.refresh>button.enable"
    }
}
