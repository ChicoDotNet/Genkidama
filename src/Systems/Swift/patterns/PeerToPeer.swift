enum PeerToPeerExample {
    static func run() -> Bool {
        var inbox: [String] = []
        func send(_ from: String, _ to: String, _ data: String) {
            inbox.append("\(from)>\(to):\(data)")
        }
        send("peer-a", "peer-b", "block-42")
        send("peer-a", "peer-c", "block-42")
        return inbox.joined(separator: ">") == "peer-a>peer-b:block-42>peer-a>peer-c:block-42"
    }
}
