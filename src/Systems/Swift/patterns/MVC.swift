enum MVCExample {
    static func run() -> Bool {
        var count = 0
        let render = { "count=\(count)" }
        let before = render()
        count += 1
        return before == "count=0" && render() == "count=1"
    }
}
