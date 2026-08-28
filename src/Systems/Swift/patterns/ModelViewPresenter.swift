enum ModelViewPresenterExample {
    static func run() -> Bool {
        var count = 0
        var text = ""
        let present = {
            count += 1
            text = "count=\(count)"
        }
        present()
        return count == 1 && text == "count=1"
    }
}
