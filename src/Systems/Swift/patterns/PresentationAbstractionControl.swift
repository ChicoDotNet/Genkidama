enum PresentationAbstractionControlExample {
    static func run() -> Bool {
        let view: (String, Int) -> String = { "\($0):view=\($1)" }
        return view("child", 42) == "child:view=42"
            && view("root", 42) == "root:view=42"
    }
}
