enum TemplateMethodExample {
    static func run() -> Bool {
        let pipeline: (String, () -> String) -> String = { read, transform in
            "\(read)>\(transform())>publish"
        }
        return pipeline("read-csv", { "normalize" }) == "read-csv>normalize>publish"
    }
}
