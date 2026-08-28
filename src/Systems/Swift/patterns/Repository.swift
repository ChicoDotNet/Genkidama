enum RepositoryExample {
    static func run() -> Bool {
        let rows = [(1, "Ada"), (2, "Grace")]
        return rows.first { $0.0 == 2 }?.1 == "Grace"
    }
}
