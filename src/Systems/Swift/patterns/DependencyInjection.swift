enum DependencyInjectionExample {
    static func run() -> Bool {
        let service: (() -> String) -> String = { "at:\($0())" }
        return service { "10:00" } == "at:10:00"
    }
}
