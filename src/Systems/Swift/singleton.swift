final class ProcessRegistry {
    static let shared = ProcessRegistry()
    private(set) var count = 0

    private init() {}

    func increment() {
        count += 1
    }
}

let first = ProcessRegistry.shared
let second = ProcessRegistry.shared
first.increment()
print("same=\(first === second)")
print("count=\(second.count)")
