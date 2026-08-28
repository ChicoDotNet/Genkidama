final class Handler {
    let name: String
    let limit: Int
    var next: Handler?

    init(_ name: String, _ limit: Int) {
        self.name = name
        self.limit = limit
    }

    @discardableResult
    func then(_ handler: Handler) -> Handler {
        next = handler
        return handler
    }

    func handle(_ amount: Int, visited: inout [String]) -> String {
        visited.append(name)
        if amount <= limit || next == nil {
            return name
        }
        return next!.handle(amount, visited: &visited)
    }
}

let faq = Handler("faq", 50)
let billing = Handler("billing", 500)
let escalation = Handler("escalation", Int.max)
faq.then(billing).then(escalation)

var visited: [String] = []
let handled = faq.handle(250, visited: &visited)
print("visited=\(visited.joined(separator: ">"));handled=\(handled);result=refund(250)")
