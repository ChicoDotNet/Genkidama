enum CommandExample {
    static func run() -> Bool {
        let commands: [(Int) -> Int] = [{ $0 + 50 }, { $0 - 20 }]
        let balance = commands.reduce(100) { value, command in command(value) }
        return balance == 130 && commands[1](150) == 130
    }
}
