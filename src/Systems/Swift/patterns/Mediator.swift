enum MediatorExample {
    enum MediatorError: Error {
        case unknownColleague(String)
    }

    static func run() -> Bool {
        var events: [String] = []
        let colleagues: [String: (String, String) -> Void] = [
            "inventory": { sender, message in
                events.append("inventory<-\(sender):\(message)")
            },
            "payment": { sender, message in
                events.append("payment<-\(sender):\(message)")
            }
        ]

        func send(_ sender: String, to recipient: String, message: String) throws {
            guard let receiver = colleagues[recipient] else {
                throw MediatorError.unknownColleague(recipient)
            }
            receiver(sender, message)
        }

        func payment(_ message: String) throws {
            try send("payment", to: "inventory", message: message)
        }

        func inventory(_ message: String) throws {
            try send("inventory", to: "payment", message: message)
        }

        do {
            try payment("paid")
            try inventory("reserved")
        } catch {
            return false
        }

        var rejectedUnknown = false
        do {
            try send("payment", to: "shipping", message: "paid")
        } catch MediatorError.unknownColleague(let recipient) where recipient == "shipping" {
            rejectedUnknown = true
        } catch {
            return false
        }

        return events.joined(separator: ">") ==
            "inventory<-payment:paid>payment<-inventory:reserved" && rejectedUnknown
    }
}
