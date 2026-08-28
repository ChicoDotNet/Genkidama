enum NullObjectExample {
    protocol Logger {
        func log(_ message: String) -> String
    }

    struct NullLogger: Logger {
        func log(_ message: String) -> String { "" }
    }

    struct RealLogger: Logger {
        func log(_ message: String) -> String { "log:\(message)" }
    }

    static func run() -> Bool {
        NullLogger().log("x").isEmpty && RealLogger().log("x") == "log:x"
    }
}
