enum HalfSyncHalfAsyncExample {
    static func run() -> Bool {
        ["job-1", "job-2", "job-3"]
            .map { "done:\($0)" }
            .joined(separator: ">") == "done:job-1>done:job-2>done:job-3"
    }
}
