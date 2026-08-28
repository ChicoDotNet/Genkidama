enum LeaderFollowersExample {
    static func run() -> Bool {
        let workers = ["worker-1", "worker-2", "worker-3"]
        let events = ["a", "b", "c"]
        let handled = events.enumerated().map { "\(workers[$0.offset % workers.count]):\($0.element)" }
        return handled.joined(separator: ">") == "worker-1:a>worker-2:b>worker-3:c"
            && workers[events.count % workers.count] == "worker-1"
    }
}
