@MainActor
public final class TimeQuoteApplication<Repository: TimeQuoteRepository> {
    public private(set) var state: TimeQuoteViewState = .idle

    private var service: TimeQuoteService<Repository>

    public init(repository: Repository) throws {
        self.service = try TimeQuoteService(repository: repository)
    }

    public func refresh() async {
        state = .loading
        await Task.yield()
        state = .loaded(service.allSummaries())
    }

    public func addClient(_ client: Client) async {
        state = .loading
        await Task.yield()

        do {
            try service.addClient(client)
            state = .loaded(service.allSummaries())
        } catch {
            state = .failed(String(describing: error))
        }
    }

    public func record(_ entry: TimeEntry) async {
        state = .loading
        await Task.yield()

        do {
            try service.record(entry)
            state = .loaded(service.allSummaries())
        } catch {
            state = .failed(String(describing: error))
        }
    }
}

public enum TimeQuoteViewState: Equatable, Sendable {
    case idle
    case loading
    case loaded([ClientSummary])
    case failed(String)
}
