package io.genkidama.learn.java.helpdesk.http;

import java.util.concurrent.atomic.LongAdder;

/** Thread-safe aggregate request counters that never retain request payloads or identifiers. */
public final class RequestMetrics {
    private final LongAdder requests = new LongAdder();
    private final LongAdder failures = new LongAdder();

    /** Records one completed HTTP response. */
    public void record(int statusCode) {
        requests.increment();
        if (statusCode >= 500) {
            failures.increment();
        }
    }

    /** Returns an immutable point-in-time snapshot. */
    public RequestMetricsSnapshot snapshot() {
        return new RequestMetricsSnapshot(requests.sum(), failures.sum());
    }
}
