package io.genkidama.learn.java.helpdesk.http;

import java.util.concurrent.atomic.LongAdder;

/** Thread-safe aggregate request counters that never retain request payloads or identifiers. */
public final class RequestMetrics {
    private final LongAdder requests = new LongAdder();
    private final LongAdder failures = new LongAdder();
    private final LongAdder totalDurationNanos = new LongAdder();

    /** Records one completed HTTP response. */
    public void record(int statusCode) {
        requests.increment();
        if (statusCode >= 500) {
            failures.increment();
        }
    }

    /** Records aggregate request duration without retaining request-specific data. */
    public void recordDuration(long durationNanos) {
        if (durationNanos < 0) {
            throw new IllegalArgumentException("durationNanos cannot be negative");
        }
        totalDurationNanos.add(durationNanos);
    }

    /** Returns an immutable point-in-time snapshot. */
    public RequestMetricsSnapshot snapshot() {
        return new RequestMetricsSnapshot(requests.sum(), failures.sum(), totalDurationNanos.sum());
    }
}
