package io.genkidama.learn.java.helpdesk.http;

/**
 * Aggregate HTTP diagnostics without URLs, bodies, ticket IDs or other request-specific data.
 * @param requests completed responses observed by the server
 * @param failures responses with status 5xx
 * @param totalDurationNanos cumulative handling duration measured with a monotonic clock
 */
public record RequestMetricsSnapshot(long requests, long failures, long totalDurationNanos) { }
