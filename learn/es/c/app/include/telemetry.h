#ifndef GENKIDAMA_TELEMETRY_H
#define GENKIDAMA_TELEMETRY_H

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

/** A single portable telemetry sample. */
typedef struct telemetry_record {
    int64_t timestamp_ms;
    uint32_t sensor_id;
    int32_t value_milli;
    uint8_t status;
} telemetry_record;

/** Streaming summary of a telemetry file. */
typedef struct telemetry_summary {
    size_t record_count;
    int64_t first_timestamp_ms;
    int64_t last_timestamp_ms;
    int32_t min_value_milli;
    int32_t max_value_milli;
    double average_value_milli;
} telemetry_summary;

/** Optional predicates for a streaming query. Time bounds are [start, end). */
typedef struct telemetry_filter {
    bool has_sensor;
    uint32_t sensor_id;
    bool has_start_timestamp;
    int64_t start_timestamp_ms;
    bool has_end_timestamp;
    int64_t end_timestamp_ms;
} telemetry_filter;

/** Explicit result codes returned by telemetry file operations. */
typedef enum telemetry_result {
    TELEMETRY_OK = 0,
    TELEMETRY_INVALID_ARGUMENT,
    TELEMETRY_IO_ERROR,
    TELEMETRY_INVALID_FORMAT,
    TELEMETRY_UNSUPPORTED_VERSION,
    TELEMETRY_TRUNCATED_RECORD,
    TELEMETRY_CAPACITY_EXCEEDED
} telemetry_result;

/** Read-only health snapshot. stream_result describes the first content defect, if any. */
typedef struct telemetry_diagnostics {
    size_t record_count;
    size_t valid_prefix_bytes;
    int64_t first_timestamp_ms;
    int64_t last_timestamp_ms;
    telemetry_result stream_result;
} telemetry_diagnostics;

/** Visitor invoked for each matching record; returning non-OK stops the query. */
typedef telemetry_result (*telemetry_record_visitor)(const telemetry_record *record, void *context);

/** Creates or truncates a telemetry file and writes its versioned header. */
telemetry_result telemetry_create_file(const char *path);

/** Appends one validated record only when the existing stream is fully valid. */
telemetry_result telemetry_append_record(const char *path, const telemetry_record *record);

/** Counts complete records without allocating a record array. */
telemetry_result telemetry_count_records(const char *path, size_t *out_count);

/** Reads complete records into caller-owned storage without exceeding capacity. */
telemetry_result telemetry_read_records(const char *path, telemetry_record *records, size_t capacity, size_t *out_count);

/** Computes count, timestamp bounds, min/max and average while streaming the file. */
telemetry_result telemetry_analyze_file(const char *path, telemetry_summary *out_summary);

/** Streams matching records in original order. A NULL filter means all records. */
telemetry_result telemetry_query_file(const char *path, const telemetry_filter *filter, telemetry_record_visitor visitor, void *context, size_t *out_count);

/** Exports matching records to deterministic CSV after validating the source stream. */
telemetry_result telemetry_export_csv(const char *path, const char *csv_path, const telemetry_filter *filter, size_t *out_count);

/**
 * Inspects a stream without modifying it.
 * @return TELEMETRY_OK when inspection itself succeeded; content health is reported in out_diagnostics->stream_result.
 */
telemetry_result telemetry_diagnose_file(const char *path, telemetry_diagnostics *out_diagnostics);

/**
 * Copies the valid prefix of a stream to a different destination file.
 * The source is never modified. Invalid magic/version prevents destination creation; a corrupt/truncated suffix is discarded explicitly.
 */
telemetry_result telemetry_recover_valid_prefix(const char *source_path, const char *destination_path, size_t *out_recovered_count);

/** Returns a stable diagnostic name for a result code. */
const char *telemetry_result_name(telemetry_result result);

#endif
