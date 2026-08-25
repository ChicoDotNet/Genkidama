#include "telemetry.h"

#include <stdbool.h>
#include <stdio.h>
#include <string.h>

static const unsigned char FILE_MAGIC[3] = {'G', 'T', 'L'};
static const unsigned char FILE_VERSION = 1;
static const size_t HEADER_SIZE = 4;
static const size_t RECORD_SIZE = 17;

static void write_u32_le(unsigned char *dst, uint32_t value) {
    for (size_t i = 0; i < 4; ++i) dst[i] = (unsigned char)((value >> (8u * i)) & 0xffu);
}
static void write_u64_le(unsigned char *dst, uint64_t value) {
    for (size_t i = 0; i < 8; ++i) dst[i] = (unsigned char)((value >> (8u * i)) & 0xffu);
}
static uint32_t read_u32_le(const unsigned char *src) {
    uint32_t value = 0; for (size_t i = 0; i < 4; ++i) value |= ((uint32_t)src[i]) << (8u * i); return value;
}
static uint64_t read_u64_le(const unsigned char *src) {
    uint64_t value = 0; for (size_t i = 0; i < 8; ++i) value |= ((uint64_t)src[i]) << (8u * i); return value;
}
static telemetry_result validate_header(FILE *file) {
    unsigned char header[4];
    if (fread(header, 1, sizeof header, file) != sizeof header) return TELEMETRY_INVALID_FORMAT;
    if (memcmp(header, FILE_MAGIC, sizeof FILE_MAGIC) != 0) return TELEMETRY_INVALID_FORMAT;
    return header[3] == FILE_VERSION ? TELEMETRY_OK : TELEMETRY_UNSUPPORTED_VERSION;
}
static telemetry_result encode_record(const telemetry_record *record, unsigned char *bytes) {
    if (record == NULL || bytes == NULL || record->timestamp_ms < 0 || record->sensor_id == 0) return TELEMETRY_INVALID_ARGUMENT;
    write_u64_le(bytes, (uint64_t)record->timestamp_ms); write_u32_le(bytes + 8, record->sensor_id);
    write_u32_le(bytes + 12, (uint32_t)record->value_milli); bytes[16] = record->status; return TELEMETRY_OK;
}
static telemetry_result decode_record(const unsigned char *bytes, telemetry_record *record) {
    record->timestamp_ms = (int64_t)read_u64_le(bytes); record->sensor_id = read_u32_le(bytes + 8);
    record->value_milli = (int32_t)read_u32_le(bytes + 12); record->status = bytes[16];
    return record->timestamp_ms < 0 || record->sensor_id == 0 ? TELEMETRY_INVALID_FORMAT : TELEMETRY_OK;
}
static telemetry_result read_next_record(FILE *file, telemetry_record *record, bool *out_eof) {
    unsigned char bytes[17]; *out_eof = false; const size_t read_count = fread(bytes, 1, RECORD_SIZE, file);
    if (read_count == 0) { if (ferror(file)) return TELEMETRY_IO_ERROR; *out_eof = true; return TELEMETRY_OK; }
    if (read_count != RECORD_SIZE) return TELEMETRY_TRUNCATED_RECORD;
    return decode_record(bytes, record);
}
static bool filter_is_valid(const telemetry_filter *filter) {
    if (filter == NULL) return true;
    if (filter->has_sensor && filter->sensor_id == 0) return false;
    if (filter->has_start_timestamp && filter->start_timestamp_ms < 0) return false;
    if (filter->has_end_timestamp && filter->end_timestamp_ms < 0) return false;
    return !(filter->has_start_timestamp && filter->has_end_timestamp && filter->start_timestamp_ms >= filter->end_timestamp_ms);
}
static bool record_matches(const telemetry_record *record, const telemetry_filter *filter) {
    if (filter == NULL) return true;
    if (filter->has_sensor && record->sensor_id != filter->sensor_id) return false;
    if (filter->has_start_timestamp && record->timestamp_ms < filter->start_timestamp_ms) return false;
    if (filter->has_end_timestamp && record->timestamp_ms >= filter->end_timestamp_ms) return false;
    return true;
}
telemetry_result telemetry_create_file(const char *path) {
    if (path == NULL || path[0] == '\0') return TELEMETRY_INVALID_ARGUMENT;
    FILE *file = fopen(path, "wb");
    if (file == NULL) return TELEMETRY_IO_ERROR;
    const unsigned char header[4] = {FILE_MAGIC[0], FILE_MAGIC[1], FILE_MAGIC[2], FILE_VERSION};
    const size_t written = fwrite(header, 1, sizeof header, file); const int close_result = fclose(file);
    return written == sizeof header && close_result == 0 ? TELEMETRY_OK : TELEMETRY_IO_ERROR;
}
telemetry_result telemetry_count_records(const char *path, size_t *out_count) {
    if (path == NULL || out_count == NULL) return TELEMETRY_INVALID_ARGUMENT;
    *out_count = 0;
    FILE *file = fopen(path, "rb");
    if (file == NULL) return TELEMETRY_IO_ERROR;
    telemetry_result result = validate_header(file); while (result == TELEMETRY_OK) { telemetry_record record; bool eof = false; result = read_next_record(file, &record, &eof); if (result != TELEMETRY_OK || eof) break; if (*out_count == SIZE_MAX) { result = TELEMETRY_CAPACITY_EXCEEDED; break; } ++(*out_count); }
    if (fclose(file) != 0 && result == TELEMETRY_OK) result = TELEMETRY_IO_ERROR;
    return result;
}
telemetry_result telemetry_append_record(const char *path, const telemetry_record *record) {
    unsigned char bytes[17]; telemetry_result result = encode_record(record, bytes); if (path == NULL || path[0] == '\0' || result != TELEMETRY_OK) return TELEMETRY_INVALID_ARGUMENT;
    size_t existing_count = 0; result = telemetry_count_records(path, &existing_count); (void)existing_count; if (result != TELEMETRY_OK) return result;
    FILE *file = fopen(path, "ab"); if (file == NULL) return TELEMETRY_IO_ERROR; const size_t written = fwrite(bytes, 1, RECORD_SIZE, file); const int close_result = fclose(file);
    return written == RECORD_SIZE && close_result == 0 ? TELEMETRY_OK : TELEMETRY_IO_ERROR;
}
telemetry_result telemetry_read_records(const char *path, telemetry_record *records, size_t capacity, size_t *out_count) {
    if (path == NULL || out_count == NULL || (capacity > 0 && records == NULL)) return TELEMETRY_INVALID_ARGUMENT;
    *out_count = 0;
    FILE *file = fopen(path, "rb");
    if (file == NULL) return TELEMETRY_IO_ERROR;
    telemetry_result result = validate_header(file); while (result == TELEMETRY_OK) { telemetry_record record; bool eof = false; result = read_next_record(file, &record, &eof); if (result != TELEMETRY_OK || eof) break; if (*out_count >= capacity) { result = TELEMETRY_CAPACITY_EXCEEDED; break; } records[*out_count] = record; ++(*out_count); }
    if (fclose(file) != 0 && result == TELEMETRY_OK) result = TELEMETRY_IO_ERROR;
    return result;
}
telemetry_result telemetry_analyze_file(const char *path, telemetry_summary *out_summary) {
    if (path == NULL || out_summary == NULL) return TELEMETRY_INVALID_ARGUMENT;
    *out_summary = (telemetry_summary){0};
    FILE *file = fopen(path, "rb");
    if (file == NULL) return TELEMETRY_IO_ERROR;
    telemetry_result result = validate_header(file); while (result == TELEMETRY_OK) { telemetry_record record; bool eof = false; result = read_next_record(file, &record, &eof); if (result != TELEMETRY_OK || eof) break; if (out_summary->record_count == SIZE_MAX) { result = TELEMETRY_CAPACITY_EXCEEDED; break; }
        const size_t next_count = out_summary->record_count + 1; if (out_summary->record_count == 0) { out_summary->first_timestamp_ms = record.timestamp_ms; out_summary->min_value_milli = record.value_milli; out_summary->max_value_milli = record.value_milli; out_summary->average_value_milli = (double)record.value_milli; }
        else { if (record.value_milli < out_summary->min_value_milli) out_summary->min_value_milli = record.value_milli; if (record.value_milli > out_summary->max_value_milli) out_summary->max_value_milli = record.value_milli; out_summary->average_value_milli += ((double)record.value_milli - out_summary->average_value_milli) / (double)next_count; }
        out_summary->last_timestamp_ms = record.timestamp_ms; out_summary->record_count = next_count; }
    if (fclose(file) != 0 && result == TELEMETRY_OK) result = TELEMETRY_IO_ERROR;
    return result;
}
telemetry_result telemetry_query_file(const char *path, const telemetry_filter *filter, telemetry_record_visitor visitor, void *context, size_t *out_count) {
    if (path == NULL || visitor == NULL || out_count == NULL || !filter_is_valid(filter)) return TELEMETRY_INVALID_ARGUMENT;
    *out_count = 0;
    FILE *file = fopen(path, "rb");
    if (file == NULL) return TELEMETRY_IO_ERROR;
    telemetry_result result = validate_header(file); while (result == TELEMETRY_OK) { telemetry_record record; bool eof = false; result = read_next_record(file, &record, &eof); if (result != TELEMETRY_OK || eof) break; if (!record_matches(&record, filter)) continue; if (*out_count == SIZE_MAX) { result = TELEMETRY_CAPACITY_EXCEEDED; break; } result = visitor(&record, context); if (result != TELEMETRY_OK) break; ++(*out_count); }
    if (fclose(file) != 0 && result == TELEMETRY_OK) result = TELEMETRY_IO_ERROR;
    return result;
}
typedef struct csv_context { FILE *file; } csv_context;
static telemetry_result write_csv_record(const telemetry_record *record, void *context) { csv_context *csv = context; const int written = fprintf(csv->file, "%lld,%lu,%ld,%u\n", (long long)record->timestamp_ms, (unsigned long)record->sensor_id, (long)record->value_milli, (unsigned)record->status); return written < 0 ? TELEMETRY_IO_ERROR : TELEMETRY_OK; }
telemetry_result telemetry_export_csv(const char *path, const char *csv_path, const telemetry_filter *filter, size_t *out_count) {
    if (path == NULL || csv_path == NULL || csv_path[0] == '\0' || out_count == NULL || !filter_is_valid(filter)) return TELEMETRY_INVALID_ARGUMENT;
    size_t validated_count = 0; telemetry_result result = telemetry_count_records(path, &validated_count); (void)validated_count; if (result != TELEMETRY_OK) return result;
    FILE *file = fopen(csv_path, "wb"); if (file == NULL) return TELEMETRY_IO_ERROR; if (fputs("timestamp_ms,sensor_id,value_milli,status\n", file) == EOF) { fclose(file); return TELEMETRY_IO_ERROR; }
    csv_context context = {file}; result = telemetry_query_file(path, filter, write_csv_record, &context, out_count); if (fclose(file) != 0 && result == TELEMETRY_OK) result = TELEMETRY_IO_ERROR;
    return result;
}
telemetry_result telemetry_diagnose_file(const char *path, telemetry_diagnostics *out_diagnostics) {
    if (path == NULL || out_diagnostics == NULL) return TELEMETRY_INVALID_ARGUMENT;
    *out_diagnostics = (telemetry_diagnostics){.stream_result = TELEMETRY_OK};
    FILE *file = fopen(path, "rb"); if (file == NULL) return TELEMETRY_IO_ERROR; telemetry_result result = validate_header(file);
    if (result != TELEMETRY_OK) { out_diagnostics->stream_result = result; fclose(file); return TELEMETRY_OK; }
    out_diagnostics->valid_prefix_bytes = HEADER_SIZE;
    while (true) { telemetry_record record; bool eof = false; result = read_next_record(file, &record, &eof); if (result != TELEMETRY_OK) { out_diagnostics->stream_result = result; break; } if (eof) break;
        if (out_diagnostics->record_count == SIZE_MAX || out_diagnostics->valid_prefix_bytes > SIZE_MAX - RECORD_SIZE) { fclose(file); return TELEMETRY_CAPACITY_EXCEEDED; }
        if (out_diagnostics->record_count == 0) out_diagnostics->first_timestamp_ms = record.timestamp_ms;
        out_diagnostics->last_timestamp_ms = record.timestamp_ms;
        ++out_diagnostics->record_count; out_diagnostics->valid_prefix_bytes += RECORD_SIZE; }
    if (fclose(file) != 0) return TELEMETRY_IO_ERROR;
    return TELEMETRY_OK;
}
telemetry_result telemetry_recover_valid_prefix(const char *source_path, const char *destination_path, size_t *out_recovered_count) {
    if (source_path == NULL || destination_path == NULL || out_recovered_count == NULL || source_path[0] == '\0' || destination_path[0] == '\0' || strcmp(source_path, destination_path) == 0) return TELEMETRY_INVALID_ARGUMENT;
    *out_recovered_count = 0; FILE *source = fopen(source_path, "rb"); if (source == NULL) return TELEMETRY_IO_ERROR; telemetry_result result = validate_header(source); if (result != TELEMETRY_OK) { fclose(source); return result; }
    FILE *destination = fopen(destination_path, "wb"); if (destination == NULL) { fclose(source); return TELEMETRY_IO_ERROR; } const unsigned char header[4] = {FILE_MAGIC[0], FILE_MAGIC[1], FILE_MAGIC[2], FILE_VERSION};
    if (fwrite(header, 1, sizeof header, destination) != sizeof header) result = TELEMETRY_IO_ERROR;
    while (result == TELEMETRY_OK) { telemetry_record record; bool eof = false; telemetry_result read_result = read_next_record(source, &record, &eof); if (read_result == TELEMETRY_TRUNCATED_RECORD || read_result == TELEMETRY_INVALID_FORMAT) break; if (read_result != TELEMETRY_OK) { result = read_result; break; } if (eof) break;
        unsigned char bytes[17]; if (encode_record(&record, bytes) != TELEMETRY_OK || fwrite(bytes, 1, RECORD_SIZE, destination) != RECORD_SIZE) { result = TELEMETRY_IO_ERROR; break; } if (*out_recovered_count == SIZE_MAX) { result = TELEMETRY_CAPACITY_EXCEEDED; break; } ++(*out_recovered_count); }
    if (fclose(source) != 0 && result == TELEMETRY_OK) result = TELEMETRY_IO_ERROR;
    if (fclose(destination) != 0 && result == TELEMETRY_OK) result = TELEMETRY_IO_ERROR;
    if (result != TELEMETRY_OK) remove(destination_path);
    return result;
}
const char *telemetry_result_name(telemetry_result result) {
    switch (result) { case TELEMETRY_OK: return "ok"; case TELEMETRY_INVALID_ARGUMENT: return "invalid_argument"; case TELEMETRY_IO_ERROR: return "io_error"; case TELEMETRY_INVALID_FORMAT: return "invalid_format"; case TELEMETRY_UNSUPPORTED_VERSION: return "unsupported_version"; case TELEMETRY_TRUNCATED_RECORD: return "truncated_record"; case TELEMETRY_CAPACITY_EXCEEDED: return "capacity_exceeded"; default: return "unknown"; }
}
