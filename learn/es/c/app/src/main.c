#include "telemetry.h"

#include <errno.h>
#include <inttypes.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static void usage(const char *program) {
    fprintf(stderr, "Uso:\n  %s init ARCHIVO\n  %s log ARCHIVO TIMESTAMP_MS SENSOR_ID VALUE_MILLI STATUS\n  %s list ARCHIVO\n  %s summary ARCHIVO\n  %s query ARCHIVO SENSOR_ID|* START_MS|* END_MS|*\n  %s export ARCHIVO CSV SENSOR_ID|* START_MS|* END_MS|*\n  %s diagnose ARCHIVO\n  %s recover ORIGEN DESTINO\n", program, program, program, program, program, program, program, program);
}
static long long parse_ll(const char *text, bool *ok) { char *end = NULL; errno = 0; const long long value = strtoll(text, &end, 10); *ok = errno == 0 && end != text && *end == '\0'; return value; }
static int print_error(const char *operation, telemetry_result result) { fprintf(stderr, "%s: %s\n", operation, telemetry_result_name(result)); return 2; }
static int parse_filter(const char *sensor_text, const char *start_text, const char *end_text, telemetry_filter *filter) {
    *filter = (telemetry_filter){0}; bool ok = false;
    if (strcmp(sensor_text, "*") != 0) { const long long sensor = parse_ll(sensor_text, &ok); if (!ok || sensor < 1 || sensor > UINT32_MAX) return 0; filter->has_sensor = true; filter->sensor_id = (uint32_t)sensor; }
    if (strcmp(start_text, "*") != 0) { const long long start = parse_ll(start_text, &ok); if (!ok || start < 0) return 0; filter->has_start_timestamp = true; filter->start_timestamp_ms = (int64_t)start; }
    if (strcmp(end_text, "*") != 0) { const long long end = parse_ll(end_text, &ok); if (!ok || end < 0) return 0; filter->has_end_timestamp = true; filter->end_timestamp_ms = (int64_t)end; }
    return !(filter->has_start_timestamp && filter->has_end_timestamp && filter->start_timestamp_ms >= filter->end_timestamp_ms);
}
static telemetry_result print_record(const telemetry_record *record, void *context) { (void)context; printf("%" PRId64 " sensor=%" PRIu32 " value_milli=%" PRId32 " status=%u\n", record->timestamp_ms, record->sensor_id, record->value_milli, (unsigned)record->status); return ferror(stdout) ? TELEMETRY_IO_ERROR : TELEMETRY_OK; }
static int query_records(const char *path, const telemetry_filter *filter) { size_t count = 0; const telemetry_result result = telemetry_query_file(path, filter, print_record, NULL, &count); if (result != TELEMETRY_OK) return print_error("query", result); printf("Coincidencias: %zu\n", count); return 0; }
static int list_records(const char *path) {
    size_t count = 0; telemetry_result result = telemetry_count_records(path, &count); if (result != TELEMETRY_OK) return print_error("list", result); if (count == 0) { puts("Registros: 0"); return 0; }
    if (count > SIZE_MAX / sizeof(telemetry_record)) { fprintf(stderr, "list: cantidad imposible de reservar\n"); return 2; }
    telemetry_record *records = malloc(count * sizeof *records); if (records == NULL) { fprintf(stderr, "list: sin memoria para %zu registros\n", count); return 2; }
    size_t loaded = 0; result = telemetry_read_records(path, records, count, &loaded); if (result != TELEMETRY_OK) { free(records); return print_error("list", result); }
    for (size_t i = 0; i < loaded; ++i) print_record(&records[i], NULL);
    free(records);
    printf("Registros: %zu\n", loaded);
    return 0;
}
static int print_summary(const char *path) {
    telemetry_summary summary; telemetry_result result = telemetry_analyze_file(path, &summary); if (result != TELEMETRY_OK) return print_error("summary", result); printf("Registros: %zu\n", summary.record_count);
    if (summary.record_count > 0) { printf("Primero: %" PRId64 "\nUltimo: %" PRId64 "\n", summary.first_timestamp_ms, summary.last_timestamp_ms); printf("Min: %" PRId32 "\nMax: %" PRId32 "\nPromedio: %.2f\n", summary.min_value_milli, summary.max_value_milli, summary.average_value_milli); } return 0;
}
static int print_diagnostics(const char *path) {
    telemetry_diagnostics diagnostics; const telemetry_result result = telemetry_diagnose_file(path, &diagnostics); if (result != TELEMETRY_OK) return print_error("diagnose", result);
    printf("Estado: %s\nRegistros validos: %zu\nPrefijo valido: %zu bytes\n", telemetry_result_name(diagnostics.stream_result), diagnostics.record_count, diagnostics.valid_prefix_bytes);
    if (diagnostics.record_count > 0) printf("Primero: %" PRId64 "\nUltimo: %" PRId64 "\n", diagnostics.first_timestamp_ms, diagnostics.last_timestamp_ms);
    return diagnostics.stream_result == TELEMETRY_OK ? 0 : 2;
}
int main(int argc, char **argv) {
    if (argc < 3) { usage(argv[0]); return 1; }
    if (strcmp(argv[1], "init") == 0 && argc == 3) { telemetry_result result = telemetry_create_file(argv[2]); if (result != TELEMETRY_OK) return print_error("init", result); printf("Archivo creado: %s\n", argv[2]); return 0; }
    if (strcmp(argv[1], "log") == 0 && argc == 7) { bool ok_timestamp = false, ok_sensor = false, ok_value = false, ok_status = false; const long long timestamp = parse_ll(argv[3], &ok_timestamp); const long long sensor = parse_ll(argv[4], &ok_sensor); const long long value = parse_ll(argv[5], &ok_value); const long long status = parse_ll(argv[6], &ok_status); if (!ok_timestamp || !ok_sensor || !ok_value || !ok_status || timestamp < 0 || sensor < 1 || sensor > UINT32_MAX || value < INT32_MIN || value > INT32_MAX || status < 0 || status > UINT8_MAX) { fprintf(stderr, "log: argumento numerico invalido\n"); return 1; } telemetry_record record = {(int64_t)timestamp, (uint32_t)sensor, (int32_t)value, (uint8_t)status}; telemetry_result result = telemetry_append_record(argv[2], &record); if (result != TELEMETRY_OK) return print_error("log", result); puts("Registro agregado."); return 0; }
    if (strcmp(argv[1], "list") == 0 && argc == 3) return list_records(argv[2]);
    if (strcmp(argv[1], "summary") == 0 && argc == 3) return print_summary(argv[2]);
    if (strcmp(argv[1], "query") == 0 && argc == 6) { telemetry_filter filter; if (!parse_filter(argv[3], argv[4], argv[5], &filter)) { fprintf(stderr, "query: filtro invalido\n"); return 1; } return query_records(argv[2], &filter); }
    if (strcmp(argv[1], "export") == 0 && argc == 7) { telemetry_filter filter; if (!parse_filter(argv[4], argv[5], argv[6], &filter)) { fprintf(stderr, "export: filtro invalido\n"); return 1; } size_t count = 0; const telemetry_result result = telemetry_export_csv(argv[2], argv[3], &filter, &count); if (result != TELEMETRY_OK) return print_error("export", result); printf("Exportados: %zu\n", count); return 0; }
    if (strcmp(argv[1], "diagnose") == 0 && argc == 3) return print_diagnostics(argv[2]);
    if (strcmp(argv[1], "recover") == 0 && argc == 4) { size_t recovered = 0; const telemetry_result result = telemetry_recover_valid_prefix(argv[2], argv[3], &recovered); if (result != TELEMETRY_OK) return print_error("recover", result); printf("Recuperados: %zu\nDestino: %s\n", recovered, argv[3]); return 0; }
    usage(argv[0]); return 1;
}
