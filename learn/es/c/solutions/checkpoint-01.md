# Solución de referencia — Checkpoint 01

Una solución pequeña reutiliza `validate_header`, avanza el archivo en bloques de 17 bytes y mantiene `out_count` en cero hasta validar los argumentos/header.

La forma esencial puede ser:

```c
telemetry_result telemetry_count_records(const char *path, size_t *out_count) {
    if (path == NULL || out_count == NULL) return TELEMETRY_INVALID_ARGUMENT;
    *out_count = 0;
    FILE *file = fopen(path, "rb");
    if (file == NULL) return TELEMETRY_IO_ERROR;
    telemetry_result result = validate_header(file);
    if (result != TELEMETRY_OK) { fclose(file); return result; }

    unsigned char bytes[17];
    while (true) {
        size_t n = fread(bytes, 1, sizeof bytes, file);
        if (n == 0) { if (ferror(file)) result = TELEMETRY_IO_ERROR; break; }
        if (n != sizeof bytes) { result = TELEMETRY_INVALID_FORMAT; break; }
        ++(*out_count);
    }
    if (fclose(file) != 0 && result == TELEMETRY_OK) result = TELEMETRY_IO_ERROR;
    return result;
}
```

## Por qué esta solución

No convierte el contador en un segundo parser ni asigna memoria dinámica. La función aprovecha el formato actual y devuelve la misma taxonomía de errores.

Una alternativa futura más escalable será encapsular lectura incremental en un iterator/callback propio de C; todavía no lo necesitamos en este checkpoint.
