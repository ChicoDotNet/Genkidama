# Solución de referencia — Checkpoint 02

Una solución pequeña puede añadir una función de presentación en `main.c`:

```c
static int print_count(const char *path) {
    size_t count = 0;
    const telemetry_result result = telemetry_count_records(path, &count);
    if (result != TELEMETRY_OK) return print_error("count", result);
    printf("%zu\n", count);
    return 0;
}
```

Después agrega `count ARCHIVO` a `usage` y despacha el comando junto a `list` y `summary`.

La parte importante no es el `printf`: es que la CLI reutiliza la API que ya conoce el formato y conserva el mismo contrato de errores. No aparece un segundo parser ni un arreglo temporal.

Para probarlo, crea un archivo con tres registros y verifica salida `3`; después agrega bytes incompletos al final y exige un código distinto de cero con `truncated_record`.
