# Solución de referencia — Checkpoint 02

> Consulta esta referencia sólo después de completar un intento propio.

## Dirección

Agrega `WS-READ-COUNT` junto a los demás contadores. Increméntalo únicamente en el camino `NOT AT END`, antes de `PROCESS-RECORD`.

Ejemplo conceptual:

```text
READ EMPLOYEE-FILE
    AT END
        SET END-OF-FILE TO TRUE
    NOT AT END
        ADD 1 TO WS-READ-COUNT
        PERFORM PROCESS-RECORD
END-READ
```

Así el contador representa una observación independiente de la entrada.

## Resumen

Incluye el valor antes de `PROCESADOS` y `RECHAZADOS`:

```text
RESUMEN|LEIDOS=8|PROCESADOS=2|RECHAZADOS=6|...
```

Los importes monetarios permanecen iguales.

## Prueba

Actualiza el smoke para comprobar la línea completa del resumen. La relación esperada es `8 = 2 + 6`.

## Razón de diseño

Si `LEIDOS` se calcula sólo como suma de los otros contadores, deja de aportar una observación independiente. Medirlo al leer permite comparar entrada y clasificación.

## Extensión

Con un tercer resultado `OMITIDO`, la relación pasaría a ser:

```text
LEIDOS = PROCESADOS + RECHAZADOS + OMITIDOS
```
